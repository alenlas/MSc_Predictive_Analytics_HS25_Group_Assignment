# scripts/05_model_train_eval.R
# Purpose: Train baseline + tree/forest + XGBoost and evaluate on an imbalanced target

# ----- 0) Packages & dirs -----
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("yardstick", quietly = TRUE)) install.packages("yardstick")
if (!requireNamespace("rsample", quietly = TRUE)) install.packages("rsample")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("xgboost", quietly = TRUE)) install.packages("xgboost")
if (!requireNamespace("rpart", quietly = TRUE)) install.packages("rpart")
if (!requireNamespace("ranger", quietly = TRUE)) install.packages("ranger")

library(here); library(dplyr); library(readr)
library(yardstick); library(rsample); library(ggplot2)
library(xgboost); library(rpart); library(ranger)

set.seed(42)
dir.create(here("models"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("reports/figures"), recursive = TRUE, showWarnings = FALSE)

# ----- 1) Load data -----
df <- readRDS(here("data/processed/ticdata2000_clean.rds"))
stopifnot("CARAVAN" %in% names(df))

# ----- 2) Pick features -----
# Use top-N from IG ranking (created by 04_rank_features_ig.R)
ig_path <- here("reports/ig_ranking.csv")
if (file.exists(ig_path)) {
  ig <- readr::read_csv(ig_path, show_col_types = FALSE)
  # FSelectorRcpp uses column 'attributes'; be robust if it's named differently
  attr_col <- if ("attributes" %in% names(ig)) "attributes" else names(ig)[1]
  top_feats <- ig[[attr_col]][1:40]  # adjust N as you like
  top_feats <- intersect(top_feats, setdiff(names(df), "CARAVAN"))
} else {
  # Fallback: use everything (not ideal, but keeps script runnable)
  warning("reports/ig_ranking.csv not found — using all predictors as fallback.")
  top_feats <- setdiff(names(df), "CARAVAN")
}

d <- dplyr::select(df, all_of(c("CARAVAN", top_feats)))

# ----- 3) Split: train/valid/test with stratification -----
initial <- initial_split(d, prop = 0.80, strata = CARAVAN)
trainvalid <- training(initial); test <- testing(initial)

valid_split <- initial_split(trainvalid, prop = 0.75, strata = CARAVAN)
train <- training(valid_split); valid <- testing(valid_split)

# ----- 4) Helper metrics -----
recall_at_k <- function(truth, score, k_frac = 0.05) {
  k <- max(1, ceiling(k_frac * length(score)))
  thr <- sort(score, decreasing = TRUE)[k]
  mean(truth[score >= thr] == 1)
}

# yardstick expects numeric estimates in [0,1]; truth can be numeric 0/1 for auc/pr
pr_auc_vec2  <- function(truth, est) pr_auc_vec(truth = truth, estimate = est)
roc_auc_vec2 <- function(truth, est) roc_auc_vec(truth = truth, estimate = est)

# ----- 5) Logistic Regression (baseline) -----
glm1 <- glm(CARAVAN ~ ., data = train, family = binomial())
valid$pred_glm <- predict(glm1, valid, type = "response")
test$pred_glm  <- predict(glm1,  test, type  = "response")

# ----- 6) Decision Tree & Random Forest (need FACTOR y) -----
# Create factor copies for tree/forest so we can extract P(class==1)
train_cls <- train;  valid_cls <- valid;  test_cls <- test
train_cls$CARAVAN <- factor(train_cls$CARAVAN, levels = c(0, 1))
valid_cls$CARAVAN <- factor(valid_cls$CARAVAN, levels = c(0, 1))
test_cls$CARAVAN  <- factor(test_cls$CARAVAN,  levels = c(0, 1))

# Decision Tree (rpart)
tree1 <- rpart(CARAVAN ~ ., data = train_cls, method = "class")
valid$pred_tree <- predict(tree1, valid_cls, type = "prob")[, "1"]
test$pred_tree  <- predict(tree1, test_cls,  type = "prob")[, "1"]

# Random Forest (ranger)
rf1 <- ranger(CARAVAN ~ ., data = train_cls,
              probability = TRUE, num.trees = 500, seed = 42)
valid$pred_rf <- predict(rf1, valid_cls)$predictions[, "1"]
test$pred_rf  <- predict(rf1, test_cls)$predictions[, "1"]

# ----- 7) XGBoost (imbalance-aware) -----
mx <- as.matrix(dplyr::select(train, -CARAVAN)); my <- train$CARAVAN
vx <- as.matrix(dplyr::select(valid, -CARAVAN)); vy <- valid$CARAVAN
tx <- as.matrix(dplyr::select(test,  -CARAVAN)); ty <- test$CARAVAN

dtrain <- xgb.DMatrix(mx, label = my)
dvalid <- xgb.DMatrix(vx, label = vy)
dtest  <- xgb.DMatrix(tx, label = ty)

pos_wt <- (length(my) - sum(my)) / sum(my)  # ratio of negatives to positives
param <- list(
  objective = "binary:logistic",
  eval_metric = "aucpr",
  max_depth = 4, eta = 0.10,
  subsample = 0.8, colsample_bytree = 0.8,
  min_child_weight = 1, scale_pos_weight = pos_wt
)
watch <- list(train = dtrain, valid = dvalid)
xgb1 <- xgb.train(params = param, data = dtrain, nrounds = 500,
                  watchlist = watch, early_stopping_rounds = 30, verbose = 0)

valid$pred_xgb <- predict(xgb1, dvalid)
test$pred_xgb  <- predict(xgb1, dtest)

# Save models
saveRDS(glm1, here("models/logit_baseline.rds"))
xgb.save(xgb1, here("models/xgb1.model"))
saveRDS(tree1, here("models/tree1.rds"))
saveRDS(rf1,   here("models/rf1.rds"))

# ----- 8) Metrics on VALID and TEST -----
score_and_metrics <- function(df_scored, suffix) {
  tibble(
    model = c("glm","tree","rf","xgb"),
    set   = suffix,
    AUPRC = c(
      pr_auc_vec2(df_scored$CARAVAN, df_scored$pred_glm),
      pr_auc_vec2(df_scored$CARAVAN, df_scored$pred_tree),
      pr_auc_vec2(df_scored$CARAVAN, df_scored$pred_rf),
      pr_auc_vec2(df_scored$CARAVAN, df_scored$pred_xgb)
    ),
    ROC_AUC = c(
      roc_auc_vec2(df_scored$CARAVAN, df_scored$pred_glm),
      roc_auc_vec2(df_scored$CARAVAN, df_scored$pred_tree),
      roc_auc_vec2(df_scored$CARAVAN, df_scored$pred_rf),
      roc_auc_vec2(df_scored$CARAVAN, df_scored$pred_xgb)
    ),
    Recall_at_5pct = c(
      recall_at_k(df_scored$CARAVAN, df_scored$pred_glm, 0.05),
      recall_at_k(df_scored$CARAVAN, df_scored$pred_tree, 0.05),
      recall_at_k(df_scored$CARAVAN, df_scored$pred_rf, 0.05),
      recall_at_k(df_scored$CARAVAN, df_scored$pred_xgb, 0.05)
    )
  )
}

metrics_all <- bind_rows(
  score_and_metrics(valid, "valid"),
  score_and_metrics(test,  "test")
)
readr::write_csv(metrics_all, here("reports/model_metrics.csv"))

# ----- 9) PR curve (test set, XGB) -----
# yardstick's pr_curve prefers factor truth; make a factor copy only for plotting
test_plot <- test %>% mutate(CARAVAN = factor(CARAVAN, levels = c(0,1)))
pr_dat <- yardstick::pr_curve(test_plot, truth = CARAVAN, estimate = pred_xgb)
ggplot(pr_dat, aes(recall, precision)) +
  geom_path() +
  labs(title = "PR Curve (XGB, test)", x = "Recall", y = "Precision")
ggsave(here("reports/figures/pr_curve_xgb_test.png"), width = 6, height = 4, dpi = 150)

# ----- 10) Save top-5% scored list (operational view) -----
kfrac <- 0.05
k <- max(1, ceiling(kfrac * nrow(test)))
thr <- sort(test$pred_xgb, decreasing = TRUE)[k]
top_k <- test %>% mutate(score = pred_xgb) %>% arrange(desc(score)) %>% head(k)
readr::write_csv(top_k, here("reports/top5pct_scored_test.csv"))

cat("Modeling complete:\n- reports/model_metrics.csv\n- reports/figures/pr_curve_xgb_test.png\n- reports/top5pct_scored_test.csv\n- models/: logit_baseline.rds, tree1.rds, rf1.rds, xgb1.model\n")
