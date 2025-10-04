# scripts/05_model_train_eval.R
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("yardstick", quietly = TRUE)) install.packages("yardstick")
if (!requireNamespace("rsample", quietly = TRUE)) install.packages("rsample")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("xgboost", quietly = TRUE)) install.packages("xgboost")
if (!requireNamespace("rpart", quietly = TRUE)) install.packages("rpart")
if (!requireNamespace("ranger", quietly = TRUE)) install.packages("ranger")

library(here); library(dplyr); library(readr); library(yardstick); library(rsample)
library(ggplot2); library(xgboost); library(rpart); library(ranger)

set.seed(42)
dir.create(here("models"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("reports/figures"), recursive = TRUE, showWarnings = FALSE)

# 1) Load data + select features
df <- readRDS(here("data/processed/ticdata2000_clean.rds"))
y  <- "CARAVAN"

# Option A: keep top N features by IG (replace with your list)
ig <- readr::read_csv(here("reports/ig_ranking.csv"))
top_feats <- ig$attributes[1:40]  # adjust N
feats <- intersect(top_feats, setdiff(names(df), y))
d <- dplyr::select(df, all_of(c(y, feats)))

# 2) Split (train/valid/test) with stratification
initial <- initial_split(d, prop = 0.8, strata = !!sym(y))
trainvalid <- training(initial); test <- testing(initial)

valid_split <- initial_split(trainvalid, prop = 0.75, strata = !!sym(y))
train <- training(valid_split); valid <- testing(valid_split)

# Helpers
pr_auc_vec2 <- function(truth, estimate) pr_auc_vec(truth = truth, estimate = estimate)
recall_at_k <- function(truth, score, k_frac = 0.05) {
  k <- ceiling(k_frac * length(score))
  thr <- sort(score, decreasing = TRUE)[k]
  mean(truth[score >= thr] == 1)
}

# 3) Baseline: Logistic Regression
glm1 <- glm(CARAVAN ~ ., data = train, family = binomial())
valid$pred_glm <- predict(glm1, valid, type = "response")
test$pred_glm  <- predict(glm1, test,  type = "response")

# 4) Tree & Forest
tree1 <- rpart(CARAVAN ~ ., data = train, method = "class")
valid$pred_tree <- predict(tree1, valid, type = "prob")[, "1"]
test$pred_tree  <- predict(tree1,  test, type = "prob")[, "1"]

rf1 <- ranger(CARAVAN ~ ., data = train, probability = TRUE, num.trees = 500, seed = 42)
valid$pred_rf <- predict(rf1, valid)$predictions[, "1"]
test$pred_rf  <- predict(rf1, test)$predictions[, "1"]

# 5) XGBoost (imbalance-aware)
mx <- as.matrix(dplyr::select(train, -CARAVAN)); my <- train$CARAVAN
vx <- as.matrix(dplyr::select(valid, -CARAVAN)); vy <- valid$CARAVAN
tx <- as.matrix(dplyr::select(test,  -CARAVAN)); ty <- test$CARAVAN

dtrain <- xgb.DMatrix(mx, label = my)
dvalid <- xgb.DMatrix(vx, label = vy)
dtest  <- xgb.DMatrix(tx, label = ty)

pos_wt <- (length(my) - sum(my)) / sum(my)  # scale_pos_weight
param <- list(
  objective = "binary:logistic",
  eval_metric = "aucpr",
  max_depth = 4, eta = 0.1, subsample = 0.8, colsample_bytree = 0.8,
  min_child_weight = 1, scale_pos_weight = pos_wt
)
watch <- list(train = dtrain, valid = dvalid)
xgb1 <- xgb.train(params = param, data = dtrain, nrounds = 500,
                  watchlist = watch, early_stopping_rounds = 30, verbose = 0)

valid$pred_xgb <- predict(xgb1, dvalid)
test$pred_xgb  <- predict(xgb1, dtest)

saveRDS(glm1, here("models/logit_baseline.rds"))
xgb.save(xgb1, here("models/xgb1.model"))

# 6) Metrics on VALID and TEST
models <- c("glm","tree","rf","xgb")
metrics_tbl <- list()

score_and_metrics <- function(df_scored, suffix) {
  tibble(
    model = models,
    set   = suffix,
    AUPRC = c(
      pr_auc_vec(df_scored$CARAVAN, df_scored$pred_glm),
      pr_auc_vec(df_scored$CARAVAN, df_scored$pred_tree),
      pr_auc_vec(df_scored$CARAVAN, df_scored$pred_rf),
      pr_auc_vec(df_scored$CARAVAN, df_scored$pred_xgb)
    ),
    ROC_AUC = c(
      roc_auc_vec(df_scored$CARAVAN, df_scored$pred_glm),
      roc_auc_vec(df_scored$CARAVAN, df_scored$pred_tree),
      roc_auc_vec(df_scored$CARAVAN, df_scored$pred_rf),
      roc_auc_vec(df_scored$CARAVAN, df_scored$pred_xgb)
    ),
    Recall_at_5pct = c(
      recall_at_k(df_scored$CARAVAN, df_scored$pred_glm, 0.05),
      recall_at_k(df_scored$CARAVAN, df_scored$pred_tree, 0.05),
      recall_at_k(df_scored$CARAVAN, df_scored$pred_rf, 0.05),
      recall_at_k(df_scored$CARAVAN, df_scored$pred_xgb, 0.05)
    )
  )
}

metrics_tbl[[1]] <- score_and_metrics(valid, "valid")
metrics_tbl[[2]] <- score_and_metrics(test,  "test")
metrics_all <- bind_rows(metrics_tbl)
readr::write_csv(metrics_all, here("reports/model_metrics.csv"))

# 7) PR curve (test set)
pr_dat <- yardstick::pr_curve(test, truth = CARAVAN, estimate = pred_xgb)
ggplot(pr_dat, aes(recall, precision)) +
  geom_path() + geom_point(data = pr_dat[1,], aes(recall, precision)) +
  labs(title = "PR Curve (XGB, test)", x = "Recall", y = "Precision")
ggsave(here("reports/figures/pr_curve_xgb_test.png"), width = 6, height = 4, dpi = 150)

# 8) Save top-k list (operational)
kfrac <- 0.05
k <- ceiling(kfrac * nrow(test))
thr <- sort(test$pred_xgb, decreasing = TRUE)[k]
top_k <- test %>% mutate(score = pred_xgb) %>% arrange(desc(score)) %>% head(k)
readr::write_csv(top_k, here("reports/top5pct_scored_test.csv"))

cat("Modeling done. See reports/model_metrics.csv and figures/. Models saved in models/.\n")
