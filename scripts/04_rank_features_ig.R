
---
title: "Exploratory Data Analysis"
author: "Group"
output: html_document
---

```
Shortly: it’s the first pass you do on a dataset to understand it before modeling.

What we do in EDA:
  
  Sanity checks: rows/columns, data types, class balance, missing/duplicate values.

Validate ranges: values match the codebook (e.g., MOSTYPE ∈ 1..41, CARAVAN ∈ {0,1}).

Spot issues: outliers, near-zero-variance features, weird spikes.

Quick signals: simple summaries/plots to see which variables relate to the target.

Outputs:
  
  A few tables (summary stats, missingness), quick plots (target distribution, rates by feature), and a short list of fixes (drop/transform/keep).
```

# scripts/03_eda.R
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("skimr", quietly = TRUE)) install.packages("skimr")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")

library(here); library(dplyr); library(ggplot2); library(skimr); library(caret); library(readr)

# 0) I/O
in_path  <- here("data/processed/ticdata2000_clean.rds")
fig_dir  <- here("reports/figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

df <- readRDS(in_path)

# 1) Shape + class balance
cat("Rows x Cols:", nrow(df), "x", ncol(df), "\n")
stopifnot(nrow(df) == 5822, ncol(df) == 86)

p1 <- mean(df$CARAVAN == 1)
cat(sprintf("CARAVAN positive rate: %.2f%% (%d positives)\n", 100*p1, sum(df$CARAVAN==1)))

# 2) Missingness + duplicates
na_counts <- sapply(df, function(x) sum(is.na(x)))
na_tbl <- sort(na_counts[na_counts > 0], decreasing = TRUE)
if (length(na_tbl)) {
  cat("Columns with NAs:\n"); print(na_tbl)
} else cat("No missing values found.\n")

dup_rows <- sum(duplicated(df))
cat("Duplicate rows:", dup_rows, "\n")

# 3) Dictionary range checks (spot a few key ones)
range_checks <- list(
  MOSTYPE  = 1:41,
  MGEMOMV  = 1:5,
  MGEMLEEF = 1:6,
  MOSHOOFD = 1:10,
  MGODRK   = 0:9,
  CARAVAN  = 0:1
)
for (nm in names(range_checks)) {
  ok <- all(df[[nm]] %in% range_checks[[nm]])
  if (!ok) {
    bad_vals <- setdiff(unique(df[[nm]]), range_checks[[nm]])
    warning(sprintf("Out-of-range values in %s: %s", nm, paste(bad_vals, collapse=", ")))
  }
}

# 4) Zero-variance / near-zero-variance
nzv <- nearZeroVar(df[, setdiff(names(df), "CARAVAN")], saveMetrics = TRUE)
nzv_vars <- rownames(nzv)[nzv$nzv]
if (length(nzv_vars)) {
  writeLines(paste("Near-zero variance:", paste(nzv_vars, collapse = ", ")))
}

# 5) Quick summaries
skim_summary <- skim(df)
readr::write_csv(as.data.frame(skim_summary), here("reports/skim_summary.csv"))

# 6) Simple plots
# 6a) Target bar
p_target <- ggplot(df, aes(factor(CARAVAN))) +
  geom_bar() + labs(x = "CARAVAN", y = "Count", title = "Target distribution")
ggsave(filename = file.path(fig_dir, "target_distribution.png"), plot = p_target, width = 5, height = 4, dpi = 150)

# 6b) A few informative variables vs target (change these if needed)
vars_to_peek <- c("APERSAUT","PPERSAUT","PBRAND","MINKGEM","MOSTYPE","MOSHOOFD")
for (v in vars_to_peek) {
  if (is.numeric(df[[v]]) && length(unique(df[[v]])) > 12) {
    # numeric histogram (overall)
    g <- ggplot(df, aes(.data[[v]])) + geom_histogram(bins = 30) +
      labs(title = paste(v, "distribution"))
    ggsave(file.path(fig_dir, paste0(v, "_hist.png")), g, width = 5, height = 4, dpi = 150)
  }
  # rate plot by bucket (works for coded 0-9 variables too)
  rate <- df %>% group_by(.data[[v]]) %>% summarise(rate = mean(CARAVAN), n = n(), .groups="drop")
  g2 <- ggplot(rate, aes(x = .data[[v]], y = rate)) + geom_col() +
    labs(title = paste("CARAVAN rate by", v), x = v, y = "Rate")
  ggsave(file.path(fig_dir, paste0(v, "_caravan_rate.png")), g2, width = 6, height = 4, dpi = 150)
}

# 7) Correlation snapshot among numeric predictors (optional)
num_df <- df %>% select(where(is.numeric)) %>% select(-CARAVAN)
if (ncol(num_df) > 1) {
  cor_mat <- suppressWarnings(cor(num_df))
  # Save a quick CSV so you can view in Excel
  readr::write_csv(as.data.frame(cor_mat), here("reports/correlation_matrix.csv"))
}

cat("EDA complete. Outputs:\n",
    "- reports/skim_summary.csv\n",
    "- reports/correlation_matrix.csv (optional)\n",
    "- figures in reports/figures/\n")
