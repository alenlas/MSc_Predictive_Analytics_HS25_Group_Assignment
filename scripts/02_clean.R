# scripts/02_clean.R
# Purpose: Apply dictionary names, run sanity checks, and write processed CSV/RDS

# 0) Packages
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

# 1) I/O setup
dir.create(here("data/processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("reports"), recursive = TRUE, showWarnings = FALSE)

# 2) Load interim raw import (frozen by 01_import_raw.R)
raw_rds <- here("data/interim/ticdata2000_raw.rds")
stopifnot(file.exists(raw_rds))
df <- readRDS(raw_rds)

# 3) Load dictionary lines
#    (Use the raw snapshot to guarantee traceability, but you can also re-read data/raw/dictionary.txt)
dict_lines <- readLines(here("data/interim/dictionary_snapshot.txt"), warn = FALSE)
dict_lines <- iconv(dict_lines, from = "", to = "UTF-8", sub = " ")

# Drop header if present
if (length(dict_lines) > 0 &&
    grepl("^\\s*Nr\\s+Name\\s+Description\\s+Domain\\s*$", dict_lines[1])) {
  dict_lines <- dict_lines[-1]
}

# 4) Extract candidate variable names (only from the top numbered block)
m         <- regexec("^\\s*\\d+\\s+([[:alnum:]_]+)\\b", dict_lines, perl = TRUE)
tok       <- regmatches(dict_lines, m)
all_names <- vapply(tok, function(x) if (length(x) >= 2) x[2] else NA_character_, character(1))
all_names <- all_names[!is.na(all_names)]

p <- ncol(df)
cat("Interim shape:", nrow(df), "x", p, "\n")
cat("Extracted", length(all_names), "candidate names from dictionary.\n")

var_names <- head(all_names, p)
stopifnot(length(var_names) == p)

# 5) Apply names; handle rare off-by-one scenarios
if (ncol(df) == length(var_names)) {
  names(df) <- var_names
} else if (ncol(df) == length(var_names) + 1) {
  first_is_int <- all(grepl("^\\s*-?\\d+\\s*$", as.character(df[[1]])) | is.na(df[[1]]))
  if (first_is_int) {
    df <- df[, -1, drop = FALSE]
    names(df) <- var_names
    message("Dropped a leading index-like column to align with names.")
  } else {
    stop("Column mismatch persists: data has ", ncol(df), " vs names ", length(var_names))
  }
} else {
  stop("Column mismatch persists: data has ", ncol(df), " vs names ", length(var_names))
}

if ("Nr" %in% names(df)) df[["Nr"]] <- NULL

# 6) Sanity checks (shape + key domains)
stopifnot(nrow(df) == 5822, ncol(df) == 86)
stopifnot(all(df$MOSTYPE  %in% 1:41))
stopifnot(all(df$MGEMOMV  %in% 1:5))
stopifnot(all(df$MGEMLEEF %in% 1:6))
stopifnot(all(df$MOSHOOFD %in% 1:10))
stopifnot(all(df$MGODRK   %in% 0:9))
stopifnot(all(df$CARAVAN  %in% 0:1))

# Optional: quick duplicate & NA checks (report-only; no mutation here)
dup_rows <- sum(duplicated(df))
na_cols  <- which(colSums(is.na(df)) > 0)
message(sprintf("Duplicates: %d; Columns with NAs: %d", dup_rows, length(na_cols)))

# 7) Save processed outputs
out_csv <- here("data/processed/ticdata2000_clean.csv")
out_rds <- here("data/processed/ticdata2000_clean.rds")
write.csv(df, out_csv, row.names = FALSE)
saveRDS(df,  out_rds)

# 8) Cleaning log
clean_log <- here("reports/clean_log.md")
pos_rate  <- mean(df$CARAVAN)
cat(
  sprintf(paste0(
    "# Cleaning Log\n\n",
    "- Timestamp: %s\n",
    "- Output shape: %d x %d\n",
    "- CARAVAN positive rate: %.2f%% (%d positives)\n",
    "- Duplicates: %d\n",
    "- Columns with NAs: %d\n",
    "- Outputs:\n",
    "  - %s\n",
    "  - %s\n"
  ),
  Sys.time(), nrow(df), ncol(df),
  100*pos_rate, sum(df$CARAVAN==1),
  dup_rows, length(na_cols),
  out_csv, out_rds),
  file = clean_log
)

cat("Clean data saved to data/processed/ and log to reports/clean_log.md\n")
