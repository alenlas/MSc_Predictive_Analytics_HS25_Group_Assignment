# scripts/01_import_raw.R
# Purpose: Read raw files ONLY and save a frozen interim snapshot (no renaming/cleaning)

# 0) Packages
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

# 1) Ensure dirs
dir.create(here("data/interim"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("reports"), recursive = TRUE, showWarnings = FALSE)

# 2) Read RAW data (no renaming, no domain checks)
raw_path  <- here("data/raw/ticdata2000.txt")
stopifnot(file.exists(raw_path))
raw_df <- read.table(raw_path,
                     header = FALSE, sep = "", quote = "",
                     comment.char = "", strip.white = TRUE, fill = TRUE)
cat("Imported RAW shape:", nrow(raw_df), "x", ncol(raw_df), "\n")

# 3) Snapshot dictionary as-is (for traceability)
dict_path <- here("data/raw/dictionary.txt")
stopifnot(file.exists(dict_path))
dict_raw  <- readLines(dict_path, warn = FALSE)

# 4) Save frozen interim artifacts
saveRDS(raw_df,  here("data/interim/ticdata2000_raw.rds"))
writeLines(dict_raw, here("data/interim/dictionary_snapshot.txt"))

# 5) Minimal import log
log_path <- here("reports/import_log.md")
cat(
  sprintf("# Import Log\n\n- Timestamp: %s\n- Raw shape: %d x %d\n- Raw file: %s\n- Dictionary snapshot: %s\n",
          Sys.time(), nrow(raw_df), ncol(raw_df),
          raw_path, here("data/interim/dictionary_snapshot.txt")),
  file = log_path
)

cat("Saved raw import to data/interim/ (ticdata2000_raw.rds, dictionary_snapshot.txt)\n")
