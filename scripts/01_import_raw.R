## 0) Packages
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

## 1) Read dictionary (from data/raw)
dict_path <- here("data/raw/dictionary.txt")
dict_raw   <- readLines(dict_path, warn = FALSE)
dict_lines <- iconv(dict_raw, from = "", to = "UTF-8", sub = " ")

# Drop header if present
if (length(dict_lines) > 0 &&
    grepl("^\\s*Nr\\s+Name\\s+Description\\s+Domain\\s*$", dict_lines[1])) {
  dict_lines <- dict_lines[-1]
}

## 2) Extract candidate variable names
m         <- regexec("^\\s*\\d+\\s+([[:alnum:]_]+)\\b", dict_lines, perl = TRUE)
tok       <- regmatches(dict_lines, m)
all_names <- vapply(tok, function(x) if (length(x) >= 2) x[2] else NA_character_, character(1))
all_names <- all_names[!is.na(all_names)]
cat("Extracted", length(all_names), "name-like tokens from dictionary.\n")

## 3) Read data (from data/raw)
data_path <- here("data/raw/ticdata2000.txt")
df <- read.table(data_path,
                 header = FALSE, sep = "", quote = "",
                 comment.char = "", strip.white = TRUE, fill = TRUE)
p <- ncol(df)
cat("Data has", p, "columns.\n")

## 4) Keep only the first p names from dictionary’s top block
var_names <- head(all_names, p)
stopifnot(length(var_names) == p)

## 5) Apply names (handle rare off-by-one)
if (ncol(df) == length(var_names)) {
  names(df) <- var_names
} else if (ncol(df) == length(var_names) + 1) {
  first_is_int <- all(grepl("^\\s*-?\\d+\\s*$", as.character(df[[1]])) | is.na(df[[1]]))
  if (first_is_int) {
    df <- df[, -1, drop = FALSE]
    names(df) <- var_names
    message("Dropped a leading index-like column to align with names.")
  } else stop("Column mismatch persists.")
} else stop("Column mismatch persists.")

if ("Nr" %in% names(df)) df[["Nr"]] <- NULL

## 6) Sanity checks (shape + some known domains)
stopifnot(nrow(df) == 5822, ncol(df) == 86)
stopifnot(all(df$MOSTYPE  %in% 1:41))
stopifnot(all(df$MGEMOMV  %in% 1:5))
stopifnot(all(df$MGEMLEEF %in% 1:6))
stopifnot(all(df$MOSHOOFD %in% 1:10))
stopifnot(all(df$MGODRK   %in% 0:9))
stopifnot(all(df$CARAVAN  %in% 0:1))

## 7) Save + quick peek (to data/processed)
out_dir <- here("data/processed")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(df, file.path(out_dir, "ticdata2000_clean.csv"), row.names = FALSE)
saveRDS(df, file.path(out_dir, "ticdata2000_clean.rds"))
cat("Saved to data/processed/\n")
cat("First 10 column names:\n"); print(head(names(df), 10))
