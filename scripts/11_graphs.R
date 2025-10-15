# Pakete
library(tidyverse)
# At the very top of your script (after library(tidyverse))
library(conflicted)

conflicts_prefer(
  dplyr::select,
  dplyr::filter,
  dplyr::rename,
  dplyr::slice,
  dplyr::lag
)

library(pheatmap)
library(viridisLite)





# ==== Daten laden ====
# Import our data and assing it to churndt
library(here)
df <- read.csv(here("data/processed/ticdata2000_clean.csv"))

# Sicherstellen: Ziel als numerisch 0/1
stopifnot("CARAVAN" %in% names(df))
df$CARAVAN <- as.integer(df$CARAVAN)

# Produkt-Zähl-/Besitzspalten (A-Variablen auf Kundenebene)
product_count_cols <- c(
  "AWAPART","AWABEDR","AWALAND","APERSAUT","ABESAUT","AMOTSCO","AVRAAUT",
  "AAANHANG","ATRACTOR","AWERKT","ABROM","ALEVEN","APERSONG","AGEZONG",
  "AWAOREG","ABRAND","AZEILPL","APLEZIER","AFIETS","AINBOED","ABYSTAND"
)

# Nur vorhandene Spalten nehmen
product_count_cols <- intersect(product_count_cols, names(df))

# Numerisch erzwingen
corr_df <- df %>%
  dplyr::select(tidyselect::all_of(c(product_count_cols, "CARAVAN"))) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(),
                              ~ suppressWarnings(as.numeric(.x))))
# Pearson-Korrelationen
cmat <- suppressWarnings(cor(corr_df, use = "pairwise.complete.obs", method = "pearson"))

# Korrelation jeder Produktspalte mit CARAVAN
prod_vs_target <- cmat[setdiff(colnames(cmat), "CARAVAN"), "CARAVAN", drop = FALSE]

pheatmap(
  prod_vs_target,
  color = viridis(100),
  main  = "Korrelation (Produkte) mit CARAVAN",
  cluster_rows = TRUE, cluster_cols = FALSE,
  border_color = NA
)

# ==> Nicht so praktisch

library(tidyverse)
library(here)

# === Daten laden ===
df <- read.csv(here("data/processed/ticdata2000_clean.csv"))

# Zielvariable prüfen
stopifnot("CARAVAN" %in% names(df))

# 1) Gruppierte Mittelwerte (für numerische Variablen)
group_means <- df %>%
  group_by(CARAVAN) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# 2) Mittelwertdifferenzen berechnen
diffs <- as.data.frame(t(group_means[group_means$CARAVAN == 1, -1] -
                           group_means[group_means$CARAVAN == 0, -1]))
colnames(diffs) <- "mean_diff"
diffs$attribute <- rownames(diffs)
rownames(diffs) <- NULL

# 3) Nur valide numerische Werte
diffs <- diffs %>% drop_na(mean_diff)

# 4) Top 20 Merkmale
top20 <- diffs %>%
  arrange(desc(mean_diff)) %>%
  slice_head(n = 20) %>%
  as_tibble()

# 5) Ergebnis anzeigen
print(top20)

# 6) Visualisierung
ggplot(top20, aes(x = reorder(attribute, mean_diff), y = mean_diff)) +
  geom_col(fill = "#2166ac") +
  coord_flip() +
  labs(
    title = "Top 20 Merkmale mit höchster Ausprägung bei CARAVAN = 1",
    x = "Attribut",
    y = "Δ Mittelwert (CARAVAN=1 minus CARAVAN=0)"
  ) +
  theme_minimal(base_size = 12)

# ==> Hier die Version mit gemappten beschriftungen:

# Pakete
library(tidyverse)
library(here)

#---------------------------
# 1) DICTIONARY ROBUST LADEN
#---------------------------
dict_paths <- c(
  here("data/raw/dictionary.txt"),
  here("data/processed/dictionary.txt"),
  here("data/dictionary.txt"),
  "/mnt/data/dictionary.txt"  # Fallback (Upload)
)
dict_path <- dict_paths[file.exists(dict_paths)][1]
if (is.na(dict_path)) stop("dictionary.txt nicht gefunden – Pfad prüfen.")

read_dict_lines <- function(path) {
  encs <- c("UTF-8", "latin1", "windows-1252")
  for (enc in encs) {
    out <- try(readr::read_lines(path, locale = readr::locale(encoding = enc)), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
  }
  # Fallback: roher Binär-Read + iconv
  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  txt <- iconv(rawToChar(raw), from = "", to = "UTF-8", sub = "byte")
  unlist(strsplit(txt, "\n", fixed = TRUE))
}

dict_lines <- read_dict_lines(dict_path)

# Parser: Zeilen wie "1 APERSAUT Number of car policies ..."
rx <- "^\\s*\\d+\\s+([A-Z0-9_]+)\\s+(.*)$"
dict_tbl <- tibble(line = dict_lines) |>
  dplyr::filter(stringr::str_detect(line, rx)) |>
  dplyr::mutate(code = stringr::str_match(line, rx)[, 2],
                desc = stringr::str_match(line, rx)[, 3]) |>
  dplyr::mutate(label = desc |>
                  stringr::str_remove("\\s+see\\s+L\\d+.*$") |>
                  stringr::str_remove("\\s*\\.\\.\\..*$") |>
                  stringr::str_squish()) |>
  dplyr::distinct(code, .keep_all = TRUE) |>
  dplyr::select(code, label)


# Optionale, freundlichere Bezeichnungen überschreiben (falls im Dictionary zu generisch)
friendly_overrides <- tribble(
  ~code,       ~label,
  "CARAVAN",   "Mobile home policy (Target)",
  "APERSAUT",  "Auto insurance (household count)",
  "ABRAND",    "Fire insurance (household count)",
  "AWAPART",   "3rd-party liability (household count)",
  "PPERSAUT",  "Auto insurance prevalence (postcode)",
  "PBRAND",    "Fire insurance prevalence (postcode)",
  "PWAPART",   "Liability prevalence (postcode)",
  "MINKGEM",   "Average income (class)",
  "MKOOPKLA",  "Purchasing power class",
  "MHKOOP",    "Home owners",
  "MOPLHOOG",  "High education",
  "MBERHOOG",  "High social status",
  "MOSTYPE",   "Customer subtype (postcode)",
  "MOSHOOFD",  "Customer main type (L2)"
)
dict_tbl <- friendly_overrides |>
  rows_update(dict_tbl, by = "code", unmatched = "ignore") |>
  bind_rows(anti_join(dict_tbl, friendly_overrides, by = "code"))

#---------------------------
# 2) DATEN LADEN
#---------------------------
data_paths <- c(
  here("data/processed/ticdata2000_clean.csv"),
  "/mnt/data/ticdata2000_clean.csv"
)
data_path <- data_paths[file.exists(data_paths)][1]
if (is.na(data_path)) stop("ticdata2000_clean.csv nicht gefunden – Pfad prüfen.")

df <- read.csv(data_path, check.names = FALSE)
stopifnot("CARAVAN" %in% names(df))
# sicherheitshalber numerisch (0/1)
df$CARAVAN <- as.integer(df$CARAVAN)

#----------------------------------------------------------
# 3A) TOP-20 nach Mittelwert-Differenz (CARAVAN=1 minus =0)
#----------------------------------------------------------
group_means <- df |>
  group_by(CARAVAN) |>
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

diffs <- as.data.frame(t(group_means[group_means$CARAVAN == 1, -1] -
                           group_means[group_means$CARAVAN == 0, -1]))
colnames(diffs) <- "mean_diff"
diffs$code <- rownames(diffs)
rownames(diffs) <- NULL

top20_diff <- diffs |>
  filter(code != "CARAVAN") |>
  drop_na(mean_diff) |>
  arrange(desc(mean_diff)) |>
  slice_head(n = 20) |>
  left_join(dict_tbl, by = "code") |>
  mutate(label = if_else(is.na(label) | label == "", code, label))

# Plot
ggplot(top20_diff, aes(x = reorder(label, mean_diff), y = mean_diff)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Merkmale (höhere Ausprägung bei CARAVAN = 1)",
    subtitle = "Δ Mittelwert (CARAVAN=1 minus CARAVAN=0)",
    x = NULL, y = "Δ Mittelwert"
  ) +
  theme_minimal(base_size = 12)

#----------------------------------------------------------
# 3B) TOP-20 nach Häufigkeit bei CARAVAN=1 (für 0/1/Zählspalten)
#     -> Anteil Kunden mit Merkmal (>=1) innerhalb CARAVAN=1
#----------------------------------------------------------
freq_1 <- df |>
  filter(CARAVAN == 1) |>
  summarise(across(where(is.numeric), ~mean(.x > 0, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "code", values_to = "freq") |>
  filter(code != "CARAVAN") |>
  arrange(desc(freq)) |>
  slice_head(n = 20) |>
  left_join(dict_tbl, by = "code") |>
  mutate(label = if_else(is.na(label) | label == "", code, label))

ggplot(freq_1, aes(x = reorder(label, freq), y = freq)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Top 20 häufigste Merkmale unter CARAVAN = 1",
    subtitle = "Anteil Kunden mit Merkmal (>=1)",
    x = NULL, y = "Anteil"
  ) +
  theme_minimal(base_size = 12)

# ==> Jetzt nur für Personal labels:

# ---- Packages ----
library(tidyverse)
library(here)

# ---- Paths ----
data_paths <- c(
  here("data/processed/ticdata2000_clean.csv"),
  "/mnt/data/ticdata2000_clean.csv"
)
data_path <- data_paths[file.exists(data_paths)][1]
if (is.na(data_path)) stop("ticdata2000_clean.csv not found.")

dict_paths <- c(
  here("data/raw/dictionary.txt"),
  here("data/processed/dictionary.txt"),
  here("data/dictionary.txt"),
  "/mnt/data/dictionary.txt"
)
dict_path <- dict_paths[file.exists(dict_paths)][1]
if (is.na(dict_path)) stop("dictionary.txt not found.")

# ---- Read dictionary (encoding-safe) ----
read_dict_lines <- function(path) {
  for (enc in c("UTF-8", "latin1", "windows-1252")) {
    x <- try(readr::read_lines(path, locale = readr::locale(encoding = enc)), silent = TRUE)
    if (!inherits(x, "try-error")) return(x)
  }
  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  txt <- iconv(rawToChar(raw), from = "", to = "UTF-8", sub = "byte")
  unlist(strsplit(txt, "\n", fixed = TRUE))
}
dict_lines <- read_dict_lines(dict_path)

# Typical TIC dictionary line format: "NN CODE description..."
rx <- "^\\s*\\d+\\s+([A-Z0-9_]+)\\s+(.*)$"
dict_tbl <- tibble(line = dict_lines) |>
  dplyr::filter(stringr::str_detect(line, rx)) |>
  dplyr::mutate(
    code  = stringr::str_match(line, rx)[, 2],
    label = stringr::str_match(line, rx)[, 3] |>
      stringr::str_remove("\\s+see\\s+L\\d+.*$") |>
      stringr::str_remove("\\s*\\.\\.\\..*$") |>
      stringr::str_squish()
  ) |>
  dplyr::distinct(code, .keep_all = TRUE) |>
  dplyr::select(code, label)


# Friendly overrides (optional)
friendly_overrides <- tribble(
  ~code,       ~label,
  "APERSAUT",  "Auto insurance (household)",
  "ABRAND",    "Fire insurance (household)",
  "AWAPART",   "3rd-party liability (household)",
  "PPERSAUT",  "Auto insurance (postcode prev.)",
  "PBRAND",    "Fire insurance (postcode prev.)",
  "PWAPART",   "Liability (postcode prev.)"
)
dict_tbl <- friendly_overrides |>
  rows_update(dict_tbl, by = "code", unmatched = "ignore") |>
  bind_rows(anti_join(dict_tbl, friendly_overrides, by = "code"))

# ---- Load data ----
df <- read.csv(data_path, check.names = FALSE)
stopifnot("CARAVAN" %in% names(df))

# --- Select attributes 44–85 only ---
# (In TIC: 1–43 socio-demo, 44–86 products, 86 = CARAVAN)
if (ncol(df) < 85) stop("Data has fewer than 85 columns; check file.")
prod_vars <- names(df)[44:85]           # strictly the product block (excludes CARAVAN)
df_block  <- df |> select(all_of(c(prod_vars, "CARAVAN")))

# Ensure CARAVAN numeric 0/1 for aggregation safety
df_block$CARAVAN <- as.integer(df_block$CARAVAN)

# ---------- Top-20 by mean difference (CARAVAN=1 − 0) ----------
group_means <- df_block |>
  group_by(CARAVAN) |>
  summarise(across(all_of(prod_vars), ~mean(as.numeric(.x), na.rm = TRUE)), .groups = "drop")

diffs <- as.data.frame(t(group_means[group_means$CARAVAN == 1, -1] -
                           group_means[group_means$CARAVAN == 0, -1]))
colnames(diffs) <- "mean_diff"
diffs$code <- rownames(diffs); rownames(diffs) <- NULL

top20_diff <- diffs |>
  drop_na(mean_diff) |>
  arrange(desc(mean_diff)) |>
  slice_head(n = 20) |>
  left_join(dict_tbl, by = "code") |>
  mutate(label = if_else(is.na(label) | label == "", code, label))

# View table
print(top20_diff, row.names = FALSE)

# Plot
ggplot(top20_diff, aes(x = reorder(label, mean_diff), y = mean_diff)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 (Attributes 44–85): higher for CARAVAN = 1",
    subtitle = "Mean difference (CARAVAN=1 minus =0)",
    x = NULL, y = "Δ mean"
  ) +
  theme_minimal(base_size = 12)

# ---------- Top-20 by frequency within CARAVAN=1 (share ≥1) ----------
freq_1 <- df_block |>
  filter(CARAVAN == 1) |>
  summarise(across(all_of(prod_vars), ~mean(as.numeric(.x) > 0, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "code", values_to = "freq") |>
  arrange(desc(freq)) |>
  slice_head(n = 20) |>
  left_join(dict_tbl, by = "code") |>
  mutate(label = if_else(is.na(label) | label == "", code, label))

# View table
print(freq_1, row.names = FALSE)

# Plot
ggplot(freq_1, aes(x = reorder(label, freq), y = freq)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Top 20 (Attributes 44–85): most frequent within CARAVAN = 1",
    subtitle = "Share of customers with value ≥1",
    x = NULL, y = "Share"
  ) +
  theme_minimal(base_size = 12)

### Vergleich aller Insurance Policies, für future growth

library(tidyverse)
library(here)

# ==== Daten laden ====
df <- read.csv(here("data/processed/ticdata2000_clean.csv"))
stopifnot("CARAVAN" %in% names(df))

# Nur Versicherungsrelevante Spalten extrahieren (44–85 + CARAVAN)
insurance_vars <- c("APERSAUT","ABRAND","AWAPART","ALEVEN","AMOTSCO","AFIETS","CARAVAN")
df_ins <- df %>% select(all_of(insurance_vars))

# ==== 1) Berechne Besitzrate (Anteil Kunden mit >=1 Police) ====
ins_rates <- df_ins %>%
  summarise(across(everything(), ~mean(.x > 0, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "product", values_to = "rate")

# ==== 2) Freundliche Labels hinzufügen ====
labels <- tribble(
  ~product,    ~label,
  "APERSAUT",  "Car insurance",
  "ABRAND",    "Fire insurance",
  "AWAPART",   "Liability insurance",
  "ALEVEN",    "Life insurance",
  "AMOTSCO",  "Motorcycle insurance",
  "AFIETS",    "Bicycle insurance",
  "CARAVAN",   "Caravan insurance"
)
ins_rates <- ins_rates %>% left_join(labels, by = "product")

# ==== 3) Sortiere nach Rate ====
ins_rates <- ins_rates %>% arrange(desc(rate))

# ==== 3.5) Print ownership shares in console ====

# make sure ins_rates exists and has column 'rate'
stopifnot(exists("ins_rates"))
stopifnot("rate" %in% names(ins_rates))

ins_rates_summary <- ins_rates %>%
  mutate(share_percent = scales::percent(rate, accuracy = 0.1)) %>%
  select(label, share_percent) %>%
  arrange(desc(as.numeric(str_remove(share_percent, "%"))))

print(ins_rates_summary, n = Inf)


# ==== 4) Plot ====
ggplot(ins_rates, aes(x = reorder(label, rate), y = rate, fill = product == "CARAVAN")) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("grey70", "#d73027")) +
  labs(
    title = "Insurance Ownership Rates",
    subtitle = "Caravan insurance compared to other products",
    x = NULL, y = "Share of customers with policy"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# ==== 5) Grafik speichern ====
output_dir <- here("graphs_for_presentation")

# Ordner anlegen, falls er noch nicht existiert
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

output_path <- file.path(output_dir, "insurance_ownership_rates.png")

ggsave(
  filename = output_path,
  width = 8,        # Breite in Inches (z. B. 8 = ca. 20 cm)
  height = 6,       # Höhe in Inches
  dpi = 300,        # Druckqualität
  bg = "white"      # Weißer Hintergrund (besser für Präsentationen)
)

message("✅ Grafik gespeichert unter: ", output_path)


### Gleicher graph wie oben, aber mit Prozenten.
library(tidyverse)
library(here)

# ==== Daten laden ====
df <- read.csv(here("data/processed/ticdata2000_clean.csv"))
stopifnot("CARAVAN" %in% names(df))

# Nur Versicherungsrelevante Spalten extrahieren (44–85 + CARAVAN)
insurance_vars <- c("APERSAUT","ABRAND","AWAPART","ALEVEN","AMOTSCO","AFIETS","CARAVAN")
df_ins <- df %>% select(all_of(insurance_vars))

# ==== 1) Berechne Besitzrate (Anteil Kunden mit >=1 Police) ====
ins_rates <- df_ins %>%
  summarise(across(everything(), ~mean(.x > 0, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "product", values_to = "rate")

# ==== 2) Freundliche Labels hinzufügen ====
labels <- tribble(
  ~product,    ~label,
  "APERSAUT",  "Car insurance",
  "ABRAND",    "Fire insurance",
  "AWAPART",   "Liability insurance",
  "ALEVEN",    "Life insurance",
  "AMOTSCO",   "Motorcycle insurance",
  "AFIETS",    "Bicycle insurance",
  "CARAVAN",   "Caravan insurance"
)
ins_rates <- ins_rates %>% left_join(labels, by = "product")

# ==== 3) Sortiere nach Rate ====
ins_rates <- ins_rates %>% arrange(desc(rate))

# ==== 3.5) Konsolenausgabe mit Prozentwerten ====
ins_rates_summary <- ins_rates %>%
  mutate(share_percent = scales::percent(rate, accuracy = 0.1)) %>%
  select(label, share_percent)

print(ins_rates_summary, n = Inf)

# ==== 4) Plot mit Prozentwerten auf den Balken ====
p <- ggplot(ins_rates, aes(x = reorder(label, rate), y = rate, fill = product == "CARAVAN")) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::percent(rate, accuracy = 0.1)),
            hjust = -0.1, size = 4) +  # Prozentangaben auf den Balken
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, max(ins_rates$rate) * 1.15)) +
  scale_fill_manual(values = c("grey70", "#d73027")) +
  labs(
    title = "Insurance Ownership Rates",
    subtitle = "Caravan insurance compared to other products",
    x = NULL, y = "Share of customers with policy"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

p

# ==== 5) Grafik speichern ====
output_dir <- here("graphs_for_presentation")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

output_path <- file.path(output_dir, "insurance_ownership_rates_prozent.png")

ggsave(
  filename = output_path,
  plot = p,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

message("✅ Grafik gespeichert unter: ", output_path)



### Gleicher Vergleichsgraph aber erweitert auf alle 22 Insurances

library(tidyverse)
library(here)

# ==== Load data ====
df <- read.csv(here("data/processed/ticdata2000_clean.csv"))
stopifnot(ncol(df) >= 86)
stopifnot("CARAVAN" %in% names(df))

# ==== 1) Select all insurance ownership columns (65–86) ====
ins_vars <- names(df)[65:86]
df_ins <- df %>% select(all_of(ins_vars))

# ==== 2) Calculate ownership rates ====
ins_rates <- df_ins %>%
  summarise(across(everything(), ~mean(.x > 0, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "product", values_to = "rate")

# ==== 3) Add friendly labels ====
labels <- tribble(
  ~product,   ~label,
  "AWAPART",  "Liability insurance",
  "AWABEDR",  "Business insurance",
  "AWALAND",  "Agricultural insurance",
  "APERSAUT", "Car insurance",
  "ABESAUT",  "Company car insurance",
  "AMOTSCO",  "Motorcycle insurance",
  "AVRAAUT",  "Caravan/Trailer insurance",
  "AAANHANG", "Trailer insurance",
  "ATRACTOR", "Tractor insurance",
  "AWERKT",   "Work accident insurance",
  "ABROM",    "Fire insurance",
  "ALEVEN",   "Life insurance",
  "APERSONG", "Personal accident insurance",
  "AGEZONG",  "Health insurance",
  "AWAOREG",  "Disability insurance",
  "ABRAND",   "Fire/Property insurance",
  "AZEILPL",  "Boat/Sailing insurance",
  "APLEZIER", "Pleasure insurance",
  "AFIETS",   "Bicycle insurance",
  "AINBOED",  "Home contents insurance",
  "ABYSTAND", "Legal assistance insurance",
  "CARAVAN",  "Caravan insurance (target)"
)
ins_rates <- ins_rates %>%
  left_join(labels, by = "product") %>%
  mutate(label = if_else(is.na(label), product, label))

# ==== 4) Sort descending by ownership rate ====
ins_rates <- ins_rates %>% arrange(desc(rate))

# ==== 5) Plot ====
ggplot(ins_rates, aes(x = reorder(label, rate), y = rate,
                      fill = product == "CARAVAN")) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("grey70", "#d73027")) +
  labs(
    title = "Insurance Ownership Rates (Attributes 65–86)",
    subtitle = "Caravan insurance compared to all other household products",
    x = NULL,
    y = "Share of customers with policy"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

### Graphs der Carvan ownership über die subtypes
# 1 MOSTYPE Customer Subtype see L0
# 4 MGEMLEEF Avg age see L1
# 5 MOSHOOFD Customer main type see L2
# 6 MGODRK Roman catholic see L3

# --- Packages ---
library(tidyverse)
library(here)

# --- Load data ---
df <- read.csv(here("data/processed/ticdata2000_clean.csv"), check.names = FALSE)
stopifnot(all(c("CARAVAN","MOSTYPE","MGEMLEEF","MOSHOOFD","MGODRK") %in% names(df)))
df$CARAVAN <- as.integer(df$CARAVAN)

# --- Code -> Label maps -------------------------------------------------------
L0_map <- tibble::tribble(
  ~value, ~label,
  1, "High Income, expensive child",
  2, "Very Important Provincials",
  3, "High status seniors",
  4, "Affluent senior apartments",
  5, "Mixed seniors",
  6, "Career and childcare",
  7, "Dinki's (double income no kids)",
  8, "Middle class families",
  9, "Modern, complete families",
  10, "Stable family",
  11, "Family starters",
  12, "Affluent young families",
  13, "Young all american family",
  14, "Junior cosmopolitan",
  15, "Senior cosmopolitans",
  16, "Students in apartments",
  17, "Fresh masters in the city",
  18, "Single youth",
  19, "Suburban youth",
  20, "Etnically diverse",
  21, "Young urban have-nots",
  22, "Mixed apartment dwellers",
  23, "Young and rising",
  24, "Young, low educated",
  25, "Young seniors in the city",
  26, "Own home elderly",
  27, "Seniors in apartments",
  28, "Residential elderly",
  29, "Porchless seniors: no front yard",
  30, "Religious elderly singles",
  31, "Low income catholics",
  32, "Mixed seniors",
  33, "Lower class large families",
  34, "Large family, employed child",
  35, "Village families",
  36, "Couples with teens 'Married with children'",
  37, "Mixed small town dwellers",
  38, "Traditional families",
  39, "Large religous families",
  40, "Large family farms",
  41, "Mixed rurals"
)

L1_map <- tibble::tribble(
  ~value, ~label,
  1, "20–30 years",
  2, "30–40 years",
  3, "40–50 years",
  4, "50–60 years",
  5, "60–70 years",
  6, "70–80 years"
)

L2_map <- tibble::tribble(
  ~value, ~label,
  1, "Successful hedonists",
  2, "Driven Growers",
  3, "Average Family",
  4, "Career Loners",
  5, "Living well",
  6, "Cruising Seniors",
  7, "Retired and Religeous",
  8, "Family with grown ups",
  9, "Conservative families",
  10, "Farmers"
)

L3_map <- tibble::tribble(
  ~value, ~label,
  0, "0%",
  1, "1–10%",
  2, "11–23%",
  3, "24–36%",
  4, "37–49%",
  5, "50–62%",
  6, "63–75%",
  7, "76–88%",
  8, "89–99%",
  9, "100%"
)

# --- Helper: summarize + plot + save -----------------------------------------
summarise_caravan <- function(data, var, map_tbl) {
  data %>%
    mutate(.val = .data[[var]]) %>%
    group_by(.val) %>%
    summarise(
      owners = sum(CARAVAN == 1, na.rm = TRUE),
      total  = n(),
      rate   = owners / pmax(total, 1),
      .groups = "drop"
    ) %>%
    right_join(rename(map_tbl, .val = value), by = ".val") %>%
    mutate(
      owners = replace_na(owners, 0),
      total  = replace_na(total, 0),
      rate   = replace_na(rate, 0)
    )
}

plot_caravan <- function(sum_tbl, title, filename) {
  # order by owners desc
  sum_tbl <- sum_tbl %>% arrange(desc(owners))
  ymax <- max(sum_tbl$owners, na.rm = TRUE)
  p <- ggplot(sum_tbl, aes(x = reorder(label, owners), y = owners)) +
    geom_col() +
    geom_text(aes(label = paste0(owners, " (", scales::percent(rate, accuracy = 0.1), ")")),
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12)),
                       limits = c(0, ymax * 1.12)) +
    labs(
      title = title,
      subtitle = "Total Caravan owners (count) • label shows count and ownership rate",
      x = NULL, y = "Owners (count)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  
  print(p)
  
  # Save
  out_dir <- here("graphs_for_presentation")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  ggsave(file.path(out_dir, filename), plot = p, width = 9, height = 7, dpi = 300, bg = "white")
}

# --- Build summaries & plots --------------------------------------------------
# MOSTYPE (L0)
sum_L0 <- summarise_caravan(df, "MOSTYPE", L0_map)
plot_caravan(sum_L0, "Caravan ownership by MOSTYPE (L0)", "caravan_by_L0_MOSTYPE.png")

# MGEMLEEF (L1)
sum_L1 <- summarise_caravan(df, "MGEMLEEF", L1_map)
plot_caravan(sum_L1, "Caravan ownership by MGEMLEEF (L1: Avg age)", "caravan_by_L1_MGEMLEEF.png")

# MOSHOOFD (L2)
sum_L2 <- summarise_caravan(df, "MOSHOOFD", L2_map)
plot_caravan(sum_L2, "Caravan ownership by MOSHOOFD (L2: Main type)", "caravan_by_L2_MOSHOOFD.png")

# MGODRK (L3)
sum_L3 <- summarise_caravan(df, "MGODRK", L3_map)
plot_caravan(sum_L3, "Caravan ownership by MGODRK (L3: Roman catholic % bucket)", "caravan_by_L3_MGODRK.png")

### Age Bar hcart with two BARS


# --- Dual-axis horizontal bars: owners (count) + rate (%) ---
plot_caravan_L1_dual <- function(sum_tbl, title = "Caravan by MGEMLEEF (dual axis)", filename = NULL) {
  # Order brackets by owners (you can choose rate instead)
  sum_tbl <- sum_tbl %>% arrange(desc(owners))
  sum_tbl <- sum_tbl %>% mutate(label = factor(label, levels = rev(label)))  # top = largest
  
  # Scale factor so the tallest rate bar lines up visually with the tallest count bar
  max_count <- max(sum_tbl$owners, na.rm = TRUE)
  max_rate  <- max(sum_tbl$rate,   na.rm = TRUE)
  scale_factor <- ifelse(is.finite(max_rate) && max_rate > 0, max_count / max_rate, 1)
  
  # Long format with scaling applied to rate
  df_long <- bind_rows(
    sum_tbl %>% transmute(label, metric = "Owners (count)", value = owners),
    sum_tbl %>% transmute(label, metric = "Rate (%)",       value = rate * scale_factor)
  )
  
  # Positions for dodging (two bars side-by-side per bracket)
  pd <- position_dodge(width = 0.8)
  
  library(ggplot2)
  p <- ggplot(df_long, aes(x = label, y = value, fill = metric)) +
    geom_col(position = pd, width = 0.7) +
    # Text labels: counts on the count bars, % on the rate bars
    geom_text(
      data = df_long %>% filter(metric == "Owners (count)"),
      aes(label = scales::comma(round(value))), 
      position = pd, hjust = -0.1, size = 3.6
    ) +
    geom_text(
      data = df_long %>% filter(metric == "Rate (%)"),
      aes(label = scales::percent(value / scale_factor, accuracy = 0.1)),
      position = pd, hjust = -0.1, size = 3.6
    ) +
    coord_flip() +
    scale_y_continuous(
      name = "Owners (count)",
      labels = scales::comma,
      expand = expansion(mult = c(0, 0.12)),
      sec.axis = sec_axis(~ . / scale_factor,
                          name = "Caravan rate",
                          labels = scales::percent_format(accuracy = 0.1))
    ) +
    scale_fill_manual(
      values = c("Owners (count)" = "grey70", "Rate (%)" = "#8b0000")
    ) +
    labs(
      title = title,
      subtitle = "Two bars per age bracket • Left axis: counts (grey) • Right axis: rate (dark red)",
      x = NULL, y = NULL, fill = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "top"
    )
  
  print(p)
  
  # Optional save
  if (!is.null(filename)) {
    out_dir <- here::here("graphs_for_presentation")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    ggsave(file.path(out_dir, filename), plot = p, width = 9, height = 7, dpi = 300, bg = "white")
  }
}

# ---- Use it with your existing sum_L1 ----
plot_caravan_L1_dual(
  sum_tbl = sum_L1,
  title = "Caravan ownership by MGEMLEEF (counts vs rate)",
  filename = "caravan_by_L1_MGEMLEEF_dualaxis.png"
)
