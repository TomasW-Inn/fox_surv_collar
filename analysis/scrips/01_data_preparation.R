# =============================================================================
# Red Fox Survival Analysis
# Script 1: Data Preparation — Andersen-Gill Dataset
# =============================================================================
# This script reads the raw fox telemetry data (df_fox_new.csv), applies all
# cleaning and coding decisions, and produces the week-structured counting-
# process dataset ready for Cox PH modelling.
#
# KEY DECISIONS DOCUMENTED:
#   1. Ann (two collars, no interruption) → merged into one continuous record
#   2. Frans, Gunilla, Ingvar, Oskar, Cristina → two separate intervals (recapture)
#   3. Cristina1 (SA) and Cristina2 (AD) → different age class per interval
#   4. Foxes with < 14 days tracking excluded (Gunilla1, Källsvedan, Oskar2, Stiga)
#   5. CENS = 0 → right-censored; CENS = 1 → known mortality event
#   6. Fox year starts at ISO week 36 (early September)
#   7. SA reclassified to AD after ISO week 35 in the calendar year following capture
#   8. Season coded as continuous fox_week (1–52), fox week 1 = ISO week 36
# =============================================================================

library(dplyr)
library(lubridate)
library(tidyr)
library(here)

data <- "data/raw"
dat <- "data/processed"
script <- "analysis/scripts"
output <- "data/processed"

# ── 0. Load raw data ──────────────────────────────────────────────────────────
# Set working directory to folder containing df_fox_new.csv, or provide full path
df_raw <- read.csv(here(data, "df_fox_new.csv"), stringsAsFactors = FALSE)

df_raw$date.begin <- as.Date(df_raw$date.begin)
df_raw$date.end <- as.Date(df_raw$date.end)
df_raw$duration_days <- as.numeric(df_raw$date.end - df_raw$date.begin)

cat("Raw data: ", nrow(df_raw), "foxes\n")
cat("Known mortalities: ", sum(df_raw$CENS == 1), "\n\n")

# ── 1. Merge Ann1 + Ann2 (two collars, no interruption) ──────────────────────
# Ann1: 2013-06-25 to 2013-10-30  (CENS=0, collar battery)
# Ann2: 2013-12-26 to 2014-09-15  (CENS=0, collar battery)
# These represent the same individual tracked continuously; the 57-day gap is
# a collar-change period. Merged into one record using Ann1 begin, Ann2 end.

ann_merged <- df_raw %>%
  filter(FoxID == "Ann1") %>%
  mutate(
    FoxID = "Ann",
    date.end = df_raw$date.end[df_raw$FoxID == "Ann2"],
    CENS = df_raw$CENS[df_raw$FoxID == "Ann2"],
    CAUSE = df_raw$CAUSE[df_raw$FoxID == "Ann2"],
    duration_days = as.numeric(df_raw$date.end[df_raw$FoxID == "Ann2"] - date.begin)
  )

df <- df_raw %>%
  filter(!FoxID %in% c("Ann1", "Ann2")) %>%
  bind_rows(ann_merged)

cat("After Ann merge: ", nrow(df), "rows\n")

# ── 2. Exclude foxes tracked < 14 days ───────────────────────────────────────
# Following previous analyses; minimum 2-week threshold.
# Excluded: Gunilla1 (6d), Källsvedan (5d), Oskar2 (9d), Stiga (9d)
# Note: Gunilla2 and Oskar1 are retained as independent intervals.

excluded <- df %>% filter(duration_days < 14)
cat("Excluded (< 14 days tracked):\n")
print(excluded[, c("FoxID", "SEX", "AGE", "duration_days", "CENS")])

df <- df %>% filter(duration_days >= 14)
cat(
  "\nAfter exclusion: ", nrow(df), "foxes,",
  sum(df$CENS == 1), "known mortalities\n\n"
)

# ── 3. Build week-structured Andersen-Gill dataset ────────────────────────────
# Each fox contributes one row per calendar week present in the study.
# The last week per fox receives CENS = original CENS value (event or censored).
# All preceding weeks are CENS = 0 (still alive).
# Time variable: cumulative fox-week index (0-based start, 1-based end) for
# counting process format: Surv(sw, ew, cens).

build_fox_weeks <- function(fox_row) {
  begin <- fox_row$date.begin
  end <- fox_row$date.end

  # All Mondays from begin to end
  week_starts <- seq(
    from = begin + (7 - as.integer(format(begin, "%u")) %% 7) %% 7,
    to   = end,
    by   = "week"
  )
  # If no Monday falls in range, use begin date
  if (length(week_starts) == 0) week_starts <- begin

  n_weeks <- length(week_starts)

  data.frame(
    FoxID      = fox_row$FoxID,
    ID         = fox_row$ID,
    SEX        = fox_row$SEX,
    AGE_orig   = fox_row$AGE,
    AREA       = fox_row$AREA,
    CAUSE      = fox_row$CAUSE,
    CENS       = c(rep(0, n_weeks - 1), fox_row$CENS), # event only on last week
    week_start = week_starts,
    row_index  = seq_len(n_weeks) # 1-based week index within fox
  )
}

ag_list <- lapply(seq_len(nrow(df)), function(i) build_fox_weeks(df[i, ]))
ag <- bind_rows(ag_list)

cat("Fox-week dataset: ", nrow(ag), "rows,", sum(ag$CENS), "events\n")

# ── 4. Calendar variables ─────────────────────────────────────────────────────
ag$iso_week <- isoweek(ag$week_start)
ag$iso_year <- isoyear(ag$week_start)

# Fox-year week: ISO week 36 = fox week 1
ag$fox_week <- ifelse(ag$iso_week >= 36,
  ag$iso_week - 35,
  ag$iso_week + 17
)

# ── 5. SA → AD reclassification ───────────────────────────────────────────────
# Subadults become adults after ISO week 35 of the calendar year following
# their capture year. Only 2 foxes qualify: Cristina1 and Gijom.

ag <- ag %>%
  group_by(FoxID) %>%
  mutate(capture_year = min(iso_year)) %>%
  ungroup() %>%
  mutate(
    AGE = ifelse(
      AGE_orig == "SA" & iso_year > capture_year & iso_week > 35,
      "AD", AGE_orig
    )
  )

reclassified <- ag %>%
  filter(AGE != AGE_orig) %>%
  distinct(FoxID) %>%
  pull(FoxID)
cat("SA → AD reclassified during study:", paste(reclassified, collapse = ", "), "\n\n")

# ── 6. Counting process time variables ───────────────────────────────────────
# sw = start of interval (0-based), ew = end of interval (1-based)
ag <- ag %>%
  group_by(FoxID) %>%
  arrange(week_start, .by_group = TRUE) %>%
  mutate(
    cumulative_week = row_number(),
    sw = cumulative_week - 1L,
    ew = cumulative_week
  ) %>%
  ungroup()

# ── 7. Covariate coding ───────────────────────────────────────────────────────
# AGE_bin:      1 = SA (subadult), 0 = AD (adult)  — reference = adult
# SEX_bin:      1 = Male, 0 = Female               — reference = female
# season_linear: fox_week as continuous (1–52)      — best-fitting season coding
# season_binary: 1 = spring/summer (fox wk 18–52), 0 = autumn/winter
# season4:      4-level factor (autumn/winter/spring/summer)

ag <- ag %>%
  mutate(
    AGE_bin = as.integer(AGE == "SA"),
    SEX_bin = as.integer(SEX == "M"),
    season_linear = fox_week,
    season_binary = as.integer(fox_week >= 18),
    season4 = case_when(
      fox_week <= 13 ~ "autumn", # ~Sep–Nov (fox wk  1–13)
      fox_week <= 26 ~ "winter", # ~Dec–Feb (fox wk 14–26)
      fox_week <= 39 ~ "spring", # ~Mar–May (fox wk 27–39)
      TRUE ~ "summer" # ~Jun–Aug (fox wk 40–52)
    ),
    season4 = factor(season4, levels = c("autumn", "winter", "spring", "summer"))
  )

# ── 8. Final checks ───────────────────────────────────────────────────────────
stopifnot(all(ag$ew > ag$sw)) # valid intervals
stopifnot(all(ag$ew - ag$sw == 1)) # each interval = 1 week
stopifnot(all(ag %>% group_by(FoxID) %>%
  summarise(n_events = sum(CENS)) %>%
  pull(n_events) <= 1)) # max 1 event per fox

event_check <- ag %>%
  group_by(FoxID) %>%
  summarise(n_events = sum(CENS), .groups = "drop")
cat("Foxes with 0 events (censored):", sum(event_check$n_events == 0), "\n")
cat("Foxes with 1 event:            ", sum(event_check$n_events == 1), "\n")
cat(
  "Foxes with >1 event:           ", sum(event_check$n_events > 1),
  " (should be 0)\n\n"
)

cat("Events by SEX:\n")
print(table(ag$SEX[ag$CENS == 1]))
cat("Events by AGE:\n")
print(table(ag$AGE[ag$CENS == 1]))
cat("Events by season4:\n")
print(table(ag$season4[ag$CENS == 1]))

# ── 9. Save ───────────────────────────────────────────────────────────────────
write.csv(ag, here(dat, "fox_ag_dataset.csv"), row.names = FALSE)
cat("\nSaved: fox_ag_dataset.csv\n")
cat("Columns: ", paste(names(ag), collapse = ", "), "\n")
