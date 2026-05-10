# =============================================================================
# Red Fox Survival Analysis
# Script 3: Mortality Cause Descriptive Analysis
# =============================================================================
# Requires: df_fox_new.csv  (raw data)
#
# Produces:
#   - Frequency tables of mortality causes by sex, age, area
#   - Proportional breakdown for comparison with published studies
#   - Figures: stacked bar chart and seasonal distribution of deaths
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)


data <- "data/raw"
dat <- "data/processed"
script <- "analysis/scripts"
output <- "data/processed"

# ── Load and prepare ──────────────────────────────────────────────────────────
df_raw <- read.csv(here(data, "df_fox_new.csv"), stringsAsFactors = FALSE)

df_raw$date.begin <- as.Date(df_raw$date.begin)
df_raw$date.end <- as.Date(df_raw$date.end)
df_raw$duration_days <- as.numeric(df_raw$date.end - df_raw$date.begin)

# Apply same exclusions as in 01_data_preparation.R
# Merge Ann
ann_merged <- df_raw[df_raw$FoxID == "Ann1", ]
ann_merged$FoxID <- "Ann"
ann_merged$date.end <- df_raw$date.end[df_raw$FoxID == "Ann2"]
ann_merged$CENS <- df_raw$CENS[df_raw$FoxID == "Ann2"]
ann_merged$CAUSE <- df_raw$CAUSE[df_raw$FoxID == "Ann2"]
ann_merged$duration_days <- as.numeric(ann_merged$date.end - ann_merged$date.begin)

df <- df_raw %>%
  filter(!FoxID %in% c("Ann1", "Ann2")) %>%
  bind_rows(ann_merged) %>%
  filter(duration_days >= 14)

# Cause labels (CAUSE codes 2 and 7 were never assigned)
cause_labels <- c(
  "1" = "Harvested",
  "3" = "Vehicle collision",
  "4" = "Disease",
  "5" = "Predation",
  "6" = "Stress / Malnutrition",
  "9" = "Censored (collar/battery)"
)
df$CAUSE_label <- cause_labels[as.character(df$CAUSE)]

# Known mortalities only
mort <- df %>% filter(CENS == 1)
cat("Total foxes in analysis:", nrow(df), "\n")
cat("Known mortalities:      ", nrow(mort), "\n\n")

# ── Table 1: Overall cause proportions ───────────────────────────────────────
cat(paste(rep("=", 50), collapse = ""), "\n")
cat("TABLE 1: Mortality causes (n =", nrow(mort), ")\n")
cause_summary <- mort %>%
  count(CAUSE_label) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))
print(cause_summary, row.names = FALSE)

# ── Table 2: Cause × Sex ─────────────────────────────────────────────────────
cat("\nTABLE 2: Cause by Sex\n")
print(table(mort$CAUSE_label, mort$SEX))

cat("\nRow proportions (% within cause):\n")
print(round(100 * prop.table(table(mort$CAUSE_label, mort$SEX), margin = 1), 1))

# ── Table 3: Cause × Age ─────────────────────────────────────────────────────
cat("\nTABLE 3: Cause by Age class\n")
print(table(mort$CAUSE_label, mort$AGE))

# ── Table 4: Cause × Area ────────────────────────────────────────────────────
cat("\nTABLE 4: Cause by Area\n")
print(table(mort$CAUSE_label, mort$AREA))

head(mort)
df2 <- mort %>%
  group_by(SEX, AGE, CAUSE_label) %>%
  summarise(
    n = n()
  ) %>%
  arrange(SEX, AGE, desc(n))
head(df2)

# ── Table 5: Sample sizes ─────────────────────────────────────────────────────
cat("\nTABLE 5: Sample composition\n")
cat("Sex × Age:\n")
print(addmargins(table(df$SEX, df$AGE)))
cat("\nSex × Area:\n")
print(addmargins(table(df$SEX, df$AREA)))


# ── Figure 1: Mortality causes stacked bar by sex ────────────────────────────
cause_order <- c(
  "Harvested", "Vehicle collision", "Stress / Malnutrition",
  "Disease", "Predation"
)
mort_plot <- mort %>%
  filter(CAUSE_label != "Censored (collar/battery)") %>%
  mutate(CAUSE_label = factor(CAUSE_label, levels = rev(cause_order)))

p1 <- ggplot(mort_plot, aes(x = SEX, fill = CAUSE_label)) +
  geom_bar(position = "stack", width = 0.6) +
  scale_fill_manual(values = c(
    "Harvested"              = "#4393c3",
    "Vehicle collision"      = "#d6604d",
    "Stress / Malnutrition"  = "#f4a582",
    "Disease"                = "#92c5de",
    "Predation"              = "#2166ac"
  )) +
  labs(
    title = "Mortality causes by sex",
    x = "Sex", y = "Number of deaths", fill = "Cause"
  ) +
  theme_classic(base_size = 13) +
  theme(legend.position = "right")

p2 <- ggplot(mort_plot, aes(x = AGE, fill = CAUSE_label)) +
  geom_bar(position = "stack", width = 0.6) +
  scale_fill_manual(values = c(
    "Harvested"              = "#4393c3",
    "Vehicle collision"      = "#d6604d",
    "Stress / Malnutrition"  = "#f4a582",
    "Disease"                = "#92c5de",
    "Predation"              = "#2166ac"
  )) +
  labs(
    title = "Mortality causes by age class",
    x = "Age class", y = "Number of deaths", fill = "Cause"
  ) +
  theme_classic(base_size = 13) +
  theme(legend.position = "right")

# ── Figure 2: Seasonal distribution of deaths ────────────────────────────────
# Attach fox_week to mortality rows using date.end
isoweek_r <- function(date) as.integer(format(date, "%V"))
mort$iso_week <- isoweek_r(mort$date.end)
mort$fox_week <- ifelse(mort$iso_week >= 36,
  mort$iso_week - 35,
  mort$iso_week + 17
)

month_breaks <- c(1, 5, 9, 14, 18, 22, 27, 31, 35, 40, 44, 48)
month_labels <- c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")

p3 <- ggplot(
  mort_plot %>%
    mutate(
      iso_week = isoweek_r(as.Date(date.end)),
      fox_week = ifelse(iso_week >= 36, iso_week - 35, iso_week + 17)
    ),
  aes(x = fox_week, fill = SEX)
) +
  geom_bar(width = 1) +
  annotate("rect",
    xmin = 14, xmax = 26, ymin = -Inf, ymax = Inf,
    alpha = 0.1, fill = "#2776b6"
  ) +
  annotate("text",
    x = 20, y = Inf, label = "Winter", vjust = 1.5,
    size = 3.5, colour = "#2b6da7"
  ) +
  scale_fill_manual(values = c("F" = "#808080", "M" = "#272626")) +
  scale_x_continuous(breaks = month_breaks, labels = month_labels) +
  labs(
    title = "Seasonal distribution of deaths by sex",
    subtitle = "Fox week 1 = ISO week 36 (~early September); shaded = winter (Dec–Feb)",
    x = "Approximate calendar month", y = "Number of deaths", fill = "Sex"
  ) +
  theme_classic(base_size = 13)

pdf("mortality_causes.pdf", width = 10, height = 6)
print(p1)
print(p2)
print(p3)
dev.off()
cat("\nSaved: mortality_causes.pdf\n")

pdf("mortality_seasonal.pdf", width = 10, height = 6)
print(p3)
dev.off()


# ── Tracking duration summary ─────────────────────────────────────────────────
cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("TRACKING DURATION (days)\n")
cat("All foxes:\n")
print(summary(df$duration_days))
cat("\nBy CENS status:\n")
print(tapply(df$duration_days, df$CENS, summary))

# ── Study period ──────────────────────────────────────────────────────────────
cat(sprintf(
  "\nStudy period: %s to %s\n",
  min(df$date.begin), max(df$date.end)
))
cat(sprintf(
  "Duration: %.1f years\n",
  as.numeric(max(df$date.end) - min(df$date.begin)) / 365.25
))

# Create data
data <- matrix(c(16, 6, 26, 9),
  nrow = 2,
  dimnames = list(Group = c("Male", "Female"), Outcome = c("Harvested", "Other"))
)
data
# Perform Fisher's Exact Test
result <- fisher.test(data)
print(result)
