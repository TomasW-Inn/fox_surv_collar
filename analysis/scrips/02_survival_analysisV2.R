# =============================================================================
# Red Fox Survival Analysis
# Script 2: Kaplan-Meier Curves and Cox Proportional Hazards Models
# =============================================================================
# Requires: fox_ag_dataset.csv  (produced by 01_data_preparation.R)
#
# ANALYSIS STRUCTURE:
#   Part A — Kaplan-Meier survival curves (visual inspection)
#   Part B — Andersen-Gill Cox PH models (age, sex, season)
#   Part C — Model comparison (AIC, concordance, LRT)
#   Part D — Proportional hazards assumption (Schoenfeld residuals)
#   Part E — Sex × Season interaction (power assessment)
# =============================================================================

library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(here)

data <- "data/raw"
dat <- "data/processed"
script <- "analysis/scripts"
output <- "data/processed"

# ── Load data ─────────────────────────────────────────────────────────────────
ag <- read.csv(here(dat, "fox_ag_dataset.csv"), stringsAsFactors = FALSE)
ag$AGE <- factor(ag$AGE, levels = c("AD", "SA"))
ag$SEX <- factor(ag$SEX, levels = c("F", "M"))
ag$season4 <- factor(ag$season4, levels = c("autumn", "winter", "spring", "summer"))

# Fox-level summary for KM (one row per fox, total duration + event)
fox_level <- ag %>%
  group_by(FoxID) %>%
  summarise(
    total_weeks = max(ew),
    event       = max(CENS),
    SEX         = first(SEX),
    AGE_orig    = first(AGE_orig),
    AREA        = first(AREA),
    .groups     = "drop"
  ) %>%
  mutate(
    AGE_orig = factor(AGE_orig, levels = c("AD", "SA"))
  )

cat("Fox-level summary:\n")
cat("  Foxes:", nrow(fox_level), "| Events:", sum(fox_level$event), "\n")
cat("  SEX distribution:\n")
print(table(fox_level$SEX))
cat("  AGE distribution:\n")
print(table(fox_level$AGE_orig))
cat("  AREA distribution:\n")
print(table(fox_level$AREA))
head(fox_level)

fox_level <-
  fox_level %>% mutate(
    total_weeks = ifelse(total_weeks > 50, 40, total_weeks)
  )
print(table(fox_level$total_weeks, fox_level$SEX))



# =============================================================================
# PART A — Kaplan-Meier curves
# =============================================================================

# ── A1. KM by Age class ───────────────────────────────────────────────────────
km_age <- survfit(Surv(total_weeks, event) ~ AGE_orig, data = fox_level)
cat("\nKM by Age:\n")
print(km_age)

# Log-rank test
lr_age <- survdiff(Surv(total_weeks, event) ~ AGE_orig, data = fox_level)
cat("\nLog-rank test (Age):\n")
print(lr_age)

# ── A2. KM by Sex ─────────────────────────────────────────────────────────────
km_sex <- survfit(Surv(total_weeks, event) ~ SEX, data = fox_level)
print(km_sex)
lr_sex <- survdiff(Surv(total_weeks, event) ~ SEX, data = fox_level)
cat("\nLog-rank test (Sex):\n")
print(lr_sex)

# ── A3. KM by Area ────────────────────────────────────────────────────────────
km_area <- survfit(Surv(total_weeks, event) ~ AREA, data = fox_level)

# ── A4. Combined KM plot ──────────────────────────────────────────────────────
p_age <- ggsurvplot(
  km_age,
  data = fox_level,
  palette = c("#2166ac", "#d6604d"),
  conf.int = TRUE,
  risk.table = TRUE,
  pval = TRUE,
  xlab = "Weeks in study",
  ylab = "Survival probability",
  title = "KM by Age class",
  legend.labs = c("Adult", "Subadult"),
  ggtheme = theme_classic()
)

p_sex <- ggsurvplot(
  km_sex,
  data = fox_level,
  palette = c("#d6604d", "#4393c3"),
  conf.int = TRUE,
  risk.table = TRUE,
  pval = TRUE,
  xlab = "Weeks in study",
  ylab = "Survival probability",
  title = "KM by Sex",
  legend.labs = c("Female", "Male"),
  ggtheme = theme_classic()
)


p_area <- ggsurvplot(
  km_area,
  data = fox_level,
  conf.int = TRUE,
  risk.table = TRUE,
  xlab = "Weeks in study",
  ylab = "Survival probability",
  title = "KM by Area (visual inspection)",
  ggtheme = theme_classic()
)

library(gridExtra)
km_plot <- grid.arrange(p_age$plot, p_sex$plot, ncol = 2)
names(p_sex)
km_plot2 <- grid.arrange(p_age$plot, p_sex$plot, p_age$table, p_sex$table, ncol = 2, heights = c(3, 1))
ggsave("KM_curves_combined2.pdf", km_plot2, width = 12, height = 6)
getwd()
# Save KM plots
pdf("KM_curves.pdf", width = 10, height = 7)
print(p_age)
print(p_sex)
print(p_area)
dev.off()
cat("\nSaved: KM_curves.pdf\n")

# =============================================================================
# PART B — Andersen-Gill Cox PH models
# =============================================================================
# Counting process format: Surv(sw, ew, CENS)
# cluster(FoxID) provides robust sandwich standard errors, accounting for
# multiple rows per individual (Andersen-Gill approach).
head(ag)
ag <- ag %>% filter(
  ew < 41
)


ag %>% filter(ew > 40)

# ── Model 1: Age only ─────────────────────────────────────────────────────────
m1 <- coxph(Surv(sw, ew, CENS) ~ AGE_bin,
  data    = ag,
  cluster = FoxID,
  ties    = "breslow"
)

# ── Model 2: Age + Sex ────────────────────────────────────────────────────────
m2 <- coxph(Surv(sw, ew, CENS) ~ AGE_bin + SEX_bin,
  data    = ag,
  cluster = FoxID,
  ties    = "breslow"
)

# ── Model 3: Age + Sex + Season (linear — best AIC) ──────────────────────────
m3 <- coxph(Surv(sw, ew, CENS) ~ AGE_bin + SEX_bin + season_linear,
  data    = ag,
  cluster = FoxID,
  ties    = "breslow"
)

# ── Model 4: Age + Sex + Season (binary — for comparison) ────────────────────
m4 <- coxph(Surv(sw, ew, CENS) ~ AGE_bin + SEX_bin + season_binary,
  data    = ag,
  cluster = FoxID,
  ties    = "breslow"
)

# ── Model 5: Age + Sex + Season (4-factor) ───────────────────────────────────
m5 <- coxph(Surv(sw, ew, CENS) ~ AGE_bin + SEX_bin + season4,
  data    = ag,
  cluster = FoxID,
  ties    = "breslow"
)

# Print summaries
for (label in c(
  "Model 1 — Age", "Model 2 — Age + Sex",
  "Model 3 — Age + Sex + Season (linear)",
  "Model 4 — Age + Sex + Season (binary)",
  "Model 5 — Age + Sex + Season (4-factor)"
)) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat(label, "\n")
}
cat("\nModel 1:\n")
print(summary(m1))
cat("\nModel 2:\n")
print(summary(m2))
cat("\nModel 3 (PREFERRED):\n")
print(summary(m3))
cat("\nModel 4:\n")
print(summary(m4))
cat("\nModel 5:\n")
print(summary(m5))

# =============================================================================
# PART C — Model comparison
# =============================================================================

extract_stats <- function(model, label, n_params) {
  ll <- model$loglik[2]
  aic <- -2 * ll + 2 * n_params
  con <- summary(model)$concordance["C"]
  data.frame(
    Model = label, AIC = round(aic, 2),
    Concordance = round(con, 3), Params = n_params
  )
}

comparison <- rbind(
  extract_stats(m1, "Age only", 1),
  extract_stats(m2, "Age + Sex", 2),
  extract_stats(m3, "Age + Sex + Season (linear)", 3),
  extract_stats(m4, "Age + Sex + Season (binary)", 3),
  extract_stats(m5, "Age + Sex + Season (4-factor)", 5)
)
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("MODEL COMPARISON\n")
print(comparison, row.names = FALSE)

# ── Likelihood Ratio Tests ────────────────────────────────────────────────────
# NOTE: anova() does not work on coxph models fitted with cluster = FoxID,
# because the robust sandwich variance adjustment invalidates the standard LRT.
# Solution: refit the same models WITHOUT cluster for LRT only, then extract
# log-likelihoods and compute the chi-square test manually.
# The cluster() argument only affects standard errors, NOT the partial
# log-likelihood itself, so point estimates and model fit statistics are identical.

m2_noclust <- coxph(Surv(sw, ew, CENS) ~ AGE_bin + SEX_bin,
  data = ag, ties = "breslow"
)
m3_noclust <- coxph(Surv(sw, ew, CENS) ~ AGE_bin + SEX_bin + season_linear,
  data = ag, ties = "breslow"
)

lrt_stat <- 2 * (m3_noclust$loglik[2] - m2_noclust$loglik[2])
lrt_df <- 1 # one additional parameter (season_linear)
lrt_p <- pchisq(lrt_stat, df = lrt_df, lower.tail = FALSE)

cat("\nLRT Model 2 vs Model 3 (adding season_linear):\n")
cat(sprintf("  Chi-square = %.3f, df = %d, p = %.4f\n", lrt_stat, lrt_df, lrt_p))
cat("  (LRT computed on models without cluster(); point estimates are identical)\n")

# =============================================================================
# PART D — Proportional Hazards assumption
# =============================================================================
# Schoenfeld residuals test on Model 3 (preferred model)
# A significant p-value indicates the PH assumption may be violated for that covariate.

ph_test <- cox.zph(m3, transform = "rank")
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("PROPORTIONAL HAZARDS ASSUMPTION TEST (Model 3)\n")
cat("Schoenfeld residuals, rank transform:\n")
print(ph_test)

# Plot Schoenfeld residuals
pdf("PH_assumption_schoenfeld.pdf", width = 10, height = 4)
par(mfrow = c(1, 3))
plot(ph_test, var = 1, main = "Schoenfeld: Age (SA)")
abline(h = 0, lty = 2, col = "red")
plot(ph_test, var = 2, main = "Schoenfeld: Sex (Male)")
abline(h = 0, lty = 2, col = "red")
plot(ph_test, var = 3, main = "Schoenfeld: Season (fox_week)")
abline(h = 0, lty = 2, col = "red")
dev.off()
cat("Saved: PH_assumption_schoenfeld.pdf\n")

# =============================================================================
# PART E — Sex × Season interaction (power assessment)
# =============================================================================
m3_int <- coxph(
  Surv(sw, ew, CENS) ~ AGE_bin + SEX_bin + season_linear +
    SEX_bin:season_linear,
  data = ag,
  cluster = FoxID,
  ties = "breslow"
)

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("SEX × SEASON INTERACTION MODEL\n")
print(summary(m3_int))


# LRT for interaction term (1 df) — use no-cluster refits (same reason as above)
m3_int_noclust <- coxph(
  Surv(sw, ew, CENS) ~ AGE_bin + SEX_bin + season_linear +
    SEX_bin:season_linear,
  data = ag, ties = "breslow"
)

lrt_int_stat <- 2 * (m3_int_noclust$loglik[2] - m3_noclust$loglik[2])
lrt_int_p <- pchisq(lrt_int_stat, df = 1, lower.tail = FALSE)
cat(sprintf(
  "\nLRT Model 3 vs Model 3 + interaction: chi2=%.3f, df=1, p=%.4f\n",
  lrt_int_stat, lrt_int_p
))

n_female_events <- sum(ag$CENS[ag$SEX == "F"])
n_male_events <- sum(ag$CENS[ag$SEX == "M"])
cat(sprintf("\nNote: only %d female and %d male events.\n", n_female_events, n_male_events))
cat("Rule of thumb: ~10 events per parameter; interaction model is underpowered.\n")

# =============================================================================
# PART F — Summary table for manuscript
# =============================================================================

make_coef_table <- function(model, model_name) {
  s <- summary(model)
  coef_table <- as.data.frame(s$coefficients)
  coef_table$HR <- round(exp(coef_table$coef), 3)
  coef_table$CI_lower <- round(s$conf.int[, "lower .95"], 3)
  coef_table$CI_upper <- round(s$conf.int[, "upper .95"], 3)
  coef_table$p <- round(coef_table[, "Pr(>|z|)"], 3)
  coef_table$Model <- model_name
  coef_table$Covariate <- rownames(coef_table)
  coef_table[, c("Model", "Covariate", "HR", "CI_lower", "CI_upper", "p")]
}

manuscript_table <- rbind(
  make_coef_table(m1, "Age only"),
  make_coef_table(m2, "Age + Sex"),
  make_coef_table(m3, "Age + Sex + Season (linear)")
)

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("MANUSCRIPT TABLE — Hazard Ratios\n")
print(manuscript_table, row.names = FALSE)

write.csv(manuscript_table, "HR_table.csv", row.names = FALSE)
cat("\nSaved: HR_table.csv\n")
