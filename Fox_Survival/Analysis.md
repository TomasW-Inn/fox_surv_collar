#### Script 1 — `01_data_preparation.R`

Reads `df_fox_new.csv` and produces `fox_ag_dataset.csv`. Handles all the decisions we've made: Ann merger, recapture intervals, <14-day exclusions, SA→AD reclassification, fox-year week coding, and all covariate encodings. Every decision has an explanatory comment for the methods section.

#### Script 2 — `02_survival_analysis.R`

Reads `fox_ag_dataset.csv` and runs:

- Kaplan-Meier curves with log-rank tests (by age, sex, area) → `KM_curves.pdf`
- Five Cox PH models (age / age+sex / +season linear / +season binary / +season 4-factor)
- AIC and concordance comparison table
- Proportional hazards test via `cox.zph()` (Schoenfeld residuals) → `PH_assumption_schoenfeld.pdf`
- Sex×season interaction test with explicit power note
- Hazard ratio table ready for the manuscript → `HR_table.csv`

#### Script 3 — `03_mortality_causes.R`

Reads raw data directly and produces all descriptive mortality tables (cause × sex, cause × age, cause × area) plus three figures → `mortality_causes.pdf`

---

**Required R packages:** `survival`, `survminer`, `dplyr`, `tidyr`, `ggplot2` — all standard. You can install any missing ones with `install.packages(c("survminer"))`.
