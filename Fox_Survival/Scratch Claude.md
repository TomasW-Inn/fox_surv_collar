	# 5 maj
#### Sample composition

- **132 foxes** tracked 2011–2019, across 4 areas: Kolmården (n=88), Hedmark (n=20), Hedemora (n=14), Grimsö (n=10)
- 79 males / 53 females; 78 adults / 54 subadults
- **35 known mortalities** (CENS=1), **97 right-censored** (CENS=0, all CAUSE=9 battery/collar)


#### Mortality breakdown (n=35)


|Cause|n|%|
|---|---|---|
|Harvested|22|63%|
|Vehicle|5|14%|
|Stress/Malnutrition|4|11%|
|Disease|3|9%|
|Predation|1|3%|
Notable patterns: all vehicle deaths and stress/malnutrition were **males**; all disease deaths were **females**. Harvesting was the dominant cause across all areas, ages, and sexes.

#### Items flagging for decisions before modeling

**1. Recaptures (6 individuals):** Ann, Cristina, Frans, Gunilla, Ingvar, Oskar all appear with a "2" suffix — these are the same animal recollared after collar loss. The gap between first and second capture period needs checking (the name-matching didn't find the originals, suggesting the originals may be named slightly differently in the data — e.g. "Ann1" vs "Ann"). These animals need to be treated as **two independent intervals** in the Andersen-Gill model, which it handles naturally.

**2. Very short tracking periods (<14 days):** Four foxes (Gunilla1, Källsvedan, Oskar2, Stiga) were tracked for only 5–9 days, all censored. Your previous analysis excluded subadults contributing fewer than 2 weeks — **do you want to apply a minimum duration threshold** (e.g. <2 weeks) for all foxes, or only subadults?

**3. Area imbalance:** Kolmården dominates (67% of foxes). With only 10 foxes in Grimsö, **area as a covariate may be low-powered** — worth discussing whether to pool some areas.

**4. Event count (n=35):** This is the key constraint for modeling. A general rule of thumb is ~10 events per covariate. With 35 events, you can reliably support **3 covariates** (e.g. age, sex, season) — consistent with the previous approach.

---### Dataset construction

The Andersen-Gill dataset contains **2,652 fox-weeks** from **128 foxes** (4 excluded for <14 days), with **35 known mortality events**. Each fox contributes one row per week present; the final week is either a known death (CENS=1) or censored. Robust sandwich standard errors (cluster by FoxID) account for multiple rows per individual. Only 2 subadults (Cristina1, Gijom) were reclassified to adult — both had stayed in the study past week 35 of their second year.

### Model results

|Model|Covariates|Partial AIC|Concordance|LR-test p|
|---|---|---|---|---|
|1|Age|307.5|0.585|0.046|
|**2**|**Age + Sex**|**306.8**|**0.616**|**0.036**|
|3|Age + Sex + Season|308.6|0.627|0.079|

**Best model by AIC is Model 2 (Age + Sex).** Adding season does not improve fit — note this contrasts with the preliminary analysis, likely because the season variable here encodes the _last_ season of each fox rather than time-varying season across the study.
Key hazard ratios from Model 2:

- **Subadults: HR = 1.93** (95% CI 0.88–4.19, p=0.099) — ~twice the hazard of adults, though wide CI
- **Males: HR = 1.83** (95% CI 0.73–4.61, p=0.197) — elevated but imprecise

**Proportional hazards assumption:** All three Schoenfeld residual tests pass (all ρ < 0.14, all p > 0.4). No time-varying violations detected.

### Conclusion on season

**Age + Sex + Season (linear fox_week)** is the best-supported model. It is parsimonious, improves concordance substantially (0.62→0.72), and the AIC is lowest among single-df season additions. The sex×season interaction is biologically interesting but the data are simply not strong enough to estimate it — exactly as you anticipated. This is worth a sentence in the methods/discussion.