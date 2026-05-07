
### Suggested Results Section Structure

**1. Mortality causes** _(~1 paragraph)_ Open with the descriptive foundation. Report the total number of foxes monitored, total fox-weeks, and the 35 mortality events. Break down causes: harvesting dominated (n=22, 63%), followed by vehicle collision, stress/malnutrition, disease, and predation. Note the sex asymmetry — all vehicle and stress deaths were male, all disease deaths female — but flag that these numbers are very small and should be interpreted with caution.

**2. Survival curves — Kaplan-Meier** _(~1 paragraph + Figure)_ Present the KM curves visually and describe the observed patterns. Report the log-rank test results for age (p=0.043) and sex (p=0.095). Describe the shape: subadults show markedly lower early survival, converging toward adults over time. Note that the KM curves do not adjust for covariates and serve primarily as visual inspection tools.

**3. Model selection** _(~1 paragraph + Table)_ Report the AIC comparison across the five candidate models. State that the model including age, sex, and season as a continuous linear fox-week covariate provided the best fit (lowest AIC, concordance = 0.72). Note that the binary season coding performed worst, and that adding season improved concordance substantially over the age+sex base model. A table of model names, AIC, concordance, and number of parameters works well here.

**4. Cox PH model results — preferred model** _(~1–2 paragraphs + Table)_ Report the hazard ratios with 95% CIs and p-values from the preferred model (age + sex + season linear). Frame around three main findings:

- **Age:** Subadults had approximately twice the hazard of adults (HR~1.9, CI 0.88–4.2), though the confidence interval overlapped 1, reflecting low precision
- **Sex:** Males had elevated hazard relative to females (HR~1.8, CI 0.73–4.6), similarly imprecise
- **Season:** Hazard declined as the fox year progressed (HR per fox-week ~0.96), suggesting autumn/early winter is the most dangerous period

Explicitly acknowledge that wide confidence intervals reflect the limited number of events (n=35) and that conclusions about direction of effects are more reliable than conclusions about magnitude.

**5. Proportional hazards assumption** _(1–2 sentences)_ State that the PH assumption was satisfied for all covariates in the preferred model (Schoenfeld residuals, all p > 0.4), supporting the validity of the CPH framework.

**6. Sex × season interaction** _(1–2 sentences)_ Report the LRT result (chi²=1.10, df=1, p=0.295) and state that the interaction did not improve model fit, consistent with insufficient power to detect a sex-specific seasonal pattern given only nine female mortality events.

---

#### Suggested figure/table set

- **Figure 1:** KM curves by age class and by sex (two panels)
- **Figure 2:** Seasonal distribution of deaths by sex (bar chart with monthly x-axis)
- **Table 1:** Model comparison (AIC, concordance, parameters)
- **Table 2:** Hazard ratios from preferred model (HR, 95% CI, p)

---

This flows from descriptive → visual → model selection → inference → assumption checking, which is the standard progression for this type of analysis. When you come back with the actual model output numbers I can draft the full text.