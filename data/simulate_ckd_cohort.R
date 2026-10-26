# simulate_ckd_cohort.R
# ---------------------------------------------------------------------------
# Simulates a SYNTHETIC cohort for teaching purposes. No real patient data.
# Context: community-based cohort, adults 40-85 years, screened for chronic
# kidney disease (CKD) at baseline and followed for up to 5 years.
#
# Output: data/ckd_kohorte.csv (one row per participant)
# Codebook: data/codebook.md
#
# Design choices (deliberately simple, but clinically plausible):
#  - eGFR declines with age and is lower with diabetes / hypertension
#  - albuminuria (UACR) is log-normal and higher with diabetes
#  - CKD at baseline  := eGFR < 60 OR UACR >= 30 mg/g  (KDIGO-style, simplified)
#  - a dipstick albumin test is simulated with fixed sensitivity/specificity
#    against the UACR reference (used in module 2 for 2x2 tables)
#  - incident CKD during follow-up among those free of CKD at baseline,
#    exponential time-to-event with covariate-dependent hazard
#
# Run from the project root:  Rscript data/simulate_ckd_cohort.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

set.seed(20261026)
n <- 2000

# --- Baseline characteristics ----------------------------------------------
alter <- round(pmin(pmax(rnorm(n, mean = 61, sd = 11), 40), 85))
geschlecht <- sample(c("w", "m"), n, replace = TRUE, prob = c(0.52, 0.48))

# Diabetes probability rises with age (logistic), ~15 % overall
p_dm <- plogis(-1.75 + 0.035 * (alter - 60))
diabetes <- rbinom(n, 1, p_dm)

# Hypertension ~45 % overall, more with age and diabetes
p_ht <- plogis(-0.4 + 0.045 * (alter - 60) + 0.6 * diabetes)
hypertonie <- rbinom(n, 1, p_ht)

# eGFR (mL/min/1.73 m^2): age-related decline, lower with DM / HT
egfr <- 100 - 0.75 * (alter - 40) - 7 * diabetes - 4 * hypertonie +
  rnorm(n, 0, 12)
egfr <- round(pmin(pmax(egfr, 8), 130), 1)

# UACR (mg/g): log-normal, higher with diabetes and hypertension
log_uacr <- 1.9 + 0.9 * diabetes + 0.35 * hypertonie +
  0.01 * (alter - 60) + rnorm(n, 0, 0.9)
uacr <- round(exp(log_uacr), 1)

# CKD at baseline (simplified KDIGO definition)
ckd_baseline <- as.integer(egfr < 60 | uacr >= 30)

# Dipstick albumin screening test vs. reference UACR >= 30 mg/g
# Fixed test characteristics: sensitivity 0.80, specificity 0.90
albuminurie_ref <- as.integer(uacr >= 30)
teststreifen_pos <- ifelse(
  albuminurie_ref == 1,
  rbinom(n, 1, 0.80),
  rbinom(n, 1, 1 - 0.90)
)

# --- Follow-up: incident CKD among those CKD-free at baseline ---------------
max_fu <- 5
# baseline hazard ~ 1.2 %/year, increased by age, DM, HT, lower eGFR
log_hazard <- log(0.012) + 0.03 * (alter - 60) + 0.8 * diabetes +
  0.4 * hypertonie - 0.02 * (egfr - 90)
t_event <- rexp(n, rate = exp(log_hazard))
# administrative censoring at 5 years + random loss to follow-up
t_censor <- pmin(max_fu, rexp(n, rate = 0.04))
followup_jahre <- round(pmin(t_event, t_censor), 2)
ckd_inzident <- as.integer(t_event <= t_censor)

# Participants with CKD at baseline are not at risk for *incident* CKD
followup_jahre[ckd_baseline == 1] <- NA_real_
ckd_inzident[ckd_baseline == 1] <- NA_integer_

kohorte <- tibble(
  id = sprintf("P%04d", seq_len(n)),
  alter,
  geschlecht,
  diabetes,
  hypertonie,
  egfr,
  uacr,
  ckd_baseline,
  teststreifen_pos,
  followup_jahre,
  ckd_inzident
)

dir.create("data", showWarnings = FALSE)
write_csv(kohorte, "data/ckd_kohorte.csv", na = "")

# --- Quick sanity checks (printed, not saved) --------------------------------
cat("n =", nrow(kohorte), "\n")
cat("CKD-Praevalenz (Baseline):", round(mean(kohorte$ckd_baseline), 3), "\n")
cat("Diabetes:", round(mean(kohorte$diabetes), 3),
    " Hypertonie:", round(mean(kohorte$hypertonie), 3), "\n")
risiko <- filter(kohorte, ckd_baseline == 0)
cat("Unter Risiko:", nrow(risiko),
    " inzidente Faelle:", sum(risiko$ckd_inzident),
    " Personenjahre:", round(sum(risiko$followup_jahre)), "\n")
cat("Inzidenzrate (pro 1000 PJ):",
    round(1000 * sum(risiko$ckd_inzident) / sum(risiko$followup_jahre), 1), "\n")
tab <- table(Test = kohorte$teststreifen_pos, Ref = albuminurie_ref)
cat("Teststreifen Sens:", round(tab["1", "1"] / sum(tab[, "1"]), 3),
    " Spez:", round(tab["0", "0"] / sum(tab[, "0"]), 3), "\n")
