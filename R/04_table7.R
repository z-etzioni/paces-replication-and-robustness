# ============================================================
# TABLE 7: OLS AND 2SLS ESTIMATES
# ============================================================
library(tidyverse)
library(haven)

# ---- Load data and construct samples ----
# Table 7 uses the main survey data plus a separate dataset for test scores.
tab7_raw <- read_sas("data/raw/tab7.sas7bdat") %>% rename_with(tolower)
tab7_test_raw <- read_sas("data/raw/tab7test.sas7bdat") %>% rename_with(tolower)

# Reproduce the complete-case restrictions imposed by the authors' SAS program.
sample_variables <- c(
  "scyfnsh", "finish6", "prscha_1", "rept6", "nrept", "svy",
  "inschl", "finish7", "vouch0", "prsch_c", "finish8", "prscha_2",
  "totscyrs", "rept"
)
main_controls <- c(
  "svy", "hsvisit", "djamundi", "phone", "age", "sex2",
  paste0("strata", 1:6), "stratams", "dbogota", "d1993", "d1995", "d1997",
  paste0("dmonth", 1:12), "sex_miss"
)
test_controls <- c(
  paste0("tsite", 1:3), "svy", "hsvisit", "age", "sex", "mom_sch",
  paste0("strata", 1:6), "dad_sch", "mom_miss", "dad_miss"
)

tab7_complete <- tab7_raw %>%
  filter(if_all(all_of(sample_variables), ~ !is.na(.x)))

# Construct the main cohorts and the outcome-specific samples required by the paper.
samples <- list(
  bog95 = tab7_complete %>% filter(bog95smp == 1),
  combined = tab7_complete %>%
    filter(bog95smp == 1 | bog97smp == 1 | jam93smp == 1),
  finish8 = tab7_complete %>% filter(bog95smp == 1 | jam93smp == 1),
  test = tab7_test_raw %>% filter(!is.na(t_site), t_site > 0)
)

# Extract the treatment coefficient and its HC0 heteroskedasticity-robust standard error.
make_iv_formula <- function(outcome, controls) {
  as.formula(paste(
    outcome, "~", paste(c("usesch", controls), collapse = " + "),
    "|", paste(c("vouch0", controls), collapse = " + ")
  ))
}

# Estimate comparable OLS and 2SLS models using identical controls and observations.
extract_hc0 <- function(model, term = "usesch") {
  result <- lmtest::coeftest(
    model, vcov. = sandwich::vcovHC(model, type = "HC0")
  )
  c(
    estimate = unname(result[term, "Estimate"]),
    std_error = unname(result[term, "Std. Error"])
  )
}

# Combine loser summary statistics with the Bogotá 1995 and pooled-sample estimates.
fit_models <- function(data, outcome, controls) {
  ols <- lm(reformulate(c("usesch", controls), outcome), data = data)
  iv <- AER::ivreg(make_iv_formula(outcome, controls), data = data)
  ols_result <- extract_hc0(ols)
  iv_result <- extract_hc0(iv)
  c(
    ols_estimate = ols_result[["estimate"]],
    ols_se = ols_result[["std_error"]],
    iv_estimate = iv_result[["estimate"]],
    iv_se = iv_result[["std_error"]],
    n = nobs(ols)
  )
}

# Eighth-grade completion excludes Bogotá 1997; test scores have no pooled estimate.
make_table7_row <- function(outcome, bog95_data, controls, combined_data = NULL) {
  loser <- bog95_data %>%
    filter(vouch0 == 0, !is.na(.data[[outcome]])) %>%
    summarise(
      mean = mean(.data[[outcome]]),
      sd = sd(.data[[outcome]]),
      n = n()
    )
  bog95 <- fit_models(bog95_data, outcome, controls)
  combined <- if (is.null(combined_data)) {
    setNames(rep(NA_real_, 5), names(bog95))
  } else {
    fit_models(combined_data, outcome, controls)
  }
  tibble(
    outcome = outcome,
    loser_mean = loser$mean, loser_sd = loser$sd, loser_n = loser$n,
    bog95_ols_estimate = bog95[["ols_estimate"]],
    bog95_ols_se = bog95[["ols_se"]],
    bog95_iv_estimate = bog95[["iv_estimate"]],
    bog95_iv_se = bog95[["iv_se"]],
    combined_ols_estimate = combined[["ols_estimate"]],
    combined_ols_se = combined[["ols_se"]],
    combined_iv_estimate = combined[["iv_estimate"]],
    combined_iv_se = combined[["iv_se"]],
    bog95_n = bog95[["n"]], combined_n = combined[["n"]]
  )
}

# With one excluded instrument, the robust first-stage Wald F-statistic equals t².
table7_results <- bind_rows(
  map_dfr(
    c("scyfnsh", "inschl", "nrept"),
    ~ make_table7_row(.x, samples$bog95, main_controls, samples$combined)
  ),
  make_table7_row("finish8", samples$bog95, main_controls, samples$finish8),
  make_table7_row("totalpts", samples$test, test_controls),
  make_table7_row("married", samples$bog95, main_controls, samples$combined)
)

# ---- Robust first-stage diagnostics ----
summarise_first_stage <- function(data, controls, sample_name, outcome = NULL) {
  model_data <- data %>%
    filter(if_all(
      all_of(c("usesch", "vouch0", controls, outcome)), ~ !is.na(.x)
    ))
  model <- lm(
    reformulate(c("vouch0", controls), "usesch"), data = model_data
  )
  result <- lmtest::coeftest(
    model, vcov. = sandwich::vcovHC(model, type = "HC0")
  )
  t_value <- unname(result["vouch0", "t value"])
  tibble(
    sample = sample_name, n = nobs(model),
    estimate = unname(result["vouch0", "Estimate"]),
    robust_se = unname(result["vouch0", "Std. Error"]),
    robust_t = t_value, robust_f = t_value^2,
    p_value = unname(result["vouch0", "Pr(>|t|)"])
  )
}

table7_first_stage <- bind_rows(
  summarise_first_stage(samples$bog95, main_controls, "Bogota 1995"),
  summarise_first_stage(samples$combined, main_controls, "Combined"),
  summarise_first_stage(samples$finish8, main_controls, "Combined: finished eighth grade"),
  summarise_first_stage(samples$test, test_controls, "Test-score sample", "totalpts")
)

# Export raw estimates, first-stage diagnostics, and a presentation-ready version.
format_result <- function(estimate, std_error) {
  ifelse(is.na(estimate), "—", sprintf("%.3f (%.3f)", estimate, std_error))
}
table7_formatted <- table7_results %>%
  transmute(
    `Dependent variable` = recode(
      outcome,
      scyfnsh = "Highest grade completed",
      inschl = "In school",
      nrept = "Total repetitions since lottery",
      finish8 = "Finished eighth grade",
      totalpts = "Test scores (total points)",
      married = "Married or living with companion"
    ),
    `Loser mean (SD)` = format_result(loser_mean, loser_sd),
    `Bogota 1995 OLS` = format_result(bog95_ols_estimate, bog95_ols_se),
    `Bogota 1995 2SLS` = format_result(bog95_iv_estimate, bog95_iv_se),
    `Combined OLS` = format_result(combined_ols_estimate, combined_ols_se),
    `Combined 2SLS` = format_result(combined_iv_estimate, combined_iv_se)
  )

dir.create(
  "output/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  table7_results,
  "output/tables/table7_results.csv"
)

write_csv(
  table7_first_stage,
  "output/tables/table7_first_stage.csv"
)

write_csv(
  table7_formatted,
  "output/tables/table7_formatted.csv"
)