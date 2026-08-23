# 06_attrition_final.R: attrition diagnostics and IPW sensitivity analysis
library(tidyverse)
library(sandwich)
source("R/01_clean.R")
table_output_directory <- "output/tables"
figure_output_directory <- "output/figures"

dir.create(
  table_output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figure_output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)
# ---- Attempted-contact sample and response-rate tests ----
attrition_sample <- paces_raw %>%
  filter(BOG95ASD == 1 | BOG97ASD == 1 | JAM93ASD == 1) %>%
  mutate(cohort = case_when(
    BOG95ASD == 1 ~ "Bogota 1995",
    BOG97ASD == 1 ~ "Bogota 1997",
    JAM93ASD == 1 ~ "Jamundi 1993"
  ))
response_summary <- paces_raw %>%
  filter(!is.na(RESPONSE), !is.na(VOUCH0)) %>%
  summarise(
    attempted = n_distinct(ID), completed = sum(RESPONSE == 1),
    nonresponses = sum(RESPONSE == 0), response_rate = mean(RESPONSE)
  )
response_rates <- attrition_sample %>%
  group_by(cohort, VOUCH0) %>%
  summarise(
    attempted = n(), completed = sum(RESPONSE == 1),
    response_rate = mean(RESPONSE), .groups = "drop"
  )
robust_result <- function(model, term, specification) {
  se <- sqrt(diag(sandwich::vcovHC(model, type = "HC0")))[[term]]
  estimate <- coef(model)[[term]]
  z <- estimate / se
  tibble(
    specification, estimate = unname(estimate), std_error = unname(se),
    z_statistic = unname(z),
    p_value = 2 * pnorm(abs(z), lower.tail = FALSE), n = nobs(model)
  )
}
fit_response <- function(data, formula, specification) {
  robust_result(lm(formula, data = data), "VOUCH0", specification)
}
response_results <- bind_rows(
  fit_response(attrition_sample, RESPONSE ~ VOUCH0, "Unadjusted") %>%
    mutate(sample = "Combined", .before = 1),
  fit_response(
    attrition_sample, RESPONSE ~ VOUCH0 + factor(cohort), "Cohort-adjusted"
  ) %>% mutate(sample = "Combined", .before = 1),
  attrition_sample %>%
    group_by(cohort) %>%
    group_modify(~ fit_response(
      .x, RESPONSE ~ VOUCH0, "Within-cohort unadjusted"
    )) %>%
    ungroup() %>%
    rename(sample = cohort)
) %>% select(sample, everything())
# ---- Response model and inverse-probability weights ----
attrition_ipw <- attrition_sample %>%
  mutate(
    age_application = if_else(between(AGE2, 9, 25), AGE2, NA_real_),
    age_missing = as.integer(is.na(age_application)),
    sex_name_missing = as.integer(is.na(SEX_NAME)),
    age_application = replace_na(
      age_application, median(age_application, na.rm = TRUE)
    ),
    sex_name = replace_na(SEX_NAME, 0)
  )
response_model <- glm(
  RESPONSE ~ VOUCH0 * cohort + PHONE + age_application + age_missing +
    sex_name + sex_name_missing,
  family = binomial("logit"), data = attrition_ipw
)
attrition_ipw <- attrition_ipw %>%
  mutate(response_probability = predict(response_model, type = "response"))
respondents <- attrition_ipw %>%
  filter(RESPONSE == 1) %>%
  mutate(response_weight = 1 / response_probability)
weight_diagnostics <- respondents %>%
  summarise(
    respondents = n(), minimum = min(response_weight),
    p25 = quantile(response_weight, .25), median = median(response_weight),
    mean = mean(response_weight), p75 = quantile(response_weight, .75),
    maximum = max(response_weight), sum_of_weights = sum(response_weight),
    effective_sample_size = sum(response_weight)^2 / sum(response_weight^2)
  )
balance_variables <- tribble(
  ~variable,          ~characteristic,
  "PHONE",            "Applicant has phone",
  "age_application",  "Age at application",
  "age_missing",      "Age missing",
  "sex_name",         "Gender based on name",
  "sex_name_missing", "Gender missing"
)
balance_one <- function(variable, characteristic, treatment) {
  attempted <- filter(attrition_ipw, VOUCH0 == treatment)
  observed <- filter(respondents, VOUCH0 == treatment)
  attempted_mean <- mean(attempted[[variable]])
  observed_mean <- mean(observed[[variable]])
  attempted_sd <- sd(attempted[[variable]])
  tibble(
    variable, characteristic, VOUCH0 = treatment, attempted_mean,
    respondent_mean = observed_mean,
    ipw_mean = weighted.mean(observed[[variable]], observed$response_weight),
    respondent_smd = (observed_mean - attempted_mean) / attempted_sd,
    ipw_smd = (ipw_mean - attempted_mean) / attempted_sd
  )
}
balance_table <- balance_variables %>%
  crossing(treatment = c(0, 1)) %>%
  pmap_dfr(balance_one)
# ---- Original and IPW-adjusted outcome estimates ----
basic_controls <- c(
  "SVY", "HSVISIT", "DJAMUNDI", "PHONE", "AGE", "SEX2",
  paste0("STRATA", 1:6), "STRATAMS", "DBOGOTA",
  "D1993", "D1995", "D1997", paste0("DMONTH", 1:12), "SEX_MISS"
)
attrition_outcomes <- tribble(
  ~variable,  ~outcome,                              ~exclude_bog97,
  "SCYFNSH",  "Highest grade completed",             FALSE,
  "FINISH8",  "Finished 8th grade",                   TRUE,
  "REPT",     "Ever repeated after the lottery",     FALSE,
  "NREPT",    "Total repetitions since the lottery", FALSE,
  "TOTSCYRS", "Years in school since the lottery",   FALSE
)
compare_outcome <- function(variable, outcome, exclude_bog97) {
  data <- filter(respondents, TAB3SMPL == 1)
  if (exclude_bog97) data <- filter(data, BOG97SMP != 1)
  formula <- reformulate(c("VOUCH0", basic_controls), response = variable)
  bind_rows(
    robust_result(lm(formula, data), "VOUCH0", "Original unweighted"),
    robust_result(
      lm(formula, data, weights = response_weight), "VOUCH0", "Attrition IPW"
    )
  ) %>% mutate(variable = variable, outcome = outcome, .before = 1)
}
attrition_results <- pmap_dfr(attrition_outcomes, compare_outcome)
attrition_table <- attrition_results %>%
  mutate(model = if_else(
    specification == "Original unweighted", "unweighted", "ipw"
  )) %>%
  select(variable, outcome, model, estimate, std_error, p_value, n) %>%
  pivot_wider(
    names_from = model, values_from = c(estimate, std_error, p_value, n),
    names_glue = "{model}_{.value}"
  ) %>%
  mutate(estimate_change = ipw_estimate - unweighted_estimate)
# ---- Exports and coefficient plot ----
write_csv(
  response_results,
  file.path(
    table_output_directory,
    "attrition_response_tests.csv"
  )
)

write_csv(
  balance_table,
  file.path(
    table_output_directory,
    "attrition_weight_balance.csv"
  )
)

write_csv(
  attrition_table,
  file.path(
    table_output_directory,
    "attrition_ipw_results.csv"
  )
)
plot_data <- attrition_results %>%
  mutate(
    ci_lower = estimate - 1.96 * std_error,
    ci_upper = estimate + 1.96 * std_error,
    outcome = factor(outcome, levels = rev(attrition_outcomes$outcome)),
    specification = factor(
      specification, levels = c("Original unweighted", "Attrition IPW")
    )
  )
dodge <- position_dodge(.55)
attrition_plot <- ggplot(plot_data, aes(estimate, outcome, color = specification)) +
  geom_vline(xintercept = 0, color = "grey65", linetype = "dashed") +
  geom_errorbar(
    aes(xmin = ci_lower, xmax = ci_upper), orientation = "y",
    width = .16, position = dodge
  ) +
  geom_point(position = dodge, size = 2.8) +
  scale_color_manual(values = c(
    "Original unweighted" = "grey35", "Attrition IPW" = "#0072B2"
  )) +
  labs(
    title = "Original and Attrition-Weighted Voucher Effects",
    subtitle = "Points are estimates; bars are 95% confidence intervals",
    x = "Estimated effect in outcome-specific units", y = NULL, color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank())
ggsave(file.path(figure_output_directory, "attrition_ipw_coefficient_plot.png"),
       attrition_plot, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(figure_output_directory, "attrition_ipw_coefficient_plot.pdf"),
       attrition_plot, width = 9, height = 5.5)
# ---- Validation and displayed results ----
expected_files <- c(
  file.path(
    table_output_directory,
    c(
      "attrition_response_tests.csv",
      "attrition_weight_balance.csv",
      "attrition_ipw_results.csv"
    )
  ),
  file.path(
    figure_output_directory,
    c(
      "attrition_ipw_coefficient_plot.png",
      "attrition_ipw_coefficient_plot.pdf"
    )
  )
)
finish8_check <- attrition_results %>%
  filter(variable == "FINISH8", specification == "Original unweighted") %>%
  pull(estimate)
stopifnot(
  nrow(attrition_sample) == 2985, n_distinct(attrition_sample$ID) == 2985,
  sum(attrition_sample$RESPONSE == 1) == 1618, response_model$converged,
  all(is.finite(respondents$response_weight)), max(respondents$response_weight) < 10,
  weight_diagnostics$effective_sample_size > .90 * nrow(respondents),
  max(abs(balance_table$ipw_smd), na.rm = TRUE) < .10,
  nrow(attrition_results) == 10, abs(finish8_check - .0809) < .001,
  all(file.exists(expected_files))
)
print(attrition_table, n = Inf, width = Inf)
message("All attrition validation checks passed.")