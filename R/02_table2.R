# 02_table2.R
# Replicate Table 2: personal characteristics and voucher status

library(tidyverse)

# ---- Samples ----

panel_a_samples <- list(
  "Bogota 1995" = paces %>%
    filter(AGE2 >= 9, AGE2 <= 25, ID <= 4044),
  "Bogota 1997" = paces %>%
    filter(AGE2 >= 9, AGE2 <= 25, DBOGOTA == 1, D1997 == 1),
  "Jamundi 1993" = paces %>%
    filter(AGE2 >= 9, AGE2 <= 25, DJAMUNDI == 1),
  "Combined sample" = paces %>%
    filter(AGE2 >= 9, AGE2 <= 25, DBOGOTA == 1 | DJAMUNDI == 1)
)
panel_b_samples <- list(
  "Bogota 1995" = paces %>%
    filter(AGE2 >= 9, AGE2 <= 25, BOG95ASD == 1),
  "Bogota 1997" = paces %>%
    filter(AGE2 >= 9, AGE2 <= 25, BOG97ASD == 1),
  "Jamundi 1993" = paces %>%
    filter(AGE2 >= 9, AGE2 <= 25, JAM93ASD == 1),
  "Combined sample" = paces %>%
    filter(
      AGE2 >= 9, AGE2 <= 25,
      BOG95ASD == 1 | BOG97ASD == 1 | JAM93ASD == 1
    )
)
panel_c_samples <- list(
  "Bogota 1995" = paces %>% filter(BOG95SMP == 1),
  "Bogota 1997" = paces %>% filter(BOG97SMP == 1),
  "Jamundi 1993" = paces %>% filter(JAM93SMP == 1),
  "Combined sample" = paces %>%
    filter(BOG95SMP == 1 | BOG97SMP == 1 | JAM93SMP == 1),
  "Test-takers" = test_takers
)

# ---- Outcomes and controls ----

panel_ab_outcomes <- c("PHONE", "AGE2", "SEX_NAME")
panel_c_outcomes <- c(
  "AGE", "SEX2", "MOM_SCH", "DAD_SCH",
  "MOM_AGE", "DAD_AGE", "DAD_MW"
)
basic_controls <- c(
  "VOUCH0", "DBOGOTA", "DJAMUNDI", "D1993", "D1995", "D1997"
)
survey_controls <- c(
  "VOUCH0", "SVY", "HSVISIT", "DJAMUNDI", "DBOGOTA",
  "D1993", "D1995", "D1997",
  paste0("DMONTH", 1:12), paste0("DAREA", 1:19)
)
outcome_labels <- c(
  PHONE = "Has phone",
  AGE2 = "Age at time of application",
  SEX_NAME = "Male",
  AGE = "Age at time of survey",
  SEX2 = "Male",
  MOM_SCH = "Mother's highest grade completed",
  DAD_SCH = "Father's highest grade completed",
  MOM_AGE = "Mother's age",
  DAD_AGE = "Father's age",
  DAD_MW = "Father earns more than two minimum wages"
)

# The public file gives a test-taker loser mean of 0.452 for SEX2,
# versus 0.447 in the paper. SEX2 matches the authors' regression outcome.

# ---- Estimation ----

estimate_effect <- function(data, outcome, controls) {
  model_data <- data %>%
    select(all_of(c(outcome, controls))) %>%
    drop_na()
  
  if (n_distinct(model_data[[outcome]]) < 2) {
    return(tibble(
      voucher_effect = NA_real_,
      robust_se = NA_real_,
      model_n = nrow(model_data)
    ))
  }
  
  model <- lm(
    reformulate(controls, response = outcome),
    data = model_data
  )
  robust_vcov <- sandwich::vcovHC(model, type = "HC0")
  
  tibble(
    voucher_effect = unname(coef(model)["VOUCH0"]),
    robust_se = sqrt(unname(robust_vcov["VOUCH0", "VOUCH0"])),
    model_n = nobs(model)
  )
}
build_panel <- function(panel, samples, outcomes, controls) {
  imap_dfr(samples, function(data, sample) {
    map_dfr(outcomes, function(outcome) {
      loser_values <- data[[outcome]][
        which(data$VOUCH0 == 0 & !is.na(data[[outcome]]))
      ]
      
      estimate_effect(data, outcome, controls) %>%
        mutate(
          panel = panel,
          sample = sample,
          outcome = outcome,
          outcome_label = unname(outcome_labels[[outcome]]),
          outcome_order = match(outcome, outcomes),
          loser_mean = mean(loser_values),
          loser_sd = sd(loser_values),
          loser_n = length(loser_values),
          maximum_sample_n = nrow(data),
          .before = 1
        )
    })
  })
}
table2_results <- bind_rows(
  build_panel(
    "A. PACES application data",
    panel_a_samples,
    panel_ab_outcomes,
    basic_controls
  ),
  build_panel(
    "B. All attempted contacts",
    panel_b_samples,
    panel_ab_outcomes,
    basic_controls
  ),
  build_panel(
    "C. Survey data",
    panel_c_samples,
    panel_c_outcomes,
    survey_controls
  )
) %>%
  mutate(
    panel = factor(panel, levels = c(
      "A. PACES application data",
      "B. All attempted contacts",
      "C. Survey data"
    )),
    sample = factor(sample, levels = c(
      "Bogota 1995", "Bogota 1997", "Jamundi 1993",
      "Combined sample", "Test-takers"
    ))
  ) %>%
  arrange(panel, sample, outcome_order)

# ---- Validate, format, and export ----

expected_missing <- with(
  table2_results,
  panel == "B. All attempted contacts" &
    sample %in% c("Bogota 1995", "Bogota 1997") &
    outcome == "PHONE"
)
stopifnot(
  nrow(table2_results) == 59,
  all(is.finite(table2_results$loser_mean)),
  all(is.finite(table2_results$loser_sd)),
  all(is.na(table2_results$voucher_effect) == expected_missing),
  all(is.na(table2_results$robust_se) == expected_missing),
  all(is.finite(table2_results$voucher_effect[!expected_missing])),
  all(is.finite(table2_results$robust_se[!expected_missing])),
  all(table2_results$loser_n > 0),
  all(table2_results$model_n > 0)
)
continuous_outcomes <- c(
  "AGE2", "AGE", "MOM_SCH", "DAD_SCH", "MOM_AGE", "DAD_AGE"
)
table2_formatted <- table2_results %>%
  transmute(
    Panel = panel,
    Sample = sample,
    Outcome = outcome_label,
    `Loser mean` = if_else(
      outcome %in% continuous_outcomes,
      sprintf("%.1f", loser_mean),
      sprintf("%.3f", loser_mean)
    ),
    `Loser SD` = if_else(
      outcome %in% continuous_outcomes,
      sprintf("%.1f", loser_sd),
      ""
    ),
    `Voucher effect` = if_else(
      is.na(voucher_effect),
      "—",
      sprintf("%.3f", voucher_effect)
    ),
    `Robust SE` = if_else(
      is.na(robust_se),
      "",
      sprintf("%.3f", robust_se)
    ),
    `Loser N` = loser_n,
    `Model N` = model_n,
    `Maximum sample N` = maximum_sample_n
  )
write_csv(
  table2_results %>% select(-outcome_order),
  "output/tables/table2_results.csv",
  na = ""
)
write_csv(
  table2_formatted,
  "output/tables/table2_formatted.csv",
  na = ""
)
message(
  "Table 2 exports created: ",
  nrow(table2_results),
  " rows."
)