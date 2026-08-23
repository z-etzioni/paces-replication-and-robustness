library(tidyverse)
library(sandwich)

source("R/01_clean.R")


# Samples
table3_sample <- paces %>%
  filter(TAB3SMPL == 1,
         BOG95SMP == 1 | BOG97SMP == 1 | JAM93SMP == 1)

table3_bog95 <- table3_sample %>%
  filter(BOG95SMP == 1)

table3_finish78 <- table3_sample %>%
  filter(BOG95SMP == 1 | JAM93SMP == 1)


# Separate dataset for "ever used a scholarship"
tab7 <- haven::read_sas("data/raw/tab7.sas7bdat")

tab7_required <- c(
  "SCYFNSH", "FINISH6", "PRSCHA_1", "REPT6", "NREPT", "SVY",
  "INSCHL", "FINISH7", "VOUCH0", "PRSCH_C", "FINISH8",
  "PRSCHA_2", "TOTSCYRS", "REPT"
)

table3_ever_sample <- tab7 %>%
  drop_na(all_of(tab7_required)) %>%
  filter(BOG95SMP == 1 | BOG97SMP == 1 | JAM93SMP == 1)

table3_ever_bog95 <- table3_ever_sample %>%
  filter(BOG95SMP == 1)


# Controls
basic_controls <- c(
  "SVY", "HSVISIT", "DJAMUNDI", "PHONE", "AGE", "SEX2",
  paste0("STRATA", 1:6), "STRATAMS", "DBOGOTA",
  "D1993", "D1995", "D1997",
  paste0("DMONTH", 1:12), "SEX_MISS"
)

barrio_controls <- c(basic_controls, paste0("DAREA", 1:19))


# Run one regression
fit_table3 <- function(data, outcome, controls = character()) {
  
  model <- lm(
    reformulate(c("VOUCH0", controls), response = outcome),
    data = data
  )
  
  robust_se <- sqrt(
    diag(sandwich::vcovHC(model, type = "HC0"))
  )[["VOUCH0"]]
  
  tibble(
    estimate = unname(coef(model)[["VOUCH0"]]),
    uncertainty = unname(robust_se),
    uncertainty_type = "Robust SE",
    n = nobs(model)
  )
}


# Outcomes and special sample rules
table3_outcomes <- tribble(
  ~variable,  ~label,                                 ~dataset, ~exclude_bog97,
  "USNGSCH",  "Using any scholarship in survey year", "main",   FALSE,
  "USESCH",   "Ever used a scholarship",              "tab7",   FALSE,
  "PRSCHA_1", "Started 6th grade in private school",  "main",   FALSE,
  "PRSCHA_2", "Started 7th grade in private school",  "main",   FALSE,
  "PRSCH_C",  "Currently in private school",          "main",   FALSE,
  "SCYFNSH",  "Highest grade completed",              "main",   FALSE,
  "INSCHL",   "Currently in school",                   "main",   FALSE,
  "FINISH6",  "Finished 6th grade",                    "main",   FALSE,
  "FINISH7",  "Finished 7th grade",                    "main",   TRUE,
  "FINISH8",  "Finished 8th grade",                    "main",   TRUE,
  "REPT6",    "Repetitions of 6th grade",             "main",   FALSE,
  "REPT",     "Ever repeated after the lottery",      "main",   FALSE,
  "NREPT",    "Total repetitions since the lottery",  "main",   FALSE,
  "TOTSCYRS", "Years in school since the lottery",    "main",   FALSE
)


# Generate all six columns for one outcome
build_table3_outcome <- function(
    variable,
    label,
    dataset,
    exclude_bog97
) {
  
  if (dataset == "tab7") {
    bogota_data <- table3_ever_bog95
    combined_data <- table3_ever_sample
  } else {
    bogota_data <- table3_bog95
    combined_data <- if (exclude_bog97) {
      table3_finish78
    } else {
      table3_sample
    }
  }
  
  loser_values <- bogota_data[[variable]][
    bogota_data[["VOUCH0"]] == 0
  ]
  
  loser_values <- loser_values[!is.na(loser_values)]
  
  bind_rows(
    "bogota_loser_mean" = tibble(
      estimate = mean(loser_values),
      uncertainty = sd(loser_values),
      uncertainty_type = "SD",
      n = length(loser_values)
    ),
    "bogota_no_controls" =
      fit_table3(bogota_data, variable),
    "bogota_basic" =
      fit_table3(bogota_data, variable, basic_controls),
    "bogota_barrio" =
      fit_table3(bogota_data, variable, barrio_controls),
    "combined_basic" =
      fit_table3(combined_data, variable, basic_controls),
    "combined_barrio" =
      fit_table3(combined_data, variable, barrio_controls),
    .id = "specification"
  ) %>%
    mutate(
      variable = .env$variable,
      outcome = .env$label,
      .before = 1
    )
}


# Estimate the full table
table3_results <- purrr::pmap_dfr(
  table3_outcomes,
  build_table3_outcome
)

stopifnot(nrow(table3_results) == 84)


# Create the paper-style wide version
table3_display <- table3_results %>%
  mutate(result = sprintf("%.3f (%.3f)", estimate, uncertainty)) %>%
  select(outcome, specification, result) %>%
  pivot_wider(
    names_from = specification,
    values_from = result
  )

print(table3_display, n = Inf, width = Inf)


# Save reproducible outputs
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

write_csv(
  table3_results,
  "output/tables/table3_results_long.csv"
)

write_csv(
  table3_display,
  "output/tables/table3_results_display.csv"
)

