# ---- Construct BH-adjusted results ----

bh_results <- table3_results %>%
  filter(
    specification == "combined_basic",
    uncertainty_type == "Robust SE"
  ) %>%
  transmute(
    variable,
    outcome,
    family = case_when(
      variable %in% c(
        "USNGSCH",
        "USESCH",
        "PRSCHA_1",
        "PRSCHA_2",
        "PRSCH_C"
      ) ~ "Program take-up and school sector",
      
      variable %in% c(
        "SCYFNSH",
        "INSCHL",
        "FINISH6",
        "FINISH7",
        "FINISH8",
        "REPT6",
        "REPT",
        "NREPT",
        "TOTSCYRS"
      ) ~ "Educational progression",
      
      TRUE ~ NA_character_
    ),
    estimate,
    std_error = uncertainty,
    z_statistic = estimate / std_error,
    p_value = 2 * stats::pnorm(
      abs(z_statistic),
      lower.tail = FALSE
    ),
    n
  ) %>%
  group_by(family) %>%
  mutate(
    p_value_bh = p.adjust(
      p_value,
      method = "BH"
    )
  ) %>%
  ungroup() %>%
  mutate(
    p_value_bh_all = p.adjust(
      p_value,
      method = "BH"
    ),
    significant_raw = p_value < 0.05,
    significant_bh = p_value_bh < 0.05,
    significant_bh_all = p_value_bh_all < 0.05
  )

# ---- Validate results ----

stopifnot(
  nrow(bh_results) == 14,
  n_distinct(bh_results$variable) == 14,
  all(is.finite(bh_results$estimate)),
  all(is.finite(bh_results$std_error)),
  all(between(bh_results$p_value, 0, 1)),
  all(bh_results$p_value_bh >= bh_results$p_value),
  all(bh_results$p_value_bh_all >= bh_results$p_value)
)


# ---- Create formatted results table ----

bh_table <- bh_results %>%
  transmute(
    Outcome = outcome,
    Family = family,
    Estimate = sprintf("%.3f", estimate),
    `Robust SE` = sprintf("%.3f", std_error),
    `Raw p-value` = format.pval(p_value, digits = 3, eps = 0.001),
    `BH p-value: within family` =
      format.pval(p_value_bh, digits = 3, eps = 0.001),
    `BH p-value: all outcomes` =
      format.pval(p_value_bh_all, digits = 3, eps = 0.001),
    `Survives family BH` = if_else(significant_bh, "Yes", "No")
  )

print(bh_table, width = Inf)


# ---- Export results ----

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

write_csv(
  bh_results,
  file.path(
    table_output_directory,
    "table3_bh_results.csv"
  )
)

write_csv(
  bh_table,
  file.path(
    table_output_directory,
    "table3_bh_results_formatted.csv"
  )
)

# ---- Prepare educational outcomes for plotting ----

plot_order <- c(
  "Highest grade completed",
  "Currently in school",
  "Finished 6th grade",
  "Finished 7th grade",
  "Finished 8th grade",
  "Repetitions of 6th grade",
  "Ever repeated after the lottery",
  "Total repetitions since the lottery",
  "Years in school since the lottery"
)

plot_data <- bh_results %>%
  filter(family == "Educational progression") %>%
  mutate(
    confidence_low = estimate - 1.96 * std_error,
    confidence_high = estimate + 1.96 * std_error,
    bh_status = if_else(
      significant_bh,
      "Survives BH correction",
      "Does not survive"
    ),
    bh_status = factor(
      bh_status,
      levels = c("Survives BH correction", "Does not survive")
    ),
    outcome = factor(outcome, levels = rev(plot_order))
  )


# ---- Create coefficient plot ----

bh_plot <- ggplot(
  plot_data,
  aes(x = estimate, y = outcome, color = bh_status)
) +
  geom_vline(
    xintercept = 0,
    color = "grey50",
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_errorbar(
    aes(xmin = confidence_low, xmax = confidence_high),
    orientation = "y",
    width = 0.15,
    linewidth = 0.8
  ) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "Survives BH correction" = "#1F5A94",
      "Does not survive" = "grey60"
    )
  ) +
  labs(
    title = "PACES Voucher Effects on Educational Progression",
    subtitle = paste(
      "Combined-sample estimates with basic controls;",
      "95% confidence intervals"
    ),
    x = "Estimated effect of winning the voucher lottery",
    y = NULL,
    color = NULL,
    caption = str_wrap(
      paste(
        "Color indicates significance after the within-family",
        "Benjamini-Hochberg correction. Confidence intervals are",
        "unadjusted, and effects are reported in each outcome's",
        "original units."
      ),
      width = 105
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(color = "grey40", hjust = 0)
  )

print(bh_plot)


# ---- Export coefficient plot ----

# ---- Export coefficient plot ----

ggsave(
  file.path(
    figure_output_directory,
    "table3_bh_coefficient_plot.png"
  ),
  plot = bh_plot,
  width = 11,
  height = 7.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(
    figure_output_directory,
    "table3_bh_coefficient_plot.pdf"
  ),
  plot = bh_plot,
  width = 11,
  height = 7.5,
  units = "in"
)