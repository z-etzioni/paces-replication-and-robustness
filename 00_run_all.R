# Run the complete PACES replication and extensions

analysis_scripts <- c(
  "R/01_clean.R",
  "R/02_table2.R",
  "R/03_table3.R",
  "R/04_table7.R",
  "R/05_bh_correction.R",
  "R/06_attrition_ipw.R"
)

missing_scripts <- analysis_scripts[
  !file.exists(analysis_scripts)
]

if (length(missing_scripts) > 0) {
  stop(
    "Missing analysis scripts: ",
    paste(missing_scripts, collapse = ", ")
  )
}

dir.create(
  "output/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "output/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

for (script in analysis_scripts) {
  message("\nRunning ", script)
  
  withCallingHandlers(
    source(script),
    warning = function(w) {
      known_hc0_warning <- startsWith(
        conditionMessage(w),
        "HC0 covariances become (close to) singular"
      )
      
      if (known_hc0_warning) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

message(
  "\nAnalysis complete. Results saved in output/tables ",
  "and output/figures."
)