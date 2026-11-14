##############################################################################
# 00_check_project.R
# Pre-flight checks only. This script does NOT fit any model.
##############################################################################

suppressPackageStartupMessages({
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required. Run 00_install_packages.R first.")
  }
})

required_files <- c(
  "BrainNormR_v5_FINAL_PUBLIC.R",
  "BrainNormR_v5_FINAL_PUBLIC_ALL_ONLY.R",
  "BrainNormR_v5_FINAL_PUBLIC_THREE_GROUP.R",
  "11_export_model_equations.R",
  file.path("Config", "BrainNormR_mode_config.R"),
  "01_run_structural_atlas.R",
  "02_run_cognitive_normative.R",
  "03_biomarker_module.R",
  "04_make_thesis_outputs.R",
  "05_run_single_outcome.R",
  "00_build_grouped_structural_dataset.R",
  "06_run_grouped_structural_models.R",
  "07_plot_5measures_one_region.R",
  "08_plot_5measures_batch.R",
  "09_make_publication_figure_suite.R",
  "10_final_pipeline_audit.R",
  "15_run_normative_deviation_profiles.R",
  file.path("Config", "Deviation_profile_config.R"),
  file.path("Config", "DK68_structural_config.csv"),
  file.path("Config", "Cognition_Config.csv")
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files)) {
  stop(
    "Missing project file(s):\n",
    paste(missing_files, collapse = "\n")
  )
}


# Validate the dual-mode switch without fitting any model.
mode_env <- new.env(parent = emptyenv())
tryCatch(
  sys.source(file.path("Config", "BrainNormR_mode_config.R"), envir = mode_env),
  error = function(e) stop("Could not read Config/BrainNormR_mode_config.R: ", conditionMessage(e))
)
if (!exists("BRAINNORMR_MODE", envir = mode_env, inherits = FALSE)) {
  stop("BRAINNORMR_MODE is missing from Config/BrainNormR_mode_config.R")
}
mode_value <- toupper(trimws(as.character(mode_env$BRAINNORMR_MODE)[1]))
if (!mode_value %in% c("ALL_ONLY", "THREE_GROUP")) {
  stop("Invalid BRAINNORMR_MODE: ", mode_value, ". Use ALL_ONLY or THREE_GROUP.")
}
message("[MODE] ", mode_value)

if (!dir.exists("Data")) stop("Missing Data/ folder.")
if (!dir.exists("Results")) dir.create("Results", recursive = TRUE)
if (!dir.exists("DerivedData")) dir.create("DerivedData", recursive = TRUE)

data_files <- list.files(
  "Data",
  pattern = "\\.(xlsx|xls)$",
  full.names = TRUE,
  ignore.case = TRUE
)

data_files <- data_files[!grepl("^~\\$", basename(data_files))]

if (length(data_files) == 0L) {

  message(
    "[DATA] No Excel modelling dataset is present yet. ",
    "Add exactly one .xlsx/.xls file to Data/."
  )

} else if (length(data_files) > 1L) {

  stop(
    "[DATA] More than one Excel dataset found. ",
    "Keep exactly one modelling dataset in Data/:\n",
    paste(basename(data_files), collapse = "\n")
  )

} else {

  message("[DATA] ", data_files[1])

  header <- names(
    readxl::read_excel(
      data_files[1],
      sheet = 1,
      n_max = 1
    )
  )

  base_required <- c("Age", "sex")
  missing_base <- setdiff(base_required, header)

  if (length(missing_base)) {
    warning(
      "Missing essential base column(s): ",
      paste(missing_base, collapse = ", ")
    )
  }

  structural <- read.csv(
    file.path("Config", "DK68_structural_config.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  cognition <- read.csv(
    file.path("Config", "Cognition_Config.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  message("[CONFIG] Structural rows: ", nrow(structural))
  message("[CONFIG] Structural ROIs: ", length(unique(structural$ROI)))
  message("[CONFIG] Measures: ", paste(unique(structural$Measure), collapse = ", "))
  message("[CONFIG] Cognitive rows: ", nrow(cognition))

  message(
    sprintf(
      "[MATCH] Structural columns present: %d/%d",
      sum(structural$Column %in% header),
      nrow(structural)
    )
  )

  message(
    sprintf(
      "[MATCH] Cognitive columns present: %d/%d",
      sum(cognition$Column %in% header),
      nrow(cognition)
    )
  )

  if ("Volume" %in% structural$Measure || "Area" %in% structural$Measure) {
    if (!"eTIV" %in% header) {
      warning("eTIV is missing, but Volume/Area models require eTIV adjustment.")
    }
  }

  if (!"edu" %in% header) {
    warning("edu is missing; cognitive normative models require education adjustment.")
  }
}

message("\nPre-flight check finished. No model was fitted.")
