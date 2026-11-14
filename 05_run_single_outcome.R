##############################################################################
# 05_run_single_outcome.R
# BrainNormR v5.0 - Single-outcome runner
#
# Purpose:
# Run exactly one structural or cognitive outcome with the same frozen core
# used by the batch wrappers.
#
# IMPORTANT:
# - Edit only the USER SETTINGS section below.
# - Do not duplicate or edit the statistical engine here.
# - The actual modelling is performed by BrainNormR_v5_FINAL_PUBLIC.R.
##############################################################################

suppressPackageStartupMessages({
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Install package: readxl")
  }
})

# ============================================================================
# USER SETTINGS
# ============================================================================

CORE_FILE <- "BrainNormR_v5_FINAL_PUBLIC.R"
DATA_DIR  <- "Data"
ROOT_OUT  <- "Results"
SHEET_NAME <- 1

# ---- Outcome to analyse ----
# Examples:
# Structural FD:       "lh_insula_FD"
# Structural Volume:   "lh_insula_volume"
# Cognitive outcome:   "RAVLT_A1"
OUTCOME_COL <- "lh_insula_FD"

# Allowed:
# "FD", "Thickness", "Curvature", "Volume", "Area", "Cognition", "Other"
OUTCOME_TYPE <- "FD"

# Axis label used in figures
Y_LABEL <- "Left insula fractal dimension"

# ---- Metadata ----
# For structural ROIs, explicit metadata is recommended.
# For cognition use:
#   ANALYSIS_LEVEL <- "Cognition"
#   REGION_NAME    <- OUTCOME_COL
#   HEMISPHERE     <- "NA"
#   LOBE_NAME      <- "NA"
ANALYSIS_LEVEL <- "ROI"
REGION_NAME    <- "lh_insula"
HEMISPHERE     <- "Left"
LOBE_NAME      <- "Insula"

# If FALSE and a completed final workbook already exists, the script stops
# instead of overwriting the result.
OVERWRITE_EXISTING <- FALSE

# ============================================================================
# PROJECT HELPERS
# ============================================================================

safe_name <- function(x) {
  gsub("[^A-Za-z0-9_\\-]+", "_", as.character(x))
}

find_single_data_file <- function(data_dir = DATA_DIR) {

  if (!dir.exists(data_dir)) {
    stop("Data directory not found: ", data_dir)
  }

  files <- list.files(
    data_dir,
    pattern = "\\.(xlsx|xls)$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  files <- files[!grepl("^~\\$", basename(files))]

  if (length(files) == 0L) {
    stop("No Excel data file found in: ", data_dir)
  }

  if (length(files) > 1L) {
    stop(
      paste0(
        "More than one Excel data file was found in Data/. ",
        "Keep exactly one modelling dataset there.\n",
        paste(basename(files), collapse = "\n")
      )
    )
  }

  files[1]
}

is_completed_result <- function(path) {

  if (!file.exists(path)) return(FALSE)

  sheets <- tryCatch(
    readxl::excel_sheets(path),
    error = function(e) character(0)
  )

  if (!all(c("Summary", "RunMetadata") %in% sheets)) {
    return(FALSE)
  }

  ok <- tryCatch({
    x <- readxl::read_excel(path, sheet = "Summary", n_max = 1)
    nrow(x) >= 1
  }, error = function(e) FALSE)

  isTRUE(ok)
}

normalize_user_type <- function(x) {

  z <- tolower(trimws(as.character(x)[1]))

  if (z %in% c("fd", "fractal", "fractal dimension", "fractal_dimension")) {
    return("FD")
  }
  if (z %in% c("thickness", "cortical thickness")) return("Thickness")
  if (z %in% c("curvature")) return("Curvature")
  if (z %in% c("volume")) return("Volume")
  if (z %in% c("area", "surface area")) return("Area")
  if (z %in% c("cognition", "cognitive")) return("Cognition")
  if (z %in% c("other")) return("Other")

  stop(
    "Unsupported OUTCOME_TYPE: ", x,
    "\nAllowed values: FD, Thickness, Curvature, Volume, Area, Cognition, Other"
  )
}

# ============================================================================
# PRE-FLIGHT CHECKS
# ============================================================================

if (!file.exists(CORE_FILE)) {
  stop("Core file not found: ", CORE_FILE)
}

DATA_FILE <- find_single_data_file(DATA_DIR)

OUTCOME_TYPE <- normalize_user_type(OUTCOME_TYPE)

raw_names <- names(
  readxl::read_excel(
    DATA_FILE,
    sheet = SHEET_NAME,
    n_max = 1
  )
)

required_base <- c("Age", "sex")

if (OUTCOME_TYPE == "Cognition") {
  required_base <- c(required_base, "edu")
}

if (OUTCOME_TYPE %in% c("Volume", "Area")) {
  required_base <- c(required_base, "eTIV")
}

missing_base <- setdiff(required_base, raw_names)

if (length(missing_base)) {
  stop(
    "Required dataset column(s) missing:\n",
    paste(missing_base, collapse = "\n")
  )
}

if (!OUTCOME_COL %in% raw_names) {
  stop(
    paste0(
      "Outcome column was not found in the dataset:\n",
      OUTCOME_COL,
      "\n\nData file used:\n",
      DATA_FILE
    )
  )
}

# ============================================================================
# LOAD FROZEN CORE
# ============================================================================

# The public core is source-safe and does not automatically fit a model.
source(CORE_FILE, local = .GlobalEnv)

# ============================================================================
# BUILD OUTCOME-SPECIFIC CONFIG
# ============================================================================

cfg <- CONFIG

cfg$DATA_PATH      <- DATA_FILE
cfg$SHEET_NAME     <- SHEET_NAME

cfg$OUTCOME_COL    <- OUTCOME_COL
cfg$FD_COL         <- OUTCOME_COL
cfg$OUTCOME_TYPE   <- OUTCOME_TYPE
cfg$Y_LABEL        <- Y_LABEL

cfg$ANALYSIS_LEVEL <- ANALYSIS_LEVEL
cfg$REGION_NAME    <- REGION_NAME
cfg$HEMISPHERE     <- HEMISPHERE
cfg$LOBE_NAME      <- LOBE_NAME

# Explicit covariate policy for consistency with batch wrappers.
cfg$USE_ETIV <- OUTCOME_TYPE %in% c("Volume", "Area")
cfg$USE_EDU  <- identical(OUTCOME_TYPE, "Cognition")

cfg$OUT_DIR <- file.path(
  ROOT_OUT,
  "SingleOutcome",
  safe_name(OUTCOME_TYPE),
  safe_name(if (!is.null(REGION_NAME) && nzchar(REGION_NAME)) REGION_NAME else OUTCOME_COL)
)

dir.create(
  cfg$OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

final_xlsx <- file.path(
  cfg$OUT_DIR,
  sprintf(
    "Summary_results_Q1_AllOnly_%s.xlsx",
    safe_name(cfg$OUTCOME_COL)
  )
)

if (!isTRUE(OVERWRITE_EXISTING) && is_completed_result(final_xlsx)) {
  stop(
    paste0(
      "A completed result already exists:\n",
      final_xlsx,
      "\n\nSet OVERWRITE_EXISTING <- TRUE only if you intentionally want to rerun it."
    )
  )
}

# ============================================================================
# RUN ONE OUTCOME
# ============================================================================

cat("\n============================================================\n")
cat("BrainNormR single-outcome run\n")
cat("============================================================\n")
cat("Data:    ", DATA_FILE, "\n", sep = "")
cat("Outcome: ", OUTCOME_COL, "\n", sep = "")
cat("Type:    ", OUTCOME_TYPE, "\n", sep = "")
cat("Output:  ", cfg$OUT_DIR, "\n", sep = "")
cat("============================================================\n\n")

single_outcome_result <- main(cfg)

cat("\nSingle-outcome analysis finished successfully.\n")
cat("Final output folder:\n", cfg$OUT_DIR, "\n", sep = "")
