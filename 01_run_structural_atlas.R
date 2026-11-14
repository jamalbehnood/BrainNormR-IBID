##############################################################################
# 01_run_structural_atlas.R
# FINAL project wrapper for structural normative modelling
# DK68 x {Volume, Area, Thickness, Curvature, FD}
##############################################################################

suppressPackageStartupMessages({
  if (!requireNamespace("readxl", quietly=TRUE)) stop("Install package: readxl")
  if (!requireNamespace("writexl", quietly=TRUE)) stop("Install package: writexl")
})

# ==========================
# USER SETTINGS
# ==========================
CORE_FILE   <- "BrainNormR_v5_FINAL_PUBLIC.R"
DATA_DIR    <- "Data"
CONFIG_FILE <- file.path("Config", "DK68_structural_config.csv")
ROOT_OUT    <- "Results"
SHEET_NAME  <- 1

# Safest workflow for power interruptions:
# 1) First keep TEST_MODE=TRUE and test one row.
# 2) Then set TEST_MODE=FALSE and run small chunks.
TEST_MODE=FALSE
#TEST_MODE <- TRUE
TEST_N    <- 1L

# Filter before chunking: "FD", "Volume", "Area", "Thickness", "Curvature", or "ALL"
MEASURE_FILTER <- "FD"

# Used only when TEST_MODE=FALSE. These are rows AFTER applying MEASURE_FILTER.
START_ROW <- 46L
END_ROW   <- 70L

RESUME        <- TRUE   # skip a completed outcome if its final xlsx already exists
STOP_ON_ERROR <- FALSE  # log a failed outcome and continue to the next row

# ==========================
# PROJECT HELPERS
# ==========================
find_single_data_file <- function(data_dir=DATA_DIR) {
  files <- list.files(data_dir, pattern="\\.(xlsx|xls)$", full.names=TRUE, ignore.case=TRUE)
  files <- files[!grepl("^~\\$", basename(files))]
  if (length(files) == 0L) stop("No Excel data file found in: ", data_dir)
  if (length(files) > 1L) stop(
    "More than one Excel data file found in Data/. Keep only the modelling dataset there, or set DATA_FILE manually.\n",
    paste(basename(files), collapse="\n")
  )
  files[1]
}

safe_name <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", as.character(x))

if (!file.exists(CORE_FILE)) stop("Core file not found: ", CORE_FILE)
if (!file.exists(CONFIG_FILE)) stop("Structural config not found: ", CONFIG_FILE)
DATA_FILE <- find_single_data_file(DATA_DIR)

# Core is source-safe: loading it does not launch a model.
source(CORE_FILE, local=.GlobalEnv)

required_fields <- c("ROI","Measure","Column")
validate_structural_config <- function(tab) {
  miss <- setdiff(required_fields, names(tab))
  if (length(miss)) stop("Structural config is missing columns: ", paste(miss, collapse=", "))
  valid <- c("Volume","Area","Thickness","Curvature","FD")
  bad <- setdiff(unique(tab$Measure), valid)
  if (length(bad)) stop("Unsupported Measure value(s): ", paste(bad, collapse=", "))
  raw_names <- names(readxl::read_excel(DATA_FILE, sheet=SHEET_NAME, n_max=1))
  missing_outcomes <- setdiff(tab$Column, raw_names)
  if (length(missing_outcomes)) stop(
    "These structural columns were not found in the data file:\n",
    paste(missing_outcomes, collapse="\n")
  )
  invisible(TRUE)
}

expected_result_file <- function(cfg) {
  stem <- if (!is.null(cfg$OUTPUT_STEM) && nzchar(as.character(cfg$OUTPUT_STEM))) {
    safe_name(cfg$OUTPUT_STEM)
  } else {
    safe_name(cfg$OUTCOME_COL)
  }
  file.path(cfg$OUT_DIR, sprintf("Summary_results_Q1_AllOnly_%s.xlsx", stem))
}

# Resume only from a genuinely completed/readable workbook.
is_completed_result <- function(path) {
  if (!file.exists(path)) return(FALSE)
  sh <- tryCatch(readxl::excel_sheets(path), error=function(e) character())
  if (!all(c("Summary", "RunMetadata") %in% sh)) return(FALSE)
  sm <- tryCatch(readxl::read_excel(path, sheet="Summary", n_max=2), error=function(e) NULL)
  !is.null(sm) && nrow(sm) >= 1L
}

run_one_structural <- function(row) {
  cfg <- CONFIG
  roi <- as.character(row$ROI)
  measure <- as.character(row$Measure)
  column <- as.character(row$Column)

  cfg$DATA_PATH      <- DATA_FILE
  cfg$SHEET_NAME     <- SHEET_NAME
  cfg$OUTCOME_COL    <- column
  cfg$FD_COL         <- column
  cfg$OUTCOME_TYPE   <- measure
  cfg$Y_LABEL        <- column
  cfg$ANALYSIS_LEVEL <- "ROI"
  cfg$REGION_NAME    <- roi
  cfg$HEMISPHERE     <- "AUTO"
  cfg$LOBE_NAME      <- "NA"

  # Explicitly preserve the study's covariate policy.
  cfg$USE_ETIV <- measure %in% c("Volume", "Area")
  cfg$USE_EDU  <- FALSE

  # Output naming is deliberately separated from OUTCOME_COL:
  # OUTCOME_COL must remain the exact data-column name.
  # OUTPUT_STEM is used only for human-readable output names.
  cfg$OUTPUT_STEM <- paste0(safe_name(roi), "_", safe_name(measure))

  # New folder convention:
  # Results/Structural/<Measure>/ROI/<ROI>_<Measure>/
  cfg$OUT_DIR <- file.path(
    ROOT_OUT, "Structural", measure, "ROI", cfg$OUTPUT_STEM
  )
  dir.create(cfg$OUT_DIR, recursive=TRUE, showWarnings=FALSE)

  outfile <- expected_result_file(cfg)
  if (isTRUE(RESUME) && is_completed_result(outfile)) {
    message("[SKIP existing] ", column)
    return(list(cfg=cfg, status="SKIPPED_EXISTING", error=NA_character_))
  }

  message("\n============================================================")
  message("Structural model: ", column, " | ", measure, " | ", roi)
  message("Data: ", DATA_FILE)
  message("Output: ", cfg$OUT_DIR)
  message("============================================================")

  tryCatch({
    main(cfg)

    # The frozen core writes the workbook using OUTCOME_COL.
    # Rename ONLY the final workbook so the file name carries ROI + Measure,
    # while the statistical model and the real source-column name remain unchanged.
    core_xlsx <- file.path(
      cfg$OUT_DIR,
      sprintf("Summary_results_Q1_AllOnly_%s.xlsx", safe_name(cfg$OUTCOME_COL))
    )
    target_xlsx <- expected_result_file(cfg)

    if (!identical(normalizePath(core_xlsx, winslash="/", mustWork=FALSE),
                   normalizePath(target_xlsx, winslash="/", mustWork=FALSE))) {
      if (file.exists(core_xlsx)) {
        if (file.exists(target_xlsx)) unlink(target_xlsx)
        ok_rename <- file.rename(core_xlsx, target_xlsx)
        if (!isTRUE(ok_rename)) {
          stop("Model completed, but final workbook could not be renamed to: ", target_xlsx)
        }
      } else if (!file.exists(target_xlsx)) {
        stop("Model completed, but final workbook was not found for renaming: ", core_xlsx)
      }
    }

    message("Final workbook: ", target_xlsx)
    list(cfg=cfg, status="OK", error=NA_character_)
  }, error=function(e) {
    msg <- conditionMessage(e)
    message("[FAILED] ", column, " : ", msg)
    if (isTRUE(STOP_ON_ERROR)) stop(e)
    list(cfg=cfg, status="FAILED", error=msg)
  })
}

run_structural_atlas <- function() {

  tab <- read.csv(
    CONFIG_FILE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Validate only config structure first
  miss <- setdiff(required_fields, names(tab))
  if (length(miss)) {
    stop("Structural config is missing columns: ", paste(miss, collapse = ", "))
  }

  valid <- c("Volume", "Area", "Thickness", "Curvature", "FD")
  bad <- setdiff(unique(tab$Measure), valid)
  if (length(bad)) {
    stop("Unsupported Measure value(s): ", paste(bad, collapse = ", "))
  }

  # Apply measure filter BEFORE checking data-column names
  if (!identical(toupper(MEASURE_FILTER), "ALL")) {
    tab <- tab[
      tolower(tab$Measure) == tolower(MEASURE_FILTER),
      ,
      drop = FALSE
    ]
    if (!nrow(tab)) {
      stop("No rows match MEASURE_FILTER = ", MEASURE_FILTER)
    }
  }

  n_available <- nrow(tab)

  # Choose only rows that are actually going to run
  if (isTRUE(TEST_MODE)) {

    tab_run <- head(
      tab,
      max(1L, as.integer(TEST_N))
    )

    message(
      sprintf(
        "[TEST MODE] Running %d row(s) from %d available row(s).",
        nrow(tab_run),
        n_available
      )
    )

  } else {

    first <- max(1L, as.integer(START_ROW))
    last  <- min(n_available, as.integer(END_ROW))

    if (first > last) {
      stop("START_ROW is greater than END_ROW after filtering.")
    }

    tab_run <- tab[first:last, , drop = FALSE]

    message(
      sprintf(
        "[CHUNK] %s rows %d-%d of %d",
        MEASURE_FILTER,
        first,
        last,
        n_available
      )
    )
  }

  # Validate ONLY the selected outcome columns
  raw_names <- names(
    readxl::read_excel(
      DATA_FILE,
      sheet = SHEET_NAME,
      n_max = 1
    )
  )

  missing_outcomes <- setdiff(
    as.character(tab_run$Column),
    raw_names
  )

  if (length(missing_outcomes)) {
    stop(
      paste0(
        "\nThe following selected structural outcome column(s) were not found in the data file:\n",
        paste(missing_outcomes, collapse = "\n"),
        "\n\nData file used:\n",
        DATA_FILE
      )
    )
  }

  log_dir <- file.path(ROOT_OUT, "Structural")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  log_file <- file.path(log_dir, "RUN_LOG.csv")

  for (i in seq_len(nrow(tab_run))) {

    row <- tab_run[i, , drop = FALSE]
    ans <- run_one_structural(row)

    rec <- data.frame(
      Timestamp = as.character(Sys.time()),
      Outcome = as.character(row$Column),
      Measure = as.character(row$Measure),
      Region = as.character(row$ROI),
      Status = ans$status,
      Error = ans$error,
      stringsAsFactors = FALSE
    )

    if (file.exists(log_file)) {
      old_log <- tryCatch(
        read.csv(log_file, stringsAsFactors = FALSE),
        error = function(e) data.frame()
      )
      rec <- rbind(old_log, rec)
    }

    write.csv(rec, log_file, row.names = FALSE)

    rm(ans)
    gc()
  }

  message("\nStructural chunk finished.")
  invisible(TRUE)
}

structural_run <- run_structural_atlas()
