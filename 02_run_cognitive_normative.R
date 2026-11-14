##############################################################################
# 02_run_cognitive_normative.R
# FINAL project wrapper for cognitive normative modelling
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
CONFIG_FILE <- file.path("Config", "Cognition_Config.csv")
ROOT_OUT    <- "Results"
SHEET_NAME  <- 1
TEST_MODE=FALSE
#TEST_MODE <- TRUE
TEST_N    <- 1L

# Used only when TEST_MODE=FALSE
START_ROW <- 1L
END_ROW   <- 13L

RESUME        <- TRUE
STOP_ON_ERROR <- FALSE

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
if (!file.exists(CONFIG_FILE)) stop("Cognition config not found: ", CONFIG_FILE)
DATA_FILE <- find_single_data_file(DATA_DIR)
source(CORE_FILE, local=.GlobalEnv)

required_fields <- c("Test","Column")
validate_cognition_config <- function(tab) {
  miss <- setdiff(required_fields, names(tab))
  if (length(miss)) stop("Cognition config is missing columns: ", paste(miss, collapse=", "))
  raw_names <- names(readxl::read_excel(DATA_FILE, sheet=SHEET_NAME, n_max=1))
  missing_outcomes <- setdiff(tab$Column, raw_names)
  if (length(missing_outcomes)) stop(
    "These cognitive columns were not found in the data file:\n",
    paste(missing_outcomes, collapse="\n")
  )
  invisible(TRUE)
}

expected_result_file <- function(cfg) {
  file.path(cfg$OUT_DIR, sprintf("Summary_results_Q1_AllOnly_%s.xlsx", safe_name(cfg$OUTCOME_COL)))
}

# Resume only from a genuinely completed/readable workbook.
is_completed_result <- function(path) {
  if (!file.exists(path)) return(FALSE)
  sh <- tryCatch(readxl::excel_sheets(path), error=function(e) character())
  if (!all(c("Summary", "RunMetadata") %in% sh)) return(FALSE)
  sm <- tryCatch(readxl::read_excel(path, sheet="Summary", n_max=2), error=function(e) NULL)
  !is.null(sm) && nrow(sm) >= 1L
}

run_one_cognition <- function(row) {
  cfg <- CONFIG
  test <- as.character(row$Test)
  column <- as.character(row$Column)

  cfg$DATA_PATH      <- DATA_FILE
  cfg$SHEET_NAME     <- SHEET_NAME
  cfg$OUTCOME_COL    <- column
  cfg$FD_COL         <- column
  cfg$OUTCOME_TYPE   <- "Cognition"
  cfg$Y_LABEL        <- test
  cfg$ANALYSIS_LEVEL <- "Cognition"
  cfg$REGION_NAME    <- test
  cfg$HEMISPHERE     <- "NA"
  cfg$LOBE_NAME      <- "NA"
  cfg$USE_ETIV       <- FALSE
  cfg$USE_EDU        <- TRUE

  cfg$OUT_DIR <- file.path(ROOT_OUT, "Cognition", safe_name(test))
  dir.create(cfg$OUT_DIR, recursive=TRUE, showWarnings=FALSE)

  outfile <- expected_result_file(cfg)
  if (isTRUE(RESUME) && is_completed_result(outfile)) {
    message("[SKIP existing] ", column)
    return(list(cfg=cfg, status="SKIPPED_EXISTING", error=NA_character_))
  }

  message("\n============================================================")
  message("Cognitive model: ", column, " | ", test)
  message("Data: ", DATA_FILE)
  message("Output: ", cfg$OUT_DIR)
  message("============================================================")

  tryCatch({
    main(cfg)
    list(cfg=cfg, status="OK", error=NA_character_)
  }, error=function(e) {
    msg <- conditionMessage(e)
    message("[FAILED] ", column, " : ", msg)
    if (isTRUE(STOP_ON_ERROR)) stop(e)
    list(cfg=cfg, status="FAILED", error=msg)
  })
}

run_cognitive_atlas <- function() {
  tab <- read.csv(CONFIG_FILE, stringsAsFactors=FALSE, check.names=FALSE)
  validate_cognition_config(tab)
  n_available <- nrow(tab)

  if (isTRUE(TEST_MODE)) {
    tab_run <- head(tab, max(1L, as.integer(TEST_N)))
    message(sprintf("[TEST MODE] Running %d cognitive outcome(s) of %d.", nrow(tab_run), n_available))
  } else {
    first <- max(1L, as.integer(START_ROW))
    last  <- min(n_available, as.integer(END_ROW))
    if (first > last) stop("START_ROW is greater than END_ROW.")
    tab_run <- tab[first:last, , drop=FALSE]
    message(sprintf("[CHUNK] Cognition rows %d-%d of %d", first, last, n_available))
  }

  log_dir <- file.path(ROOT_OUT, "Cognition")
  dir.create(log_dir, recursive=TRUE, showWarnings=FALSE)
  log_file <- file.path(log_dir, "RUN_LOG.csv")

  for (i in seq_len(nrow(tab_run))) {
    row <- tab_run[i, , drop=FALSE]
    ans <- run_one_cognition(row)
    rec <- data.frame(
      Timestamp=as.character(Sys.time()),
      Outcome=as.character(row$Column),
      Test=as.character(row$Test),
      Status=ans$status,
      Error=ans$error,
      stringsAsFactors=FALSE
    )
    if (file.exists(log_file)) {
      old <- tryCatch(read.csv(log_file, stringsAsFactors=FALSE), error=function(e) data.frame())
      rec <- rbind(old, rec)
    }
    write.csv(rec, log_file, row.names=FALSE)
    rm(ans); gc()
  }

  message("\nCognitive chunk finished.")
  invisible(TRUE)
}

cognitive_run <- run_cognitive_atlas()
