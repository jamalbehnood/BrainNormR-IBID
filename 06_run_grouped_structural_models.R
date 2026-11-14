##############################################################################
# 06_run_grouped_structural_models.R
# BrainNormR v5.0 - grouped structural model runner
#
# Runs WholeBrain / Lobe / Hemisphere / Yeo7 variables created by:
#   00_build_grouped_structural_dataset.R
#
# This script does NOT change the BrainNormR statistical core.
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

DATA_FILE <- file.path(
  "DerivedData",
  "BrainNormR_grouped_structural_dataset.xlsx"
)

CONFIG_FILE <- file.path(
  "Config",
  "Grouped_structural_config.csv"
)

ROOT_OUT <- "Results"
SHEET_NAME <- "ModelingData"

# Choose one:
#   "WholeBrain"
#   "Lobe"
#   "Hemisphere"
#   "Yeo7"
LEVEL_FILTER <- "Lobe"

# Choose one:
#   "FD"
#   "Volume"
#   "Area"
#   "Thickness"
#   "Curvature"
#   "ALL"
MEASURE_FILTER <- "ALL"

# NEW: optional hemisphere filter.
# Recommended values:
#   "ALL"
#   "Left"
#   "Right"
#   "Bilateral"
#
# For Lobe data this is inferred robustly from both the config Hemisphere
# field and the grouped variable prefix (lh_, rh_, bilateral_).
HEMISPHERE_FILTER <- "ALL"

# NEW: optional group filter.
# Examples for Lobe:
#   "ALL", "Frontal", "Parietal", "Temporal",
#   "Occipital", "Cingulate", "Insula"
GROUP_FILTER <- "ALL"

TEST_MODE <- FALSE
TEST_N <- 1L

# Used only when TEST_MODE = FALSE.
# IMPORTANT: row numbering is AFTER applying:
# LEVEL + MEASURE + HEMISPHERE + GROUP filters.
#
# Set RUN_ALL_FILTERED <- TRUE to run every row remaining after filters.
# For long jobs keep FALSE and use START_ROW / END_ROW for chunks.
RUN_ALL_FILTERED <- FALSE
START_ROW <- 61L
END_ROW <- 90L

RESUME <- TRUE
STOP_ON_ERROR <- FALSE

# ============================================================================
# HELPERS
# ============================================================================

safe_name <- function(x) {
  gsub("[^A-Za-z0-9_\\-]+", "_", as.character(x))
}


normalize_grouped_hemi <- function(hemi, column = "") {

  h <- tolower(trimws(as.character(hemi)))
  col <- tolower(trimws(as.character(column)))

  # Column prefix is the most reliable source for Lobe grouped variables.
  if (grepl("^lh_", col)) return("Left")
  if (grepl("^rh_", col)) return("Right")
  if (grepl("^bilateral_", col)) return("Bilateral")

  if (h %in% c("left", "lh", "l")) return("Left")
  if (h %in% c("right", "rh", "r")) return("Right")
  if (h %in% c("bilateral", "both", "both hemispheres", "all")) return("Bilateral")

  # WholeBrain / Yeo7 may not have a meaningful hemisphere label.
  if (!nzchar(h) || h %in% c("na", "n/a", "none", "unknown")) return("NA")

  # Preserve an unexpected config value rather than silently changing it.
  as.character(hemi)
}

filter_is_all <- function(x) {
  is.null(x) || !length(x) || is.na(x[1]) ||
    !nzchar(trimws(as.character(x[1]))) ||
    identical(toupper(trimws(as.character(x[1]))), "ALL")
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

# ============================================================================
# PRE-FLIGHT
# ============================================================================

if (!file.exists(CORE_FILE)) {
  stop("Core file not found: ", CORE_FILE)
}

if (!file.exists(DATA_FILE)) {
  stop(
    "Grouped dataset not found: ", DATA_FILE,
    "\nRun 00_build_grouped_structural_dataset.R first."
  )
}

if (!file.exists(CONFIG_FILE)) {
  stop(
    "Grouped config not found: ", CONFIG_FILE,
    "\nRun 00_build_grouped_structural_dataset.R first."
  )
}

tab <- read.csv(
  CONFIG_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required <- c(
  "Level",
  "Group",
  "Hemisphere",
  "Measure",
  "Column",
  "YLabel"
)

miss <- setdiff(required, names(tab))

if (length(miss)) {
  stop("Grouped config is missing: ", paste(miss, collapse = ", "))
}

# Filter by analysis level.
tab <- tab[
  tolower(tab$Level) == tolower(LEVEL_FILTER),
  ,
  drop = FALSE
]

if (!nrow(tab)) {
  stop("No rows match LEVEL_FILTER = ", LEVEL_FILTER)
}

# Filter by morphometric measure.
if (!filter_is_all(MEASURE_FILTER)) {

  tab <- tab[
    tolower(tab$Measure) == tolower(MEASURE_FILTER),
    ,
    drop = FALSE
  ]

  if (!nrow(tab)) {
    stop("No rows match MEASURE_FILTER = ", MEASURE_FILTER)
  }
}

# Standardize hemisphere labels before filtering/output.
tab$HemisphereNormalized <- mapply(
  normalize_grouped_hemi,
  tab$Hemisphere,
  tab$Column,
  USE.NAMES = FALSE
)

# Optional hemisphere filter.
if (!filter_is_all(HEMISPHERE_FILTER)) {

  target_hemi <- normalize_grouped_hemi(
    HEMISPHERE_FILTER,
    ""
  )

  tab <- tab[
    tolower(tab$HemisphereNormalized) == tolower(target_hemi),
    ,
    drop = FALSE
  ]

  if (!nrow(tab)) {
    stop(
      "No rows match HEMISPHERE_FILTER = ",
      HEMISPHERE_FILTER
    )
  }
}

# Optional group filter.
if (!filter_is_all(GROUP_FILTER)) {

  tab <- tab[
    tolower(tab$Group) == tolower(GROUP_FILTER),
    ,
    drop = FALSE
  ]

  if (!nrow(tab)) {
    stop(
      "No rows match GROUP_FILTER = ",
      GROUP_FILTER
    )
  }
}

# Deterministic ordering makes chunk numbers reproducible and understandable.
hemi_order <- match(
  tab$HemisphereNormalized,
  c("Left", "Right", "Bilateral", "NA")
)
hemi_order[is.na(hemi_order)] <- 99L

measure_order <- match(
  tab$Measure,
  c("Volume", "Area", "Thickness", "Curvature", "FD")
)
measure_order[is.na(measure_order)] <- 99L

tab <- tab[
  order(
    hemi_order,
    as.character(tab$Group),
    measure_order,
    as.character(tab$Column)
  ),
  ,
  drop = FALSE
]

rownames(tab) <- NULL

message(
  sprintf(
    "[FILTERED] Level=%s | Measure=%s | Hemisphere=%s | Group=%s | %d outcome(s)",
    LEVEL_FILTER,
    MEASURE_FILTER,
    HEMISPHERE_FILTER,
    GROUP_FILTER,
    nrow(tab)
  )
)

# Show exactly what the filtered row numbers mean BEFORE chunking.
preview_cols <- intersect(
  c(
    "Level",
    "Group",
    "HemisphereNormalized",
    "Measure",
    "Column"
  ),
  names(tab)
)

print(
  data.frame(
    FilteredRow = seq_len(nrow(tab)),
    tab[, preview_cols, drop = FALSE],
    check.names = FALSE
  ),
  row.names = FALSE
)

# Select rows that will actually run.
if (isTRUE(TEST_MODE)) {

  tab_run <- head(
    tab,
    max(1L, as.integer(TEST_N))
  )

  message(
    sprintf(
      "[TEST MODE] Running %d grouped outcome(s).",
      nrow(tab_run)
    )
  )

} else if (isTRUE(RUN_ALL_FILTERED)) {

  tab_run <- tab

  message(
    sprintf(
      "[ALL FILTERED] Running all %d selected grouped outcome(s).",
      nrow(tab_run)
    )
  )

} else {

  first <- max(1L, as.integer(START_ROW))
  last <- min(nrow(tab), as.integer(END_ROW))

  if (first > last) {
    stop(
      "START_ROW is greater than END_ROW after all filters. ",
      "Filtered outcomes available: ",
      nrow(tab)
    )
  }

  tab_run <- tab[first:last, , drop = FALSE]

  message(
    sprintf(
      "[CHUNK] %s / %s / %s / %s rows %d-%d of %d",
      LEVEL_FILTER,
      MEASURE_FILTER,
      HEMISPHERE_FILTER,
      GROUP_FILTER,
      first,
      last,
      nrow(tab)
    )
  )
}

message("[SELECTED FOR THIS RUN]")
print(
  data.frame(
    RunRow = seq_len(nrow(tab_run)),
    tab_run[, preview_cols, drop = FALSE],
    check.names = FALSE
  ),
  row.names = FALSE
)

# Validate only selected columns.
header <- names(
  readxl::read_excel(
    DATA_FILE,
    sheet = SHEET_NAME,
    n_max = 1
  )
)

missing_outcomes <- setdiff(tab_run$Column, header)

if (length(missing_outcomes)) {
  stop(
    "Selected grouped outcome column(s) not found:\n",
    paste(missing_outcomes, collapse = "\n")
  )
}

# ============================================================================
# LOAD CORE
# ============================================================================

source(CORE_FILE, local = .GlobalEnv)

# ============================================================================
# ONE MODEL
# ============================================================================

run_one_grouped <- function(row) {

  level <- as.character(row$Level)
  group <- as.character(row$Group)
  hemi_raw <- as.character(row$Hemisphere)
  hemi <- if ("HemisphereNormalized" %in% names(row)) {
    as.character(row$HemisphereNormalized)
  } else {
    normalize_grouped_hemi(hemi_raw, as.character(row$Column))
  }
  measure <- as.character(row$Measure)
  column <- as.character(row$Column)
  ylabel <- as.character(row$YLabel)

  # Keep hemisphere-separated grouped results in distinct folders.
  # This prevents Left / Right / Bilateral versions of the same lobe name
  # from being mixed in a single directory.
  out_dir <- file.path(
    ROOT_OUT,
    "StructuralGrouped",
    safe_name(level),
    safe_name(measure),
    safe_name(hemi),
    safe_name(group)
  )

  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  final_xlsx <- file.path(
    out_dir,
    sprintf(
      "Summary_results_Q1_AllOnly_%s.xlsx",
      safe_name(column)
    )
  )

  if (isTRUE(RESUME) && is_completed_result(final_xlsx)) {

    message(
      "[SKIP] completed: ",
      level, " / ", measure, " / ", group
    )

    return(
      list(
        status = "SKIPPED",
        error = ""
      )
    )
  }

  cfg <- CONFIG

  cfg$DATA_PATH <- DATA_FILE
  cfg$SHEET_NAME <- SHEET_NAME

  cfg$OUTCOME_COL <- column
  cfg$FD_COL <- column
  cfg$OUTCOME_TYPE <- measure
  cfg$Y_LABEL <- ylabel

  cfg$ANALYSIS_LEVEL <- level
  cfg$REGION_NAME <- group
  cfg$HEMISPHERE <- hemi
  cfg$LOBE_NAME <- if (tolower(level) == "lobe") group else "NA"

  cfg$USE_ETIV <- measure %in% c("Volume", "Area")
  cfg$USE_EDU <- FALSE

  cfg$OUT_DIR <- out_dir

  message(
    "\n[RUN] ",
    level, " | ",
    measure, " | ",
    hemi, " | ",
    group, " | ",
    column
  )

  ans <- tryCatch({

    main(cfg)

    list(
      status = "SUCCESS",
      error = ""
    )

  }, error = function(e) {

    msg <- conditionMessage(e)

    message(
      "[FAILED] ",
      level, " / ",
      measure, " / ",
      group, ": ",
      msg
    )

    if (isTRUE(STOP_ON_ERROR)) {
      stop(e)
    }

    list(
      status = "FAILED",
      error = msg
    )
  })

  ans
}

# ============================================================================
# RUN CHUNK + LOG
# ============================================================================

log_dir <- file.path(ROOT_OUT, "StructuralGrouped")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(log_dir, "RUN_LOG.csv")

for (i in seq_len(nrow(tab_run))) {

  row <- tab_run[i, , drop = FALSE]
  ans <- run_one_grouped(row)

  rec <- data.frame(
    Timestamp = as.character(Sys.time()),
    Level = as.character(row$Level),
    Group = as.character(row$Group),
    Hemisphere = if ("HemisphereNormalized" %in% names(row)) {
      as.character(row$HemisphereNormalized)
    } else {
      normalize_grouped_hemi(as.character(row$Hemisphere), as.character(row$Column))
    },
    Measure = as.character(row$Measure),
    Column = as.character(row$Column),
    Status = ans$status,
    Error = ans$error,
    stringsAsFactors = FALSE
  )

  if (file.exists(log_file)) {

    old_log <- tryCatch(
      read.csv(
        log_file,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      error = function(e) data.frame()
    )

    rec <- rbind(old_log, rec)
  }

  write.csv(
    rec,
    log_file,
    row.names = FALSE
  )

  rm(ans)
  gc()
}

message("\nGrouped structural chunk finished.")
