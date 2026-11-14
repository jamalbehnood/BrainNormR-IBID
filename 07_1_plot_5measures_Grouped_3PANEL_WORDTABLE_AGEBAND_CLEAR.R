# =============================================================================
# BrainNormR -- CODE 07.1
# Grouped structural 5-measure plots: WholeBrain + Hemisphere + Lobe + Yeo7
#
# OUTPUT DESIGN:
#   Page/Figure: THREE scientific panels only
#     a) Standardized normative trajectories (P50)
#     b) Standardized conditional variability (Conditional_SD / SampleSD)
#     c) Standardized age gradients (Derivative_per_SD)
#
#   Separate editable Word file:
#     Morphometric summary table
#
# IMPORTANT:
#   Panel-B age-band labels are placed in a dedicated upper whitespace zone
#   so they do not overlap the conditional-variability curves.
#
# Measures (rows in summary table):
#   Volume, Area, Thickness, Curvature, FD
#
# No model is refitted. This script reads BrainNormR result workbooks only.
# Required workbook sheets:
#   Summary, CentileCurves, DerivativeCurve, VariabilityCurve
#
# Required packages:
#   readxl, dplyr, ggplot2, patchwork, gridExtra, grid
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(gridExtra)
  library(grid)
})

# Editable Word-table export
if (!requireNamespace("officer", quietly = TRUE) ||
    !requireNamespace("flextable", quietly = TRUE)) {
  stop(
    "Packages 'officer' and 'flextable' are required for editable Word tables.\n",
    "Install them once with:\n",
    "install.packages(c('officer', 'flextable'))"
  )
}

# =============================================================================
# EDITABLE WORD TABLE HELPER
# =============================================================================

export_summary_table_docx <- function(table_df, file, display_title, level) {

  word_table <- table_df
  names(word_table) <- c(
    "Measure",
    "Normative\ntype",
    "Variability\ntype",
    "Gradient\ntype",
    "Peak\nage",
    "Trough\nage",
    "Peak |grad|\nage"
  )

  ft <- flextable::flextable(word_table)
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::valign(ft, valign = "center", part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "body")
  ft <- flextable::fontsize(ft, size = 9.5, part = "header")
  ft <- flextable::padding(ft, padding = 3, part = "all")
  ft <- flextable::line_spacing(ft, space = 1.0, part = "all")

  ft <- flextable::width(ft, j = 1, width = 0.85)
  ft <- flextable::width(ft, j = 2, width = 1.15)
  ft <- flextable::width(ft, j = 3, width = 1.18)
  ft <- flextable::width(ft, j = 4, width = 1.15)
  ft <- flextable::width(ft, j = 5, width = 0.65)
  ft <- flextable::width(ft, j = 6, width = 0.70)
  ft <- flextable::width(ft, j = 7, width = 0.92)
  ft <- flextable::set_table_properties(ft, layout = "fixed", width = 1)

  doc <- officer::read_docx()

  doc <- officer::body_add_par(
    doc,
    paste0(display_title, ": morphometric summary"),
    style = "heading 1"
  )

  doc <- officer::body_add_par(
    doc,
    paste0("Analysis level: ", level),
    style = "Normal"
  )

  doc <- flextable::body_add_flextable(
    doc,
    value = ft,
    align = "center"
  )

  doc <- officer::body_add_par(
    doc,
    "* Peak |grad| = age of maximal absolute standardized gradient; * = reliable-interior boundary.",
    style = "Normal"
  )

  doc <- officer::body_end_section_portrait(
    doc,
    w = 8.27,
    h = 11.69
  )

  print(doc, target = file)
  invisible(file)
}

# =============================================================================
# 1. USER SETTINGS -- CODE 07.1
# =============================================================================

# BrainNormR grouped structural results root.
# Expected model-output structure:
#   StructuralGrouped/<Level>/<Measure>/<Hemisphere>/<Group>/Summary_results_*.xlsx
GROUPED_ROOT <- file.path("Results", "StructuralGrouped")

# Run all grouped structural levels requested for the thesis figures.
# The code discovers all valid groups automatically and requires all five measures
# (Volume, Area, Thickness, Curvature, FD) for each group before plotting it.
LEVELS_TO_RUN <- c("WholeBrain", "Hemisphere", "Lobe", "Yeo7")

# Optional folder aliases. This makes the script tolerant if the Yeo folder is
# named "Yeo" instead of "Yeo7" on disk.
LEVEL_FOLDER_ALIASES <- list(
  WholeBrain = c("WholeBrain"),
  Hemisphere = c("Hemisphere"),
  Lobe       = c("Lobe"),
  Yeo7       = c("Yeo7", "Yeo")
)

# Output root. Each result is saved in:
#   Combined_5Measures_Grouped/<Level>/<Hemisphere>/<Group>/
OUTPUT_ROOT <- file.path(GROUPED_ROOT, "Combined_5Measures_Grouped_CurvesPlusEditableWordTable")

# NULL = earliest age common to the five measures within each group.
REFERENCE_AGE <- NULL

# Plot options -- unchanged from the original code.
SHOW_TRAJECTORY_RIBBONS <- FALSE
SHOW_DERIVATIVE_CI      <- FALSE
SHOW_EXTREMA_MARKERS    <- TRUE
SHOW_MAXCHANGE_MARKERS  <- TRUE

# Age bands
AGE_BANDS <- data.frame(
  Band = c("Young", "Middle-aged", "Older"),
  xmin = c(20, 40, 60),
  xmax = c(40, 60, 80),
  stringsAsFactors = FALSE
)

AGE_BAND_COLORS <- c(
  "Young"       = "#DCEEFF",
  "Middle-aged" = "#FFF0C2",
  "Older"       = "#FFDCDC"
)

AGE_BAND_ALPHA <- 0.32

AGE_AXIS_MAX <- 80
AGE_AXIS_BREAKS <- c(20, 30, 40, 50, 60, 70, 80)

# Automatic curve-classification thresholds -- unchanged.
TRAJECTORY_FLAT_RANGE_SD <- 0.10
VARIABILITY_FLAT_REL_TOL <- 0.005

# Export dimensions: three stacked panels with room below for thesis caption.
FIG_WIDTH  <- 7.8
FIG_HEIGHT <- 9.0
FIG_DPI    <- 600

MEASURE_LEVELS <- c("Volume", "Area", "Thickness", "Curvature", "FD")

# =============================================================================
# 2. BASIC CHECKS + GROUPED JOB DISCOVERY
# =============================================================================

if (!dir.exists(GROUPED_ROOT)) {
  stop("GROUPED_ROOT does not exist:\n", GROUPED_ROOT)
}

dir.create(OUTPUT_ROOT, recursive = TRUE, showWarnings = FALSE)

required_sheets <- c("Summary", "CentileCurves", "DerivativeCurve", "VariabilityCurve")

# Resolve the actual on-disk folder for a requested analysis level.
resolve_level_folder <- function(level_name) {
  candidates <- LEVEL_FOLDER_ALIASES[[level_name]]
  if (is.null(candidates)) candidates <- level_name

  paths <- file.path(GROUPED_ROOT, candidates)
  ok <- dir.exists(paths)

  if (!any(ok)) {
    warning(
      "No folder found for level '", level_name, "'. Checked:\n",
      paste(paths, collapse = "\n")
    )
    return(NA_character_)
  }

  # Prefer the first alias in the list if more than one exists.
  paths[which(ok)[1]]
}

# Find all final BrainNormR workbooks directly below:
#   <Level>/<Measure>/<Hemisphere>/<Group>/
# No group names are hard-coded; WholeBrain, hemisphere, lobe and Yeo groups
# are discovered from the actual folder tree produced by the grouped pipeline.
discover_measure_outputs <- function(level_label, level_dir, measure) {

  measure_root <- file.path(level_dir, measure)
  if (!dir.exists(measure_root)) return(data.frame())

  dirs <- list.dirs(measure_root, recursive = TRUE, full.names = TRUE)
  dirs <- dirs[normalizePath(dirs, winslash = "/", mustWork = FALSE) !=
                 normalizePath(measure_root, winslash = "/", mustWork = FALSE)]

  out <- lapply(dirs, function(d) {
    found <- list.files(
      d,
      pattern = "^Summary_results_Q1_AllOnly_.*\\.xlsx$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    found <- found[!grepl("^~\\$", basename(found))]
    found <- unique(found)

    # Only directories containing a final summary workbook are candidate groups.
    if (!length(found)) return(NULL)

    if (length(found) > 1L) {
      stop(
        "More than one final Summary workbook was found in:\n", d,
        "\nPlease keep one final workbook per grouped result folder.\n",
        paste(found, collapse = "\n")
      )
    }

    root_norm <- normalizePath(measure_root, winslash = "/", mustWork = FALSE)
    dir_norm  <- normalizePath(d, winslash = "/", mustWork = FALSE)

    prefix <- paste0(root_norm, "/")
    if (!startsWith(dir_norm, prefix)) return(NULL)
    rel <- substring(dir_norm, nchar(prefix) + 1L)
    parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]

    # Grouped structural outputs from 06 are expected as Hemisphere/Group.
    if (length(parts) != 2L) return(NULL)

    data.frame(
      Level = level_label,
      LevelFolder = basename(level_dir),
      Measure = measure,
      Hemisphere = parts[1],
      Group = parts[2],
      Folder = d,
      File = found[1],
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(out)
}

# Build one master inventory from all requested levels/measures.
inventory_list <- list()
for (lev in LEVELS_TO_RUN) {
  lev_dir <- resolve_level_folder(lev)
  if (is.na(lev_dir)) next

  for (m in MEASURE_LEVELS) {
    inventory_list[[paste(lev, m, sep = "__")]] <-
      discover_measure_outputs(lev, lev_dir, m)
  }
}

inventory <- dplyr::bind_rows(inventory_list)

if (!nrow(inventory)) {
  stop(
    "No grouped BrainNormR Summary workbooks were discovered under:\n",
    GROUPED_ROOT
  )
}

# A plotting job is valid only if all five morphometric measures are present.
jobs <- inventory %>%
  dplyr::group_by(Level, Hemisphere, Group) %>%
  dplyr::summarise(
    N_Measures = dplyr::n_distinct(Measure),
    MeasuresFound = paste(sort(unique(Measure)), collapse = ", "),
    .groups = "drop"
  ) %>%
  dplyr::filter(N_Measures == length(MEASURE_LEVELS)) %>%
  dplyr::arrange(factor(Level, levels = LEVELS_TO_RUN), Hemisphere, Group)

if (!nrow(jobs)) {
  stop(
    "Grouped outputs were found, but no group contains all five measures: ",
    paste(MEASURE_LEVELS, collapse = ", ")
  )
}

# Show exactly what will be processed before figure generation starts.
cat("\n============================================================\n")
cat("BrainNormR 07.1 -- grouped five-measure figure jobs\n")
cat("============================================================\n")
print(jobs, n = Inf)
cat("\nTotal valid groups: ", nrow(jobs), "\n", sep = "")
cat("============================================================\n\n")

# =============================================================================
# 3. HELPER FUNCTIONS
# =============================================================================

first_nonempty <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x)) x[1] else NA_character_
}

truthy <- function(x) {
  if (!length(x) || is.na(x[1])) return(FALSE)
  z <- x[1]
  if (is.logical(z)) return(isTRUE(z))
  if (is.numeric(z)) return(is.finite(z) && z != 0)
  tolower(trimws(as.character(z))) %in% c("true", "t", "1", "yes", "y")
}

detect_measure <- function(summary_df, file_path) {
  candidates <- c()
  for (nm in c("OutcomeType", "Measure", "Outcome")) {
    if (nm %in% names(summary_df)) {
      candidates <- c(candidates, as.character(summary_df[[nm]][1]))
    }
  }
  candidates <- c(candidates, basename(file_path))
  txt <- paste(candidates, collapse = " | ")

  if (grepl("thickness", txt, ignore.case = TRUE)) return("Thickness")
  if (grepl("curvature|curv", txt, ignore.case = TRUE)) return("Curvature")
  if (grepl("volume|_vol\\b|\\bvol\\b", txt, ignore.case = TRUE)) return("Volume")
  if (grepl("area", txt, ignore.case = TRUE)) return("Area")
  if (grepl("(^|[^A-Za-z])FD([^A-Za-z]|$)|fractal", txt, ignore.case = TRUE)) return("FD")
  NA_character_
}

get_region_name <- function(summary_df) {
  for (nm in c("Region", "RegionName", "ROI", "Group")) {
    if (nm %in% names(summary_df)) {
      z <- first_nonempty(summary_df[[nm]])
      if (!is.na(z)) return(z)
    }
  }
  NA_character_
}

find_p50_col <- function(df) {
  candidates <- c("P50", "P50_0", "P50.0", "P50th", "Median")
  z <- intersect(candidates, names(df))
  if (length(z)) return(z[1])

  nn <- gsub("[^A-Za-z0-9]", "", names(df))
  ii <- which(toupper(nn) == "P50")
  if (length(ii)) return(names(df)[ii[1]])
  stop("Could not identify the 50th-centile column in CentileCurves.")
}

find_centile_col <- function(df, pct) {
  target <- paste0("P", gsub("\\.", "_", format(pct, trim = TRUE, scientific = FALSE)))
  if (target %in% names(df)) return(target)
  target_norm <- toupper(gsub("[^A-Za-z0-9]", "", target))
  nn <- toupper(gsub("[^A-Za-z0-9]", "", names(df)))
  ii <- which(nn == target_norm)
  if (length(ii)) return(names(df)[ii[1]])
  NA_character_
}

safe_approx <- function(x, y, xout) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 2) return(NA_real_)
  approx(x[ok], y[ok], xout = xout, rule = 2)$y
}

safe_num <- function(df, nm) {
  if (!nm %in% names(df)) return(NA_real_)
  suppressWarnings(as.numeric(df[[nm]][1]))
}

fmt_age <- function(x, boundary = FALSE) {
  if (!is.finite(x)) return("--")
  z <- sprintf("%.1f", x)
  if (isTRUE(boundary)) paste0(z, "*") else z
}

# Robust slope over a segment, used only for descriptive variability class.
segment_slope <- function(age, y, which_part = c("first", "last")) {
  which_part <- match.arg(which_part)
  ok <- is.finite(age) & is.finite(y)
  age <- age[ok]; y <- y[ok]
  if (length(age) < 6) return(NA_real_)

  ord <- order(age)
  age <- age[ord]; y <- y[ord]
  n <- length(age)
  k <- max(3L, floor(n / 3))
  ii <- if (which_part == "first") seq_len(k) else (n-k+1):n
  fit <- tryCatch(lm(y[ii] ~ age[ii]), error = function(e) NULL)
  if (is.null(fit)) return(NA_real_)
  as.numeric(coef(fit)[2])
}

# Main class for central normative trajectory.
# Peak/Trough flags from the fitted full-data model take precedence.
classify_trajectory <- function(traj_df, summary_df) {
  has_peak <- all(c("InternalPeak", "PeakAge") %in% names(summary_df)) &&
    truthy(summary_df$InternalPeak[1]) && is.finite(safe_num(summary_df, "PeakAge"))
  has_trough <- all(c("InternalTrough", "TroughAge") %in% names(summary_df)) &&
    truthy(summary_df$InternalTrough[1]) && is.finite(safe_num(summary_df, "TroughAge"))

  if (has_peak && has_trough) return("Complex")
  if (has_peak) return("Peak-shaped")
  if (has_trough) return("Trough-shaped")

  z <- traj_df[is.finite(traj_df$Age) & is.finite(traj_df$StandardizedTrajectory), , drop = FALSE]
  if (nrow(z) < 3) return("Unclassified")
  z <- z[order(z$Age), , drop = FALSE]

  rg <- diff(range(z$StandardizedTrajectory, na.rm = TRUE))
  if (is.finite(rg) && rg < TRAJECTORY_FLAT_RANGE_SD) return("Stable / flat")

  delta <- tail(z$StandardizedTrajectory, 1) - head(z$StandardizedTrajectory, 1)
  if (!is.finite(delta)) return("Unclassified")
  if (delta > 0) "Monotonic increase" else if (delta < 0) "Monotonic decline" else "Stable / flat"
}

# Descriptive class for conditional variability curve.
classify_variability <- function(var_df) {
  z <- var_df[is.finite(var_df$Age) & is.finite(var_df$Conditional_SD_std), , drop = FALSE]
  if (nrow(z) < 6) return(list(class = "Unclassified", rel_range = NA_real_))
  z <- z[order(z$Age), , drop = FALSE]

  mu <- mean(z$Conditional_SD_std, na.rm = TRUE)
  rr <- if (is.finite(mu) && abs(mu) > .Machine$double.eps) {
    diff(range(z$Conditional_SD_std, na.rm = TRUE)) / abs(mu)
  } else {
    NA_real_
  }

  if (is.finite(rr) && rr < VARIABILITY_FLAT_REL_TOL) {
    return(list(class = "Stable variability", rel_range = rr))
  }

  s1 <- segment_slope(z$Age, z$Conditional_SD_std, "first")
  s2 <- segment_slope(z$Age, z$Conditional_SD_std, "last")

  age_span <- diff(range(z$Age))
  y_range  <- diff(range(z$Conditional_SD_std))
  # 10% of the average full-range slope is treated as near-zero for classification.
  slope_tol <- if (is.finite(age_span) && age_span > 0 && is.finite(y_range)) {
    max(1e-8, 0.10 * y_range / age_span)
  } else {
    1e-8
  }

  pos1 <- is.finite(s1) && s1 >  slope_tol
  neg1 <- is.finite(s1) && s1 < -slope_tol
  pos2 <- is.finite(s2) && s2 >  slope_tol
  neg2 <- is.finite(s2) && s2 < -slope_tol

  cls <- if (neg1 && pos2) {
    "U-shaped variability"
  } else if (pos1 && neg2) {
    "Inverted-U variability"
  } else if (pos1 && (pos2 || !neg2)) {
    "Increasing variability"
  } else if (neg1 && (neg2 || !pos2)) {
    "Decreasing variability"
  } else {
    # Fallback to net direction if the endpoint slopes are nearly flat/ambiguous.
    net <- tail(z$Conditional_SD_std, 1) - head(z$Conditional_SD_std, 1)
    if (is.finite(y_range) && y_range > 0 && is.finite(net) && abs(net) >= 0.50 * y_range) {
      if (net > 0) "Increasing variability" else "Decreasing variability"
    } else {
      "Complex variability"
    }
  }

  list(class = cls, rel_range = rr)
}


# -----------------------------------------------------------------------------
# Descriptive classification of the standardized age-gradient curve (Panel C)
#
# This classification is used ONLY to summarize Panel C in the table.
# It does not modify the fitted model, derivative, or any exported model result.
#
# Possible classes:
#   Negative - attenuating
#   Negative - accelerating
#   Negative - persistent
#   Positive - attenuating
#   Positive - accelerating
#   Positive - persistent
#   Negative -> Positive
#   Positive -> Negative
#   Near-zero / stable
#   Complex
# -----------------------------------------------------------------------------

classify_gradient <- function(deriv_df) {

  z <- deriv_df[
    is.finite(deriv_df$Age) &
      is.finite(deriv_df$Derivative_per_SD),
    ,
    drop = FALSE
  ]

  if (nrow(z) < 6) return("Unclassified")

  z <- z[order(z$Age), , drop = FALSE]
  d <- z$Derivative_per_SD

  max_abs <- max(abs(d), na.rm = TRUE)

  if (!is.finite(max_abs)) return("Unclassified")

  # Table-classification threshold only.
  # If the entire standardized derivative curve is extremely small,
  # call it effectively near-zero.
  absolute_zero_tol <- 0.0025

  if (max_abs < absolute_zero_tol) {
    return("Near-zero / stable")
  }

  # Relative zero band prevents tiny numerical sign changes near zero from
  # being interpreted as genuine direction reversals.
  zero_tol <- max(1e-5, 0.05 * max_abs)

  n <- nrow(z)
  k <- max(3L, floor(n / 4))

  early <- median(d[seq_len(k)], na.rm = TRUE)
  late  <- median(d[(n-k+1):n], na.rm = TRUE)

  sign_class <- function(x) {
    if (!is.finite(x)) return(0L)
    if (x >  zero_tol) return( 1L)
    if (x < -zero_tol) return(-1L)
    0L
  }

  s_early <- sign_class(early)
  s_late  <- sign_class(late)

  # Robust sign sequence over the whole curve.
  signs <- ifelse(
    d > zero_tol,
    1L,
    ifelse(d < -zero_tol, -1L, 0L)
  )

  nz_signs <- signs[signs != 0L]

  if (length(nz_signs)) {
    compressed <- nz_signs[c(TRUE, diff(nz_signs) != 0L)]
    n_sign_changes <- max(0L, length(compressed) - 1L)
  } else {
    compressed <- integer()
    n_sign_changes <- 0L
  }

  if (n_sign_changes > 1L) {
    return("Complex")
  }

  if (s_early < 0L && s_late > 0L) {
    return("Negative -> Positive")
  }

  if (s_early > 0L && s_late < 0L) {
    return("Positive -> Negative")
  }

  if (s_early < 0L && s_late == 0L) {
    return("Negative - attenuating")
  }

  if (s_early > 0L && s_late == 0L) {
    return("Positive - attenuating")
  }

  if (s_early == 0L && s_late < 0L) {
    return("Negative - accelerating")
  }

  if (s_early == 0L && s_late > 0L) {
    return("Positive - accelerating")
  }

  # Same-sign curves: compare the magnitude early versus late.
  # Require at least a 20% difference before calling attenuation/acceleration.
  if (s_early < 0L && s_late < 0L) {

    ratio <- abs(late) / max(abs(early), .Machine$double.eps)

    if (ratio <= 0.80) return("Negative - attenuating")
    if (ratio >= 1.20) return("Negative - accelerating")

    return("Negative - persistent")
  }

  if (s_early > 0L && s_late > 0L) {

    ratio <- abs(late) / max(abs(early), .Machine$double.eps)

    if (ratio <= 0.80) return("Positive - attenuating")
    if (ratio >= 1.20) return("Positive - accelerating")

    return("Positive - persistent")
  }

  # Fallback when one/both endpoints are near zero but the curve has a clear
  # dominant sign or one single sign reversal.
  if (length(compressed) == 1L) {
    if (compressed[1] < 0L) return("Negative - attenuating")
    if (compressed[1] > 0L) return("Positive - attenuating")
  }

  if (length(compressed) == 2L) {
    if (identical(compressed, c(-1L, 1L))) return("Negative -> Positive")
    if (identical(compressed, c(1L, -1L))) return("Positive -> Negative")
  }

  "Complex"
}


# =============================================================================
# 3A. RUN THE ORIGINAL FIVE-MEASURE FIGURE PIPELINE FOR ONE GROUP
# =============================================================================

run_one_group <- function(level_name, hemisphere_name, group_name, files) {

  # Per-group settings used by the unchanged plotting/summary logic below.
  REGION_NAME <- group_name

  REGION_TITLE <- if (identical(level_name, "WholeBrain")) {
    paste0("WholeBrain | ", hemisphere_name)
  } else {
    paste0(group_name, " | ", hemisphere_name, " | ", level_name)
  }

  OUTPUT_DIR <- file.path(
    OUTPUT_ROOT,
    level_name,
    hemisphere_name,
    group_name
  )
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

  names(files) <- MEASURE_LEVELS

  cat("\n------------------------------------------------------------\n")
  cat("Running: ", level_name, " | ", hemisphere_name, " | ", group_name, "\n", sep = "")
  cat("------------------------------------------------------------\n")
  for (m in MEASURE_LEVELS) {
    cat(sprintf("  %-10s -> %s\n", m, files[[m]]))
  }

  # Workbook QC is performed independently for every group.
  for (f in files) {
    sh <- excel_sheets(f)
    missing_sh <- setdiff(required_sheets, sh)
    if (length(missing_sh)) {
      stop(
        "Missing sheet(s) in:\n", basename(f), "\n",
        paste(missing_sh, collapse = ", ")
      )
    }
  }

# =============================================================================
# 4. READ THE FIVE OUTCOME WORKBOOKS
# =============================================================================

traj_list      <- list()
deriv_list     <- list()
variability_list <- list()
meta_list      <- list()
extrema_list   <- list()
summary_raw    <- list()
maxchange_list <- list()

for (f in files) {
  sm <- as.data.frame(read_excel(f, sheet = "Summary"))
  cc <- as.data.frame(read_excel(f, sheet = "CentileCurves"))
  dc <- as.data.frame(read_excel(f, sheet = "DerivativeCurve"))
  vc <- as.data.frame(read_excel(f, sheet = "VariabilityCurve"))

  measure <- detect_measure(sm, f)
  if (is.na(measure)) {
    stop("Could not identify measure for file:\n", basename(f))
  }

  if (!"SampleSD" %in% names(sm)) stop("SampleSD is missing in: ", basename(f))
  sample_sd <- suppressWarnings(as.numeric(sm$SampleSD[1]))
  if (!is.finite(sample_sd) || sample_sd <= 0) stop("Invalid SampleSD in: ", basename(f))

  if (!"Age" %in% names(cc)) stop("Age missing from CentileCurves in: ", basename(f))
  if (!"Age" %in% names(dc)) stop("Age missing from DerivativeCurve in: ", basename(f))
  if (!all(c("Age", "Conditional_SD") %in% names(vc))) {
    stop("VariabilityCurve must contain Age and Conditional_SD in: ", basename(f))
  }

  p50_col  <- find_p50_col(cc)
  p025_col <- find_centile_col(cc, 2.5)
  p975_col <- find_centile_col(cc, 97.5)

  if ("Derivative_per_SD" %in% names(dc)) {
    deriv_std <- suppressWarnings(as.numeric(dc$Derivative_per_SD))
  } else if ("Derivative" %in% names(dc)) {
    deriv_std <- suppressWarnings(as.numeric(dc$Derivative)) / sample_sd
  } else {
    stop("Neither Derivative_per_SD nor Derivative exists in: ", basename(f))
  }

  deriv_lo_std <- rep(NA_real_, nrow(dc))
  deriv_hi_std <- rep(NA_real_, nrow(dc))
  if (all(c("Derivative_L95", "Derivative_U95") %in% names(dc))) {
    deriv_lo_std <- suppressWarnings(as.numeric(dc$Derivative_L95)) / sample_sd
    deriv_hi_std <- suppressWarnings(as.numeric(dc$Derivative_U95)) / sample_sd
  }

  traj_list[[measure]] <- data.frame(
    Age = suppressWarnings(as.numeric(cc$Age)),
    P50 = suppressWarnings(as.numeric(cc[[p50_col]])),
    P025 = if (!is.na(p025_col)) suppressWarnings(as.numeric(cc[[p025_col]])) else NA_real_,
    P975 = if (!is.na(p975_col)) suppressWarnings(as.numeric(cc[[p975_col]])) else NA_real_,
    Measure = measure,
    SampleSD = sample_sd,
    stringsAsFactors = FALSE
  )

  deriv_list[[measure]] <- data.frame(
    Age = suppressWarnings(as.numeric(dc$Age)),
    Derivative_per_SD = deriv_std,
    Derivative_L95_per_SD = deriv_lo_std,
    Derivative_U95_per_SD = deriv_hi_std,
    Measure = measure,
    stringsAsFactors = FALSE
  )

  variability_list[[measure]] <- data.frame(
    Age = suppressWarnings(as.numeric(vc$Age)),
    Conditional_SD = suppressWarnings(as.numeric(vc$Conditional_SD)),
    Conditional_SD_std = suppressWarnings(as.numeric(vc$Conditional_SD)) / sample_sd,
    Measure = measure,
    SampleSD = sample_sd,
    stringsAsFactors = FALSE
  )

  # Peak / trough markers from Summary
  ext <- data.frame(
    Measure = character(), Age = numeric(), Value = numeric(),
    StandardizedValue = numeric(), PointType = character(),
    stringsAsFactors = FALSE
  )

  if (all(c("InternalPeak", "PeakAge", "PeakValue") %in% names(sm)) && truthy(sm$InternalPeak[1])) {
    a <- safe_num(sm, "PeakAge"); v <- safe_num(sm, "PeakValue")
    if (is.finite(a) && is.finite(v)) {
      ext <- rbind(ext, data.frame(Measure=measure, Age=a, Value=v,
                                  StandardizedValue=NA_real_, PointType="Peak (max)",
                                  stringsAsFactors=FALSE))
    }
  }

  if (all(c("InternalTrough", "TroughAge", "TroughValue") %in% names(sm)) && truthy(sm$InternalTrough[1])) {
    a <- safe_num(sm, "TroughAge"); v <- safe_num(sm, "TroughValue")
    if (is.finite(a) && is.finite(v)) {
      ext <- rbind(ext, data.frame(Measure=measure, Age=a, Value=v,
                                  StandardizedValue=NA_real_, PointType="Trough (min)",
                                  stringsAsFactors=FALSE))
    }
  }
  extrema_list[[measure]] <- ext

  # Maximal age-related change marker from Summary
  mc_age <- safe_num(sm, "MaxChangeAge")
  mc_d   <- safe_num(sm, "DerivativeAtMaxChange")
  maxchange_list[[measure]] <- data.frame(
    Measure = measure,
    Age = mc_age,
    Derivative_per_SD = if (is.finite(mc_d)) mc_d / sample_sd else NA_real_,
    Direction = if ("ChangeDirection" %in% names(sm)) as.character(sm$ChangeDirection[1]) else NA_character_,
    Boundary = if ("MaxChangeAtInteriorBoundary" %in% names(sm)) truthy(sm$MaxChangeAtInteriorBoundary[1]) else FALSE,
    stringsAsFactors = FALSE
  )

  meta_list[[measure]] <- data.frame(
    Measure = measure,
    Region = get_region_name(sm),
    File = basename(f),
    SampleSD = sample_sd,
    stringsAsFactors = FALSE
  )

  summary_raw[[measure]] <- sm
}

# =============================================================================
# 5. VERIFY ALL FIVE MEASURES
# =============================================================================

found_measures <- names(traj_list)
missing_measures <- setdiff(MEASURE_LEVELS, found_measures)
if (length(missing_measures)) {
  stop("Missing measure(s): ", paste(missing_measures, collapse = ", "))
}
if (length(unique(found_measures)) != 5L) {
  stop("The five files do not represent five unique morphometric measures.")
}

# =============================================================================
# 6. COMMON AGE RANGE + REFERENCE AGE
# =============================================================================

age_min_each <- sapply(traj_list, function(z) min(z$Age[is.finite(z$Age)], na.rm = TRUE))
age_max_each <- sapply(traj_list, function(z) max(z$Age[is.finite(z$Age)], na.rm = TRUE))
common_age_min <- max(age_min_each)
common_age_max <- min(age_max_each)

if (!is.finite(common_age_min) || !is.finite(common_age_max) || common_age_min >= common_age_max) {
  stop("The five outcomes do not have a valid overlapping age range.")
}

reference_age <- if (is.null(REFERENCE_AGE)) common_age_min else as.numeric(REFERENCE_AGE)
if (!is.finite(reference_age) || reference_age < common_age_min || reference_age > common_age_max) {
  stop("REFERENCE_AGE must be inside the common age range: ",
       round(common_age_min, 3), " to ", round(common_age_max, 3))
}

cat("\nCommon age range: ", round(common_age_min,2), " - ", round(common_age_max,2), "\n", sep="")
cat("Reference age: ", round(reference_age,2), "\n", sep="")

age_bands_plot <- AGE_BANDS
age_bands_plot$xmin <- pmax(age_bands_plot$xmin, common_age_min)
age_bands_plot$xmax <- pmin(age_bands_plot$xmax, common_age_max)
age_bands_plot <- age_bands_plot[age_bands_plot$xmin < age_bands_plot$xmax, , drop=FALSE]

# =============================================================================
# 7. STANDARDIZE TRAJECTORIES + EXTREMA
# =============================================================================

for (m in MEASURE_LEVELS) {
  z <- traj_list[[m]]
  ref_p50 <- safe_approx(z$Age, z$P50, reference_age)
  if (!is.finite(ref_p50)) stop("Could not compute reference P50 for ", m)

  z$StandardizedTrajectory <- (z$P50 - ref_p50) / z$SampleSD
  z$P025_std <- (z$P025 - ref_p50) / z$SampleSD
  z$P975_std <- (z$P975 - ref_p50) / z$SampleSD
  traj_list[[m]] <- z

  if (nrow(extrema_list[[m]]) > 0) {
    extrema_list[[m]]$StandardizedValue <-
      (extrema_list[[m]]$Value - ref_p50) / z$SampleSD[1]
  }
}

traj_all <- bind_rows(traj_list) %>%
  filter(is.finite(Age), Age >= common_age_min, Age <= common_age_max) %>%
  mutate(Measure = factor(Measure, levels = MEASURE_LEVELS))

deriv_all <- bind_rows(deriv_list) %>%
  filter(is.finite(Age), Age >= common_age_min, Age <= common_age_max) %>%
  mutate(Measure = factor(Measure, levels = MEASURE_LEVELS))

var_all <- bind_rows(variability_list) %>%
  filter(is.finite(Age), Age >= common_age_min, Age <= common_age_max) %>%
  mutate(Measure = factor(Measure, levels = MEASURE_LEVELS))

meta_all <- bind_rows(meta_list)

extrema_all <- bind_rows(extrema_list)
if (nrow(extrema_all)) {
  extrema_all <- extrema_all %>%
    filter(is.finite(Age), Age >= common_age_min, Age <= common_age_max,
           is.finite(StandardizedValue)) %>%
    mutate(
      Measure = factor(Measure, levels = MEASURE_LEVELS),
      PointType = factor(PointType, levels = c("Peak (max)", "Trough (min)"))
    )
}

maxchange_all <- bind_rows(maxchange_list) %>%
  filter(is.finite(Age), Age >= common_age_min, Age <= common_age_max,
         is.finite(Derivative_per_SD)) %>%
  mutate(Measure = factor(Measure, levels = MEASURE_LEVELS))

# =============================================================================
# 8. REGION TITLE
# =============================================================================

if (is.null(REGION_TITLE)) {
  region_candidates <- unique(meta_all$Region[!is.na(meta_all$Region) & nzchar(trimws(meta_all$Region))])
  if (length(region_candidates) == 1L) {
    region_title <- region_candidates
  } else if (length(region_candidates) > 1L) {
    region_title <- paste(region_candidates, collapse = " / ")
    warning("Different Region labels were found: ", region_title)
  } else {
    region_title <- REGION_NAME
  }
} else {
  region_title <- REGION_TITLE
}

safe_region <- gsub("[^A-Za-z0-9_-]+", "_", region_title)

# =============================================================================
# 9. AUTOMATIC CLASSIFICATION + SUMMARY TABLE DATA
# =============================================================================

summary_rows <- lapply(MEASURE_LEVELS, function(m) {
  td <- traj_list[[m]]
  vd <- variability_list[[m]]
  dd <- deriv_list[[m]]
  sm <- summary_raw[[m]]

  td <- td[is.finite(td$Age) & td$Age >= common_age_min & td$Age <= common_age_max, , drop=FALSE]
  vd <- vd[is.finite(vd$Age) & vd$Age >= common_age_min & vd$Age <= common_age_max, , drop=FALSE]
  dd <- dd[is.finite(dd$Age) & dd$Age >= common_age_min & dd$Age <= common_age_max, , drop=FALSE]

  traj_class <- classify_trajectory(td, sm)
  var_info <- classify_variability(vd)
  gradient_class <- classify_gradient(dd)

  peak_age   <- if ("InternalPeak" %in% names(sm) && truthy(sm$InternalPeak[1])) safe_num(sm,"PeakAge") else NA_real_
  trough_age <- if ("InternalTrough" %in% names(sm) && truthy(sm$InternalTrough[1])) safe_num(sm,"TroughAge") else NA_real_
  max_age    <- safe_num(sm,"MaxChangeAge")
  max_boundary <- if ("MaxChangeAtInteriorBoundary" %in% names(sm)) truthy(sm$MaxChangeAtInteriorBoundary[1]) else FALSE
  direction_at_max <- if ("ChangeDirection" %in% names(sm)) first_nonempty(sm$ChangeDirection) else NA_character_
  if (is.na(direction_at_max)) direction_at_max <- "--"

  data.frame(
    Measure = m,
    `Normative type` = traj_class,
    `Variability type` = var_info$class,
    `Gradient type` = gradient_class,
    `Peak age` = fmt_age(peak_age),
    `Trough age` = fmt_age(trough_age),
    `Max-change age` = fmt_age(max_age, max_boundary),
    DirectionAtMaxChange = direction_at_max,
    VariabilityRelativeRange = var_info$rel_range,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
})

summary_table_full <- bind_rows(summary_rows)
summary_table_full$Measure <- factor(summary_table_full$Measure, levels=MEASURE_LEVELS)
summary_table_full <- summary_table_full[order(summary_table_full$Measure), , drop=FALSE]
summary_table_full$Measure <- as.character(summary_table_full$Measure)

# Compact display table used in Panel D
summary_table_display <- summary_table_full[, c(
  "Measure", "Normative type", "Variability type", "Gradient type",
  "Peak age", "Trough age", "Max-change age"
)]

# =============================================================================
# 10. AGE-BAND LABEL HELPERS
# =============================================================================

age_axis_min <- min(floor(reference_age), min(AGE_BANDS$xmin, na.rm = TRUE))
age_axis_max <- AGE_AXIS_MAX

age_band_display_label <- function(band) {
  dplyr::case_when(
    band == "Young" ~ "Young\n20-40",
    band == "Middle-aged" ~ "Middle-aged\n40-60",
    band == "Older" ~ "Older\n60-80",
    TRUE ~ as.character(band)
  )
}

make_age_band_labels <- function(plot_band_df, y_top, y_bottom, top_pad = 0.12) {
  if (!nrow(plot_band_df) || !is.finite(y_top) || !is.finite(y_bottom)) {
    return(data.frame())
  }

  yr <- y_top - y_bottom
  if (!is.finite(yr) || yr <= 0) {
    yr <- max(abs(c(y_top, y_bottom)), na.rm = TRUE)
  }
  if (!is.finite(yr) || yr <= 0) yr <- 1

  out <- plot_band_df
  out$x <- (out$xmin + out$xmax) / 2

  # Dedicated upper label zone.
  out$y <- y_top + top_pad * yr
  out$BandLabel <- vapply(out$Band, age_band_display_label, character(1))
  out
}

traj_band_labels  <- make_age_band_labels(
  age_bands_plot,
  max(traj_all$StandardizedTrajectory, traj_all$P975_std, na.rm = TRUE),
  min(traj_all$StandardizedTrajectory, traj_all$P025_std, na.rm = TRUE)
)

var_y_top <- max(var_all$Conditional_SD_std, na.rm = TRUE)
var_y_bottom <- min(var_all$Conditional_SD_std, na.rm = TRUE)
var_y_range <- var_y_top - var_y_bottom
if (!is.finite(var_y_range) || var_y_range <= 0) {
  var_y_range <- max(abs(c(var_y_top, var_y_bottom)), na.rm = TRUE)
}
if (!is.finite(var_y_range) || var_y_range <= 0) var_y_range <- 1

# Panel B: labels are deliberately higher than in panels A/C.
var_band_labels <- make_age_band_labels(
  age_bands_plot,
  var_y_top,
  var_y_bottom,
  top_pad = 0.28
)

# Explicit upper whitespace reserved only for the age-band labels.
var_panel_ymin <- var_y_bottom - 0.04 * var_y_range
var_panel_ymax <- var_y_top + 0.42 * var_y_range

deriv_band_labels <- make_age_band_labels(
  age_bands_plot,
  max(deriv_all$Derivative_per_SD, deriv_all$Derivative_U95_per_SD, na.rm = TRUE),
  min(deriv_all$Derivative_per_SD, deriv_all$Derivative_L95_per_SD, na.rm = TRUE)
)

# =============================================================================
# 11. COMMON PLOT THEME AND COLORS
# =============================================================================

MEASURE_COLORS <- c(
  "Volume"    = "#1B9E77",
  "Area"      = "#D95F02",
  "Thickness" = "#7570B3",
  "Curvature" = "#E7298A",
  "FD"        = "#66A61E"
)

common_theme <- theme_classic(base_size = 10.5) +
  theme(
    plot.title = element_text(face="bold"),
    plot.tag = element_text(face="bold", size=13),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = margin(t = 24, r = 8, b = 8, l = 8)
  )

# =============================================================================
# 11. PANEL A: STANDARDIZED NORMATIVE TRAJECTORIES
# =============================================================================

p_traj <- ggplot()
if (nrow(age_bands_plot)) {
  p_traj <- p_traj + geom_rect(
    data=age_bands_plot,
    aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf,fill=Band),
    inherit.aes=FALSE, alpha=AGE_BAND_ALPHA, color=NA
  )
}
if (nrow(traj_band_labels)) {
  p_traj <- p_traj + geom_label(
    data = traj_band_labels,
    aes(x = x, y = y, label = BandLabel),
    inherit.aes = FALSE,
    size = 2.7,
    fontface = "bold",
    color = "#444444",
    fill = "white",
    label.size = 0.18,
    label.padding = grid::unit(0.10, "lines"),
    lineheight = 0.92,
    vjust = 0.5
  )
}

if (isTRUE(SHOW_TRAJECTORY_RIBBONS) &&
    any(is.finite(traj_all$P025_std)) && any(is.finite(traj_all$P975_std))) {
  p_traj <- p_traj + geom_ribbon(
    data=traj_all,
    aes(Age,ymin=P025_std,ymax=P975_std,group=Measure,color=Measure),
    alpha=.05, linewidth=0, inherit.aes=FALSE, show.legend=FALSE
  )
}

p_traj <- p_traj +
  geom_hline(yintercept=0, linetype="dashed", linewidth=.4) +
  geom_vline(xintercept=reference_age, linetype="dotted", linewidth=.4) +
  geom_line(data=traj_all,
            aes(Age,StandardizedTrajectory,color=Measure,group=Measure),
            linewidth=1.05, na.rm=TRUE)

if (isTRUE(SHOW_EXTREMA_MARKERS) && nrow(extrema_all)) {
  p_traj <- p_traj + geom_point(
    data=extrema_all,
    aes(Age,StandardizedValue,color=Measure,shape=PointType),
    size=3, stroke=.9, fill="white", na.rm=TRUE,
    # Actual extrema should NOT alter the five-measure color legend.
    show.legend=FALSE
  )
}

# ---------------------------------------------------------------------------
# Dedicated marker legend
# ---------------------------------------------------------------------------
# Force all three scientific marker types to appear in the legend even when
# a particular ROI has no peak or no trough. This also keeps the measure
# legend as clean colored lines only.
marker_legend_df <- data.frame(
  Age = rep(reference_age, 3),
  Y = rep(0, 3),
  MarkerType = factor(
    c(
      "Peak (max)",
      "Trough (min)",
      "Max-change (peak |gradient|)"
    ),
    levels = c(
      "Peak (max)",
      "Trough (min)",
      "Max-change (peak |gradient|)"
    )
  )
)

p_traj <- p_traj +
  geom_point(
    data = marker_legend_df,
    aes(x = Age, y = Y, shape = MarkerType),
    inherit.aes = FALSE,
    alpha = 0,
    color = "black",
    fill = "white",
    size = 3,
    stroke = .9,
    show.legend = TRUE
  ) +
  scale_color_manual(values=MEASURE_COLORS, drop=FALSE) +
  scale_fill_manual(values=AGE_BAND_COLORS, drop=FALSE) +
  scale_shape_manual(
    values=c(
      "Peak (max)"=24,
      "Trough (min)"=25,
      "Max-change (peak |gradient|)"=23
    ),
    limits=c(
      "Peak (max)",
      "Trough (min)",
      "Max-change (peak |gradient|)"
    ),
    drop=FALSE
  ) +
  scale_x_continuous(
    limits = c(age_axis_min, age_axis_max),
    breaks = AGE_AXIS_BREAKS,
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title="Standardized normative trajectories",
    subtitle=paste0("Conditional median relative to age ", sprintf("%.1f",reference_age),
                    "; scaled by outcome sample SD"),
    x="Age (years)", y="Standardized change from reference age (SD)", tag="a"
  ) + common_theme +
  guides(
    fill = "none",
    color = guide_legend(
      order = 1,
      nrow = 1,
      override.aes = list(
        shape = NA,
        linetype = 1,
        linewidth = 1.2
      )
    ),
    shape = guide_legend(
      order = 2,
      nrow = 1,
      override.aes = list(
        alpha = 1,
        color = "black",
        fill = "white",
        size = 3,
        stroke = .9
      )
    )
  )

# =============================================================================
# 12. PANEL B: STANDARDIZED CONDITIONAL VARIABILITY
# =============================================================================

p_var <- ggplot()
if (nrow(age_bands_plot)) {
  p_var <- p_var + geom_rect(
    data=age_bands_plot,
    aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf,fill=Band),
    inherit.aes=FALSE, alpha=AGE_BAND_ALPHA, color=NA, show.legend=FALSE
  )
}
if (nrow(var_band_labels)) {
  p_var <- p_var + geom_label(
    data = var_band_labels,
    aes(x = x, y = y, label = BandLabel),
    inherit.aes = FALSE,
    size = 2.45,
    fontface = "bold",
    color = "#444444",
    fill = "white",
    label.size = 0.16,
    label.padding = grid::unit(0.08, "lines"),
    lineheight = 0.90,
    vjust = 0.5
  )
}

p_var <- p_var +
  geom_line(
    data=var_all,
    aes(Age,Conditional_SD_std,color=Measure,group=Measure),
    linewidth=1.05, na.rm=TRUE
  ) +
  scale_color_manual(values=MEASURE_COLORS, drop=FALSE) +
  scale_fill_manual(values=AGE_BAND_COLORS, drop=FALSE) +
  scale_x_continuous(
    limits = c(age_axis_min, age_axis_max),
    breaks = AGE_AXIS_BREAKS,
    expand = c(0, 0)
  ) +
  coord_cartesian(
    ylim = c(var_panel_ymin, var_panel_ymax),
    clip = "off"
  ) +
  labs(
    title="Standardized conditional variability",
    subtitle="Conditional SD divided by the outcome sample SD",
    x="Age (years)", y="Conditional SD / sample SD", tag="b"
  ) + common_theme +
  theme(legend.position="none")

# =============================================================================
# 13. PANEL C: STANDARDIZED AGE GRADIENTS
# =============================================================================

p_deriv <- ggplot()
if (nrow(age_bands_plot)) {
  p_deriv <- p_deriv + geom_rect(
    data=age_bands_plot,
    aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf,fill=Band),
    inherit.aes=FALSE, alpha=AGE_BAND_ALPHA, color=NA, show.legend=FALSE
  )
}
if (nrow(deriv_band_labels)) {
  p_deriv <- p_deriv + geom_label(
    data = deriv_band_labels,
    aes(x = x, y = y, label = BandLabel),
    inherit.aes = FALSE,
    size = 2.7,
    fontface = "bold",
    color = "#444444",
    fill = "white",
    label.size = 0.18,
    label.padding = grid::unit(0.10, "lines"),
    lineheight = 0.92,
    vjust = 0.5
  )
}

if (isTRUE(SHOW_DERIVATIVE_CI) &&
    any(is.finite(deriv_all$Derivative_L95_per_SD)) &&
    any(is.finite(deriv_all$Derivative_U95_per_SD))) {
  p_deriv <- p_deriv + geom_ribbon(
    data=deriv_all,
    aes(Age,ymin=Derivative_L95_per_SD,ymax=Derivative_U95_per_SD,
        group=Measure,color=Measure),
    alpha=.05, linewidth=0, inherit.aes=FALSE, show.legend=FALSE
  )
}

p_deriv <- p_deriv +
  geom_hline(yintercept=0, linetype="dashed", linewidth=.4) +
  geom_line(
    data=deriv_all,
    aes(Age,Derivative_per_SD,color=Measure,group=Measure),
    linewidth=1.05, na.rm=TRUE
  )

if (isTRUE(SHOW_MAXCHANGE_MARKERS) && nrow(maxchange_all)) {
  p_deriv <- p_deriv + geom_point(
    data=maxchange_all,
    aes(Age,Derivative_per_SD,color=Measure),
    shape=23, size=3.0, stroke=.9, fill="white", show.legend=FALSE
  )
}

p_deriv <- p_deriv +
  scale_color_manual(values=MEASURE_COLORS, drop=FALSE) +
  scale_fill_manual(values=AGE_BAND_COLORS, drop=FALSE) +
  scale_x_continuous(
    limits = c(age_axis_min, age_axis_max),
    breaks = AGE_AXIS_BREAKS,
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title="Standardized cross-sectional age gradients",
    subtitle="Diamond = maximal age-related change (peak |standardized gradient|)",
    x="Age (years)", y="Standardized age gradient (SD/year)", tag="c"
  ) + common_theme +
  theme(legend.position="none")

# =============================================================================
# 14. THREE-PANEL FIGURE + SEPARATE EDITABLE WORD TABLE
# =============================================================================

# Display title
display_title <- REGION_TITLE

# Keep the legend only under panel A, as in the reference batch script.
p_traj_final <- p_traj + theme(
  legend.position = "bottom",
  legend.box = "vertical",
  legend.margin = margin(t = 2, b = 2)
)

p_var_final <- p_var + theme(legend.position = "none")
p_deriv_final <- p_deriv + theme(legend.position = "none")

# Three scientific panels only.
curves_page <- (
  p_traj_final /
    p_var_final /
    p_deriv_final
) +
  plot_layout(
    ncol = 1,
    heights = c(1.14, 1.00, 1.00)
  ) +
  plot_annotation(
    title = paste0(display_title, ": five morphometric measures"),
    subtitle = paste0("Analysis level: ", level_name),
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, hjust = .5),
      plot.subtitle = element_text(size = 10.5, hjust = .5)
    )
  )

print(curves_page)

# =============================================================================
# 15. SAVE CURVES + WORD TABLE + DATA
# =============================================================================

safe_region <- gsub("[^A-Za-z0-9_-]+", "_", group_name)
base_name <- paste0(safe_region, "_5M_VerticalA4")

curves_png_file <- file.path(
  OUTPUT_DIR,
  paste0(base_name, "_Curves.png")
)

curves_pdf_file <- file.path(
  OUTPUT_DIR,
  paste0(base_name, "_Curves.pdf")
)

table_docx_file <- file.path(
  OUTPUT_DIR,
  paste0(base_name, "_Table_EDITABLE.docx")
)

# Curves page is intentionally shorter than full A4 to leave room for
# the thesis caption below the figure after insertion into Word.
ggsave(
  curves_png_file,
  curves_page,
  width = FIG_WIDTH,
  height = FIG_HEIGHT,
  dpi = FIG_DPI,
  bg = "white"
)

ggsave(
  curves_pdf_file,
  curves_page,
  width = FIG_WIDTH,
  height = FIG_HEIGHT,
  bg = "white"
)

# Native, fully editable Word summary table on a separate portrait page.
export_summary_table_docx(
  table_df = summary_table_display,
  file = table_docx_file,
  display_title = display_title,
  level = level_name
)

write.csv(
  traj_all,
  file.path(OUTPUT_DIR, paste0(base_name, "_StandardizedTrajectories.csv")),
  row.names = FALSE
)

write.csv(
  var_all,
  file.path(OUTPUT_DIR, paste0(base_name, "_StandardizedVariability.csv")),
  row.names = FALSE
)

write.csv(
  deriv_all,
  file.path(OUTPUT_DIR, paste0(base_name, "_StandardizedDerivatives.csv")),
  row.names = FALSE
)

write.csv(
  summary_table_full,
  file.path(OUTPUT_DIR, paste0(base_name, "_CurveClassification_Summary.csv")),
  row.names = FALSE
)

if (nrow(extrema_all)) {
  write.csv(
    extrema_all,
    file.path(OUTPUT_DIR, paste0(base_name, "_ExtremaMarkers.csv")),
    row.names = FALSE
  )
}

if (nrow(maxchange_all)) {
  write.csv(
    maxchange_all,
    file.path(OUTPUT_DIR, paste0(base_name, "_MaxChangeMarkers.csv")),
    row.names = FALSE
  )
}

cat("\n============================================================\n")
cat("CODE 07.1 completed for this grouped unit.\n")
cat("Level: ", level_name, "\n", sep = "")
cat("Hemisphere: ", hemisphere_name, "\n", sep = "")
cat("Group: ", group_name, "\n", sep = "")
cat("Reference age: ", round(reference_age, 2), "\n", sep = "")
cat("Curves PNG: ", curves_png_file, "\n", sep = "")
cat("Curves PDF: ", curves_pdf_file, "\n", sep = "")
cat("Editable Word table: ", table_docx_file, "\n", sep = "")
cat("============================================================\n")


  invisible(list(
    Level = level_name,
    Hemisphere = hemisphere_name,
    Group = group_name,
    OutputDir = OUTPUT_DIR
  ))
}

# =============================================================================
# 17. RUN ALL WHOLEBRAIN / HEMISPHERE / LOBE / YEO7 GROUPS
# =============================================================================

run_log <- vector("list", nrow(jobs))

for (i in seq_len(nrow(jobs))) {

  lev  <- as.character(jobs$Level[i])
  hemi <- as.character(jobs$Hemisphere[i])
  grp  <- as.character(jobs$Group[i])

  z <- inventory %>%
    dplyr::filter(Level == lev, Hemisphere == hemi, Group == grp)

  files_i <- vapply(MEASURE_LEVELS, function(m) {
    hit <- z$File[z$Measure == m]
    if (length(hit) != 1L) {
      stop(
        "Expected exactly one workbook for ", lev, " / ", hemi, " / ", grp,
        " / ", m, "; found ", length(hit), "."
      )
    }
    hit[1]
  }, character(1))
  names(files_i) <- MEASURE_LEVELS

  run_log[[i]] <- tryCatch(
    {
      ans <- run_one_group(lev, hemi, grp, files_i)
      data.frame(
        Level = lev,
        Hemisphere = hemi,
        Group = grp,
        Status = "OK",
        Message = "",
        OutputDir = ans$OutputDir,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      message("ERROR in ", lev, " / ", hemi, " / ", grp, ": ", conditionMessage(e))
      data.frame(
        Level = lev,
        Hemisphere = hemi,
        Group = grp,
        Status = "ERROR",
        Message = conditionMessage(e),
        OutputDir = file.path(OUTPUT_ROOT, lev, hemi, grp),
        stringsAsFactors = FALSE
      )
    }
  )
}

run_log_df <- dplyr::bind_rows(run_log)

write.csv(
  run_log_df,
  file.path(OUTPUT_ROOT, "07_1_Grouped_5Measure_RunLog.csv"),
  row.names = FALSE
)

cat("\n============================================================\n")
cat("BrainNormR 07.1 grouped 3-panel curves + editable Word tables completed.\n")
cat("Successful groups: ", sum(run_log_df$Status == "OK"), "\n", sep = "")
cat("Failed groups:     ", sum(run_log_df$Status == "ERROR"), "\n", sep = "")
cat("Run log: ", file.path(OUTPUT_ROOT, "07_1_Grouped_5Measure_RunLog.csv"), "\n", sep = "")
cat("Output root: ", OUTPUT_ROOT, "\n", sep = "")
cat("============================================================\n")
