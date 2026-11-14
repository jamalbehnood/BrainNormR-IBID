# =============================================================================
# BrainNormR
# BATCH 4-PANEL FIVE-MEASURE FIGURES
#
# For every region/group with all five measures:
#   Volume, Area, Thickness, Curvature, FD
#
# creates EXACTLY the same conceptual layout as the one-region script:
#
#   a) Standardized normative trajectories (P50)
#   b) Standardized conditional variability (Conditional_SD / SampleSD)
#   c) Standardized age gradients (Derivative_per_SD)
#   d) Morphometric summary table
#
# Reads, but NEVER refits:
#   Summary
#   CentileCurves
#   DerivativeCurve
#   VariabilityCurve
#
# Output is generated for every complete Region/Group found recursively under
# ROOT_RESULTS_DIR.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(gridExtra)
})

# Editable Word-table export requires these packages.
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

export_summary_table_docx <- function(
  table_df,
  file,
  display_title,
  level
) {

  # Use shorter, portrait-friendly headings while keeping the table editable.
  word_table <- table_df
  names(word_table) <- c(
    "Measure",
    "Normative
type",
    "Variability
type",
    "Gradient
type",
    "Peak
age",
    "Trough
age",
    "Peak |grad|
age"
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

  # Explicit column widths so the table fits an A4 portrait page more reliably.
  ft <- flextable::width(ft, j = 1, width = 0.85)
  ft <- flextable::width(ft, j = 2, width = 1.15)
  ft <- flextable::width(ft, j = 3, width = 1.18)
  ft <- flextable::width(ft, j = 4, width = 1.15)
  ft <- flextable::width(ft, j = 5, width = 0.65)
  ft <- flextable::width(ft, j = 6, width = 0.70)
  ft <- flextable::width(ft, j = 7, width = 0.92)
  ft <- flextable::set_table_properties(
    ft,
    layout = "fixed",
    width = 1
  )

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

  # Keep the Word table on a standard A4 portrait page so it can be copied
  # directly into the thesis with minimal editing.
  doc <- officer::body_end_section_portrait(
    doc,
    w = 8.27,
    h = 11.69
  )

  print(doc, target = file)

  invisible(file)
}

# =============================================================================
# 1. USER SETTINGS
# =============================================================================

ROOT_RESULTS_DIR <- file.path("Results", "Structural")

OUTPUT_ROOT <- file.path(
  ROOT_RESULTS_DIR,
  "Combined_5Measures_AllRegions_CurvesPlusEditableWordTable"
)

# NULL = all levels.
# For your present structural ROI analysis, "ROI" is recommended.
LEVEL_FILTER <- "ROI"

REFERENCE_AGE <- NULL

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

AGE_AXIS_MIN <- 20
AGE_AXIS_MAX <- 80
AGE_AXIS_BREAKS <- c(20, 30, 40, 50, 60, 70, 80)

SHOW_TRAJECTORY_RIBBONS <- FALSE
SHOW_DERIVATIVE_CI      <- FALSE
SHOW_EXTREMA_MARKERS    <- TRUE
SHOW_MAXCHANGE_MARKERS  <- TRUE

# Descriptive classification thresholds only.
TRAJECTORY_FLAT_RANGE_SD <- 0.10
VARIABILITY_FLAT_REL_TOL <- 0.005

FIG_WIDTH  <- 7.8
FIG_HEIGHT <- 9.0
FIG_DPI    <- 600

MEASURE_LEVELS <- c(
  "Volume", "Area", "Thickness", "Curvature", "FD"
)

MEASURE_COLORS <- c(
  "Volume"    = "#1B9E77",
  "Area"      = "#D95F02",
  "Thickness" = "#7570B3",
  "Curvature" = "#E7298A",
  "FD"        = "#66A61E"
)

# =============================================================================
# 2. BASIC CHECKS
# =============================================================================

if (!dir.exists(ROOT_RESULTS_DIR)) {
  stop("ROOT_RESULTS_DIR does not exist:\n", ROOT_RESULTS_DIR)
}

dir.create(OUTPUT_ROOT, recursive = TRUE, showWarnings = FALSE)

all_files <- list.files(
  ROOT_RESULTS_DIR,
  pattern = "^Summary_results_Q1_AllOnly_.*\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

all_files <- all_files[!grepl("^~\\$", basename(all_files))]

if (!length(all_files)) {
  stop(
    "No BrainNormR Summary_results_Q1_AllOnly_*.xlsx files were found under:\n",
    ROOT_RESULTS_DIR
  )
}

cat("\nFound ", length(all_files), " result workbooks.\n", sep = "")

# =============================================================================
# 3. HELPERS
# =============================================================================

first_nonempty <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x)) x[1] else NA_character_
}

safe_chr <- function(df, candidates, fallback = NA_character_) {
  for (nm in candidates) {
    if (nm %in% names(df)) {
      z <- first_nonempty(df[[nm]])
      if (!is.na(z)) return(z)
    }
  }
  fallback
}

safe_num <- function(df, nm) {
  if (!nm %in% names(df)) return(NA_real_)
  suppressWarnings(as.numeric(df[[nm]][1]))
}

truthy <- function(x) {
  if (!length(x) || is.na(x[1])) return(FALSE)
  z <- x[1]
  if (is.logical(z)) return(isTRUE(z))
  if (is.numeric(z)) return(is.finite(z) && z != 0)
  tolower(trimws(as.character(z))) %in% c("true","t","1","yes","y")
}

safe_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9_-]+", "_", as.character(x))
  gsub("_+", "_", x)
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

find_p50_col <- function(df) {
  candidates <- c("P50", "P50_0", "P50.0", "P50th", "Median")
  z <- intersect(candidates, names(df))
  if (length(z)) return(z[1])

  nn <- toupper(gsub("[^A-Za-z0-9]", "", names(df)))
  ii <- which(nn == "P50")
  if (length(ii)) return(names(df)[ii[1]])

  stop("Could not identify P50 column.")
}

find_centile_col <- function(df, pct) {
  target <- paste0(
    "P",
    gsub("\\.", "_", format(pct, trim = TRUE, scientific = FALSE))
  )
  if (target %in% names(df)) return(target)

  target_norm <- toupper(gsub("[^A-Za-z0-9]", "", target))
  nn <- toupper(gsub("[^A-Za-z0-9]", "", names(df)))
  ii <- which(nn == target_norm)

  if (length(ii)) names(df)[ii[1]] else NA_character_
}

safe_approx <- function(x, y, xout) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 2) return(NA_real_)
  approx(x[ok], y[ok], xout = xout, rule = 2)$y
}

fmt_age <- function(x, boundary = FALSE) {
  if (!is.finite(x)) return("--")
  z <- sprintf("%.1f", x)
  if (isTRUE(boundary)) paste0(z, "*") else z
}

segment_slope <- function(age, y, which_part = c("first", "last")) {

  which_part <- match.arg(which_part)

  ok <- is.finite(age) & is.finite(y)
  age <- age[ok]
  y <- y[ok]

  if (length(age) < 6) return(NA_real_)

  ord <- order(age)
  age <- age[ord]
  y <- y[ord]

  n <- length(age)
  k <- max(3L, floor(n / 3))

  ii <- if (which_part == "first") {
    seq_len(k)
  } else {
    (n - k + 1):n
  }

  fit <- tryCatch(
    lm(y[ii] ~ age[ii]),
    error = function(e) NULL
  )

  if (is.null(fit)) return(NA_real_)

  as.numeric(coef(fit)[2])
}

classify_trajectory <- function(traj_df, summary_df) {

  has_peak <-
    all(c("InternalPeak","PeakAge") %in% names(summary_df)) &&
    truthy(summary_df$InternalPeak[1]) &&
    is.finite(safe_num(summary_df, "PeakAge"))

  has_trough <-
    all(c("InternalTrough","TroughAge") %in% names(summary_df)) &&
    truthy(summary_df$InternalTrough[1]) &&
    is.finite(safe_num(summary_df, "TroughAge"))

  if (has_peak && has_trough) return("Complex")
  if (has_peak) return("Peak-shaped")
  if (has_trough) return("Trough-shaped")

  z <- traj_df[
    is.finite(traj_df$Age) &
      is.finite(traj_df$StandardizedTrajectory),
    ,
    drop = FALSE
  ]

  if (nrow(z) < 3) return("Unclassified")

  z <- z[order(z$Age), , drop = FALSE]

  rg <- diff(range(z$StandardizedTrajectory, na.rm = TRUE))

  if (is.finite(rg) && rg < TRAJECTORY_FLAT_RANGE_SD) {
    return("Stable / flat")
  }

  delta <-
    tail(z$StandardizedTrajectory, 1) -
    head(z$StandardizedTrajectory, 1)

  if (!is.finite(delta)) return("Unclassified")

  if (delta > 0) {
    "Monotonic increase"
  } else if (delta < 0) {
    "Monotonic decline"
  } else {
    "Stable / flat"
  }
}

classify_variability <- function(var_df) {

  z <- var_df[
    is.finite(var_df$Age) &
      is.finite(var_df$Conditional_SD_std),
    ,
    drop = FALSE
  ]

  if (nrow(z) < 6) {
    return(list(class = "Unclassified", rel_range = NA_real_))
  }

  z <- z[order(z$Age), , drop = FALSE]

  mu <- mean(z$Conditional_SD_std, na.rm = TRUE)

  rr <- if (is.finite(mu) && abs(mu) > .Machine$double.eps) {
    diff(range(z$Conditional_SD_std, na.rm = TRUE)) / abs(mu)
  } else {
    NA_real_
  }

  if (is.finite(rr) && rr < VARIABILITY_FLAT_REL_TOL) {
    return(list(
      class = "Stable variability",
      rel_range = rr
    ))
  }

  s1 <- segment_slope(
    z$Age,
    z$Conditional_SD_std,
    "first"
  )

  s2 <- segment_slope(
    z$Age,
    z$Conditional_SD_std,
    "last"
  )

  age_span <- diff(range(z$Age))
  y_range <- diff(range(z$Conditional_SD_std))

  slope_tol <- if (
    is.finite(age_span) &&
      age_span > 0 &&
      is.finite(y_range)
  ) {
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

    net <-
      tail(z$Conditional_SD_std, 1) -
      head(z$Conditional_SD_std, 1)

    if (
      is.finite(y_range) &&
        y_range > 0 &&
        is.finite(net) &&
        abs(net) >= 0.50 * y_range
    ) {

      if (net > 0) {
        "Increasing variability"
      } else {
        "Decreasing variability"
      }

    } else {

      "Complex variability"

    }
  }

  list(
    class = cls,
    rel_range = rr
  )
}


# -----------------------------------------------------------------------------
# Descriptive classification of the standardized age-gradient curve (Panel C)
#
# This does NOT alter any model result. It summarizes the observed shape of
# Derivative_per_SD for the table only.
#
# Main classes:
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

  # A very small derivative throughout the age range is treated as
  # effectively age-invariant for descriptive purposes.
  absolute_zero_tol <- 0.0025  # SD/year; table-classification threshold only

  if (max_abs < absolute_zero_tol) {
    return("Near-zero / stable")
  }

  # Relative zero band protects the classification against tiny numerical
  # sign changes close to zero.
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

  # Robust sign sequence across the full curve, ignoring values inside
  # the near-zero tolerance band.
  signs <- ifelse(d > zero_tol, 1L, ifelse(d < -zero_tol, -1L, 0L))
  nz_signs <- signs[signs != 0L]

  if (length(nz_signs)) {
    compressed <- nz_signs[c(TRUE, diff(nz_signs) != 0L)]
    n_sign_changes <- max(0L, length(compressed) - 1L)
  } else {
    compressed <- integer()
    n_sign_changes <- 0L
  }

  # More than one meaningful sign reversal is not summarized as a simple
  # two-phase pattern.
  if (n_sign_changes > 1L) {
    return("Complex")
  }

  if (s_early < 0L && s_late > 0L) {
    return("Negative -> Positive")
  }

  if (s_early > 0L && s_late < 0L) {
    return("Positive -> Negative")
  }

  # If one end lies inside the near-zero band, infer the dominant sign from
  # the other end and describe whether the gradient attenuates toward zero.
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

  # Same-sign curves: compare absolute magnitude early versus late.
  # A 20% difference is required before calling attenuation/acceleration;
  # otherwise the gradient is labelled persistent.
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

  # Fallback when endpoints are both near zero but the interior is not.
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

age_band_display_label <- function(band) {
  dplyr::case_when(
    band == "Young" ~ "Young\n20-40",
    band == "Middle-aged" ~ "Middle-aged\n40-60",
    band == "Older" ~ "Older\n60-80",
    TRUE ~ as.character(band)
  )
}

make_band_label_df <- function(bands, y_top, y_bottom) {

  if (!nrow(bands)) return(data.frame())

  yr <- y_top - y_bottom

  if (!is.finite(yr) || yr <= 0) {
    yr <- max(abs(c(y_top, y_bottom)), na.rm = TRUE)
  }

  if (!is.finite(yr) || yr <= 0) yr <- 1

  out <- bands
  out$x <- (out$xmin + out$xmax) / 2

  # Place labels above the main curves in a reserved whitespace band.
  out$y <- y_top + 0.10 * yr

  out$label <- vapply(
    out$Band,
    age_band_display_label,
    character(1)
  )

  out
}

# =============================================================================
# 4. BUILD FILE INDEX
# =============================================================================

index_rows <- list()
skipped_index <- list()

for (f in all_files) {

  one <- tryCatch({

    sh <- excel_sheets(f)

    required_sheets <- c(
      "Summary",
      "CentileCurves",
      "DerivativeCurve",
      "VariabilityCurve"
    )

    miss <- setdiff(required_sheets, sh)

    if (length(miss)) {
      stop(
        "Missing sheet(s): ",
        paste(miss, collapse = ", ")
      )
    }

    sm <- as.data.frame(
      read_excel(f, sheet = "Summary")
    )

    measure <- detect_measure(sm, f)

    if (is.na(measure)) {
      stop("Could not identify structural measure")
    }

    level <- safe_chr(
      sm,
      c("Level","AnalysisLevel"),
      fallback = "Unknown"
    )

    region <- safe_chr(
      sm,
      c("Region","RegionName","ROI","Group"),
      fallback = NA_character_
    )

    hemi <- safe_chr(
      sm,
      c("Hemisphere","Hemi"),
      fallback = ""
    )

    lobe <- safe_chr(
      sm,
      c("Lobe","LobeName"),
      fallback = ""
    )

    if (is.na(region) || !nzchar(trimws(region))) {
      region <- basename(dirname(f))
      region <- sub(
        paste0("_(", paste(MEASURE_LEVELS, collapse = "|"), ")$"),
        "",
        region,
        ignore.case = TRUE
      )
    }

    data.frame(
      File = f,
      Level = level,
      Region = region,
      Hemisphere = hemi,
      Lobe = lobe,
      Measure = measure,
      stringsAsFactors = FALSE
    )

  }, error = function(e) {

    skipped_index[[length(skipped_index) + 1L]] <<-
      data.frame(
        File = f,
        Reason = conditionMessage(e),
        stringsAsFactors = FALSE
      )

    NULL
  })

  if (!is.null(one)) {
    index_rows[[length(index_rows) + 1L]] <- one
  }
}

if (!length(index_rows)) {
  stop("No valid BrainNormR workbooks could be indexed.")
}

file_index <- bind_rows(index_rows)

if (!is.null(LEVEL_FILTER)) {

  file_index <- file_index[
    tolower(file_index$Level) ==
      tolower(LEVEL_FILTER),
    ,
    drop = FALSE
  ]

  if (!nrow(file_index)) {
    stop(
      "No files remained after LEVEL_FILTER = ",
      LEVEL_FILTER
    )
  }
}

file_index$GroupKey <- paste(
  file_index$Level,
  file_index$Region,
  file_index$Hemisphere,
  file_index$Lobe,
  sep = "|||"
)

group_keys <- unique(file_index$GroupKey)

cat(
  "Detected ",
  length(group_keys),
  " candidate region/group units.\n",
  sep = ""
)

# =============================================================================
# 5. LOGS
# =============================================================================

success_log <- list()
skip_log <- list()

# =============================================================================
# 6. PROCESS EACH REGION/GROUP
# =============================================================================

for (gkey in group_keys) {

  g <- file_index[
    file_index$GroupKey == gkey,
    ,
    drop = FALSE
  ]

  level  <- g$Level[1]
  region <- g$Region[1]
  hemi   <- g$Hemisphere[1]
  lobe   <- g$Lobe[1]

  cat(
    "\n------------------------------------------------------------\n",
    "Processing: ",
    level,
    " | ",
    region,
    if (nzchar(hemi)) paste0(" | ", hemi) else "",
    "\n",
    sep = ""
  )

  counts <- table(
    factor(
      g$Measure,
      levels = MEASURE_LEVELS
    )
  )

  missing_measures <- MEASURE_LEVELS[counts == 0]
  duplicate_measures <- MEASURE_LEVELS[counts > 1]

  if (length(missing_measures) || length(duplicate_measures)) {

    reason <- paste0(
      if (length(missing_measures)) {
        paste0(
          "Missing: ",
          paste(missing_measures, collapse = ", "),
          ". "
        )
      } else "",
      if (length(duplicate_measures)) {
        paste0(
          "Duplicate: ",
          paste(duplicate_measures, collapse = ", "),
          "."
        )
      } else ""
    )

    warning("Skipping ", region, ": ", reason)

    skip_log[[length(skip_log) + 1L]] <-
      data.frame(
        Level = level,
        Region = region,
        Hemisphere = hemi,
        Lobe = lobe,
        Reason = reason,
        stringsAsFactors = FALSE
      )

    next
  }

  traj_list <- list()
  deriv_list <- list()
  variability_list <- list()
  extrema_list <- list()
  maxchange_list <- list()
  summary_raw <- list()
  sample_sd_list <- list()

  region_failed <- FALSE
  fail_reason <- ""

  for (m in MEASURE_LEVELS) {

    f <- g$File[g$Measure == m][1]

    ok <- tryCatch({

      sm <- as.data.frame(
        read_excel(f, sheet = "Summary")
      )

      cc <- as.data.frame(
        read_excel(f, sheet = "CentileCurves")
      )

      dc <- as.data.frame(
        read_excel(f, sheet = "DerivativeCurve")
      )

      vc <- as.data.frame(
        read_excel(f, sheet = "VariabilityCurve")
      )

      if (!"SampleSD" %in% names(sm)) {
        stop("SampleSD missing from Summary")
      }

      sample_sd <- suppressWarnings(
        as.numeric(sm$SampleSD[1])
      )

      if (!is.finite(sample_sd) || sample_sd <= 0) {
        stop("Invalid SampleSD")
      }

      if (!"Age" %in% names(cc)) {
        stop("Age missing from CentileCurves")
      }

      if (!"Age" %in% names(dc)) {
        stop("Age missing from DerivativeCurve")
      }

      if (!all(c("Age","Conditional_SD") %in% names(vc))) {
        stop(
          "VariabilityCurve must contain Age and Conditional_SD"
        )
      }

      p50_col <- find_p50_col(cc)
      p025_col <- find_centile_col(cc, 2.5)
      p975_col <- find_centile_col(cc, 97.5)

      deriv_std <- if (
        "Derivative_per_SD" %in% names(dc)
      ) {

        suppressWarnings(
          as.numeric(dc$Derivative_per_SD)
        )

      } else if ("Derivative" %in% names(dc)) {

        suppressWarnings(
          as.numeric(dc$Derivative)
        ) / sample_sd

      } else {

        stop(
          "Neither Derivative_per_SD nor Derivative exists"
        )
      }

      deriv_lo_std <- rep(
        NA_real_,
        nrow(dc)
      )

      deriv_hi_std <- rep(
        NA_real_,
        nrow(dc)
      )

      if (all(
        c(
          "Derivative_L95",
          "Derivative_U95"
        ) %in% names(dc)
      )) {

        deriv_lo_std <-
          suppressWarnings(
            as.numeric(dc$Derivative_L95)
          ) / sample_sd

        deriv_hi_std <-
          suppressWarnings(
            as.numeric(dc$Derivative_U95)
          ) / sample_sd
      }

      traj_list[[m]] <- data.frame(
        Age = suppressWarnings(as.numeric(cc$Age)),
        P50 = suppressWarnings(as.numeric(cc[[p50_col]])),
        P025 = if (!is.na(p025_col)) {
          suppressWarnings(as.numeric(cc[[p025_col]]))
        } else {
          NA_real_
        },
        P975 = if (!is.na(p975_col)) {
          suppressWarnings(as.numeric(cc[[p975_col]]))
        } else {
          NA_real_
        },
        Measure = m,
        SampleSD = sample_sd,
        stringsAsFactors = FALSE
      )

      deriv_list[[m]] <- data.frame(
        Age = suppressWarnings(as.numeric(dc$Age)),
        Derivative_per_SD = deriv_std,
        Derivative_L95_per_SD = deriv_lo_std,
        Derivative_U95_per_SD = deriv_hi_std,
        Measure = m,
        stringsAsFactors = FALSE
      )

      variability_list[[m]] <- data.frame(
        Age = suppressWarnings(as.numeric(vc$Age)),
        Conditional_SD =
          suppressWarnings(
            as.numeric(vc$Conditional_SD)
          ),
        Conditional_SD_std =
          suppressWarnings(
            as.numeric(vc$Conditional_SD)
          ) / sample_sd,
        Measure = m,
        SampleSD = sample_sd,
        stringsAsFactors = FALSE
      )

      ext <- data.frame(
        Measure = character(),
        Age = numeric(),
        Value = numeric(),
        StandardizedValue = numeric(),
        PointType = character(),
        stringsAsFactors = FALSE
      )

      if (
        all(
          c(
            "InternalPeak",
            "PeakAge",
            "PeakValue"
          ) %in% names(sm)
        ) &&
        truthy(sm$InternalPeak[1])
      ) {

        a <- safe_num(sm, "PeakAge")
        v <- safe_num(sm, "PeakValue")

        if (is.finite(a) && is.finite(v)) {

          ext <- rbind(
            ext,
            data.frame(
              Measure = m,
              Age = a,
              Value = v,
              StandardizedValue = NA_real_,
              PointType = "Peak (max)",
              stringsAsFactors = FALSE
            )
          )
        }
      }

      if (
        all(
          c(
            "InternalTrough",
            "TroughAge",
            "TroughValue"
          ) %in% names(sm)
        ) &&
        truthy(sm$InternalTrough[1])
      ) {

        a <- safe_num(sm, "TroughAge")
        v <- safe_num(sm, "TroughValue")

        if (is.finite(a) && is.finite(v)) {

          ext <- rbind(
            ext,
            data.frame(
              Measure = m,
              Age = a,
              Value = v,
              StandardizedValue = NA_real_,
              PointType = "Trough (min)",
              stringsAsFactors = FALSE
            )
          )
        }
      }

      extrema_list[[m]] <- ext

      mc_age <- safe_num(
        sm,
        "MaxChangeAge"
      )

      mc_d <- safe_num(
        sm,
        "DerivativeAtMaxChange"
      )

      maxchange_list[[m]] <- data.frame(
        Measure = m,
        Age = mc_age,
        Derivative_per_SD =
          if (is.finite(mc_d)) {
            mc_d / sample_sd
          } else {
            NA_real_
          },
        Direction =
          if ("ChangeDirection" %in% names(sm)) {
            as.character(sm$ChangeDirection[1])
          } else {
            NA_character_
          },
        Boundary =
          if (
            "MaxChangeAtInteriorBoundary" %in% names(sm)
          ) {
            truthy(
              sm$MaxChangeAtInteriorBoundary[1]
            )
          } else {
            FALSE
          },
        stringsAsFactors = FALSE
      )

      summary_raw[[m]] <- sm
      sample_sd_list[[m]] <- sample_sd

      TRUE

    }, error = function(e) {

      region_failed <<- TRUE
      fail_reason <<-
        paste0(
          m,
          ": ",
          conditionMessage(e)
        )

      FALSE
    })

    if (!ok) break
  }

  if (region_failed) {

    warning(
      "Skipping ",
      region,
      ": ",
      fail_reason
    )

    skip_log[[length(skip_log) + 1L]] <-
      data.frame(
        Level = level,
        Region = region,
        Hemisphere = hemi,
        Lobe = lobe,
        Reason = fail_reason,
        stringsAsFactors = FALSE
      )

    next
  }

  # ---------------------------------------------------------------------------
  # Common age range + reference age
  # ---------------------------------------------------------------------------

  age_min_each <- sapply(
    traj_list,
    function(z) {
      min(
        z$Age[is.finite(z$Age)],
        na.rm = TRUE
      )
    }
  )

  age_max_each <- sapply(
    traj_list,
    function(z) {
      max(
        z$Age[is.finite(z$Age)],
        na.rm = TRUE
      )
    }
  )

  common_age_min <- max(age_min_each)
  common_age_max <- min(age_max_each)

  if (
    !is.finite(common_age_min) ||
      !is.finite(common_age_max) ||
      common_age_min >= common_age_max
  ) {

    reason <- "No valid common age range"

    skip_log[[length(skip_log) + 1L]] <-
      data.frame(
        Level = level,
        Region = region,
        Hemisphere = hemi,
        Lobe = lobe,
        Reason = reason,
        stringsAsFactors = FALSE
      )

    next
  }

  reference_age <- if (is.null(REFERENCE_AGE)) {
    common_age_min
  } else {
    as.numeric(REFERENCE_AGE)
  }

  if (
    !is.finite(reference_age) ||
      reference_age < common_age_min ||
      reference_age > common_age_max
  ) {

    reason <- paste0(
      "REFERENCE_AGE outside common age range: ",
      round(common_age_min, 2),
      " - ",
      round(common_age_max, 2)
    )

    skip_log[[length(skip_log) + 1L]] <-
      data.frame(
        Level = level,
        Region = region,
        Hemisphere = hemi,
        Lobe = lobe,
        Reason = reason,
        stringsAsFactors = FALSE
      )

    next
  }

  # ---------------------------------------------------------------------------
  # Standardize trajectories and extrema
  # ---------------------------------------------------------------------------

  for (m in MEASURE_LEVELS) {

    z <- traj_list[[m]]

    ref_p50 <- safe_approx(
      z$Age,
      z$P50,
      reference_age
    )

    if (!is.finite(ref_p50)) {

      region_failed <- TRUE
      fail_reason <-
        paste0(
          m,
          ": could not compute P50 at reference age"
        )

      break
    }

    z$StandardizedTrajectory <-
      (z$P50 - ref_p50) /
      z$SampleSD

    z$P025_std <-
      (z$P025 - ref_p50) /
      z$SampleSD

    z$P975_std <-
      (z$P975 - ref_p50) /
      z$SampleSD

    traj_list[[m]] <- z

    if (nrow(extrema_list[[m]]) > 0) {

      extrema_list[[m]]$StandardizedValue <-
        (
          extrema_list[[m]]$Value -
            ref_p50
        ) /
        z$SampleSD[1]
    }
  }

  if (region_failed) {

    skip_log[[length(skip_log) + 1L]] <-
      data.frame(
        Level = level,
        Region = region,
        Hemisphere = hemi,
        Lobe = lobe,
        Reason = fail_reason,
        stringsAsFactors = FALSE
      )

    next
  }

  traj_all <- bind_rows(
    traj_list
  ) %>%
    filter(
      is.finite(Age),
      Age >= common_age_min,
      Age <= common_age_max
    ) %>%
    mutate(
      Measure =
        factor(
          Measure,
          levels = MEASURE_LEVELS
        )
    )

  deriv_all <- bind_rows(
    deriv_list
  ) %>%
    filter(
      is.finite(Age),
      Age >= common_age_min,
      Age <= common_age_max
    ) %>%
    mutate(
      Measure =
        factor(
          Measure,
          levels = MEASURE_LEVELS
        )
    )

  var_all <- bind_rows(
    variability_list
  ) %>%
    filter(
      is.finite(Age),
      Age >= common_age_min,
      Age <= common_age_max
    ) %>%
    mutate(
      Measure =
        factor(
          Measure,
          levels = MEASURE_LEVELS
        )
    )

  extrema_all <- bind_rows(
    extrema_list
  )

  if (nrow(extrema_all)) {

    extrema_all <- extrema_all %>%
      filter(
        is.finite(Age),
        Age >= common_age_min,
        Age <= common_age_max,
        is.finite(StandardizedValue)
      ) %>%
      mutate(
        Measure =
          factor(
            Measure,
            levels = MEASURE_LEVELS
          ),
        PointType =
          factor(
            PointType,
            levels =
              c(
                "Peak (max)",
                "Trough (min)"
              )
          )
      )
  }

  maxchange_all <- bind_rows(
    maxchange_list
  ) %>%
    filter(
      is.finite(Age),
      Age >= common_age_min,
      Age <= common_age_max,
      is.finite(Derivative_per_SD)
    ) %>%
    mutate(
      Measure =
        factor(
          Measure,
          levels = MEASURE_LEVELS
        )
    )

  # ---------------------------------------------------------------------------
  # Age bands + labels
  # ---------------------------------------------------------------------------

  age_bands_plot <- AGE_BANDS

  # The displayed bands remain 20-40 / 40-60 / 60-80.
  # Model curves themselves stop where modeled data stop (e.g. 77).
  age_bands_plot$xmin <-
    pmax(
      age_bands_plot$xmin,
      AGE_AXIS_MIN
    )

  age_bands_plot$xmax <-
    pmin(
      age_bands_plot$xmax,
      AGE_AXIS_MAX
    )

  age_bands_plot <-
    age_bands_plot[
      age_bands_plot$xmin <
        age_bands_plot$xmax,
      ,
      drop = FALSE
    ]

  traj_band_labels <- make_band_label_df(
    age_bands_plot,
    max(
      traj_all$StandardizedTrajectory,
      traj_all$P975_std,
      na.rm = TRUE
    ),
    min(
      traj_all$StandardizedTrajectory,
      traj_all$P025_std,
      na.rm = TRUE
    )
  )

  var_band_labels <- make_band_label_df(
    age_bands_plot,
    max(
      var_all$Conditional_SD_std,
      na.rm = TRUE
    ),
    min(
      var_all$Conditional_SD_std,
      na.rm = TRUE
    )
  )

  deriv_band_labels <- make_band_label_df(
    age_bands_plot,
    max(
      deriv_all$Derivative_per_SD,
      deriv_all$Derivative_U95_per_SD,
      na.rm = TRUE
    ),
    min(
      deriv_all$Derivative_per_SD,
      deriv_all$Derivative_L95_per_SD,
      na.rm = TRUE
    )
  )

  # ---------------------------------------------------------------------------
  # Summary table
  # ---------------------------------------------------------------------------

  summary_rows <- lapply(
    MEASURE_LEVELS,
    function(m) {

      td <- traj_list[[m]]
      vd <- variability_list[[m]]
      dd <- deriv_list[[m]]
      sm <- summary_raw[[m]]

      td <- td[
        is.finite(td$Age) &
          td$Age >= common_age_min &
          td$Age <= common_age_max,
        ,
        drop = FALSE
      ]

      vd <- vd[
        is.finite(vd$Age) &
          vd$Age >= common_age_min &
          vd$Age <= common_age_max,
        ,
        drop = FALSE
      ]

      dd <- dd[
        is.finite(dd$Age) &
          dd$Age >= common_age_min &
          dd$Age <= common_age_max,
        ,
        drop = FALSE
      ]

      traj_class <-
        classify_trajectory(
          td,
          sm
        )

      var_info <-
        classify_variability(
          vd
        )

      gradient_class <-
        classify_gradient(
          dd
        )

      peak_age <-
        if (
          "InternalPeak" %in% names(sm) &&
            truthy(sm$InternalPeak[1])
        ) {
          safe_num(sm, "PeakAge")
        } else {
          NA_real_
        }

      trough_age <-
        if (
          "InternalTrough" %in% names(sm) &&
            truthy(sm$InternalTrough[1])
        ) {
          safe_num(sm, "TroughAge")
        } else {
          NA_real_
        }

      max_age <-
        safe_num(
          sm,
          "MaxChangeAge"
        )

      max_boundary <-
        if (
          "MaxChangeAtInteriorBoundary" %in%
            names(sm)
        ) {
          truthy(
            sm$MaxChangeAtInteriorBoundary[1]
          )
        } else {
          FALSE
        }

      direction_at_max <-
        if (
          "ChangeDirection" %in%
            names(sm)
        ) {
          first_nonempty(
            sm$ChangeDirection
          )
        } else {
          NA_character_
        }

      if (is.na(direction_at_max)) {
        direction_at_max <- "--"
      }

      data.frame(
        Measure = m,
        `Normative type` = traj_class,
        `Variability type` = var_info$class,
        `Gradient type` = gradient_class,
        `Peak age` =
          fmt_age(
            peak_age
          ),
        `Trough age` =
          fmt_age(
            trough_age
          ),
        `Max-change age` =
          fmt_age(
            max_age,
            max_boundary
          ),
        DirectionAtMaxChange = direction_at_max,
        VariabilityRelativeRange =
          var_info$rel_range,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  )

  summary_table_full <-
    bind_rows(
      summary_rows
    )

  summary_table_full$Measure <-
    factor(
      summary_table_full$Measure,
      levels = MEASURE_LEVELS
    )

  summary_table_full <-
    summary_table_full[
      order(summary_table_full$Measure),
      ,
      drop = FALSE
    ]

  summary_table_full$Measure <-
    as.character(
      summary_table_full$Measure
    )

  summary_table_display <-
    summary_table_full[
      ,
      c(
        "Measure",
        "Normative type",
        "Variability type",
        "Gradient type",
        "Peak age",
        "Trough age",
        "Max-change age"
      )
    ]

  # ---------------------------------------------------------------------------
  # Common theme
  # ---------------------------------------------------------------------------

  common_theme <-
    theme_classic(
      base_size = 11.5
    ) +
    theme(
      plot.title =
        element_text(
          face = "bold",
          size = 14
        ),
      plot.subtitle =
        element_text(
          size = 10.8
        ),
      plot.tag =
        element_text(
          face = "bold",
          size = 14
        ),
      axis.title =
        element_text(
          size = 11.0
        ),
      axis.title.y =
        element_text(
          size = 10.5,
          lineheight = 0.95,
          margin = margin(r = 10)
        ),
      axis.title.x =
        element_text(
          size = 11.0,
          margin = margin(t = 5)
        ),
      axis.text =
        element_text(
          size = 10.2
        ),
      legend.position = "bottom",
      legend.title =
        element_blank(),
      legend.text =
        element_text(
          size = 10.2
        ),
      plot.margin =
        margin(
          t = 26,
          r = 12,
          b = 8,
          l = 18
        )
    )

  # ===========================================================================
  # PANEL A: STANDARDIZED NORMATIVE TRAJECTORIES
  # ===========================================================================

  p_traj <- ggplot()

  if (nrow(age_bands_plot)) {

    p_traj <- p_traj +
      geom_rect(
        data = age_bands_plot,
        aes(
          xmin = xmin,
          xmax = xmax,
          ymin = -Inf,
          ymax = Inf,
          fill = Band
        ),
        inherit.aes = FALSE,
        alpha = AGE_BAND_ALPHA,
        color = NA
      )
  }

  if (nrow(traj_band_labels)) {

    p_traj <- p_traj +
      geom_label(
        data = traj_band_labels,
        aes(
          x = x,
          y = y,
          label = label
        ),
        inherit.aes = FALSE,
        size = 2.5,
        fontface = "bold",
        color = "#444444",
        fill = "white",
        linewidth = 0.16,
        label.padding = grid::unit(0.10, "lines"),
        lineheight = 0.92,
        vjust = 0.5
      )
  }

  if (
    isTRUE(
      SHOW_TRAJECTORY_RIBBONS
    ) &&
    any(
      is.finite(
        traj_all$P025_std
      )
    ) &&
    any(
      is.finite(
        traj_all$P975_std
      )
    )
  ) {

    p_traj <- p_traj +
      geom_ribbon(
        data = traj_all,
        aes(
          x = Age,
          ymin = P025_std,
          ymax = P975_std,
          group = Measure,
          color = Measure
        ),
        inherit.aes = FALSE,
        alpha = 0.05,
        linewidth = 0,
        show.legend = FALSE
      )
  }

  p_traj <- p_traj +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    geom_vline(
      xintercept = reference_age,
      linetype = "dotted",
      linewidth = 0.4
    ) +
    geom_line(
      data = traj_all,
      aes(
        Age,
        StandardizedTrajectory,
        color = Measure,
        group = Measure
      ),
      linewidth = 1.05,
      na.rm = TRUE
    )

  if (
    isTRUE(
      SHOW_EXTREMA_MARKERS
    ) &&
    nrow(extrema_all)
  ) {

    p_traj <- p_traj +
      geom_point(
        data = extrema_all,
        aes(
          Age,
          StandardizedValue,
          color = Measure,
          shape = PointType
        ),
        size = 3,
        stroke = 0.9,
        fill = "white",
        na.rm = TRUE,
        show.legend = FALSE
      )
  }

  marker_legend_df <- data.frame(
    Age =
      rep(
        reference_age,
        3
      ),
    Y =
      rep(
        0,
        3
      ),
    MarkerType =
      factor(
        c(
          "Peak (max)",
          "Trough (min)",
          "Max-change (peak |gradient|)"
        ),
        levels =
          c(
            "Peak (max)",
            "Trough (min)",
            "Max-change (peak |gradient|)"
          )
      )
  )

  p_traj <- p_traj +
    geom_point(
      data = marker_legend_df,
      aes(
        x = Age,
        y = Y,
        shape = MarkerType
      ),
      inherit.aes = FALSE,
      alpha = 0,
      color = "black",
      fill = "white",
      size = 3,
      stroke = 0.9,
      show.legend = TRUE
    ) +
    scale_color_manual(
      values = MEASURE_COLORS,
      drop = FALSE
    ) +
    scale_fill_manual(
      values = AGE_BAND_COLORS,
      drop = FALSE
    ) +
    scale_shape_manual(
      values =
        c(
          "Peak (max)" = 24,
          "Trough (min)" = 25,
          "Max-change (peak |gradient|)" = 23
        ),
      limits =
        c(
          "Peak (max)",
          "Trough (min)",
          "Max-change (peak |gradient|)"
        ),
      drop = FALSE
    ) +
    scale_x_continuous(
      limits =
        c(
          AGE_AXIS_MIN,
          AGE_AXIS_MAX
        ),
      breaks =
        AGE_AXIS_BREAKS,
      expand =
        c(
          0,
          0
        )
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title =
        "Standardized normative trajectories",
      subtitle =
        paste0(
          "Conditional median relative to age ",
          sprintf(
            "%.1f",
            reference_age
          ),
          "; scaled by outcome sample SD"
        ),
      x = "Age (years)",
      y =
        "Change from reference age (SD)",
      tag = "a"
    ) +
    common_theme +
    guides(
      fill = "none",
      color =
        guide_legend(
          order = 1,
          nrow = 1,
          override.aes =
            list(
              shape = NA,
              linetype = 1,
              linewidth = 1.2
            )
        ),
      shape =
        guide_legend(
          order = 2,
          nrow = 1,
          override.aes =
            list(
              alpha = 1,
              color = "black",
              fill = "white",
              size = 3,
              stroke = 0.9
            )
        )
    )

  # ===========================================================================
  # PANEL B: STANDARDIZED CONDITIONAL VARIABILITY
  # ===========================================================================

  p_var <- ggplot()

  if (nrow(age_bands_plot)) {

    p_var <- p_var +
      geom_rect(
        data = age_bands_plot,
        aes(
          xmin = xmin,
          xmax = xmax,
          ymin = -Inf,
          ymax = Inf,
          fill = Band
        ),
        inherit.aes = FALSE,
        alpha = AGE_BAND_ALPHA,
        color = NA,
        show.legend = FALSE
      )
  }

  if (nrow(var_band_labels)) {

    p_var <- p_var +
      geom_label(
        data = var_band_labels,
        aes(
          x = x,
          y = y,
          label = label
        ),
        inherit.aes = FALSE,
        size = 2.5,
        fontface = "bold",
        color = "#444444",
        fill = "white",
        linewidth = 0.16,
        label.padding = grid::unit(0.10, "lines"),
        lineheight = 0.92,
        vjust = 0.5
      )
  }

  p_var <- p_var +
    geom_line(
      data = var_all,
      aes(
        Age,
        Conditional_SD_std,
        color = Measure,
        group = Measure
      ),
      linewidth = 1.05,
      na.rm = TRUE
    ) +
    scale_color_manual(
      values =
        MEASURE_COLORS,
      drop = FALSE
    ) +
    scale_fill_manual(
      values =
        AGE_BAND_COLORS,
      drop = FALSE
    ) +
    scale_x_continuous(
      limits =
        c(
          AGE_AXIS_MIN,
          AGE_AXIS_MAX
        ),
      breaks =
        AGE_AXIS_BREAKS,
      expand =
        c(
          0,
          0
        )
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title =
        "Standardized conditional variability",
      subtitle =
        "Conditional SD divided by the outcome sample SD",
      x = "Age (years)",
      y =
        "Conditional SD / sample SD",
      tag = "b"
    ) +
    common_theme +
    theme(
      legend.position = "none"
    )

  # ===========================================================================
  # PANEL C: STANDARDIZED AGE GRADIENTS
  # ===========================================================================

  p_deriv <- ggplot()

  if (nrow(age_bands_plot)) {

    p_deriv <- p_deriv +
      geom_rect(
        data = age_bands_plot,
        aes(
          xmin = xmin,
          xmax = xmax,
          ymin = -Inf,
          ymax = Inf,
          fill = Band
        ),
        inherit.aes = FALSE,
        alpha = AGE_BAND_ALPHA,
        color = NA,
        show.legend = FALSE
      )
  }

  if (nrow(deriv_band_labels)) {

    p_deriv <- p_deriv +
      geom_label(
        data = deriv_band_labels,
        aes(
          x = x,
          y = y,
          label = label
        ),
        inherit.aes = FALSE,
        size = 2.5,
        fontface = "bold",
        color = "#444444",
        fill = "white",
        linewidth = 0.16,
        label.padding = grid::unit(0.10, "lines"),
        lineheight = 0.92,
        vjust = 0.5
      )
  }

  if (
    isTRUE(
      SHOW_DERIVATIVE_CI
    ) &&
    any(
      is.finite(
        deriv_all$Derivative_L95_per_SD
      )
    ) &&
    any(
      is.finite(
        deriv_all$Derivative_U95_per_SD
      )
    )
  ) {

    p_deriv <- p_deriv +
      geom_ribbon(
        data = deriv_all,
        aes(
          Age,
          ymin = Derivative_L95_per_SD,
          ymax = Derivative_U95_per_SD,
          group = Measure,
          color = Measure
        ),
        inherit.aes = FALSE,
        alpha = 0.05,
        linewidth = 0,
        show.legend = FALSE
      )
  }

  p_deriv <- p_deriv +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    geom_line(
      data = deriv_all,
      aes(
        Age,
        Derivative_per_SD,
        color = Measure,
        group = Measure
      ),
      linewidth = 1.05,
      na.rm = TRUE
    )

  if (
    isTRUE(
      SHOW_MAXCHANGE_MARKERS
    ) &&
    nrow(maxchange_all)
  ) {

    p_deriv <- p_deriv +
      geom_point(
        data = maxchange_all,
        aes(
          Age,
          Derivative_per_SD,
          color = Measure
        ),
        shape = 23,
        size = 3.0,
        stroke = 0.9,
        fill = "white",
        show.legend = FALSE
      )
  }

  p_deriv <- p_deriv +
    scale_color_manual(
      values = MEASURE_COLORS,
      drop = FALSE
    ) +
    scale_fill_manual(
      values = AGE_BAND_COLORS,
      drop = FALSE
    ) +
    scale_x_continuous(
      limits =
        c(
          AGE_AXIS_MIN,
          AGE_AXIS_MAX
        ),
      breaks =
        AGE_AXIS_BREAKS,
      expand =
        c(
          0,
          0
        )
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title =
        "Standardized cross-sectional age gradients",
      subtitle =
        "Diamond = maximal age-related change (peak |standardized gradient|)",
      x = "Age (years)",
      y =
        "Age gradient (SD/year)",
      tag = "c"
    ) +
    common_theme +
    theme(
      legend.position = "none"
    )

  # ===========================================================================
  # PANEL D: TABLE
  # ===========================================================================

  table_display <-
    summary_table_display

  names(table_display) <-
    c(
      "Measure",
      "Normative\ntype",
      "Variability\ntype",
      "Gradient\ntype",
      "Peak\nage",
      "Trough\nage",
      "Peak |grad|\nage"
    )

  theme_tbl <-
    ttheme_minimal(
      base_size = 8.5,
      core =
        list(
          fg_params =
            list(
              hjust = 0.5,
              x = 0.5
            ),
          bg_params =
            list(
              fill =
                c(
                  "white",
                  "#F7F7F7"
                ),
              col =
                "#D0D0D0"
            )
        ),
      colhead =
        list(
          fg_params =
            list(
              fontface = "bold",
              hjust = 0.5,
              x = 0.5
            ),
          bg_params =
            list(
              fill = "#EAEAEA",
              col = "#BFBFBF"
            )
        )
    )

  tbl_grob <-
    gridExtra::tableGrob(
      table_display,
      rows = NULL,
      theme = theme_tbl
    )

  tbl_panel_grob <-
    gridExtra::arrangeGrob(
      tbl_grob,
      ncol = 1,
      top =
        grid::textGrob(
          "d   Morphometric summary",
          x = 0.01,
          hjust = 0,
          gp =
            grid::gpar(
              fontsize = 11,
              fontface = "bold"
            )
        ),
      bottom =
        grid::textGrob(
          "* Peak |grad| = age of maximal absolute standardized gradient; * = reliable-interior boundary",
          x = 0.01,
          hjust = 0,
          gp =
            grid::gpar(
              fontsize = 7.5,
              col = "#555555"
            )
        ),
      padding =
        grid::unit(
          0.12,
          "line"
        )
    )

  tbl_tag <-
    patchwork::wrap_elements(
      full = tbl_panel_grob,
      clip = FALSE
    )

  # ===========================================================================
  # COMBINE EXACTLY AS ONE-REGION 2 x 2
  # ===========================================================================

  title_parts <- c(
    region,
    if (nzchar(hemi)) hemi else NULL,
    if (
      nzchar(lobe) &&
        tolower(lobe) !=
          tolower(region)
    ) {
      lobe
    } else {
      NULL
    }
  )

  display_title <-
    paste(
      title_parts,
      collapse = " | "
    )

  # A4-friendly split-page composition:
  # Page 1 = three scientific plots only
  # Page 2 = table only
  p_traj  <- p_traj  + theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = 2, b = 2)
  )
  p_var   <- p_var   + theme(legend.position = "none")
  p_deriv <- p_deriv + theme(legend.position = "none")

  curves_page <- (
    p_traj /
      p_var /
      p_deriv
  ) +
    plot_layout(
      ncol = 1,
      heights = c(1.14, 1.00, 1.00)
    ) +
    plot_annotation(
      title =
        paste0(
          display_title,
          ": five morphometric measures"
        ),
      subtitle =
        paste0(
          "Analysis level: ",
          level
        )
    )

  table_page <- (
    tbl_tag
  ) +
    plot_annotation(
      title =
        paste0(
          display_title,
          ": morphometric summary table"
        ),
      subtitle =
        paste0(
          "Analysis level: ",
          level
        )
    )

  # ---------------------------------------------------------------------------
  # Save
  # ---------------------------------------------------------------------------

  # ---------------------------------------------------------------------------
  # Output folders:
  #   OUTPUT_ROOT/
  #     <Level>/
  #       <Region[_Hemisphere][_Lobe]>/
  #         all figure and CSV files for that single region/group
  # ---------------------------------------------------------------------------

  level_out <- file.path(
    OUTPUT_ROOT,
    safe_filename(level)
  )

  # Windows-safe short output names.
  # `region` is already unique within each analysis level, so repeating
  # hemisphere/lobe in both folder and filename is unnecessary.
  region_folder_name <- safe_filename(region)

  region_out <- file.path(
    level_out,
    region_folder_name
  )

  dir.create(
    region_out,
    recursive = TRUE,
    showWarnings = FALSE
  )

  base_name <- paste0(
    safe_filename(region),
    "_5M_VerticalA4"
  )

  png_file <- file.path(
    region_out,
    paste0(base_name, ".png")
  )

  pdf_file <- file.path(
    region_out,
    paste0(base_name, ".pdf")
  )

  curves_png_file <- file.path(
    region_out,
    paste0(base_name, "_Curves.png")
  )

  curves_pdf_file <- file.path(
    region_out,
    paste0(base_name, "_Curves.pdf")
  )

  table_docx_file <- file.path(
    region_out,
    paste0(base_name, "_Table_EDITABLE.docx")
  )

  # Curves page is intentionally shorter than a full A4 page so that,
  # after insertion into the thesis document, there is still room below the
  # figure for the caption on the same page.
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

  # Export the summary table as a native, fully editable Word table.
  export_summary_table_docx(
    table_df = summary_table_display,
    file = table_docx_file,
    display_title = display_title,
    level = level
  )

  png_file <- curves_png_file
  pdf_file <- curves_pdf_file

  write.csv(
    traj_all,
    file.path(
      region_out,
      paste0(
        base_name,
        "_StandardizedTrajectories.csv"
      )
    ),
    row.names = FALSE
  )

  write.csv(
    var_all,
    file.path(
      region_out,
      paste0(
        base_name,
        "_StandardizedVariability.csv"
      )
    ),
    row.names = FALSE
  )

  write.csv(
    deriv_all,
    file.path(
      region_out,
      paste0(
        base_name,
        "_StandardizedDerivatives.csv"
      )
    ),
    row.names = FALSE
  )

  write.csv(
    summary_table_full,
    file.path(
      region_out,
      paste0(
        base_name,
        "_CurveClassification_Summary.csv"
      )
    ),
    row.names = FALSE
  )

  if (nrow(extrema_all) > 0) {

    write.csv(
      extrema_all,
      file.path(
        region_out,
        paste0(
          base_name,
          "_ExtremaMarkers.csv"
        )
      ),
      row.names = FALSE
    )
  }

  if (nrow(maxchange_all) > 0) {

    write.csv(
      maxchange_all,
      file.path(
        region_out,
        paste0(
          base_name,
          "_MaxChangeMarkers.csv"
        )
      ),
      row.names = FALSE
    )
  }

  success_log[[length(success_log) + 1L]] <-
    data.frame(
      Level = level,
      Region = region,
      Hemisphere = hemi,
      Lobe = lobe,
      OutputFolder = region_out,
      ReferenceAge = reference_age,
      CommonAgeMin = common_age_min,
      CommonAgeMax = common_age_max,
      CurvesPNG = curves_png_file,
      CurvesPDF = curves_pdf_file,
      EditableTableDOCX = table_docx_file,
      PNG = png_file,
      PDF = pdf_file,
      stringsAsFactors = FALSE
    )

  cat(
    "Saved 4-panel figure: ",
    png_file,
    "\n",
    sep = ""
  )
}

# =============================================================================
# 7. WRITE LOGS
# =============================================================================

success_df <- if (length(success_log)) {
  bind_rows(success_log)
} else {
  data.frame()
}

skip_df <- if (length(skip_log)) {
  bind_rows(skip_log)
} else {
  data.frame()
}

skipped_index_df <- if (
  length(skipped_index)
) {
  bind_rows(skipped_index)
} else {
  data.frame()
}

write.csv(
  success_df,
  file.path(
    OUTPUT_ROOT,
    "Batch_4Panel_Success_Log.csv"
  ),
  row.names = FALSE
)

write.csv(
  skip_df,
  file.path(
    OUTPUT_ROOT,
    "Batch_4Panel_Skipped_Groups.csv"
  ),
  row.names = FALSE
)

write.csv(
  skipped_index_df,
  file.path(
    OUTPUT_ROOT,
    "Batch_4Panel_Unindexed_Files.csv"
  ),
  row.names = FALSE
)

write.csv(
  file_index,
  file.path(
    OUTPUT_ROOT,
    "Batch_4Panel_File_Index.csv"
  ),
  row.names = FALSE
)

cat(
  "\n============================================================\n"
)

cat(
  "Batch FOUR-PANEL five-measure figure generation completed.\n"
)

cat(
  "Successful groups: ",
  nrow(success_df),
  "\n",
  sep = ""
)

cat(
  "Skipped groups: ",
  nrow(skip_df),
  "\n",
  sep = ""
)

cat(
  "Output root: ",
  OUTPUT_ROOT,
  "\n",
  sep = ""
)

cat(
  "============================================================\n"
)
