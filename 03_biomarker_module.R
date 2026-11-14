##############################################################################
# 03_biomarker_module_EXPANDED.R
#
# BrainNormR - Expanded normative-deviation / candidate-biomarker module
#
# PURPOSE
# -------
# This module aggregates completed BrainNormR NormativeScores outputs and
# produces outcome-level and participant-level normative-deviation summaries.
#
# PRIMARY SCORE
# -------------
# Cross-fitted normative score (CF_Norm_Z) is used as the PRIMARY individual
# deviation score whenever it exists.
#
# Norm_Z is retained and summarized separately when available.
#
# IMPORTANT
# ---------
# 1. This module DOES NOT estimate brain age.
# 2. It DOES NOT invert Y|Age normative curves to obtain predicted age.
# 3. Its outputs are research-level normative-deviation / candidate-biomarker
#    summaries, not clinically validated biomarkers.
# 4. No BrainNormR models are refitted here.
##############################################################################

suppressPackageStartupMessages({
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Install package: readxl")
  if (!requireNamespace("writexl", quietly = TRUE))
    stop("Install package: writexl")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Install package: dplyr")
})

# =============================================================================
# 1. USER SETTINGS
# =============================================================================

# Root folder that contains completed BrainNormR results.
ROOT_OUT <- "Results"

# Output folder
BIOMARKER_OUT <- file.path(ROOT_OUT, "Biomarker")

# Final workbook name
OUTPUT_FILE <- file.path(
  BIOMARKER_OUT,
  "MASTER_NORMATIVE_DEVIATIONS_EXPANDED.xlsx"
)

# Deviation thresholds reported in the output.
THRESHOLDS <- c(1.96, 2.00, 2.58)

# Primary threshold for the individual "deviation burden" table.
PRIMARY_BURDEN_THRESHOLD <- 2.00

# The primary burden is structural-only. A separate sensitivity sheet retains
# the combined structural + cognitive burden without mixing cognition into the
# prespecified structural summary.
INCLUDE_COGNITION_IN_OVERALL_BURDEN <- FALSE

dir.create(
  BIOMARKER_OUT,
  recursive = TRUE,
  showWarnings = FALSE
)

# =============================================================================
# 2. HELPERS
# =============================================================================

find_result_workbooks <- function(root = ROOT_OUT) {

  x <- list.files(
    root,
    pattern = "^Summary_results_Q1_AllOnly_.*\\.xlsx$",
    recursive = TRUE,
    full.names = TRUE
  )

  # Prevent accidental re-reading of temporary Excel lock files.
  x <- x[!grepl("^~\\$", basename(x))]

  # Prevent reading our own biomarker output if naming ever changes.
  x <- x[!grepl(
    "MASTER_NORMATIVE_DEVIATIONS",
    basename(x),
    ignore.case = TRUE
  )]

  x
}


read_sheet_safe <- function(path, sheet) {

  sh <- tryCatch(
    readxl::excel_sheets(path),
    error = function(e) character()
  )

  if (!sheet %in% sh)
    return(NULL)

  tryCatch(
    as.data.frame(
      readxl::read_excel(
        path,
        sheet = sheet
      )
    ),
    error = function(e) NULL
  )
}


get_first_value <- function(df, candidates, default = NA_character_) {

  for (nm in candidates) {

    if (nm %in% names(df) && nrow(df)) {

      z <- as.character(df[[nm]][1])

      if (!is.na(z) && nzchar(trimws(z)))
        return(z)
    }
  }

  default
}


safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}


safe_mean <- function(x) {
  x <- safe_numeric(x)
  if (!any(is.finite(x))) return(NA_real_)
  mean(x[is.finite(x)])
}


safe_sd <- function(x) {
  x <- safe_numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  stats::sd(x)
}


safe_median <- function(x) {
  x <- safe_numeric(x)
  if (!any(is.finite(x))) return(NA_real_)
  stats::median(x[is.finite(x)])
}


safe_prop <- function(logical_vector) {

  ok <- !is.na(logical_vector)

  if (!any(ok))
    return(NA_real_)

  mean(logical_vector[ok])
}


# =============================================================================
# 3. STANDARDIZE ONE WORKBOOK
# =============================================================================

standardize_one_workbook <- function(path) {

  sm <- read_sheet_safe(
    path,
    "Summary"
  )

  ns <- read_sheet_safe(
    path,
    "NormativeScores"
  )

  if (is.null(sm) || !nrow(sm) ||
      is.null(ns) || !nrow(ns)) {
    return(NULL)
  }

  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------

  meta <- data.frame(
    Outcome = get_first_value(
      sm,
      c("Outcome"),
      basename(path)
    ),
    OutcomeType = get_first_value(
      sm,
      c("OutcomeType"),
      "Unknown"
    ),
    Level = get_first_value(
      sm,
      c("Level", "AnalysisLevel"),
      "Unknown"
    ),
    Region = get_first_value(
      sm,
      c("Region", "RegionName", "ROI", "Group"),
      "Unknown"
    ),
    Hemisphere = get_first_value(
      sm,
      c("Hemisphere", "Hemi"),
      NA_character_
    ),
    Lobe = get_first_value(
      sm,
      c("Lobe", "LobeName"),
      NA_character_
    ),
    stringsAsFactors = FALSE
  )

  meta_cols <- names(meta)

  # Avoid duplicate metadata names if a NormativeScores sheet already
  # contains them.
  duplicate_meta <- intersect(
    meta_cols,
    names(ns)
  )

  if (length(duplicate_meta)) {
    ns <- ns[
      ,
      setdiff(
        names(ns),
        duplicate_meta
      ),
      drop = FALSE
    ]
  }

  ns <- cbind(
    meta[
      rep(1, nrow(ns)),
      meta_cols,
      drop = FALSE
    ],
    ns
  )

  ns$SourceWorkbook <- normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )

  # ---------------------------------------------------------------------------
  # Subject ID
  # ---------------------------------------------------------------------------

  # Current BrainNormR core exports SubjectID. Keep defensive fallbacks for
  # older completed results.
  id_candidates <- c(
    "SubjectID",
    "ParticipantID",
    "Participant",
    "Subject",
    "ID"
  )

  id_found <- intersect(
    id_candidates,
    names(ns)
  )

  if (length(id_found)) {

    names(ns)[
      names(ns) == id_found[1]
    ] <- "SubjectID"

  } else {

    # Do NOT assume row-order matching across different workbooks.
    # A workbook-local fallback ID is created only to keep the raw row traceable;
    # these fallback IDs are excluded from cross-outcome participant burden.
    ns$SubjectID <- paste0(
      "NO_ID__",
      basename(path),
      "__row_",
      seq_len(nrow(ns))
    )

    ns$SubjectID_IsFallback <- TRUE
  }

  if (!"SubjectID_IsFallback" %in% names(ns))
    ns$SubjectID_IsFallback <- FALSE

  # ---------------------------------------------------------------------------
  # Ensure numeric score columns
  # ---------------------------------------------------------------------------

  for (zv in c(
    "Norm_Z",
    "CF_Norm_Z",
    "Norm_Prob",
    "CF_Prob",
    "Norm_Centile"
  )) {

    if (zv %in% names(ns))
      ns[[zv]] <- safe_numeric(ns[[zv]])
  }

  # ---------------------------------------------------------------------------
  # Canonical primary score
  #
  # Cross-fitted score has priority.
  # ---------------------------------------------------------------------------

  # Row-wise fallback: use CF_Norm_Z when available for that participant,
  # otherwise fall back to Norm_Z for that participant.
  cf_z <- if ("CF_Norm_Z" %in% names(ns)) ns$CF_Norm_Z else rep(NA_real_, nrow(ns))
  full_z <- if ("Norm_Z" %in% names(ns)) ns$Norm_Z else rep(NA_real_, nrow(ns))

  ns$Primary_Norm_Z <- ifelse(
    is.finite(cf_z),
    cf_z,
    ifelse(is.finite(full_z), full_z, NA_real_)
  )

  ns$Primary_Score_Type <- ifelse(
    is.finite(cf_z),
    "CF_Norm_Z",
    ifelse(is.finite(full_z), "Norm_Z", NA_character_)
  )

  # Keep centiles matched to the same score type used in each row.
  cf_centile <- if ("CF_Prob" %in% names(ns)) {
    100 * ns$CF_Prob
  } else {
    rep(NA_real_, nrow(ns))
  }

  full_centile <- if ("Norm_Centile" %in% names(ns)) {
    ns$Norm_Centile
  } else if ("Norm_Prob" %in% names(ns)) {
    100 * ns$Norm_Prob
  } else {
    rep(NA_real_, nrow(ns))
  }

  ns$Primary_Centile <- ifelse(
    ns$Primary_Score_Type == "CF_Norm_Z" & is.finite(cf_centile),
    cf_centile,
    ifelse(
      ns$Primary_Score_Type == "Norm_Z" & is.finite(full_centile),
      full_centile,
      NA_real_
    )
  )

  # Retain score-specific centile columns for score-specific summaries.
  ns$CF_Centile <- cf_centile
  ns$NormZ_Centile <- full_centile

  ns
}


# =============================================================================
# 4. OUTCOME-LEVEL SUMMARY FUNCTION
# =============================================================================

make_outcome_summary <- function(scores, z_col, score_label, centile_col = NULL) {

  if (!z_col %in% names(scores))
    return(data.frame())

  dat <- scores

  dat$.Z_tmp <- safe_numeric(
    dat[[z_col]]
  )

  dat |>
    dplyr::group_by(
      OutcomeType,
      Level,
      Region,
      Hemisphere,
      Lobe,
      Outcome
    ) |>
    dplyr::summarise(

      ScoreType = score_label,

      N = sum(
        is.finite(.Z_tmp)
      ),

      Mean_Z = safe_mean(
        .Z_tmp
      ),

      SD_Z = safe_sd(
        .Z_tmp
      ),

      Median_Z = safe_median(
        .Z_tmp
      ),

      Min_Z = if (
        any(is.finite(.Z_tmp))
      ) {
        min(
          .Z_tmp[
            is.finite(.Z_tmp)
          ]
        )
      } else {
        NA_real_
      },

      Max_Z = if (
        any(is.finite(.Z_tmp))
      ) {
        max(
          .Z_tmp[
            is.finite(.Z_tmp)
          ]
        )
      } else {
        NA_real_
      },

      Median_Centile = if (
        !is.null(centile_col) &&
        centile_col %in% names(dat)
      ) {
        safe_median(
          .data[[centile_col]]
        )
      } else {
        NA_real_
      },

      # --------------------- |Z| thresholds ---------------------

      N_absZ_ge_1_96 =
        sum(
          abs(.Z_tmp) >= 1.96,
          na.rm = TRUE
        ),

      Prop_absZ_ge_1_96 =
        safe_prop(
          ifelse(
            is.finite(.Z_tmp),
            abs(.Z_tmp) >= 1.96,
            NA
          )
        ),

      N_absZ_ge_2 =
        sum(
          abs(.Z_tmp) >= 2,
          na.rm = TRUE
        ),

      Prop_absZ_ge_2 =
        safe_prop(
          ifelse(
            is.finite(.Z_tmp),
            abs(.Z_tmp) >= 2,
            NA
          )
        ),

      N_absZ_ge_2_58 =
        sum(
          abs(.Z_tmp) >= 2.58,
          na.rm = TRUE
        ),

      Prop_absZ_ge_2_58 =
        safe_prop(
          ifelse(
            is.finite(.Z_tmp),
            abs(.Z_tmp) >= 2.58,
            NA
          )
        ),

      # ---------------- positive and negative separately ----------------

      N_Z_le_minus2 =
        sum(
          .Z_tmp <= -2,
          na.rm = TRUE
        ),

      Prop_Z_le_minus2 =
        safe_prop(
          ifelse(
            is.finite(.Z_tmp),
            .Z_tmp <= -2,
            NA
          )
        ),

      N_Z_ge_plus2 =
        sum(
          .Z_tmp >= 2,
          na.rm = TRUE
        ),

      Prop_Z_ge_plus2 =
        safe_prop(
          ifelse(
            is.finite(.Z_tmp),
            .Z_tmp >= 2,
            NA
          )
        ),

      .groups = "drop"
    )
}


# =============================================================================
# 5. LEVEL SUMMARY
# =============================================================================

make_level_summary <- function(scores) {

  scores |>
    dplyr::group_by(
      OutcomeType,
      Level
    ) |>
    dplyr::summarise(

      N_Outcomes = dplyr::n_distinct(
        Outcome
      ),

      N_ScoreRows = sum(
        is.finite(
          Primary_Norm_Z
        )
      ),

      Mean_Z = safe_mean(
        Primary_Norm_Z
      ),

      SD_Z = safe_sd(
        Primary_Norm_Z
      ),

      Median_Z = safe_median(
        Primary_Norm_Z
      ),

      Prop_absZ_ge_1_96 =
        safe_prop(
          ifelse(
            is.finite(
              Primary_Norm_Z
            ),
            abs(
              Primary_Norm_Z
            ) >= 1.96,
            NA
          )
        ),

      Prop_absZ_ge_2 =
        safe_prop(
          ifelse(
            is.finite(
              Primary_Norm_Z
            ),
            abs(
              Primary_Norm_Z
            ) >= 2,
            NA
          )
        ),

      Prop_absZ_ge_2_58 =
        safe_prop(
          ifelse(
            is.finite(
              Primary_Norm_Z
            ),
            abs(
              Primary_Norm_Z
            ) >= 2.58,
            NA
          )
        ),

      .groups = "drop"
    )
}


# =============================================================================
# 6. PARTICIPANT DEVIATION BURDEN
# =============================================================================

make_individual_burden <- function(
    scores,
    by_level = FALSE,
    include_cognition = INCLUDE_COGNITION_IN_OVERALL_BURDEN) {

  dat <- scores |>
    dplyr::filter(
      !SubjectID_IsFallback,
      is.finite(
        Primary_Norm_Z
      )
    )

  if (!isTRUE(include_cognition) &&
      !by_level) {

    dat <- dat |>
      dplyr::filter(
        tolower(Level) != "cognition",
        tolower(OutcomeType) != "cognition"
      )
  }

  if (!nrow(dat))
    return(data.frame())

  if (by_level) {

    grp <- c(
      "SubjectID",
      "Level"
    )

  } else {

    grp <- "SubjectID"
  }

  dat |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(grp)
      )
    ) |>
    dplyr::summarise(

      N_Valid_Outcomes =
        dplyr::n(),

      Mean_Primary_Z =
        safe_mean(
          Primary_Norm_Z
        ),

      Median_Primary_Z =
        safe_median(
          Primary_Norm_Z
        ),

      Mean_Abs_Z =
        safe_mean(
          abs(
            Primary_Norm_Z
          )
        ),

      Max_Abs_Z =
        max(
          abs(
            Primary_Norm_Z
          ),
          na.rm = TRUE
        ),

      # ---------------- threshold 1.96 ----------------

      N_absZ_ge_1_96 =
        sum(
          abs(
            Primary_Norm_Z
          ) >= 1.96,
          na.rm = TRUE
        ),

      Prop_absZ_ge_1_96 =
        mean(
          abs(
            Primary_Norm_Z
          ) >= 1.96,
          na.rm = TRUE
        ),

      # ---------------- threshold 2.00 ----------------

      N_absZ_ge_2 =
        sum(
          abs(
            Primary_Norm_Z
          ) >= 2,
          na.rm = TRUE
        ),

      Prop_absZ_ge_2 =
        mean(
          abs(
            Primary_Norm_Z
          ) >= 2,
          na.rm = TRUE
        ),

      N_Negative_Z_le_minus2 =
        sum(
          Primary_Norm_Z <= -2,
          na.rm = TRUE
        ),

      N_Positive_Z_ge_plus2 =
        sum(
          Primary_Norm_Z >= 2,
          na.rm = TRUE
        ),

      # ---------------- threshold 2.58 ----------------

      N_absZ_ge_2_58 =
        sum(
          abs(
            Primary_Norm_Z
          ) >= 2.58,
          na.rm = TRUE
        ),

      Prop_absZ_ge_2_58 =
        mean(
          abs(
            Primary_Norm_Z
          ) >= 2.58,
          na.rm = TRUE
        ),

      .groups = "drop"
    )
}


# =============================================================================
# 7. PARTICIPANT x OUTCOME EXTREME-DEVIATION TABLE
# =============================================================================

make_extreme_deviation_table <- function(scores) {

  dat <- scores |>
    dplyr::filter(
      !SubjectID_IsFallback,
      is.finite(
        Primary_Norm_Z
      ),
      abs(
        Primary_Norm_Z
      ) >= PRIMARY_BURDEN_THRESHOLD
    )

  if (!nrow(dat))
    return(data.frame())

  keep <- c(
    "SubjectID",
    "OutcomeType",
    "Level",
    "Region",
    "Hemisphere",
    "Lobe",
    "Outcome",
    "Primary_Norm_Z",
    "Primary_Centile",
    "Primary_Score_Type",
    "Norm_Z",
    "CF_Norm_Z",
    "ObservedValue",
    "Age",
    "Sex",
    "sex",
    "edu",
    "eTIV",
    "SourceWorkbook"
  )

  keep <- intersect(
    keep,
    names(dat)
  )

  dat |>
    dplyr::select(
      dplyr::all_of(
        keep
      )
    ) |>
    dplyr::arrange(
      SubjectID,
      dplyr::desc(
        abs(
          Primary_Norm_Z
        )
      )
    )
}


# =============================================================================
# 8. MODULE RUNNER
# =============================================================================

run_biomarker_module <- function() {

  cat(
    "\n============================================================\n"
  )

  cat(
    " BrainNormR expanded normative-deviation module\n"
  )

  cat(
    "============================================================\n"
  )

  files <- find_result_workbooks(
    ROOT_OUT
  )

  if (!length(files)) {

    stop(
      "No completed BrainNormR result workbooks found under Results/."
    )
  }

  cat(
    "Completed workbooks found: ",
    length(files),
    "\n",
    sep = ""
  )

  # ---------------------------------------------------------------------------
  # Read and standardize all workbooks
  # ---------------------------------------------------------------------------

  parts <- lapply(
    files,
    standardize_one_workbook
  )

  parts <- Filter(
    Negate(is.null),
    parts
  )

  if (!length(parts)) {

    stop(
      "No readable NormativeScores sheets were found."
    )
  }

  scores <- dplyr::bind_rows(
    parts
  )

  if (!"Primary_Norm_Z" %in% names(scores) ||
      !any(is.finite(scores$Primary_Norm_Z))) {

    stop(
      "No usable CF_Norm_Z or Norm_Z values were found."
    )
  }

  # ---------------------------------------------------------------------------
  # Outcome summaries
  # ---------------------------------------------------------------------------

  summary_primary <- make_outcome_summary(
    scores = scores,
    z_col = "Primary_Norm_Z",
    score_label = "PRIMARY: row-wise CF_Norm_Z when available, otherwise Norm_Z",
    centile_col = "Primary_Centile"
  )

  summary_cf <- if (
    "CF_Norm_Z" %in% names(scores) &&
    any(is.finite(scores$CF_Norm_Z))
  ) {

    make_outcome_summary(
      scores = scores,
      z_col = "CF_Norm_Z",
      score_label = "CF_Norm_Z",
      centile_col = "CF_Centile"
    )

  } else {

    data.frame()
  }

  summary_full <- if (
    "Norm_Z" %in% names(scores) &&
    any(is.finite(scores$Norm_Z))
  ) {

    make_outcome_summary(
      scores = scores,
      z_col = "Norm_Z",
      score_label = "Norm_Z",
      centile_col = "NormZ_Centile"
    )

  } else {

    data.frame()
  }

  # ---------------------------------------------------------------------------
  # Level summaries
  # ---------------------------------------------------------------------------

  level_summary <- make_level_summary(
    scores
  )

  # ---------------------------------------------------------------------------
  # Participant burden
  # ---------------------------------------------------------------------------

  individual_burden_all <- make_individual_burden(
    scores,
    by_level = FALSE,
    include_cognition = FALSE
  )

  individual_burden_all_plus_cognition <- make_individual_burden(
    scores,
    by_level = FALSE,
    include_cognition = TRUE
  )

  individual_burden_level <- make_individual_burden(
    scores,
    by_level = TRUE
  )

  # ---------------------------------------------------------------------------
  # List of individual extreme deviations
  # ---------------------------------------------------------------------------

  extreme_deviations <- make_extreme_deviation_table(
    scores
  )

  # ---------------------------------------------------------------------------
  # Data-quality / audit information
  # ---------------------------------------------------------------------------

  audit <- data.frame(
    Item = c(
      "Workbooks_found",
      "Workbooks_with_readable_NormativeScores",
      "Total_score_rows",
      "Unique_subjects_with_real_ID",
      "Rows_using_CF_Norm_Z",
      "Rows_using_Norm_Z_fallback",
      "Rows_with_primary_Z",
      "Primary_burden_threshold_absZ",
      "Overall_burden_includes_cognition",
      "Sensitivity_burden_includes_cognition"
    ),
    Value = c(
      length(files),
      length(parts),
      nrow(scores),
      length(
        unique(
          scores$SubjectID[
            !scores$SubjectID_IsFallback
          ]
        )
      ),
      sum(
        scores$Primary_Score_Type == "CF_Norm_Z",
        na.rm = TRUE
      ),
      sum(
        scores$Primary_Score_Type == "Norm_Z",
        na.rm = TRUE
      ),
      sum(
        is.finite(
          scores$Primary_Norm_Z
        )
      ),
      PRIMARY_BURDEN_THRESHOLD,
      FALSE,
      TRUE
    ),
    stringsAsFactors = FALSE
  )

  notes <- data.frame(
    Note = c(
      "Primary_Norm_Z uses row-wise CF_Norm_Z whenever that participant has a valid cross-fitted value; Norm_Z is used only as the row-wise fallback.",
      "OutcomeSummary_CF and OutcomeSummary_NormZ are reported separately with centiles matched to the corresponding score type when available.",
      "IndividualBurden_All is structural-only; IndividualBurden_AllPlusCog is a separate structural-plus-cognition sensitivity analysis.",
      "IndividualDeviationBurden counts how many completed outcomes for each participant exceed the selected normative-deviation thresholds.",
      "Deviation burden is affected by the number and type of outcomes included; compare burden only across participants with comparable outcome availability.",
      "Positive and negative extreme deviations are counted separately at |Z| >= 2.",
      "The thresholds 1.96, 2.00 and 2.58 are descriptive research cut points; they are not diagnostic thresholds.",
      "This module does not estimate brain age and does not invert outcome-given-age normative curves.",
      "These outputs are candidate research biomarkers and require external validation before clinical interpretation."
    ),
    stringsAsFactors = FALSE
  )

  # ---------------------------------------------------------------------------
  # Workbook
  # ---------------------------------------------------------------------------

  sheets <- list(
    NormativeScores_All = scores,
    OutcomeSummary_Primary = summary_primary,
    LevelSummary_Primary = level_summary,
    IndividualBurden_All = individual_burden_all,
    IndividualBurden_AllPlusCog = individual_burden_all_plus_cognition,
    IndividualBurden_Level = individual_burden_level,
    ExtremeDeviations = extreme_deviations,
    Audit = audit,
    Notes = notes
  )

  if (nrow(summary_cf))
    sheets$OutcomeSummary_CF <- summary_cf

  if (nrow(summary_full))
    sheets$OutcomeSummary_NormZ <- summary_full

  # writexl does not accept zero-column data frames in some versions.
  sheets <- sheets[
    vapply(
      sheets,
      function(x) {
        is.data.frame(x) &&
          ncol(x) > 0
      },
      logical(1)
    )
  ]

  writexl::write_xlsx(
    sheets,
    OUTPUT_FILE
  )

  cat(
    "\nOutput written to:\n",
    OUTPUT_FILE,
    "\n",
    sep = ""
  )

  cat(
    "\nPrimary individual score: CF_Norm_Z when available.\n"
  )

  cat(
    "No BrainNormR model was refitted.\n"
  )

  cat(
    "============================================================\n"
  )

  invisible(
    list(
      scores = scores,
      outcome_summary_primary = summary_primary,
      outcome_summary_cf = summary_cf,
      outcome_summary_normz = summary_full,
      level_summary = level_summary,
      individual_burden_all = individual_burden_all,
      individual_burden_all_plus_cognition = individual_burden_all_plus_cognition,
      individual_burden_level = individual_burden_level,
      extreme_deviations = extreme_deviations,
      audit = audit
    )
  )
}


# =============================================================================
# 9. RUN
# =============================================================================

biomarker_results <- run_biomarker_module()
