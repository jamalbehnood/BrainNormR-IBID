##############################################################################
# 04_make_thesis_outputs.R
# BrainNormR comprehensive master-output aggregator
# This script does NOT refit any model.
##############################################################################

suppressPackageStartupMessages({
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Install package: readxl")
  if (!requireNamespace("writexl", quietly = TRUE)) stop("Install package: writexl")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install package: dplyr")
})
library(readxl); library(writexl); library(dplyr)

RESULTS_ROOT <- "Results"
OUT_DIR <- file.path(RESULTS_ROOT, "Thesis_Outputs")
MASTER_RESULTS_FILE <- file.path(OUT_DIR, "MASTER_RESULTS.xlsx")
MASTER_CURVES_FILE <- file.path(OUT_DIR, "MASTER_CURVES.xlsx")
MASTER_PARTICIPANT_FILE <- file.path(OUT_DIR, "MASTER_PARTICIPANT_OUTPUTS.xlsx")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

find_result_workbooks <- function(root = RESULTS_ROOT) {
  x <- list.files(root, pattern = "^Summary_results_Q1_AllOnly_.*\\.xlsx$",
                  recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  x[!grepl("^~\\$", basename(x))]
}

metadata_from_summary <- function(path) {
  sh <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
  if (!"Summary" %in% sh) return(data.frame())
  sm <- tryCatch(
    as.data.frame(readxl::read_excel(path, sheet = "Summary")),
    error = function(e) NULL
  )
  if (is.null(sm) || !nrow(sm)) return(data.frame())
  wanted <- intersect(
    c("Outcome","OutcomeType","Level","Region","Hemisphere","Lobe"),
    names(sm)
  )
  if (!length(wanted)) return(data.frame())
  sm[1, wanted, drop = FALSE]
}

read_sheet_safe <- function(path, sheet) {
  sh <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
  if (!sheet %in% sh) return(NULL)

  x <- tryCatch(
    as.data.frame(readxl::read_excel(path, sheet = sheet)),
    error = function(e) NULL
  )

  if (is.null(x) || !nrow(x)) return(NULL)

  # EngineComparison* uses a column named Level for the GAMLSS search stage
  # (e.g., Level1/Level2), not for the anatomical analysis level.
  # Rename it before adding the actual analysis metadata from Summary.
  if (sheet %in% c("EngineComparisonAll", "EngineComparisonBest") &&
      "Level" %in% names(x)) {
    names(x)[names(x) == "Level"] <- "EngineSelectionLevel"
  }

  # Most non-Summary core sheets do not carry outcome metadata themselves.
  # Add the one-row metadata from Summary so master tables remain traceable.
  if (sheet != "Summary") {
    md <- metadata_from_summary(path)
    if (nrow(md)) {
      for (nm in names(md)) {
        if (!nm %in% names(x)) x[[nm]] <- md[[nm]][1]
      }
    }
  }

  x$SourceFile <- normalizePath(path, winslash = "/", mustWork = FALSE)
  x
}

bind_sheet_from_files <- function(files, sheet) {
  z <- lapply(files, read_sheet_safe, sheet = sheet)
  z <- Filter(Negate(is.null), z)
  if (!length(z)) return(data.frame())
  dplyr::bind_rows(z)
}

keep_existing <- function(df, wanted) {
  wanted <- intersect(wanted, names(df))
  if (!length(wanted)) return(data.frame())
  df[, wanted, drop = FALSE]
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

files <- find_result_workbooks()
if (!length(files)) {
  stop("No completed BrainNormR workbooks found under Results/.")
}
cat("Completed BrainNormR workbooks found: ", length(files), "\n", sep = "")

# Aggregate every unique active core sheet.
Summary <- bind_sheet_from_files(files, "Summary")
ModelSelection <- bind_sheet_from_files(files, "ModelSelection")
NestedCV <- bind_sheet_from_files(files, "NestedCV")
NestedSelection <- bind_sheet_from_files(files, "NestedSelection")
SelectionStability <- bind_sheet_from_files(files, "SelectionStability")
ValidationSummary <- bind_sheet_from_files(files, "ValidationSummary")
BootstrapSummary <- bind_sheet_from_files(files, "BootstrapSummary")
ResidualTests <- bind_sheet_from_files(files, "ResidualTests")
CentileCurves <- bind_sheet_from_files(files, "CentileCurves")
DerivativeCurve <- bind_sheet_from_files(files, "DerivativeCurve")
VariabilityCurve <- bind_sheet_from_files(files, "VariabilityCurve")
NormativeScores <- bind_sheet_from_files(files, "NormativeScores")
AgeGroupDeviation <- bind_sheet_from_files(files, "AgeGroupDeviation")
Calibration <- bind_sheet_from_files(files, "Calibration")
EngineComparisonAll <- bind_sheet_from_files(files, "EngineComparisonAll")
EngineComparisonBest <- bind_sheet_from_files(files, "EngineComparisonBest")
RunMetadata <- bind_sheet_from_files(files, "RunMetadata")
SexInteraction <- bind_sheet_from_files(files, "SexInteraction")

# BH-FDR for Age x Sex sensitivity within OutcomeType x Level families.
if (nrow(Summary) && "AgeSex_P" %in% names(Summary)) {
  Summary$AgeSex_P <- safe_num(Summary$AgeSex_P)
  group_vars <- intersect(c("OutcomeType", "Level"), names(Summary))
  if (length(group_vars)) {
    Summary <- Summary |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::mutate(
        AgeSex_q_BH = {
          p <- AgeSex_P
          q <- rep(NA_real_, length(p))
          ok <- is.finite(p)
          if (any(ok)) q[ok] <- p.adjust(p[ok], method = "BH")
          q
        }
      ) |>
      dplyr::ungroup()
  } else {
    p <- Summary$AgeSex_P
    q <- rep(NA_real_, length(p))
    ok <- is.finite(p)
    if (any(ok)) q[ok] <- p.adjust(p[ok], method = "BH")
    Summary$AgeSex_q_BH <- q
  }
}

MilestoneSummary <- keep_existing(
  Summary,
  c(
    "Outcome","OutcomeType","Level","Region","Hemisphere","Lobe",
    "InternalPeak","PeakStable","PeakBootstrapSupport",
    "PeakAge","PeakAge_L95","PeakAge_U95","PeakValue",
    "InternalTrough","TroughStable","TroughBootstrapSupport",
    "TroughAge","TroughAge_L95","TroughAge_U95","TroughValue",
    "InternalMaxChange","MaxChangeStable","MaxChangeBootstrapSupport",
    "MaxChangeAge","MaxChangeAge_L95","MaxChangeAge_U95",
    "DerivativeAtMaxChange","ChangeDirection","SourceFile"
  )
)

ValidationCompact <- keep_existing(
  Summary,
  c(
    "Outcome","OutcomeType","Level","Region","Hemisphere","Lobe",
    "Family","MuSpec","SigmaSpec",
    "CV_RMSE","CV_MAE","CV_R2","CV_NLL","Coverage95",
    "PIT_p","CrossFit_Z_Mean","CrossFit_Z_SD",
    "BootstrapSuccessProp",
    "FamilySelectionStability","MuSelectionStability","SigmaSelectionStability",
    "Shapiro_p","AD_p","AgeSex_P","AgeSex_q_BH","SourceFile"
  )
)

CalibrationSummary <- data.frame()
if (nrow(Calibration) &&
    all(c("ExpectedProportion","ObservedProportion") %in% names(Calibration))) {
  Calibration$ExpectedProportion <- safe_num(Calibration$ExpectedProportion)
  Calibration$ObservedProportion <- safe_num(Calibration$ObservedProportion)
  if (!"CalibrationError" %in% names(Calibration)) {
    Calibration$CalibrationError <- Calibration$ObservedProportion - Calibration$ExpectedProportion
  } else {
    Calibration$CalibrationError <- safe_num(Calibration$CalibrationError)
  }
  group_vars <- intersect(
    c("Outcome","OutcomeType","Level","Region","Hemisphere","Lobe","SourceFile"),
    names(Calibration)
  )
  CalibrationSummary <- Calibration |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      N_Centiles = sum(is.finite(CalibrationError)),
      MeanAbsCalibrationError =
        if (any(is.finite(CalibrationError))) mean(abs(CalibrationError), na.rm = TRUE) else NA_real_,
      MaxAbsCalibrationError =
        if (any(is.finite(CalibrationError))) max(abs(CalibrationError), na.rm = TRUE) else NA_real_,
      RMSE_CalibrationError =
        if (any(is.finite(CalibrationError))) sqrt(mean(CalibrationError^2, na.rm = TRUE)) else NA_real_,
      .groups = "drop"
    )
}

results_sheets <- list(
  Summary = Summary,
  MilestoneSummary = MilestoneSummary,
  ValidationCompact = ValidationCompact,
  CalibrationSummary = CalibrationSummary,
  ModelSelection = ModelSelection,
  NestedSelection = NestedSelection,
  SelectionStability = SelectionStability,
  ValidationSummary = ValidationSummary,
  BootstrapSummary = BootstrapSummary,
  ResidualTests = ResidualTests,
  AgeGroupDeviation = AgeGroupDeviation,
  Calibration = Calibration,
  EngineComparisonAll = EngineComparisonAll,
  EngineComparisonBest = EngineComparisonBest,
  RunMetadata = RunMetadata,
  SexInteraction = SexInteraction
)
results_sheets <- results_sheets[vapply(results_sheets, function(x) is.data.frame(x) && ncol(x) > 0, logical(1))]
writexl::write_xlsx(results_sheets, MASTER_RESULTS_FILE)

curve_sheets <- list(
  CentileCurves = CentileCurves,
  DerivativeCurve = DerivativeCurve,
  VariabilityCurve = VariabilityCurve
)
curve_sheets <- curve_sheets[vapply(curve_sheets, function(x) is.data.frame(x) && ncol(x) > 0, logical(1))]
writexl::write_xlsx(curve_sheets, MASTER_CURVES_FILE)

participant_sheets <- list(
  NormativeScores = NormativeScores,
  NestedCV = NestedCV
)
participant_sheets <- participant_sheets[vapply(participant_sheets, function(x) is.data.frame(x) && ncol(x) > 0, logical(1))]
writexl::write_xlsx(participant_sheets, MASTER_PARTICIPANT_FILE)

write.csv(
  data.frame(SourceFile = normalizePath(files, winslash = "/", mustWork = FALSE)),
  file.path(OUT_DIR, "MASTER_SOURCE_WORKBOOK_INDEX.csv"),
  row.names = FALSE
)

cat("\n============================================================\n")
cat("BrainNormR thesis/master aggregation completed.\n")
cat("MASTER_RESULTS: ", MASTER_RESULTS_FILE, "\n", sep = "")
cat("MASTER_CURVES: ", MASTER_CURVES_FILE, "\n", sep = "")
cat("MASTER_PARTICIPANT_OUTPUTS: ", MASTER_PARTICIPANT_FILE, "\n", sep = "")
cat("Source workbooks aggregated: ", length(files), "\n", sep = "")
cat("============================================================\n")
