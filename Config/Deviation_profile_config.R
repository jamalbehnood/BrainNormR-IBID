##############################################################################
# BrainNormR normative-deviation profile analysis configuration
##############################################################################

# Input created by 03_biomarker_module.R
PROFILE_INPUT_FILE <- file.path(
  "Results",
  "Biomarker",
  "MASTER_NORMATIVE_DEVIATIONS_EXPANDED.xlsx"
)

# Output location for the Paper 3 / individualized-deviation analysis.
PROFILE_OUTPUT_DIR <- file.path("Results", "Deviation_Profiles")

# Cross-fitted scores are the prespecified primary individual scores.
# The analysis stops if this column is unavailable or contains no finite data.
PROFILE_SCORE_COLUMN <- "CF_Norm_Z"

# Structural morphology is primary. Cognition is deliberately excluded and
# remains available for a separate brain-cognition analysis.
PROFILE_STRUCTURAL_ONLY <- TRUE

# Anatomical levels used in the primary multiscale analysis. Missing levels are
# reported in the audit but do not stop exploratory runs when at least
# PROFILE_MIN_LEVELS are available.
PROFILE_INCLUDED_LEVELS <- c("WholeBrain", "Hemisphere", "Lobe", "ROI")
PROFILE_INCLUDE_YEO7 <- FALSE
PROFILE_MIN_LEVELS <- 3L

# Avoid double-counting left, right and bilateral versions of the same lobe.
# Recommended primary setting: use bilateral/combined lobe outcomes only.
PROFILE_PRIMARY_LOBE_SCOPE <- "BILATERAL_ONLY"

# Primary and sensitivity thresholds for absolute normative deviation.
PROFILE_THRESHOLDS <- c(1.96, 2.00, 2.58)
PROFILE_PRIMARY_THRESHOLD <- 2.00

# Reproducible resampling settings. Increase to 5000 for the locked manuscript
# analysis if computing time permits.
PROFILE_SEED <- 20260925L
PROFILE_N_PERMUTATIONS <- 2000L
PROFILE_N_STABILITY_REPEATS <- 1000L
PROFILE_STABILITY_MIN_OUTCOMES_PER_BLOCK <- 4L

# Windows-safe optional parallel execution via future.apply.
PROFILE_USE_PARALLEL <- TRUE
PROFILE_N_WORKERS <- 4L

# Coverage rules. The primary analysis requires real participant IDs and no
# duplicate SubjectID x Outcome records. Outcome availability may vary, but
# balanced burden requires at least this proportion of expected blocks.
PROFILE_MIN_BLOCK_COVERAGE <- 0.80

# Figures are written as both 600-dpi PNG and vector PDF.
PROFILE_FIGURE_DPI <- 600L

# Set FALSE only when sourcing the file to inspect/test functions interactively.
BRAINNORMR_PROFILE_AUTORUN <- TRUE
