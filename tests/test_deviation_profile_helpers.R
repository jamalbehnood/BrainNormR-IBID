##############################################################################
# Lightweight synthetic smoke test for the deviation-profile module.
# Run from the project root with:
#   Rscript tests/test_deviation_profile_helpers.R
##############################################################################

Sys.setenv(BRAINNORMR_PROFILE_AUTORUN = "FALSE")
source("15_run_normative_deviation_profiles.R")

set.seed(42)
subjects <- sprintf("S%03d", 1:30)
measures <- c("Volume", "Area", "Thickness", "Curvature", "FD")

make_outcomes <- function(level, n_per_measure, bilateral = FALSE) {
  do.call(rbind, lapply(measures, function(measure) {
    outcomes <- paste(level, measure, seq_len(n_per_measure), sep = "_")
    expand.grid(
      SubjectID = subjects,
      Outcome = outcomes,
      stringsAsFactors = FALSE
    ) |>
      transform(
        OutcomeType = measure,
        Level = level,
        Region = level,
        Hemisphere = if (bilateral) "Bilateral" else NA_character_,
        Lobe = if (level == "Lobe") "Frontal" else NA_character_,
        CF_Norm_Z = stats::rnorm(length(SubjectID)),
        SubjectID_IsFallback = FALSE
      )
  }))
}

synthetic <- dplyr::bind_rows(
  make_outcomes("WholeBrain", 1),
  make_outcomes("Lobe", 4, bilateral = TRUE),
  make_outcomes("ROI", 8)
)

PROFILE_INCLUDED_LEVELS <- c("WholeBrain", "Lobe", "ROI")
PROFILE_MIN_LEVELS <- 3L
PROFILE_INCLUDE_YEO7 <- FALSE
PROFILE_PRIMARY_LOBE_SCOPE <- "BILATERAL_ONLY"
PROFILE_SCORE_COLUMN <- "CF_Norm_Z"
PROFILE_STRUCTURAL_ONLY <- TRUE
PROFILE_N_PERMUTATIONS <- 10L
PROFILE_N_STABILITY_REPEATS <- 10L
PROFILE_STABILITY_MIN_OUTCOMES_PER_BLOCK <- 4L
PROFILE_USE_PARALLEL <- FALSE

prepared <- prepare_profile_scores(synthetic)
stopifnot(n_distinct(prepared$SubjectID) == 30L)
stopifnot(n_distinct(prepared$OutcomeType) == 5L)
stopifnot(n_distinct(prepared$Level) == 3L)

burden <- make_burden_table(prepared, "SubjectID", c(1.96, 2.00, 2.58))
stopifnot(nrow(burden) == 30L * 3L)
stopifnot(all(burden$Prop_Extreme >= 0 & burden$Prop_Extreme <= 1))

burden_level <- make_burden_table(
  prepared, c("SubjectID", "Level"), c(1.96, 2.00, 2.58)
)
burden_block <- make_burden_table(
  prepared, c("SubjectID", "Level", "OutcomeType"), c(1.96, 2.00, 2.58)
)
balanced <- make_balanced_burden(burden_level, burden_block)
stopifnot(nrow(balanced) == 30L * 3L)

score_object <- make_score_matrix(prepared)
extreme <- ifelse(abs(score_object$matrix) >= 2, 1, 0)
storage.mode(extreme) <- "double"
block_matrix <- make_block_matrix(burden_block, 2, score_object$subject_ids)

null_1 <- run_independent_outcome_null(extreme, row_mean_na(extreme))
null_2 <- run_block_preserving_null(block_matrix)
stopifnot(nrow(null_1$samples) == PROFILE_N_PERMUTATIONS)
stopifnot(nrow(null_2$samples) == PROFILE_N_PERMUTATIONS)
stopifnot(all(c("Standardized_Difference", "P_Upper_FDR") %in% names(null_1$summary)))
stopifnot(all(c("Standardized_Difference", "P_Upper_FDR") %in% names(null_2$summary)))

metadata <- prepared |>
  dplyr::distinct(Outcome, OutcomeType, Level, Region, Hemisphere, Lobe)
stability <- run_profile_stability(extreme, metadata, block_matrix)
stopifnot(nrow(stability$iterations) == 2L * PROFILE_N_STABILITY_REPEATS)

cat("Deviation-profile synthetic smoke test: PASS\n")
