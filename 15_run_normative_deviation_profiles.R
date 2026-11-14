##############################################################################
# 15_run_normative_deviation_profiles.R
#
# Individualized multiscale normative-deviation burden and profile analysis.
#
# This post-processing module does not fit or refit normative models. It reads
# the cross-fitted participant-level scores created by 03_biomarker_module.R
# and provides the inferential and stability analyses required for a standalone
# individualized-deviation manuscript.
##############################################################################

suppressPackageStartupMessages({
  required_packages <- c(
    "readxl", "writexl", "dplyr", "tidyr", "ggplot2", "scales"
  )
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages)) {
    stop(
      "Install required packages before running this module: ",
      paste(missing_packages, collapse = ", ")
    )
  }
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

CONFIG_FILE <- file.path("Config", "Deviation_profile_config.R")
if (!file.exists(CONFIG_FILE)) {
  stop("Missing configuration file: ", CONFIG_FILE)
}
source(CONFIG_FILE, local = FALSE)

# =============================================================================
# Helpers
# =============================================================================

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

safe_mean <- function(x) {
  x <- safe_numeric(x)
  if (!any(is.finite(x))) return(NA_real_)
  mean(x[is.finite(x)])
}

safe_sd <- function(x) {
  x <- safe_numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  stats::sd(x)
}

safe_quantile <- function(x, probability) {
  x <- safe_numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probability, names = FALSE, type = 7))
}

safe_spearman <- function(x, y) {
  x <- safe_numeric(x)
  y <- safe_numeric(y)
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3L || stats::sd(x[ok]) == 0 || stats::sd(y[ok]) == 0) {
    return(NA_real_)
  }
  suppressWarnings(stats::cor(x[ok], y[ok], method = "spearman"))
}

row_mean_na <- function(x) {
  out <- rowMeans(x, na.rm = TRUE)
  out[rowSums(is.finite(x)) == 0L] <- NA_real_
  out
}

gini_nonnegative <- function(x) {
  x <- safe_numeric(x)
  x <- x[is.finite(x)]
  if (!length(x) || sum(x) <= 0) return(NA_real_)
  x <- sort(pmax(x, 0))
  n <- length(x)
  (2 * sum(seq_len(n) * x) / (n * sum(x))) - ((n + 1) / n)
}

top_set <- function(x, proportion = 0.10) {
  x <- safe_numeric(x)
  ok <- which(is.finite(x))
  if (!length(ok)) return(integer())
  n_top <- max(1L, ceiling(proportion * length(ok)))
  ok[order(x[ok], decreasing = TRUE)[seq_len(n_top)]]
}

jaccard_index <- function(a, b) {
  a <- unique(a)
  b <- unique(b)
  union_n <- length(union(a, b))
  if (!union_n) return(NA_real_)
  length(intersect(a, b)) / union_n
}

row_cosine <- function(a, b) {
  if (!all(dim(a) == dim(b))) stop("Cosine matrices must have equal dimensions.")
  numerator <- rowSums(a * b, na.rm = TRUE)
  denominator <- sqrt(rowSums(a^2, na.rm = TRUE)) *
    sqrt(rowSums(b^2, na.rm = TRUE))
  out <- numerator / denominator
  both_zero <- rowSums(abs(a), na.rm = TRUE) == 0 &
    rowSums(abs(b), na.rm = TRUE) == 0
  out[both_zero] <- 1
  out[!is.finite(out)] <- NA_real_
  out
}

normalise_level <- function(x) {
  z <- trimws(as.character(x))
  out <- z
  out[tolower(z) %in% c("roi", "region", "regional")] <- "ROI"
  out[tolower(z) %in% c("lobe", "lobar")] <- "Lobe"
  out[tolower(z) %in% c("hemisphere", "hemispheric")] <- "Hemisphere"
  out[tolower(z) %in% c("wholebrain", "whole brain", "whole-brain", "global")] <- "WholeBrain"
  out[tolower(z) %in% c("yeo7", "network", "networks")] <- "Yeo7"
  out[tolower(z) %in% c("cognition", "cognitive")] <- "Cognition"
  out
}

normalise_measure <- function(x) {
  z <- trimws(as.character(x))
  out <- z
  out[tolower(z) %in% c("volume", "vol")] <- "Volume"
  out[tolower(z) %in% c("area", "surface area", "surfacearea")] <- "Area"
  out[tolower(z) %in% c("thickness", "ct")] <- "Thickness"
  out[tolower(z) %in% c("curvature", "mean curvature", "meancurvature")] <- "Curvature"
  out[tolower(z) %in% c("fd", "fractal", "fractal dimension") ] <- "FD"
  out
}

is_cognition_row <- function(outcome_type, level) {
  tolower(trimws(as.character(outcome_type))) == "cognition" |
    tolower(trimws(as.character(level))) == "cognition"
}

is_bilateral_lobe <- function(hemisphere, outcome) {
  h <- tolower(trimws(as.character(hemisphere)))
  h[is.na(h)] <- ""
  h %in% c("bilateral", "both", "combined", "all") |
    grepl("^bilateral_", as.character(outcome), ignore.case = TRUE)
}

summarise_vector <- function(x) {
  x <- safe_numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(c(
      Mean = NA_real_, SD = NA_real_, Variance = NA_real_, IQR = NA_real_,
      P90 = NA_real_, P95 = NA_real_, Maximum = NA_real_, Gini = NA_real_
    ))
  }
  c(
    Mean = mean(x),
    SD = if (length(x) > 1L) stats::sd(x) else NA_real_,
    Variance = if (length(x) > 1L) stats::var(x) else NA_real_,
    IQR = stats::IQR(x, na.rm = TRUE),
    P90 = safe_quantile(x, 0.90),
    P95 = safe_quantile(x, 0.95),
    Maximum = max(x),
    Gini = gini_nonnegative(x)
  )
}

mean_off_diagonal_spearman <- function(x) {
  if (ncol(x) < 2L) return(NA_real_)
  r <- suppressWarnings(stats::cor(x, method = "spearman", use = "pairwise.complete.obs"))
  safe_mean(r[upper.tri(r)])
}

profile_map <- function(indices, function_to_run) {
  use_parallel <- isTRUE(PROFILE_USE_PARALLEL) &&
    length(indices) > 1L &&
    requireNamespace("future", quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE)

  if (!use_parallel) {
    return(lapply(indices, function_to_run))
  }

  previous_plan <- future::plan()
  on.exit(future::plan(previous_plan), add = TRUE)
  workers <- max(1L, min(as.integer(PROFILE_N_WORKERS), length(indices)))
  future::plan(future::multisession, workers = workers)
  future.apply::future_lapply(
    indices,
    function_to_run,
    future.seed = TRUE,
    future.packages = character()
  )
}

save_figure_pair <- function(plot_object, file_stub, width, height) {
  figure_dir <- file.path(PROFILE_OUTPUT_DIR, "Figures")
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  png_path <- file.path(figure_dir, paste0(file_stub, ".png"))
  pdf_path <- file.path(figure_dir, paste0(file_stub, ".pdf"))
  ggplot2::ggsave(
    png_path, plot_object, width = width, height = height,
    dpi = PROFILE_FIGURE_DPI, bg = "white"
  )
  ggplot2::ggsave(
    pdf_path, plot_object, width = width, height = height, bg = "white"
  )
  c(PNG = png_path, PDF = pdf_path)
}

# =============================================================================
# Data preparation and burden tables
# =============================================================================

read_profile_input <- function() {
  if (!file.exists(PROFILE_INPUT_FILE)) {
    stop(
      "Missing biomarker workbook: ", PROFILE_INPUT_FILE,
      ". Run 03_biomarker_module.R first."
    )
  }
  available_sheets <- readxl::excel_sheets(PROFILE_INPUT_FILE)
  if (!"NormativeScores_All" %in% available_sheets) {
    stop("The biomarker workbook has no NormativeScores_All sheet.")
  }
  as.data.frame(
    readxl::read_excel(PROFILE_INPUT_FILE, sheet = "NormativeScores_All")
  )
}

prepare_profile_scores <- function(scores) {
  required_columns <- c(
    "SubjectID", "Outcome", "OutcomeType", "Level", PROFILE_SCORE_COLUMN
  )
  missing_columns <- setdiff(required_columns, names(scores))
  if (length(missing_columns)) {
    stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
  }

  for (optional_column in c("Region", "Hemisphere", "Lobe")) {
    if (!optional_column %in% names(scores)) scores[[optional_column]] <- NA_character_
  }

  scores$Level <- normalise_level(scores$Level)
  scores$OutcomeType <- normalise_measure(scores$OutcomeType)
  scores$Profile_Z <- safe_numeric(scores[[PROFILE_SCORE_COLUMN]])

  if (!"SubjectID_IsFallback" %in% names(scores)) {
    scores$SubjectID_IsFallback <- FALSE
  }

  scores <- scores %>%
    filter(
      !is.na(SubjectID),
      nzchar(trimws(as.character(SubjectID))),
      !SubjectID_IsFallback,
      is.finite(Profile_Z)
    )

  if (isTRUE(PROFILE_STRUCTURAL_ONLY)) {
    scores <- scores %>%
      filter(!is_cognition_row(OutcomeType, Level))
  }

  included_levels <- PROFILE_INCLUDED_LEVELS
  if (isTRUE(PROFILE_INCLUDE_YEO7)) {
    included_levels <- unique(c(included_levels, "Yeo7"))
  }
  scores <- scores %>% filter(Level %in% included_levels)

  if (identical(toupper(PROFILE_PRIMARY_LOBE_SCOPE), "BILATERAL_ONLY")) {
    bilateral_flag <- is_bilateral_lobe(scores$Hemisphere, scores$Outcome)
    scores <- scores[scores$Level != "Lobe" | bilateral_flag, , drop = FALSE]
  }

  if (!nrow(scores)) stop("No eligible profile-analysis rows remain after filtering.")

  duplicate_keys <- scores %>%
    count(SubjectID, Outcome, name = "N") %>%
    filter(N > 1L)

  if (nrow(duplicate_keys)) {
    duplicate_file <- file.path(PROFILE_OUTPUT_DIR, "DUPLICATE_SUBJECT_OUTCOME_KEYS.csv")
    dir.create(PROFILE_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(duplicate_keys, duplicate_file, row.names = FALSE)
    stop(
      "Duplicate SubjectID x Outcome records were detected. See: ",
      duplicate_file
    )
  }

  outcome_metadata_conflicts <- scores %>%
    distinct(Outcome, OutcomeType, Level, Region, Hemisphere, Lobe) %>%
    count(Outcome, name = "N_Metadata_Rows") %>%
    filter(N_Metadata_Rows > 1L)

  if (nrow(outcome_metadata_conflicts)) {
    stop("One or more outcomes have conflicting metadata rows.")
  }

  available_levels <- sort(unique(scores$Level))
  if (length(available_levels) < as.integer(PROFILE_MIN_LEVELS)) {
    stop(
      "Only ", length(available_levels), " eligible anatomical levels are present; ",
      "at least ", PROFILE_MIN_LEVELS, " are required."
    )
  }

  scores$SubjectID <- as.character(scores$SubjectID)
  scores$Outcome <- as.character(scores$Outcome)
  scores$Block <- paste(scores$Level, scores$OutcomeType, sep = "__")
  scores
}

make_burden_table <- function(scores, group_variables, thresholds) {
  output <- lapply(thresholds, function(threshold) {
    scores %>%
      group_by(across(all_of(group_variables))) %>%
      summarise(
        Threshold = threshold,
        N_Valid_Outcomes = n(),
        Mean_Z = safe_mean(Profile_Z),
        Median_Z = stats::median(Profile_Z, na.rm = TRUE),
        Mean_Abs_Z = safe_mean(abs(Profile_Z)),
        Max_Abs_Z = max(abs(Profile_Z), na.rm = TRUE),
        N_Extreme = sum(abs(Profile_Z) >= threshold, na.rm = TRUE),
        Prop_Extreme = mean(abs(Profile_Z) >= threshold, na.rm = TRUE),
        N_Negative = sum(Profile_Z <= -threshold, na.rm = TRUE),
        Prop_Negative = mean(Profile_Z <= -threshold, na.rm = TRUE),
        N_Positive = sum(Profile_Z >= threshold, na.rm = TRUE),
        Prop_Positive = mean(Profile_Z >= threshold, na.rm = TRUE),
        .groups = "drop"
      )
  })
  bind_rows(output)
}

make_balanced_burden <- function(burden_by_level, burden_by_block) {
  balanced_level <- burden_by_level %>%
    group_by(SubjectID, Threshold) %>%
    summarise(
      N_Levels_Available = sum(is.finite(Prop_Extreme)),
      Balanced_Level_Burden = safe_mean(Prop_Extreme),
      .groups = "drop"
    )

  balanced_block <- burden_by_block %>%
    group_by(SubjectID, Threshold) %>%
    summarise(
      N_Blocks_Available = sum(is.finite(Prop_Extreme)),
      Balanced_LevelMeasure_Burden = safe_mean(Prop_Extreme),
      .groups = "drop"
    )

  full_join(
    balanced_level,
    balanced_block,
    by = c("SubjectID", "Threshold")
  )
}

make_score_matrix <- function(scores) {
  wide <- scores %>%
    select(SubjectID, Outcome, Profile_Z) %>%
    tidyr::pivot_wider(names_from = Outcome, values_from = Profile_Z) %>%
    arrange(SubjectID)
  subject_ids <- as.character(wide$SubjectID)
  score_matrix <- as.matrix(wide[, setdiff(names(wide), "SubjectID"), drop = FALSE])
  storage.mode(score_matrix) <- "double"
  rownames(score_matrix) <- subject_ids
  list(subject_ids = subject_ids, matrix = score_matrix)
}

make_block_matrix <- function(burden_by_block, threshold, subject_order) {
  wide <- burden_by_block %>%
    filter(abs(Threshold - threshold) < 1e-10) %>%
    mutate(Block = paste(Level, OutcomeType, sep = "__")) %>%
    select(SubjectID, Block, Prop_Extreme) %>%
    tidyr::pivot_wider(names_from = Block, values_from = Prop_Extreme)
  wide <- wide[match(subject_order, wide$SubjectID), , drop = FALSE]
  block_matrix <- as.matrix(wide[, setdiff(names(wide), "SubjectID"), drop = FALSE])
  storage.mode(block_matrix) <- "double"
  rownames(block_matrix) <- subject_order
  block_matrix
}

# =============================================================================
# Threshold sensitivity
# =============================================================================

make_threshold_sensitivity <- function(balanced_burden, raw_burden) {
  combined <- balanced_burden %>%
    left_join(
      raw_burden %>% select(SubjectID, Threshold, Raw_OutcomeWeighted_Burden = Prop_Extreme),
      by = c("SubjectID", "Threshold")
    )

  metrics <- c(
    "Raw_OutcomeWeighted_Burden",
    "Balanced_Level_Burden",
    "Balanced_LevelMeasure_Burden"
  )

  pair_rows <- list()
  top_rows <- list()
  row_index <- 1L

  for (metric in metrics) {
    wide <- combined %>%
      select(SubjectID, Threshold, all_of(metric)) %>%
      tidyr::pivot_wider(names_from = Threshold, values_from = all_of(metric))
    threshold_names <- setdiff(names(wide), "SubjectID")
    threshold_pairs <- utils::combn(threshold_names, 2L, simplify = FALSE)

    for (pair in threshold_pairs) {
      x <- wide[[pair[1]]]
      y <- wide[[pair[2]]]
      ok <- is.finite(x) & is.finite(y)
      pair_rows[[row_index]] <- data.frame(
        Metric = metric,
        Threshold_1 = as.numeric(pair[1]),
        Threshold_2 = as.numeric(pair[2]),
        N = sum(ok),
        Spearman_rho = safe_spearman(x, y),
        Mean_Absolute_Difference = safe_mean(abs(x - y)),
        stringsAsFactors = FALSE
      )
      top_rows[[row_index]] <- data.frame(
        Metric = metric,
        Threshold_1 = as.numeric(pair[1]),
        Threshold_2 = as.numeric(pair[2]),
        Top_Decile_Jaccard = jaccard_index(top_set(x), top_set(y)),
        stringsAsFactors = FALSE
      )
      row_index <- row_index + 1L
    }
  }

  list(
    participant = combined,
    pairwise = bind_rows(pair_rows),
    top_decile = bind_rows(top_rows)
  )
}

# =============================================================================
# Null / permutation analyses
# =============================================================================

run_independent_outcome_null <- function(extreme_matrix, observed_burden) {
  observed <- summarise_vector(observed_burden)

  one_iteration <- function(iteration) {
    set.seed(PROFILE_SEED + iteration)
    permuted <- extreme_matrix
    for (column_index in seq_len(ncol(permuted))) {
      available <- which(is.finite(permuted[, column_index]))
      if (length(available) > 1L) {
        permuted[available, column_index] <-
          permuted[sample(available, length(available), replace = FALSE), column_index]
      }
    }
    summarise_vector(row_mean_na(permuted))
  }

  samples <- do.call(
    rbind,
    profile_map(seq_len(as.integer(PROFILE_N_PERMUTATIONS)), one_iteration)
  )
  samples <- as.data.frame(samples)
  samples$Iteration <- seq_len(nrow(samples))

  summary_rows <- lapply(names(observed), function(statistic) {
    null_values <- safe_numeric(samples[[statistic]])
    data.frame(
      Null_Model = "Outcome-wise label permutation",
      Target = "Outcome-weighted structural burden",
      Statistic = statistic,
      Observed = unname(observed[statistic]),
      Null_Mean = safe_mean(null_values),
      Null_SD = safe_sd(null_values),
      Null_Q025 = safe_quantile(null_values, 0.025),
      Null_Q975 = safe_quantile(null_values, 0.975),
      P_Upper = (1 + sum(null_values >= observed[statistic], na.rm = TRUE)) /
        (1 + sum(is.finite(null_values))),
      stringsAsFactors = FALSE
    )
  })

  summary_table <- bind_rows(summary_rows) %>%
    mutate(
      Standardized_Difference = (Observed - Null_Mean) / Null_SD,
      P_Upper_FDR = stats::p.adjust(P_Upper, method = "BH")
    )

  list(
    observed = observed,
    samples = samples,
    summary = summary_table
  )
}

run_block_preserving_null <- function(block_matrix) {
  observed_burden <- row_mean_na(block_matrix)
  observed <- c(
    summarise_vector(observed_burden),
    Mean_Block_Rho = mean_off_diagonal_spearman(block_matrix)
  )

  one_iteration <- function(iteration) {
    set.seed(PROFILE_SEED + 100000L + iteration)
    permuted <- block_matrix
    for (column_index in seq_len(ncol(permuted))) {
      available <- which(is.finite(permuted[, column_index]))
      if (length(available) > 1L) {
        permuted[available, column_index] <-
          permuted[sample(available, length(available), replace = FALSE), column_index]
      }
    }
    c(
      summarise_vector(row_mean_na(permuted)),
      Mean_Block_Rho = mean_off_diagonal_spearman(permuted)
    )
  }

  samples <- do.call(
    rbind,
    profile_map(seq_len(as.integer(PROFILE_N_PERMUTATIONS)), one_iteration)
  )
  samples <- as.data.frame(samples)
  samples$Iteration <- seq_len(nrow(samples))

  summary_rows <- lapply(names(observed), function(statistic) {
    null_values <- safe_numeric(samples[[statistic]])
    data.frame(
      Null_Model = "Level-by-measure block permutation",
      Target = "Equal-weighted multiscale structural burden",
      Statistic = statistic,
      Observed = unname(observed[statistic]),
      Null_Mean = safe_mean(null_values),
      Null_SD = safe_sd(null_values),
      Null_Q025 = safe_quantile(null_values, 0.025),
      Null_Q975 = safe_quantile(null_values, 0.975),
      P_Upper = (1 + sum(null_values >= observed[statistic], na.rm = TRUE)) /
        (1 + sum(is.finite(null_values))),
      stringsAsFactors = FALSE
    )
  })

  summary_table <- bind_rows(summary_rows) %>%
    mutate(
      Standardized_Difference = (Observed - Null_Mean) / Null_SD,
      P_Upper_FDR = stats::p.adjust(P_Upper, method = "BH")
    )

  list(
    observed = observed,
    samples = samples,
    summary = summary_table
  )
}

# =============================================================================
# Profile stability
# =============================================================================

run_profile_stability <- function(extreme_matrix, outcome_metadata, observed_blocks) {
  block_by_outcome <- paste(
    outcome_metadata$Level[match(colnames(extreme_matrix), outcome_metadata$Outcome)],
    outcome_metadata$OutcomeType[match(colnames(extreme_matrix), outcome_metadata$Outcome)],
    sep = "__"
  )
  block_indices <- split(seq_len(ncol(extreme_matrix)), block_by_outcome)
  block_indices <- block_indices[
    lengths(block_indices) >= as.integer(PROFILE_STABILITY_MIN_OUTCOMES_PER_BLOCK)
  ]

  eligible_blocks <- intersect(names(block_indices), colnames(observed_blocks))
  block_indices <- block_indices[eligible_blocks]
  observed_stability_blocks <- observed_blocks[, eligible_blocks, drop = FALSE]
  observed_balanced <- row_mean_na(observed_stability_blocks)

  if (length(block_indices) < 2L) {
    stop("Too few eligible level-by-measure blocks for profile stability analysis.")
  }

  bootstrap_iteration <- function(iteration) {
    set.seed(PROFILE_SEED + 200000L + iteration)
    boot_blocks <- matrix(
      NA_real_, nrow = nrow(extreme_matrix), ncol = length(block_indices),
      dimnames = list(rownames(extreme_matrix), names(block_indices))
    )
    for (block_name in names(block_indices)) {
      indices <- block_indices[[block_name]]
      sampled <- sample(indices, length(indices), replace = TRUE)
      boot_blocks[, block_name] <- row_mean_na(
        extreme_matrix[, sampled, drop = FALSE]
      )
    }
    boot_balanced <- row_mean_na(boot_blocks)
    data.frame(
      Iteration = iteration,
      Method = "Outcome bootstrap within block",
      Balanced_Burden_Rho = safe_spearman(observed_balanced, boot_balanced),
      Top_Decile_Jaccard = jaccard_index(
        top_set(observed_balanced), top_set(boot_balanced)
      ),
      Mean_Profile_Cosine = safe_mean(
        row_cosine(observed_stability_blocks, boot_blocks)
      ),
      Mean_Absolute_Profile_Difference = safe_mean(
        abs(observed_stability_blocks - boot_blocks)
      ),
      N_Blocks = length(block_indices),
      stringsAsFactors = FALSE
    )
  }

  split_half_iteration <- function(iteration) {
    set.seed(PROFILE_SEED + 300000L + iteration)
    first_half <- matrix(
      NA_real_, nrow = nrow(extreme_matrix), ncol = length(block_indices),
      dimnames = list(rownames(extreme_matrix), names(block_indices))
    )
    second_half <- first_half

    for (block_name in names(block_indices)) {
      shuffled <- sample(block_indices[[block_name]], replace = FALSE)
      cut_point <- floor(length(shuffled) / 2)
      first_indices <- shuffled[seq_len(cut_point)]
      second_indices <- shuffled[(cut_point + 1L):length(shuffled)]
      first_half[, block_name] <- row_mean_na(
        extreme_matrix[, first_indices, drop = FALSE]
      )
      second_half[, block_name] <- row_mean_na(
        extreme_matrix[, second_indices, drop = FALSE]
      )
    }

    first_balanced <- row_mean_na(first_half)
    second_balanced <- row_mean_na(second_half)
    raw_rho <- safe_spearman(first_balanced, second_balanced)
    spearman_brown <- if (is.finite(raw_rho) && raw_rho > -1) {
      (2 * raw_rho) / (1 + raw_rho)
    } else {
      NA_real_
    }

    data.frame(
      Iteration = iteration,
      Method = "Stratified split-half",
      Balanced_Burden_Rho = raw_rho,
      Spearman_Brown = spearman_brown,
      Top_Decile_Jaccard = jaccard_index(
        top_set(first_balanced), top_set(second_balanced)
      ),
      Mean_Profile_Cosine = safe_mean(row_cosine(first_half, second_half)),
      Mean_Absolute_Profile_Difference = safe_mean(abs(first_half - second_half)),
      N_Blocks = length(block_indices),
      stringsAsFactors = FALSE
    )
  }

  bootstrap_results <- bind_rows(
    profile_map(
      seq_len(as.integer(PROFILE_N_STABILITY_REPEATS)),
      bootstrap_iteration
    )
  )
  split_half_results <- bind_rows(
    profile_map(
      seq_len(as.integer(PROFILE_N_STABILITY_REPEATS)),
      split_half_iteration
    )
  )
  iterations <- bind_rows(bootstrap_results, split_half_results)

  stability_metrics <- c(
    "Balanced_Burden_Rho", "Spearman_Brown", "Top_Decile_Jaccard",
    "Mean_Profile_Cosine", "Mean_Absolute_Profile_Difference"
  )
  summary_table <- iterations %>%
    tidyr::pivot_longer(
      cols = any_of(stability_metrics),
      names_to = "Metric",
      values_to = "Value"
    ) %>%
    group_by(Method, Metric) %>%
    summarise(
      N_Valid = sum(is.finite(Value)),
      Mean = safe_mean(Value),
      SD = safe_sd(Value),
      Median = safe_quantile(Value, 0.50),
      Q025 = safe_quantile(Value, 0.025),
      Q975 = safe_quantile(Value, 0.975),
      .groups = "drop"
    )

  list(
    iterations = iterations,
    summary = summary_table,
    eligible_blocks = data.frame(
      Block = names(block_indices),
      N_Outcomes = lengths(block_indices),
      stringsAsFactors = FALSE
    )
  )
}

# =============================================================================
# Correlation and calibration audits
# =============================================================================

make_correlation_table <- function(burden_table, category_column, threshold) {
  wide <- burden_table %>%
    filter(abs(Threshold - threshold) < 1e-10) %>%
    select(SubjectID, all_of(category_column), Prop_Extreme) %>%
    rename(Category = all_of(category_column)) %>%
    tidyr::pivot_wider(names_from = Category, values_from = Prop_Extreme)
  categories <- setdiff(names(wide), "SubjectID")
  if (length(categories) < 2L) return(data.frame())
  matrix_values <- as.matrix(wide[, categories, drop = FALSE])
  correlation_matrix <- suppressWarnings(
    stats::cor(matrix_values, method = "spearman", use = "pairwise.complete.obs")
  )
  as.data.frame(as.table(correlation_matrix), stringsAsFactors = FALSE) %>%
    rename(Category_1 = Var1, Category_2 = Var2, Spearman_rho = Freq)
}

make_demographic_audit <- function(scores, participant_summary) {
  metadata_columns <- intersect(c("SubjectID", "Age", "Sex", "sex"), names(scores))
  metadata <- scores %>%
    select(all_of(metadata_columns)) %>%
    distinct(SubjectID, .keep_all = TRUE)

  if (!"Sex" %in% names(metadata) && "sex" %in% names(metadata)) {
    metadata$Sex <- as.character(metadata$sex)
  }

  analysis_data <- participant_summary %>%
    filter(abs(Threshold - PROFILE_PRIMARY_THRESHOLD) < 1e-10) %>%
    left_join(metadata, by = "SubjectID")

  audit_rows <- list()

  if ("Age" %in% names(analysis_data)) {
    age <- safe_numeric(analysis_data$Age)
    burden <- analysis_data$Balanced_LevelMeasure_Burden
    ok <- is.finite(age) & is.finite(burden)
    if (sum(ok) >= 3L) {
      test <- suppressWarnings(stats::cor.test(
        age[ok], burden[ok], method = "spearman", exact = FALSE
      ))
      audit_rows[[length(audit_rows) + 1L]] <- data.frame(
        Audit = "Age calibration",
        Comparison = "Age vs balanced level-by-measure burden",
        N = sum(ok),
        Effect = unname(test$estimate),
        Effect_Label = "Spearman rho",
        P_Value = test$p.value,
        stringsAsFactors = FALSE
      )
    }
  }

  if ("Sex" %in% names(analysis_data)) {
    sex <- as.character(analysis_data$Sex)
    burden <- analysis_data$Balanced_LevelMeasure_Burden
    valid_groups <- names(which(table(sex[is.finite(burden)]) >= 2L))
    if (length(valid_groups) == 2L) {
      group_1 <- burden[sex == valid_groups[1] & is.finite(burden)]
      group_2 <- burden[sex == valid_groups[2] & is.finite(burden)]
      test <- suppressWarnings(stats::wilcox.test(group_1, group_2, exact = FALSE))
      audit_rows[[length(audit_rows) + 1L]] <- data.frame(
        Audit = "Sex calibration",
        Comparison = paste(valid_groups, collapse = " vs "),
        N = length(group_1) + length(group_2),
        Effect = safe_mean(group_1) - safe_mean(group_2),
        Effect_Label = "Mean burden difference",
        P_Value = test$p.value,
        stringsAsFactors = FALSE
      )
    }
  }

  output <- bind_rows(audit_rows)
  if (nrow(output)) {
    output$Q_Value <- stats::p.adjust(output$P_Value, method = "BH")
  }
  output
}

make_directional_audit <- function(raw_burden) {
  dat <- raw_burden %>%
    filter(abs(Threshold - PROFILE_PRIMARY_THRESHOLD) < 1e-10)
  if (!nrow(dat)) return(data.frame())
  test <- suppressWarnings(stats::wilcox.test(
    dat$Prop_Positive,
    dat$Prop_Negative,
    paired = TRUE,
    exact = FALSE
  ))
  data.frame(
    Threshold = PROFILE_PRIMARY_THRESHOLD,
    Mean_Positive_Burden = safe_mean(dat$Prop_Positive),
    Mean_Negative_Burden = safe_mean(dat$Prop_Negative),
    Mean_Difference = safe_mean(dat$Prop_Positive - dat$Prop_Negative),
    Wilcoxon_V = unname(test$statistic),
    P_Value = test$p.value,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# Figures
# =============================================================================

make_profile_figures <- function(
    participant_summary,
    burden_by_level,
    burden_by_measure,
    block_matrix,
    independent_null,
    block_null,
    stability) {

  figure_index <- list()

  ranked <- participant_summary %>%
    filter(abs(Threshold - PROFILE_PRIMARY_THRESHOLD) < 1e-10) %>%
    filter(is.finite(Balanced_LevelMeasure_Burden)) %>%
    arrange(Balanced_LevelMeasure_Burden) %>%
    mutate(
      Rank = row_number(),
      Burden_Percent = 100 * Balanced_LevelMeasure_Burden
    )

  plot_1 <- ggplot(ranked, aes(Burden_Percent, Rank)) +
    geom_segment(
      aes(x = 0, xend = Burden_Percent, yend = Rank),
      linewidth = 0.35, color = "#547487"
    ) +
    geom_point(size = 1.7, color = "#1F4E5F") +
    labs(
      title = "Equal-weighted multiscale normative-deviation burden",
      subtitle = paste0(
        "Structural morphology; |cross-fitted normative z| ≥ ",
        PROFILE_PRIMARY_THRESHOLD
      ),
      x = "Balanced burden (%)",
      y = "Participants (ranked)"
    ) +
    theme_classic(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
  files_1 <- save_figure_pair(
    plot_1, "P3_F1_Ranked_Balanced_Burden", 8.6, 6.2
  )
  figure_index[[1]] <- data.frame(
    Figure = "P3_F1", Title = "Ranked balanced multiscale burden",
    PNG = files_1["PNG"], PDF = files_1["PDF"], stringsAsFactors = FALSE
  )

  level_plot_data <- burden_by_level %>%
    filter(abs(Threshold - PROFILE_PRIMARY_THRESHOLD) < 1e-10) %>%
    mutate(Burden_Percent = 100 * Prop_Extreme)
  measure_plot_data <- burden_by_measure %>%
    filter(abs(Threshold - PROFILE_PRIMARY_THRESHOLD) < 1e-10) %>%
    mutate(Burden_Percent = 100 * Prop_Extreme)

  plot_2a <- ggplot(level_plot_data, aes(Level, Burden_Percent)) +
    geom_boxplot(outlier.shape = NA, fill = "#DDE7EC", color = "#4D6571") +
    geom_jitter(width = 0.12, alpha = 0.22, size = 0.65, color = "#1F4E5F") +
    labs(title = "Burden by anatomical level", x = NULL, y = "Burden (%)") +
    theme_classic(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))

  plot_2b <- ggplot(measure_plot_data, aes(OutcomeType, Burden_Percent)) +
    geom_boxplot(outlier.shape = NA, fill = "#E7E1D7", color = "#75644B") +
    geom_jitter(width = 0.12, alpha = 0.22, size = 0.65, color = "#765D36") +
    labs(title = "Burden by morphometric measure", x = NULL, y = "Burden (%)") +
    theme_classic(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 25, hjust = 1)
    )

  # Avoid a patchwork dependency in this post-processing module: save panels
  # separately with a shared figure code.
  files_2a <- save_figure_pair(
    plot_2a, "P3_F2A_Burden_By_Level", 8.2, 5.8
  )
  files_2b <- save_figure_pair(
    plot_2b, "P3_F2B_Burden_By_Measure", 8.2, 5.8
  )
  figure_index[[2]] <- data.frame(
    Figure = "P3_F2A", Title = "Burden by anatomical level",
    PNG = files_2a["PNG"], PDF = files_2a["PDF"], stringsAsFactors = FALSE
  )
  figure_index[[3]] <- data.frame(
    Figure = "P3_F2B", Title = "Burden by morphometric measure",
    PNG = files_2b["PNG"], PDF = files_2b["PDF"], stringsAsFactors = FALSE
  )

  profile_order <- order(row_mean_na(block_matrix))
  heatmap_data <- as.data.frame(block_matrix[profile_order, , drop = FALSE])
  heatmap_data$Participant_Rank <- seq_len(nrow(heatmap_data))
  heatmap_long <- heatmap_data %>%
    tidyr::pivot_longer(
      cols = -Participant_Rank,
      names_to = "Block",
      values_to = "Burden"
    ) %>%
    mutate(Burden_Percent = 100 * Burden)

  plot_3 <- ggplot(heatmap_long, aes(Block, Participant_Rank, fill = Burden_Percent)) +
    geom_raster() +
    scale_fill_viridis_c(option = "C", na.value = "grey92") +
    labs(
      title = "Individual multiscale deviation profiles",
      subtitle = "Participants ordered by equal-weighted structural burden",
      x = "Anatomical level × morphometric measure",
      y = "Participants (ranked)",
      fill = "Burden (%)"
    ) +
    theme_classic(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 55, hjust = 1)
    )
  files_3 <- save_figure_pair(
    plot_3, "P3_F3_Individual_Profile_Heatmap", 12, 8
  )
  figure_index[[4]] <- data.frame(
    Figure = "P3_F3", Title = "Individual multiscale profile heatmap",
    PNG = files_3["PNG"], PDF = files_3["PDF"], stringsAsFactors = FALSE
  )

  null_plot_data <- bind_rows(
    independent_null$samples %>%
      transmute(Model = "Outcome-wise permutation", SD = SD),
    block_null$samples %>%
      transmute(Model = "Block-preserving permutation", SD = SD)
  )
  observed_lines <- data.frame(
    Model = c("Outcome-wise permutation", "Block-preserving permutation"),
    Observed_SD = c(
      unname(independent_null$observed["SD"]),
      unname(block_null$observed["SD"])
    ),
    stringsAsFactors = FALSE
  )

  plot_4 <- ggplot(null_plot_data, aes(SD)) +
    geom_histogram(bins = 35, fill = "#D6E1E6", color = "white") +
    geom_vline(
      data = observed_lines,
      aes(xintercept = Observed_SD),
      color = "#A33A3A", linewidth = 0.9
    ) +
    facet_wrap(~Model, scales = "free") +
    labs(
      title = "Permutation tests of between-person burden heterogeneity",
      subtitle = "Red line = observed standard deviation",
      x = "Null standard deviation of participant burden",
      y = "Permutation count"
    ) +
    theme_classic(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))
  files_4 <- save_figure_pair(
    plot_4, "P3_F4_Permutation_Heterogeneity", 10, 6.5
  )
  figure_index[[5]] <- data.frame(
    Figure = "P3_F4", Title = "Permutation heterogeneity tests",
    PNG = files_4["PNG"], PDF = files_4["PDF"], stringsAsFactors = FALSE
  )

  threshold_plot_data <- participant_summary %>%
    filter(is.finite(Balanced_LevelMeasure_Burden)) %>%
    mutate(
      Threshold_Label = factor(
        sprintf("|z| ≥ %.2f", Threshold),
        levels = sprintf("|z| ≥ %.2f", sort(unique(Threshold)))
      ),
      Burden_Percent = 100 * Balanced_LevelMeasure_Burden
    )
  plot_5 <- ggplot(
    threshold_plot_data,
    aes(Threshold_Label, Burden_Percent, group = SubjectID)
  ) +
    geom_line(alpha = 0.08, color = "#607D8B") +
    geom_boxplot(
      aes(group = Threshold_Label),
      width = 0.40, outlier.shape = NA,
      fill = scales::alpha("#D9E4E9", 0.85), color = "#405B68"
    ) +
    labs(
      title = "Sensitivity of balanced burden to deviation threshold",
      x = NULL,
      y = "Balanced burden (%)"
    ) +
    theme_classic(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))
  files_5 <- save_figure_pair(
    plot_5, "P3_F5_Threshold_Sensitivity", 8.2, 5.8
  )
  figure_index[[6]] <- data.frame(
    Figure = "P3_F5", Title = "Deviation-threshold sensitivity",
    PNG = files_5["PNG"], PDF = files_5["PDF"], stringsAsFactors = FALSE
  )

  stability_plot_data <- stability$iterations %>%
    select(
      Method, Balanced_Burden_Rho, Spearman_Brown,
      Top_Decile_Jaccard, Mean_Profile_Cosine
    ) %>%
    tidyr::pivot_longer(
      cols = -Method,
      names_to = "Metric",
      values_to = "Value"
    ) %>%
    filter(is.finite(Value))
  plot_6 <- ggplot(stability_plot_data, aes(Method, Value, fill = Method)) +
    geom_boxplot(width = 0.55, outlier.shape = NA) +
    facet_wrap(~Metric, scales = "free_y") +
    scale_fill_manual(values = c(
      "Outcome bootstrap within block" = "#7896A5",
      "Stratified split-half" = "#B39B78"
    )) +
    labs(
      title = "Internal stability of individualized deviation profiles",
      x = NULL,
      y = "Stability estimate"
    ) +
    theme_classic(base_size = 9.5) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 20, hjust = 1),
      legend.position = "none"
    )
  files_6 <- save_figure_pair(
    plot_6, "P3_F6_Profile_Stability", 11, 7
  )
  figure_index[[7]] <- data.frame(
    Figure = "P3_F6", Title = "Internal profile stability",
    PNG = files_6["PNG"], PDF = files_6["PDF"], stringsAsFactors = FALSE
  )

  bind_rows(figure_index)
}

# =============================================================================
# Main runner
# =============================================================================

run_normative_deviation_profiles <- function() {
  dir.create(PROFILE_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(PROFILE_OUTPUT_DIR, "Tables"), recursive = TRUE, showWarnings = FALSE)

  cat("\n============================================================\n")
  cat(" BrainNormR individualized normative-deviation profiles\n")
  cat("============================================================\n")

  raw_scores <- read_profile_input()
  scores <- prepare_profile_scores(raw_scores)

  raw_burden <- make_burden_table(scores, "SubjectID", PROFILE_THRESHOLDS)
  burden_by_level <- make_burden_table(
    scores, c("SubjectID", "Level"), PROFILE_THRESHOLDS
  )
  burden_by_measure <- make_burden_table(
    scores, c("SubjectID", "OutcomeType"), PROFILE_THRESHOLDS
  )
  burden_by_block <- make_burden_table(
    scores, c("SubjectID", "Level", "OutcomeType"), PROFILE_THRESHOLDS
  )
  balanced_burden <- make_balanced_burden(burden_by_level, burden_by_block)
  threshold_results <- make_threshold_sensitivity(balanced_burden, raw_burden)

  score_object <- make_score_matrix(scores)
  score_matrix <- score_object$matrix
  extreme_matrix <- ifelse(
    is.finite(score_matrix),
    abs(score_matrix) >= PROFILE_PRIMARY_THRESHOLD,
    NA
  )
  storage.mode(extreme_matrix) <- "double"

  block_matrix <- make_block_matrix(
    burden_by_block,
    PROFILE_PRIMARY_THRESHOLD,
    score_object$subject_ids
  )

  participant_summary <- threshold_results$participant
  primary_participant <- participant_summary %>%
    filter(abs(Threshold - PROFILE_PRIMARY_THRESHOLD) < 1e-10)
  expected_blocks <- ncol(block_matrix)
  primary_participant <- primary_participant %>%
    mutate(
      Expected_Blocks = expected_blocks,
      Block_Coverage = N_Blocks_Available / Expected_Blocks,
      Coverage_OK = Block_Coverage >= PROFILE_MIN_BLOCK_COVERAGE
    )

  if (any(!primary_participant$Coverage_OK, na.rm = TRUE)) {
    warning("Some participants have insufficient level-by-measure block coverage.")
  }

  observed_outcome_weighted <- row_mean_na(extreme_matrix)
  independent_null <- run_independent_outcome_null(
    extreme_matrix,
    observed_outcome_weighted
  )
  block_null <- run_block_preserving_null(block_matrix)

  outcome_metadata <- scores %>%
    distinct(Outcome, OutcomeType, Level, Region, Hemisphere, Lobe)
  stability <- run_profile_stability(
    extreme_matrix,
    outcome_metadata,
    block_matrix
  )

  level_correlations <- make_correlation_table(
    burden_by_level, "Level", PROFILE_PRIMARY_THRESHOLD
  )
  measure_correlations <- make_correlation_table(
    burden_by_measure, "OutcomeType", PROFILE_PRIMARY_THRESHOLD
  )
  demographic_audit <- make_demographic_audit(scores, participant_summary)
  directional_audit <- make_directional_audit(raw_burden)

  levels_present <- sort(unique(scores$Level))
  measures_present <- sort(unique(scores$OutcomeType))
  levels_missing <- setdiff(PROFILE_INCLUDED_LEVELS, levels_present)
  expected_measures <- c("Volume", "Area", "Thickness", "Curvature", "FD")
  measures_missing <- setdiff(expected_measures, measures_present)

  coverage_audit <- burden_by_block %>%
    filter(abs(Threshold - PROFILE_PRIMARY_THRESHOLD) < 1e-10) %>%
    group_by(Level, OutcomeType) %>%
    summarise(
      N_Participants = n_distinct(SubjectID),
      Min_Outcomes_Per_Participant = min(N_Valid_Outcomes),
      Median_Outcomes_Per_Participant = stats::median(N_Valid_Outcomes),
      Max_Outcomes_Per_Participant = max(N_Valid_Outcomes),
      .groups = "drop"
    )

  analysis_audit <- data.frame(
    Item = c(
      "Input_file", "Score_column", "Structural_only", "Lobe_scope",
      "Participants", "Structural_outcomes", "Score_rows",
      "Levels_present", "Levels_missing", "Measures_present",
      "Measures_missing", "Primary_threshold", "Sensitivity_thresholds",
      "Permutation_repeats", "Stability_repeats", "Expected_blocks",
      "Participants_passing_block_coverage", "Duplicate_subject_outcome_keys"
    ),
    Value = c(
      PROFILE_INPUT_FILE,
      PROFILE_SCORE_COLUMN,
      PROFILE_STRUCTURAL_ONLY,
      PROFILE_PRIMARY_LOBE_SCOPE,
      n_distinct(scores$SubjectID),
      n_distinct(scores$Outcome),
      nrow(scores),
      paste(levels_present, collapse = ", "),
      if (length(levels_missing)) paste(levels_missing, collapse = ", ") else "None",
      paste(measures_present, collapse = ", "),
      if (length(measures_missing)) paste(measures_missing, collapse = ", ") else "None",
      PROFILE_PRIMARY_THRESHOLD,
      paste(PROFILE_THRESHOLDS, collapse = ", "),
      PROFILE_N_PERMUTATIONS,
      PROFILE_N_STABILITY_REPEATS,
      expected_blocks,
      sum(primary_participant$Coverage_OK, na.rm = TRUE),
      0
    ),
    stringsAsFactors = FALSE
  )

  notes <- data.frame(
    Note = c(
      "The primary analysis uses cross-fitted CF_Norm_Z scores and structural outcomes only.",
      "Bilateral/combined lobe outcomes are primary to avoid counting left, right and bilateral lobe aggregates as independent observations.",
      "Outcome-weighted burden is descriptive and is dominated by levels containing more outcomes; equal-weighted level and level-by-measure burden are the primary multiscale summaries.",
      "The outcome-wise permutation preserves each outcome marginal distribution and missingness while breaking participant-level co-occurrence across outcomes.",
      "The block permutation operates on level-by-measure burdens, preserving each block distribution while breaking participant alignment across blocks.",
      "Outcome bootstrap and stratified split-half analyses quantify rank, top-decile and profile stability.",
      "Thresholds 1.96, 2.00 and 2.58 are research cut points and are not diagnostic thresholds.",
      "Cross-sectional normative deviations are not longitudinal rates of within-person change.",
      "External replication remains necessary before clinical interpretation."
    ),
    stringsAsFactors = FALSE
  )

  profile_matrix_export <- data.frame(
    SubjectID = rownames(block_matrix),
    block_matrix,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  figure_index <- make_profile_figures(
    participant_summary,
    burden_by_level,
    burden_by_measure,
    block_matrix,
    independent_null,
    block_null,
    stability
  )

  workbook_sheets <- list(
    Analysis_Audit = analysis_audit,
    Coverage_Audit = coverage_audit,
    Participant_Summary = participant_summary,
    Burden_By_Level = burden_by_level,
    Burden_By_Measure = burden_by_measure,
    Burden_LevelMeasure = burden_by_block,
    Profile_Matrix = profile_matrix_export,
    Threshold_Pairwise = threshold_results$pairwise,
    Threshold_TopDecile = threshold_results$top_decile,
    Null_Summary = bind_rows(independent_null$summary, block_null$summary),
    Null_Outcome_Samples = independent_null$samples,
    Null_Block_Samples = block_null$samples,
    Stability_Summary = stability$summary,
    Stability_Iterations = stability$iterations,
    Stability_Blocks = stability$eligible_blocks,
    Level_Correlations = level_correlations,
    Measure_Correlations = measure_correlations,
    Demographic_Audit = demographic_audit,
    Directional_Audit = directional_audit,
    Figure_Index = figure_index,
    Notes = notes
  )
  workbook_sheets <- workbook_sheets[
    vapply(
      workbook_sheets,
      function(x) is.data.frame(x) && ncol(x) > 0L,
      logical(1)
    )
  ]

  output_workbook <- file.path(
    PROFILE_OUTPUT_DIR,
    "NORMATIVE_DEVIATION_PROFILE_ANALYSIS.xlsx"
  )
  writexl::write_xlsx(workbook_sheets, output_workbook)

  csv_tables <- list(
    Participant_Summary = participant_summary,
    Null_Summary = bind_rows(independent_null$summary, block_null$summary),
    Stability_Summary = stability$summary,
    Figure_Index = figure_index
  )
  for (table_name in names(csv_tables)) {
    utils::write.csv(
      csv_tables[[table_name]],
      file.path(PROFILE_OUTPUT_DIR, "Tables", paste0(table_name, ".csv")),
      row.names = FALSE
    )
  }

  session_file <- file.path(PROFILE_OUTPUT_DIR, "profile_analysis_sessionInfo.txt")
  capture.output(
    {
      cat("BrainNormR normative-deviation profile analysis\n")
      cat("Run time: ", as.character(Sys.time()), "\n", sep = "")
      cat("Seed: ", PROFILE_SEED, "\n", sep = "")
      cat("Permutation repeats: ", PROFILE_N_PERMUTATIONS, "\n", sep = "")
      cat("Stability repeats: ", PROFILE_N_STABILITY_REPEATS, "\n\n", sep = "")
      print(utils::sessionInfo())
    },
    file = session_file
  )

  cat("\nProfile-analysis workbook written to:\n", output_workbook, "\n", sep = "")
  cat("No normative model was refitted.\n")
  cat("============================================================\n")

  invisible(list(
    scores = scores,
    participant_summary = participant_summary,
    burden_by_level = burden_by_level,
    burden_by_measure = burden_by_measure,
    burden_by_block = burden_by_block,
    independent_null = independent_null,
    block_null = block_null,
    stability = stability,
    output_workbook = output_workbook
  ))
}

autorun_environment <- Sys.getenv("BRAINNORMR_PROFILE_AUTORUN", unset = "")
if (nzchar(autorun_environment)) {
  BRAINNORMR_PROFILE_AUTORUN <- toupper(trimws(autorun_environment)) %in%
    c("TRUE", "T", "YES", "Y", "1")
}

if (isTRUE(BRAINNORMR_PROFILE_AUTORUN)) {
  deviation_profile_results <- run_normative_deviation_profiles()
}
