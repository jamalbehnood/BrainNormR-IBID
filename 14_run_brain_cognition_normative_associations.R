###############################################################################
# 14_run_brain_cognition_normative_associations.R
#
# BrainNormR — Paper 2 module
# Normative brain–cognition association analysis
#
# PRIMARY QUESTION
#   Are individual deviations from normative cortical morphology associated with
#   individual deviations from normative cognitive performance?
#
# PRIMARY MODEL
#   Cognitive_CF_Norm_Z ~ Structural_CF_Norm_Z
#
# SECONDARY / INTERPRETABILITY MODEL
#   Observed_Cognition ~ Structural_CF_Norm_Z + Age + Sex + Education
#
# IMPORTANT PRINCIPLES
#   1) CF_Norm_Z is the ONLY primary deviation score used in this module.
#      Full-sample Norm_Z is not substituted for missing CF_Norm_Z.
#   2) BrainNormR models are NOT refitted here.
#   3) Structural and cognitive rows are joined only by reliable SubjectID.
#   4) No Age×Sex or brain×sex interaction is fitted.
#   5) HC3 heteroskedasticity-robust covariance is used for inference.
#   6) Multiple testing is controlled with BH-FDR using prespecified families.
#   7) Cognitive ObservedValue is assumed to be the exact variable used in the
#      cognitive BrainNormR model. No score inversion/transformation is done here.
#
# RECOMMENDED PIPELINE POSITION
#   ... -> 03_biomarker_module.R
#       -> 04_make_thesis_outputs.R
#       -> 13_run_age_period_analysis.R
#       -> 14_run_brain_cognition_normative_associations.R
#       -> 09_make_publication_figure_suite.R (or a future Paper-2 figure suite)
#
###############################################################################

suppressPackageStartupMessages({
  pkgs <- c(
    "readxl",
    "writexl",
    "dplyr",
    "tidyr",
    "ggplot2",
    "sandwich"
  )

  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    stop(
      "Install required package(s) first: ",
      paste(miss, collapse = ", ")
    )
  }

  library(readxl)
  library(writexl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(sandwich)
})

###############################################################################
# 1. USER SETTINGS
###############################################################################

# Preferred source: expanded biomarker workbook because it already standardizes
# all participant-level normative outputs and keeps structural + cognition rows.
BIOMARKER_FILE <- file.path(
  "Results",
  "Biomarker",
  "MASTER_NORMATIVE_DEVIATIONS_EXPANDED.xlsx"
)

# Output folder and files.
OUT_DIR <- file.path("Results", "Brain_Cognition_Associations")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

OUT_XLSX <- file.path(
  OUT_DIR,
  "BRAIN_COGNITION_NORMATIVE_ASSOCIATIONS.xlsx"
)

# Minimum complete cases required for one association model.
MIN_N <- 60L

# Primary individual normative score.
# IMPORTANT: no fallback to Norm_Z in the primary analysis.
PRIMARY_Z_COL <- "CF_Norm_Z"

# Structural levels to include.
# ROI and Left/Right lobes are useful but create many tests; they are retained
# as high-resolution/supplementary analyses.
INCLUDE_LEVELS <- c(
  "WholeBrain",
  "Lobe",
  "Hemisphere",
  "Yeo7",
  "ROI"
)

# Structural measures.
MEASURE_LEVELS <- c(
  "Volume",
  "Area",
  "Thickness",
  "Curvature",
  "FD"
)

# HC3 robust covariance.
USE_HC3 <- TRUE

# Minimum FDR threshold.
ALPHA_FDR <- 0.05

# Optional sensitivity analysis using full-sample Norm_Z.
# This is NOT primary and is OFF by default.
RUN_NORMZ_SENSITIVITY <- FALSE

# Figures.
MAKE_FIGURES <- TRUE
FIG_DPI <- 600

# Avoid huge ROI figures unless explicitly wanted.
MAKE_ROI_HEATMAP <- TRUE

# Number of strongest FDR-significant primary associations shown in forest plot.
TOP_N_FOREST <- 25L

###############################################################################
# 2. HELPERS
###############################################################################

safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

truthy <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  z <- toupper(trimws(as.character(x)))
  z %in% c("TRUE", "T", "1", "YES", "Y")
}

normalise_level <- function(x) {
  z <- tolower(trimws(as.character(x)))

  dplyr::case_when(
    z %in% c("wholebrain", "whole_brain", "whole brain", "whole-cortex",
             "whole cortex", "wholecortex") ~ "WholeBrain",
    z %in% c("lobe", "lobar") ~ "Lobe",
    z %in% c("hemisphere", "hemispheric") ~ "Hemisphere",
    z %in% c("yeo7", "yeo_7", "network", "networks") ~ "Yeo7",
    z %in% c("roi", "dk68", "region") ~ "ROI",
    z %in% c("cognition", "cognitive") ~ "Cognition",
    TRUE ~ as.character(x)
  )
}

normalise_measure <- function(x) {
  z <- tolower(trimws(as.character(x)))

  dplyr::case_when(
    z == "volume" ~ "Volume",
    z == "area" ~ "Area",
    z == "thickness" ~ "Thickness",
    z == "curvature" ~ "Curvature",
    z %in% c("fd", "fractal", "fractal dimension", "fractaldimension") ~ "FD",
    TRUE ~ as.character(x)
  )
}

resolve_measure <- function(outcome_type, outcome) {
  z <- normalise_measure(outcome_type)

  ifelse(
    z %in% MEASURE_LEVELS,
    z,
    normalise_measure(outcome)
  )
}

first_existing <- function(df, candidates) {
  x <- intersect(candidates, names(df))
  if (length(x)) x[1] else NA_character_
}

get_sex_numeric <- function(df) {
  sx <- first_existing(df, c("sex", "Sex", "SEX"))

  if (is.na(sx)) {
    return(rep(NA_real_, nrow(df)))
  }

  x <- df[[sx]]

  # Numeric coding if already numeric.
  xn <- suppressWarnings(as.numeric(x))
  if (sum(is.finite(xn)) >= 0.8 * sum(!is.na(x))) {
    return(xn)
  }

  # Conservative text mapping.
  z <- tolower(trimws(as.character(x)))

  dplyr::case_when(
    z %in% c("female", "f", "woman", "women") ~ 1,
    z %in% c("male", "m", "man", "men") ~ 0,
    TRUE ~ NA_real_
  )
}

get_edu_numeric <- function(df) {
  ed <- first_existing(
    df,
    c("edu", "Education", "education", "EDU", "EducationYears")
  )

  if (is.na(ed)) {
    return(rep(NA_real_, nrow(df)))
  }

  safe_num(df[[ed]])
}

get_age_numeric <- function(df) {
  ag <- first_existing(df, c("Age", "age", "AGE"))

  if (is.na(ag)) {
    return(rep(NA_real_, nrow(df)))
  }

  safe_num(df[[ag]])
}

get_reliable_subject_id <- function(df) {
  sid <- first_existing(df, c("SubjectID", "ID", "subject_id", "subject"))

  if (is.na(sid)) {
    stop(
      "NormativeScores_All has no usable SubjectID/ID column. ",
      "Brain-cognition joining cannot be done safely."
    )
  }

  out <- as.character(df[[sid]])
  out[is.na(out)] <- ""
  trimws(out)
}

robust_inference <- function(model, term, use_hc3 = TRUE) {

  cf <- coef(model)

  if (!term %in% names(cf)) {
    return(
      data.frame(
        Estimate = NA_real_,
        SE = NA_real_,
        CI_L95 = NA_real_,
        CI_U95 = NA_real_,
        t = NA_real_,
        p_raw = NA_real_,
        stringsAsFactors = FALSE
      )
    )
  }

  V <- if (isTRUE(use_hc3)) {
    tryCatch(
      sandwich::vcovHC(model, type = "HC3"),
      error = function(e) vcov(model)
    )
  } else {
    vcov(model)
  }

  est <- unname(cf[[term]])
  se <- sqrt(diag(V))[term]

  if (!is.finite(est) || !is.finite(se) || se <= 0) {
    return(
      data.frame(
        Estimate = est,
        SE = se,
        CI_L95 = NA_real_,
        CI_U95 = NA_real_,
        t = NA_real_,
        p_raw = NA_real_,
        stringsAsFactors = FALSE
      )
    )
  }

  df_res <- df.residual(model)
  crit <- qt(0.975, df = df_res)
  tval <- est / se
  p <- 2 * pt(abs(tval), df = df_res, lower.tail = FALSE)

  data.frame(
    Estimate = est,
    SE = se,
    CI_L95 = est - crit * se,
    CI_U95 = est + crit * se,
    t = tval,
    p_raw = p,
    stringsAsFactors = FALSE
  )
}

partial_r_from_t <- function(t_value, df_resid) {
  if (!is.finite(t_value) || !is.finite(df_resid) || df_resid <= 0) {
    return(NA_real_)
  }

  sign(t_value) * sqrt(t_value^2 / (t_value^2 + df_resid))
}

delta_r2_for_term <- function(full_model, reduced_model) {
  r2_full <- tryCatch(summary(full_model)$r.squared, error = function(e) NA_real_)
  r2_red  <- tryCatch(summary(reduced_model)$r.squared, error = function(e) NA_real_)

  if (!is.finite(r2_full) || !is.finite(r2_red)) {
    return(NA_real_)
  }

  r2_full - r2_red
}

standardized_beta_simple <- function(y, x) {
  ok <- is.finite(y) & is.finite(x)
  if (sum(ok) < 5L) return(NA_real_)

  sy <- sd(y[ok])
  sx <- sd(x[ok])

  if (!is.finite(sy) || sy <= 0 || !is.finite(sx) || sx <= 0) {
    return(NA_real_)
  }

  NA_real_
}

structural_scope <- function(level, hemisphere, region) {

  lev <- normalise_level(level)
  hem <- clean_text(hemisphere)
  reg <- clean_text(region)

  if (lev == "WholeBrain") {
    return("WholeBrain")
  }

  if (lev == "Yeo7") {
    return("Yeo7")
  }

  if (lev == "ROI") {
    return("ROI")
  }

  if (lev == "Hemisphere") {
    return("Hemisphere")
  }

  if (lev == "Lobe") {
    if (
      tolower(hem) %in% c("bilateral", "both", "all") ||
      grepl("^bilateral_", reg, ignore.case = TRUE)
    ) {
      return("Lobe_Bilateral")
    }

    if (tolower(hem) %in% c("left", "lh")) {
      return("Lobe_Left")
    }

    if (tolower(hem) %in% c("right", "rh")) {
      return("Lobe_Right")
    }

    return("Lobe_Unspecified")
  }

  lev
}

make_structural_label <- function(level, region, hemisphere, lobe) {

  lev <- normalise_level(level)
  reg <- clean_text(region)
  hem <- clean_text(hemisphere)
  lob <- clean_text(lobe)

  if (lev == "WholeBrain") return("Whole cortex")

  if (lev == "Hemisphere") {
    if (tolower(hem) %in% c("left", "lh")) return("Left hemisphere")
    if (tolower(hem) %in% c("right", "rh")) return("Right hemisphere")
    if (nzchar(reg)) return(reg)
  }

  if (lev == "Lobe") {
    base <- if (nzchar(lob)) lob else reg

    if (tolower(hem) %in% c("left", "lh")) {
      return(paste0(base, " (Left)"))
    }

    if (tolower(hem) %in% c("right", "rh")) {
      return(paste0(base, " (Right)"))
    }

    if (
      tolower(hem) %in% c("bilateral", "both", "all") ||
      grepl("^bilateral_", reg, ignore.case = TRUE)
    ) {
      base <- sub("^bilateral_", "", base, ignore.case = TRUE)
      return(paste0(base, " (Bilateral)"))
    }

    return(base)
  }

  if (nzchar(reg)) return(reg)

  ""
}

safe_filename <- function(x) {
  gsub("[^A-Za-z0-9_\\-]+", "_", as.character(x))
}

###############################################################################
# 3. LOAD + STANDARDIZE NORMATIVE SCORES
###############################################################################

if (!file.exists(BIOMARKER_FILE)) {
  stop(
    "Biomarker workbook not found:\n",
    BIOMARKER_FILE,
    "\n\nRun 03_biomarker_module.R first."
  )
}

sheets <- readxl::excel_sheets(BIOMARKER_FILE)

if (!"NormativeScores_All" %in% sheets) {
  stop(
    "Sheet 'NormativeScores_All' not found in:\n",
    BIOMARKER_FILE
  )
}

scores <- as.data.frame(
  readxl::read_excel(
    BIOMARKER_FILE,
    sheet = "NormativeScores_All"
  ),
  check.names = FALSE
)

required <- c(
  "Outcome",
  "OutcomeType",
  "Level",
  "Region",
  PRIMARY_Z_COL
)

missing_required <- setdiff(required, names(scores))

if (length(missing_required)) {
  stop(
    "NormativeScores_All is missing required column(s): ",
    paste(missing_required, collapse = ", ")
  )
}

scores$SubjectID <- get_reliable_subject_id(scores)

# Exclude synthetic/fallback IDs if the biomarker module marked them.
if ("SubjectID_IsFallback" %in% names(scores)) {
  fallback <- truthy(scores$SubjectID_IsFallback)
} else {
  fallback <- rep(FALSE, nrow(scores))
}

scores$Age_std <- get_age_numeric(scores)
scores$Sex_std <- get_sex_numeric(scores)
scores$Edu_std <- get_edu_numeric(scores)

scores$Level_std <- normalise_level(scores$Level)
scores$Measure_std <- resolve_measure(scores$OutcomeType, scores$Outcome)

if (!"Hemisphere" %in% names(scores)) scores$Hemisphere <- NA_character_
if (!"Lobe" %in% names(scores)) scores$Lobe <- NA_character_

scores$StructuralScope <- mapply(
  structural_scope,
  scores$Level_std,
  scores$Hemisphere,
  scores$Region,
  USE.NAMES = FALSE
)

scores$StructuralLabel <- mapply(
  make_structural_label,
  scores$Level_std,
  scores$Region,
  scores$Hemisphere,
  scores$Lobe,
  USE.NAMES = FALSE
)

scores$CF_Norm_Z <- safe_num(scores$CF_Norm_Z)

if ("Norm_Z" %in% names(scores)) {
  scores$Norm_Z <- safe_num(scores$Norm_Z)
}

if ("ObservedValue" %in% names(scores)) {
  scores$ObservedValue <- safe_num(scores$ObservedValue)
} else {
  scores$ObservedValue <- NA_real_
}

scores <- scores %>%
  mutate(
    SubjectID_Reliable = !fallback & nzchar(SubjectID)
  )

###############################################################################
# 4. SEPARATE STRUCTURAL AND COGNITIVE PARTICIPANT-LEVEL SCORES
###############################################################################

structural <- scores %>%
  filter(
    SubjectID_Reliable,
    Level_std %in% INCLUDE_LEVELS,
    Measure_std %in% MEASURE_LEVELS
  ) %>%
  transmute(
    SubjectID,
    StructuralOutcome = as.character(Outcome),
    Measure = Measure_std,
    Level = Level_std,
    StructuralScope,
    StructuralRegion = as.character(Region),
    StructuralHemisphere = as.character(Hemisphere),
    StructuralLobe = as.character(Lobe),
    StructuralLabel,
    Structural_CF_Norm_Z = CF_Norm_Z,
    Structural_Norm_Z = if ("Norm_Z" %in% names(scores)) Norm_Z else NA_real_,
    Age = Age_std,
    Sex = Sex_std,
    Education = Edu_std
  )

cognition <- scores %>%
  filter(
    SubjectID_Reliable,
    Level_std == "Cognition"
  ) %>%
  transmute(
    SubjectID,
    CognitiveOutcome = as.character(Outcome),
    CognitiveLabel = ifelse(
      nzchar(clean_text(Region)),
      as.character(Region),
      as.character(Outcome)
    ),
    CognitiveDomain = ifelse(
      nzchar(clean_text(Lobe)),
      as.character(Lobe),
      NA_character_
    ),
    Cognitive_CF_Norm_Z = CF_Norm_Z,
    Cognitive_Norm_Z = if ("Norm_Z" %in% names(scores)) Norm_Z else NA_real_,
    Observed_Cognition = ObservedValue,
    Age = Age_std,
    Sex = Sex_std,
    Education = Edu_std
  )

if (!nrow(structural)) {
  stop("No structural participant-level normative scores were found.")
}

if (!nrow(cognition)) {
  stop("No cognitive participant-level normative scores were found.")
}

###############################################################################
# 5. DUPLICATE / JOIN QC
###############################################################################

structural_dup <- structural %>%
  count(
    SubjectID,
    StructuralOutcome,
    name = "N"
  ) %>%
  filter(N > 1L)

cognition_dup <- cognition %>%
  count(
    SubjectID,
    CognitiveOutcome,
    name = "N"
  ) %>%
  filter(N > 1L)

if (nrow(structural_dup)) {
  warning(
    nrow(structural_dup),
    " duplicated SubjectID × structural outcome combinations found. ",
    "The first non-missing row will be retained."
  )
}

if (nrow(cognition_dup)) {
  warning(
    nrow(cognition_dup),
    " duplicated SubjectID × cognitive outcome combinations found. ",
    "The first non-missing row will be retained."
  )
}

structural <- structural %>%
  arrange(
    SubjectID,
    StructuralOutcome,
    desc(is.finite(Structural_CF_Norm_Z))
  ) %>%
  distinct(
    SubjectID,
    StructuralOutcome,
    .keep_all = TRUE
  )

cognition <- cognition %>%
  arrange(
    SubjectID,
    CognitiveOutcome,
    desc(is.finite(Cognitive_CF_Norm_Z)),
    desc(is.finite(Observed_Cognition))
  ) %>%
  distinct(
    SubjectID,
    CognitiveOutcome,
    .keep_all = TRUE
  )

score_qc <- bind_rows(
  data.frame(
    Dataset = "Structural",
    N_Rows = nrow(structural),
    N_Subjects = n_distinct(structural$SubjectID),
    N_Outcomes = n_distinct(structural$StructuralOutcome),
    N_CF_Z = sum(is.finite(structural$Structural_CF_Norm_Z)),
    Prop_CF_Z = mean(is.finite(structural$Structural_CF_Norm_Z)),
    stringsAsFactors = FALSE
  ),
  data.frame(
    Dataset = "Cognition",
    N_Rows = nrow(cognition),
    N_Subjects = n_distinct(cognition$SubjectID),
    N_Outcomes = n_distinct(cognition$CognitiveOutcome),
    N_CF_Z = sum(is.finite(cognition$Cognitive_CF_Norm_Z)),
    Prop_CF_Z = mean(is.finite(cognition$Cognitive_CF_Norm_Z)),
    stringsAsFactors = FALSE
  )
)

###############################################################################
# 6. PRIMARY MODEL:
#    COGNITIVE CF_NORM_Z ~ STRUCTURAL CF_NORM_Z
###############################################################################

structural_outcomes <- structural %>%
  distinct(
    StructuralOutcome,
    Measure,
    Level,
    StructuralScope,
    StructuralRegion,
    StructuralHemisphere,
    StructuralLobe,
    StructuralLabel
  )

cognitive_outcomes <- cognition %>%
  distinct(
    CognitiveOutcome,
    CognitiveLabel,
    CognitiveDomain
  )

primary_rows <- list()
primary_qc_rows <- list()
pair_index <- 0L

for (si in seq_len(nrow(structural_outcomes))) {

  s_meta <- structural_outcomes[si, , drop = FALSE]

  sx <- structural %>%
    filter(StructuralOutcome == s_meta$StructuralOutcome) %>%
    select(
      SubjectID,
      Structural_CF_Norm_Z,
      Structural_Norm_Z,
      Age_s = Age,
      Sex_s = Sex,
      Edu_s = Education
    )

  for (ci in seq_len(nrow(cognitive_outcomes))) {

    c_meta <- cognitive_outcomes[ci, , drop = FALSE]

    cy <- cognition %>%
      filter(CognitiveOutcome == c_meta$CognitiveOutcome) %>%
      select(
        SubjectID,
        Cognitive_CF_Norm_Z,
        Cognitive_Norm_Z,
        Observed_Cognition,
        Age_c = Age,
        Sex_c = Sex,
        Edu_c = Education
      )

    pair_index <- pair_index + 1L

    dat <- inner_join(sx, cy, by = "SubjectID")

    # Prefer covariates from cognitive row; fallback to structural row.
    dat <- dat %>%
      mutate(
        Age = ifelse(is.finite(Age_c), Age_c, Age_s),
        Sex = ifelse(is.finite(Sex_c), Sex_c, Sex_s),
        Education = ifelse(is.finite(Edu_c), Edu_c, Edu_s)
      )

    dat_p <- dat %>%
      filter(
        is.finite(Structural_CF_Norm_Z),
        is.finite(Cognitive_CF_Norm_Z)
      )

    n_pair <- nrow(dat_p)

    qc_base <- data.frame(
      StructuralOutcome = s_meta$StructuralOutcome,
      Measure = s_meta$Measure,
      Level = s_meta$Level,
      StructuralScope = s_meta$StructuralScope,
      StructuralRegion = s_meta$StructuralRegion,
      StructuralHemisphere = s_meta$StructuralHemisphere,
      StructuralLobe = s_meta$StructuralLobe,
      StructuralLabel = s_meta$StructuralLabel,
      CognitiveOutcome = c_meta$CognitiveOutcome,
      CognitiveLabel = c_meta$CognitiveLabel,
      CognitiveDomain = c_meta$CognitiveDomain,
      N_Joined = nrow(dat),
      N_PrimaryComplete = n_pair,
      stringsAsFactors = FALSE
    )

    if (n_pair < MIN_N) {

      primary_qc_rows[[length(primary_qc_rows) + 1L]] <- cbind(
        qc_base,
        data.frame(
          Status = "SKIP_LOW_N",
          stringsAsFactors = FALSE
        )
      )

      next
    }

    # Both variables are normative z-scores; coefficient is therefore directly
    # interpretable as cognitive-deviation SD per structural-deviation SD.
    mod <- tryCatch(
      lm(
        Cognitive_CF_Norm_Z ~ Structural_CF_Norm_Z,
        data = dat_p
      ),
      error = function(e) NULL
    )

    if (is.null(mod)) {

      primary_qc_rows[[length(primary_qc_rows) + 1L]] <- cbind(
        qc_base,
        data.frame(
          Status = "SKIP_MODEL_ERROR",
          stringsAsFactors = FALSE
        )
      )

      next
    }

    inf <- robust_inference(
      mod,
      term = "Structural_CF_Norm_Z",
      use_hc3 = USE_HC3
    )

    r2 <- summary(mod)$r.squared
    adjr2 <- summary(mod)$adj.r.squared
    pr <- partial_r_from_t(inf$t, df.residual(mod))

    out <- cbind(
      qc_base,
      data.frame(
        Model = "Cognitive_CF_Norm_Z ~ Structural_CF_Norm_Z",
        Beta_Z_to_Z = inf$Estimate,
        SE_HC3 = inf$SE,
        CI_L95 = inf$CI_L95,
        CI_U95 = inf$CI_U95,
        t = inf$t,
        p_raw = inf$p_raw,
        Partial_r = pr,
        R2 = r2,
        Adjusted_R2 = adjr2,
        Residual_SD = sigma(mod),
        SE_Type = if (USE_HC3) "HC3" else "Model-based",
        stringsAsFactors = FALSE
      )
    )

    primary_rows[[length(primary_rows) + 1L]] <- out

    primary_qc_rows[[length(primary_qc_rows) + 1L]] <- cbind(
      qc_base,
      data.frame(
        Status = "OK",
        stringsAsFactors = FALSE
      )
    )
  }
}

primary <- bind_rows(primary_rows)
primary_qc <- bind_rows(primary_qc_rows)

###############################################################################
# 7. SECONDARY MODEL:
#    OBSERVED COGNITION ~ STRUCTURAL CF_Z + AGE + SEX + EDUCATION
###############################################################################

raw_rows <- list()
raw_qc_rows <- list()

for (si in seq_len(nrow(structural_outcomes))) {

  s_meta <- structural_outcomes[si, , drop = FALSE]

  sx <- structural %>%
    filter(StructuralOutcome == s_meta$StructuralOutcome) %>%
    select(
      SubjectID,
      Structural_CF_Norm_Z,
      Age_s = Age,
      Sex_s = Sex,
      Edu_s = Education
    )

  for (ci in seq_len(nrow(cognitive_outcomes))) {

    c_meta <- cognitive_outcomes[ci, , drop = FALSE]

    cy <- cognition %>%
      filter(CognitiveOutcome == c_meta$CognitiveOutcome) %>%
      select(
        SubjectID,
        Observed_Cognition,
        Age_c = Age,
        Sex_c = Sex,
        Edu_c = Education
      )

    dat <- inner_join(sx, cy, by = "SubjectID") %>%
      mutate(
        Age = ifelse(is.finite(Age_c), Age_c, Age_s),
        Sex = ifelse(is.finite(Sex_c), Sex_c, Sex_s),
        Education = ifelse(is.finite(Edu_c), Edu_c, Edu_s)
      ) %>%
      filter(
        is.finite(Structural_CF_Norm_Z),
        is.finite(Observed_Cognition),
        is.finite(Age),
        is.finite(Sex),
        is.finite(Education)
      )

    n_pair <- nrow(dat)

    qc_base <- data.frame(
      StructuralOutcome = s_meta$StructuralOutcome,
      Measure = s_meta$Measure,
      Level = s_meta$Level,
      StructuralScope = s_meta$StructuralScope,
      StructuralRegion = s_meta$StructuralRegion,
      StructuralHemisphere = s_meta$StructuralHemisphere,
      StructuralLobe = s_meta$StructuralLobe,
      StructuralLabel = s_meta$StructuralLabel,
      CognitiveOutcome = c_meta$CognitiveOutcome,
      CognitiveLabel = c_meta$CognitiveLabel,
      CognitiveDomain = c_meta$CognitiveDomain,
      N_Complete = n_pair,
      stringsAsFactors = FALSE
    )

    if (n_pair < MIN_N) {

      raw_qc_rows[[length(raw_qc_rows) + 1L]] <- cbind(
        qc_base,
        data.frame(
          Status = "SKIP_LOW_N",
          stringsAsFactors = FALSE
        )
      )

      next
    }

    # Main raw-cognition model.
    full_mod <- tryCatch(
      lm(
        Observed_Cognition ~ Structural_CF_Norm_Z + Age + Sex + Education,
        data = dat
      ),
      error = function(e) NULL
    )

    # Nested reduced model for ΔR² attributable to structural normative deviation.
    red_mod <- tryCatch(
      lm(
        Observed_Cognition ~ Age + Sex + Education,
        data = dat
      ),
      error = function(e) NULL
    )

    if (is.null(full_mod) || is.null(red_mod)) {

      raw_qc_rows[[length(raw_qc_rows) + 1L]] <- cbind(
        qc_base,
        data.frame(
          Status = "SKIP_MODEL_ERROR",
          stringsAsFactors = FALSE
        )
      )

      next
    }

    inf <- robust_inference(
      full_mod,
      term = "Structural_CF_Norm_Z",
      use_hc3 = USE_HC3
    )

    delta_r2 <- delta_r2_for_term(full_mod, red_mod)
    pr <- partial_r_from_t(inf$t, df.residual(full_mod))

    # Standardized outcome coefficient.
    # Structural_CF_Norm_Z is already in z units. We standardize only the
    # cognitive outcome for an interpretable SD-outcome effect, while keeping
    # covariates in their original coding.
    dat$Observed_Cognition_Z <- as.numeric(scale(dat$Observed_Cognition))

    std_mod <- tryCatch(
      lm(
        Observed_Cognition_Z ~ Structural_CF_Norm_Z + Age + Sex + Education,
        data = dat
      ),
      error = function(e) NULL
    )

    std_beta <- if (!is.null(std_mod) &&
                    "Structural_CF_Norm_Z" %in% names(coef(std_mod))) {
      unname(coef(std_mod)[["Structural_CF_Norm_Z"]])
    } else {
      NA_real_
    }

    out <- cbind(
      qc_base,
      data.frame(
        Model = paste0(
          "Observed_Cognition ~ Structural_CF_Norm_Z + Age + Sex + Education"
        ),
        Raw_Cognitive_Effect = inf$Estimate,
        Standardized_Beta = std_beta,
        SE_HC3 = inf$SE,
        CI_L95 = inf$CI_L95,
        CI_U95 = inf$CI_U95,
        t = inf$t,
        p_raw = inf$p_raw,
        Partial_r = pr,
        Full_R2 = summary(full_mod)$r.squared,
        Full_Adjusted_R2 = summary(full_mod)$adj.r.squared,
        Delta_R2_Structural_CF_Z = delta_r2,
        Residual_SD = sigma(full_mod),
        SE_Type = if (USE_HC3) "HC3" else "Model-based",
        stringsAsFactors = FALSE
      )
    )

    raw_rows[[length(raw_rows) + 1L]] <- out

    raw_qc_rows[[length(raw_qc_rows) + 1L]] <- cbind(
      qc_base,
      data.frame(
        Status = "OK",
        stringsAsFactors = FALSE
      )
    )
  }
}

raw_assoc <- bind_rows(raw_rows)
raw_qc <- bind_rows(raw_qc_rows)

###############################################################################
# 8. FDR FAMILIES
###############################################################################

# PRIMARY Z-to-Z families:
#
# A) WholeBrain:
#    all 5 structural measures within each cognitive outcome.
#
# B) Bilateral Lobe:
#    all bilateral-lobe structural tests within each cognitive outcome
#    (6 lobes × 5 measures when complete).
#
# C) Yeo7:
#    all Yeo7 structural tests within each cognitive outcome
#    (7 networks × 5 measures when complete).
#
# D) Hemisphere:
#    all hemisphere tests within each cognitive outcome.
#
# E) Left / Right Lobe:
#    all same-scope lobe tests within each cognitive outcome.
#
# F) ROI:
#    BH across 68 ROIs WITHIN each CognitiveOutcome × Measure.
#    This preserves anatomical resolution without mixing different morphometric
#    families in the ROI correction.

apply_fdr <- function(df) {

  if (!nrow(df)) return(df)

  df <- df %>%
    mutate(
      FDR_Family = case_when(
        StructuralScope == "ROI" ~ paste(
          "ROI",
          CognitiveOutcome,
          Measure,
          sep = " | "
        ),
        TRUE ~ paste(
          StructuralScope,
          CognitiveOutcome,
          sep = " | "
        )
      )
    ) %>%
    group_by(FDR_Family) %>%
    mutate(
      q_BH_Family = p.adjust(p_raw, method = "BH"),
      N_Tests_In_Family = n()
    ) %>%
    ungroup() %>%
    mutate(
      q_BH_Global = p.adjust(p_raw, method = "BH"),
      Significant_FDR = is.finite(q_BH_Family) &
        q_BH_Family < ALPHA_FDR
    )

  df
}

primary <- apply_fdr(primary)
raw_assoc <- apply_fdr(raw_assoc)

###############################################################################
# 9. PRIMARY SUMMARIES
###############################################################################

primary_sig <- primary %>%
  filter(Significant_FDR) %>%
  arrange(q_BH_Family, desc(abs(Beta_Z_to_Z)))

raw_sig <- raw_assoc %>%
  filter(Significant_FDR) %>%
  arrange(q_BH_Family, desc(abs(Standardized_Beta)))

top_primary <- primary_sig %>%
  arrange(q_BH_Family, desc(abs(Beta_Z_to_Z))) %>%
  slice_head(n = TOP_N_FOREST)

# Counts by level/measure.
primary_summary <- primary %>%
  group_by(
    StructuralScope,
    Measure
  ) %>%
  summarise(
    N_Tests = n(),
    N_FDR_Significant = sum(Significant_FDR, na.rm = TRUE),
    Prop_FDR_Significant = mean(Significant_FDR, na.rm = TRUE),
    Median_Abs_Beta = median(abs(Beta_Z_to_Z), na.rm = TRUE),
    Min_q_BH = suppressWarnings(min(q_BH_Family, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    Min_q_BH = ifelse(is.infinite(Min_q_BH), NA_real_, Min_q_BH)
  )

# Strongest association per cognitive outcome.
strongest_by_cognition <- primary %>%
  filter(is.finite(Beta_Z_to_Z)) %>%
  group_by(CognitiveOutcome) %>%
  slice_max(
    order_by = abs(Beta_Z_to_Z),
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

###############################################################################
# 10. OPTIONAL FULL-FIT NORM_Z SENSITIVITY
###############################################################################

normz_sensitivity <- data.frame()

if (isTRUE(RUN_NORMZ_SENSITIVITY) &&
    "Norm_Z" %in% names(scores)) {

  sens_rows <- list()

  for (si in seq_len(nrow(structural_outcomes))) {

    s_meta <- structural_outcomes[si, , drop = FALSE]

    sx <- structural %>%
      filter(StructuralOutcome == s_meta$StructuralOutcome) %>%
      select(
        SubjectID,
        Structural_Norm_Z
      )

    for (ci in seq_len(nrow(cognitive_outcomes))) {

      c_meta <- cognitive_outcomes[ci, , drop = FALSE]

      cy <- cognition %>%
        filter(CognitiveOutcome == c_meta$CognitiveOutcome) %>%
        select(
          SubjectID,
          Cognitive_Norm_Z
        )

      dat <- inner_join(sx, cy, by = "SubjectID") %>%
        filter(
          is.finite(Structural_Norm_Z),
          is.finite(Cognitive_Norm_Z)
        )

      if (nrow(dat) < MIN_N) next

      mod <- tryCatch(
        lm(
          Cognitive_Norm_Z ~ Structural_Norm_Z,
          data = dat
        ),
        error = function(e) NULL
      )

      if (is.null(mod)) next

      inf <- robust_inference(
        mod,
        term = "Structural_Norm_Z",
        use_hc3 = USE_HC3
      )

      sens_rows[[length(sens_rows) + 1L]] <- data.frame(
        StructuralOutcome = s_meta$StructuralOutcome,
        Measure = s_meta$Measure,
        Level = s_meta$Level,
        StructuralScope = s_meta$StructuralScope,
        StructuralRegion = s_meta$StructuralRegion,
        StructuralHemisphere = s_meta$StructuralHemisphere,
        StructuralLobe = s_meta$StructuralLobe,
        StructuralLabel = s_meta$StructuralLabel,
        CognitiveOutcome = c_meta$CognitiveOutcome,
        CognitiveLabel = c_meta$CognitiveLabel,
        CognitiveDomain = c_meta$CognitiveDomain,
        N = nrow(dat),
        Beta_NormZ_to_NormZ = inf$Estimate,
        SE_HC3 = inf$SE,
        CI_L95 = inf$CI_L95,
        CI_U95 = inf$CI_U95,
        p_raw = inf$p_raw,
        stringsAsFactors = FALSE
      )
    }
  }

  normz_sensitivity <- bind_rows(sens_rows)

  if (nrow(normz_sensitivity)) {
    normz_sensitivity <- apply_fdr(normz_sensitivity)
  }
}

###############################################################################
# 11. FIGURES
###############################################################################

figure_index <- list()

save_figure_pair <- function(p, stem, width, height) {

  png_path <- file.path(OUT_DIR, paste0(stem, ".png"))
  pdf_path <- file.path(OUT_DIR, paste0(stem, ".pdf"))

  ggsave(
    png_path,
    p,
    width = width,
    height = height,
    dpi = FIG_DPI,
    bg = "white"
  )

  ggsave(
    pdf_path,
    p,
    width = width,
    height = height,
    bg = "white"
  )

  c(PNG = png_path, PDF = pdf_path)
}

if (isTRUE(MAKE_FIGURES) && nrow(primary)) {

  # ---------------------------------------------------------------------------
  # Figure 1: Bilateral lobe × cognition heatmap
  # ---------------------------------------------------------------------------

  hp <- primary %>%
    filter(
      StructuralScope == "Lobe_Bilateral",
      is.finite(Beta_Z_to_Z)
    ) %>%
    mutate(
      CellLabel = ifelse(
        Significant_FDR,
        "*",
        ""
      )
    )

  if (nrow(hp)) {

    p <- ggplot(
      hp,
      aes(
        x = CognitiveLabel,
        y = interaction(StructuralLabel, Measure, sep = " | "),
        fill = Beta_Z_to_Z
      )
    ) +
      geom_tile(
        color = "white",
        linewidth = 0.25
      ) +
      geom_text(
        aes(label = CellLabel),
        size = 3.4
      ) +
      scale_fill_gradient2(
        low = "#3B6FB6",
        mid = "white",
        high = "#C95A55",
        midpoint = 0,
        name = "β\n(CF z → CF z)"
      ) +
      labs(
        title = "Bilateral lobar normative brain–cognition associations",
        subtitle = paste0(
          "Cognitive CF_Norm_Z ~ Structural CF_Norm_Z; ",
          "* = BH-FDR < ", ALPHA_FDR
        ),
        x = "Cognitive outcome",
        y = NULL
      ) +
      theme_minimal(base_size = 9) +
      theme(
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )

    paths <- save_figure_pair(
      p,
      "P2_F1_BilateralLobe_BrainCognition_Heatmap",
      width = 13,
      height = 10
    )

    figure_index[[length(figure_index) + 1L]] <- data.frame(
      FigureCode = "P2_F1",
      Title = "Bilateral lobar normative brain-cognition heatmap",
      PNG = paths["PNG"],
      PDF = paths["PDF"],
      stringsAsFactors = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Figure 2: Yeo7 × cognition heatmap
  # ---------------------------------------------------------------------------

  hp <- primary %>%
    filter(
      StructuralScope == "Yeo7",
      is.finite(Beta_Z_to_Z)
    ) %>%
    mutate(
      CellLabel = ifelse(
        Significant_FDR,
        "*",
        ""
      )
    )

  if (nrow(hp)) {

    p <- ggplot(
      hp,
      aes(
        x = CognitiveLabel,
        y = interaction(StructuralLabel, Measure, sep = " | "),
        fill = Beta_Z_to_Z
      )
    ) +
      geom_tile(
        color = "white",
        linewidth = 0.25
      ) +
      geom_text(
        aes(label = CellLabel),
        size = 3.2
      ) +
      scale_fill_gradient2(
        low = "#3B6FB6",
        mid = "white",
        high = "#C95A55",
        midpoint = 0,
        name = "β\n(CF z → CF z)"
      ) +
      labs(
        title = "Yeo7 normative brain–cognition associations",
        subtitle = paste0(
          "Cognitive CF_Norm_Z ~ Structural CF_Norm_Z; ",
          "* = BH-FDR < ", ALPHA_FDR
        ),
        x = "Cognitive outcome",
        y = NULL
      ) +
      theme_minimal(base_size = 8.8) +
      theme(
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )

    paths <- save_figure_pair(
      p,
      "P2_F2_Yeo7_BrainCognition_Heatmap",
      width = 13,
      height = 11
    )

    figure_index[[length(figure_index) + 1L]] <- data.frame(
      FigureCode = "P2_F2",
      Title = "Yeo7 normative brain-cognition heatmap",
      PNG = paths["PNG"],
      PDF = paths["PDF"],
      stringsAsFactors = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Figure 3: strongest FDR-significant primary associations
  # ---------------------------------------------------------------------------

  fp <- top_primary %>%
    filter(
      is.finite(Beta_Z_to_Z),
      is.finite(CI_L95),
      is.finite(CI_U95)
    ) %>%
    mutate(
      Association = paste0(
        StructuralLabel,
        " | ",
        Measure,
        " → ",
        CognitiveLabel
      )
    )

  if (nrow(fp)) {

    fp$Association <- factor(
      fp$Association,
      levels = rev(fp$Association)
    )

    p <- ggplot(
      fp,
      aes(
        x = Beta_Z_to_Z,
        y = Association
      )
    ) +
      geom_vline(
        xintercept = 0,
        linetype = "dashed",
        linewidth = 0.45
      ) +
      geom_errorbarh(
        aes(
          xmin = CI_L95,
          xmax = CI_U95
        ),
        height = 0.15
      ) +
      geom_point(
        size = 2.4
      ) +
      labs(
        title = "Strongest FDR-significant normative brain–cognition associations",
        subtitle = "HC3 robust 95% CI; both structural and cognitive outcomes are cross-fitted normative z-scores",
        x = "β (cognitive CF z per 1 structural CF z)",
        y = NULL
      ) +
      theme_classic(base_size = 9) +
      theme(
        plot.title = element_text(face = "bold")
      )

    paths <- save_figure_pair(
      p,
      "P2_F3_Top_BrainCognition_Forest",
      width = 11,
      height = max(7, 0.34 * nrow(fp) + 2)
    )

    figure_index[[length(figure_index) + 1L]] <- data.frame(
      FigureCode = "P2_F3",
      Title = "Top FDR-significant normative brain-cognition associations",
      PNG = paths["PNG"],
      PDF = paths["PDF"],
      stringsAsFactors = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Figure 4: ROI high-resolution heatmap
  # ---------------------------------------------------------------------------

  if (isTRUE(MAKE_ROI_HEATMAP)) {

    rp <- primary %>%
      filter(
        StructuralScope == "ROI",
        is.finite(Beta_Z_to_Z)
      ) %>%
      mutate(
        CellLabel = ifelse(
          Significant_FDR,
          "*",
          ""
        )
      )

    if (nrow(rp)) {

      p <- ggplot(
        rp,
        aes(
          x = CognitiveLabel,
          y = StructuralLabel,
          fill = Beta_Z_to_Z
        )
      ) +
        geom_tile(
          color = "white",
          linewidth = 0.15
        ) +
        geom_point(
          data = rp %>% filter(Significant_FDR),
          shape = 8,
          size = 1.2,
          color = "black"
        ) +
        scale_fill_gradient2(
          low = "#3B6FB6",
          mid = "white",
          high = "#C95A55",
          midpoint = 0,
          name = "β"
        ) +
        facet_wrap(
          ~ Measure,
          ncol = 1,
          scales = "free_y"
        ) +
        labs(
          title = "DK68 normative brain–cognition association map",
          subtitle = "Asterisk marker = BH-FDR significant within CognitiveOutcome × Measure across ROIs",
          x = "Cognitive outcome",
          y = NULL
        ) +
        theme_minimal(base_size = 6.8) +
        theme(
          panel.grid = element_blank(),
          plot.title = element_text(face = "bold"),
          axis.text.x = element_text(
            angle = 45,
            hjust = 1
          )
        )

      paths <- save_figure_pair(
        p,
        "P2_S1_ROI_BrainCognition_Heatmap",
        width = 14,
        height = 24
      )

      figure_index[[length(figure_index) + 1L]] <- data.frame(
        FigureCode = "P2_S1",
        Title = "DK68 normative brain-cognition heatmap",
        PNG = paths["PNG"],
        PDF = paths["PDF"],
        stringsAsFactors = FALSE
      )
    }
  }
}

figure_index_df <- bind_rows(figure_index)

###############################################################################
# 12. README / METHODS NOTES
###############################################################################

readme <- data.frame(
  Item = c(
    "Scientific role",
    "Primary input",
    "Primary score",
    "Primary analysis",
    "Primary interpretation",
    "Secondary analysis",
    "Secondary covariates",
    "Sex interaction",
    "Robust inference",
    "Primary multiple testing",
    "ROI multiple testing",
    "Norm_Z sensitivity",
    "Cognitive score handling",
    "Important caution"
  ),
  Description = c(
    "Paper-2 module linking individual normative cortical deviations to individual cognitive deviations/performance.",
    normalizePath(BIOMARKER_FILE, winslash = "/", mustWork = FALSE),
    "CF_Norm_Z only. Missing CF_Norm_Z values are not replaced by full-sample Norm_Z in the primary analysis.",
    "Cognitive_CF_Norm_Z ~ Structural_CF_Norm_Z.",
    "The beta is cognitive normative-deviation SD per 1 structural normative-deviation SD.",
    "Observed_Cognition ~ Structural_CF_Norm_Z + Age + Sex + Education.",
    "Age, Sex, Education. Structural CF_Norm_Z has already been conditioned by its own BrainNormR structural model; eTIV is therefore not re-added here.",
    "Not included.",
    if (USE_HC3) "HC3 heteroskedasticity-robust covariance." else "Model-based covariance.",
    "BH within each StructuralScope × CognitiveOutcome family, except ROI.",
    "For ROI, BH is applied within CognitiveOutcome × Measure across 68 ROIs.",
    if (RUN_NORMZ_SENSITIVITY) "Enabled; secondary only." else "Disabled by default.",
    "ObservedValue is used exactly as modeled by the cognition BrainNormR pipeline; this script does not invert or transform cognitive scores.",
    "All associations are cross-sectional. They do not establish causality, mediation, or longitudinal within-person change."
  ),
  stringsAsFactors = FALSE
)

###############################################################################
# 13. EXPORT
###############################################################################

sheets_out <- list(
  README = readme,
  Score_QC = score_qc,
  Structural_Inventory = structural_outcomes,
  Cognitive_Inventory = cognitive_outcomes,
  Primary_CFZ_to_CFZ = primary,
  Primary_Significant_FDR = primary_sig,
  Primary_Summary = primary_summary,
  Strongest_By_Cognition = strongest_by_cognition,
  Secondary_CFZ_to_RawCog = raw_assoc,
  Secondary_Significant_FDR = raw_sig,
  Primary_Model_QC = primary_qc,
  Secondary_Model_QC = raw_qc,
  Structural_Duplicates_QC = structural_dup,
  Cognition_Duplicates_QC = cognition_dup,
  Figure_Index = figure_index_df
)

if (isTRUE(RUN_NORMZ_SENSITIVITY) &&
    nrow(normz_sensitivity)) {
  sheets_out$NormZ_Sensitivity <- normz_sensitivity
}

sheets_out <- sheets_out[
  vapply(
    sheets_out,
    function(x) is.data.frame(x) && ncol(x) > 0,
    logical(1)
  )
]

writexl::write_xlsx(
  sheets_out,
  OUT_XLSX
)

write.csv(
  primary,
  file.path(
    OUT_DIR,
    "Primary_CFNormZ_to_CFNormZ_AllTests.csv"
  ),
  row.names = FALSE
)

write.csv(
  primary_sig,
  file.path(
    OUT_DIR,
    "Primary_CFNormZ_to_CFNormZ_FDR_Significant.csv"
  ),
  row.names = FALSE
)

write.csv(
  raw_assoc,
  file.path(
    OUT_DIR,
    "Secondary_CFNormZ_to_RawCognition_AllTests.csv"
  ),
  row.names = FALSE
)

write.csv(
  primary_qc,
  file.path(
    OUT_DIR,
    "Primary_Model_QC.csv"
  ),
  row.names = FALSE
)

if (nrow(figure_index_df)) {
  write.csv(
    figure_index_df,
    file.path(
      OUT_DIR,
      "BRAIN_COGNITION_FIGURE_INDEX.csv"
    ),
    row.names = FALSE
  )
}

capture.output(
  sessionInfo(),
  file = file.path(
    OUT_DIR,
    "sessionInfo_BrainCognitionAssociations.txt"
  )
)

###############################################################################
# 14. COMPLETION MESSAGE
###############################################################################

cat("\n============================================================\n")
cat("BrainNormR brain–cognition association module completed.\n")
cat("============================================================\n")
cat("Input: ", BIOMARKER_FILE, "\n", sep = "")
cat("Output: ", OUT_XLSX, "\n", sep = "")
cat("\nStructural outcomes: ", nrow(structural_outcomes), "\n", sep = "")
cat("Cognitive outcomes: ", nrow(cognitive_outcomes), "\n", sep = "")
cat("Primary CF-z to CF-z tests: ", nrow(primary), "\n", sep = "")
cat(
  "Primary FDR-significant tests: ",
  nrow(primary_sig),
  "\n",
  sep = ""
)
cat("Secondary raw-cognition tests: ", nrow(raw_assoc), "\n", sep = "")
cat(
  "Secondary FDR-significant tests: ",
  nrow(raw_sig),
  "\n",
  sep = ""
)
cat("Figures created: ", nrow(figure_index_df), "\n", sep = "")
cat("============================================================\n")
