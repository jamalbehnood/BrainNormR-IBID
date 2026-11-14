###############################################################################
# BrainNormR — Thesis & Article Descriptive Tables and Figures
# Version: 1.0
#
# Purpose
#   Create publication/thesis-ready descriptive tables and figures directly
#   from the complete modelling dataset BEFORE normative-model post-processing.
#
# Designed for the current BrainNormR dataset structure:
#   - ID, Age, sex, edu, Hand_encoded, eTIV
#   - DK68 ROI measures: FD, volume, thickness, area, curvature
#   - Cognitive variables
#   - Whole-cortex, hemisphere-specific + bilateral lobe, hemisphere and Yeo7 grouped measures
#
# IMPORTANT
#   * This script does NOT refit BrainNormR/GAMLSS models.
#   * It is descriptive/sample-characterisation code.
#   * Raw reaction/TMT time variables remain raw here unless you explicitly
#     replace them by pre-specified speed variables.
###############################################################################

suppressPackageStartupMessages({
  pkgs <- c("readxl", "writexl", "dplyr", "tidyr", "ggplot2",
            "patchwork", "scales")
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    stop("Install required packages first: ", paste(miss, collapse = ", "))
  }
  library(readxl)
  library(writexl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

###############################################################################
# 1. USER SETTINGS
###############################################################################

# Recommended: put exactly one modelling Excel file in Data/.
DATA_DIR <- "Data"
DATA_FILE <- NULL      # NULL = auto-detect one .xlsx/.xls file in Data/
SHEET_NAME <- 1

OUT_DIR <- file.path("Results", "Descriptive_Thesis_Article")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

ID_COL   <- "ID"
AGE_COL  <- "Age"
SEX_COL  <- "sex"
EDU_COL  <- "edu"
HAND_COL <- "Hand_encoded"
ETIV_COL <- "eTIV"

FEMALE_CODE <- 1
MALE_CODE   <- 0

# Descriptive age periods (used only for summaries/plots, not modelling)
AGE_PERIOD_BREAKS <- c(-Inf, 40, 60, Inf)
AGE_PERIOD_LABELS <- c("Young adulthood (<40)",
                       "Middle adulthood (40-59)",
                       "Late adulthood (>=60)")

# Figure export
FIG_DPI <- 600
FIG_W   <- 10
FIG_H   <- 7.5

# Optional inference in descriptive tables.
# These are descriptive sex comparisons, not substitutes for the pooled
# BrainNormR Age x Sex sensitivity analysis.
ADD_SEX_COMPARISONS <- TRUE

###############################################################################
# 2. DATA FILE DISCOVERY
###############################################################################

if (is.null(DATA_FILE)) {
  if (!dir.exists(DATA_DIR)) {
    stop("Data directory not found: ", DATA_DIR)
  }
  candidates <- list.files(
    DATA_DIR,
    pattern = "\\.(xlsx|xls)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  candidates <- candidates[!grepl("^~\\$", basename(candidates))]
  if (length(candidates) != 1L) {
    stop(
      "Expected exactly one Excel modelling file in Data/. Found: ",
      length(candidates),
      "\nSet DATA_FILE manually at the top of this script if needed."
    )
  }
  DATA_FILE <- candidates[1]
}

if (!file.exists(DATA_FILE)) stop("DATA_FILE not found: ", DATA_FILE)

cat("Reading: ", DATA_FILE, "\n", sep = "")
df <- as.data.frame(readxl::read_excel(DATA_FILE, sheet = SHEET_NAME))

###############################################################################
# 3. BASIC VALIDATION AND DERIVED LABELS
###############################################################################

required_base <- c(AGE_COL, SEX_COL)
missing_base <- setdiff(required_base, names(df))
if (length(missing_base)) {
  stop("Missing required column(s): ", paste(missing_base, collapse = ", "))
}

# Preserve original column names exactly.
df[[AGE_COL]] <- suppressWarnings(as.numeric(df[[AGE_COL]]))

df$SexLabel <- dplyr::case_when(
  df[[SEX_COL]] == FEMALE_CODE ~ "Female",
  df[[SEX_COL]] == MALE_CODE   ~ "Male",
  TRUE                         ~ "Other/Unknown"
)
df$SexLabel <- factor(df$SexLabel, levels = c("Female", "Male", "Other/Unknown"))

df$AgePeriod <- cut(
  df[[AGE_COL]],
  breaks = AGE_PERIOD_BREAKS,
  labels = AGE_PERIOD_LABELS,
  right = FALSE,
  include.lowest = TRUE
)

# Convenient ID when no ID is present
if (!ID_COL %in% names(df)) {
  df[[ID_COL]] <- sprintf("Row_%03d", seq_len(nrow(df)))
}

###############################################################################
# 4. VARIABLE INVENTORY
###############################################################################

all_names <- names(df)

# ROI patterns
roi_fd   <- grep("^(lh|rh)_.+_FD$", all_names, value = TRUE, ignore.case = FALSE)
roi_vol  <- grep("^(lh|rh)_.+_volume$", all_names, value = TRUE, ignore.case = TRUE)
roi_thk  <- grep("^(lh|rh)_.+_thickness$", all_names, value = TRUE, ignore.case = TRUE)
roi_area <- grep("^(lh|rh)_.+_area$", all_names, value = TRUE, ignore.case = TRUE)
roi_curv <- grep("^(lh|rh)_.+_curvature$", all_names, value = TRUE, ignore.case = TRUE)

# Exclude grouped columns accidentally matching lh_/rh_
roi_vol  <- roi_vol [!grepl("^(lh|rh)_(Frontal|Parietal|Temporal|Occipital|Cingulate|Insula|hemisphere)_",
                               roi_vol, ignore.case=FALSE)]
roi_thk  <- roi_thk [!grepl("^(lh|rh)_(Frontal|Parietal|Temporal|Occipital|Cingulate|Insula|hemisphere)_",
                               roi_thk, ignore.case=FALSE)]
roi_area <- roi_area[!grepl("^(lh|rh)_(Frontal|Parietal|Temporal|Occipital|Cingulate|Insula|hemisphere)_",
                               roi_area, ignore.case=FALSE)]
roi_curv <- roi_curv[!grepl("^(lh|rh)_(Frontal|Parietal|Temporal|Occipital|Cingulate|Insula|hemisphere)_",
                               roi_curv, ignore.case=FALSE)]
roi_fd   <- roi_fd  [!grepl("^(lh|rh)_(Frontal|Parietal|Temporal|Occipital|Cingulate|Insula|hemisphere)_",
                               roi_fd, ignore.case=FALSE)]

wholebrain_vars <- grep("^wholebrain_(volume|area|thickness|curvature|FD)$",
                        all_names, value = TRUE, ignore.case = TRUE)

# Hemisphere-specific lobar aggregates (Left / Right)
lobe_vars <- grep("^(lh|rh)_(Frontal|Parietal|Temporal|Occipital|Cingulate|Insula)_(volume|area|thickness|curvature|FD)$",
                  all_names, value = TRUE, ignore.case = TRUE)

# Bilateral lobar aggregates created by the updated grouped-data builder.
# Example: bilateral_Frontal_volume, bilateral_Insula_FD
bilateral_lobe_vars <- grep("^bilateral_(Frontal|Parietal|Temporal|Occipital|Cingulate|Insula)_(volume|area|thickness|curvature|FD)$",
                            all_names, value = TRUE, ignore.case = TRUE)

hemisphere_vars <- grep("^(lh|rh)_hemisphere_(volume|area|thickness|curvature|FD)$",
                        all_names, value = TRUE, ignore.case = TRUE)

yeo_vars <- grep("^Yeo7_.+_(volume|area|thickness|curvature|FD)$",
                 all_names, value = TRUE, ignore.case = TRUE)

# Cognition: use explicit current-file names when present, then allow additions.
cognition_candidates <- c(
  "RAVLT-A1", "RAVLT-B1", "RAVLT-C1",
  "FDST-Tot.", "BDST-Tot.",
  "TMT-A-time", "TMT-B-time",
  "SDMT-All", "Block-score",
  "Simple RT-Mean T.", "Discriminative RT-Mean T.", "Choice RT-Mean T.",
  "RAVLTA1", "RAVLTB1", "RAVLTC1",
  "FDSTTot", "BDSTTot",
  "TMTA_speed", "TMTB_speed",
  "SDMTAll", "Blockscore",
  "SimpleRT_speed", "DiscRT_speed", "ChoiceRT_speed",
  "VFTCat", "IQ"
)
cognition_vars <- intersect(cognition_candidates, all_names)

base_vars <- intersect(c(ID_COL, AGE_COL, SEX_COL, EDU_COL, HAND_COL, ETIV_COL), all_names)

inventory <- data.frame(
  Category = c(
    rep("Base/Demographic", length(base_vars)),
    rep("Cognition", length(cognition_vars)),
    rep("WholeBrain", length(wholebrain_vars)),
    rep("Hemisphere", length(hemisphere_vars)),
    rep("Lobe_HemisphereSpecific", length(lobe_vars)),
    rep("Lobe_Bilateral", length(bilateral_lobe_vars)),
    rep("Yeo7", length(yeo_vars)),
    rep("ROI_FD", length(roi_fd)),
    rep("ROI_Volume", length(roi_vol)),
    rep("ROI_Thickness", length(roi_thk)),
    rep("ROI_Area", length(roi_area)),
    rep("ROI_Curvature", length(roi_curv))
  ),
  Variable = c(
    base_vars, cognition_vars, wholebrain_vars, hemisphere_vars, lobe_vars,
    bilateral_lobe_vars, yeo_vars, roi_fd, roi_vol, roi_thk, roi_area, roi_curv
  ),
  stringsAsFactors = FALSE
)

cat("\nVariable inventory:\n")
print(table(inventory$Category))

###############################################################################
# 5. HELPER FUNCTIONS
###############################################################################

safe_num <- function(x) suppressWarnings(as.numeric(x))

fmt_num <- function(x, digits = 2) {
  if (!is.finite(x)) return(NA_character_)
  formatC(x, digits = digits, format = "f")
}

fmt_mean_sd <- function(x, digits = 2) {
  x <- safe_num(x); x <- x[is.finite(x)]
  if (!length(x)) return(NA_character_)
  sprintf(paste0("%.",digits,"f ± %.",digits,"f"), mean(x), sd(x))
}

fmt_median_iqr <- function(x, digits = 2) {
  x <- safe_num(x); x <- x[is.finite(x)]
  if (!length(x)) return(NA_character_)
  q <- quantile(x, c(.25,.5,.75), na.rm=TRUE, names=FALSE)
  sprintf(paste0("%.",digits,"f [%.",digits,"f, %.",digits,"f]"), q[2], q[1], q[3])
}

fmt_range <- function(x, digits = 2) {
  x <- safe_num(x); x <- x[is.finite(x)]
  if (!length(x)) return(NA_character_)
  rr <- range(x)
  sprintf(paste0("%.",digits,"f–%.",digits,"f"), rr[1], rr[2])
}

hedges_g <- function(x, group) {
  x <- safe_num(x)
  ok <- is.finite(x) & group %in% c("Female","Male")
  x <- x[ok]; group <- droplevels(factor(group[ok], levels=c("Female","Male")))
  if (length(unique(group)) != 2) return(NA_real_)
  x1 <- x[group=="Female"]; x0 <- x[group=="Male"]
  n1 <- length(x1); n0 <- length(x0)
  if (n1 < 2 || n0 < 2) return(NA_real_)
  s1 <- sd(x1); s0 <- sd(x0)
  sp <- sqrt(((n1-1)*s1^2 + (n0-1)*s0^2)/(n1+n0-2))
  if (!is.finite(sp) || sp == 0) return(NA_real_)
  d <- (mean(x1)-mean(x0))/sp
  J <- 1 - 3/(4*(n1+n0)-9)
  J*d
}

welch_p <- function(x, group) {
  x <- safe_num(x)
  ok <- is.finite(x) & group %in% c("Female","Male")
  if (sum(ok) < 6 || length(unique(group[ok])) != 2) return(NA_real_)
  tryCatch(t.test(x[ok] ~ droplevels(group[ok]))$p.value,
           error=function(e) NA_real_)
}

continuous_summary <- function(data, vars, add_tests = FALSE) {
  if (!length(vars)) return(data.frame())
  out <- lapply(vars, function(v) {
    x <- safe_num(data[[v]])
    Fv <- x[data$SexLabel=="Female"]
    Mv <- x[data$SexLabel=="Male"]
    data.frame(
      Variable = v,
      N = sum(is.finite(x)),
      Missing_n = sum(!is.finite(x)),
      Missing_pct = 100*mean(!is.finite(x)),
      Overall_Mean_SD = fmt_mean_sd(x),
      Overall_Median_IQR = fmt_median_iqr(x),
      Overall_Range = fmt_range(x),
      Female_Mean_SD = fmt_mean_sd(Fv),
      Male_Mean_SD = fmt_mean_sd(Mv),
      Sex_Welch_p = if (add_tests) welch_p(x, data$SexLabel) else NA_real_,
      Hedges_g_FemaleMinusMale = if (add_tests) hedges_g(x, data$SexLabel) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  ans <- bind_rows(out)
  if (add_tests && nrow(ans)) {
    ans$Sex_BH_q <- p.adjust(ans$Sex_Welch_p, method="BH")
  }
  ans
}

categorical_summary <- function(data, var) {
  if (!var %in% names(data)) return(data.frame())
  x <- as.character(data[[var]])
  x[is.na(x) | trimws(x)==""] <- "Missing"
  tab <- as.data.frame(table(x, data$SexLabel, useNA="ifany"), stringsAsFactors=FALSE)
  names(tab) <- c("Category","Sex","N")
  tab <- tab %>%
    group_by(Sex) %>%
    mutate(Percent_within_sex = 100*N/sum(N)) %>%
    ungroup()
  tab
}

zscore_long <- function(data, vars, id_cols=c("Age","SexLabel","AgePeriod")) {
  vars <- vars[vars %in% names(data)]
  if (!length(vars)) return(data.frame())
  zdat <- data %>%
    select(all_of(intersect(id_cols,names(data))), all_of(vars)) %>%
    mutate(across(all_of(vars), ~{
      z <- safe_num(.x)
      s <- sd(z, na.rm=TRUE)
      if (!is.finite(s) || s==0) rep(NA_real_, length(z)) else (z-mean(z,na.rm=TRUE))/s
    })) %>%
    pivot_longer(cols=all_of(vars), names_to="Variable", values_to="Z")
  zdat
}

measure_from_name <- function(x) {
  dplyr::case_when(
    grepl("_volume$", x, ignore.case=TRUE) ~ "Volume",
    grepl("_area$", x, ignore.case=TRUE) ~ "Area",
    grepl("_thickness$", x, ignore.case=TRUE) ~ "Thickness",
    grepl("_curvature$", x, ignore.case=TRUE) ~ "Curvature",
    grepl("_FD$", x, ignore.case=FALSE) | grepl("_fd$", x, ignore.case=TRUE) ~ "FD",
    TRUE ~ "Other"
  )
}

save_plot <- function(p, stem, width=FIG_W, height=FIG_H) {
  ggsave(file.path(OUT_DIR, paste0(stem, ".png")), p,
         width=width, height=height, dpi=FIG_DPI, bg="white")
  ggsave(file.path(OUT_DIR, paste0(stem, ".pdf")), p,
         width=width, height=height, bg="white")
}

theme_pub <- theme_classic(base_size=11) +
  theme(
    plot.title = element_text(face="bold", size=13),
    plot.subtitle = element_text(size=10),
    strip.background = element_rect(fill="grey95", color="grey75"),
    strip.text = element_text(face="bold"),
    legend.position = "bottom"
  )

###############################################################################
# 6. SAMPLE OVERVIEW / TABLE 1
###############################################################################

n_all <- nrow(df)
n_f <- sum(df$SexLabel=="Female", na.rm=TRUE)
n_m <- sum(df$SexLabel=="Male", na.rm=TRUE)

sample_overview <- data.frame(
  Item = c("Total N", "Female n", "Male n",
           "Age mean ± SD", "Age median [IQR]", "Age range"),
  Value = c(
    n_all, n_f, n_m,
    fmt_mean_sd(df[[AGE_COL]]),
    fmt_median_iqr(df[[AGE_COL]]),
    fmt_range(df[[AGE_COL]])
  ),
  stringsAsFactors=FALSE
)

table1_cont_vars <- intersect(c(AGE_COL, EDU_COL, ETIV_COL), names(df))
table1_cont <- continuous_summary(df, table1_cont_vars, ADD_SEX_COMPARISONS)

# Make friendly labels
friendly <- c(
  Age = "Age (years)",
  edu = "Education (years)",
  eTIV = "Estimated total intracranial volume"
)
table1_cont$Label <- ifelse(table1_cont$Variable %in% names(friendly),
                            friendly[table1_cont$Variable],
                            table1_cont$Variable)
table1_cont <- table1_cont %>%
  select(Label, everything(), -Variable)

sex_counts <- as.data.frame(table(df$SexLabel), stringsAsFactors=FALSE)
names(sex_counts) <- c("Sex","N")
sex_counts$Percent <- 100*sex_counts$N/sum(sex_counts$N)

hand_table <- categorical_summary(df, HAND_COL)

age_period_by_sex <- df %>%
  filter(!is.na(AgePeriod), !is.na(SexLabel)) %>%
  count(AgePeriod, SexLabel, name="N") %>%
  group_by(SexLabel) %>%
  mutate(Percent_within_sex=100*N/sum(N)) %>%
  ungroup()

###############################################################################
# 7. DESCRIPTIVE TABLES BY DATA LEVEL
###############################################################################

cognition_desc <- continuous_summary(df, cognition_vars, ADD_SEX_COMPARISONS)
wholebrain_desc <- continuous_summary(df, wholebrain_vars, ADD_SEX_COMPARISONS)
hemisphere_desc     <- continuous_summary(df, hemisphere_vars, FALSE)
lobe_desc           <- continuous_summary(df, lobe_vars, FALSE)
bilateral_lobe_desc <- continuous_summary(df, bilateral_lobe_vars, FALSE)
yeo_desc            <- continuous_summary(df, yeo_vars, FALSE)

# Add readable lobe/measure labels to the bilateral table.
if (nrow(bilateral_lobe_desc)) {
  bilateral_lobe_desc$Measure <- measure_from_name(bilateral_lobe_desc$Variable)
  bilateral_lobe_desc$Lobe <- bilateral_lobe_desc$Variable
  bilateral_lobe_desc$Lobe <- sub("^bilateral_", "", bilateral_lobe_desc$Lobe, ignore.case=TRUE)
  bilateral_lobe_desc$Lobe <- sub("_(volume|area|thickness|curvature|FD)$", "",
                                  bilateral_lobe_desc$Lobe, ignore.case=TRUE)
  bilateral_lobe_desc <- bilateral_lobe_desc %>%
    select(Measure, Lobe, everything())
}

roi_all <- unique(c(roi_fd, roi_vol, roi_thk, roi_area, roi_curv))
roi_desc <- continuous_summary(df, roi_all, FALSE)
if (nrow(roi_desc)) {
  roi_desc$Measure <- measure_from_name(roi_desc$Variable)
  roi_desc$Hemisphere <- ifelse(grepl("^lh_",roi_desc$Variable), "Left",
                         ifelse(grepl("^rh_",roi_desc$Variable), "Right", NA))
  roi_desc$ROI <- roi_desc$Variable
  roi_desc$ROI <- sub("^(lh|rh)_", "", roi_desc$ROI)
  roi_desc$ROI <- sub("_(FD|fd|volume|area|thickness|curvature)$", "",
                      roi_desc$ROI, ignore.case=TRUE)
  roi_desc <- roi_desc %>%
    select(Measure, Hemisphere, ROI, everything())
}

###############################################################################
# 8. MISSINGNESS
###############################################################################

missingness <- data.frame(
  Variable = names(df),
  Missing_n = vapply(df, function(x) sum(is.na(x) | (is.character(x) & trimws(x)=="")), numeric(1)),
  stringsAsFactors=FALSE
)
missingness$Missing_pct <- 100*missingness$Missing_n/nrow(df)
missingness$Category <- dplyr::case_when(
  missingness$Variable %in% cognition_vars ~ "Cognition",
  missingness$Variable %in% wholebrain_vars ~ "WholeBrain",
  missingness$Variable %in% hemisphere_vars ~ "Hemisphere",
  missingness$Variable %in% lobe_vars ~ "Lobe_HemisphereSpecific",
  missingness$Variable %in% bilateral_lobe_vars ~ "Lobe_Bilateral",
  missingness$Variable %in% yeo_vars ~ "Yeo7",
  missingness$Variable %in% roi_all ~ "ROI",
  missingness$Variable %in% base_vars ~ "Base/Demographic",
  TRUE ~ "Other"
)
missingness <- missingness %>% arrange(desc(Missing_pct), Variable)

###############################################################################
# 9. TABLE: AGE PERIOD SAMPLE COMPOSITION
###############################################################################

age_period_summary <- df %>%
  filter(!is.na(AgePeriod)) %>%
  group_by(AgePeriod) %>%
  summarise(
    N = n(),
    Female_n = sum(SexLabel=="Female", na.rm=TRUE),
    Male_n = sum(SexLabel=="Male", na.rm=TRUE),
    Age_Mean = mean(.data[[AGE_COL]], na.rm=TRUE),
    Age_SD = sd(.data[[AGE_COL]], na.rm=TRUE),
    Education_Mean = if (EDU_COL %in% names(df)) mean(safe_num(.data[[EDU_COL]]), na.rm=TRUE) else NA_real_,
    Education_SD = if (EDU_COL %in% names(df)) sd(safe_num(.data[[EDU_COL]]), na.rm=TRUE) else NA_real_,
    .groups="drop"
  )

###############################################################################
# 10. FIGURE 1 — AGE DISTRIBUTION AND SAMPLE COMPOSITION
###############################################################################

p1a <- ggplot(df, aes(x=.data[[AGE_COL]], fill=SexLabel)) +
  geom_histogram(position="identity", alpha=.45, bins=18, color="white") +
  geom_density(aes(y=after_stat(count)), alpha=.12, linewidth=.7) +
  labs(
    title="Age distribution of the study sample",
    x="Age (years)", y="Count", fill=NULL
  ) +
  theme_pub

p1b <- ggplot(age_period_by_sex, aes(x=AgePeriod, y=N, fill=SexLabel)) +
  geom_col(position=position_dodge(width=.8), width=.72) +
  labs(
    title="Sample composition across adult age periods",
    x=NULL, y="Participants, n", fill=NULL
  ) +
  scale_x_discrete(labels=c("Young\n(<40)", "Middle\n(40–59)", "Late\n(≥60)")) +
  theme_pub

fig1 <- p1a / p1b + plot_annotation(tag_levels="A")
save_plot(fig1, "Figure_01_Sample_Age_Distribution", width=10, height=8)

###############################################################################
# 11. FIGURE 2 — EDUCATION vs AGE (DESCRIPTIVE)
###############################################################################

if (EDU_COL %in% names(df)) {
  p2 <- ggplot(df, aes(x=.data[[AGE_COL]], y=safe_num(.data[[EDU_COL]]), color=SexLabel)) +
    geom_point(alpha=.45, size=1.7) +
    geom_smooth(method="lm", formula=y~x, se=TRUE, linewidth=.8) +
    labs(
      title="Education across age",
      subtitle="Descriptive linear trend only; not a normative-model result",
      x="Age (years)", y="Education (years)", color=NULL
    ) +
    theme_pub
  save_plot(p2, "Figure_02_Age_Education", width=8.5, height=6.5)
}

###############################################################################
# 12. FIGURE 3 — WHOLE-CORTEX FIVE-MEASURE DESCRIPTIVE TRAJECTORIES
###############################################################################

if (length(wholebrain_vars)) {
  wb_long <- zscore_long(df, wholebrain_vars)
  wb_long$Measure <- measure_from_name(wb_long$Variable)
  wb_long$Measure <- factor(wb_long$Measure,
                            levels=c("Volume","Area","Thickness","Curvature","FD"))

  p3 <- ggplot(wb_long, aes(x=Age, y=Z)) +
    geom_point(alpha=.22, size=.9) +
    geom_smooth(method="loess", formula=y~x, se=TRUE,
                span=.75, linewidth=.9) +
    geom_hline(yintercept=0, linetype="dashed", linewidth=.35) +
    facet_wrap(~Measure, scales="free_y", ncol=3) +
    labs(
      title="Whole-cortex structural measures across age",
      subtitle="Raw measures standardized within each measure; LOESS is descriptive only",
      x="Age (years)", y="Standardized value (z)"
    ) +
    theme_pub
  save_plot(p3, "Figure_03_WholeCortex_5Measures_Descriptive", width=10.5, height=8)
}

###############################################################################
# 13. FIGURE 4 — WHOLE-CORTEX MEASURES BY AGE PERIOD
###############################################################################

if (length(wholebrain_vars)) {
  wb_box <- zscore_long(df, wholebrain_vars)
  wb_box$Measure <- measure_from_name(wb_box$Variable)
  wb_box$Measure <- factor(wb_box$Measure,
                           levels=c("Volume","Area","Thickness","Curvature","FD"))

  p4 <- ggplot(wb_box, aes(x=AgePeriod, y=Z, fill=AgePeriod)) +
    geom_boxplot(width=.68, outlier.alpha=.20) +
    facet_wrap(~Measure, ncol=3) +
    scale_x_discrete(labels=c("Young","Middle","Late")) +
    labs(
      title="Whole-cortex morphology across adult age periods",
      x=NULL, y="Standardized value (z)", fill=NULL
    ) +
    theme_pub +
    theme(legend.position="none")
  save_plot(p4, "Figure_04_WholeCortex_By_AgePeriod", width=10.5, height=8)
}

###############################################################################
# 14. FIGURE 5 — COGNITIVE OUTCOMES ACROSS AGE
###############################################################################

if (length(cognition_vars)) {
  cog_long <- zscore_long(df, cognition_vars)
  p5 <- ggplot(cog_long, aes(x=Age, y=Z)) +
    geom_point(alpha=.20, size=.75) +
    geom_smooth(method="loess", formula=y~x, se=TRUE,
                span=.75, linewidth=.8) +
    geom_hline(yintercept=0, linetype="dashed", linewidth=.30) +
    facet_wrap(~Variable, scales="free_y", ncol=3) +
    labs(
      title="Cognitive measures across age",
      subtitle=paste0(
        "Each outcome is standardized separately. ",
        "Raw time-based variables retain their original direction."
      ),
      x="Age (years)", y="Standardized value (z)"
    ) +
    theme_pub +
    theme(strip.text=element_text(size=8))
  save_plot(p5, "Figure_05_Cognition_Descriptive_Age_Trends", width=11, height=10)
}

###############################################################################
# 15. FIGURE 6 — CORRELATION HEATMAP
###############################################################################

corr_vars <- unique(c(AGE_COL, EDU_COL, ETIV_COL, wholebrain_vars, cognition_vars))
corr_vars <- corr_vars[corr_vars %in% names(df)]
corr_num <- df %>%
  select(all_of(corr_vars)) %>%
  mutate(across(everything(), safe_num))

if (ncol(corr_num) >= 2) {
  C <- suppressWarnings(cor(corr_num, use="pairwise.complete.obs", method="pearson"))
  corr_long <- as.data.frame(as.table(C), stringsAsFactors=FALSE)
  names(corr_long) <- c("Variable1","Variable2","r")
  corr_long <- corr_long %>%
    mutate(
      Variable1=factor(Variable1, levels=colnames(C)),
      Variable2=factor(Variable2, levels=rev(colnames(C)))
    )

  p6 <- ggplot(corr_long, aes(Variable1, Variable2, fill=r)) +
    geom_tile(color="white", linewidth=.25) +
    scale_fill_gradient2(low="#6F8FB8", mid="#F7F7F7", high="#C46E6E",
                         midpoint=0, limits=c(-1,1), name="Pearson r") +
    coord_fixed() +
    labs(
      title="Correlation matrix of demographics, whole-cortex morphology, and cognition",
      x=NULL, y=NULL
    ) +
    theme_minimal(base_size=9) +
    theme(
      axis.text.x=element_text(angle=55, hjust=1, vjust=1),
      panel.grid=element_blank(),
      legend.position="right"
    )
  save_plot(p6, "Figure_06_Correlation_Heatmap", width=12, height=10)
}

###############################################################################
# 16. FIGURE 7 — SEX-STRATIFIED WHOLE-CORTEX DISTRIBUTIONS
###############################################################################

if (length(wholebrain_vars)) {
  wb_sex <- zscore_long(df, wholebrain_vars)
  wb_sex$Measure <- measure_from_name(wb_sex$Variable)

  p7 <- ggplot(wb_sex %>% filter(SexLabel %in% c("Female","Male")),
               aes(x=SexLabel, y=Z, fill=SexLabel)) +
    geom_violin(alpha=.30, trim=FALSE, width=.9) +
    geom_boxplot(width=.22, outlier.shape=NA, alpha=.75) +
    facet_wrap(~Measure, ncol=3) +
    labs(
      title="Sex-stratified distributions of whole-cortex structural measures",
      subtitle="Descriptive standardized distributions; inferential Age × Sex testing belongs to BrainNormR",
      x=NULL, y="Standardized value (z)", fill=NULL
    ) +
    theme_pub +
    theme(legend.position="none")
  save_plot(p7, "Figure_07_WholeCortex_By_Sex", width=10.5, height=8)
}

###############################################################################
# 17. FIGURE 8 — MISSING DATA OVERVIEW
###############################################################################

miss_plot_data <- missingness %>%
  filter(Missing_n > 0) %>%
  arrange(desc(Missing_pct)) %>%
  slice_head(n=30)

if (nrow(miss_plot_data)) {
  miss_plot_data$Variable <- factor(
    miss_plot_data$Variable,
    levels=rev(miss_plot_data$Variable)
  )

  p8 <- ggplot(miss_plot_data, aes(x=Missing_pct, y=Variable)) +
    geom_col(width=.7) +
    labs(
      title="Variables with missing data",
      subtitle="Top 30 variables by missing-data percentage",
      x="Missing (%)", y=NULL
    ) +
    theme_pub
  save_plot(p8, "Figure_08_Missingness", width=9, height=8)
}

###############################################################################
# 18. SUPPLEMENTARY FIGURE — LOBAR STANDARDIZED AGE TRENDS
###############################################################################

if (length(lobe_vars)) {
  lob_long <- zscore_long(df, lobe_vars)
  lob_long$Measure <- measure_from_name(lob_long$Variable)
  lob_long$Hemisphere <- ifelse(grepl("^lh_",lob_long$Variable),"Left","Right")
  lob_long$Lobe <- lob_long$Variable
  lob_long$Lobe <- sub("^(lh|rh)_", "", lob_long$Lobe)
  lob_long$Lobe <- sub("_(volume|area|thickness|curvature|FD)$", "",
                       lob_long$Lobe, ignore.case=TRUE)

  # Summarize raw points by 2-year bins to keep a supplementary figure readable.
  lob_plot <- lob_long %>%
    mutate(AgeBin = floor(Age/2)*2) %>%
    group_by(AgeBin, Measure, Hemisphere, Lobe) %>%
    summarise(Z=mean(Z,na.rm=TRUE), .groups="drop")

  pS1 <- ggplot(lob_plot, aes(x=AgeBin, y=Z,
                             linetype=Hemisphere,
                             group=interaction(Lobe,Hemisphere))) +
    geom_line(linewidth=.55, alpha=.8) +
    facet_grid(Measure ~ Lobe, scales="free_y") +
    labs(
      title="Supplementary: lobar standardized descriptive age profiles",
      x="Age (years)", y="Mean standardized value (z)", linetype=NULL
    ) +
    theme_minimal(base_size=8) +
    theme(
      panel.grid.minor=element_blank(),
      strip.text=element_text(size=7),
      legend.position="bottom"
    )
  save_plot(pS1, "Supplementary_Figure_S1_Lobar_Age_Profiles",
            width=13, height=10)
}

###############################################################################
# 19. SUPPLEMENTARY FIGURE — YEO7 STANDARDIZED AGE TRENDS
###############################################################################

if (length(yeo_vars)) {
  yeo_long <- zscore_long(df, yeo_vars)
  yeo_long$Measure <- measure_from_name(yeo_long$Variable)
  yeo_long$Network <- yeo_long$Variable
  yeo_long$Network <- sub("^Yeo7_", "", yeo_long$Network)
  yeo_long$Network <- sub("_(volume|area|thickness|curvature|FD)$", "",
                          yeo_long$Network, ignore.case=TRUE)

  yeo_plot <- yeo_long %>%
    mutate(AgeBin=floor(Age/2)*2) %>%
    group_by(AgeBin, Measure, Network) %>%
    summarise(Z=mean(Z,na.rm=TRUE), .groups="drop")

  pS2 <- ggplot(yeo_plot, aes(x=AgeBin, y=Z, group=Network)) +
    geom_line(linewidth=.6, alpha=.85) +
    facet_wrap(~Measure, ncol=1, scales="free_y") +
    labs(
      title="Supplementary: Yeo7 network-level descriptive age profiles",
      x="Age (years)", y="Mean standardized value (z)"
    ) +
    theme_pub
  save_plot(pS2, "Supplementary_Figure_S2_Yeo7_Age_Profiles",
            width=9.5, height=12)
}

###############################################################################
# 20. SUPPLEMENTARY FIGURE S3 — BILATERAL LOBAR STANDARDIZED AGE TRENDS
###############################################################################

# This figure complements S1. S1 preserves left/right hemispheric information,
# whereas S3 shows the bilateral lobe aggregates used in grouped BrainNormR.
# Values are standardized within each bilateral lobe × measure variable.
# Two-year bins are used only for descriptive visualization.
if (length(bilateral_lobe_vars)) {
  bilob_long <- zscore_long(df, bilateral_lobe_vars)
  bilob_long$Measure <- measure_from_name(bilob_long$Variable)
  bilob_long$Measure <- factor(
    bilob_long$Measure,
    levels=c("Volume","Area","Thickness","Curvature","FD")
  )

  bilob_long$Lobe <- bilob_long$Variable
  bilob_long$Lobe <- sub("^bilateral_", "", bilob_long$Lobe, ignore.case=TRUE)
  bilob_long$Lobe <- sub(
    "_(volume|area|thickness|curvature|FD)$", "",
    bilob_long$Lobe, ignore.case=TRUE
  )
  bilob_long$Lobe <- factor(
    bilob_long$Lobe,
    levels=c("Frontal","Parietal","Temporal","Occipital","Cingulate","Insula")
  )

  bilob_plot <- bilob_long %>%
    mutate(AgeBin=floor(Age/2)*2) %>%
    group_by(AgeBin, Measure, Lobe) %>%
    summarise(Z=mean(Z,na.rm=TRUE), .groups="drop")

  pS3 <- ggplot(bilob_plot, aes(x=AgeBin, y=Z, group=Lobe)) +
    geom_line(linewidth=.65, alpha=.85) +
    geom_hline(yintercept=0, linetype="dashed", linewidth=.30) +
    facet_grid(Measure ~ Lobe, scales="free_y") +
    labs(
      title="Supplementary: bilateral lobar standardized descriptive age profiles",
      subtitle="Bilateral lobe aggregates; 2-year age bins; descriptive only",
      x="Age (years)", y="Mean standardized value (z)"
    ) +
    theme_minimal(base_size=8) +
    theme(
      panel.grid.minor=element_blank(),
      strip.text=element_text(size=7),
      legend.position="none"
    )

  save_plot(
    pS3,
    "Supplementary_Figure_S3_Bilateral_Lobar_Age_Profiles",
    width=13, height=10
  )
}

###############################################################################
# 21. DATA DICTIONARY / VARIABLE CLASSIFICATION
###############################################################################

data_dictionary <- data.frame(
  Variable = names(df),
  Class = vapply(df, function(x) class(x)[1], character(1)),
  Category = dplyr::case_when(
    names(df) %in% base_vars ~ "Base/Demographic",
    names(df) %in% cognition_vars ~ "Cognition",
    names(df) %in% wholebrain_vars ~ "WholeBrain",
    names(df) %in% hemisphere_vars ~ "Hemisphere",
    names(df) %in% lobe_vars ~ "Lobe_HemisphereSpecific",
    names(df) %in% bilateral_lobe_vars ~ "Lobe_Bilateral",
    names(df) %in% yeo_vars ~ "Yeo7",
    names(df) %in% roi_fd ~ "ROI_FD",
    names(df) %in% roi_vol ~ "ROI_Volume",
    names(df) %in% roi_thk ~ "ROI_Thickness",
    names(df) %in% roi_area ~ "ROI_Area",
    names(df) %in% roi_curv ~ "ROI_Curvature",
    TRUE ~ "Other"
  ),
  stringsAsFactors=FALSE
)

###############################################################################
# 22. ARTICLE-READY COMPACT TABLES
###############################################################################

# Article Table 1: compact demographics
article_table1 <- table1_cont %>%
  select(
    Variable=Label,
    N, Missing_n, Missing_pct,
    Overall=Overall_Mean_SD,
    Female=Female_Mean_SD,
    Male=Male_Mean_SD,
    Sex_Welch_p, Sex_BH_q, Hedges_g_FemaleMinusMale
  )

# Article Table 2: whole-cortex five structural measures
article_table2 <- wholebrain_desc
if (nrow(article_table2)) {
  article_table2$Measure <- measure_from_name(article_table2$Variable)
  article_table2 <- article_table2 %>%
    select(Measure, Variable, N, Missing_n, Missing_pct,
           Overall_Mean_SD, Female_Mean_SD, Male_Mean_SD,
           Sex_Welch_p, Sex_BH_q, Hedges_g_FemaleMinusMale)
}

# Article Table 3: cognition
article_table3 <- cognition_desc %>%
  select(Variable, N, Missing_n, Missing_pct,
         Overall_Mean_SD, Female_Mean_SD, Male_Mean_SD,
         Sex_Welch_p, Sex_BH_q, Hedges_g_FemaleMinusMale)

###############################################################################
# 23. EXPORT EXCEL WORKBOOKS
###############################################################################

sheets <- list(
  README = data.frame(
    Item=c(
      "Purpose",
      "Primary descriptive workbook",
      "Sex comparison",
      "Important caution 1",
      "Important caution 2"
    ),
    Description=c(
      "Descriptive sample characterisation for thesis/article; no BrainNormR model refitting.",
      "Use Article_Table1/2/3 for manuscript-ready compact tables; full tables are for thesis/supplement. BilateralLobe_Descriptives contains the six bilateral lobes when those variables exist.",
      "Welch t-test + Hedges g; BH q is provided within each table where tests are included.",
      "Time-based cognition variables retain their raw direction in this script.",
      "Descriptive sex comparisons do not replace the pooled BrainNormR Age x Sex sensitivity analysis."
    )
  ),
  Sample_Overview = sample_overview,
  Article_Table1_Demographics = article_table1,
  Sex_Counts = sex_counts,
  Handedness_by_Sex = hand_table,
  AgePeriod_by_Sex = age_period_by_sex,
  AgePeriod_Summary = age_period_summary,
  Article_Table2_WholeBrain = article_table2,
  Article_Table3_Cognition = article_table3,
  WholeBrain_Descriptives = wholebrain_desc,
  Cognition_Descriptives = cognition_desc,
  Hemisphere_Descriptives = hemisphere_desc,
  Lobe_Descriptives = lobe_desc,
  BilateralLobe_Descriptives = bilateral_lobe_desc,
  Yeo7_Descriptives = yeo_desc,
  ROI_Descriptives = roi_desc,
  Missingness = missingness,
  Variable_Inventory = inventory,
  Data_Dictionary = data_dictionary
)

# writexl cannot write zero-column data frames.
sheets <- sheets[vapply(sheets, function(x) is.data.frame(x) && ncol(x)>0, logical(1))]

xlsx_out <- file.path(OUT_DIR, "THESIS_ARTICLE_DESCRIPTIVE_TABLES.xlsx")
writexl::write_xlsx(sheets, xlsx_out)

###############################################################################
# 24. MACHINE-READABLE CSV EXPORTS
###############################################################################

write.csv(article_table1,
          file.path(OUT_DIR, "Article_Table1_Demographics.csv"),
          row.names=FALSE)
write.csv(article_table2,
          file.path(OUT_DIR, "Article_Table2_WholeBrain.csv"),
          row.names=FALSE)
write.csv(article_table3,
          file.path(OUT_DIR, "Article_Table3_Cognition.csv"),
          row.names=FALSE)
write.csv(missingness,
          file.path(OUT_DIR, "Missingness_Summary.csv"),
          row.names=FALSE)
write.csv(bilateral_lobe_desc,
          file.path(OUT_DIR, "BilateralLobe_Descriptives.csv"),
          row.names=FALSE)
write.csv(data_dictionary,
          file.path(OUT_DIR, "Data_Dictionary.csv"),
          row.names=FALSE)

###############################################################################
# 25. SESSION / RUN METADATA
###############################################################################

meta <- data.frame(
  Field=c("RunDateTime","DataFile","Sheet","N","N_Female","N_Male",
          "Age_Min","Age_Max","R_Version"),
  Value=c(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    normalizePath(DATA_FILE, winslash="/", mustWork=FALSE),
    as.character(SHEET_NAME),
    nrow(df), n_f, n_m,
    min(df[[AGE_COL]], na.rm=TRUE),
    max(df[[AGE_COL]], na.rm=TRUE),
    R.version.string
  ),
  stringsAsFactors=FALSE
)
write.csv(meta, file.path(OUT_DIR, "RUN_METADATA.csv"), row.names=FALSE)
capture.output(sessionInfo(), file=file.path(OUT_DIR, "sessionInfo.txt"))

###############################################################################
# 26. COMPLETION MESSAGE
###############################################################################

cat("\n============================================================\n")
cat("Descriptive thesis/article outputs completed.\n")
cat("Output directory: ", OUT_DIR, "\n", sep="")
cat("Main workbook:    ", xlsx_out, "\n", sep="")
cat("Detected variables:\n")
cat("  Cognition:   ", length(cognition_vars), "\n", sep="")
cat("  WholeBrain:  ", length(wholebrain_vars), "\n", sep="")
cat("  Hemisphere:       ", length(hemisphere_vars), "\n", sep="")
cat("  Lobe L/R:         ", length(lobe_vars), "\n", sep="")
cat("  Lobe Bilateral:   ", length(bilateral_lobe_vars), "\n", sep="")
cat("  Yeo7:             ", length(yeo_vars), "\n", sep="")
cat("  ROI total:   ", length(roi_all), "\n", sep="")
cat("============================================================\n")
