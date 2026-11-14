##############################################################################
# 10_final_pipeline_audit.R
#
# BrainNormR FINAL FROZEN audit
#
# Run this LAST, after all requested analyses, 03, 04, 08 and 09.
# It does NOT fit or refit any statistical model.
#
# Output:
#   Results/Final_Audit/FINAL_PIPELINE_AUDIT.xlsx
#   Results/Final_Audit/FINAL_PIPELINE_AUDIT.txt
##############################################################################

suppressPackageStartupMessages({
  pkgs <- c("readxl","writexl","dplyr")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("Missing package: ", p, ". Run 00_install_packages.R first.")
  }
})
library(readxl); library(writexl); library(dplyr)

RESULTS_ROOT <- "Results"
AUDIT_DIR <- file.path(RESULTS_ROOT,"Final_Audit")
AUDIT_XLSX <- file.path(AUDIT_DIR,"FINAL_PIPELINE_AUDIT.xlsx")
AUDIT_TXT <- file.path(AUDIT_DIR,"FINAL_PIPELINE_AUDIT.txt")
dir.create(AUDIT_DIR,recursive=TRUE,showWarnings=FALSE)

# =============================================================================
# HELPERS
# =============================================================================

safe_num <- function(x) suppressWarnings(as.numeric(x))

read_sheet_safe <- function(path,sheet) {
  if (!file.exists(path)) return(NULL)
  sh <- tryCatch(readxl::excel_sheets(path),error=function(e) character())
  if (!sheet %in% sh) return(NULL)
  tryCatch(as.data.frame(readxl::read_excel(path,sheet=sheet)),error=function(e) NULL)
}

find_result_workbooks <- function() {
  x <- list.files(
    RESULTS_ROOT,
    pattern="^Summary_results_Q1_AllOnly_.*\\.xlsx$",
    recursive=TRUE,
    full.names=TRUE,
    ignore.case=TRUE
  )
  x[!grepl("^~\\$",basename(x))]
}

status_rank <- function(x) {
  ifelse(x=="FAIL",3L,ifelse(x=="WARNING",2L,1L))
}

detect_measure <- function(outcome,outcome_type=NA_character_) {
  txt <- paste(outcome,outcome_type,collapse=" | ")
  if (grepl("thickness",txt,ignore.case=TRUE)) return("Thickness")
  if (grepl("curvature|curv",txt,ignore.case=TRUE)) return("Curvature")
  if (grepl("volume|_vol\\b|\\bvol\\b",txt,ignore.case=TRUE)) return("Volume")
  if (grepl("area",txt,ignore.case=TRUE)) return("Area")
  if (grepl("(^|[^A-Za-z])FD([^A-Za-z]|$)|fractal",txt,ignore.case=TRUE)) return("FD")
  NA_character_
}

# =============================================================================
# 1. PROJECT FILES
# =============================================================================

required_project_files <- c(
  "BrainNormR_v5_FINAL_PUBLIC.R",
  "BrainNormR_v5_FINAL_PUBLIC_ALL_ONLY.R",
  "BrainNormR_v5_FINAL_PUBLIC_THREE_GROUP.R",
  "11_export_model_equations.R",
  file.path("Config","BrainNormR_mode_config.R"),
  "00_install_packages.R",
  "00_check_project.R",
  "00_build_grouped_structural_dataset.R",
  "01_run_structural_atlas.R",
  "02_run_cognitive_normative.R",
  "03_biomarker_module.R",
  "04_make_thesis_outputs.R",
  "05_run_single_outcome.R",
  "06_run_grouped_structural_models.R",
  "07_plot_5measures_one_region.R",
  "08_plot_5measures_batch.R",
  "09_make_publication_figure_suite.R",
  "10_final_pipeline_audit.R",
  "15_run_normative_deviation_profiles.R",
  file.path("Config","Deviation_profile_config.R"),
  file.path("Config","DK68_structural_config.csv"),
  file.path("Config","Cognition_Config.csv")
)

ProjectFiles <- data.frame(
  Item=required_project_files,
  Exists=file.exists(required_project_files),
  Status=ifelse(file.exists(required_project_files),"PASS","FAIL"),
  stringsAsFactors=FALSE
)

# =============================================================================
# 1b. DUAL-MODE CONFIGURATION
# =============================================================================

mode_value <- NA_character_
mode_note <- "Mode config could not be read."
mode_file <- file.path("Config", "BrainNormR_mode_config.R")
if (file.exists(mode_file)) {
  ee <- new.env(parent = emptyenv())
  ok_mode <- tryCatch({ sys.source(mode_file, envir = ee); TRUE }, error = function(e) FALSE)
  if (ok_mode && exists("BRAINNORMR_MODE", envir = ee, inherits = FALSE)) {
    mode_value <- toupper(trimws(as.character(ee$BRAINNORMR_MODE)[1]))
    mode_note <- if (mode_value == "ALL_ONLY") {
      "Pooled primary normative model only."
    } else if (mode_value == "THREE_GROUP") {
      "Pooled All plus independent Female and Male normative models."
    } else {
      "Unsupported mode value."
    }
  }
}

ModeAudit <- data.frame(
  Check = c("Valid modelling mode", "All-only engine present", "Three-group engine present"),
  Value = c(mode_value,
            file.exists("BrainNormR_v5_FINAL_PUBLIC_ALL_ONLY.R"),
            file.exists("BrainNormR_v5_FINAL_PUBLIC_THREE_GROUP.R")),
  Status = c(ifelse(mode_value %in% c("ALL_ONLY","THREE_GROUP"), "PASS", "FAIL"),
             ifelse(file.exists("BrainNormR_v5_FINAL_PUBLIC_ALL_ONLY.R"), "PASS", "FAIL"),
             ifelse(file.exists("BrainNormR_v5_FINAL_PUBLIC_THREE_GROUP.R"), "PASS", "FAIL")),
  Note = c(mode_note,
           "Canonical pooled engine.",
           "Optional independent All/Female/Male engine."),
  stringsAsFactors = FALSE
)

# =============================================================================
# 2. DATASET
# =============================================================================

data_files <- list.files(
  "Data",
  pattern="\\.(xlsx|xls)$",
  full.names=TRUE,
  ignore.case=TRUE
)
data_files <- data_files[!grepl("^~\\$",basename(data_files))]

DatasetAudit <- data.frame(
  Check=c("Exactly one modelling Excel file in Data/"),
  Value=length(data_files),
  Status=ifelse(length(data_files)==1L,"PASS","FAIL"),
  Note=ifelse(
    length(data_files)==1L,
    basename(data_files),
    paste0("Found ",length(data_files)," modelling Excel files.")
  ),
  stringsAsFactors=FALSE
)

# =============================================================================
# 3. RESULT WORKBOOK INDEX + SHEET COMPLETENESS
# =============================================================================

files <- find_result_workbooks()

required_sheets <- c(
  "Summary","ModelSelection","NestedCV","NestedSelection","SelectionStability",
  "ValidationSummary","BootstrapSummary","ResidualTests",
  "CentileCurves","DerivativeCurve","VariabilityCurve",
  "NormativeScores","AgeGroupDeviation","Calibration",
  "EngineComparisonAll","EngineComparisonBest","RunMetadata"
)

WorkbookRows <- list()
SummaryRows <- list()
MetadataRows <- list()

for (f in files) {

  sh <- tryCatch(readxl::excel_sheets(f),error=function(e) character())
  missing <- setdiff(required_sheets,sh)

  sm <- read_sheet_safe(f,"Summary")
  md <- read_sheet_safe(f,"RunMetadata")

  if (!is.null(sm) && nrow(sm)) {
    sm$SourceFile <- normalizePath(f,winslash="/",mustWork=FALSE)
    SummaryRows[[length(SummaryRows)+1L]] <- sm
  }

  if (!is.null(md) && nrow(md)) {
    md$SourceFile <- normalizePath(f,winslash="/",mustWork=FALSE)
    MetadataRows[[length(MetadataRows)+1L]] <- md
  }

  WorkbookRows[[length(WorkbookRows)+1L]] <- data.frame(
    SourceFile=normalizePath(f,winslash="/",mustWork=FALSE),
    RequiredSheets=length(required_sheets),
    PresentRequiredSheets=sum(required_sheets %in% sh),
    MissingSheets=if(length(missing))paste(missing,collapse="; ")else "",
    Status=if(length(missing))"FAIL"else"PASS",
    stringsAsFactors=FALSE
  )
}

WorkbookSheets <- if(length(WorkbookRows)) bind_rows(WorkbookRows) else data.frame()
AllSummary <- if(length(SummaryRows)) bind_rows(SummaryRows) else data.frame()
AllMetadata <- if(length(MetadataRows)) bind_rows(MetadataRows) else data.frame()

# =============================================================================
# 4. EXPECTED OUTCOME COVERAGE
# =============================================================================

struct_cfg <- read.csv(
  file.path("Config","DK68_structural_config.csv"),
  stringsAsFactors=FALSE,
  check.names=FALSE
)

cog_cfg <- read.csv(
  file.path("Config","Cognition_Config.csv"),
  stringsAsFactors=FALSE,
  check.names=FALSE
)

actual_outcomes <- if(nrow(AllSummary) && "Outcome" %in% names(AllSummary)) {
  unique(as.character(AllSummary$Outcome))
} else {
  character()
}

expected_struct <- unique(as.character(struct_cfg$Column))
expected_cog <- unique(as.character(cog_cfg$Column))

make_coverage <- function(expected,category) {
  data.frame(
    Category=category,
    Outcome=expected,
    Present=expected %in% actual_outcomes,
    Status=ifelse(expected %in% actual_outcomes,"PASS","FAIL"),
    stringsAsFactors=FALSE
  )
}

OutcomeCoverage <- bind_rows(
  make_coverage(expected_struct,"DK68 structural"),
  make_coverage(expected_cog,"Cognition")
)

group_cfg_file <- file.path("Config","Grouped_structural_config.csv")

if (file.exists(group_cfg_file)) {
  group_cfg <- read.csv(group_cfg_file,stringsAsFactors=FALSE,check.names=FALSE)
  if ("Column" %in% names(group_cfg)) {
    expected_group <- unique(as.character(group_cfg$Column))
    OutcomeCoverage <- bind_rows(
      OutcomeCoverage,
      make_coverage(expected_group,"Grouped structural")
    )
  }
} else {
  OutcomeCoverage <- bind_rows(
    OutcomeCoverage,
    data.frame(
      Category="Grouped structural",
      Outcome="Grouped_structural_config.csv",
      Present=FALSE,
      Status="WARNING",
      stringsAsFactors=FALSE
    )
  )
}

# =============================================================================
# 5. FIVE-MEASURE COMPLETENESS
# =============================================================================

FiveMeasureCompleteness <- data.frame()

if (nrow(AllSummary) &&
    all(c("Outcome","Level","Region") %in% names(AllSummary))) {

  if (!"OutcomeType" %in% names(AllSummary))
    AllSummary$OutcomeType <- NA_character_

  AllSummary$Measure <- mapply(
    detect_measure,
    AllSummary$Outcome,
    AllSummary$OutcomeType,
    USE.NAMES=FALSE
  )

  structural_summary <- AllSummary |>
    filter(Measure %in% c("Volume","Area","Thickness","Curvature","FD"))

  if (nrow(structural_summary)) {
    FiveMeasureCompleteness <- structural_summary |>
      group_by(Level,Region) |>
      summarise(
        MeasuresFound=paste(sort(unique(Measure)),collapse=", "),
        NMeasures=n_distinct(Measure),
        .groups="drop"
      ) |>
      mutate(
        Status=ifelse(NMeasures==5L,"PASS","FAIL")
      )
  }
}

# =============================================================================
# 6. BOOTSTRAP / MILESTONE QC
# =============================================================================

BootstrapQC <- data.frame()

if (nrow(AllSummary)) {

  cols <- intersect(
    c(
      "Outcome","OutcomeType","Level","Region",
      "BootstrapSuccessProp",
      "PeakBootstrapSupport","PeakStable",
      "TroughBootstrapSupport","TroughStable",
      "MaxChangeBootstrapSupport","MaxChangeStable"
    ),
    names(AllSummary)
  )

  BootstrapQC <- AllSummary[,cols,drop=FALSE]

  if ("BootstrapSuccessProp" %in% names(BootstrapQC)) {
    BootstrapQC$BootstrapSuccessProp <- safe_num(BootstrapQC$BootstrapSuccessProp)
    BootstrapQC$BootstrapStatus <- ifelse(
      is.finite(BootstrapQC$BootstrapSuccessProp) &
        BootstrapQC$BootstrapSuccessProp >= .80,
      "PASS",
      "WARNING"
    )
  }
}

# =============================================================================
# 7. RUN-METADATA SETTINGS AUDIT
# =============================================================================

SettingsAudit <- data.frame()

if (nrow(AllMetadata) && all(c("Key","Value") %in% names(AllMetadata))) {

  wanted <- c(
    "PipelineVersion","N_BOOT","BootMinSuccessProp","MilestoneBootMinProp",
    "NestedCVFolds","EngineCVFolds","EngineMetric","GridSize"
  )

  M <- AllMetadata |>
    filter(Key %in% wanted) |>
    select(SourceFile,Key,Value)

  expected_values <- c(
    N_BOOT="1000",
    BootMinSuccessProp="0.8",
    MilestoneBootMinProp="0.8",
    NestedCVFolds="5",
    EngineCVFolds="5",
    EngineMetric="CV_NLL",
    GridSize="400"
  )

  SettingsAudit <- M |>
    rowwise() |>
    mutate(
      Expected=if(Key %in% names(expected_values)) expected_values[[Key]] else NA_character_,
      Status=if(
        is.na(Expected) ||
        as.character(Value)==Expected
      ) "PASS" else "WARNING"
    ) |>
    ungroup()
}

# =============================================================================
# 8. MASTER / BIOMARKER / PUBLICATION OUTPUTS
# =============================================================================

final_outputs <- c(
  file.path(RESULTS_ROOT,"Biomarker","MASTER_NORMATIVE_DEVIATIONS_EXPANDED.xlsx"),
  file.path(RESULTS_ROOT,"Deviation_Profiles","NORMATIVE_DEVIATION_PROFILE_ANALYSIS.xlsx"),
  file.path(RESULTS_ROOT,"Thesis_Outputs","MASTER_RESULTS.xlsx"),
  file.path(RESULTS_ROOT,"Thesis_Outputs","MASTER_CURVES.xlsx"),
  file.path(RESULTS_ROOT,"Thesis_Outputs","MASTER_PARTICIPANT_OUTPUTS.xlsx"),
  file.path(RESULTS_ROOT,"Publication_Figures","FIGURE_INDEX.csv"),
  file.path(RESULTS_ROOT,"Publication_Figures","PUBLICATION_FIGURE_LOG.txt")
)

FinalOutputs <- data.frame(
  Item=final_outputs,
  Exists=file.exists(final_outputs),
  Status=ifelse(file.exists(final_outputs),"PASS","FAIL"),
  stringsAsFactors=FALSE
)

profile_file <- file.path(
  RESULTS_ROOT,
  "Deviation_Profiles",
  "NORMATIVE_DEVIATION_PROFILE_ANALYSIS.xlsx"
)
profile_required_sheets <- c(
  "Analysis_Audit", "Participant_Summary", "Burden_By_Level",
  "Burden_By_Measure", "Burden_LevelMeasure", "Null_Summary",
  "Stability_Summary", "Threshold_Pairwise"
)
profile_present_sheets <- if (file.exists(profile_file)) {
  tryCatch(readxl::excel_sheets(profile_file), error = function(e) character())
} else {
  character()
}
DeviationProfileAudit <- data.frame(
  Item = profile_required_sheets,
  Present = profile_required_sheets %in% profile_present_sheets,
  Status = ifelse(profile_required_sheets %in% profile_present_sheets, "PASS", "FAIL"),
  stringsAsFactors = FALSE
)

# Main figures M1-M6 are required for the frozen manuscript figure suite.
main_fig_codes <- paste0("M",1:6)

FigureAudit <- data.frame(
  FigureCode=main_fig_codes,
  PNG=file.path(
    RESULTS_ROOT,"Publication_Figures","Main",
    c(
      "M1_Multiscale_Standardized_Trajectories.png",
      "M2_Multiscale_Standardized_Age_Gradients.png",
      "M3_Milestone_Timing.png",
      "M4_DK68_Map_MaxChange_Age.png",
      "M5_DK68_Map_MaxChange_Magnitude.png",
      "M6_Conditional_Variability_Heatmap.png"
    )
  ),
  stringsAsFactors=FALSE
)

FigureAudit$Exists <- file.exists(FigureAudit$PNG)
FigureAudit$Status <- ifelse(FigureAudit$Exists,"PASS","FAIL")

figure_index_file <- file.path(
  RESULTS_ROOT,"Publication_Figures","FIGURE_INDEX.csv"
)

SupplementAudit <- data.frame()

if (file.exists(figure_index_file)) {
  fi <- tryCatch(read.csv(figure_index_file,stringsAsFactors=FALSE),error=function(e)NULL)
  if (!is.null(fi) && nrow(fi)) {
    SupplementAudit <- fi[grepl("^S",fi$FigureCode),,drop=FALSE]
  }
}

# =============================================================================
# 8b. MODEL-EQUATION EXPORT (optional reproducibility output)
# =============================================================================

equation_xlsx <- file.path(RESULTS_ROOT, "Model_Equations", "GAMLSS_Model_Equations.xlsx")
saved_model_files <- list.files(
  RESULTS_ROOT,
  pattern = "_final_GAMLSS_model\\.rds$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

EquationAudit <- data.frame(
  Check = c("Saved final GAMLSS model objects", "Equation export workbook"),
  Value = c(as.character(length(saved_model_files)), as.character(file.exists(equation_xlsx))),
  Status = c(
    ifelse(length(saved_model_files) > 0, "PASS", "WARNING"),
    ifelse(file.exists(equation_xlsx), "PASS", "WARNING")
  ),
  Note = c(
    "Final-model RDS objects enable exact formula/coefficient documentation.",
    "Run 11_export_model_equations.R after modelling when mathematical-equation export is desired."
  ),
  stringsAsFactors = FALSE
)

# =============================================================================
# 9. OVERALL STATUS
# =============================================================================

status_vectors <- list(
  ProjectFiles$Status,
  ModeAudit$Status,
  DatasetAudit$Status,
  if(nrow(WorkbookSheets)) WorkbookSheets$Status else "FAIL",
  if(nrow(OutcomeCoverage)) OutcomeCoverage$Status else "FAIL",
  if(nrow(FiveMeasureCompleteness)) FiveMeasureCompleteness$Status else "WARNING",
  FinalOutputs$Status,
  DeviationProfileAudit$Status,
  FigureAudit$Status
)

all_status <- unlist(status_vectors,use.names=FALSE)

overall <- if(any(all_status=="FAIL")) {
  "FAIL"
} else if(any(all_status=="WARNING")) {
  "WARNING"
} else {
  "PASS"
}

OverallStatus <- data.frame(
  OverallStatus=overall,
  ResultWorkbooksFound=length(files),
  StructuralExpected=length(expected_struct),
  StructuralPresent=sum(expected_struct %in% actual_outcomes),
  CognitionExpected=length(expected_cog),
  CognitionPresent=sum(expected_cog %in% actual_outcomes),
  MainFiguresPresent=sum(FigureAudit$Exists),
  MainFiguresRequired=nrow(FigureAudit),
  stringsAsFactors=FALSE
)

# =============================================================================
# 10. WRITE AUDIT WORKBOOK
# =============================================================================

sheets <- list(
  OverallStatus=OverallStatus,
  ProjectFiles=ProjectFiles, ModeAudit=ModeAudit,
  EquationAudit=EquationAudit,
  DatasetAudit=DatasetAudit,
  OutcomeCoverage=OutcomeCoverage,
  FiveMeasureCompleteness=FiveMeasureCompleteness,
  WorkbookSheets=WorkbookSheets,
  BootstrapQC=BootstrapQC,
  SettingsAudit=SettingsAudit,
  FinalOutputs=FinalOutputs,
  DeviationProfileAudit=DeviationProfileAudit,
  MainFigureAudit=FigureAudit,
  SupplementFigureAudit=SupplementAudit
)

sheets <- sheets[
  vapply(sheets,function(x)is.data.frame(x)&&ncol(x)>0,logical(1))
]

writexl::write_xlsx(sheets,AUDIT_XLSX)

txt <- c(
  "BrainNormR FINAL PIPELINE AUDIT",
  "===============================",
  paste0("Overall status: ",overall),
  paste0("Result workbooks found: ",length(files)),
  paste0("DK68 structural outcomes present: ",
         sum(expected_struct %in% actual_outcomes),"/",length(expected_struct)),
  paste0("Cognitive outcomes present: ",
         sum(expected_cog %in% actual_outcomes),"/",length(expected_cog)),
  paste0("Main publication figures present: ",
         sum(FigureAudit$Exists),"/",nrow(FigureAudit)),
  "",
  "Interpretation:",
  "PASS = all mandatory checks passed.",
  "WARNING = pipeline is usable but one or more non-mandatory or QC items need review.",
  "FAIL = at least one mandatory project/output/completeness check failed.",
  "",
  paste0("Detailed workbook: ",AUDIT_XLSX)
)

writeLines(txt,AUDIT_TXT)

cat("\n============================================================\n")
cat("FINAL BrainNormR audit completed.\n")
cat("Overall status: ",overall,"\n",sep="")
cat("Audit workbook: ",AUDIT_XLSX,"\n",sep="")
cat("Audit text: ",AUDIT_TXT,"\n",sep="")
cat("============================================================\n")
