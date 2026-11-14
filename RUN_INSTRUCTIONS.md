# BrainNormR v5.6.0 — Run Instructions

## 1. Open the project
Open `BrainNormR_Project.Rproj` in RStudio.


## 1A. Select modelling mode
Before running analyses, edit:
`Config/BrainNormR_mode_config.R`

For the standard pooled analysis:
```r
BRAINNORMR_MODE <- "ALL_ONLY"
```

For pooled All + independent Female + Male models:
```r
BRAINNORMR_MODE <- "THREE_GROUP"
```

Do not edit the two engine files to switch modes. Existing wrappers continue to
source `BrainNormR_v5_FINAL_PUBLIC.R`, which loads the selected engine.

## 2. Install packages
Run once:
```r
source("00_install_packages.R")
```

## 3. Pre-flight check
```r
source("00_check_project.R")
```

## 4. Put the modelling dataset in `Data/`
Keep exactly one `.xlsx`/`.xls` modelling file in `Data/`.

Required base columns include:
- `Age`
- `sex`
- `edu` for cognition
- `eTIV` for Volume/Area

Do not rename configured structural/cognitive columns.

## 5. Run DK68 structural outcomes
Edit and run:
```r
source("01_run_structural_atlas.R")
```
Repeat for:
`Volume`, `Area`, `Thickness`, `Curvature`, `FD`.

Use small row chunks if desired. Completed outputs should remain in `Results/`.

## 6. Run cognition
```r
source("02_run_cognitive_normative.R")
```

## 7. Build grouped structural data
```r
source("00_build_grouped_structural_dataset.R")
```
This creates WholeBrain, Lobe, Hemisphere, and optional Yeo7 variables in
`DerivedData/` and creates `Config/Grouped_structural_config.csv`.

## 8. Run grouped models
```r
source("06_run_grouped_structural_models.R")
```
Choose:
```r
LEVEL_FILTER <- "WholeBrain"
# or "Lobe", "Hemisphere", "Yeo7"
```
and one:
```r
MEASURE_FILTER <- "Volume"
# "Area", "Thickness", "Curvature", "FD"
```
Repeat until all requested grouped outcomes are complete.

## 9. Optional one-outcome rerun
Use only when needed:
```r
source("05_run_single_outcome.R")
```

## 10. Expanded biomarker/deviation outputs
After main modelling is complete:
```r
source("03_biomarker_module.R")
```
Expected:
`Results/Biomarker/MASTER_NORMATIVE_DEVIATIONS_EXPANDED.xlsx`

## 11. Build all master tables
```r
source("04_make_thesis_outputs.R")
```
Expected:
- `Results/Thesis_Outputs/MASTER_RESULTS.xlsx`
- `Results/Thesis_Outputs/MASTER_CURVES.xlsx`
- `Results/Thesis_Outputs/MASTER_PARTICIPANT_OUTPUTS.xlsx`

## 11A. Individualized normative-deviation profiles

After `03_biomarker_module.R`, run:

```r
source("15_run_normative_deviation_profiles.R")
```

This creates:

- `Results/Deviation_Profiles/NORMATIVE_DEVIATION_PROFILE_ANALYSIS.xlsx`
- `Results/Deviation_Profiles/Figures/`
- `Results/Deviation_Profiles/Tables/`

The default analysis is structural-only, uses `CF_Norm_Z`, gives equal weight
to anatomical level × morphometric-measure blocks, and runs reproducible null,
threshold-sensitivity and profile-stability analyses. For the locked manuscript
analysis, consider increasing permutation repeats from 2000 to 5000 in
`Config/Deviation_profile_config.R`.

## 12. Per-region five-measure figures
For one selected region:
```r
source("07_plot_5measures_one_region.R")
```
For all complete regions/groups:
```r
source("08_plot_5measures_batch.R")
```

## 13. Manuscript and supplement figure suite
```r
source("09_make_publication_figure_suite.R")
```
Expected folders:
- `Results/Publication_Figures/Main/`
- `Results/Publication_Figures/Supplement/`
- `Results/Publication_Figures/Tables/`

Always inspect:
`Results/Publication_Figures/FIGURE_INDEX.csv`

## 14. Export final mathematical model equations
After the requested modelling runs are complete:
```r
source("11_export_model_equations.R")
```
Expected:
- `Results/Model_Equations/GAMLSS_Model_Equations.xlsx`
- `Results/Model_Equations/GAMLSS_Model_Equations.csv`

This step does not refit models. It reads the saved final GAMLSS objects.

## 15. FINAL audit — run last
```r
source("10_final_pipeline_audit.R")
```

Read:
`Results/Final_Audit/FINAL_PIPELINE_AUDIT.xlsx`

Do not call the pipeline complete until mandatory items in the audit are PASS.

## Important
- Do not delete completed result workbooks after an interruption.
- Do not replace the original file in `Data/` with a derived workbook.
- Yeo7 requires a validated overlap-weight file.
- WholeBrain here refers to the global cortical aggregate from the 68 DK cortical ROIs.
- BrainNormR does not itself estimate brain age.
- Do not interpret deviation burden as a clinical diagnostic score.

## Compute mode

Before a production run, edit `Config/BrainNormR_mode_config.R`.

Recommended workstation setting:

```r
BRAINNORMR_COMPUTE_MODE <- "PARALLEL"
BRAINNORMR_N_WORKERS <- 6L
BRAINNORMR_REPRODUCIBLE_PARALLEL <- TRUE
```

For troubleshooting or a low-memory computer:

```r
BRAINNORMR_COMPUTE_MODE <- "SEQUENTIAL"
BRAINNORMR_N_WORKERS <- 1L
```

Parallelization is limited to independent bootstrap iterations and nested-CV
folds. Excel/PDF writing and higher-level outcome execution remain sequential.
Runtime and worker information are exported with the analysis results.
