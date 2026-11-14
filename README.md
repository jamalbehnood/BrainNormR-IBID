# BrainNormR v5.6.0 — Individualized Deviation Profiles

BrainNormR is an R-based distributional normative-modelling pipeline for
cross-sectional adult structural neuroimaging and cognitive outcomes.

This frozen release contains a mode dispatcher plus two frozen statistical engines and wrappers for:
ROI-level structural analysis, cognitive normative modelling, grouped cortical
analysis, individual normative-deviation and profile inference, thesis/master aggregation,
per-region five-measure plots, publication figure generation, and a final
completeness audit.


## Modelling mode

The canonical entry point is still `BrainNormR_v5_FINAL_PUBLIC.R`, but it is now a
small dispatcher controlled by `Config/BrainNormR_mode_config.R`.

Default/recommended mode:
```r
BRAINNORMR_MODE <- "ALL_ONLY"
```
This loads `BrainNormR_v5_FINAL_PUBLIC_ALL_ONLY.R` and reproduces the pooled
analysis: nonlinear Age with Sex adjustment (plus eTIV for Volume/Area and
Education for cognition).

Optional mode for sufficiently large sex-specific samples:
```r
BRAINNORMR_MODE <- "THREE_GROUP"
```
This loads `BrainNormR_v5_FINAL_PUBLIC_THREE_GROUP.R`. It first runs and saves
the unchanged pooled All analysis, then independently fits Female and Male
normative models. Sex is omitted from the sex-specific formulas because it is
constant within each subset; eTIV and Education adjustments remain outcome
specific. The pooled Age-by-Sex interaction remains the primary inferential
sex-difference test. Stratified curves are secondary/reference analyses.

The three-group engine performs an eligibility precheck before expensive
sex-specific fitting. Thresholds are configurable in the same mode-config file.
If the precheck fails, the pooled All analysis remains saved and the stratified
analysis is skipped rather than causing the completed pooled run to be lost.

## Main execution order

```text
00_install_packages.R
00_check_project.R
01_run_structural_atlas.R
02_run_cognitive_normative.R
00_build_grouped_structural_dataset.R
06_run_grouped_structural_models.R
03_biomarker_module.R
15_run_normative_deviation_profiles.R # Paper 3 burden, null and stability analyses
04_make_thesis_outputs.R
07_plot_5measures_one_region.R        # optional per-region/manual figure
08_plot_5measures_batch.R             # batch per-region figures
09_make_publication_figure_suite.R    # manuscript + supplement figures
11_export_model_equations.R           # optional final-model mathematical equations
10_final_pipeline_audit.R             # run LAST
```

`05_run_single_outcome.R` is available for deliberate one-outcome testing or
reruns and is not part of the normal batch sequence.

## Analysis levels

- DK68 ROI
- WholeBrain cortical aggregate
- six lobes
- left/right hemisphere
- optional Yeo7 network aggregation using validated overlap weights
- cognition

## Structural measures

- Volume
- Area
- Thickness
- Curvature
- FD

WholeBrain/Lobe/Hemisphere aggregation uses:
- Volume: sum
- Area: sum
- Thickness: surface-area-weighted mean
- Curvature: surface-area-weighted mean
- FD: surface-area-weighted mean of ROI-level FD

Grouped FD is a regional composite and is not geometric FD recomputed from a
single merged surface.

## Core modelling

Age is the primary normative predictor.

- FD / Thickness / Curvature: nonlinear Age + Sex
- Volume / Area: nonlinear Age + Sex + eTIV
- Cognition: nonlinear Age + Sex + Education

The primary distributional engine is GAMLSS. The staged GAMLSS search uses:
- 12 candidate families
- 44 mu forms
- 6 sigma forms
- up to 648 GAMLSS fits per outcome

Comparators:
- 5 GAM candidates
- 47 FP/POLY candidates

Validation:
- five-fold nested model selection / cross-fitting
- CV-NLL primary engine metric
- RMSE, MAE, R2, coverage
- 1000 conditional bootstrap refits
- Age-by-Sex sensitivity
- PIT / centile calibration
- residual diagnostics
- model-selection stability
- milestone bootstrap support

The approximate theoretical upper burden is about 5,202 fitting operations per
outcome. This is not 5,202 distinct model specifications.

## Main outcome figure

The shared core generates five panels:
1. raw data
2. pooled adjusted normative trajectory
3. conditional variability using distribution-derived Conditional SD
4. cross-sectional age gradient
5. cross-fitted normative deviation

The raw family-specific GAMLSS sigma remains exported separately. It should not
universally be interpreted as an SD. Nu and tau remain intercept-only when used.

## Expanded biomarker module

`03_biomarker_module.R` uses `CF_Norm_Z` as the primary individual deviation
score when available, with `Norm_Z` only as fallback. Its primary overall
burden is structural-only; a separate sensitivity sheet retains the combined
structural-plus-cognitive burden. It produces:
- all normative scores
- outcome summaries
- level summaries
- individual deviation burden
- level-specific burden
- extreme-deviation table
- audit and interpretation notes

It does not estimate brain age.

## Individualized deviation profiles

`15_run_normative_deviation_profiles.R` is the dedicated Paper 3 analysis. It
does not refit normative models. It reads `NormativeScores_All` and creates:

- outcome-weighted structural burden
- burden by anatomical level and morphometric measure
- equal-weighted level and level-by-measure multiscale burden
- positive and negative deviation summaries
- sensitivity analyses at |z| thresholds 1.96, 2.00 and 2.58
- outcome-wise label-permutation tests of between-person heterogeneity
- level-by-measure block permutations that preserve each block distribution
- outcome-bootstrap and stratified split-half profile stability
- level and phenotype concordance matrices
- age and sex calibration audits
- manuscript-ready PNG/PDF figures and an indexed Excel workbook

The primary lobe representation is bilateral/combined to avoid treating left,
right and bilateral versions of the same lobe as independent observations.
Configuration is in `Config/Deviation_profile_config.R`.

## Master outputs

`04_make_thesis_outputs.R` creates:
- `MASTER_RESULTS.xlsx`
- `MASTER_CURVES.xlsx`
- `MASTER_PARTICIPANT_OUTPUTS.xlsx`

It aggregates all unique active core outputs and enriches non-Summary sheets
with outcome metadata. It also computes BH-FDR for Age-by-Sex sensitivity
within OutcomeType × Level families and produces compact milestone,
validation, and calibration summaries.

## Publication figures

`09_make_publication_figure_suite.R` creates six main figures and up to sixteen
supplementary figures, including:
- multiscale trajectories
- standardized age gradients
- Peak/Trough/Maximal-change timing
- DK68 maps for timing and magnitude
- conditional-variability heatmap
- hemisphere and Yeo7 plots
- ROI milestone heatmap
- validation/calibration overview
- family distribution
- Age×Sex sensitivity
- bootstrap milestone support
- deviation burden
- GAMLSS vs GAM vs FP/POLY comparison
- centile calibration
- model-selection stability
- age-group normative-deviation calibration

The DK68 map code explicitly converts FreeSurfer `lh_*/rh_*` labels to the
region + hemisphere naming expected by ggseg.


## Mathematical equations of final models

The core now saves each selected final GAMLSS object as an `.rds` file inside
the relevant Results subtree. Run:
```r
source("11_export_model_equations.R")
```
to recursively collect all saved final models and create:
- `Results/Model_Equations/GAMLSS_Model_Equations.xlsx`
- `Results/Model_Equations/GAMLSS_Model_Equations.csv`

The export contains family, link, formula, coefficients and linear-predictor
equations for mu, sigma, nu and tau when supported. In `THREE_GROUP` mode the
All, Female and Male equations are retained as separate fitted models.

## Final audit

Run `10_final_pipeline_audit.R` last. It checks:
- required project files
- modelling dataset count
- expected DK68/cognition/grouped outcome coverage
- five-measure completeness
- required workbook sheets
- bootstrap QC
- frozen model settings
- master/biomarker outputs
- individualized-deviation profile outputs
- main publication figures

It creates:
- `Results/Final_Audit/FINAL_PIPELINE_AUDIT.xlsx`
- `Results/Final_Audit/FINAL_PIPELINE_AUDIT.txt`

Interpretation:
- PASS = mandatory checks passed
- WARNING = review a non-mandatory/QC item
- FAIL = at least one mandatory completeness check failed

## Interpretation limits

- Cross-sectional age effects are not longitudinal within-person change.
- PIT normative scores are not simple raw z scores.
- Bootstrap intervals are conditional on the selected model specification.
- BrainNormR outcome-given-age curves do not directly estimate brain age.
- Research deviations / milestones are not clinically validated biomarkers
  without external validation.

## v5.5 compute backend and runtime profiling

BrainNormR now supports `SEQUENTIAL` and `PARALLEL` compute modes through
`Config/BrainNormR_mode_config.R`. Parallel mode uses a Windows-safe
`future::multisession` backend for independent bootstrap iterations and
nested-CV folds only; model fitting inside each task stays sequential to avoid
nested parallelism and uncontrolled RAM use.

With `BRAINNORMR_REPRODUCIBLE_PARALLEL <- TRUE`, deterministic per-task seeds
are used so task scheduling does not change resampling streams. Runtime metadata
is written into the standard `RunMetadata` sheet for All-only analyses and a
`Runtime` sheet in three-group workbooks, including stage and total elapsed time,
backend, and worker count.

Default release settings:
- `BRAINNORMR_COMPUTE_MODE <- "PARALLEL"`
- `BRAINNORMR_N_WORKERS <- 6L`
- `BRAINNORMR_REPRODUCIBLE_PARALLEL <- TRUE`

For low-memory systems, switch to `SEQUENTIAL` or reduce `N_WORKERS`.
