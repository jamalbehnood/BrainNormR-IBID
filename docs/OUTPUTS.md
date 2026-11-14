# BrainNormR Outputs

Typical outcome workbooks include:

- Summary
- ModelSelection
- NestedCV
- NestedSelection
- SelectionStability
- ValidationSummary
- BootstrapSummary
- ResidualTests
- CentileCurves
- CurveData
- DerivativeCurve
- VariabilityCurve
- Milestones
- NormativeScores
- AgeGroupDeviation
- Calibration
- EngineComparisonAll
- EngineComparisonBest
- RunMetadata
- SexInteraction, when available

`VariabilityCurve` includes:

- Age
- Conditional_SD
- P25
- P75
- IQR
- FamilyScaleParameter

The current core generates a five-panel publication figure:
raw data, adjusted normative trajectory, conditional variability,
cross-sectional age gradient, and cross-fitted normative deviation.

The post-analysis scripts `07_plot_5measures_one_region.R` and
`08_plot_5measures_batch.R` combine completed structural workbooks without
refitting any BrainNormR models.


## Final frozen post-processing

`03_biomarker_module.R` produces:
`Results/Biomarker/MASTER_NORMATIVE_DEVIATIONS_EXPANDED.xlsx`.

Its primary `IndividualBurden_All` sheet is structural-only.
`IndividualBurden_AllPlusCog` is the separate structural-plus-cognition
sensitivity analysis.

`15_run_normative_deviation_profiles.R` produces:
- `Results/Deviation_Profiles/NORMATIVE_DEVIATION_PROFILE_ANALYSIS.xlsx`
- participant, null, threshold-sensitivity and stability CSV tables
- indexed Paper 3 burden, profile, permutation, threshold and stability figures
  in PNG and PDF
- a reproducibility `sessionInfo` text file

The profile workbook contains analysis and coverage audits, participant burden,
level and phenotype burden, equal-weighted profile matrices, two permutation
null models, threshold sensitivity, stability summaries, concordance matrices,
demographic calibration checks and interpretation notes.

`04_make_thesis_outputs.R` produces:
- `MASTER_RESULTS.xlsx`
- `MASTER_CURVES.xlsx`
- `MASTER_PARTICIPANT_OUTPUTS.xlsx`

`09_make_publication_figure_suite.R` produces the manuscript/supplement figure
suite and `FIGURE_INDEX.csv`.

`10_final_pipeline_audit.R` produces the final PASS/WARNING/FAIL audit workbook.

## Runtime metadata

All-only result workbooks record compute mode, workers, start/end time, stage
durations, and total elapsed runtime in `RunMetadata`. Three-group workbooks
also contain a `Runtime` sheet with separate All, Female, and Male rows.
