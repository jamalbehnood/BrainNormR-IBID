# v5.6.0 — Individualized normative-deviation profiles

- Added `15_run_normative_deviation_profiles.R` for the standalone Paper 3
  analysis without refitting normative models.
- Added structural-only primary burden and a separate cognition-inclusive
  sensitivity burden.
- Added equal-weighted multiscale and level-by-measure burden summaries.
- Added outcome-wise and block-preserving permutation null models.
- Added threshold sensitivity, top-decile overlap, outcome-bootstrap and
  stratified split-half profile stability.
- Added scale/phenotype concordance, directionality and demographic calibration
  audits.
- Added indexed publication figures and machine-readable result tables.
- Extended pre-flight and final audits to require the new module and outputs.

# v5.5.0 — Reproducible parallel compute + runtime profiling

- Added configurable `SEQUENTIAL` / `PARALLEL` compute backend.
- Parallel backend uses `future::multisession` and caps workers at available cores.
- Parallelized independent bootstrap iterations and nested-CV folds.
- Added deterministic per-task seeds for scheduling-independent resampling.
- Avoided nested parallelism; candidate fits and file writing remain sequential.
- Added runtime profiling for GAMLSS search, bootstrap, nested validation,
  engine comparison, post-processing, and total elapsed time.
- Added runtime/backend/worker metadata to result Excel outputs.
- Three-group mode now exports a combined `Runtime` sheet for All/Female/Male.
- Preserved All-only and Three-group analysis-mode dispatcher behavior.


# Changelog

## 5.4.0 FINAL FROZEN — Dual Mode
- Added a mode dispatcher controlled by `Config/BrainNormR_mode_config.R`.
- Preserved the original pooled engine as `BrainNormR_v5_FINAL_PUBLIC_ALL_ONLY.R`.
- Added optional `BrainNormR_v5_FINAL_PUBLIC_THREE_GROUP.R` for independent All,
  Female and Male normative modelling when sex-specific sample adequacy passes.
- Kept pooled Age-by-Sex interaction as the primary sex-difference inference.
- Added final GAMLSS object saving for reproducible model-equation extraction.
- Added `11_export_model_equations.R`, recursive across all Results subfolders.
- Updated pre-flight checks, documentation and static release audit for dual mode.
- No change to the primary ALL_ONLY statistical search/validation specification.

## 5.3.0-final-frozen — 2026-08-18

- Froze the final BrainNormR analysis architecture.
- Replaced the old biomarker aggregator with the expanded CF_Norm_Z-first module.
- Expanded `04_make_thesis_outputs.R` to aggregate all unique active core sheets.
- Added metadata enrichment to non-Summary master sheets.
- Added full Peak/Trough/Maximal-change compact milestone table.
- Added calibration-error summary.
- Added `MASTER_PARTICIPANT_OUTPUTS.xlsx`.
- Harmonized engine-CV metadata/config with the five-fold nested design.
- Added `09_make_publication_figure_suite.R`.
- Fixed FreeSurfer DK68 -> ggseg region/hemisphere mapping for cortical maps.
- Added engine-comparison, centile-calibration, selection-stability, and
  age-group normative-deviation supplementary figures.
- Added `10_final_pipeline_audit.R`.
- Added `docs/PUBLICATION_FIGURES.md`.
- Updated package installation, pre-flight checks, README, run instructions,
  outputs documentation, and flowchart.

## 5.2.0-github-final — 2026-08-18

- Added WholeBrain structural aggregation across all 68 DK cortical ROIs.
- Added automatic WholeBrain rows to the generated grouped structural config.
- Retained six-lobe, hemisphere, and optional validated Yeo7 workflows.
- Updated the source-safe core publication figure from four to five panels.
- Added a Conditional Variability panel using distribution-derived Conditional SD.
- Retained the family-specific GAMLSS sigma parameter in exported variability data.
- Kept nu and tau intercept-only when required by the selected family.
- Added one-region five-measure standardized trajectory/derivative plotting.
- Added peak/trough markers and editable Young/Middle-aged/Older age bands.
- Added recursive batch five-measure plotting across valid ROI/group units.
- Added batch success, skipped-group, unindexed-file, and file-index audit logs.
- Updated README, run instructions, grouped-analysis documentation, methods notes, and outputs documentation.

## 5.1.0-github-final — 2026-08-17

- Consolidated the final GitHub-ready BrainNormR package.
- Kept one source-safe BrainNormR v5 core.
- Included corrected structural batch wrapper.
- Included cognitive batch wrapper.
- Included single-outcome runner.
- Added six-lobe and hemisphere grouped structural aggregation.
- Added grouped structural model runner.
- Added optional Yeo7 overlap-weight workflow and template.
- Added package installer and pre-flight project checker.
- Added English-only final instructions.
- Added Git protections for raw data, derived participant-level data, and generated results.
- Preserved PIT-based normative-score export and milestone bootstrap-support logic.
