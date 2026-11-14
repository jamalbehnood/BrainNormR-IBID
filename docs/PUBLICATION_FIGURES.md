# Publication Figure Suite

Run `09_make_publication_figure_suite.R` only after `04_make_thesis_outputs.R`.

## Main figures

- M1 Multiscale standardized normative trajectories
- M2 Multiscale standardized cross-sectional age gradients
- M3 Timing of Peak, Trough, and Maximal age-related change
- M4 DK68 map: age at maximal age-related change
- M5 DK68 map: magnitude/direction of maximal standardized age-related change
- M6 Conditional-variability heatmap

## Supplementary figures

- S1 Hemisphere standardized trajectories
- S2 Hemisphere standardized age gradients
- S3 Yeo7 standardized trajectories
- S4 Yeo7 standardized age gradients
- S5 Conditional-SD trajectories
- S6 ROI milestone-age heatmap
- S7 Validation/calibration overview
- S8 Selected GAMLSS family distribution
- S9 Age-by-Sex sensitivity summary
- S10 Bootstrap milestone support
- S11 Individual normative-deviation burden
- S12 Deviation burden by analysis level
- S13 GAMLSS vs GAM vs FP/POLY nested-CV comparison
- S14 Cross-fitted centile calibration
- S15 Model-selection stability
- S16 Cross-fitted normative deviation across age groups

Figures are saved as PNG (600 dpi) and PDF where generated. Missing optional
inputs cause the corresponding supplementary figure to be skipped rather than
forcing an artificial result.

The script writes `FIGURE_INDEX.csv` and `PUBLICATION_FIGURE_LOG.txt` so every
created/skipped figure is auditable.
