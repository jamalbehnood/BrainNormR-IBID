# Methods Notes

## Predictor and adjustment policy

Age is the primary normative predictor.

- FD / Thickness / Curvature: Age + Sex adjustment.
- Volume / Area: Age + Sex + eTIV adjustment.
- Cognition: Age + Sex + Education adjustment.

## GAMLSS search

The prespecified search contains:

- 12 candidate families;
- 44 mu forms: 8 FP1 + 36 FP2;
- 6 sigma forms: Constant + 5 FP1.

The search is staged:

1. 44 mu models with constant sigma are tested within each family.
2. Up to the two best mu forms are retained.
3. Five non-constant sigma forms are tested for each retained mu.

Maximum:

- 54 GAMLSS fits per family;
- 648 GAMLSS fits per outcome in full-sample GAMLSS selection;
- 5 GAM comparator fits;
- 47 FP/POLY comparator fits;
- up to 700 full-sample candidate fits.

Five-fold nested selection gives an approximate upper total of 4,200
selection-related fit operations. Adding 1,000 conditional bootstrap refits
and two Age-by-Sex sensitivity fits gives an approximate theoretical maximum
of 5,202 model-fit operations per outcome.

These are fitting operations, not 5,202 independent candidate specifications.

## Distributional parameters

The current core allows age-varying mu and a selected constant or age-varying
sigma form. When required by the selected family, nu and tau are intercept-only.

The raw GAMLSS sigma parameter is family-specific and should not universally
be called the standard deviation. BrainNormR exports both the family-specific
scale parameter and distribution-derived Conditional SD/IQR.

## Cross-sectional interpretation

Age effects describe cross-sectional age associations rather than
within-person longitudinal rates of change.


## v5.4 dual-mode note
The default `ALL_ONLY` engine remains the primary pooled normative analysis.
`THREE_GROUP` is an optional secondary mode that reruns the full normative
workflow independently within Female and Male subsets after eligibility checks,
while preserving the pooled All result and pooled Age-by-Sex interaction.
Final selected GAMLSS objects are saved for exact equation extraction with
`11_export_model_equations.R`.

## Parallel computation and reproducibility

Parallel execution changes computation scheduling, not the statistical model.
BrainNormR parallelizes independent bootstrap iterations and nested
cross-validation folds. Candidate model fitting within each task remains
sequential. With reproducible-parallel mode enabled, each resampling/fold task
receives a deterministic seed derived from the master seed, task label and task
index. This makes results independent of worker scheduling. Small
floating-point differences may still occur across BLAS/compiler/platform
implementations, so exact bitwise identity across different computers is not
guaranteed.
