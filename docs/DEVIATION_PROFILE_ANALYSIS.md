# Individualized Normative-Deviation Profile Analysis

## Purpose

`15_run_normative_deviation_profiles.R` evaluates whether cross-fitted
structural normative deviations cluster within individuals and whether the
resulting profiles are robust across anatomical scales, morphometric measures,
deviation thresholds and outcome resampling.

The module is post-processing only. It does not select, fit or refit any
normative model.

## Primary input

The input is:

`Results/Biomarker/MASTER_NORMATIVE_DEVIATIONS_EXPANDED.xlsx`

The required sheet is `NormativeScores_All`. The prespecified primary score is
`CF_Norm_Z`, the cross-fitted normative deviation generated without using the
participant's outcome value to fit that participant's prediction model.

## Primary analysis population

- Structural morphology only
- WholeBrain, Hemisphere, bilateral/combined Lobe and DK68 ROI levels
- Volume, Area, Thickness, Curvature and FD
- Yeo7 optional and disabled by default
- Cognition excluded from the primary burden

Bilateral lobe aggregates are used in the primary profile to avoid counting
left, right and combined lobe summaries as independent observations.

## Burden definitions

For participant *i*, outcome *j* and threshold *t*:

`E_ij(t) = I(|z_ij| >= t)`

The outcome-weighted burden is the proportion of available outcomes for which
`E_ij(t) = 1`. Because ROI-level analyses contain more outcomes than coarse
anatomical levels, this value is descriptive rather than the primary
multiscale summary.

The primary balanced burden is calculated in two stages:

1. Calculate the extreme-deviation proportion separately within every
   anatomical-level × morphometric-measure block.
2. Average the available block proportions with equal block weight.

The workbook also reports an equal-level burden, burden by scale, burden by
measure, and positive and negative burden separately.

## Threshold sensitivity

The complete analysis is repeated at absolute z thresholds 1.96, 2.00 and
2.58. Pairwise sensitivity outputs include:

- Spearman rank correlation
- mean absolute burden difference
- top-decile Jaccard overlap

The 2.00 threshold is primary. The other thresholds are sensitivity analyses,
not alternative significance searches.

## Null model 1: outcome-wise label permutation

Participant labels are independently permuted within every outcome. This
preserves:

- the marginal deviation distribution of each outcome
- the number of valid observations per outcome
- the overall extreme-deviation rate

It removes participant-level co-occurrence across outcomes. Observed
between-participant burden dispersion is compared with the permutation
distribution using SD, variance, IQR, upper quantiles, maximum and Gini
concentration. Empirical upper-tail P values use the plus-one correction;
BH-FDR-adjusted values and standardized observed-minus-null differences are
reported across the statistics within each null model.

This null addresses whether extreme outcomes cluster within particular
participants more than expected under outcome-wise independence.

## Null model 2: level-by-measure block permutation

Participant rows are permuted independently for each anatomical-level ×
morphometric-measure burden block. This preserves the complete marginal burden
distribution within each block while removing participant alignment across
blocks.

The test evaluates whether equal-weighted multiscale burden and cross-block
concordance are greater than expected when block-specific profiles are not
aligned to the same individuals. The same plus-one empirical P-value and
within-model BH-FDR reporting rules are used.

## Profile stability

Two complementary resampling procedures are used.

### Outcome bootstrap within blocks

Outcomes are sampled with replacement inside each eligible level × measure
block. Each resample produces participant block profiles and balanced burden.
Stability metrics include:

- participant-rank Spearman correlation
- top-decile Jaccard overlap
- mean participant-profile cosine similarity
- mean absolute profile difference

### Stratified split-half

Outcomes are randomly divided into two halves inside each eligible block.
Agreement between halves is summarized using:

- raw split-half Spearman correlation
- Spearman-Brown corrected reliability
- top-decile Jaccard overlap
- profile cosine similarity
- absolute profile difference

Blocks with fewer than the configured minimum number of outcomes are omitted
from stability resampling but remain available in descriptive burden tables.

## Calibration and concordance audits

The module additionally reports:

- age association with balanced burden
- sex-group burden comparison
- positive-versus-negative directional burden
- Spearman concordance across anatomical levels
- Spearman concordance across morphometric measures
- participant and block coverage

Age and sex analyses are calibration checks because the normative models
already include the corresponding covariate adjustments. They are not primary
biological association tests.

## Output

The main workbook is:

`Results/Deviation_Profiles/NORMATIVE_DEVIATION_PROFILE_ANALYSIS.xlsx`

Key sheets are:

- `Analysis_Audit`
- `Coverage_Audit`
- `Participant_Summary`
- `Burden_By_Level`
- `Burden_By_Measure`
- `Burden_LevelMeasure`
- `Profile_Matrix`
- `Threshold_Pairwise`
- `Threshold_TopDecile`
- `Null_Summary`
- `Stability_Summary`
- `Level_Correlations`
- `Measure_Correlations`
- `Demographic_Audit`
- `Directional_Audit`
- `Figure_Index`

## Interpretation limits

- Normative deviations are research measurements, not diagnoses.
- Cross-sectional deviations do not measure longitudinal change.
- The same normative sample is used to develop the reference curves, although
  participant scores are cross-fitted.
- Stability is internal and does not replace external validation.
- Correlated or anatomically nested outcomes must not be interpreted as
  independent tests.
