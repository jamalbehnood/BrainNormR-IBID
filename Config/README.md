# Config

- `DK68_structural_config.csv`: ROI-level structural configuration.
- `Cognition_Config.csv`: cognitive outcome configuration.
- `Yeo7_DK68_overlap_weights_TEMPLATE.csv`: template only.

WholeBrain, Lobe, Hemisphere, and optional Yeo7 grouped configuration rows
are generated automatically by `00_build_grouped_structural_dataset.R`.

Adding WholeBrain does not require manual changes to the DK68 structural
configuration or the Yeo7 overlap template.


## BrainNormR mode config

`BrainNormR_mode_config.R` controls the modelling engine:
- `ALL_ONLY` = pooled model only (default)
- `THREE_GROUP` = pooled All plus independent Female and Male models after
  minimum-sample/age-span prechecks

The switch does not change the ROI/cognition configuration files.

### Compute settings

`BrainNormR_mode_config.R` also controls compute mode:
`BRAINNORMR_COMPUTE_MODE`, `BRAINNORMR_N_WORKERS`, and
`BRAINNORMR_REPRODUCIBLE_PARALLEL`. Parallel mode is Windows-safe and is
restricted to independent bootstrap/CV tasks.

## Individualized deviation profiles

`Deviation_profile_config.R` controls the post-model Paper 3 analysis. The
defaults use cross-fitted structural scores, bilateral lobe aggregates, four
anatomical scales, three deviation thresholds, reproducible permutations and
outcome-resampling stability analyses. Yeo7 remains optional.
