# BrainNormR Grouped Structural Analysis

This add-on contains two R scripts:

1. `00_build_grouped_structural_dataset.R`
   - Creates whole-brain structural variables.
   - Creates 6-lobe variables.
   - Creates Left/Right hemisphere variables.
   - Optionally creates Yeo7 network variables if `Config/Yeo7_DK68_overlap_weights.csv` is supplied.

2. `06_run_grouped_structural_models.R`
   - Runs the existing frozen BrainNormR core on WholeBrain, Lobe, Hemisphere, or Yeo7 outcomes.

## Aggregation rules

### Whole-brain
Whole-brain aggregates are calculated across all 68 DK cortical ROIs:

- Volume: sum of all 68 ROI volumes.
- Area: sum of all 68 ROI surface areas.
- Thickness: surface-area-weighted mean of all 68 ROI thickness values.
- Curvature: surface-area-weighted mean of all 68 ROI curvature values.
- FD: surface-area-weighted mean of all 68 ROI-level FD values.

Important: whole-brain FD is an area-weighted regional FD composite and is **not** the same as recomputing geometric fractal dimension from the merged whole cortical surface.

### Lobe / Hemisphere / Yeo7
- Volume: sum.
- Area: sum.
- Thickness: surface-area-weighted mean.
- Curvature: surface-area-weighted mean.
- FD: surface-area-weighted mean of ROI-level FD.

Important: grouped FD is a regional composite and is not the same as recomputing geometric fractal dimension from a merged cortical surface.

## Derived variables

The builder writes a new derived workbook and does **not** overwrite the original participant-level Excel file.

Whole-brain variables created are:

- `wholebrain_volume`
- `wholebrain_area`
- `wholebrain_thickness`
- `wholebrain_curvature`
- `wholebrain_FD`

The generated `Grouped_structural_config.csv` is created automatically by the builder and includes the corresponding WholeBrain rows. It should not be edited manually for WholeBrain.

## Yeo7

The code intentionally does not invent a DK68-to-Yeo7 mapping. For Yeo7, supply a real overlap file with:

`ROI, Network, Weight`

where `Weight` is the fraction (0 to 1) of a DK ROI assigned to that network, preferably derived from vertex/surface overlap.

The existing `Yeo7_DK68_overlap_weights_TEMPLATE.csv` does **not** require any change for the addition of WholeBrain because WholeBrain aggregation is calculated directly from all 68 DK ROIs and does not use Yeo overlap weights.

## Typical workflow

1. Put the two R scripts in the BrainNormR project root.
2. Keep `DK68_structural_config.csv` in `Config/`.
3. Put `Yeo7_DK68_overlap_weights.csv` in `Config/` only if you have a validated mapping.
4. Run `00_build_grouped_structural_dataset.R`.
5. The builder creates:
   - `DerivedData/BrainNormR_grouped_structural_dataset.xlsx`
   - `Config/Grouped_structural_config.csv`
6. Then run `06_run_grouped_structural_models.R`.
7. In the grouped runner choose one analysis level:

```r
LEVEL_FILTER <- "WholeBrain"
# or
LEVEL_FILTER <- "Lobe"
# or
LEVEL_FILTER <- "Hemisphere"
# or
LEVEL_FILTER <- "Yeo7"
```

8. Choose a morphometric measure with `MEASURE_FILTER`, for example:

```r
MEASURE_FILTER <- "FD"
```

For WholeBrain there is one outcome per morphometric measure, so a typical run is:

```r
LEVEL_FILTER <- "WholeBrain"
MEASURE_FILTER <- "FD"

TEST_MODE <- FALSE
START_ROW <- 1L
END_ROW <- 1L
```

## Whole-brain covariate policy

The same BrainNormR core and covariate rules are retained:

- FD / Thickness / Curvature: Age as the primary normative predictor + Sex adjustment.
- Volume / Area: Age as the primary normative predictor + Sex + eTIV adjustment.

No separate statistical core is used for WholeBrain.

## Important methodological note

WholeBrain, Hemisphere, Lobe, and Yeo7 are different aggregation levels of the same ROI-level structural measurements. Yeo7 should be treated as a network-level representation parallel to anatomical aggregation rather than as a strict anatomical level below the lobes.
