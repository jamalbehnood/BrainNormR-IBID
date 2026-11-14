##############################################################################
# 00_build_grouped_structural_dataset.R
# BrainNormR grouped structural aggregation
#
# Builds:
#   1) Whole-brain structural aggregates
#   2) 6 lobes by hemisphere (Left / Right) + 6 bilateral lobe aggregates
#   3) Left / Right hemispheres
#   4) Optional Yeo7 networks (only when overlap weights are supplied)
#
# IMPORTANT:
# - This script DOES NOT fit normative models.
# - It creates a derived Excel dataset and a grouped-model config file.
# - It does not modify the BrainNormR core.
##############################################################################

suppressPackageStartupMessages({
  pkgs <- c("readxl", "writexl")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Install required package(s): ", paste(missing, collapse = ", "))
  }
})

# ============================================================================
# USER SETTINGS
# ============================================================================

DATA_DIR <- "Data"
STRUCTURAL_CONFIG_FILE <- file.path("Config", "DK68_structural_config.csv")

DERIVED_DIR <- "DerivedData"
DERIVED_DATA_FILE <- file.path(DERIVED_DIR, "BrainNormR_grouped_structural_dataset.xlsx")
GROUPED_CONFIG_FILE <- file.path("Config", "Grouped_structural_config.csv")

# Optional Yeo7 overlap file.
# Required columns:
#   ROI, Network, Weight
#
# Weight should represent the fraction of a DK ROI assigned to a Yeo7 network
# (0 to 1). The preferred source is vertex/surface overlap mapping.
YEO_OVERLAP_FILE <- file.path("Config", "Yeo7_DK68_overlap_weights.csv")

BUILD_WHOLE_BRAIN <- TRUE
BUILD_LOBES <- TRUE
BUILD_HEMISPHERES <- TRUE
BUILD_YEO7 <- file.exists(YEO_OVERLAP_FILE)

# For FD, the derived grouped value is an AREA-WEIGHTED MEAN of ROI-level FD.
# It is NOT a geometric FD recomputed directly from the merged surface.
FD_GROUP_LABEL <- "Area-weighted regional FD composite"

# ============================================================================
# HELPERS
# ============================================================================

find_single_data_file <- function(data_dir = DATA_DIR) {

  if (!dir.exists(data_dir)) {
    stop("Data directory not found: ", data_dir)
  }

  files <- list.files(
    data_dir,
    pattern = "\\.(xlsx|xls)$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  files <- files[!grepl("^~\\$", basename(files))]

  if (length(files) == 0L) {
    stop("No Excel modelling dataset found in: ", data_dir)
  }

  if (length(files) > 1L) {
    stop(
      "More than one Excel file found in Data/. Keep exactly one modelling dataset there.\n",
      paste(basename(files), collapse = "\n")
    )
  }

  files[1]
}

infer_lobe <- function(roi) {

  x <- tolower(as.character(roi))
  x <- sub("^(lh_|rh_)", "", x)

  frontal <- c(
    "caudalmiddlefrontal",
    "lateralorbitofrontal",
    "medialorbitofrontal",
    "parsopercularis",
    "parsorbitalis",
    "parstriangularis",
    "precentral",
    "rostralmiddlefrontal",
    "superiorfrontal",
    "frontalpole",
    "paracentral"
  )

  temporal <- c(
    "bankssts",
    "entorhinal",
    "fusiform",
    "inferiortemporal",
    "middletemporal",
    "parahippocampal",
    "superiortemporal",
    "temporalpole",
    "transversetemporal"
  )

  parietal <- c(
    "inferiorparietal",
    "postcentral",
    "precuneus",
    "superiorparietal",
    "supramarginal"
  )

  occipital <- c(
    "cuneus",
    "lateraloccipital",
    "lingual",
    "pericalcarine"
  )

  cingulate <- c(
    "caudalanteriorcingulate",
    "isthmuscingulate",
    "posteriorcingulate",
    "rostralanteriorcingulate"
  )

  if (x %in% frontal) return("Frontal")
  if (x %in% temporal) return("Temporal")
  if (x %in% parietal) return("Parietal")
  if (x %in% occipital) return("Occipital")
  if (x %in% cingulate) return("Cingulate")
  if (identical(x, "insula")) return("Insula")

  NA_character_
}

get_hemi <- function(roi) {
  if (grepl("^lh_", roi)) return("Left")
  if (grepl("^rh_", roi)) return("Right")
  "NA"
}

safe_col <- function(x) {
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x
}

row_sum_safe <- function(df_cols) {
  x <- as.matrix(df_cols)
  storage.mode(x) <- "numeric"
  out <- rowSums(x, na.rm = TRUE)
  all_na <- rowSums(is.finite(x)) == 0
  out[all_na] <- NA_real_
  out
}

weighted_mean_by_area <- function(df, value_cols, area_cols, roi_weights = NULL) {

  V <- as.matrix(df[, value_cols, drop = FALSE])
  A <- as.matrix(df[, area_cols, drop = FALSE])

  storage.mode(V) <- "numeric"
  storage.mode(A) <- "numeric"

  if (is.null(roi_weights)) {
    W <- A
  } else {
    w <- as.numeric(roi_weights)
    if (length(w) != ncol(A)) {
      stop("roi_weights length does not match selected ROI columns.")
    }
    W <- sweep(A, 2, w, `*`)
  }

  ok <- is.finite(V) & is.finite(W) & W > 0

  num <- rowSums(ifelse(ok, V * W, 0), na.rm = TRUE)
  den <- rowSums(ifelse(ok, W, 0), na.rm = TRUE)

  out <- num / den
  out[!is.finite(out) | den <= 0] <- NA_real_
  out
}

weighted_sum_fraction <- function(df, value_cols, roi_weights) {

  X <- as.matrix(df[, value_cols, drop = FALSE])
  storage.mode(X) <- "numeric"

  w <- as.numeric(roi_weights)
  if (length(w) != ncol(X)) {
    stop("roi_weights length does not match selected ROI columns.")
  }

  Xw <- sweep(X, 2, w, `*`)
  out <- rowSums(Xw, na.rm = TRUE)

  all_na <- rowSums(is.finite(X)) == 0
  out[all_na] <- NA_real_

  out
}

add_config_row <- function(config_list,
                           level,
                           group,
                           hemisphere,
                           measure,
                           column,
                           ylabel,
                           method_note) {

  config_list[[length(config_list) + 1L]] <- data.frame(
    Level = level,
    Group = group,
    Hemisphere = hemisphere,
    Measure = measure,
    Column = column,
    YLabel = ylabel,
    AggregationMethod = method_note,
    stringsAsFactors = FALSE
  )

  config_list
}

# ============================================================================
# LOAD DATA AND DK68 CONFIG
# ============================================================================

DATA_FILE <- find_single_data_file(DATA_DIR)

if (!file.exists(STRUCTURAL_CONFIG_FILE)) {
  stop("Structural config not found: ", STRUCTURAL_CONFIG_FILE)
}

cat("\nLoading modelling data:\n", DATA_FILE, "\n", sep = "")

df <- readxl::read_excel(DATA_FILE, sheet = 1)
df <- as.data.frame(df, check.names = FALSE)

cfg <- read.csv(
  STRUCTURAL_CONFIG_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_cfg <- c("ROI", "Measure", "Column")
miss_cfg <- setdiff(required_cfg, names(cfg))

if (length(miss_cfg)) {
  stop("Structural config is missing: ", paste(miss_cfg, collapse = ", "))
}

cfg$Lobe <- vapply(cfg$ROI, infer_lobe, character(1))
cfg$Hemisphere <- vapply(cfg$ROI, get_hemi, character(1))

if (anyNA(cfg$Lobe)) {
  bad <- unique(cfg$ROI[is.na(cfg$Lobe)])
  stop(
    "Some DK ROIs could not be assigned to the 6-lobe scheme:\n",
    paste(bad, collapse = "\n")
  )
}

missing_structural_cols <- setdiff(cfg$Column, names(df))

if (length(missing_structural_cols)) {
  stop(
    "Some structural columns from DK68 config are absent from the modelling dataset.\n",
    "First missing examples:\n",
    paste(head(missing_structural_cols, 20), collapse = "\n")
  )
}

dir.create(DERIVED_DIR, recursive = TRUE, showWarnings = FALSE)

grouped_config <- list()

# ============================================================================
# 1) WHOLE-BRAIN AGGREGATION
# ============================================================================

if (isTRUE(BUILD_WHOLE_BRAIN)) {

  cat("\nBuilding whole-brain aggregates...\n")

  rois <- unique(cfg$ROI)

  get_cols_whole <- function(measure) {
    cfg$Column[
      cfg$ROI %in% rois &
      tolower(cfg$Measure) == tolower(measure)
    ]
  }

  vol_cols <- get_cols_whole("Volume")
  area_cols <- get_cols_whole("Area")
  thick_cols <- get_cols_whole("Thickness")
  curv_cols <- get_cols_whole("Curvature")
  fd_cols <- get_cols_whole("FD")

  colname <- "wholebrain_volume"
  df[[colname]] <- row_sum_safe(df[, vol_cols, drop = FALSE])

  grouped_config <- add_config_row(
    grouped_config, "WholeBrain", "WholeBrain", "Bilateral",
    "Volume", colname, "Whole-brain cortical volume",
    "Sum of all 68 DK ROI volumes"
  )

  colname <- "wholebrain_area"
  df[[colname]] <- row_sum_safe(df[, area_cols, drop = FALSE])

  grouped_config <- add_config_row(
    grouped_config, "WholeBrain", "WholeBrain", "Bilateral",
    "Area", colname, "Whole-brain cortical surface area",
    "Sum of all 68 DK ROI surface areas"
  )

  colname <- "wholebrain_thickness"
  df[[colname]] <- weighted_mean_by_area(df, thick_cols, area_cols)

  grouped_config <- add_config_row(
    grouped_config, "WholeBrain", "WholeBrain", "Bilateral",
    "Thickness", colname, "Whole-brain cortical thickness",
    "Surface-area-weighted mean of all 68 DK ROI thickness values"
  )

  colname <- "wholebrain_curvature"
  df[[colname]] <- weighted_mean_by_area(df, curv_cols, area_cols)

  grouped_config <- add_config_row(
    grouped_config, "WholeBrain", "WholeBrain", "Bilateral",
    "Curvature", colname, "Whole-brain cortical curvature",
    "Surface-area-weighted mean of all 68 DK ROI curvature values"
  )

  colname <- "wholebrain_FD"
  df[[colname]] <- weighted_mean_by_area(df, fd_cols, area_cols)

  grouped_config <- add_config_row(
    grouped_config, "WholeBrain", "WholeBrain", "Bilateral",
    "FD", colname, paste("Whole-brain", FD_GROUP_LABEL),
    "Surface-area-weighted mean of all ROI-level FD values; not geometric FD of the merged whole cortical surface"
  )
}

# ============================================================================
# 2) LOBE AGGREGATION
# ============================================================================

if (isTRUE(BUILD_LOBES)) {

  cat("\nBuilding 6-lobe aggregates...\n")

  lobes <- c("Frontal", "Parietal", "Temporal", "Occipital", "Cingulate", "Insula")
  hemis <- c("Left", "Right")

  for (hemi in hemis) {
    prefix <- if (hemi == "Left") "lh" else "rh"

    for (lobe in lobes) {

      rois <- unique(cfg$ROI[cfg$Lobe == lobe & cfg$Hemisphere == hemi])

      if (!length(rois)) next

      get_cols <- function(measure) {
        cfg$Column[
          cfg$ROI %in% rois &
          tolower(cfg$Measure) == tolower(measure)
        ]
      }

      vol_cols <- get_cols("Volume")
      area_cols <- get_cols("Area")
      thick_cols <- get_cols("Thickness")
      curv_cols <- get_cols("Curvature")
      fd_cols <- get_cols("FD")

      # Volume = sum
      colname <- safe_col(paste(prefix, lobe, "volume", sep = "_"))
      df[[colname]] <- row_sum_safe(df[, vol_cols, drop = FALSE])

      grouped_config <- add_config_row(
        grouped_config, "Lobe", lobe, hemi, "Volume", colname,
        paste(hemi, lobe, "volume"),
        "Sum of DK ROI volumes"
      )

      # Area = sum
      colname <- safe_col(paste(prefix, lobe, "area", sep = "_"))
      df[[colname]] <- row_sum_safe(df[, area_cols, drop = FALSE])

      grouped_config <- add_config_row(
        grouped_config, "Lobe", lobe, hemi, "Area", colname,
        paste(hemi, lobe, "surface area"),
        "Sum of DK ROI surface areas"
      )

      # Thickness = area-weighted mean
      colname <- safe_col(paste(prefix, lobe, "thickness", sep = "_"))
      df[[colname]] <- weighted_mean_by_area(df, thick_cols, area_cols)

      grouped_config <- add_config_row(
        grouped_config, "Lobe", lobe, hemi, "Thickness", colname,
        paste(hemi, lobe, "cortical thickness"),
        "Surface-area-weighted mean of DK ROI thickness"
      )

      # Curvature = area-weighted mean
      colname <- safe_col(paste(prefix, lobe, "curvature", sep = "_"))
      df[[colname]] <- weighted_mean_by_area(df, curv_cols, area_cols)

      grouped_config <- add_config_row(
        grouped_config, "Lobe", lobe, hemi, "Curvature", colname,
        paste(hemi, lobe, "curvature"),
        "Surface-area-weighted mean of DK ROI curvature"
      )

      # FD = area-weighted regional composite
      colname <- safe_col(paste(prefix, lobe, "FD", sep = "_"))
      df[[colname]] <- weighted_mean_by_area(df, fd_cols, area_cols)

      grouped_config <- add_config_row(
        grouped_config, "Lobe", lobe, hemi, "FD", colname,
        paste(hemi, lobe, FD_GROUP_LABEL),
        "Surface-area-weighted mean of ROI-level FD; not geometric FD of merged surface"
      )
    }
  }

  # --------------------------------------------------------------------------
  # Bilateral lobe aggregates
  #
  # These combine the homologous left- and right-hemisphere DK ROIs belonging
  # to each lobe. They are computed directly from the underlying ROI measures,
  # rather than by averaging the already-aggregated left/right lobe values.
  #
  # Volume / Area:
  #   sum across all DK ROIs from both hemispheres within the lobe.
  #
  # Thickness / Curvature / FD:
  #   surface-area-weighted mean across all DK ROIs from both hemispheres
  #   within the lobe.
  # --------------------------------------------------------------------------

  cat("\nBuilding bilateral 6-lobe aggregates...\n")

  for (lobe in lobes) {

    rois <- unique(cfg$ROI[cfg$Lobe == lobe])

    if (!length(rois)) next

    get_cols_bilateral_lobe <- function(measure) {
      cfg$Column[
        cfg$ROI %in% rois &
        tolower(cfg$Measure) == tolower(measure)
      ]
    }

    vol_cols   <- get_cols_bilateral_lobe("Volume")
    area_cols  <- get_cols_bilateral_lobe("Area")
    thick_cols <- get_cols_bilateral_lobe("Thickness")
    curv_cols  <- get_cols_bilateral_lobe("Curvature")
    fd_cols    <- get_cols_bilateral_lobe("FD")

    # Volume = bilateral sum
    colname <- safe_col(paste("bilateral", lobe, "volume", sep = "_"))
    df[[colname]] <- row_sum_safe(df[, vol_cols, drop = FALSE])

    grouped_config <- add_config_row(
      grouped_config, "Lobe", lobe, "Bilateral", "Volume", colname,
      paste("Bilateral", lobe, "volume"),
      "Sum of left and right DK ROI volumes within the lobe"
    )

    # Area = bilateral sum
    colname <- safe_col(paste("bilateral", lobe, "area", sep = "_"))
    df[[colname]] <- row_sum_safe(df[, area_cols, drop = FALSE])

    grouped_config <- add_config_row(
      grouped_config, "Lobe", lobe, "Bilateral", "Area", colname,
      paste("Bilateral", lobe, "surface area"),
      "Sum of left and right DK ROI surface areas within the lobe"
    )

    # Thickness = bilateral area-weighted mean
    colname <- safe_col(paste("bilateral", lobe, "thickness", sep = "_"))
    df[[colname]] <- weighted_mean_by_area(df, thick_cols, area_cols)

    grouped_config <- add_config_row(
      grouped_config, "Lobe", lobe, "Bilateral", "Thickness", colname,
      paste("Bilateral", lobe, "cortical thickness"),
      "Surface-area-weighted mean of left and right DK ROI thickness values within the lobe"
    )

    # Curvature = bilateral area-weighted mean
    colname <- safe_col(paste("bilateral", lobe, "curvature", sep = "_"))
    df[[colname]] <- weighted_mean_by_area(df, curv_cols, area_cols)

    grouped_config <- add_config_row(
      grouped_config, "Lobe", lobe, "Bilateral", "Curvature", colname,
      paste("Bilateral", lobe, "curvature"),
      "Surface-area-weighted mean of left and right DK ROI curvature values within the lobe"
    )

    # FD = bilateral area-weighted regional composite
    colname <- safe_col(paste("bilateral", lobe, "FD", sep = "_"))
    df[[colname]] <- weighted_mean_by_area(df, fd_cols, area_cols)

    grouped_config <- add_config_row(
      grouped_config, "Lobe", lobe, "Bilateral", "FD", colname,
      paste("Bilateral", lobe, FD_GROUP_LABEL),
      "Surface-area-weighted mean of left and right ROI-level FD values within the lobe; not geometric FD of a merged bilateral lobe surface"
    )
  }
}

# ============================================================================
# 3) HEMISPHERE AGGREGATION
# ============================================================================

if (isTRUE(BUILD_HEMISPHERES)) {

  cat("\nBuilding hemisphere aggregates...\n")

  hemis <- c("Left", "Right")

  for (hemi in hemis) {

    prefix <- if (hemi == "Left") "lh" else "rh"
    rois <- unique(cfg$ROI[cfg$Hemisphere == hemi])

    get_cols <- function(measure) {
      cfg$Column[
        cfg$ROI %in% rois &
        tolower(cfg$Measure) == tolower(measure)
      ]
    }

    vol_cols <- get_cols("Volume")
    area_cols <- get_cols("Area")
    thick_cols <- get_cols("Thickness")
    curv_cols <- get_cols("Curvature")
    fd_cols <- get_cols("FD")

    colname <- safe_col(paste(prefix, "hemisphere", "volume", sep = "_"))
    df[[colname]] <- row_sum_safe(df[, vol_cols, drop = FALSE])

    grouped_config <- add_config_row(
      grouped_config, "Hemisphere", hemi, hemi, "Volume", colname,
      paste(hemi, "hemisphere volume"),
      "Sum of all DK ROI volumes in hemisphere"
    )

    colname <- safe_col(paste(prefix, "hemisphere", "area", sep = "_"))
    df[[colname]] <- row_sum_safe(df[, area_cols, drop = FALSE])

    grouped_config <- add_config_row(
      grouped_config, "Hemisphere", hemi, hemi, "Area", colname,
      paste(hemi, "hemisphere surface area"),
      "Sum of all DK ROI surface areas in hemisphere"
    )

    colname <- safe_col(paste(prefix, "hemisphere", "thickness", sep = "_"))
    df[[colname]] <- weighted_mean_by_area(df, thick_cols, area_cols)

    grouped_config <- add_config_row(
      grouped_config, "Hemisphere", hemi, hemi, "Thickness", colname,
      paste(hemi, "hemisphere cortical thickness"),
      "Surface-area-weighted mean of DK ROI thickness"
    )

    colname <- safe_col(paste(prefix, "hemisphere", "curvature", sep = "_"))
    df[[colname]] <- weighted_mean_by_area(df, curv_cols, area_cols)

    grouped_config <- add_config_row(
      grouped_config, "Hemisphere", hemi, hemi, "Curvature", colname,
      paste(hemi, "hemisphere curvature"),
      "Surface-area-weighted mean of DK ROI curvature"
    )

    colname <- safe_col(paste(prefix, "hemisphere", "FD", sep = "_"))
    df[[colname]] <- weighted_mean_by_area(df, fd_cols, area_cols)

    grouped_config <- add_config_row(
      grouped_config, "Hemisphere", hemi, hemi, "FD", colname,
      paste(hemi, "hemisphere", FD_GROUP_LABEL),
      "Surface-area-weighted mean of ROI-level FD; not geometric FD of merged surface"
    )
  }
}

# ============================================================================
# 4) OPTIONAL YEO7 AGGREGATION
# ============================================================================

if (isTRUE(BUILD_YEO7)) {

  cat("\nBuilding Yeo7 network aggregates from overlap weights...\n")

  yw <- read.csv(
    YEO_OVERLAP_FILE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  req_y <- c("ROI", "Network", "Weight")
  miss_y <- setdiff(req_y, names(yw))

  if (length(miss_y)) {
    stop("Yeo overlap file is missing: ", paste(miss_y, collapse = ", "))
  }

  yw$Weight <- as.numeric(yw$Weight)

  if (any(!is.finite(yw$Weight)) || any(yw$Weight < 0) || any(yw$Weight > 1)) {
    stop("Yeo Weight must be finite and between 0 and 1.")
  }

  if (!all(yw$ROI %in% unique(cfg$ROI))) {
    bad <- unique(yw$ROI[!yw$ROI %in% unique(cfg$ROI)])
    stop("Yeo overlap file contains unknown DK ROI(s):\n", paste(bad, collapse = "\n"))
  }

  # Sanity check: an ROI should not be allocated >100% across networks.
  sw <- aggregate(Weight ~ ROI, data = yw, sum)

  if (any(sw$Weight > 1.001)) {
    bad <- sw$ROI[sw$Weight > 1.001]
    stop(
      "Yeo overlap weights sum to >1 for some ROIs:\n",
      paste(bad, collapse = "\n")
    )
  }

  networks <- unique(yw$Network)

  for (net in networks) {

    yn <- yw[yw$Network == net & yw$Weight > 0, , drop = FALSE]
    if (!nrow(yn)) next

    rois <- yn$ROI
    weights <- yn$Weight

    get_cols_for_rois <- function(measure) {

      m <- cfg[
        cfg$ROI %in% rois &
        tolower(cfg$Measure) == tolower(measure),
        c("ROI", "Column"),
        drop = FALSE
      ]

      # preserve Yeo file ROI order
      m <- m[match(rois, m$ROI), , drop = FALSE]

      if (anyNA(m$Column)) {
        stop("Missing ", measure, " column for one or more Yeo-mapped ROIs.")
      }

      m$Column
    }

    vol_cols <- get_cols_for_rois("Volume")
    area_cols <- get_cols_for_rois("Area")
    thick_cols <- get_cols_for_rois("Thickness")
    curv_cols <- get_cols_for_rois("Curvature")
    fd_cols <- get_cols_for_rois("FD")

    # Approximate network volume/area by overlap fraction.
    colname <- safe_col(paste("Yeo7", net, "volume", sep = "_"))
    df[[colname]] <- weighted_sum_fraction(df, vol_cols, weights)

    grouped_config <- add_config_row(
      grouped_config, "Yeo7", net, "Bilateral", "Volume", colname,
      paste("Yeo7", net, "network volume"),
      "ROI volume multiplied by DK-to-Yeo overlap fraction, then summed"
    )

    colname <- safe_col(paste("Yeo7", net, "area", sep = "_"))
    df[[colname]] <- weighted_sum_fraction(df, area_cols, weights)

    grouped_config <- add_config_row(
      grouped_config, "Yeo7", net, "Bilateral", "Area", colname,
      paste("Yeo7", net, "network surface area"),
      "ROI area multiplied by DK-to-Yeo overlap fraction, then summed"
    )

    colname <- safe_col(paste("Yeo7", net, "thickness", sep = "_"))
    df[[colname]] <- weighted_mean_by_area(df, thick_cols, area_cols, weights)

    grouped_config <- add_config_row(
      grouped_config, "Yeo7", net, "Bilateral", "Thickness", colname,
      paste("Yeo7", net, "network cortical thickness"),
      "Overlap-area-weighted mean of DK ROI thickness"
    )

    colname <- safe_col(paste("Yeo7", net, "curvature", sep = "_"))
    df[[colname]] <- weighted_mean_by_area(df, curv_cols, area_cols, weights)

    grouped_config <- add_config_row(
      grouped_config, "Yeo7", net, "Bilateral", "Curvature", colname,
      paste("Yeo7", net, "network curvature"),
      "Overlap-area-weighted mean of DK ROI curvature"
    )

    colname <- safe_col(paste("Yeo7", net, "FD", sep = "_"))
    df[[colname]] <- weighted_mean_by_area(df, fd_cols, area_cols, weights)

    grouped_config <- add_config_row(
      grouped_config, "Yeo7", net, "Bilateral", "FD", colname,
      paste("Yeo7", net, FD_GROUP_LABEL),
      "Overlap-area-weighted mean of ROI-level FD; not geometric network FD"
    )
  }

} else {

  cat(
    "\nYeo7 aggregation skipped because this file was not found:\n",
    YEO_OVERLAP_FILE,
    "\n"
  )
}

# ============================================================================
# SAVE
# ============================================================================

grouped_config_df <- do.call(rbind, grouped_config)

if (is.null(grouped_config_df) || !nrow(grouped_config_df)) {
  stop("No grouped variables were created.")
}

writexl::write_xlsx(
  list(ModelingData = df),
  DERIVED_DATA_FILE
)

write.csv(
  grouped_config_df,
  GROUPED_CONFIG_FILE,
  row.names = FALSE
)

cat("\n============================================================\n")
cat("Grouped structural dataset created successfully.\n")
cat("============================================================\n")
cat("Derived data:\n", DERIVED_DATA_FILE, "\n", sep = "")
cat("Grouped config:\n", GROUPED_CONFIG_FILE, "\n", sep = "")
cat("Number of grouped outcomes: ", nrow(grouped_config_df), "\n", sep = "")
cat("Yeo7 included: ", isTRUE(BUILD_YEO7), "\n", sep = "")
cat("============================================================\n")
