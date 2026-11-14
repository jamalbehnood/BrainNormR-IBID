##############################################################################
# 00_install_packages.R
# Install missing CRAN packages required by BrainNormR.
##############################################################################

required <- c(
  "readxl",
  "ggplot2",
  "dplyr",
  "patchwork",
  "gridExtra",
  "writexl",
  "gamlss",
  "gamlss.dist",
  "gamlss.add",
  "gamlss.util",
  "future",
  "future.apply",
  "nortest",
  "pracma",
  "tidyr",
  "scales",
  "viridis",
  "ggseg",
  "mgcv"
)

missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]

if (!length(missing)) {
  message("All required packages are already installed.")
} else {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, dependencies = TRUE)
}
