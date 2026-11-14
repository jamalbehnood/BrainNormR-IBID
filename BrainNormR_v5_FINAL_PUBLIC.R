##############################################################################
# BrainNormR v5 FINAL PUBLIC -- MODE DISPATCHER
#
# This file chooses which modelling engine is loaded.
#
# Configuration:
#   Config/BrainNormR_mode_config.R
#
# Modes:
#   ALL_ONLY    -> BrainNormR_v5_FINAL_PUBLIC_ALL_ONLY.R
#   THREE_GROUP -> BrainNormR_v5_FINAL_PUBLIC_THREE_GROUP.R
#
# source() only loads the selected engine. Existing batch wrappers can continue
# to call main(CONFIG) exactly as before.
##############################################################################

mode_config_file <- file.path("Config", "BrainNormR_mode_config.R")

if (!file.exists(mode_config_file)) {
  stop(
    paste0(
      "BrainNormR mode config not found: ", mode_config_file,
      "\nCreate it or restore it from the release package."
    )
  )
}

source(mode_config_file, local = .GlobalEnv)

if (!exists("BRAINNORMR_MODE", inherits = TRUE)) {
  BRAINNORMR_MODE <- "ALL_ONLY"
}

BRAINNORMR_MODE <- toupper(trimws(as.character(BRAINNORMR_MODE)[1]))

if (identical(BRAINNORMR_MODE, "ALL_ONLY")) {

  engine_file <- "BrainNormR_v5_FINAL_PUBLIC_ALL_ONLY.R"

} else if (identical(BRAINNORMR_MODE, "THREE_GROUP")) {

  engine_file <- "BrainNormR_v5_FINAL_PUBLIC_THREE_GROUP.R"

} else {

  stop(
    paste0(
      "Invalid BRAINNORMR_MODE: ", BRAINNORMR_MODE,
      "\nAllowed values: ALL_ONLY, THREE_GROUP"
    )
  )
}

if (!file.exists(engine_file)) {
  stop("Selected BrainNormR engine file not found: ", engine_file)
}

cat("\n============================================================\n")
cat(" BrainNormR mode dispatcher\n")
cat("============================================================\n")
cat(" Selected mode: ", BRAINNORMR_MODE, "\n", sep = "")
cat(" Engine file:   ", engine_file, "\n", sep = "")
cat("============================================================\n")

source(engine_file, local = .GlobalEnv)

# Inject compute settings after the selected core creates CONFIG.
if (exists("CONFIG", inherits = TRUE)) {
  CONFIG$COMPUTE_MODE <- toupper(trimws(as.character(BRAINNORMR_COMPUTE_MODE)[1]))
  CONFIG$N_WORKERS <- as.integer(BRAINNORMR_N_WORKERS)
  CONFIG$REPRODUCIBLE_PARALLEL <- isTRUE(BRAINNORMR_REPRODUCIBLE_PARALLEL)
}

# Inject THREE_GROUP eligibility thresholds after the selected core creates CONFIG.
if (identical(BRAINNORMR_MODE, "THREE_GROUP") && exists("CONFIG", inherits = TRUE)) {
  CONFIG$MIN_STRATIFIED_N <- as.integer(BRAINNORMR_MIN_STRATIFIED_N)
  CONFIG$MIN_STRATIFIED_AGE_SPAN <- as.numeric(BRAINNORMR_MIN_STRATIFIED_AGE_SPAN)
  CONFIG$MIN_STRATIFIED_PER_FOLD <- as.integer(BRAINNORMR_MIN_STRATIFIED_PER_FOLD)
}

cat(sprintf(
  "\n[BrainNormR dispatcher] Engine loaded | analysis mode=%s | compute mode=%s | requested workers=%d.\n",
  BRAINNORMR_MODE,
  ifelse(exists("BRAINNORMR_COMPUTE_MODE"), BRAINNORMR_COMPUTE_MODE, "SEQUENTIAL"),
  ifelse(exists("BRAINNORMR_N_WORKERS"), as.integer(BRAINNORMR_N_WORKERS), 1L)
))
cat("[BrainNormR dispatcher] Run main(CONFIG) or use the existing batch wrappers.\n")
