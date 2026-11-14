##############################################################################
# BrainNormR analysis mode
#
# Change ONLY BRAINNORMR_MODE unless you intentionally want to modify
# the eligibility thresholds for sex-stratified modelling.
##############################################################################

# Default / recommended for ordinary runs:
BRAINNORMR_MODE <- "ALL_ONLY"

# To run pooled All + independent Female + independent Male models, use:
# BRAINNORMR_MODE <- "THREE_GROUP"

# Minimum safeguards used only in THREE_GROUP mode.
# They are conservative operational defaults and can be changed deliberately.
BRAINNORMR_MIN_STRATIFIED_N <- 80L
BRAINNORMR_MIN_STRATIFIED_AGE_SPAN <- 30
BRAINNORMR_MIN_STRATIFIED_PER_FOLD <- 10L

##############################################################################
# Compute mode
#
# PARALLEL uses a multisession backend (Windows-safe) for independent
# bootstrap iterations and nested-CV folds. Candidate fitting inside each task
# remains sequential to avoid nested parallelism.
##############################################################################

# Allowed: "SEQUENTIAL" | "PARALLEL"
BRAINNORMR_COMPUTE_MODE <- "PARALLEL"

# Requested workers. The core automatically caps this at available cores.
# 6 is a conservative default for a 32-GB workstation.
BRAINNORMR_N_WORKERS <- 5L

# TRUE gives deterministic per-task seeds so scheduling order does not alter
# bootstrap/CV random streams. Keep TRUE for reproducible research.
BRAINNORMR_REPRODUCIBLE_PARALLEL <- TRUE

