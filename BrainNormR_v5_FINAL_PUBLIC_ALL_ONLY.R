##############################################################################
# BrainNormR v5.5.1 CORRECTED - Q1-grade Multi-Engine Normative Modeling Pipeline
##############################################################################

REQUIRED_PKGS <- c(
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
  "mgcv"
)

missing_pkgs <- REQUIRED_PKGS[!vapply(REQUIRED_PKGS, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    paste0(
      "Missing required R packages: ", paste(missing_pkgs, collapse = ", "),
      "\nInstall them explicitly before running this analysis. ",
      "For article reproducibility, save package versions with renv::snapshot()."
    )
  )
}

library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)
library(gridExtra)
library(writexl)
library(gamlss)
library(gamlss.dist)
library(gamlss.add)
library(gamlss.util)
library(future)
library(future.apply)
library(nortest)
library(pracma)
library(mgcv)

plan(sequential)

##############################################################################
# CONFIG
##############################################################################

CONFIG <- list(
  DATA_PATH   = file.path("Data", "YOUR_DATA.xlsx"),
  SHEET_NAME  = 1,
  # ===== ONLY CHANGE THESE FOR EACH OUTCOME =====
  OUTCOME_COL  = "Fractal",
  OUTCOME_TYPE = "FD",   # FD | Thickness | Curvature | Volume | Area | Cognition | Other
  Y_LABEL      = "Fractal",

  # ===== OPTIONAL METADATA FOR MASTER AGGREGATION =====
  # These fields do not change the statistical model; they make every output
  # self-identifying when hundreds of outcome files are combined later.
  ANALYSIS_LEVEL = "AUTO",      # AUTO | ROI | Lobe | WholeBrain | Cognition
  REGION_NAME    = NULL,        # NULL => OUTCOME_COL; or e.g., Frontal / WholeBrain
  HEMISPHERE     = "AUTO",      # AUTO | Left | Right | Bilateral | NA
  LOBE_NAME      = "NA",        # optional; can be added later from a DK lookup table
  ID_COL         = NULL,         # optional subject-ID column; NULL => row-based ID
  # ====================================================
  # ==============================================
  AGE_COL     = "Age",
  SEX_COL     = "sex",
  EDU_COL     = "edu",
  ETIV_COL    = "eTIV",
  FEMALE_CODE = 1,
  MALE_CODE   = 0,
  
  SEED   = 1234,
  N_BOOT = 1000,
  BOOT_MIN_SUCCESS_PROP = 0.80,
  MILESTONE_BOOT_MIN_PROP = 0.80,
  GRID_SIZE = 400,
  STORE_BOOT_CURVES = TRUE,
  
  # Q1-grade internal validation / cross-fitting
  NESTED_CV_FOLDS   = 5,
  NESTED_CV_REPEATS = 1,
  QUIET_NESTED_SELECTION = TRUE,
  
  # Parsimonious, independent scale modeling.  mu can use FP1/FP2;
  # sigma is selected independently from constant or FP1 forms.
  SIGMA_FP_POWERS = c(-1, 0, 0.5, 1, 2),
  MODEL_SELECTION_TOP_MU = 2,
  
  # ── Engine selection ──
  PRIMARY_ENGINE    = "GAMLSS",   # distributional normative model
  ENGINE_METRIC     = "CV_NLL",   # proper distributional score; lower is better
  RUN_ENGINE_CV     = TRUE,
  ENGINE_CV_FOLDS   = 5,
  
  # ── GAMLSS family hierarchy ──
  GAMLSS_FAMILY_LEVELS = list(
    Level1 = c("NO", "GA", "GG"),
    Level2 = c("BCCG", "BCPE", "BCT"),
    Level3 = c("SHASH", "TF", "ST1", "ST2", "ST3", "ST4")
  ),
  GAMLSS_STOP_AT_FIRST_PASS  = FALSE,
  GAMLSS_PREFER_LOWER_LEVEL  = FALSE,
  
  CANDIDATE_FAMILIES = c(
    "NO","GA","GG","BCCG","BCPE","BCT",
    "SHASH","TF","ST1","ST2","ST3","ST4"
  ),
  
  # ── Functional forms ──
  FP_POWERS    = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3),
  POLY_DEGREES = 1:3,
  GAM_BS       = c("tp", "ts", "cr", "cs", "ps"),
  GAM_K        = 5,
  
  CENTILES = c(2.5, 5, 10, 25, 50, 75, 90, 95, 97.5),
  AGE_GROUP_BREAKS = c(-Inf, 30, 40, 50, 60, Inf),
  AGE_GROUP_LABELS = c("G1", "G2", "G3", "G4", "G5"),
  BOUNDARY_TOL_PROP = 0.05,

  # Plotting safeguard for near-constant conditional variability.
  # If the total range of Conditional SD is less than 0.5% of its mean,
  # keep the original values but prevent automatic y-axis magnification
  # from turning numerical jitter into visually large spikes.
  VARIABILITY_FLAT_REL_TOL = 0.005,

  OUT_DIR = NULL
)

# Backward-compatible internal alias; do not edit FD_COL manually.
CONFIG$FD_COL <- CONFIG$OUTCOME_COL

# Project-safe default. Batch wrappers override DATA_PATH and OUT_DIR per outcome.
# The core is intentionally source-safe and does NOT auto-run on source().
if (is.null(CONFIG$OUT_DIR) || !nzchar(as.character(CONFIG$OUT_DIR))) {
  CONFIG$OUT_DIR <- "Results"
}
dir.create(CONFIG$OUT_DIR, showWarnings = FALSE, recursive = TRUE)
set.seed(CONFIG$SEED)

##############################################################################
# COMPUTE BACKEND: sequential / reproducible parallel execution
#
# Parallelization is deliberately restricted to independent resampling/fold
# tasks. Candidate fitting inside each task remains sequential to avoid nested
# parallelism and excessive RAM use.
##############################################################################

if (is.null(CONFIG$COMPUTE_MODE)) CONFIG$COMPUTE_MODE <- "SEQUENTIAL"
if (is.null(CONFIG$N_WORKERS)) CONFIG$N_WORKERS <- 1L
if (is.null(CONFIG$REPRODUCIBLE_PARALLEL)) CONFIG$REPRODUCIBLE_PARALLEL <- TRUE

compute_mode_normalize <- function(x) {
  x <- toupper(trimws(as.character(x)[1]))
  if (!x %in% c("SEQUENTIAL", "PARALLEL")) {
    stop("COMPUTE_MODE must be 'SEQUENTIAL' or 'PARALLEL'.")
  }
  x
}

stable_task_seed <- function(base_seed, label = "", task_id = 0L, stage_id = 1L) {
  lab <- paste(as.character(label), collapse = "_")
  code <- if (nzchar(lab)) sum(utf8ToInt(lab)) else 0
  val <- (
    abs(as.numeric(base_seed)) +
      as.numeric(stage_id) * 1000003 +
      as.numeric(code) * 1009 +
      as.numeric(task_id) * 9176
  ) %% 2147483646
  as.integer(val + 1)
}

configure_compute_backend <- function(cfg = CONFIG, quiet = FALSE) {
  mode <- compute_mode_normalize(cfg$COMPUTE_MODE)

  if (identical(mode, "PARALLEL")) {
    available <- suppressWarnings(as.integer(future::availableCores()))
    if (!is.finite(available) || available < 1L) available <- 1L

    requested <- suppressWarnings(as.integer(cfg$N_WORKERS))
    if (!is.finite(requested) || requested < 1L) requested <- 1L

    workers <- max(1L, min(requested, available))

    if (workers <= 1L) {
      future::plan(future::sequential)
      mode_used <- "SEQUENTIAL"
    } else {
      future::plan(future::multisession, workers = workers)
      mode_used <- "PARALLEL"
    }
  } else {
    future::plan(future::sequential)
    workers <- 1L
    mode_used <- "SEQUENTIAL"
  }

  if (!quiet) {
    cat(sprintf(
      " Compute backend: requested=%s | used=%s | workers=%d | reproducible RNG=%s\n",
      mode, mode_used, workers,
      ifelse(isTRUE(cfg$REPRODUCIBLE_PARALLEL), "TRUE", "FALSE")
    ))
  }

  invisible(list(
    requested_mode = mode,
    mode = mode_used,
    workers = workers
  ))
}

compute_lapply <- function(X, FUN, cfg = CONFIG, future_seed = TRUE) {
  mode <- compute_mode_normalize(cfg$COMPUTE_MODE)

  if (identical(mode, "PARALLEL") && future::nbrOfWorkers() > 1L) {
    future.apply::future_lapply(
      X,
      FUN,
      future.seed = future_seed,
      future.scheduling = 1
    )
  } else {
    lapply(X, FUN)
  }
}

elapsed_seconds <- function(start_time, end_time = Sys.time()) {
  as.numeric(difftime(end_time, start_time, units = "secs"))
}

runtime_record <- function(group, cfg, backend, start_time,
                           search_sec = NA_real_,
                           bootstrap_sec = NA_real_,
                           validation_sec = NA_real_,
                           engine_sec = NA_real_,
                           postprocess_sec = NA_real_,
                           end_time = Sys.time()) {
  total_sec <- elapsed_seconds(start_time, end_time)
  data.frame(
    Group = as.character(group),
    ComputeMode_Requested = compute_mode_normalize(cfg$COMPUTE_MODE),
    ComputeMode_Used = as.character(backend$mode),
    Workers = as.integer(backend$workers),
    ReproducibleParallelRNG = isTRUE(cfg$REPRODUCIBLE_PARALLEL),
    Start_Time = format(start_time, "%Y-%m-%d %H:%M:%S"),
    End_Time = format(end_time, "%Y-%m-%d %H:%M:%S"),
    GAMLSS_Search_Seconds = as.numeric(search_sec),
    Bootstrap_Seconds = as.numeric(bootstrap_sec),
    Validation_NestedCV_Seconds = as.numeric(validation_sec),
    EngineComparison_Seconds = as.numeric(engine_sec),
    Postprocess_Seconds = as.numeric(postprocess_sec),
    Total_Runtime_Seconds = total_sec,
    Total_Runtime_Minutes = total_sec / 60,
    Total_Runtime_Hours = total_sec / 3600,
    stringsAsFactors = FALSE
  )
}

##############################################################################
# Helper functions
##############################################################################

trim_family <- function(x) {
  trimws(as.character(x)[1])
}

family_has_nu <- function(family_name) {
  family_name <- trim_family(family_name)
  family_name %in% c(
    "GG", "BCCG", "BCPE", "BCT",
    "SHASH", "ST1", "ST2", "ST3", "ST4", "TF", "PE"
  )
}

family_has_tau <- function(family_name) {
  family_name <- trim_family(family_name)
  family_name %in% c(
    "BCPE", "BCT", "SHASH",
    "ST1", "ST2", "ST3", "ST4"
  )
}

get_family_obj <- function(family_name) {
  family_name <- trim_family(family_name)
  
  switch(
    family_name,
    "NO"    = gamlss.dist::NO(),
    "GA"    = gamlss.dist::GA(),
    "GG"    = gamlss.dist::GG(),
    "BCCG"  = gamlss.dist::BCCG(),
    "BCPE"  = gamlss.dist::BCPE(),
    "BCT"   = gamlss.dist::BCT(),
    "SHASH" = gamlss.dist::SHASH(),
    "ST1"   = gamlss.dist::ST1(),
    "ST2"   = gamlss.dist::ST2(),
    "ST3"   = gamlss.dist::ST3(),
    "ST4"   = gamlss.dist::ST4(),
    "TF"    = gamlss.dist::TF(),
    "PE"    = gamlss.dist::PE(),
    stop(sprintf("Unsupported family: %s", family_name))
  )
}

set_links <- function(fam, family_name) {
  # Preserve the default links defined by each GAMLSS family.
  fam
}

make_qfun <- function(family_name, mu, sigma, nu = NULL, tau = NULL) {
  family_name <- trim_family(family_name)
  
  if (family_has_nu(family_name) && is.null(nu)) {
    nu <- rep(1, length(mu))
  }
  
  if (family_has_tau(family_name) && is.null(tau)) {
    tau <- rep(2, length(mu))
  }
  
  switch(
    family_name,
    "NO" = function(p) {
      tryCatch(qNO(p, mu = mu, sigma = sigma),
               error = function(e) rep(NA_real_, length(mu)))
    },
    "GA" = function(p) {
      tryCatch(qGA(p, mu = mu, sigma = sigma),
               error = function(e) rep(NA_real_, length(mu)))
    },
    "GG" = function(p) {
      tryCatch(qGG(p, mu = mu, sigma = sigma, nu = nu),
               error = function(e) rep(NA_real_, length(mu)))
    },
    "BCCG" = function(p) {
      tryCatch(qBCCG(p, mu = mu, sigma = sigma, nu = nu),
               error = function(e) rep(NA_real_, length(mu)))
    },
    "BCPE" = function(p) {
      tryCatch(qBCPE(p, mu = mu, sigma = sigma, nu = nu, tau = tau),
               error = function(e) rep(NA_real_, length(mu)))
    },
    "BCT" = function(p) {
      tryCatch(qBCT(p, mu = mu, sigma = sigma, nu = nu, tau = tau),
               error = function(e) rep(NA_real_, length(mu)))
    },
    "SHASH" = function(p) {
      tryCatch(qSHASH(p, mu = mu, sigma = sigma, nu = nu, tau = tau),
               error = function(e) rep(NA_real_, length(mu)))
    },
    "ST1" = function(p) {
      tryCatch(qST1(p, mu = mu, sigma = sigma, nu = nu, tau = tau),
               error = function(e) rep(NA_real_, length(mu)))
    },
    "ST2" = function(p) {
      tryCatch(qST2(p, mu = mu, sigma = sigma, nu = nu, tau = tau),
               error = function(e) rep(NA_real_, length(mu)))
    },
    "ST3" = function(p) {
      tryCatch(qST3(p, mu = mu, sigma = sigma, nu = nu, tau = tau),
               error = function(e) rep(NA_real_, length(mu)))
    },
    "ST4" = function(p) {
      tryCatch(qST4(p, mu = mu, sigma = sigma, nu = nu, tau = tau),
               error = function(e) rep(NA_real_, length(mu)))
    },
    "TF" = function(p) {
      tryCatch(qTF(p, mu = mu, sigma = sigma, nu = nu),
               error = function(e) rep(NA_real_, length(mu)))
    },
    "PE" = function(p) {
      tryCatch(qPE(p, mu = mu, sigma = sigma, nu = nu),
               error = function(e) rep(NA_real_, length(mu)))
    },
    stop(sprintf("Unsupported family: %s", family_name))
  )
}

make_pfun <- function(family_name, mu, sigma, nu = NULL, tau = NULL) {
  family_name <- trim_family(family_name)
  
  if (family_has_nu(family_name) && is.null(nu)) {
    nu <- rep(1, length(mu))
  }
  
  if (family_has_tau(family_name) && is.null(tau)) {
    tau <- rep(2, length(mu))
  }
  
  switch(
    family_name,
    "NO" = function(y) {
      tryCatch(pNO(y, mu = mu, sigma = sigma),
               error = function(e) rep(NA_real_, length(y)))
    },
    "GA" = function(y) {
      tryCatch(pGA(y, mu = mu, sigma = sigma),
               error = function(e) rep(NA_real_, length(y)))
    },
    "GG" = function(y) {
      tryCatch(pGG(y, mu = mu, sigma = sigma, nu = nu),
               error = function(e) rep(NA_real_, length(y)))
    },
    "BCCG" = function(y) {
      tryCatch(pBCCG(y, mu = mu, sigma = sigma, nu = nu),
               error = function(e) rep(NA_real_, length(y)))
    },
    "BCPE" = function(y) {
      tryCatch(pBCPE(y, mu = mu, sigma = sigma, nu = nu, tau = tau),
               error = function(e) rep(NA_real_, length(y)))
    },
    "BCT" = function(y) {
      tryCatch(pBCT(y, mu = mu, sigma = sigma, nu = nu, tau = tau),
               error = function(e) rep(NA_real_, length(y)))
    },
    "SHASH" = function(y) {
      tryCatch(pSHASH(y, mu = mu, sigma = sigma, nu = nu, tau = tau),
               error = function(e) rep(NA_real_, length(y)))
    },
    "ST1" = function(y) {
      tryCatch(pST1(y, mu = mu, sigma = sigma, nu = nu, tau = tau),
               error = function(e) rep(NA_real_, length(y)))
    },
    "ST2" = function(y) {
      tryCatch(pST2(y, mu = mu, sigma = sigma, nu = nu, tau = tau),
               error = function(e) rep(NA_real_, length(y)))
    },
    "ST3" = function(y) {
      tryCatch(pST3(y, mu = mu, sigma = sigma, nu = nu, tau = tau),
               error = function(e) rep(NA_real_, length(y)))
    },
    "ST4" = function(y) {
      tryCatch(pST4(y, mu = mu, sigma = sigma, nu = nu, tau = tau),
               error = function(e) rep(NA_real_, length(y)))
    },
    "TF" = function(y) {
      tryCatch(pTF(y, mu = mu, sigma = sigma, nu = nu),
               error = function(e) rep(NA_real_, length(y)))
    },
    "PE" = function(y) {
      tryCatch(pPE(y, mu = mu, sigma = sigma, nu = nu),
               error = function(e) rep(NA_real_, length(y)))
    },
    stop(sprintf("Unsupported family: %s", family_name))
  )
}

# Corrected: GAIC with fallbacks for non-GAMLSS models.
extract_gaic <- function(mod, k = 2) {
  # First use the native GAMLSS GAIC method.
  x <- tryCatch(GAIC(mod, k = k), error = function(e) NULL)
  if (!is.null(x)) {
    if (is.matrix(x) || is.data.frame(x)) x <- x[1, 1]
    x <- as.numeric(x[1])
    if (is.finite(x)) return(x)
  }
  # Fallback: AIC with a user-specified penalty for lm and gam models.
  x <- tryCatch(AIC(mod, k = k), error = function(e) NULL)
  if (!is.null(x)) {
    x <- as.numeric(x[1])
    if (is.finite(x)) return(x)
  }
  # Final fallback: calculate the criterion from log-likelihood manually.
  ll_obj <- tryCatch(logLik(mod), error = function(e) NULL)
  if (!is.null(ll_obj)) {
    ll <- as.numeric(ll_obj)
    df <- attr(ll_obj, "df")
    if (!is.null(df) && is.finite(ll) && is.finite(df)) {
      return(-2 * ll + k * df)
    }
  }
  NA_real_
}

# Safe inverse-link helper.
inverse_link_safe <- function(eta, link) {
  if (is.null(link) || length(link) == 0 || is.na(link)) {
    return(rep(NA_real_, length(eta)))
  }
  link <- as.character(link[1])
  lnk <- tryCatch(make.link(link), error = function(e) NULL)
  if (is.null(lnk)) {
    warning(sprintf("Unsupported link: '%s'. Returning NA.", link))
    return(rep(NA_real_, length(eta)))
  }
  as.numeric(tryCatch(lnk$linkinv(eta), error = function(e) rep(NA_real_, length(eta))))
}

# make.link helper for GAMLSS link functions.
make.link <- function(link) {
  switch(link,
         "identity" = list(linkinv = function(x) x),
         "log"      = list(linkinv = function(x) exp(x)),
         "logit"    = list(linkinv = function(x) exp(x) / (1 + exp(x))),
         "sqrt"     = list(linkinv = function(x) x^2),
         "inverse"  = list(linkinv = function(x) 1/x),
         list(linkinv = function(x) x)  # default: identity
  )
}

# ── FP builder ──
build_fp <- function(var, powers) {
  if (length(powers) == 0) return("1")
  power_count <- table(powers)
  terms <- character(0)
  for (p in as.numeric(names(power_count))) {
    cnt <- as.integer(power_count[as.character(p)])
    if (p == 0) {
      terms <- c(terms, sprintf("log(%s)", var))
      if (cnt >= 2) terms <- c(terms, sprintf("I(log(%s)^2)", var))
      if (cnt >= 3) terms <- c(terms, sprintf("I(log(%s)^3)", var))
    } else {
      base <- sprintf("I((%s)^(%s))", var, p)
      terms <- c(terms, base)
      if (cnt >= 2) {
        terms <- c(terms, sprintf("I((%s)^(%s) * log(%s))", var, p, var))
      }
      if (cnt >= 3) {
        terms <- c(terms, sprintf("I((%s)^(%s) * log(%s)^2)", var, p, var))
      }
    }
  }
  paste(terms, collapse = " + ")
}

# [legacy duplicate function removed in public release: make_formulas]
##############################################################################
# STAGE 1: Data ingestion and cleaning (unchanged)
##############################################################################

# [legacy duplicate function removed in public release: load_data]

# [legacy duplicate function removed in public release: clean_data]

# [legacy duplicate function removed in public release: validate_data]
##############################################################################
# STAGE 2: Model fitting (unchanged)
##############################################################################

# [legacy duplicate function removed in public release: fit_model]
model_centile_sanity <- function(model, data, age_grid, cfg = CONFIG) {
  curve <- tryCatch(predict_curve(model, age_grid), error = function(e) NULL)
  if (is.null(curve)) return(FALSE)
  
  y <- data$FD[is.finite(data$FD)]
  if (length(y) < 10) return(TRUE)
  
  raw_range <- range(y, na.rm = TRUE)
  span <- diff(raw_range)
  if (!is.finite(span) || span <= 0) span <- 1
  
  central_ok <- all(
    curve$central >= raw_range[1] - 0.35 * span &
      curve$central <= raw_range[2] + 0.35 * span, na.rm = TRUE)
  lo_ok <- all(curve$lo >= raw_range[1] - 1.0 * span, na.rm = TRUE)
  hi_ok <- all(curve$hi <= raw_range[2] + 1.0 * span, na.rm = TRUE)
  if (!central_ok || !lo_ok || !hi_ok) return(FALSE)
  
  fam <- attr(model, "family_name")
  if (is.null(fam)) fam <- as.character(model$family[1])
  mu    <- tryCatch(fitted(model, "mu"),    error = function(e) NULL)
  sigma <- tryCatch(fitted(model, "sigma"), error = function(e) NULL)
  if (is.null(mu) || is.null(sigma)) return(FALSE)
  
  nu  <- if (family_has_nu(fam))  tryCatch(fitted(model, "nu"),  error = function(e) NULL) else NULL
  tau <- if (family_has_tau(fam)) tryCatch(fitted(model, "tau"), error = function(e) NULL) else NULL
  
  pfun <- tryCatch(make_pfun(fam, mu, sigma, nu, tau), error = function(e) NULL)
  if (is.null(pfun)) return(FALSE)
  p <- tryCatch(pfun(model$y), error = function(e) NULL)
  if (is.null(p)) return(FALSE)
  p <- p[is.finite(p)]
  if (length(p) < 20) return(TRUE)
  
  prop_lo <- mean(p <= 0.025, na.rm = TRUE)
  prop_hi <- mean(p >= 0.975, na.rm = TRUE)
  if (prop_lo > 0.15 || prop_hi > 0.15) return(FALSE)
  TRUE
}
# [legacy duplicate function removed in public release: select_model]
##############################################################################
# STAGE 3b: Hierarchical GAMLSS family selection (new)
##############################################################################

# [legacy duplicate function removed in public release: select_gamlss_hierarchical]
##############################################################################
# STAGE 4: Prediction (revised)
##############################################################################

# [legacy duplicate function removed in public release: predict_parameters]


safe_gradient <- function(y, x) {
  if (length(y) < 3 || length(x) < 3) return(rep(NA_real_, length(y)))
  ok <- is.finite(y) & is.finite(x)
  if (sum(ok) < 3) return(rep(NA_real_, length(y)))
  if (!all(ok)) {
    y <- tryCatch(approx(x[ok], y[ok], xout = x, rule = 2)$y,
                  error = function(e) rep(NA_real_, length(y)))
  }
  if (any(!is.finite(y))) return(rep(NA_real_, length(y)))
  as.numeric(pracma::gradient(y, x))
}

# [legacy duplicate function removed in public release: predict_curve]
# ============================================================================
# STAGE 4.5: INDIVIDUAL NORMATIVE PREDICTION
# ============================================================================
# ============================================================================
# STAGE 4.5: INDIVIDUAL NORMATIVE PREDICTION (GAMLSS FIX)
# ============================================================================
predict_individual_normative <- function(model, data, response) {
  cat("\nIndividual normative prediction (CDF-based)...\n")
  
  # Fallback for the response-column name.
  if (!response %in% names(data)) {
    if ("FD" %in% names(data)) {
      message(sprintf(
        "Response column '%s' not found. Using internal column 'FD'.",
        response
      ))
      response <- "FD"
    } else {
      stop(
        paste0(
          "Response column not found: ", response,
          "\nAvailable columns: ", paste(names(data), collapse = ", ")
        )
      )
    }
  }
  
  if (!"Age" %in% names(data)) {
    stop("Age column not found in data.")
  }
  
  if (!"Age_s" %in% names(data)) {
    data$Age_s <- data$Age / 10
  }
  
  bad_age <- !is.finite(data$Age) | !is.finite(data$Age_s)
  if (any(bad_age)) {
    stop(sprintf(
      "%d rows have non-finite Age or Age_s.",
      sum(bad_age)
    ))
  }
  
  family_name <- attr(model, "family_name")
  if (is.null(family_name)) {
    family_name <- as.character(model$family[1])
  }
  family_name <- trim_family(family_name)
  
  # Prediction data must contain every covariate used by the fitted mu model.
  # In the generic v5.x core these are centered variables (Sex_c, edu_c, eTIV_c),
  # not the legacy factor Sex alone.
  newdata <- data.frame(
    Age = data$Age,
    Age_s = data$Age_s
  )

  model_covars <- attr(model, "active_covariates")
  if (is.null(model_covars)) model_covars <- active_covariates(CONFIG)
  model_covars <- unique(as.character(model_covars))

  for (v in model_covars) {
    if (!v %in% names(data)) {
      stop(sprintf("Required prediction covariate '%s' is missing from data.", v))
    }
    newdata[[v]] <- data[[v]]
  }
  
  n <- nrow(newdata)
  
  # First attempt: predictAll.
  pred_all <- tryCatch(
    predictAll(model, newdata = newdata, type = "response"),
    error = function(e) {
      message(sprintf("predictAll failed: %s", conditionMessage(e)))
      NULL
    }
  )
  
  extract_from_pred_all <- function(what) {
    if (is.null(pred_all)) return(NULL)
    
    if (is.data.frame(pred_all) || is.list(pred_all)) {
      if (what %in% names(pred_all)) {
        x <- pred_all[[what]]
        if (is.matrix(x) || is.data.frame(x)) x <- x[, 1]
        return(as.numeric(x))
      }
    }
    
    if (is.matrix(pred_all) && what %in% colnames(pred_all)) {
      return(as.numeric(pred_all[, what]))
    }
    
    NULL
  }
  
  is_training_data <- function() {
    length(model$y) == nrow(data) &&
      all(
        abs(as.numeric(model$y) - as.numeric(data[[response]])) < 1e-6,
        na.rm = TRUE
      )
  }
  
  get_param <- function(what) {
    
    # 1) Use predictAll output.
    out <- extract_from_pred_all(what)
    if (!is.null(out) && length(out) == n) {
      return(out)
    }
    
    # 2) Use direct prediction.
    out <- tryCatch(
      predict(model, what = what, newdata = newdata, type = "response"),
      error = function(e) {
        message(sprintf("predict(%s) failed: %s", what, conditionMessage(e)))
        NULL
      }
    )
    
    if (!is.null(out)) {
      if (is.matrix(out) || is.data.frame(out)) out <- out[, 1]
      out <- as.numeric(out)
      if (length(out) == n) return(out)
    }
    
    # 3) Fallback for training data.
    if (is_training_data()) {
      out <- tryCatch(fitted(model, what = what), error = function(e) NULL)
      
      if (!is.null(out)) {
        if (is.matrix(out) || is.data.frame(out)) out <- out[, 1]
        out <- as.numeric(out)
        
        if (length(out) == n) {
          warning(sprintf(
            "Using fitted(%s) fallback. This is valid only for in-sample data.",
            what
          ))
          return(out)
        }
      }
    }
    
    stop(sprintf("Prediction failed for parameter: %s", what))
  }
  
  data$mu_expected <- get_param("mu")
  data$sigma_expected <- get_param("sigma")
  
  if (family_has_nu(family_name)) {
    data$nu_expected <- get_param("nu")
    if (any(!is.finite(data$nu_expected))) {
      stop("Invalid nu prediction")
    }
  } else {
    data$nu_expected <- NA_real_
  }
  
  if (family_has_tau(family_name)) {
    data$tau_expected <- get_param("tau")
    if (any(!is.finite(data$tau_expected))) {
      stop("Invalid tau prediction")
    }
  } else {
    data$tau_expected <- NA_real_
  }
  
  if (any(!is.finite(data$mu_expected))) {
    stop("Invalid mu prediction")
  }
  
  if (any(!is.finite(data$sigma_expected))) {
    stop("Invalid sigma prediction")
  }
  
  pfun <- make_pfun(
    family_name = family_name,
    mu = data$mu_expected,
    sigma = data$sigma_expected,
    nu = if (family_has_nu(family_name)) data$nu_expected else NULL,
    tau = if (family_has_tau(family_name)) data$tau_expected else NULL
  )
  
  p <- pfun(data[[response]])
  
  if (all(!is.finite(p))) {
    stop("CDF probability could not be computed.")
  }
  
  p[!is.finite(p)] <- NA_real_
  p <- pmin(pmax(p, 1e-10), 1 - 1e-10)
  
  data$Norm_Prob <- p
  data$Norm_Z <- qnorm(p)
  data$Norm_Centile <- 100 * p
  
  data
}

calculate_normative_z <- function(data, response) {
  if (!"Norm_Z" %in% names(data)) {
    if (!"Norm_Prob" %in% names(data)) {
      stop("Norm_Prob is missing. Run predict_individual_normative first.")
    }
    data$Norm_Z <- qnorm(data$Norm_Prob)
  }
  
  data
}

calculate_centile <- function(data) {
  if (!"Norm_Centile" %in% names(data)) {
    if (!"Norm_Prob" %in% names(data)) {
      stop("Norm_Prob is missing.")
    }
    data$Norm_Centile <- 100 * data$Norm_Prob
  }
  
  data
}

classify_normative_status <- function(data) {
  if (!"Norm_Centile" %in% names(data)) {
    stop("Norm_Centile is missing.")
  }
  
  # Centile equivalents for z values -2, -1.5, 1.5 and 2.
  z_breaks <- c(-2, -1.5, 1.5, 2)
  centile_breaks <- c(-Inf, pnorm(z_breaks) * 100, Inf)
  
  data$Deviation_Class <- cut(
    data$Norm_Centile,
    breaks = centile_breaks,
    labels = c(
      "Very Low",
      "Low",
      "Typical",
      "High",
      "Very High"
    )
  )
  
  data
}
# ============================================================================
# STAGE 4.9: BRAIN DEVIATION INDEX
# ============================================================================


calculate_BDI <- function(data) {
  # Norm_Z is already the canonical PIT-based normative score.
  # Keep this function only for backward compatibility; do not create
  # outcome-specific or duplicate normative-score aliases here.
  return(data)
}

##############################################################################
# STAGE 5: Turning-point detection (unchanged)
##############################################################################

# [legacy duplicate function removed in public release: find_extrema]
##############################################################################
# STAGE 6: Bootstrap (unchanged)
##############################################################################

# [legacy duplicate function removed in public release: bootstrap_one_iter]

# [legacy duplicate function removed in public release: bootstrap_model]
##############################################################################
# STAGE 7: Engine comparison (new)
##############################################################################

# ── GAM engine ──
fit_gam_engine <- function(data, bs = "tp", k = 5) {
  f <- as.formula(sprintf("FD ~ s(Age, bs='%s', k=%d)", bs, k))
  tryCatch(mgcv::gam(f, data = data, method = "REML"), error = function(e) NULL)
}

predict_gam_engine <- function(mod, age_grid) {
  nd <- data.frame(Age = age_grid)
  pred <- tryCatch(predict(mod, newdata = nd, type = "response"),
                   error = function(e) NULL)
  if (is.null(pred)) return(NULL)
  sig <- tryCatch(summary(mod)$scale, error = function(e) NA_real_)
  data.frame(Age = age_grid, central = as.numeric(pred),
             lo = as.numeric(pred) - 1.96 * sig,
             hi = as.numeric(pred) + 1.96 * sig,
             sigma = sig)
}

# ── FP/Poly engine ──
fit_fp_engine <- function(data, powers, type = "FP") {
  if (type == "FP") {
    terms <- build_fp("Age_s", powers)
    f <- as.formula(paste("FD ~", terms))
  } else {
    # Polynomial
    f <- as.formula(sprintf("FD ~ poly(Age_s, %d, raw=TRUE)", powers[1]))
  }
  tryCatch(lm(f, data = data), error = function(e) NULL)
}

predict_fp_engine <- function(mod, age_grid) {
  nd <- data.frame(Age = age_grid, Age_s = age_grid / 10)
  pred <- tryCatch(predict(mod, newdata = nd), error = function(e) NULL)
  if (is.null(pred)) return(NULL)
  sig <- tryCatch(summary(mod)$sigma, error = function(e) NA_real_)
  data.frame(Age = age_grid, central = as.numeric(pred),
             lo = as.numeric(pred) - 1.96 * sig,
             hi = as.numeric(pred) + 1.96 * sig,
             sigma = sig)
}

# ── CV helper ──
cv_engine <- function(data, fit_fn, predict_fn, n_folds = 10, seed = 1234) {
  set.seed(seed)
  folds <- sample(rep(1:n_folds, length.out = nrow(data)))
  obs_all <- numeric(0); pred_all <- numeric(0)
  
  for (f in 1:n_folds) {
    train <- data[folds != f, , drop = FALSE]
    test  <- data[folds == f, , drop = FALSE]
    mod <- tryCatch(fit_fn(train), error = function(e) NULL)
    if (is.null(mod)) next
    nd <- data.frame(Age = test$Age, Age_s = test$Age / 10)
    pred <- tryCatch(predict(mod, newdata = nd, type = "response"),
                     error = function(e) NULL)
    if (is.null(pred)) next
    obs_all  <- c(obs_all, test$FD)
    pred_all <- c(pred_all, as.numeric(pred))
  }
  
  if (length(obs_all) < 5) return(c(RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_))
  rmse <- sqrt(mean((obs_all - pred_all)^2))
  mae  <- mean(abs(obs_all - pred_all))
  ss_res <- sum((obs_all - pred_all)^2)
  ss_tot <- sum((obs_all - mean(obs_all))^2)
  r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  c(RMSE = rmse, MAE = mae, R2 = r2)
}

# Complete comparison-engine execution.
# [legacy duplicate function removed in public release: run_engine_comparison]

# [legacy duplicate function removed in public release: empty_engine_table]
##############################################################################
# STAGE 7: Residual diagnostics
##############################################################################

compute_quantile_residuals <- function(model) {
  family_name <- attr(model, "family_name")
  if (is.null(family_name)) family_name <- as.character(model$family[1])
  
  mu    <- fitted(model, "mu")
  sigma <- fitted(model, "sigma")
  
  nu <- NULL
  tau <- NULL
  
  if (family_has_nu(family_name)) {
    nu <- tryCatch(fitted(model, "nu"), error = function(e) NULL)
  }
  
  if (family_has_tau(family_name)) {
    tau <- tryCatch(fitted(model, "tau"), error = function(e) NULL)
  }
  
  pfun <- make_pfun(family_name, mu, sigma, nu, tau)
  
  p <- pfun(model$y)
  p <- pmin(pmax(p, 1e-10), 1 - 1e-10)
  
  qnorm(p)
}

residual_diagnostics <- function(resid, label = "") {
  resid <- resid[is.finite(resid)]
  
  if (length(resid) < 3) {
    return(list(
      shapiro  = list(statistic = NA_real_, p.value = NA_real_),
      ad       = list(statistic = NA_real_, p.value = NA_real_),
      residuals = resid
    ))
  }
  
  resid_shapiro <- resid
  if (length(resid_shapiro) > 5000) {
    set.seed(1)
    resid_shapiro <- sample(resid_shapiro, 5000)
  }
  
  sw <- tryCatch(shapiro.test(resid_shapiro),
                 error = function(e) list(statistic = NA_real_, p.value = NA_real_))
  
  ad <- tryCatch(nortest::ad.test(resid),
                 error = function(e) list(statistic = NA_real_, p.value = NA_real_))
  
  cat(sprintf(
    "  [%s] Residual normality: Shapiro p=%.4f | Anderson-Darling p=%.4f\n",
    label,
    sw$p.value,
    ad$p.value
  ))
  
  list(shapiro = sw, ad = ad, residuals = resid)
}

##############################################################################
# STAGE 7b: Prediction validation, calibration, PI coverage, stability
##############################################################################

# ---- helpers for CV metrics ----
cv_metrics <- function(obs, pred) {
  ok <- is.finite(obs) & is.finite(pred)
  obs <- obs[ok]; pred <- pred[ok]
  if (length(obs) < 5) {
    return(c(RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_))
  }
  rmse <- sqrt(mean((obs - pred)^2))
  mae  <- mean(abs(obs - pred))
  ss_res <- sum((obs - pred)^2)
  ss_tot <- sum((obs - mean(obs))^2)
  r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  c(RMSE = rmse, MAE = mae, R2 = r2)
}

fit_predict_fold <- function(train,
                             test,
                             family_name,
                             mu_powers,
                             sigma_powers) {
  
  mod <- tryCatch(
    fit_model(
      data         = train,
      family_name  = family_name,
      mu_powers    = mu_powers,
      sigma_powers = sigma_powers,
      include_sex  = FALSE,
      interaction  = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(mod) || !isTRUE(mod$converged)) {
    return(rep(NA_real_, nrow(test)))
  }
  
  age_out <- as.numeric(test$Age)
  
  age_unique <- sort(unique(age_out[is.finite(age_out)]))
  
  if (length(age_unique) == 0) {
    return(rep(NA_real_, nrow(test)))
  }
  
  curve <- tryCatch(
    predict_curve(
      model    = mod,
      age_grid = age_unique,
      sex_ref  = NULL
    ),
    error = function(e) NULL
  )
  
  if (is.null(curve)) {
    return(rep(NA_real_, nrow(test)))
  }
  
  if (all(!is.finite(curve$central))) {
    return(rep(NA_real_, nrow(test)))
  }
  
  ord <- order(curve$Age)
  
  pred <- tryCatch(
    approx(
      x     = curve$Age[ord],
      y     = curve$central[ord],
      xout  = age_out,
      rule  = 2
    )$y,
    error = function(e) rep(NA_real_, length(age_out))
  )
  
  pred
}

# 10-fold CV (and optional repeated CV)
run_cv_validation <- function(data, family_name, mu_powers, sigma_powers,
                              n_folds = 10, n_repeats = 1, label = "") {
  cat(sprintf("  [%s] %d-fold CV (repeats=%d)...\n", label, n_folds, n_repeats))
  
  n <- nrow(data)
  if (n < n_folds * 3) {
    warning(sprintf("[%s] Sample too small for %d-fold CV.", label, n_folds))
    return(list(
      single = c(RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_),
      repeated = NULL,
      n_folds = n_folds,
      n_repeats = n_repeats
    ))
  }
  
  all_metrics <- list()
  
  for (rep in seq_len(n_repeats)) {
    set.seed(CONFIG$SEED + rep)
    folds <- sample(rep(seq_len(n_folds), length.out = n))
    
    obs_all  <- numeric(0)
    pred_all <- numeric(0)
    
    for (f in seq_len(n_folds)) {
      test_idx  <- which(folds == f)
      train_idx <- setdiff(seq_len(n), test_idx)
      
      pred_f <- fit_predict_fold(
        train        = data[train_idx, , drop = FALSE],
        test         = data[test_idx,  , drop = FALSE],
        family_name  = family_name,
        mu_powers    = mu_powers,
        sigma_powers = sigma_powers
      )
      
      obs_all  <- c(obs_all,  data$FD[test_idx])
      pred_all <- c(pred_all, pred_f)
    }
    
    all_metrics[[rep]] <- cv_metrics(obs_all, pred_all)
  }
  
  mat <- do.call(rbind, all_metrics)
  
  single <- if (n_repeats >= 1) colMeans(mat, na.rm = TRUE) else mat[1, ]
  
  repeated_summary <- NULL
  if (n_repeats > 1) {
    repeated_summary <- data.frame(
      Metric = c("RMSE", "MAE", "R2"),
      Mean   = colMeans(mat, na.rm = TRUE),
      SD     = apply(mat, 2, sd, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
  
  cat(sprintf(
    "  [%s] CV RMSE=%.4f | MAE=%.4f | R2=%.4f\n",
    label, single["RMSE"], single["MAE"], single["R2"]
  ))
  
  list(
    single   = single,
    repeated = repeated_summary,
    n_folds  = n_folds,
    n_repeats = n_repeats,
    all_reps = mat
  )
}

# Bootstrap optimism correction (Harrell-style)
bootstrap_optimism <- function(data, family_name, mu_powers, sigma_powers,
                               n_boot = 100, label = "", mod_full = NULL) {
  cat(sprintf("  [%s] Bootstrap optimism correction (%d)...\n", label, n_boot))
  if (is.null(mod_full)) mod_full <- fit_model(data, family_name, mu_powers, sigma_powers, maxit = 5000)
  NA_LIST <- list(apparent_RMSE = NA_real_, apparent_MAE = NA_real_, apparent_R2 = NA_real_,
                  mean_optimism = NA_real_, corrected_RMSE = NA_real_, n_boot_ok = 0)
  if (is.null(mod_full)) { cat(sprintf("  [%s] full model not converged -> NA\n", label)); return(NA_LIST) }
  grid <- sort(unique(data$Age))
  curve_full <- tryCatch(predict_curve(mod_full, grid), error = function(e) NULL)
  if (is.null(curve_full)) return(NA_LIST)
  apparent <- cv_metrics(data$FD, approx(curve_full$Age, curve_full$central, xout = data$Age, rule = 2)$y)
  opt_rmse <- numeric(0)
  for (b in seq_len(n_boot)) {
    idx <- sample(seq_len(nrow(data)), replace = TRUE); bd <- data[idx, , drop = FALSE]
    mb <- fit_model(bd, family_name, mu_powers, sigma_powers, maxit = 5000)
    if (is.null(mb)) next
    cb <- tryCatch(predict_curve(mb, grid), error = function(e) NULL)
    if (is.null(cb)) next
    tr <- cv_metrics(bd$FD,  approx(cb$Age, cb$central, xout = bd$Age,  rule = 2)$y)
    te <- cv_metrics(data$FD, approx(cb$Age, cb$central, xout = data$Age, rule = 2)$y)
    if (is.finite(tr["RMSE"]) && is.finite(te["RMSE"])) opt_rmse <- c(opt_rmse, tr["RMSE"] - te["RMSE"])
  }
  mean_opt  <- if (length(opt_rmse) > 10) mean(opt_rmse) else NA_real_
  corrected <- apparent["RMSE"] - mean_opt
  cat(sprintf("  [%s] Apparent RMSE=%.4f | optimism=%.4f | corrected=%.4f (ok=%d)\n",
              label, apparent["RMSE"], mean_opt, corrected, length(opt_rmse)))
  list(apparent_RMSE = unname(apparent["RMSE"]), apparent_MAE = unname(apparent["MAE"]),
       apparent_R2 = unname(apparent["R2"]), mean_optimism = mean_opt,
       corrected_RMSE = unname(corrected), n_boot_ok = length(opt_rmse))
}

# Calibration: Observed vs Predicted (median)
calibration_metrics <- function(model, data, label = "") {
  cat(sprintf("  [%s] Calibration (Observed vs Predicted)...\n", label))
  grid  <- sort(unique(data$Age))
  curve <- tryCatch(predict_curve(model, grid),
                    error = function(e) { cat(sprintf("  [%s] curve error: %s\n", label, conditionMessage(e))); NULL })
  if (is.null(curve)) return(list(slope = NA_real_, intercept = NA_real_, R2 = NA_real_, plot_data = NULL))
  pred <- approx(curve$Age, curve$central, xout = data$Age, rule = 2)$y
  obs  <- data$FD; ok <- is.finite(obs) & is.finite(pred); obs <- obs[ok]; pred <- pred[ok]
  if (length(obs) < 10) return(list(slope = NA_real_, intercept = NA_real_, R2 = NA_real_, plot_data = NULL))
  fit_cal <- lm(obs ~ pred)
  slope <- unname(coef(fit_cal)[2]); intercept <- unname(coef(fit_cal)[1]); r2 <- summary(fit_cal)$r.squared
  cat(sprintf("  [%s] Calibration: slope=%.4f | intercept=%.4f | R2=%.4f\n", label, slope, intercept, r2))
  list(slope = slope, intercept = intercept, R2 = r2, plot_data = data.frame(Observed = obs, Predicted = pred))
}



# 95% Prediction Interval coverage
pi_coverage <- function(model, data, label = "") {
  cat(sprintf("  [%s] 95%% PI coverage...\n", label))
  grid  <- sort(unique(data$Age))
  curve <- tryCatch(predict_curve(model, grid), error = function(e) NULL)
  if (is.null(curve)) return(list(coverage = NA_real_, expected = 0.95, n = 0))
  lo <- approx(curve$Age, curve$lo, xout = data$Age, rule = 2)$y
  hi <- approx(curve$Age, curve$hi, xout = data$Age, rule = 2)$y
  y  <- data$FD; ok <- is.finite(y) & is.finite(lo) & is.finite(hi); y <- y[ok]; lo <- lo[ok]; hi <- hi[ok]
  if (length(y) < 10) return(list(coverage = NA_real_, expected = 0.95, n = length(y)))
  covered <- mean(y >= lo & y <= hi)
  cat(sprintf("  [%s] 95%% PI coverage = %.1f%% (n=%d)\n", label, 100*covered, length(y)))
  list(coverage = covered, expected = 0.95, n = length(y))
}
pi_coverage_cv <- function(data,
                           family_name,
                           mu_powers,
                           sigma_powers,
                           n_folds = 10,
                           seed = 1234,
                           label = "") {
  cat(sprintf("  [%s] Cross-validated 95%% PI coverage...\n", label))
  
  n <- nrow(data)
  
  if (n < n_folds * 3) {
    return(list(coverage = NA_real_, expected = 0.95, n = 0))
  }
  
  set.seed(seed)
  folds <- sample(rep(seq_len(n_folds), length.out = n))
  
  covered <- logical(0)
  
  for (f in seq_len(n_folds)) {
    test_idx <- which(folds == f)
    
    train <- data[-test_idx, , drop = FALSE]
    test  <- data[test_idx, , drop = FALSE]
    
    mod <- tryCatch(
      fit_model(
        data = train,
        family_name = family_name,
        mu_powers = mu_powers,
        sigma_powers = sigma_powers,
        include_sex = FALSE,
        interaction = FALSE
      ),
      error = function(e) NULL
    )
    
    if (is.null(mod) || !isTRUE(mod$converged)) next
    
    age_unique <- sort(unique(test$Age[is.finite(test$Age)]))
    
    if (length(age_unique) == 0) next
    
    curve <- tryCatch(
      predict_curve(mod, age_unique),
      error = function(e) NULL
    )
    
    if (is.null(curve)) next
    
    lo <- approx(curve$Age, curve$lo, xout = test$Age, rule = 2)$y
    hi <- approx(curve$Age, curve$hi, xout = test$Age, rule = 2)$y
    
    ok <- is.finite(test$FD) & is.finite(lo) & is.finite(hi)
    
    covered <- c(
      covered,
      test$FD[ok] >= lo[ok] & test$FD[ok] <= hi[ok]
    )
  }
  
  if (length(covered) < 10) {
    return(list(coverage = NA_real_, expected = 0.95, n = length(covered)))
  }
  
  coverage <- mean(covered)
  
  cat(sprintf(
    "  [%s] CV 95%% PI coverage = %.1f%% (n=%d)\n",
    label, 100 * coverage, length(covered)
  ))
  
  list(
    coverage = coverage,
    expected = 0.95,
    n = length(covered)
  )
}

# Model stability of Peak Age across bootstrap (already computed in boot_mat)
# [legacy duplicate function removed in public release: peak_stability]
# [legacy duplicate function removed in public release: run_full_validation]
##############################################################################
# STAGE 8: Summary tables
##############################################################################

get_val <- function(x) {
  if (is.null(x) || length(x) == 0) return("NA")
  if (is.na(x)) return("NA")
  if (is.character(x)) return(x)
  as.character(round(as.numeric(x), 4))
}

get_ci <- function(boot_obj, row_name) {
  if (is.null(boot_obj) || is.null(boot_obj$stats)) return("[NA, NA]")
  if (!row_name %in% rownames(boot_obj$stats)) return("[NA, NA]")
  
  lo <- boot_obj$stats[row_name, "lower"]
  hi <- boot_obj$stats[row_name, "upper"]
  
  if (is.na(lo) || is.na(hi)) return("[NA, NA]")
  
  sprintf("[%.1f, %.1f]", lo, hi)
}

# [legacy duplicate function removed in public release: boundary_note]

# [legacy duplicate function removed in public release: build_summary_table]

# [legacy duplicate function removed in public release: build_bootstrap_detail]

##############################################################################
# STAGE 9: Sex difference analysis
##############################################################################

# [legacy duplicate function removed in public release: analyze_sex_differences]

##############################################################################
# STAGE 10: Visualization
##############################################################################

create_plots <- function(df, results, cfg) {
  cat("[9/9] Creating plots...\n")
  
  COL_F   <- "#E74C3C"
  COL_M   <- "#2980B9"
  COL_ALL <- "#2C3E50"
  
  COL_F_BAND <- "#F5B7B1"
  COL_M_BAND <- "#AED6F1"
  
  col_values <- c(Female = COL_F, Male = COL_M, All = COL_ALL)
  
  theme_main <- theme_classic(base_size = 9) +
    theme(
      plot.title      = element_text(hjust = 0, size = 9, margin = margin(b = 4)),
      axis.title      = element_text(size = 8),
      axis.text       = element_text(size = 7, color = "black"),
      legend.title    = element_blank(),
      legend.text     = element_text(size = 6.5),
      legend.background = element_blank(),
      legend.key      = element_blank(),
      plot.tag        = element_text(face = "bold", size = 12)
    )
  
  xlim_age <- c(
    floor(min(df$Age, na.rm = TRUE)) - 1,
    ceiling(max(df$Age, na.rm = TRUE)) + 1
  )
  
  build_curve_df <- function(r, group_name) {
    data.frame(
      Age      = r$Age,
      Group    = group_name,
      central  = r$central,
      lo       = r$lo,
      hi       = r$hi,
      sigma    = r$sigma,
      dcentral = r$dcentral,
      stringsAsFactors = FALSE
    )
  }
  
  curve_df <- rbind(
    build_curve_df(results$Female, "Female"),
    build_curve_df(results$Male,   "Male"),
    build_curve_df(results$All,    "All")
  )
  
  curve_df$Group <- factor(curve_df$Group, levels = c("Female", "Male", "All"))
  
  # ---------------- Panel A ----------------
  p_a <- ggplot(df, aes(Age, FD, color = Sex)) +
    geom_point(alpha = 0.40, size = 1.6, stroke = 0) +
    scale_color_manual(values = c(Female = COL_F, Male = COL_M)) +
    labs(
      title = "Raw data",
      x = "Age (years)",
      y = cfg$Y_LABEL,
      tag = "a"
    ) +
    coord_cartesian(xlim = xlim_age) +
    theme_main +
    theme(
      legend.position = c(0.99, 0.99),
      legend.justification = c(1, 1),
      legend.key.size = unit(0.35, "cm")
    )
  
  # ---------------- Panel B ----------------
  # Use raw data range to prevent implausible centiles from stretching the y-axis.
  raw_vals <- df$FD[is.finite(df$FD)]
  
  if (length(raw_vals) < 5) {
    ylim_b <- c(0, 1)
  } else {
    raw_range <- range(raw_vals)
    pad <- 0.12 * diff(raw_range)
    ylim_b <- raw_range + c(-pad, pad)
  }
  make_pts_b <- function(r, g) {
    
    ex <- r$extrema
    
    out <- data.frame(
      Age   = numeric(0),
      y     = numeric(0),
      Group = character(0),
      kind  = character(0),
      lab   = character(0),
      vjust = numeric(0),
      stringsAsFactors = FALSE
    )
    
    # Only show internal maxima as "Peak".
    if (!isTRUE(ex$boundary_max) && is.finite(ex$age_max) && is.finite(ex$value_max)) {
      out <- rbind(out, data.frame(
        Age   = ex$age_max,
        y     = ex$value_max,
        Group = g,
        kind  = "Peak (max)",
        lab   = paste0(round(ex$age_max), "y"),
        vjust = -0.8,
        stringsAsFactors = FALSE
      ))
    }
    
    # Only show internal minima as "Trough".
    if (!isTRUE(ex$boundary_min) && is.finite(ex$age_min) && is.finite(ex$value_min)) {
      out <- rbind(out, data.frame(
        Age   = ex$age_min,
        y     = ex$value_min,
        Group = g,
        kind  = "Trough (min)",
        lab   = paste0(round(ex$age_min), "y"),
        vjust = 1.7,
        stringsAsFactors = FALSE
      ))
    }
    
    out
  }
  
  pts_b <- rbind(
    make_pts_b(results$Female, "Female"),
    make_pts_b(results$Male,   "Male"),
    make_pts_b(results$All,    "All")
  )
  
  pts_b$Group <- factor(pts_b$Group, levels = c("Female", "Male", "All"))
  pts_b$kind  <- factor(pts_b$kind, levels = c("Peak (max)", "Trough (min)"))
  
  p_b <- ggplot() +
    geom_point(
      data = df,
      aes(Age, FD),
      color = "grey50",
      alpha = 0.14,
      size = 0.85
    ) +
    geom_ribbon(
      data = subset(curve_df, Group == "Female"),
      aes(Age, ymin = lo, ymax = hi),
      fill = COL_F_BAND,
      alpha = 0.45
    ) +
    geom_ribbon(
      data = subset(curve_df, Group == "Male"),
      aes(Age, ymin = lo, ymax = hi),
      fill = COL_M_BAND,
      alpha = 0.45
    ) +
    geom_line(
      data = subset(curve_df, Group %in% c("Female", "Male")),
      aes(Age, lo, color = Group),
      linewidth = 0.35,
      linetype = "dotted",
      show.legend = FALSE
    ) +
    geom_line(
      data = subset(curve_df, Group %in% c("Female", "Male")),
      aes(Age, hi, color = Group),
      linewidth = 0.35,
      linetype = "dotted",
      show.legend = FALSE
    ) +
    geom_line(
      data = curve_df,
      aes(Age, central, color = Group),
      linewidth = 0.95
    ) +
    geom_point(
      data = pts_b,
      aes(Age, y, color = Group, shape = kind),
      size = 2.5
    ) +
    geom_text(
      data = pts_b,
      aes(Age, y, label = lab, color = Group, vjust = vjust),
      size = 2.2,
      hjust = -0.12,
      show.legend = FALSE
    ) +
    scale_color_manual(values = col_values) +
    scale_shape_manual(values = c("Peak (max)" = 16, "Trough (min)" = 15)) +
    guides(
      color = guide_legend(order = 1, override.aes = list(shape = NA, linewidth = 1.2)),
      shape = guide_legend(order = 2, override.aes = list(color = "grey30", size = 2.4))
    ) +
    labs(
      title = "Normative trajectories (2.5/50/97.5 centiles)",
      subtitle = "* Extremum lies at the age boundary and should not be interpreted as an internal turning point.",
      x = "Age (years)",
      y = cfg$Y_LABEL,
      tag = "b"
    ) +
    coord_cartesian(xlim = xlim_age, ylim = ylim_b) +
    theme_main +
    theme(
      legend.position = "right",
      legend.box = "vertical",
      legend.key.size = unit(0.35, "cm")
    )
  
  # ---------------- Panel C ----------------
  # ---------------- Panel C ----------------
  
  sigma_df <- subset(
    curve_df,
    Group %in% c("Female", "Male")
  )
  sigma_peak <- do.call(
    rbind,
    lapply(
      split(sigma_df, sigma_df$Group),
      function(x){
        x[which.max(x$sigma), ]
      }
    )
  )
  
  p_c <- ggplot(
    sigma_df,
    aes(
      Age,
      sigma,
      color = Group
    )
  ) +
    
    geom_line(
      linewidth = 1
    ) +
    
    geom_point(
      data = sigma_peak,
      aes(
        Age,
        sigma,
        color = Group
      ),
      size = 2.5
    ) +
    
    scale_color_manual(
      values = c(
        Female = COL_F,
        Male   = COL_M
      )
    ) +
    
    labs(
      title = "Normative variability across age",
      subtitle = "Age-dependent change in scale parameter",
      x = "Age (years)",
      y = expression(
        "Variability (" * sigma * ")"
      ),
      tag = "c"
    ) +
    
    coord_cartesian(
      xlim = xlim_age,
      ylim = c(0, NA)
    ) +
    
    theme_main
  # ---------------- Panel D ----------------
  make_pts_d <- function(r, g) {
    ex <- r$extrema
    
    if (!is.finite(ex$age_vel) || !is.finite(ex$d_vel)) return(NULL)
    
    data.frame(
      Age      = ex$age_vel,
      dcentral = ex$d_vel,
      Group    = g,
      kind     = "Maximal age-related change",
      lab      = paste0(round(ex$age_vel), ifelse(isTRUE(ex$boundary_vel), "y*", "y")),
      stringsAsFactors = FALSE
    )
  }
  
  pts_d <- rbind(
    make_pts_d(results$Female, "Female"),
    make_pts_d(results$Male,   "Male"),
    make_pts_d(results$All,    "All")
  )
  
  if (!is.null(pts_d) && nrow(pts_d) > 0) {
    pts_d$Group <- factor(pts_d$Group, levels = c("Female", "Male", "All"))
  }
  
  p_d <- ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.5, color = "grey70") +
    geom_line(
      data = curve_df,
      aes(Age, dcentral, color = Group),
      linewidth = 0.95
    ) +
    {
      if (!is.null(pts_d) && nrow(pts_d) > 0) {
        list(
          geom_point(
            data = pts_d,
            aes(Age, dcentral, color = Group, shape = kind),
            size = 2.6
          ),
          geom_text(
            data = pts_d,
            aes(Age, dcentral, label = lab, color = Group),
            size = 2.2,
            hjust = -0.12,
            vjust = -0.7,
            show.legend = FALSE
          )
        )
      }
    } +
    scale_color_manual(values = col_values) +
    scale_shape_manual(values = c("Maximal age-related change" = 17)) +
    guides(
      color = guide_legend(order = 1, override.aes = list(shape = NA, linewidth = 1.2)),
      shape = guide_legend(order = 2, override.aes = list(color = "grey30", size = 2.4))
    ) +
    labs(
      title = "Cross-sectional age gradient",
      subtitle = "Derivative of median centile. * Edge of reliable interior search window.",
      x = "Age (years)",
      y = expression("Age gradient (" * d * "Median" / dAge * ")"),
      tag = "d"
    ) +
    coord_cartesian(xlim = xlim_age) +
    theme_main +
    theme(
      legend.position = "right",
      legend.justification = "top",
      legend.key.size = unit(0.35, "cm")
    )
  
  # ---------------- Panel E ----------------
  df_e <- rbind(
    data.frame(deviation = results$Female$resid, Group = "Female"),
    data.frame(deviation = results$Male$resid,   Group = "Male"),
    data.frame(deviation = results$All$resid,    Group = "All")
  )
  
  df_e$Group <- factor(df_e$Group, levels = c("Female", "Male", "All"))
  
  p_e <- ggplot(df_e, aes(deviation, color = Group, fill = Group)) +
    geom_density(alpha = 0.25, linewidth = 0.8) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey40") +
    scale_color_manual(values = col_values) +
    scale_fill_manual(values = col_values) +
    guides(
      color = "none",
      fill  = guide_legend(override.aes = list(alpha = 0.35, linewidth = 0.8))
    ) +
    labs(
      title = "Quantile residuals",
      x = "Residual (z)",
      y = "Density",
      tag = "e"
    ) +
    theme_main +
    theme(
      legend.position = c(0.99, 0.99),
      legend.justification = c(1, 1),
      legend.key.size = unit(0.35, "cm")
    )
  
  # ---------------- Panel F ----------------
  summary_tbl <- results$summary_table
  
  keep_cols <- c(
    "Group", "Family", "Peak Age", "Peak Age CI",
    "Trough Age", "Trough Age CI",
    "Maximal Change Age", "Boundary note"
  )
  
  tbl_plot <- summary_tbl[, intersect(keep_cols, names(summary_tbl)), drop = FALSE]
  
  tbl_grob <- tableGrob(
    tbl_plot,
    rows = NULL,
    theme = ttheme_minimal(
      base_size = 6,
      core = list(
        bg_params = list(
          fill  = rep(c("#F5B0BC", "#A8E0D9", "#F0F0F0"), length.out = nrow(tbl_plot)),
          alpha = 0.55
        )
      ),
      colhead = list(
        fg_params = list(fontface = "bold", col = "grey20", cex = 0.8),
        bg_params = list(fill = "#E0E0E0", alpha = 0.8)
      )
    )
  )
  
  p_f <- ggplot() +
    annotation_custom(tbl_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    theme_void() +
    labs(tag = "f") +
    theme(
      plot.tag = element_text(face = "bold", size = 12),
      plot.tag.position = c(0.01, 0.99),
      plot.margin = margin(5, 5, 5, 5)
    )
  # ---------------- Panel G ----------------
  # Individual Normative Deviation
  
  # ---------------- Panel G ----------------
  
  norm_F_plot <- results$normative_deviation$Female
  norm_M_plot <- results$normative_deviation$Male
  
  norm_plot <- rbind(
    norm_F_plot,
    norm_M_plot
  )
  
  norm_plot$Sex <- factor(
    norm_plot$Sex,
    levels = c("Female", "Male")
  )
  
  p_g <- ggplot(
    norm_plot,
    aes(
      x = Age,
      y = Norm_Z,
      color = Sex
    )
  ) +
    
    geom_hline(
      yintercept = 0,
      linewidth = 0.45,
      color = "grey35"
    ) +
    
    geom_hline(
      yintercept = c(-2, 2),
      linewidth = 0.45,
      linetype = "dashed",
      color = "grey55"
    ) +
    
    geom_point(
      alpha = 0.45,
      size = 1.4
    ) +
    
    scale_color_manual(
      values = c(
        Female = COL_F,
        Male   = COL_M
      )
    ) +
    
    labs(
      title = "Individual normative deviation",
      subtitle = "Sex-specific normative z-scores",
      x = "Age (years)",
      y = "Normative deviation (z)",
      tag = "g"
    ) +
    
    coord_cartesian(
      xlim = xlim_age
    ) +
    
    theme_main +
    
    theme(
      legend.position = "right",
      legend.justification = "top",
      legend.key.size = unit(0.35, "cm")
    )
  # ============================================================
  # Assemble final multi-panel figure
  # ============================================================
  
  final_plot <- 
    p_a /
    (p_b | p_c) /
    (p_d | p_e) /
    p_g /
    p_f +
    patchwork::plot_layout(
      heights = c(
        1.0,
        1.2,
        1.0,
        1.0,
        0.75
      )
    )
  
  return(
    list(
      plot = final_plot,
      curve_df = curve_df
    )
  )
}

##############################################################################
# STAGE 11: Save results
##############################################################################

# [legacy duplicate function removed in public release: save_results]
##############################################################################
# Multi-engine comparison helpers
# Engine A: GAMLSS primary normative model
# Engine B: GAM / spline comparison
# Engine C: FP / polynomial comparison
##############################################################################

empty_engine_table <- function() {
  data.frame(
    Group     = character(),
    Engine    = character(),
    Framework = character(),
    Level     = character(),
    Family    = character(),
    Spec      = character(),
    AIC       = numeric(),
    BIC       = numeric(),
    GAIC      = numeric(),
    CV_RMSE   = numeric(),
    CV_MAE    = numeric(),
    CV_R2     = numeric(),
    stringsAsFactors = FALSE
  )
}

engine_row <- function(group,
                       engine,
                       framework,
                       level,
                       family,
                       spec,
                       aic,
                       bic,
                       gaic,
                       cv) {
  
  if (is.null(cv)) {
    cv <- c(RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_)
  }
  
  data.frame(
    Group     = group,
    Engine    = engine,
    Framework = framework,
    Level     = level,
    Family    = family,
    Spec      = spec,
    AIC       = aic,
    BIC       = bic,
    GAIC      = gaic,
    CV_RMSE   = unname(cv["RMSE"]),
    CV_MAE    = unname(cv["MAE"]),
    CV_R2     = unname(cv["R2"]),
    stringsAsFactors = FALSE
  )
}

make_cv_folds <- function(n, n_folds, seed) {
  n_folds <- max(2, min(n_folds, floor(n / 3)))
  set.seed(seed)
  sample(rep(seq_len(n_folds), length.out = n))
}

##############################################################################
# GAMLSS selected-model CV using the same folds as comparison engines
##############################################################################

cv_gamlss_selected <- function(data,
                               sel,
                               cfg,
                               folds) {
  
  obs_all  <- numeric(0)
  pred_all <- numeric(0)
  
  ok_folds <- 0
  
  for (f in sort(unique(folds))) {
    
    test_idx  <- which(folds == f)
    train_idx <- setdiff(seq_len(nrow(data)), test_idx)
    
    pred_f <- tryCatch(
      fit_predict_fold(
        train        = data[train_idx, , drop = FALSE],
        test         = data[test_idx,  , drop = FALSE],
        family_name  = sel$best_family,
        mu_powers    = sel$best_mu_powers,
        sigma_powers = sel$best_sigma_powers
      ),
      error = function(e) {
        warning(sprintf(
          "CV fold %s failed for GAMLSS: %s",
          f,
          conditionMessage(e)
        ))
        rep(NA_real_, length(test_idx))
      }
    )
    
    if (any(is.finite(pred_f))) {
      ok_folds <- ok_folds + 1
    }
    
    obs_all  <- c(obs_all,  data$FD[test_idx])
    pred_all <- c(pred_all, pred_f)
  }
  
  if (ok_folds == 0) {
    warning("All CV folds failed for GAMLSS selected model.")
    metrics <- c(RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_)
  } else {
    metrics <- cv_metrics(obs_all, pred_all)
  }
  
  list(
    single    = metrics,
    repeated  = NULL,
    n_folds   = length(unique(folds)),
    n_repeats = 1,
    all_reps  = matrix(metrics, nrow = 1)
  )
}
##############################################################################
# Specification builders
##############################################################################

build_poly_specs <- function(cfg) {
  lapply(cfg$POLY_DEGREES, function(d) {
    list(
      engine    = "POLY",
      framework = "Polynomial",
      name      = paste0("Poly", d),
      formula   = as.formula(sprintf(
        "FD ~ poly(Age_s, %d, raw = TRUE)",
        d
      ))
    )
  })
}

build_fp_lm_specs <- function(cfg) {
  
  powers_set <- cfg$FP_POWERS
  out <- list()
  
  # FP1
  for (p in powers_set) {
    terms <- build_fp("Age_s", p)
    out[[length(out) + 1]] <- list(
      engine    = "FP",
      framework = "FractionalPolynomial",
      name      = sprintf("FP1(%s)", p),
      formula   = as.formula(paste("FD ~", terms))
    )
  }
  
  # FP2
  n_p <- length(powers_set)
  
  for (i in seq_len(n_p)) {
    for (j in i:n_p) {
      
      p1 <- powers_set[i]
      p2 <- powers_set[j]
      
      terms <- build_fp("Age_s", c(p1, p2))
      
      out[[length(out) + 1]] <- list(
        engine    = "FP",
        framework = "FractionalPolynomial",
        name      = sprintf("FP2(%s,%s)", p1, p2),
        formula   = as.formula(paste("FD ~", terms))
      )
    }
  }
  
  out
}

build_gam_specs <- function(cfg) {
  
  out <- list()
  
  for (bs in cfg$GAM_BS) {
    for (k in cfg$GAM_K) {
      
      k <- as.integer(k)
      
      out[[length(out) + 1]] <- list(
        engine    = "GAM",
        framework = "GAM_Spline",
        name      = sprintf("GAM_%s_k%d", bs, k),
        bs        = bs,
        k         = k
      )
    }
  }
  
  out
}

##############################################################################
# Engine fitting
##############################################################################

fit_engine <- function(data, spec, cfg) {
  
  # ------------------------------------------------------------------
  # GAM / spline engine
  # ------------------------------------------------------------------
  if (spec$engine == "GAM") {
    
    u <- length(unique(data$Age))
    
    if (u < 5) {
      return(NULL)
    }
    
    k_candidates <- unique(c(
      min(spec$k, u - 1),
      max(4, min(spec$k, u - 1)),
      4
    ))
    
    for (kk in k_candidates) {
      
      if (kk >= u) next
      
      f <- as.formula(sprintf(
        "FD ~ s(Age, bs = '%s', k = %d)",
        spec$bs,
        kk
      ))
      
      mod <- tryCatch(
        mgcv::gam(f, data = data, method = "REML"),
        error = function(e) NULL
      )
      
      if (!is.null(mod) && (is.null(mod$converged) || isTRUE(mod$converged))) {
        return(mod)
      }
    }
    
    return(NULL)
  }
  
  # ------------------------------------------------------------------
  # FP / Poly engine
  # ------------------------------------------------------------------
  if (spec$engine == "FP") {
    
    mod <- tryCatch(
      lm(spec$formula, data = data),
      error = function(e) NULL
    )
    
    if (is.null(mod)) return(NULL)
    
    cf <- tryCatch(coef(mod), error = function(e) NULL)
    
    if (is.null(cf) || any(!is.finite(cf))) {
      return(NULL)
    }
    
    return(mod)
  }
  
  NULL
}

##############################################################################
# Engine prediction curve
##############################################################################

predict_engine_curve <- function(mod, spec, age_grid, train_data = NULL) {
  
  new_data <- data.frame(
    Age   = age_grid,
    Age_s = age_grid / 10
  )
  
  mu <- tryCatch(
    as.numeric(predict(mod, newdata = new_data, type = "response")),
    error = function(e) NULL
  )
  
  if (is.null(mu) || length(mu) != length(age_grid)) {
    return(data.frame(
      Age     = age_grid,
      central = NA_real_,
      lo      = NA_real_,
      hi      = NA_real_,
      sigma   = NA_real_
    ))
  }
  
  sig <- tryCatch(sigma(mod), error = function(e) NULL)
  
  if (is.null(sig) || !is.finite(sig)) {
    sig <- tryCatch(sqrt(summary(mod)$scale), error = function(e) NA_real_)
  }
  
  if (is.null(sig) || !is.finite(sig)) {
    sig <- sd(residuals(mod), na.rm = TRUE)
  }
  
  z <- qnorm(0.975)
  
  data.frame(
    Age     = age_grid,
    central = mu,
    lo      = mu - z * sig,
    hi      = mu + z * sig,
    sigma   = sig
  )
}

##############################################################################
# Engine CV
##############################################################################

cv_engine_spec <- function(data, spec, cfg, folds) {
  
  obs_all  <- numeric(0)
  pred_all <- numeric(0)
  
  for (f in sort(unique(folds))) {
    
    test_idx  <- which(folds == f)
    train_idx <- setdiff(seq_len(nrow(data)), test_idx)
    
    train <- data[train_idx, , drop = FALSE]
    test  <- data[test_idx,  , drop = FALSE]
    
    mod <- fit_engine(train, spec, cfg)
    
    if (is.null(mod)) next
    
    pred <- tryCatch(
      as.numeric(predict(mod, newdata = test, type = "response")),
      error = function(e) NULL
    )
    
    if (is.null(pred)) {
      nd <- data.frame(
        Age   = test$Age,
        Age_s = test$Age / 10
      )
      
      pred <- tryCatch(
        as.numeric(predict(mod, newdata = nd, type = "response")),
        error = function(e) rep(NA_real_, nrow(test))
      )
    }
    
    obs_all  <- c(obs_all,  test$FD)
    pred_all <- c(pred_all, pred)
  }
  
  cv_metrics(obs_all, pred_all)
}

##############################################################################
# Run comparison engines
##############################################################################

run_comparison_engines <- function(data,
                                   label,
                                   cfg,
                                   age_grid,
                                   folds = NULL) {
  
  if (is.null(folds)) {
    folds <- make_cv_folds(
      n = nrow(data),
      n_folds = cfg$ENGINE_CV_FOLDS,
      seed = cfg$SEED
    )
  }
  
  rows <- list()
  models <- list()
  
  n_obs <- nrow(data)
  
  cv_none <- c(
    RMSE = NA_real_,
    MAE  = NA_real_,
    R2   = NA_real_
  )
  
  # ------------------------------------------------------------
  # Engine B: GAM / spline
  # ------------------------------------------------------------
  for (spec in build_gam_specs(cfg)) {
    
    mod <- fit_engine(data, spec, cfg)
    
    if (is.null(mod)) next
    
    cvm <- cv_none
    
    if (isTRUE(cfg$RUN_ENGINE_CV)) {
      cvm <- tryCatch(
        cv_engine_spec(data, spec, cfg, folds),
        error = function(e) cv_none
      )
    }
    
    aic  <- extract_gaic(mod, k = 2, n_obs = n_obs)
    bic  <- extract_gaic(mod, k = log(n_obs), n_obs = n_obs)
    gaic <- extract_gaic(mod, k = 3, n_obs = n_obs)
    
    rows[[length(rows) + 1]] <- engine_row(
      group     = label,
      engine    = "GAM",
      framework = spec$framework,
      level     = NA_character_,
      family    = "Gaussian",
      spec      = spec$name,
      aic       = aic,
      bic       = bic,
      gaic      = gaic,
      cv        = cvm
    )
    
    models[[spec$name]] <- mod
  }
  
  # ------------------------------------------------------------
  # Engine C: FP / Poly
  # ------------------------------------------------------------
  fp_specs <- c(
    build_poly_specs(cfg),
    build_fp_lm_specs(cfg)
  )
  
  for (spec in fp_specs) {
    
    mod <- fit_engine(data, spec, cfg)
    
    if (is.null(mod)) next
    
    cvm <- cv_none
    
    if (isTRUE(cfg$RUN_ENGINE_CV)) {
      cvm <- tryCatch(
        cv_engine_spec(data, spec, cfg, folds),
        error = function(e) cv_none
      )
    }
    
    aic  <- extract_gaic(mod, k = 2, n_obs = n_obs)
    bic  <- extract_gaic(mod, k = log(n_obs), n_obs = n_obs)
    gaic <- extract_gaic(mod, k = 3, n_obs = n_obs)
    
    rows[[length(rows) + 1]] <- engine_row(
      group     = label,
      engine    = "FP",
      framework = spec$framework,
      level     = NA_character_,
      family    = "Gaussian",
      spec      = spec$name,
      aic       = aic,
      bic       = bic,
      gaic      = gaic,
      cv        = cvm
    )
    
    models[[spec$name]] <- mod
  }
  
  all_table <- if (length(rows) > 0) {
    do.call(rbind, rows)
  } else {
    empty_engine_table()
  }
  
  if (nrow(all_table) == 0) {
    return(list(
      all_table  = empty_engine_table(),
      best_table = empty_engine_table(),
      models     = models
    ))
  }
  
  metric <- cfg$ENGINE_METRIC
  
  if (!metric %in% names(all_table)) {
    metric <- "CV_RMSE"
  }
  
  pick_best <- function(engine) {
    
    sub <- all_table[all_table$Engine == engine, , drop = FALSE]
    
    if (nrow(sub) == 0) return(NULL)
    
    vals <- sub[[metric]]
    
    if (all(is.na(vals))) {
      vals <- sub$AIC
    }
    
    if (all(is.na(vals))) {
      vals <- sub$GAIC
    }
    
    if (all(is.na(vals))) {
      return(sub[1, , drop = FALSE])
    }
    
    if (metric == "CV_R2" || metric == "R2") {
      idx <- which.max(vals)
    } else {
      idx <- which.min(vals)
    }
    
    sub[idx, , drop = FALSE]
  }
  
  best_table <- do.call(
    rbind,
    Filter(Negate(is.null), list(
      pick_best("GAM"),
      pick_best("FP")
    ))
  )
  
  if (is.null(best_table) || nrow(best_table) == 0) {
    best_table <- empty_engine_table()
  }
  
  rownames(best_table) <- NULL
  
  list(
    all_table  = all_table,
    best_table = best_table,
    models     = models
  )
}
##############################################################################
# MAIN PIPELINE (revised)
##############################################################################


##############################################################################
# Q1 METHODOLOGICAL CORRECTIONS (v3)
# These definitions intentionally override earlier helper definitions.
##############################################################################

# ---------- Distribution helpers ----------
make_dfun <- function(family_name, mu, sigma, nu = NULL, tau = NULL) {
  family_name <- trim_family(family_name)
  function(y, log = FALSE) {
    fn_name <- paste0("d", family_name)
    fn <- tryCatch(get(fn_name, envir = asNamespace("gamlss.dist")), error = function(e) NULL)
    if (is.null(fn)) return(rep(NA_real_, length(y)))
    args <- list(x = y, mu = mu, sigma = sigma, log = log)
    if (family_has_nu(family_name))  args$nu  <- nu
    if (family_has_tau(family_name)) args$tau <- tau
    tryCatch(as.numeric(do.call(fn, args)), error = function(e) rep(NA_real_, length(y)))
  }
}

distribution_moments_from_quantiles <- function(family_name, mu, sigma,
                                                nu = NULL, tau = NULL,
                                                p_grid = seq(0.002, 0.998, length.out = 121)) {
  qfun <- make_qfun(family_name, mu, sigma, nu, tau)
  qs <- vapply(p_grid, function(p) qfun(p), numeric(length(mu)))
  if (is.vector(qs)) qs <- matrix(qs, nrow = length(mu))

  # E(Y) = integral Q(p) dp over the deliberately trimmed probability range.
  # Use trapezoidal integration of the quantile function.
  w <- diff(p_grid)
  prob_span <- max(p_grid) - min(p_grid)
  qmid <- (qs[, -1, drop = FALSE] + qs[, -ncol(qs), drop = FALSE]) / 2
  mean_y <- rowSums(sweep(qmid, 2, w, `*`), na.rm = TRUE) / prob_span

  # Numerically stable variance calculation:
  # Var(Y) = E[(Y - E(Y))^2], rather than E(Y^2) - E(Y)^2.
  # The latter can suffer catastrophic cancellation when the conditional
  # variance is small relative to the level of the outcome, creating tiny
  # artificial age-to-age oscillations in the derived Conditional SD.
  centered_q <- sweep(qs, 1, mean_y, FUN = "-")
  centered_sq <- centered_q^2
  centered_sq_mid <- (
    centered_sq[, -1, drop = FALSE] +
      centered_sq[, -ncol(centered_sq), drop = FALSE]
  ) / 2
  var_y <- rowSums(
    sweep(centered_sq_mid, 2, w, `*`),
    na.rm = TRUE
  ) / prob_span

  # Guard only against tiny negative round-off; genuine non-finite rows remain NA.
  var_y[!is.finite(var_y)] <- NA_real_
  var_y <- pmax(var_y, 0)

  list(mean = mean_y, sd = sqrt(var_y))
}

# [legacy duplicate function removed in public release: predict_curve]

# [legacy duplicate function removed in public release: predict_distribution_rows]

# ---------- Independent mu / sigma model selection ----------
build_mu_candidates <- function(cfg) {
  powers_set <- cfg$FP_POWERS
  out <- list()
  for (p in powers_set) {
    out[[length(out) + 1]] <- list(name = sprintf("FP1(%s)", p), powers = c(p))
  }
  for (i in seq_along(powers_set)) {
    for (j in i:length(powers_set)) {
      out[[length(out) + 1]] <- list(
        name = sprintf("FP2(%s,%s)", powers_set[i], powers_set[j]),
        powers = c(powers_set[i], powers_set[j])
      )
    }
  }
  out
}

build_sigma_candidates <- function(cfg) {
  out <- list(list(name = "Constant", powers = numeric(0)))
  for (p in unique(cfg$SIGMA_FP_POWERS)) {
    out[[length(out) + 1]] <- list(name = sprintf("FP1(%s)", p), powers = c(p))
  }
  out
}

rank_model_rows <- function(comp) {
  pass <- any(comp$PassSanity, na.rm = TRUE) & !is.na(comp$PassSanity) & comp$PassSanity
  idx <- if (any(pass, na.rm = TRUE)) which(pass) else seq_len(nrow(comp))
  ord <- order(
    ifelse(is.na(comp$GAIC[idx]), Inf, comp$GAIC[idx]),
    ifelse(is.na(comp$BIC[idx]),  Inf, comp$BIC[idx]),
    ifelse(is.na(comp$AIC[idx]),  Inf, comp$AIC[idx])
  )
  idx[ord]
}

select_model <- function(data, include_sex = FALSE, label = "",
                         cfg = CONFIG, forced_family = cfg$CANDIDATE_FAMILIES,
                         age_grid = NULL) {
  quiet <- isTRUE(cfg$QUIET_SELECTION)
  if (!quiet) cat(sprintf(" [%s] Selecting GAMLSS with independent mu/sigma forms...\n", label))
  if (is.null(age_grid)) age_grid <- seq(min(data$Age), max(data$Age), length.out = 200)
  
  mu_cands <- build_mu_candidates(cfg)
  sig_cands <- build_sigma_candidates(cfg)
  top_n <- max(1L, as.integer(cfg$MODEL_SELECTION_TOP_MU))
  
  comparison_list <- list(); model_list <- list(); spec_list <- list()
  add_fit <- function(fam_name, mu_c, sig_c, stage) {
    mod <- tryCatch(
      fit_model(data, fam_name, mu_c$powers, sig_c$powers,
                include_sex = include_sex, interaction = FALSE, label = label),
      error = function(e) NULL
    )
    if (is.null(mod) || !isTRUE(mod$converged)) return(NULL)
    sane <- tryCatch(model_centile_sanity(mod, data, age_grid, cfg), error = function(e) FALSE)
    aic <- extract_gaic(mod, k = 2)
    bic <- extract_gaic(mod, k = log(nrow(data)))
    gaic <- extract_gaic(mod, k = 3)
    if (!is.finite(aic)) return(NULL)
    idx <- length(comparison_list) + 1L
    comparison_list[[idx]] <<- data.frame(
      Group = label, Family = fam_name,
      Spec = sprintf("mu:%s | sigma:%s", mu_c$name, sig_c$name),
      MuSpec = mu_c$name, SigmaSpec = sig_c$name,
      mu_powers = paste(mu_c$powers, collapse = ","),
      sigma_powers = if (length(sig_c$powers)) paste(sig_c$powers, collapse = ",") else "constant",
      Stage = stage,
      AIC = aic, BIC = bic, GAIC = gaic,
      GlobalDeviance = tryCatch(as.numeric(mod$G.deviance), error = function(e) NA_real_),
      N = nrow(data), PassSanity = sane, stringsAsFactors = FALSE
    )
    model_list[[idx]] <<- mod
    spec_list[[idx]] <<- list(
      family = fam_name, mu = mu_c$powers, sigma = sig_c$powers,
      mu_name = mu_c$name, sigma_name = sig_c$name
    )
    idx
  }
  
  # Stage 1: choose promising location forms under constant scale, separately for each family.
  for (fam_name in forced_family) {
    fam_idx <- integer(0)
    sig0 <- sig_cands[[1]]
    for (mu_c in mu_cands) {
      idx <- add_fit(fam_name, mu_c, sig0, "mu_screen")
      if (!is.null(idx)) fam_idx <- c(fam_idx, idx)
    }
    if (length(fam_idx) == 0) next
    fam_comp <- do.call(rbind, comparison_list[fam_idx])
    ord_local <- rank_model_rows(fam_comp)
    top_local_rows <- ord_local[seq_len(min(top_n, length(ord_local)))]
    top_mu_names <- unique(fam_comp$MuSpec[top_local_rows])
    
    # Stage 2: scale form is selected independently, using only the best few mu forms.
    for (mu_name in top_mu_names) {
      mu_c <- mu_cands[[which(vapply(mu_cands, function(z) z$name == mu_name, logical(1)))[1]]]
      for (sig_c in sig_cands[-1]) add_fit(fam_name, mu_c, sig_c, "sigma_refine")
    }
  }
  
  if (length(comparison_list) == 0) stop(sprintf("[%s] No converged GAMLSS models found.", label))
  comparison <- do.call(rbind, comparison_list)
  ord <- rank_model_rows(comparison)
  best_idx <- ord[1]
  comparison <- comparison[order(
    ifelse(is.na(comparison$GAIC), Inf, comparison$GAIC),
    ifelse(is.na(comparison$BIC),  Inf, comparison$BIC),
    ifelse(is.na(comparison$AIC),  Inf, comparison$AIC)
  ), , drop = FALSE]
  rownames(comparison) <- NULL
  
  sp <- spec_list[[best_idx]]
  best_row <- do.call(rbind, comparison_list)[best_idx, , drop = FALSE]
  if (!quiet) {
    cat(sprintf(
      " [%s] Selected: %s | mu=%s | sigma=%s | GAIC=%.2f | sanity=%s\n",
      label, sp$family, sp$mu_name, sp$sigma_name, best_row$GAIC,
      ifelse(isTRUE(best_row$PassSanity), "PASS", "FAIL")
    ))
  }
  list(
    best_model = model_list[[best_idx]], best_family = sp$family,
    best_spec_name = best_row$Spec,
    best_mu_spec = sp$mu_name, best_sigma_spec = sp$sigma_name,
    best_mu_powers = sp$mu, best_sigma_powers = sp$sigma,
    best_aic = best_row$AIC, best_row = best_row,
    comparison = comparison
  )
}

select_gamlss_hierarchical <- function(data, include_sex = FALSE,
                                       label = "", cfg = CONFIG) {
  quiet <- isTRUE(cfg$QUIET_SELECTION)
  age_grid <- seq(min(data$Age), max(data$Age), length.out = 200)
  level_names <- names(cfg$GAMLSS_FAMILY_LEVELS)
  level_results <- list()
  for (lvl in level_names) {
    if (!quiet) cat(sprintf(" [%s] %s: %s\n", label, lvl, paste(cfg$GAMLSS_FAMILY_LEVELS[[lvl]], collapse = ", ")))
    sel <- tryCatch(
      select_model(data, include_sex, paste(label, lvl), cfg,
                   cfg$GAMLSS_FAMILY_LEVELS[[lvl]], age_grid),
      error = function(e) NULL
    )
    if (is.null(sel)) next
    comp <- sel$comparison
    comp$Level <- lvl
    comp <- comp[, c("Level", setdiff(names(comp), "Level")), drop = FALSE]
    br <- sel$best_row
    br$Level <- lvl
    level_results[[lvl]] <- list(sel = sel, best_row = br,
                                 pass = isTRUE(br$PassSanity), level = lvl,
                                 comparison = comp)
    if (isTRUE(cfg$GAMLSS_STOP_AT_FIRST_PASS) && isTRUE(br$PassSanity)) break
  }
  if (length(level_results) == 0) stop(sprintf("[%s] No GAMLSS model converged.", label))
  pass_pool <- Filter(function(x) isTRUE(x$pass), level_results)
  pool <- if (length(pass_pool)) pass_pool else level_results
  score <- vapply(pool, function(x) as.numeric(x$best_row$GAIC), numeric(1))
  if (isTRUE(cfg$GAMLSS_PREFER_LOWER_LEVEL)) {
    lev <- match(vapply(pool, `[[`, character(1), "level"), level_names)
    chosen <- pool[[order(lev, score)[1]]]
  } else {
    chosen <- pool[[which.min(score)]]
  }
  out <- chosen$sel
  out$level <- chosen$level
  out$comparison <- do.call(rbind, lapply(level_results, `[[`, "comparison"))
  out$search_note <- sprintf("Independent mu/sigma selection | Level=%s | Sanity=%s",
                             chosen$level, ifelse(chosen$pass, "PASS", "FAIL"))
  if (!quiet) cat(sprintf(" [%s] Final: %s | %s\n", label, out$best_family, out$best_spec_name))
  out
}

# ---------- Turning points: only genuinely internal extrema are estimable ----------
find_extrema <- function(age_grid, y, dy = NULL, d2y = NULL,
                         boundary_tol_prop = 0.05) {
  if (is.null(dy)) dy <- safe_gradient(y, age_grid)
  if (is.null(d2y)) d2y <- safe_gradient(dy, age_grid)

  # Peak/trough and maximal-change estimation require finite age, central
  # trajectory, and first derivative. The second derivative is retained for
  # compatibility/diagnostics but is NOT used to define maximal change.
  ok <- is.finite(age_grid) & is.finite(y) & is.finite(dy)

  if (sum(ok) < 7) return(list(
    age_max=NA_real_, value_max=NA_real_, age_min=NA_real_, value_min=NA_real_,
    age_vel=NA_real_, d_vel=NA_real_, boundary_max=NA, boundary_min=NA,
    boundary_vel=NA, has_internal_peak=FALSE, has_internal_trough=FALSE,
    has_internal_velocity=FALSE, peak_note="Not estimable", trough_note="Not estimable",
    velocity_note="Not estimable"
  ))

  x <- age_grid[ok]
  yy <- y[ok]
  d1 <- dy[ok]
  d2 <- d2y[ok]

  lo <- min(x)
  hi <- max(x)
  tol <- boundary_tol_prop * (hi - lo)
  interior_age <- function(a) is.finite(a) && a > lo + tol && a < hi - tol

  zero_crossings <- function(x, f, target_y) {
    out <- list()
    for (i in seq_len(length(x)-1L)) {
      if (!is.finite(f[i]) || !is.finite(f[i+1])) next
      if (f[i] == 0) {
        a <- x[i]; val <- target_y[i]
      } else if (f[i] * f[i+1] < 0) {
        w <- abs(f[i]) / (abs(f[i]) + abs(f[i+1]))
        a <- x[i] * (1-w) + x[i+1] * w
        val <- target_y[i] * (1-w) + target_y[i+1] * w
      } else next
      out[[length(out)+1L]] <- c(age=a, value=val, left=f[i], right=f[i+1])
    }
    out
  }

  # ------------------------------------------------------------------------
  # Internal peak / trough of the central trajectory:
  # determined by sign changes of the FIRST derivative.
  # ------------------------------------------------------------------------
  z1 <- zero_crossings(x, d1, yy)
  peaks <- Filter(function(z) interior_age(z["age"]) && z["left"] > 0 && z["right"] < 0, z1)
  troughs <- Filter(function(z) interior_age(z["age"]) && z["left"] < 0 && z["right"] > 0, z1)

  if (length(peaks)) {
    vals <- vapply(peaks, function(z) z["value"], numeric(1))
    z <- peaks[[which.max(vals)]]
    age_max <- unname(z["age"])
    value_max <- unname(z["value"])
    has_peak <- TRUE
    peak_note <- "Internal peak (derivative + to -)"
  } else {
    age_max <- value_max <- NA_real_
    has_peak <- FALSE
    imax <- which.max(yy)
    b_age <- x[imax]
    peak_note <- if (b_age <= lo + tol || b_age >= hi - tol)
      sprintf("No internal peak; global maximum is boundary-driven (%.1fy)", b_age)
    else "No derivative-confirmed internal peak"
  }

  if (length(troughs)) {
    vals <- vapply(troughs, function(z) z["value"], numeric(1))
    z <- troughs[[which.min(vals)]]
    age_min <- unname(z["age"])
    value_min <- unname(z["value"])
    has_trough <- TRUE
    trough_note <- "Internal trough (derivative - to +)"
  } else {
    age_min <- value_min <- NA_real_
    has_trough <- FALSE
    imin <- which.min(yy)
    b_age_min <- x[imin]
    trough_note <- if (b_age_min <= lo + tol || b_age_min >= hi - tol)
      sprintf("No internal trough; global minimum is boundary-driven (%.1fy)", b_age_min)
    else "No derivative-confirmed internal trough"
  }

  # ------------------------------------------------------------------------
  # Maximal age-related change
  #
  # Exact definition used throughout BrainNormR v5.5.1:
  #   Age* = argmax |d(Central trajectory)/dAge|
  #
  # The search is restricted to the prespecified RELIABLE INTERIOR age range
  # [lo + tol, hi - tol] to reduce numerical boundary artefacts.
  # We maximize the ABSOLUTE derivative magnitude but preserve the SIGN of the
  # original derivative in d_vel so Increase vs Decline remains identifiable.
  #
  # IMPORTANT: this is NOT defined by d2 = 0. A zero of the second derivative
  # is only a local extremum of the first derivative and need not be the
  # largest age gradient over the reliable age range.
  # ------------------------------------------------------------------------
  interior_idx <- which(
    is.finite(x) & is.finite(d1) &
      x > (lo + tol) & x < (hi - tol)
  )

  if (length(interior_idx) >= 1L) {
    abs_d1 <- abs(d1[interior_idx])
    j_local <- which.max(abs_d1)
    j <- interior_idx[j_local]

    age_vel <- x[j]
    d_vel <- d1[j]
    has_vel <- is.finite(age_vel) && is.finite(d_vel)

    # TRUE means the selected maximum is the first/last available grid point
    # of the reliable interior window. The estimate is still reported, but the
    # flag warns that |gradient| may continue increasing toward the excluded
    # raw-age boundary.
    boundary_vel <- isTRUE(j == min(interior_idx) || j == max(interior_idx))

    vel_note <- if (boundary_vel) {
      paste0(
        "Maximal absolute age gradient within the reliable interior age range; ",
        "maximum occurs at the edge of the interior search window"
      )
    } else {
      "Maximal absolute age gradient within the reliable interior age range"
    }
  } else {
    age_vel <- NA_real_
    d_vel <- NA_real_
    has_vel <- FALSE
    boundary_vel <- NA
    vel_note <- paste0(
      "Maximal age-related change not estimable: ",
      "no valid derivative values in the reliable interior age range"
    )
  }

  list(
    age_max=age_max, value_max=value_max, mu_max=value_max,
    age_min=age_min, value_min=value_min, mu_min=value_min,
    age_vel=age_vel, d_vel=d_vel, dmu_vel=d_vel,
    boundary_max=!has_peak, boundary_min=!has_trough,
    boundary_vel=boundary_vel,
    has_internal_peak=has_peak, has_internal_trough=has_trough,
    # Backward-compatible field name. TRUE means a valid maximal-change
    # estimate exists inside the reliable search window.
    has_internal_velocity=has_vel,
    peak_note=peak_note, trough_note=trough_note, velocity_note=vel_note
  )
}

# ---------- Conditional bootstrap with derivative uncertainty bands ----------
# [legacy duplicate function removed in public release: bootstrap_one_iter]

# [legacy duplicate function removed in public release: bootstrap_model]

peak_stability <- function(boot_obj, ex = NULL, label = "") {
  if (!is.null(boot_obj) && !is.null(boot_obj$valid) && !isTRUE(boot_obj$valid)) {
    return(list(median=NA_real_, mad=NA_real_, iqr=NA_real_, range=NA_real_,
                n=0, internal_prop=NA_real_, stable=FALSE,
                note="Bootstrap QC failed overall; peak stability inference suppressed."))
  }
  if (!is.null(ex) && !isTRUE(ex$has_internal_peak)) {
    return(list(median=NA_real_, mad=NA_real_, iqr=NA_real_, range=NA_real_,
                n=0, internal_prop=0, stable=NA,
                note="No internal peak in the fitted trajectory; stability is not defined."))
  }
  if (is.null(boot_obj) || is.null(boot_obj$boot_mat)) return(list(
    median=NA_real_, mad=NA_real_, iqr=NA_real_, range=NA_real_, n=0,
    internal_prop=NA_real_, stable=NA, note="Bootstrap unavailable"))
  x <- boot_obj$boot_mat[,"age_max"]
  xf <- x[is.finite(x)]
  prop <- length(xf) / nrow(boot_obj$boot_mat)
  if (length(xf) < 20) return(list(
    median=NA_real_, mad=NA_real_, iqr=NA_real_, range=NA_real_, n=length(xf),
    internal_prop=prop, stable=NA,
    note="Too few bootstrap replicates contained an internal peak."))
  list(median=median(xf), mad=mad(xf,constant=1), iqr=IQR(xf),
       range=diff(range(xf)), n=length(xf), internal_prop=prop,
       stable=NA,
       note="Descriptive stability only; no arbitrary binary stable/unstable threshold is used.")
}

# ---------- Stratified nested cross-validation and cross-fitted normative Z ----------
# [legacy duplicate function removed in public release: make_age_stratified_folds]

# [legacy duplicate function removed in public release: nested_gamlss_cv]

crossfit_normative_object <- function(nested_obj) {
  d <- nested_obj$crossfit_data

  # Keep cross-fitted scores explicitly separate from full-sample scores.
  # CF_* values are generated from held-out predictions in nested CV.
  d$CF_Centile <- 100 * d$CF_Prob

  # Norm_* is deliberately NOT aliased to CF_* here. Norm_Prob / Norm_Z /
  # Norm_Centile are added later from the final full-sample GAMLSS model.
  d$Norm_Prob <- NA_real_
  d$Norm_Z <- NA_real_
  d$Norm_Centile <- NA_real_

  d$mu_expected <- NA_real_
  d$sigma_expected <- NA_real_
  d$nu_expected <- NA_real_
  d$tau_expected <- NA_real_

  d$Normative_Score_Type <- paste0(
    "CF_Norm_Z = cross-fitted held-out score; ",
    "Norm_Z = final full-sample GAMLSS score"
  )
  d
}

merge_fullfit_normative_scores <- function(norm, full_norm) {
  if (is.null(norm) || is.null(full_norm)) {
    stop("Normative-score merge received a NULL object.")
  }

  # Prefer stable SubjectID alignment when both tables have unique IDs.
  if ("SubjectID" %in% names(norm) && "SubjectID" %in% names(full_norm) &&
      !anyDuplicated(norm$SubjectID) && !anyDuplicated(full_norm$SubjectID)) {
    idx <- match(norm$SubjectID, full_norm$SubjectID)
    if (anyNA(idx)) {
      stop("Internal error: full-fit and cross-fitted SubjectID values do not match.")
    }
  } else {
    if (nrow(norm) != nrow(full_norm)) {
      stop("Internal error: full-fit and cross-fitted normative rows do not match.")
    }
    idx <- seq_len(nrow(norm))
  }

  needed <- c("Norm_Prob", "Norm_Z", "Norm_Centile",
              "mu_expected", "sigma_expected", "nu_expected", "tau_expected")
  missing_cols <- setdiff(needed, names(full_norm))
  if (length(missing_cols)) {
    stop("Full-fit normative prediction is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  for (nm in needed) norm[[nm]] <- full_norm[[nm]][idx]

  # Classification is based on the canonical full-fit Norm_Centile; CF scores
  # remain available separately for validation and downstream cross-fitted use.
  norm <- classify_normative_status(norm)
  norm <- calculate_BDI(norm)
  norm
}

run_full_validation <- function(data, sel, boot_obj, ex = NULL,
                                label="", cfg=CONFIG, folds=NULL) {
  cat(sprintf("\n--- Nested validation: %s ---\n", label))
  nested <- nested_gamlss_cv(data, cfg, label, folds)
  stab <- peak_stability(boot_obj, ex, label)
  list(
    nested_cv=nested,
    cv10=list(single=nested$single, repeated=NULL, n_folds=length(unique(nested$folds)), n_repeats=1),
    cv_repeated=NULL,
    optimism=list(corrected_RMSE=NA_real_, note="Not reported: nested CV is the primary optimism-controlled validation."),
    calibration=nested$calibration,
    pi_coverage=list(coverage=unname(nested$single["Coverage95"]), expected=.95, n=sum(is.finite(nested$crossfit_data$CF_Lower95))),
    peak_stability=stab,
    crossfit_data=nested$crossfit_data,
    selection_table=nested$selection_table,
    pit_ks_p=nested$pit_ks_p,
    z_mean=nested$z_mean, z_sd=nested$z_sd
  )
}

# ---------- Fair nested engine comparison using proper log score ----------
# [legacy duplicate function removed in public release: select_best_gam_train]

# [legacy duplicate function removed in public release: select_best_fp_train]

nested_gaussian_engine <- function(data, folds, cfg, engine=c("GAM","FP")) {
  engine <- match.arg(engine)
  pred <- rep(NA_real_,nrow(data)); logd <- lo <- hi <- rep(NA_real_,nrow(data))
  specs <- character()
  for(f in sort(unique(folds))) {
    tr <- data[folds!=f,,drop=FALSE]; te <- data[folds==f,,drop=FALSE]
    sel <- if(engine=="GAM") select_best_gam_train(tr,cfg) else select_best_fp_train(tr,cfg)
    if(is.null(sel)) next
    m <- sel$model
    pp <- tryCatch(as.numeric(predict(m,newdata=te,type="response")),error=function(e) NULL)
    if(is.null(pp)) next
    # Correct Gaussian SD: sqrt(scale) for mgcv; summary.lm$sigma for lm.
    s <- if(engine=="GAM") sqrt(max(summary(m)$scale,0)) else summary(m)$sigma
    ii <- which(folds==f); pred[ii] <- pp
    lo[ii] <- pp-1.96*s; hi[ii] <- pp+1.96*s
    logd[ii] <- dnorm(te$FD,mean=pp,sd=s,log=TRUE)
    specs <- c(specs, sel$spec)
  }
  ok <- is.finite(pred); met <- cv_metrics(data$FD[ok],pred[ok])
  cv_nll <- -mean(logd[is.finite(logd)],na.rm=TRUE)
  covok <- is.finite(lo)&is.finite(hi)
  cov <- mean(data$FD[covok]>=lo[covok]&data$FD[covok]<=hi[covok])
  list(metrics=c(RMSE=unname(met["RMSE"]),MAE=unname(met["MAE"]),R2=unname(met["R2"]),NLL=cv_nll,Coverage95=cov),
       selected_specs=specs,pred=pred)
}

# [legacy duplicate function removed in public release: run_engine_comparison]

# ---------- Summary / sex differences ----------
boundary_note <- function(ex) {
  paste(c(ex$peak_note, ex$trough_note, ex$velocity_note), collapse="; ")
}

build_summary_table <- function(diag_F, diag_M, diag_All,
                                ex_F, ex_M, ex_All,
                                boot_F, boot_M, boot_All,
                                sel_F, sel_M, sel_All) {
  sels <- list(sel_F,sel_M,sel_All); exs <- list(ex_F,ex_M,ex_All); boots <- list(boot_F,boot_M,boot_All)
  diags <- list(diag_F,diag_M,diag_All); groups <- c("Female","Male","All")
  out <- lapply(seq_along(groups),function(i){
    ex<-exs[[i]]; b<-boots[[i]]; s<-sels[[i]]; dg<-diags[[i]]
    data.frame(Group=groups[i],Family=s$best_family,MuSpec=s$best_mu_spec,SigmaSpec=s$best_sigma_spec,
               `Shapiro P`=get_val(dg$shapiro$p.value),
               `Peak Age`=get_val(ex$age_max),`Peak Age CI`=get_ci(b,"age_max"),
               `Peak Value`=get_val(ex$value_max),
               `Trough Age`=get_val(ex$age_min),`Trough Age CI`=get_ci(b,"age_min"),
               `Trough Value`=get_val(ex$value_min),
               `Maximal Change Age`=get_val(ex$age_vel),`Maximal Change Age CI`=get_ci(b,"age_vel"),
               `Derivative at Maximal Change`=get_val(ex$d_vel),
               `Boundary note`=boundary_note(ex),
               `Bootstrap CI type`=b$ci_type, check.names=FALSE,stringsAsFactors=FALSE)
  })
  do.call(rbind,out)
}

build_bootstrap_detail <- function(results) {
  groups<-c("Female","Male","All")
  stat_names<-c("age_max","value_max","age_min","value_min","age_vel","d_vel")
  labels<-c("Peak Age","Peak Value","Trough Age","Trough Value","Maximal Change Age","Derivative at Maximal Change")
  do.call(rbind,lapply(groups,function(g){
    ex<-results[[g]]$extrema;b<-results[[g]]$bootstrap
    est<-c(ex$age_max,ex$value_max,ex$age_min,ex$value_min,ex$age_vel,ex$d_vel)
    data.frame(Group=g,Statistic=labels,Estimate=est,Median=b$stats[stat_names,"median"],
               Lower95=b$stats[stat_names,"lower"],Upper95=b$stats[stat_names,"upper"],
               N_Internal_Boot=b$stats[stat_names,"n_internal"],CI_Type=b$ci_type,stringsAsFactors=FALSE)
  }))
}

analyze_sex_differences <- function(df, results, cfg=CONFIG) {
  cat("\n=== Sex difference analysis: primary test is pooled Age x Sex interaction ===\n")
  female_fd<-df$FD[df$Sex=="Female"]; male_fd<-df$FD[df$Sex=="Male"]
  descriptive<-list(Female=c(Mean=mean(female_fd),SD=sd(female_fd),N=length(female_fd)),
                    Male=c(Mean=mean(male_fd),SD=sd(male_fd),N=length(male_fd)))
  t_test_raw<-tryCatch(t.test(FD~Sex,data=df),error=function(e)NULL)
  wilcox_raw<-tryCatch(wilcox.test(FD~Sex,data=df),error=function(e)NULL)
  fam<-attr(results$All$best_model,"family_name")
  mu_p<-attr(results$All$best_model,"mu_powers"); sig_p<-attr(results$All$best_model,"sigma_powers")
  m0<-fit_model(df,fam,mu_p,sig_p,TRUE,FALSE)
  m1<-fit_model(df,fam,mu_p,sig_p,TRUE,TRUE)
  interaction_test<-NULL
  if(!is.null(m0)&&!is.null(m1)){
    ll0<-as.numeric(logLik(m0)); ll1<-as.numeric(logLik(m1)); df0<-attr(logLik(m0),"df");df1<-attr(logLik(m1),"df")
    if(is.finite(ll0)&&is.finite(ll1)&&df1>df0){
      lr<-max(0,2*(ll1-ll0));dd<-df1-df0;p<-pchisq(lr,dd,lower.tail=FALSE)
      interaction_test<-list(family=fam,LR_stat=lr,df=dd,p_value=p,AIC_null=extract_gaic(m0,2),AIC_int=extract_gaic(m1,2))
      cat(sprintf(" Primary LR Age x Sex: LR=%.3f, df=%d, p=%.4f\n",lr,dd,p))
    }
  }
  # Secondary descriptive bootstrap contrasts. Pair independent draws randomly;
  # undefined boundary-driven extrema remain NA and therefore are not tested.
  pars<-c("age_max","value_max","age_min","value_min","age_vel","d_vel")
  labs<-c("Peak Age","Peak Value","Trough Age","Trough Value","Maximal Change Age","Derivative at Maximal Change")
  bt<-data.frame(Parameter=labs,Difference_Median=NA_real_,Lower95=NA_real_,Upper95=NA_real_,P_boot=NA_real_)
  set.seed(cfg$SEED+991)
  for(i in seq_along(pars)){
    f<-results$Female$bootstrap$boot_mat[,pars[i]];m<-results$Male$bootstrap$boot_mat[,pars[i]]
    f<-f[is.finite(f)];m<-m[is.finite(m)]
    n<-min(length(f),length(m))
    if(n<50) next
    d<-sample(f,n,replace=FALSE)-sample(m,n,replace=FALSE)
    q<-quantile(d,c(.025,.975));bt$Difference_Median[i]<-median(d);bt$Lower95[i]<-q[1];bt$Upper95[i]<-q[2]
    bt$P_boot[i]<-min(1,2*min(mean(d<=0),mean(d>=0)))
  }
  bt$P_adj_BH<-p.adjust(bt$P_boot,"BH")
  primary_sig<-!is.null(interaction_test)&&is.finite(interaction_test$p_value)&&interaction_test$p_value<.05
  list(descriptive=descriptive,t_test_raw=t_test_raw,wilcox_raw=wilcox_raw,
       interaction_test=interaction_test,boot_diff_table=bt,
       significant=primary_sig,
       inference_note="Primary sex-difference inference is the pooled Age-by-Sex likelihood-ratio test. Bootstrap extrema contrasts are secondary and are not evaluated when an internal extremum is undefined.")
}

# ---------- Q1 main figure + diagnostics ----------
# [legacy duplicate function removed in public release: create_q1_main_plot]


# [legacy duplicate function removed in public release: create_residual_diagnostic_plots]

# Override save to add Q1 figure, cross-fitted data, nested selections and diagnostics.
# [legacy duplicate function removed in public release: save_results]

##############################################################################
# MAIN PIPELINE - Q1 REVISED
##############################################################################



##############################################################################
# Q1 ALL-ONLY GENERIC OUTCOME OVERRIDES (v4)
# Primary model: pooled sample only. Sex is adjusted in the pooled model.
# Outcome-specific covariates:
#   FD / Thickness / Curvature / Other : Sex
#   Volume / Area                      : Sex + eTIV
#   Cognition                          : Sex + Education
##############################################################################

# ---- User-facing configuration ----
if (is.null(CONFIG$OUTCOME_COL)) CONFIG$OUTCOME_COL <- CONFIG$FD_COL
if (is.null(CONFIG$OUTCOME_TYPE)) CONFIG$OUTCOME_TYPE <- "FD"
if (is.null(CONFIG$ETIV_COL)) CONFIG$ETIV_COL <- "eTIV"
if (is.null(CONFIG$ADJUST_SEX)) CONFIG$ADJUST_SEX <- TRUE
if (is.null(CONFIG$ADJUST_EDU_FOR_COGNITION)) CONFIG$ADJUST_EDU_FOR_COGNITION <- TRUE
if (is.null(CONFIG$ADJUST_ETIV_FOR_SIZE)) CONFIG$ADJUST_ETIV_FOR_SIZE <- TRUE
if (is.null(CONFIG$TEST_AGE_BY_SEX_INTERACTION)) CONFIG$TEST_AGE_BY_SEX_INTERACTION <- TRUE
# Backward-compatible alias used by legacy helpers only.
CONFIG$FD_COL <- CONFIG$OUTCOME_COL

normalize_outcome_type <- function(x) {
  x <- tolower(trimws(as.character(x)[1]))
  if (x %in% c("fd", "fractal", "fractal_dimension", "fractal dimension")) return("FD")
  if (x %in% c("thickness", "cortical thickness")) return("Thickness")
  if (x %in% c("curvature", "curv")) return("Curvature")
  if (x %in% c("volume", "vol")) return("Volume")
  if (x %in% c("area", "surface area", "surface_area")) return("Area")
  if (x %in% c("cognition", "cognitive", "cog")) return("Cognition")
  "Other"
}

active_covariates <- function(cfg = CONFIG) {
  typ <- normalize_outcome_type(cfg$OUTCOME_TYPE)
  out <- character(0)
  if (isTRUE(cfg$ADJUST_SEX)) out <- c(out, "Sex_c")
  if (identical(typ, "Cognition") && isTRUE(cfg$ADJUST_EDU_FOR_COGNITION)) out <- c(out, "edu_c")
  if (typ %in% c("Volume", "Area") && isTRUE(cfg$ADJUST_ETIV_FOR_SIZE)) out <- c(out, "eTIV_c")
  unique(out)
}

pretty_active_covariates <- function(cfg = CONFIG) {
  cv <- active_covariates(cfg)
  nm <- c(Sex_c = "sex", edu_c = "education", eTIV_c = "eTIV")
  out <- unname(nm[cv])
  out[is.na(out)] <- cv[is.na(out)]
  out
}

covariate_rhs <- function(cfg = CONFIG) {
  cv <- active_covariates(cfg)
  if (!length(cv)) "" else paste(cv, collapse = " + ")
}

append_covariates <- function(base_rhs, cfg = CONFIG, interaction = FALSE) {
  cv <- active_covariates(cfg)
  sex_present <- "Sex_c" %in% cv
  other <- setdiff(cv, "Sex_c")
  if (interaction && sex_present) {
    rhs <- paste0("(", base_rhs, ") * Sex_c")
  } else {
    rhs <- base_rhs
    if (sex_present) rhs <- paste(rhs, "+ Sex_c")
  }
  if (length(other)) rhs <- paste(rhs, "+", paste(other, collapse = " + "))
  rhs
}

cat("\n[CONFIG] Generic all-only mode enabled.\n")
cat(sprintf("  Outcome: %s | Type: %s\n", CONFIG$OUTCOME_COL, normalize_outcome_type(CONFIG$OUTCOME_TYPE)))
cat(sprintf("  Covariates in location model: %s\n", paste(active_covariates(CONFIG), collapse = ", ")))

# ---- Generic data loading / centered adjustment covariates ----
# [legacy duplicate function removed in public release: load_data]

clean_data <- function(df, cfg) {
  cat("[2/9] Cleaning data...\n")
  n_raw <- nrow(df)
  typ <- normalize_outcome_type(cfg$OUTCOME_TYPE)
  req <- is.finite(df$FD) & is.finite(df$Age) & is.finite(df$sex)
  if (identical(typ, "Cognition") && isTRUE(cfg$ADJUST_EDU_FOR_COGNITION)) req <- req & is.finite(df$edu)
  if (typ %in% c("Volume", "Area") && isTRUE(cfg$ADJUST_ETIV_FOR_SIZE)) req <- req & is.finite(df$eTIV)
  df <- df[req, , drop = FALSE]
  df$Sex <- ifelse(df$sex == cfg$FEMALE_CODE, "Female",
                   ifelse(df$sex == cfg$MALE_CODE, "Male", NA_character_))
  df <- df[!is.na(df$Sex), , drop = FALSE]
  df$Sex <- factor(df$Sex, levels = c("Female", "Male"))
  if (any(df$Age <= 0, na.rm = TRUE)) stop("Age must be strictly positive for fractional-polynomial terms.")
  if (nrow(df) < 50) warning(sprintf("Only %d complete observations remain for this outcome.", nrow(df)))
  # Center covariates on the analysis sample. Zero therefore represents the pooled-sample reference.
  male01 <- as.numeric(df$Sex == "Male")
  df$Sex_c <- male01 - mean(male01, na.rm = TRUE)
  df$edu_c <- if (any(is.finite(df$edu))) df$edu - mean(df$edu, na.rm = TRUE) else 0
  df$eTIV_c <- if (any(is.finite(df$eTIV))) df$eTIV - mean(df$eTIV, na.rm = TRUE) else 0
  df$Age_s <- df$Age / 10
  attr(df, "center_values") <- list(
    male_proportion = mean(male01, na.rm = TRUE),
    edu_mean = if (any(is.finite(df$edu))) mean(df$edu, na.rm = TRUE) else NA_real_,
    eTIV_mean = if (any(is.finite(df$eTIV))) mean(df$eTIV, na.rm = TRUE) else NA_real_
  )
  cat(sprintf("  Raw: %d | Complete analysis rows: %d | Dropped: %d\n", n_raw, nrow(df), n_raw - nrow(df)))
  cat(sprintf("  Female: %d | Male: %d\n", sum(df$Sex == "Female"), sum(df$Sex == "Male")))
  df
}

validate_data <- function(df, cfg = CONFIG) {
  cat("[3/9] Validating data...\n")
  stopifnot(nrow(df) > 0)
  for (v in c("FD", "Age", "sex")) if (any(!is.finite(df[[v]]))) stop(sprintf("Column '%s' has non-finite values.", v))
  for (v in active_covariates(cfg)) if (any(!is.finite(df[[v]]))) stop(sprintf("Required adjustment variable '%s' has non-finite values.", v))
  invisible(TRUE)
}

# ---- Formulas: age shape + pooled adjustment covariates in mu only ----
make_formulas <- function(family_name, mu_powers, sigma_powers,
                          include_sex = TRUE, interaction = FALSE,
                          fp_var = "Age_s") {
  mu_terms <- build_fp(fp_var, mu_powers)
  sigma_terms <- if (length(sigma_powers)) build_fp(fp_var, sigma_powers) else "1"
  mu_rhs <- append_covariates(mu_terms, CONFIG, interaction = interaction && isTRUE(include_sex))
  list(
    form_mu = as.formula(paste("FD ~", mu_rhs)),
    # Deliberately keep sigma parsimonious: its age form is selected independently;
    # sex/education/eTIV are not forced into the scale model.
    form_sigma = as.formula(paste("~", sigma_terms)),
    form_nu = ~1,
    form_tau = ~1
  )
}

fit_model <- function(data, family_name, mu_powers, sigma_powers,
                      include_sex = TRUE, interaction = FALSE,
                      label = "", maxit = 5000) {
  fam <- set_links(get_family_obj(family_name), family_name)
  forms <- make_formulas(family_name, mu_powers, sigma_powers,
                         include_sex = include_sex, interaction = interaction,
                         fp_var = "Age_s")
  ctrl <- gamlss.control(maxit = maxit, n.cyc = 50, c.crit = 0.001, trace = FALSE)
  args <- list(formula = forms$form_mu, sigma.fo = forms$form_sigma,
               family = fam, data = data, control = ctrl)
  if (family_has_nu(family_name)) args$nu.fo <- forms$form_nu
  if (family_has_tau(family_name)) args$tau.fo <- forms$form_tau
  model <- tryCatch(suppressWarnings(do.call(gamlss, args)), error = function(e) NULL)
  if (is.null(model) || !isTRUE(model$converged)) return(NULL)
  if (is.null(model$data)) model$data <- data
  attr(model, "family_name") <- family_name
  attr(model, "mu_powers") <- mu_powers
  attr(model, "sigma_powers") <- sigma_powers
  attr(model, "include_sex") <- include_sex
  attr(model, "interaction") <- interaction
  attr(model, "active_covariates") <- active_covariates(CONFIG)
  model
}

# ---- Prediction at pooled-reference covariates (all centered covariates = 0) ----
reference_newdata <- function(age, model = NULL) {
  nd <- data.frame(Age = as.numeric(age), Age_s = as.numeric(age) / 10)
  for (v in active_covariates(CONFIG)) nd[[v]] <- 0
  nd
}

predict_params_newdata <- function(model, new_data) {
  n <- nrow(new_data)
  family_name <- attr(model, "family_name")
  if (is.null(family_name)) family_name <- as.character(model$family[1])
  get_param <- function(what) {
    p <- tryCatch(predict(model, what = what, newdata = new_data, type = "response"), error = function(e) NULL)
    if (!is.null(p)) {
      if (is.matrix(p) || is.data.frame(p)) p <- p[,1]
      p <- as.numeric(unname(p))
      if (length(p) == n) return(p)
    }
    rep(NA_real_, n)
  }
  list(
    family = family_name,
    mu = get_param("mu"),
    sigma = get_param("sigma"),
    nu = if (family_has_nu(family_name)) get_param("nu") else NULL,
    tau = if (family_has_tau(family_name)) get_param("tau") else NULL
  )
}

predict_parameters <- function(model, age_grid, sex_ref = NULL) {
  nd <- reference_newdata(age_grid, model)
  pr <- predict_params_newdata(model, nd)
  c(list(Age = age_grid), pr)
}

predict_distribution_rows <- function(model, data) {
  nd <- data
  # Cross-fitted individual scoring uses each person's actual centered covariates.
  pr <- predict_params_newdata(model, nd)
  qfun <- make_qfun(pr$family, pr$mu, pr$sigma, pr$nu, pr$tau)
  pfun <- make_pfun(pr$family, pr$mu, pr$sigma, pr$nu, pr$tau)
  dfun <- make_dfun(pr$family, pr$mu, pr$sigma, pr$nu, pr$tau)
  mom <- distribution_moments_from_quantiles(pr$family, pr$mu, pr$sigma, pr$nu, pr$tau,
                                             p_grid = seq(0.003, 0.997, length.out = 81))
  prob <- pfun(data$FD)
  prob <- pmin(pmax(prob, 1e-10), 1 - 1e-10)
  dens <- dfun(data$FD, log = FALSE)
  data.frame(
    pred_mean = mom$mean,
    pred_median = qfun(0.50),
    lo = qfun(0.025), hi = qfun(0.975),
    prob = prob, z = qnorm(prob),
    log_density = ifelse(is.finite(dens) & dens > 0, log(dens), NA_real_),
    family = pr$family,
    stringsAsFactors = FALSE
  )
}

# ---- Age x sex balanced folds for pooled modeling ----
make_age_stratified_folds <- function(data, k, seed) {
  set.seed(seed)
  k <- max(2L, min(as.integer(k), floor(nrow(data) / 5)))
  nb <- min(6, max(3, floor(nrow(data) / 25)))
  br <- unique(quantile(data$Age, probs = seq(0,1,length.out=nb+1), na.rm=TRUE, type=2))
  age_bin <- if (length(br) >= 3) cut(data$Age, breaks=br, include.lowest=TRUE, labels=FALSE) else rep(1L,nrow(data))
  strata <- interaction(age_bin, data$Sex, drop=TRUE)
  folds <- integer(nrow(data))
  for (s in levels(strata)) {
    ii <- which(strata == s)
    if (length(ii)) folds[ii] <- sample(rep(seq_len(k), length.out = length(ii)))
  }
  # Safety fallback for any unassigned rows.
  z <- which(folds == 0)
  if (length(z)) folds[z] <- sample(rep(seq_len(k), length.out=length(z)))
  folds
}

# ---- Nested GAMLSS CV: model selection happens inside each training fold ----
nested_gamlss_cv <- function(data, cfg = CONFIG, label = "All", folds = NULL) {
  k <- cfg$NESTED_CV_FOLDS
  if (is.null(folds)) folds <- make_age_stratified_folds(data, k, cfg$SEED)

  out <- data
  out$.row_id <- seq_len(nrow(data))
  for (nm in c(
    "CF_PredMean","CF_PredMedian","CF_Lower95","CF_Upper95",
    "CF_Prob","CF_Norm_Z","CF_LogDensity"
  )) out[[nm]] <- NA_real_
  out$CF_Family <- NA_character_

  fold_ids <- sort(unique(folds))

  # Sex is included only when it is an active pooled-sample adjustment and
  # both sex levels are present. It is automatically excluded in Female/Male
  # stratified models.
  include_sex_fold <- isTRUE(cfg$ADJUST_SEX) &&
    "Sex" %in% names(data) &&
    length(unique(data$Sex[!is.na(data$Sex)])) >= 2

  fold_results <- compute_lapply(
    fold_ids,
    function(f) {
      CONFIG <<- cfg

      if (isTRUE(cfg$REPRODUCIBLE_PARALLEL)) {
        set.seed(stable_task_seed(cfg$SEED, label, f, stage_id = 21L))
      }

      tr <- data[folds != f, , drop = FALSE]
      te <- data[folds == f, , drop = FALSE]

      cat(sprintf(
        "  [%s] Nested fold %d/%d: selecting on training data only...\n",
        label, f, length(fold_ids)
      ))

      sel <- tryCatch(
        select_gamlss_hierarchical(
          tr,
          include_sex_fold,
          paste0(label, "-fold", f),
          cfg
        ),
        error = function(e) NULL
      )

      if (is.null(sel)) return(NULL)

      pr <- tryCatch(
        predict_distribution_rows(sel$best_model, te),
        error = function(e) NULL
      )

      if (is.null(pr)) return(NULL)

      list(
        fold = f,
        row_index = which(folds == f),
        pred = pr,
        selection = data.frame(
          Fold = f,
          Family = sel$best_family,
          MuSpec = sel$best_mu_spec,
          SigmaSpec = sel$best_sigma_spec,
          Spec = sel$best_spec_name,
          stringsAsFactors = FALSE
        )
      )
    },
    cfg = cfg,
    future_seed = TRUE
  )

  fold_results <- fold_results[!vapply(fold_results, is.null, logical(1))]
  fold_results <- fold_results[
    order(vapply(fold_results, function(z) z$fold, numeric(1)))
  ]

  selection <- list()

  for (fr in fold_results) {
    ii <- fr$row_index
    pr <- fr$pred

    out$CF_PredMean[ii] <- pr$pred_mean
    out$CF_PredMedian[ii] <- pr$pred_median
    out$CF_Lower95[ii] <- pr$lo
    out$CF_Upper95[ii] <- pr$hi
    out$CF_Prob[ii] <- pr$prob
    out$CF_Norm_Z[ii] <- pr$z
    out$CF_LogDensity[ii] <- pr$log_density
    out$CF_Family[ii] <- pr$family
    selection[[length(selection) + 1L]] <- fr$selection
  }

  ok <- is.finite(out$FD) & is.finite(out$CF_PredMean)
  metrics <- cv_metrics(out$FD[ok], out$CF_PredMean[ok])

  mok <- is.finite(out$FD) & is.finite(out$CF_PredMedian)
  median_mae <- if (any(mok)) {
    mean(abs(out$FD[mok] - out$CF_PredMedian[mok]))
  } else NA_real_

  lok <- is.finite(out$CF_LogDensity)
  nll <- if (any(lok)) -mean(out$CF_LogDensity[lok]) else NA_real_

  covok <- is.finite(out$FD) &
    is.finite(out$CF_Lower95) &
    is.finite(out$CF_Upper95)

  coverage <- if (any(covok)) {
    mean(
      out$FD[covok] >= out$CF_Lower95[covok] &
        out$FD[covok] <= out$CF_Upper95[covok]
    )
  } else NA_real_

  calfit <- if (sum(ok) >= 10) {
    lm(out$FD[ok] ~ out$CF_PredMean[ok])
  } else NULL

  cal <- if (is.null(calfit)) {
    list(slope=NA_real_, intercept=NA_real_, R2=NA_real_, plot_data=NULL)
  } else {
    list(
      slope = unname(coef(calfit)[2]),
      intercept = unname(coef(calfit)[1]),
      R2 = summary(calfit)$r.squared,
      plot_data = data.frame(
        Observed = out$FD[ok],
        Predicted = out$CF_PredMean[ok]
      )
    )
  }

  pit <- out$CF_Prob[is.finite(out$CF_Prob)]
  pit_ks_p <- if (length(pit) >= 20) {
    tryCatch(ks.test(pit, "punif")$p.value, error = function(e) NA_real_)
  } else NA_real_

  list(
    single = c(
      RMSE = unname(metrics["RMSE"]),
      MAE = unname(metrics["MAE"]),
      R2 = unname(metrics["R2"]),
      Median_MAE = median_mae,
      NLL = nll,
      Coverage95 = coverage
    ),
    folds = folds,
    crossfit_data = out,
    selection_table = if (length(selection)) {
      do.call(rbind, selection)
    } else data.frame(),
    calibration = cal,
    pit_ks_p = pit_ks_p,
    z_mean = mean(out$CF_Norm_Z, na.rm = TRUE),
    z_sd = sd(out$CF_Norm_Z, na.rm = TRUE)
  )
}

# ---- Fair Gaussian comparison engines, using the same adjustment covariates ----
select_best_gam_train <- function(train, cfg) {
  cand <- list()
  cv <- covariate_rhs(cfg)
  plus <- if (nzchar(cv)) paste0(" + ", cv) else ""
  for (bs in cfg$GAM_BS) {
    f <- as.formula(sprintf("FD ~ s(Age, bs='%s', k=%d)%s", bs, cfg$GAM_K, plus))
    m <- tryCatch(mgcv::gam(f, data=train, method="REML"), error=function(e) NULL)
    if (!is.null(m)) cand[[length(cand)+1]] <- list(model=m,spec=sprintf("GAM_%s_k%d",bs,cfg$GAM_K),aic=AIC(m))
  }
  if (!length(cand)) return(NULL)
  cand[[which.min(vapply(cand, `[[`, numeric(1), "aic"))]]
}

select_best_fp_train <- function(train, cfg) {
  cand <- list(); cv <- covariate_rhs(cfg); plus <- if (nzchar(cv)) paste0(" + ", cv) else ""
  for (sp in build_fp_lm_specs(cfg)) {
    rhs <- sub("^FD\\s*~\\s*", "", deparse(sp$formula))
    f <- as.formula(paste0("FD ~ ", rhs, plus))
    m <- tryCatch(lm(f, data=train), error=function(e) NULL)
    if (!is.null(m)) cand[[length(cand)+1]] <- list(model=m,spec=sp$name,framework=sp$framework,aic=AIC(m))
  }
  for (sp in build_poly_specs(cfg)) {
    rhs <- sub("^FD\\s*~\\s*", "", deparse(sp$formula))
    f <- as.formula(paste0("FD ~ ", rhs, plus))
    m <- tryCatch(lm(f, data=train), error=function(e) NULL)
    if (!is.null(m)) cand[[length(cand)+1]] <- list(model=m,spec=sp$name,framework=sp$framework,aic=AIC(m))
  }
  if (!length(cand)) return(NULL)
  cand[[which.min(vapply(cand, `[[`, numeric(1), "aic"))]]
}

run_engine_comparison <- function(data, label="All", cfg=CONFIG,
                                  gamlss_sel=NULL, gamlss_nested=NULL, folds=NULL) {
  if (is.null(folds)) folds <- make_age_stratified_folds(data,cfg$NESTED_CV_FOLDS,cfg$SEED)
  if (is.null(gamlss_nested)) gamlss_nested <- nested_gamlss_cv(data,cfg,paste0(label,"-GAMLSS"),folds)
  if (is.null(gamlss_sel)) gamlss_sel <- select_gamlss_hierarchical(data,TRUE,label,cfg)
  gm <- nested_gaussian_engine(data,folds,cfg,"GAM")
  fp <- nested_gaussian_engine(data,folds,cfg,"FP")
  gmet <- gamlss_nested$single
  full_gam <- select_best_gam_train(data,cfg); full_fp <- select_best_fp_train(data,cfg)
  do.call(rbind,list(
    data.frame(Group=label,Engine="GAMLSS",Framework="Distributional",Level=gamlss_sel$level,
               Family=gamlss_sel$best_family,Spec=gamlss_sel$best_spec_name,AIC=gamlss_sel$best_row$AIC,
               BIC=gamlss_sel$best_row$BIC,GAIC=gamlss_sel$best_row$GAIC,CV_RMSE=unname(gmet["RMSE"]),
               CV_MAE=unname(gmet["MAE"]),CV_R2=unname(gmet["R2"]),CV_NLL=unname(gmet["NLL"]),
               PI95_Coverage=unname(gmet["Coverage95"]),CV_Design="Nested selection; pooled covariate-adjusted",stringsAsFactors=FALSE),
    data.frame(Group=label,Engine="GAM",Framework="Gaussian GAM",Level=NA_character_,Family="NO",
               Spec=if(is.null(full_gam)) NA_character_ else full_gam$spec,AIC=if(is.null(full_gam)) NA_real_ else AIC(full_gam$model),
               BIC=if(is.null(full_gam)) NA_real_ else BIC(full_gam$model),GAIC=NA_real_,CV_RMSE=unname(gm$metrics["RMSE"]),
               CV_MAE=unname(gm$metrics["MAE"]),CV_R2=unname(gm$metrics["R2"]),CV_NLL=unname(gm$metrics["NLL"]),
               PI95_Coverage=unname(gm$metrics["Coverage95"]),CV_Design="Nested specification selection; pooled covariate-adjusted",stringsAsFactors=FALSE),
    data.frame(Group=label,Engine="FP/POLY",Framework="Gaussian FP or polynomial",Level=NA_character_,Family="NO",
               Spec=if(is.null(full_fp)) NA_character_ else full_fp$spec,AIC=if(is.null(full_fp)) NA_real_ else AIC(full_fp$model),
               BIC=if(is.null(full_fp)) NA_real_ else BIC(full_fp$model),GAIC=NA_real_,CV_RMSE=unname(fp$metrics["RMSE"]),
               CV_MAE=unname(fp$metrics["MAE"]),CV_R2=unname(fp$metrics["R2"]),CV_NLL=unname(fp$metrics["NLL"]),
               PI95_Coverage=unname(fp$metrics["Coverage95"]),CV_Design="Nested specification selection; pooled covariate-adjusted",stringsAsFactors=FALSE)
  ))
}

# ---- Pooled sex-interaction sensitivity test only (no separate male/female models) ----
test_age_by_sex_interaction <- function(df, sel, cfg=CONFIG) {
  if (!isTRUE(cfg$TEST_AGE_BY_SEX_INTERACTION) || !isTRUE(cfg$ADJUST_SEX)) return(NULL)
  fam <- sel$best_family; mu_p <- sel$best_mu_powers; sig_p <- sel$best_sigma_powers
  m0 <- tryCatch(fit_model(df,fam,mu_p,sig_p,TRUE,FALSE,"Sex-null"),error=function(e)NULL)
  m1 <- tryCatch(fit_model(df,fam,mu_p,sig_p,TRUE,TRUE,"Sex-interaction"),error=function(e)NULL)
  if (is.null(m0) || is.null(m1)) return(NULL)
  l0 <- tryCatch(logLik(m0),error=function(e)NULL); l1 <- tryCatch(logLik(m1),error=function(e)NULL)
  if (is.null(l0)||is.null(l1)) return(NULL)
  df0 <- attr(l0,"df"); df1 <- attr(l1,"df")
  if (!is.finite(df0)||!is.finite(df1)||df1<=df0) return(NULL)
  lr <- max(0,2*(as.numeric(l1)-as.numeric(l0))); dd <- df1-df0
  list(Family=fam,LR=lr,DF=dd,P=pchisq(lr,dd,lower.tail=FALSE),
       AIC_null=extract_gaic(m0,2),AIC_interaction=extract_gaic(m1,2))
}

# ---- One-row summary ----
# [legacy duplicate function removed in public release: build_summary_table_all]

# [legacy duplicate function removed in public release: build_bootstrap_detail_all]

# ---- Main figure: pooled adjusted curve only ----
# [legacy duplicate function removed in public release: create_q1_main_plot]

create_residual_diagnostic_plots <- function(results) {
  z<-sort(results$All$resid[is.finite(results$All$resid)]); n<-length(z)
  qd<-data.frame(Expected=qnorm((seq_len(n)-.5)/n),Observed=z)
  p<-ggplot(qd,aes(Expected,Observed))+geom_abline(slope=1,intercept=0,color="grey60")+geom_point(alpha=.55,size=1)+
    theme_classic(base_size=9)+labs(title="Quantile-residual Q-Q diagnostic: pooled model",x="Expected normal quantile",y="Observed quantile")
  list(qq=p)
}

# [legacy duplicate function removed in public release: save_results]

##############################################################################
# MAIN PIPELINE - ALL ONLY / GENERIC OUTCOME
##############################################################################
# [legacy duplicate function removed in public release: main]



##############################################################################
# FINAL PLOT OVERRIDE v3 -- must remain immediately before main(CONFIG)
# Fixes: human-readable covariates, x-axis through 80, darker panel-b raw points,
#        peak/trough markers in b, peak-velocity marker in c.
##############################################################################
# [legacy duplicate function removed in public release: create_q1_main_plot]

# FINAL PLOT OVERRIDE v4 -- legends for extrema beside panels b and c.
# [legacy duplicate function removed in public release: create_q1_main_plot]

cat("
[PLOT VERSION] FINAL PLOT OVERRIDE v4 active: x-axis to 80 + extrema markers + readable covariates + extrema legends.
")


##############################################################################
# BrainNormR v5.5.1 CORRECTED PUBLICATION / GITHUB OVERRIDES
# ---------------------------------------------------------------------------
# This is the canonical active layer. Earlier definitions are retained only
# for backward compatibility with the developmental history of the script;
# the functions below are the ones used by main(CONFIG).
#
# Adds publication-ready, aggregation-ready outputs:
#   * pooled All-only modelling with outcome-specific covariate adjustment
#   * sex-stratified conditional bootstrap with dynamic success threshold
#   * 9 normative centiles (2.5 ... 97.5)
#   * family-comparable conditional SD and IQR trajectories
#   * derivative + bootstrap CI + derivative per sample SD
#   * scientifically named maximal age-related change + direction
#   * cross-fitted centile calibration table
#   * age-group normative-deviation summaries
#   * nested model-selection stability summary
#   * run metadata and master-ready one-row summary
#   * standardized Excel sheet schema for later multi-outcome aggregation
##############################################################################

# ---- Safe public defaults for metadata/output schema ----
if (is.null(CONFIG$ANALYSIS_LEVEL)) CONFIG$ANALYSIS_LEVEL <- ifelse(normalize_outcome_type(CONFIG$OUTCOME_TYPE)=="Cognition", "Cognition", "ROI")
if (is.null(CONFIG$REGION_NAME)) CONFIG$REGION_NAME <- CONFIG$OUTCOME_COL
if (is.null(CONFIG$HEMISPHERE)) CONFIG$HEMISPHERE <- "NA"
if (is.null(CONFIG$LOBE_NAME)) CONFIG$LOBE_NAME <- "NA"
if (is.null(CONFIG$ID_COL)) CONFIG$ID_COL <- NULL
if (is.null(CONFIG$BOOT_MIN_SUCCESS_PROP)) CONFIG$BOOT_MIN_SUCCESS_PROP <- 0.80
if (is.null(CONFIG$MILESTONE_BOOT_MIN_PROP)) CONFIG$MILESTONE_BOOT_MIN_PROP <- 0.80
if (is.null(CONFIG$AGE_GROUP_BREAKS)) CONFIG$AGE_GROUP_BREAKS <- c(-Inf,30,40,50,60,Inf)
if (is.null(CONFIG$AGE_GROUP_LABELS)) CONFIG$AGE_GROUP_LABELS <- c("G1","G2","G3","G4","G5")

infer_dk_lobe <- function(region_name) {
  x <- tolower(as.character(region_name)[1])
  x <- sub("^(lh_|rh_|left[_ -]?|right[_ -]?)", "", x)
  # Match canonical Desikan-Killiany cortical ROI names even when a measure
  # suffix/prefix is present in the spreadsheet column name.
  frontal <- c("caudalmiddlefrontal","lateralorbitofrontal","medialorbitofrontal",
               "parsopercularis","parsorbitalis","parstriangularis","precentral",
               "rostralmiddlefrontal","superiorfrontal","frontalpole","paracentral")
  temporal <- c("bankssts","entorhinal","fusiform","inferiortemporal","middletemporal",
                "parahippocampal","superiortemporal","temporalpole","transversetemporal")
  parietal <- c("inferiorparietal","postcentral","precuneus","superiorparietal","supramarginal")
  occipital <- c("cuneus","lateraloccipital","lingual","pericalcarine")
  cingulate <- c("caudalanteriorcingulate","isthmuscingulate","posteriorcingulate","rostralanteriorcingulate")
  if (any(vapply(frontal, grepl, logical(1), x=x, fixed=TRUE))) return("Frontal")
  if (any(vapply(temporal, grepl, logical(1), x=x, fixed=TRUE))) return("Temporal")
  if (any(vapply(parietal, grepl, logical(1), x=x, fixed=TRUE))) return("Parietal")
  if (any(vapply(occipital, grepl, logical(1), x=x, fixed=TRUE))) return("Occipital")
  if (any(vapply(cingulate, grepl, logical(1), x=x, fixed=TRUE))) return("Cingulate")
  if (grepl("insula",x,fixed=TRUE)) return("Insula")
  "NA"
}

resolve_analysis_metadata <- function(cfg) {
  typ <- normalize_outcome_type(cfg$OUTCOME_TYPE)

  region_missing <- is.null(cfg$REGION_NAME) || !nzchar(as.character(cfg$REGION_NAME))
  region_equals_outcome <- !region_missing &&
    identical(tolower(as.character(cfg$REGION_NAME)[1]),
              tolower(as.character(cfg$OUTCOME_COL)[1]))

  if (region_missing || region_equals_outcome) {
    if (typ %in% c("Volume", "Area", "Thickness", "Curvature", "FD")) {
      # For canonical structural columns such as lh_insula_volume or
      # lh_insula_FD, retain the anatomical ROI only.
      cfg$REGION_NAME <- sub(
        "_(volume|area|thickness|curvature|fd)$",
        "",
        as.character(cfg$OUTCOME_COL),
        ignore.case = TRUE
      )
    } else {
      cfg$REGION_NAME <- cfg$OUTCOME_COL
    }
  }

  rn <- tolower(as.character(cfg$REGION_NAME))
  lobe_hits <- c(Frontal="frontal",Temporal="temporal",Parietal="parietal",Occipital="occipital",Cingulate="cingulate",Insula="insula")
  is_lobe_aggregate <- any(vapply(lobe_hits,function(z) grepl(z,rn,fixed=TRUE),logical(1))) &&
                       !grepl("^(lh_|rh_|left|right)",rn)
  if (is.null(cfg$ANALYSIS_LEVEL) || toupper(cfg$ANALYSIS_LEVEL)=="AUTO") {
    if (typ=="Cognition") cfg$ANALYSIS_LEVEL <- "Cognition"
    else if (grepl("whole.?brain|global", rn)) cfg$ANALYSIS_LEVEL <- "WholeBrain"
    else if (is_lobe_aggregate) cfg$ANALYSIS_LEVEL <- "Lobe"
    else cfg$ANALYSIS_LEVEL <- "ROI"
  }
  if (is.null(cfg$HEMISPHERE) || toupper(cfg$HEMISPHERE)=="AUTO") {
    if (grepl("^(lh_|left[_ -])",rn)) cfg$HEMISPHERE <- "Left"
    else if (grepl("^(rh_|right[_ -])",rn)) cfg$HEMISPHERE <- "Right"
    else if (cfg$ANALYSIS_LEVEL %in% c("Lobe","WholeBrain")) cfg$HEMISPHERE <- "Bilateral"
    else cfg$HEMISPHERE <- "NA"
  }
  if (is.null(cfg$LOBE_NAME) || toupper(as.character(cfg$LOBE_NAME)) %in% c("AUTO","NA","")) {
    if (cfg$ANALYSIS_LEVEL=="Lobe") {
      hit <- names(lobe_hits)[vapply(lobe_hits,function(z)grepl(z,rn,fixed=TRUE),logical(1))]
      cfg$LOBE_NAME <- if(length(hit))hit[1] else "NA"
    } else if (cfg$ANALYSIS_LEVEL=="ROI") cfg$LOBE_NAME <- infer_dk_lobe(cfg$REGION_NAME)
    else cfg$LOBE_NAME <- "NA"
  }
  cfg
}

# ---- Generic loader retaining optional subject IDs ----
load_data <- function(cfg) {
  cat("[1/9] Loading data...\n")
  raw <- read_excel(cfg$DATA_PATH, sheet = cfg$SHEET_NAME)
  if (!cfg$OUTCOME_COL %in% names(raw)) stop(sprintf("Outcome column '%s' was not found in the input file.", cfg$OUTCOME_COL))
  get_num <- function(colname) {
    if (!is.null(colname) && colname %in% names(raw)) return(suppressWarnings(as.numeric(raw[[colname]])))
    rep(NA_real_, nrow(raw))
  }
  sid <- if (!is.null(cfg$ID_COL) && cfg$ID_COL %in% names(raw)) as.character(raw[[cfg$ID_COL]]) else sprintf("row_%04d", seq_len(nrow(raw)))
  data.frame(
    SubjectID = sid,
    FD   = suppressWarnings(as.numeric(raw[[cfg$OUTCOME_COL]])),
    Age  = get_num(cfg$AGE_COL),
    sex  = get_num(cfg$SEX_COL),
    edu  = get_num(cfg$EDU_COL),
    eTIV = get_num(cfg$ETIV_COL),
    stringsAsFactors = FALSE
  )
}

# ---- Quantile naming and complete normative curve ----
centile_colname <- function(pct) paste0("P", gsub("\\.", "_", format(pct, trim=TRUE, scientific=FALSE)))

predict_curve <- function(model, age_grid, sex_ref = NULL) {
  pred <- predict_parameters(model, age_grid, sex_ref)
  qfun <- make_qfun(pred$family, pred$mu, pred$sigma, pred$nu, pred$tau)
  cents <- sort(unique(as.numeric(CONFIG$CENTILES)))
  qmat <- vapply(cents/100, function(p) qfun(p), numeric(length(age_grid)))
  if (is.vector(qmat)) qmat <- matrix(qmat, nrow=length(age_grid))
  colnames(qmat) <- vapply(cents, centile_colname, character(1))
  qdf <- data.frame(Age=age_grid, qmat, check.names=FALSE)
  p50nm <- centile_colname(50); p25nm <- centile_colname(25); p75nm <- centile_colname(75)
  central <- if (p50nm %in% names(qdf)) qdf[[p50nm]] else qfun(.50)
  lo <- if (centile_colname(2.5) %in% names(qdf)) qdf[[centile_colname(2.5)]] else qfun(.025)
  hi <- if (centile_colname(97.5) %in% names(qdf)) qdf[[centile_colname(97.5)]] else qfun(.975)
  p25 <- if (p25nm %in% names(qdf)) qdf[[p25nm]] else qfun(.25)
  p75 <- if (p75nm %in% names(qdf)) qdf[[p75nm]] else qfun(.75)
  mom <- distribution_moments_from_quantiles(pred$family,pred$mu,pred$sigma,pred$nu,pred$tau)
  dcentral <- safe_gradient(central, age_grid)
  d2central <- safe_gradient(dcentral, age_grid)
  list(
    Age=age_grid, family=pred$family,
    mu=pred$mu, scale_param=pred$sigma,
    sigma=mom$sd, cond_mean=mom$mean, cond_sd=mom$sd,
    nu=pred$nu, tau=pred$tau,
    lo=lo, central=central, hi=hi,
    p25=p25, p75=p75, iqr=p75-p25,
    centiles=qdf,
    dcentral=dcentral, d2central=d2central
  )
}

# ---- Sex-stratified conditional bootstrap ----
bootstrap_indices_stratified <- function(d) {
  if ("Sex" %in% names(d) && length(unique(d$Sex[!is.na(d$Sex)])) >= 2) {
    ii <- split(seq_len(nrow(d)), d$Sex)
    unlist(lapply(ii, function(z) sample(z, length(z), replace=TRUE)), use.names=FALSE)
  } else sample(seq_len(nrow(d)), replace=TRUE)
}

bootstrap_one_iter <- function(d, family_name, mu_powers, sigma_powers,
                               include_sex, age_grid, sex_ref, cfg,
                               iter_seed = NULL) {
  if (!is.null(iter_seed) && is.finite(iter_seed)) set.seed(as.integer(iter_seed))
  idx <- bootstrap_indices_stratified(d)
  db <- d[idx,,drop=FALSE]
  if (include_sex && "Sex" %in% names(db)) {
    if (length(unique(db$Sex)) < 2) return(NULL)
    db$Sex <- factor(db$Sex, levels=levels(d$Sex))
  }
  m <- tryCatch(fit_model(db,family_name,mu_powers,sigma_powers,include_sex,FALSE),error=function(e)NULL)
  if (is.null(m) || !isTRUE(m$converged)) return(NULL)
  curve <- tryCatch(predict_curve(m,age_grid,sex_ref),error=function(e)NULL)
  if (is.null(curve)) return(NULL)
  ex <- tryCatch(find_extrema(curve$Age,curve$central,curve$dcentral,curve$d2central,cfg$BOUNDARY_TOL_PROP),error=function(e)NULL)
  if (is.null(ex)) return(NULL)
  list(stats=c(age_max=ex$age_max,value_max=ex$value_max,age_min=ex$age_min,value_min=ex$value_min,
               age_vel=ex$age_vel,d_vel=ex$d_vel),
       central=curve$central,derivative=curve$dcentral)
}

bootstrap_model <- function(d, family_name, mu_powers, sigma_powers,
                            include_sex, age_grid, sex_ref, n_boot,
                            label="", cfg=CONFIG) {
  cat(sprintf(" [%s] Conditional sex-stratified bootstrap (%d iterations; specification fixed)...\n",label,n_boot))
  res <- compute_lapply(
    seq_len(n_boot),
    function(i) {
      # Some prediction helpers use CONFIG internally; set the worker-local copy.
      CONFIG <<- cfg
      iter_seed <- if (isTRUE(cfg$REPRODUCIBLE_PARALLEL)) {
        stable_task_seed(cfg$SEED, label, i, stage_id = 11L)
      } else {
        NULL
      }
      bootstrap_one_iter(
        d, family_name, mu_powers, sigma_powers,
        include_sex, age_grid, sex_ref, cfg,
        iter_seed = iter_seed
      )
    },
    cfg = cfg,
    future_seed = TRUE
  )
  res <- res[!vapply(res,is.null,logical(1))]
  n_success <- length(res)
  prop_success <- n_success/max(1,n_boot)
  cat(sprintf(" [%s] Successful: %d/%d (%.1f%%)\n",label,n_success,n_boot,100*prop_success))
  min_required <- min(n_boot, max(5L, ceiling(as.numeric(cfg$BOOT_MIN_SUCCESS_PROP)*n_boot)))
  bootstrap_valid <- is.finite(n_success) && n_success >= min_required

  # Low bootstrap convergence is a QC failure for bootstrap-based inference,
  # but it should not erase an otherwise valid full-data fit and downstream
  # non-bootstrap outputs. Continue, flag the failure, and suppress bootstrap
  # CIs / stability claims later. Keep the production threshold unchanged.
  if (!bootstrap_valid) {
    warning(sprintf(
      "[%s] Bootstrap QC FAILED: %d/%d successful (%.1f%%); required >= %d (%.1f%%). Pipeline will continue, but bootstrap-based CIs and stability claims will be suppressed.",
      label, n_success, n_boot, 100*prop_success, min_required,
      100*as.numeric(cfg$BOOT_MIN_SUCCESS_PROP)
    ))
  }

  stat_names <- c("age_max","value_max","age_min","value_min","age_vel","d_vel")
  if (n_success < 5L) {
    stats <- matrix(NA_real_, nrow=length(stat_names), ncol=4,
                    dimnames=list(stat_names, c("median","lower","upper","n_internal")))
    return(list(
      stats=stats, n_success=n_success, n_boot=n_boot, success_prop=prop_success,
      min_required=min_required, valid=FALSE, boot_mat=NULL, curve_ci=NULL,
      ci_type=paste0("Bootstrap QC failed (", n_success, "/", n_boot,
                     " successful; required >= ", min_required,
                     "); bootstrap inference suppressed")
    ))
  }

  boot_mat <- do.call(rbind,lapply(res,`[[`,"stats"))
  min_internal <- if (n_boot < 30) 5L else 20L
  stats <- t(apply(boot_mat,2,function(x){
    xf <- x[is.finite(x)]
    if(length(xf)<min_internal) return(c(median=NA_real_,lower=NA_real_,upper=NA_real_,n_internal=length(xf)))
    q<-quantile(xf,c(.025,.975),na.rm=TRUE)
    c(median=median(xf),lower=unname(q[1]),upper=unname(q[2]),n_internal=length(xf))
  }))
  curve_ci <- NULL
  if (isTRUE(cfg$STORE_BOOT_CURVES) && isTRUE(bootstrap_valid)) {
    cm <- do.call(rbind,lapply(res,`[[`,"central")); dm <- do.call(rbind,lapply(res,`[[`,"derivative"))
    qrow <- function(m,p) apply(m,2,quantile,probs=p,na.rm=TRUE)
    curve_ci <- data.frame(Age=age_grid,
                           central_lower=qrow(cm,.025),central_median=qrow(cm,.5),central_upper=qrow(cm,.975),
                           deriv_lower=qrow(dm,.025),deriv_median=qrow(dm,.5),deriv_upper=qrow(dm,.975))
  }
  list(
    stats=stats, n_success=n_success, n_boot=n_boot, success_prop=prop_success,
    min_required=min_required, valid=bootstrap_valid, boot_mat=boot_mat, curve_ci=curve_ci,
    ci_type=if (isTRUE(bootstrap_valid)) {
      "Sex-stratified conditional bootstrap; family and functional forms fixed at full-sample selection"
    } else {
      paste0("Bootstrap QC failed (", n_success, "/", n_boot,
             " successful; required >= ", min_required,
             "); bootstrap inference suppressed")
    }
  )
}

# ---- Publication terminology ----
change_direction <- function(d) {
  if (!is.finite(d)) return(NA_character_)
  if (d > 0) "Increase" else if (d < 0) "Decline" else "Stable"
}

# ============================================================================
# Bootstrap support for internal milestones
# A milestone is reportable only when:
#   1) it exists in the full-sample fitted trajectory, AND
#   2) it is reproduced in >= MILESTONE_BOOT_MIN_PROP of successful bootstraps.
# ============================================================================

milestone_boot_support <- function(ex, boot, kind, cfg = CONFIG) {
  
  if (is.null(boot) ||
      is.null(boot$stats) ||
      is.null(boot$n_success) ||
      !is.finite(boot$n_success) ||
      boot$n_success <= 0) {
    return(list(
      full_detected = FALSE,
      n_internal = 0L,
      support_prop = NA_real_,
      stable = FALSE
    ))
  }
  
  overall_boot_valid <- if (!is.null(boot$valid)) {
    isTRUE(boot$valid)
  } else {
    is.finite(boot$n_success) && is.finite(boot$n_boot) && boot$n_boot > 0 &&
      (boot$n_success / boot$n_boot) >= as.numeric(cfg$BOOT_MIN_SUCCESS_PROP)
  }

  if (kind == "peak") {
    full_detected <- isTRUE(ex$has_internal_peak)
    stat_name <- "age_max"
    
  } else if (kind == "trough") {
    full_detected <- isTRUE(ex$has_internal_trough)
    stat_name <- "age_min"
    
  } else if (kind == "velocity") {
    full_detected <- isTRUE(ex$has_internal_velocity)
    stat_name <- "age_vel"
    
  } else {
    stop("Unknown milestone kind: ", kind)
  }
  
  if (!stat_name %in% rownames(boot$stats)) {
    return(list(
      full_detected = full_detected,
      n_internal = 0L,
      support_prop = 0,
      stable = FALSE
    ))
  }
  
  n_internal <- as.numeric(
    boot$stats[stat_name, "n_internal"]
  )
  
  support_prop <- n_internal / boot$n_success
  
  min_prop <- if (!is.null(cfg$MILESTONE_BOOT_MIN_PROP)) {
    cfg$MILESTONE_BOOT_MIN_PROP
  } else {
    0.80
  }
  
  stable <- isTRUE(overall_boot_valid) &&
    isTRUE(full_detected) &&
    is.finite(support_prop) &&
    support_prop >= min_prop
  
  list(
    full_detected = full_detected,
    n_internal = n_internal,
    support_prop = support_prop,
    stable = stable
  )
}


build_bootstrap_detail_all <- function(ex, boot, cfg = CONFIG) {
  
  sn <- c(
    "age_max",
    "value_max",
    "age_min",
    "value_min",
    "age_vel",
    "d_vel"
  )
  
  lab <- c(
    "Peak Age",
    "Peak Value",
    "Trough Age",
    "Trough Value",
    "Maximal Change Age",
    "Derivative at Maximal Change"
  )
  
  kind <- c(
    "peak",
    "peak",
    "trough",
    "trough",
    "velocity",
    "velocity"
  )
  
  est <- c(
    ex$age_max,
    ex$value_max,
    ex$age_min,
    ex$value_min,
    ex$age_vel,
    ex$d_vel
  )
  
  med <- as.numeric(boot$stats[sn, "median"])
  lo  <- as.numeric(boot$stats[sn, "lower"])
  hi  <- as.numeric(boot$stats[sn, "upper"])
  ni  <- as.numeric(boot$stats[sn, "n_internal"])
  
  support <- lapply(
    unique(kind),
    function(k) milestone_boot_support(ex, boot, k, cfg)
  )
  names(support) <- unique(kind)
  
  support_prop <- vapply(
    kind,
    function(k) support[[k]]$support_prop,
    numeric(1)
  )
  
  stable <- vapply(
    kind,
    function(k) support[[k]]$stable,
    logical(1)
  )
  
  # IMPORTANT:
  # Do not report point estimate / CI as a stable milestone when
  # the full-data trajectory does not contain it or bootstrap support is weak.
  # Preserve the full-sample point estimate for transparency, but suppress
  # bootstrap median/CI when the milestone is not sufficiently reproducible.
  med[!stable] <- NA_real_
  lo[!stable]  <- NA_real_
  hi[!stable]  <- NA_real_
  
  data.frame(
    Group = "All",
    Statistic = lab,
    Estimate = est,
    Median = med,
    Lower95 = lo,
    Upper95 = hi,
    N_Internal_Boot = ni,
    BootstrapSupportProp = support_prop,
    StableMilestone = stable,
    CI_Type = boot$ci_type,
    stringsAsFactors = FALSE
  )
}

ci_parts <- function(boot, stat, ex = NULL, kind = NULL, cfg = CONFIG) {
  
  if (is.null(boot$stats) ||
      !stat %in% rownames(boot$stats)) {
    return(c(NA_real_, NA_real_))
  }
  
  # For milestone CIs, require full-sample detection + sufficient bootstrap support
  if (!is.null(ex) && !is.null(kind)) {
    
    sup <- milestone_boot_support(
      ex = ex,
      boot = boot,
      kind = kind,
      cfg = cfg
    )
    
    if (!isTRUE(sup$stable)) {
      return(c(NA_real_, NA_real_))
    }
  }
  
  c(
    as.numeric(boot$stats[stat, "lower"]),
    as.numeric(boot$stats[stat, "upper"])
  )
}

# ---- Selection stability from nested folds ----
selection_stability_table <- function(selection_table, full_sel=NULL) {
  if (is.null(selection_table) || !nrow(selection_table)) return(data.frame(
    Component=c("Family","MuSpec","SigmaSpec"),FullSample=NA_character_,ModalNested=NA_character_,ModalProportion=NA_real_,MatchFullProportion=NA_real_))
  comps <- c("Family","MuSpec","SigmaSpec")
  full_vals <- c(if(!is.null(full_sel)) full_sel$best_family else NA_character_,
                 if(!is.null(full_sel)) full_sel$best_mu_spec else NA_character_,
                 if(!is.null(full_sel)) full_sel$best_sigma_spec else NA_character_)
  out <- lapply(seq_along(comps),function(i){
    nm<-comps[i]; x<-as.character(selection_table[[nm]]); x<-x[!is.na(x)&nzchar(x)]
    if(!length(x)) return(data.frame(Component=nm,FullSample=full_vals[i],ModalNested=NA_character_,ModalProportion=NA_real_,MatchFullProportion=NA_real_))
    tb<-sort(table(x),decreasing=TRUE); modal<-names(tb)[1]
    data.frame(Component=nm,FullSample=full_vals[i],ModalNested=modal,
               ModalProportion=as.numeric(tb[1])/length(x),
               MatchFullProportion=mean(x==full_vals[i]))
  })
  do.call(rbind,out)
}

# ---- Cross-fitted centile calibration ----
build_centile_calibration <- function(norm, cfg=CONFIG) {
  # Calibration must be out-of-sample whenever cross-fitted probabilities exist.
  p_src <- if ("CF_Prob" %in% names(norm) && any(is.finite(norm$CF_Prob))) norm$CF_Prob else norm$Norm_Prob
  p <- p_src[is.finite(p_src)]
  cents <- sort(unique(as.numeric(cfg$CENTILES)))
  if (!length(p)) return(data.frame())
  data.frame(NominalCentile=cents,ExpectedProportion=cents/100,
             ObservedProportion=vapply(cents/100,function(q)mean(p<=q),numeric(1)),
             CalibrationError=vapply(cents/100,function(q)mean(p<=q)-q,numeric(1)))
}

# ---- Age-group deviation summary for lollipop / heatmap figures ----
build_age_group_deviation <- function(norm, cfg=CONFIG) {
  br<-as.numeric(cfg$AGE_GROUP_BREAKS); lb<-as.character(cfg$AGE_GROUP_LABELS)
  if(length(br)!=length(lb)+1) stop("AGE_GROUP_BREAKS must have exactly one more element than AGE_GROUP_LABELS.")
  g<-cut(norm$Age,breaks=br,labels=lb,right=FALSE,include.lowest=TRUE)
  rows<-lapply(lb,function(z){
    z_src <- if ("CF_Norm_Z" %in% names(norm) && any(is.finite(norm$CF_Norm_Z))) norm$CF_Norm_Z else norm$Norm_Z
    ii<-which(g==z & is.finite(z_src)); zz<-z_src[ii]
    data.frame(Group=z,AgeMin=if(length(ii))min(norm$Age[ii]) else NA_real_,AgeMax=if(length(ii))max(norm$Age[ii]) else NA_real_,
               N=length(zz),Median_Z=if(length(zz))median(zz) else NA_real_,Q25_Z=if(length(zz))quantile(zz,.25,names=FALSE) else NA_real_,
               Q75_Z=if(length(zz))quantile(zz,.75,names=FALSE) else NA_real_,IQR_Z=if(length(zz))IQR(zz) else NA_real_,
               Mean_Z=if(length(zz))mean(zz) else NA_real_,SD_Z=if(length(zz)>1)sd(zz) else NA_real_)
  })
  do.call(rbind,rows)
}

# ---- Master-ready curve sheets ----
build_centile_curve_sheet <- function(curve,cfg=CONFIG) {
  md <- data.frame(Outcome=cfg$OUTCOME_COL,OutcomeType=normalize_outcome_type(cfg$OUTCOME_TYPE),
                   Level=cfg$ANALYSIS_LEVEL,Region=cfg$REGION_NAME,Hemisphere=cfg$HEMISPHERE,Lobe=cfg$LOBE_NAME,
                   stringsAsFactors=FALSE)
  cbind(md[rep(1,nrow(curve$centiles)),,drop=FALSE],curve$centiles)
}

build_derivative_curve_sheet <- function(curve,boot,sample_sd,cfg=CONFIG) {
  ci<-boot$curve_ci
  lo<-if(!is.null(ci))ci$deriv_lower else rep(NA_real_,length(curve$Age))
  hi<-if(!is.null(ci))ci$deriv_upper else rep(NA_real_,length(curve$Age))
  data.frame(Outcome=cfg$OUTCOME_COL,OutcomeType=normalize_outcome_type(cfg$OUTCOME_TYPE),Level=cfg$ANALYSIS_LEVEL,
             Region=cfg$REGION_NAME,Hemisphere=cfg$HEMISPHERE,Lobe=cfg$LOBE_NAME,
             Age=curve$Age,Derivative=curve$dcentral,Derivative_L95=lo,Derivative_U95=hi,
             Derivative_per_SD=if(is.finite(sample_sd)&&sample_sd>0)curve$dcentral/sample_sd else NA_real_)
}

build_variability_curve_sheet <- function(curve,cfg=CONFIG) {
  data.frame(Outcome=cfg$OUTCOME_COL,OutcomeType=normalize_outcome_type(cfg$OUTCOME_TYPE),Level=cfg$ANALYSIS_LEVEL,
             Region=cfg$REGION_NAME,Hemisphere=cfg$HEMISPHERE,Lobe=cfg$LOBE_NAME,
             Age=curve$Age,Conditional_SD=curve$cond_sd,P25=curve$p25,P75=curve$p75,IQR=curve$iqr,
             FamilyScaleParameter=curve$scale_param)
}

# ---- Expanded one-row summary for later MASTER_RESULTS bind_rows() ----
build_summary_table_all <- function(diag,ex,boot,sel,cfg=CONFIG,validation=NULL,sample_n=NA_integer_,age_min=NA_real_,age_max=NA_real_,sample_sd=NA_real_,selection_stability=NULL) {
  peakci <- ci_parts(
    boot, "age_max",
    ex = ex,
    kind = "peak",
    cfg = cfg
  )
  
  troughci <- ci_parts(
    boot, "age_min",
    ex = ex,
    kind = "trough",
    cfg = cfg
  )
  
  velci <- ci_parts(
    boot, "age_vel",
    ex = ex,
    kind = "velocity",
    cfg = cfg
  )
  peak_sup   <- milestone_boot_support(ex, boot, "peak", cfg)
  trough_sup <- milestone_boot_support(ex, boot, "trough", cfg)
  vel_sup    <- milestone_boot_support(ex, boot, "velocity", cfg)
  vm<-if(!is.null(validation))validation$nested_cv$single else NULL
  famstab<-mustab<-sigstab<-NA_real_
  if(!is.null(selection_stability)&&nrow(selection_stability)){
    famstab<-selection_stability$MatchFullProportion[selection_stability$Component=="Family"][1]
    mustab<-selection_stability$MatchFullProportion[selection_stability$Component=="MuSpec"][1]
    sigstab<-selection_stability$MatchFullProportion[selection_stability$Component=="SigmaSpec"][1]
  }
  data.frame(
    Group="All",Outcome=cfg$OUTCOME_COL,OutcomeType=normalize_outcome_type(cfg$OUTCOME_TYPE),Level=cfg$ANALYSIS_LEVEL,
    Region=cfg$REGION_NAME,Hemisphere=cfg$HEMISPHERE,Lobe=cfg$LOBE_NAME,N=sample_n,AgeMin=age_min,AgeMax=age_max,
    AdjustedFor=paste(pretty_active_covariates(cfg),collapse=" + "),Family=sel$best_family,MuSpec=sel$best_mu_spec,SigmaSpec=sel$best_sigma_spec,
    AIC=if(!is.null(sel$best_row))sel$best_row$AIC else NA_real_,BIC=if(!is.null(sel$best_row))sel$best_row$BIC else NA_real_,GAIC=if(!is.null(sel$best_row))sel$best_row$GAIC else NA_real_,
    InternalPeak=isTRUE(ex$has_internal_peak),PeakBootstrapSupport=peak_sup$support_prop,
    PeakStable=isTRUE(peak_sup$stable),
    PeakAge=ex$age_max,PeakAge_L95=peakci[1],PeakAge_U95=peakci[2],PeakValue=ex$value_max,
    InternalTrough=isTRUE(ex$has_internal_trough),TroughBootstrapSupport=trough_sup$support_prop,
    TroughStable=isTRUE(trough_sup$stable),
    TroughAge=ex$age_min,TroughAge_L95=troughci[1],TroughAge_U95=troughci[2],TroughValue=ex$value_min,
    InternalMaxChange=isTRUE(ex$has_internal_velocity),MaxChangeBootstrapSupport=vel_sup$support_prop,
    MaxChangeStable=isTRUE(vel_sup$stable) && !isTRUE(ex$boundary_vel),
    MaxChangeAtInteriorBoundary=isTRUE(ex$boundary_vel),
    MaxChangeNote=as.character(ex$velocity_note),
    MaxChangeAge=ex$age_vel,MaxChangeAge_L95=velci[1],MaxChangeAge_U95=velci[2],
    DerivativeAtMaxChange=ex$d_vel,ChangeDirection=change_direction(ex$d_vel),SampleSD=sample_sd,
    CV_RMSE=if(!is.null(vm))unname(vm["RMSE"])else NA_real_,CV_MAE=if(!is.null(vm))unname(vm["MAE"])else NA_real_,
    CV_R2=if(!is.null(vm))unname(vm["R2"])else NA_real_,CV_NLL=if(!is.null(vm))unname(vm["NLL"])else NA_real_,
    Coverage95=if(!is.null(vm))unname(vm["Coverage95"])else NA_real_,PIT_p=if(!is.null(validation))validation$pit_ks_p else NA_real_,
    CrossFit_Z_Mean=if(!is.null(validation))validation$z_mean else NA_real_,CrossFit_Z_SD=if(!is.null(validation))validation$z_sd else NA_real_,
    BootstrapSuccessProp=if(!is.null(boot$success_prop))boot$success_prop else boot$n_success/boot$n_boot,
    BootstrapValid=if(!is.null(boot$valid))isTRUE(boot$valid) else ((boot$n_success/boot$n_boot) >= as.numeric(cfg$BOOT_MIN_SUCCESS_PROP)),
    PeakInternalBootProp=if(!is.null(validation))validation$peak_stability$internal_prop else NA_real_,PeakMAD=if(!is.null(validation))validation$peak_stability$mad else NA_real_,
    FamilySelectionStability=famstab,MuSelectionStability=mustab,SigmaSelectionStability=sigstab,
    Shapiro_p=get_val(diag$shapiro$p.value),AD_p=get_val(diag$ad$p.value),BootstrapCIType=boot$ci_type,
    check.names=FALSE,stringsAsFactors=FALSE)
}

# ---- Run metadata: long key-value form is merge-friendly and GitHub-readable ----
build_run_metadata <- function(df,results,cfg=CONFIG) {
  cv<-attr(df,"center_values")
  vals<-list(
    PipelineVersion="BrainNormR v5.5.1 CORRECTED",RunTimestamp=as.character(Sys.time()),Outcome=cfg$OUTCOME_COL,
    OutcomeType=normalize_outcome_type(cfg$OUTCOME_TYPE),AnalysisLevel=cfg$ANALYSIS_LEVEL,Region=cfg$REGION_NAME,
    Hemisphere=cfg$HEMISPHERE,Lobe=cfg$LOBE_NAME,N=nrow(df),AgeMin=min(df$Age),AgeMax=max(df$Age),
    FemaleN=sum(df$Sex=="Female"),MaleN=sum(df$Sex=="Male"),AdjustedFor=paste(pretty_active_covariates(cfg),collapse=" + "),
    Seed=cfg$SEED,N_BOOT=cfg$N_BOOT,BootMinSuccessProp=cfg$BOOT_MIN_SUCCESS_PROP,
    MilestoneBootMinProp=cfg$MILESTONE_BOOT_MIN_PROP,GridSize=cfg$GRID_SIZE,
    GAMLSSSelectionCriterion="GAIC(k=3), with BIC/AIC retained; nested CV used for out-of-sample validation",
    AgeSexFDRNote="BH-FDR is intentionally applied after multi-outcome aggregation, not within a single-outcome run",
    NestedCVFolds=cfg$NESTED_CV_FOLDS,EngineCVFolds=cfg$ENGINE_CV_FOLDS,EngineMetric=cfg$ENGINE_METRIC,
    Centiles=paste(cfg$CENTILES,collapse=","),BoundaryTolerance=cfg$BOUNDARY_TOL_PROP,
    ScaleModel="Age-dependent sigma selected independently; demographic covariates not forced into sigma",
    MeanEducation=if(!is.null(cv))cv$edu_mean else NA_real_,MeanETIV=if(!is.null(cv))cv$eTIV_mean else NA_real_,
    MaleProportion=if(!is.null(cv))cv$male_proportion else NA_real_,RVersion=R.version.string)
  data.frame(Key=names(vals),Value=vapply(vals,function(x)paste(x,collapse=""),character(1)),stringsAsFactors=FALSE)
}

# ---- Final publication figure terminology ----
create_q1_main_plot <- function(df,results,cfg) {
  th<-theme_classic(base_size=9)+
    theme(
      plot.tag=element_text(face="bold",size=12),
      legend.title=element_blank()
    )

  xlim_age<-c(min(20,floor(min(df$Age,na.rm=TRUE))),80)
  xbreaks<-c(20,40,60,80)

  # --------------------------------------------------------------------------
  # Panel a: raw data
  # --------------------------------------------------------------------------
  pa<-ggplot(df,aes(Age,FD,color=Sex))+
    geom_point(alpha=.45,size=1.4)+
    scale_color_manual(values=c(Female="#E74C3C",Male="#2980B9"))+
    scale_x_continuous(
      breaks=xbreaks,
      limits=xlim_age,
      expand=expansion(mult=c(.01,.01))
    )+
    labs(
      title="Raw data",
      x="Age (years)",
      y=cfg$Y_LABEL,
      tag="a"
    )+
    th

  # --------------------------------------------------------------------------
  # Panel b: pooled normative trajectory
  # --------------------------------------------------------------------------
  cdf<-data.frame(
    Age=results$All$Age,
    central=results$All$central,
    lo=results$All$lo,
    hi=results$All$hi
  )

  ex<-results$All$extrema

  pts_b<-data.frame(
    age=numeric(),
    y=numeric(),
    kind=character()
  )

  if(isTRUE(ex$has_internal_peak)){
    pts_b<-rbind(
      pts_b,
      data.frame(
        age=ex$age_max,
        y=ex$value_max,
        kind="Peak (max)"
      )
    )
  }

  if(isTRUE(ex$has_internal_trough)){
    pts_b<-rbind(
      pts_b,
      data.frame(
        age=ex$age_min,
        y=ex$value_min,
        kind="Trough (min)"
      )
    )
  }

  if(nrow(pts_b)){
    pts_b$kind<-factor(
      pts_b$kind,
      levels=unique(pts_b$kind)
    )
  }

  pb<-ggplot()+
    geom_point(
      data=df,
      aes(Age,FD),
      alpha=.30,
      size=1.05,
      color="grey35"
    )+
    geom_ribbon(
      data=cdf,
      aes(Age,ymin=lo,ymax=hi),
      alpha=.16
    )+
    geom_line(
      data=cdf,
      aes(Age,central),
      linewidth=1
    )+
    {
      if(nrow(pts_b))
        geom_point(
          data=pts_b,
          aes(age,y,shape=kind),
          inherit.aes=FALSE,
          size=3.4,
          stroke=1,
          color="black",
          fill="white",
          show.legend=TRUE
        )
    }+
    scale_shape_manual(
      values=c(
        "Peak (max)"=24,
        "Trough (min)"=25
      ),
      drop=TRUE
    )+
    scale_x_continuous(
      breaks=xbreaks,
      limits=xlim_age,
      expand=expansion(mult=c(.01,.01))
    )+
    labs(
      title="Pooled covariate-adjusted normative trajectory",
      subtitle=sprintf(
        "2.5th/50th/97.5th centiles; adjusted for %s",
        paste(pretty_active_covariates(cfg),collapse=", ")
      ),
      x="Age (years)",
      y=cfg$Y_LABEL,
      tag="b"
    )+
    th+
    theme(legend.position="right")

  # --------------------------------------------------------------------------
  # Panel c: conditional variability across age
  # IMPORTANT:
  # results$All$sigma is the distribution-derived conditional SD,
  # not the raw family-specific GAMLSS sigma parameter.
  # The raw family-specific scale parameter remains available in
  # results$All$scale_param and the VariabilityCurve Excel sheet.
  #
  # Plotting safeguard:
  # When Conditional SD is essentially constant, ggplot's automatic y-axis
  # can magnify floating-point differences and make them look like large
  # biological spikes. We NEVER replace or smooth the original SD values.
  # We only use a scientifically sensible y-window for visualization.
  # --------------------------------------------------------------------------
  vdf<-data.frame(
    Age=results$All$Age,
    Conditional_SD=results$All$sigma
  )

  sd_ok <- is.finite(vdf$Conditional_SD)
  sd_mean <- if (any(sd_ok)) mean(vdf$Conditional_SD[sd_ok]) else NA_real_
  sd_range <- if (any(sd_ok)) diff(range(vdf$Conditional_SD[sd_ok])) else NA_real_
  sd_rel_range <- if (is.finite(sd_mean) && abs(sd_mean) > .Machine$double.eps) {
    sd_range / abs(sd_mean)
  } else {
    NA_real_
  }

  flat_tol <- cfg$VARIABILITY_FLAT_REL_TOL
  if (is.null(flat_tol) || !is.finite(flat_tol) || flat_tol <= 0) flat_tol <- 0.005

  sd_essentially_constant <- is.finite(sd_rel_range) && sd_rel_range < flat_tol

  if (sd_essentially_constant) {
    # Show a +/-1% window around the mean SD (at least twice the flatness
    # tolerance). This leaves the fitted values untouched but prevents
    # microscopic numerical jitter from filling the entire panel.
    y_halfspan <- max(abs(sd_mean) * max(0.01, 2 * flat_tol),
                      sqrt(.Machine$double.eps))
    sd_ylim <- c(max(0, sd_mean - y_halfspan), sd_mean + y_halfspan)
    sd_subtitle <- sprintf(
      "Conditional SD essentially constant across age (relative range = %.4f%%)",
      100 * sd_rel_range
    )
  } else {
    sd_ylim <- NULL
    sd_subtitle <- "Conditional SD derived from the fitted distribution"
  }

  pc<-ggplot(vdf,aes(Age,Conditional_SD))+
    geom_line(linewidth=1)+
    scale_x_continuous(
      breaks=xbreaks,
      limits=xlim_age,
      expand=expansion(mult=c(.01,.01))
    )+
    {
      if (!is.null(sd_ylim)) coord_cartesian(ylim=sd_ylim) else NULL
    }+
    labs(
      title="Conditional variability across age",
      subtitle=sd_subtitle,
      x="Age (years)",
      y="Conditional SD",
      tag="c"
    )+
    th

  # --------------------------------------------------------------------------
  # Panel d: derivative / cross-sectional age gradient
  # --------------------------------------------------------------------------
  ci<-results$All$bootstrap$curve_ci

  ddf<-if(!is.null(ci)){
    data.frame(
      Age=ci$Age,
      lo=ci$deriv_lower,
      mid=results$All$dcentral,
      hi=ci$deriv_upper
    )
  } else {
    data.frame(
      Age=results$All$Age,
      lo=NA_real_,
      mid=results$All$dcentral,
      hi=NA_real_
    )
  }

  pts_d<-if(isTRUE(ex$has_internal_velocity)){
    data.frame(
      age=ex$age_vel,
      y=ex$d_vel,
      kind="Maximal age-related change"
    )
  } else {
    data.frame(
      age=numeric(),
      y=numeric(),
      kind=character()
    )
  }

  pd<-ggplot(ddf,aes(Age,mid))+
    geom_hline(
      yintercept=0,
      color="grey70"
    )+
    {
      if(any(is.finite(ddf$lo)))
        geom_ribbon(
          aes(ymin=lo,ymax=hi),
          alpha=.14
        )
    }+
    geom_line(linewidth=1)+
    {
      if(nrow(pts_d))
        geom_point(
          data=pts_d,
          aes(age,y,shape=kind),
          inherit.aes=FALSE,
          size=3.5,
          stroke=1,
          color="black",
          fill="white",
          show.legend=TRUE
        )
    }+
    scale_shape_manual(
      values=c(
        "Maximal age-related change"=23
      ),
      drop=TRUE
    )+
    scale_x_continuous(
      breaks=xbreaks,
      limits=xlim_age,
      expand=expansion(mult=c(.01,.01))
    )+
    labs(
      title="Cross-sectional age gradient",
      subtitle="Derivative of adjusted conditional median; ribbon = conditional-bootstrap 95% CI",
      x="Age (years)",
      y="d(Median)/dAge",
      tag="d"
    )+
    th+
    theme(legend.position="right")

  # --------------------------------------------------------------------------
  # Panel e: cross-fitted individual normative deviation
  # --------------------------------------------------------------------------
  norm<-results$All$normative

  norm$Plot_Norm_Z <- if ("CF_Norm_Z" %in% names(norm) && any(is.finite(norm$CF_Norm_Z))) norm$CF_Norm_Z else norm$Norm_Z
  pe<-ggplot(norm,aes(Age,Plot_Norm_Z,color=Sex))+
    geom_hline(
      yintercept=c(-2,0,2),
      linetype=c("dashed","solid","dashed"),
      color="grey55"
    )+
    geom_point(alpha=.5,size=1.25)+
    scale_color_manual(
      values=c(
        Female="#E74C3C",
        Male="#2980B9"
      )
    )+
    scale_x_continuous(
      breaks=xbreaks,
      limits=xlim_age,
      expand=expansion(mult=c(.01,.01))
    )+
    labs(
      title="Cross-fitted individual normative deviation",
      subtitle="Each participant excluded from selection and fitting for their own score",
      x="Age (years)",
      y="Normative deviation (z)",
      tag="e"
    )+
    th

  # Compact five-panel layout suitable for thesis/article output:
  #   a
  # b | c
  # d | e
  final_plot <- pa / (pb | pc) / (pd | pe) +
    patchwork::plot_layout(
      heights=c(.72,1,1)
    )

  list(
    plot=final_plot,
    curve_df=cdf
  )
}

# ---- Final Excel writer with stable cross-study schema ----
save_results <- function(results,plots,cfg) {
  cat("[10/10] Saving publication-ready all-only results...\n")
  safe<-gsub("[^A-Za-z0-9_]+","_",cfg$OUTCOME_COL);dir.create(cfg$OUT_DIR,recursive=TRUE,showWarnings=FALSE)
  if(!is.null(plots$plot)){
    ggsave(file.path(cfg$OUT_DIR,sprintf("NormativeModel_Q1_AllOnly_%s.png",safe)),plots$plot,width=10.5,height=8,dpi=600,bg="white")
    ggsave(file.path(cfg$OUT_DIR,sprintf("NormativeModel_Q1_AllOnly_%s.pdf",safe)),plots$plot,width=10.5,height=8,bg="white")
  }
  dp<-create_residual_diagnostic_plots(results)
  if(!is.null(dp$qq))ggsave(file.path(cfg$OUT_DIR,sprintf("Residual_QQ_AllOnly_%s.pdf",safe)),dp$qq,width=5,height=4,bg="white")
  try({pdf(file.path(cfg$OUT_DIR,sprintf("WormPlot_AllOnly_%s.pdf",safe)),width=7,height=5);gamlss::wp(results$All$best_model,xvar=results$All$best_model$data$Age,n.inter=4);dev.off()},silent=TRUE)
  if(dev.cur()>1)try(dev.off(),silent=TRUE)

  # Generic, outcome-agnostic normative-score export.
  # Internal response remains named FD for backward compatibility only.
  norm_export <- results$All$normative
  if ("FD" %in% names(norm_export)) {
    names(norm_export)[names(norm_export) == "FD"] <- "ObservedValue"
  }
  if ("FD_Normative_Z" %in% names(norm_export)) {
    norm_export$FD_Normative_Z <- NULL
  }
  if ("Normative_Z" %in% names(norm_export)) {
    norm_export$Normative_Z <- NULL
  }

  excel<-list(
    Summary=results$summary_table,
    ModelSelection=results$family_comparison,
    NestedCV=results$validation$All$crossfit_data,
    NestedSelection=results$validation$All$selection_table,
    SelectionStability=results$selection_stability,
    ValidationSummary=results$validation_summary,
    BootstrapSummary=results$bootstrap_detail,
    ResidualTests=results$residual_tests,
    CentileCurves=results$centile_curves,
    CurveData=results$centile_curves,  # backward-compatible alias for earlier plotting scripts
    DerivativeCurve=results$derivative_curve,
    VariabilityCurve=results$variability_curve,
    Milestones=results$bootstrap_detail,
    NormativeScores=norm_export,
    AgeGroupDeviation=results$age_group_deviation,
    Calibration=results$centile_calibration,
    EngineComparisonAll=results$engine_comparison_all,
    EngineComparisonBest=results$engine_comparison_best,
    RunMetadata=results$run_metadata
  )
  if(!is.null(results$sex_interaction))excel$SexInteraction<-as.data.frame(results$sex_interaction,stringsAsFactors=FALSE)
  # Remove NULL/unsupported sheets defensively.
  excel<-excel[!vapply(excel,is.null,logical(1))]
  write_xlsx(excel,file.path(cfg$OUT_DIR,sprintf("Summary_results_Q1_AllOnly_%s.xlsx",safe)))
  capture.output(sessionInfo(),file=file.path(cfg$OUT_DIR,sprintf("sessionInfo_%s.txt",safe)))
  cat(sprintf(" Output saved to: %s\n",cfg$OUT_DIR));invisible(TRUE)
}

# ---- Canonical final main ----
main <- function(cfg=CONFIG) {
  run_start <- Sys.time()

  cfg$OUTCOME_TYPE <- normalize_outcome_type(cfg$OUTCOME_TYPE)
  cfg$FD_COL <- cfg$OUTCOME_COL
  cfg <- resolve_analysis_metadata(cfg)

  CONFIG <<- cfg
  set.seed(cfg$SEED)

  backend <- configure_compute_backend(cfg)

  cat("\n============================================================\n BrainNormR v5.5.1 CORRECTED: pooled normative modelling\n============================================================\n")
  cat(sprintf(
    " Outcome: %s | Type: %s | Level: %s | Region: %s\n",
    cfg$OUTCOME_COL, cfg$OUTCOME_TYPE, cfg$ANALYSIS_LEVEL, cfg$REGION_NAME
  ))
  cat(sprintf(
    " Location-model adjustment: %s\n",
    paste(pretty_active_covariates(cfg), collapse = " + ")
  ))

  df <- load_data(cfg)
  df <- clean_data(df, cfg)
  validate_data(df, cfg)

  sample_sd <- sd(df$FD, na.rm = TRUE)
  age_grid <- seq(
    min(df$Age),
    max(df$Age),
    length.out = cfg$GRID_SIZE
  )

  cat("\n--- Full-data pooled GAMLSS selection ---\n")
  t0 <- Sys.time()
  sel <- select_gamlss_hierarchical(df, TRUE, "All", cfg)
  search_sec <- elapsed_seconds(t0)

  # Save final fitted GAMLSS object for equation extraction.
  model_save_dir <- file.path(cfg$OUT_DIR, "Models")
  dir.create(model_save_dir, recursive = TRUE, showWarnings = FALSE)

  model_id <- gsub("[^A-Za-z0-9_\\-]+", "_", cfg$OUTCOME_COL)

  saveRDS(
    sel$best_model,
    file.path(
      model_save_dir,
      paste0(model_id, "_All_final_GAMLSS_model.rds")
    )
  )

  cat(sprintf(
    " Final GAMLSS model saved for equation export: %s\n",
    file.path(
      model_save_dir,
      paste0(model_id, "_All_final_GAMLSS_model.rds")
    )
  ))

  curve <- predict_curve(sel$best_model, age_grid)

  ex <- find_extrema(
    curve$Age,
    curve$central,
    curve$dcentral,
    curve$d2central,
    cfg$BOUNDARY_TOL_PROP
  )

  cat(sprintf(
    " All | peak=%s | trough=%s | maximal-change=%s (%s)\n",
    get_val(ex$age_max),
    get_val(ex$age_min),
    get_val(ex$age_vel),
    change_direction(ex$d_vel)
  ))

  cat("\n--- Conditional bootstrap ---\n")
  t0 <- Sys.time()
  boot <- bootstrap_model(
    df,
    sel$best_family,
    sel$best_mu_powers,
    sel$best_sigma_powers,
    TRUE,
    age_grid,
    NULL,
    cfg$N_BOOT,
    "All",
    cfg
  )
  bootstrap_sec <- elapsed_seconds(t0)

  cat("\n--- Residual diagnostics ---\n")
  qres <- compute_quantile_residuals(sel$best_model)
  diag <- residual_diagnostics(qres, "All")

  residual_tests <- data.frame(
    Group = "All",
    Shapiro_W = as.numeric(diag$shapiro$statistic),
    Shapiro_p = diag$shapiro$p.value,
    AD_stat = as.numeric(diag$ad$statistic),
    AD_p = diag$ad$p.value
  )

  cat("\n--- Nested CV + cross-fitted normative scoring ---\n")
  t0 <- Sys.time()
  val <- run_full_validation(df, sel, boot, ex, "All", cfg)

  # Cross-fitted held-out normative scores from nested CV.
  norm <- crossfit_normative_object(val$nested_cv)

  # Independent full-sample normative scores from the FINAL selected GAMLSS.
  # The cleaned analysis table uses the internal response column FD.
  norm_full <- predict_individual_normative(
    model = sel$best_model,
    data = df,
    response = "FD"
  )
  norm <- merge_fullfit_normative_scores(norm, norm_full)

  validation_sec <- elapsed_seconds(t0)

  m <- val$nested_cv$single
  ps <- val$peak_stability

  validation_summary <- data.frame(
    Group = "All",
    N = nrow(df),
    Nested_Folds = length(unique(val$nested_cv$folds)),
    CV_RMSE_Mean = unname(m["RMSE"]),
    CV_MAE_Mean = unname(m["MAE"]),
    CV_R2_Mean = unname(m["R2"]),
    CV_MAE_Median = unname(m["Median_MAE"]),
    CV_NLL = unname(m["NLL"]),
    PI95_Coverage = unname(m["Coverage95"]),
    Cal_Slope = val$calibration$slope,
    Cal_Intercept = val$calibration$intercept,
    Cal_R2 = val$calibration$R2,
    PIT_KS_p = val$pit_ks_p,
    CrossFit_Z_Mean = val$z_mean,
    CrossFit_Z_SD = val$z_sd,
    Peak_MAD = ps$mad,
    Peak_IQR = ps$iqr,
    Peak_InternalBootProp = ps$internal_prop,
    BootstrapSuccessProp = boot$success_prop,
    BootstrapValid = if(!is.null(boot$valid)) isTRUE(boot$valid) else ((boot$n_success/boot$n_boot) >= as.numeric(cfg$BOOT_MIN_SUCCESS_PROP))
  )

  cat("\n--- Nested multi-engine comparison ---\n")
  t0 <- Sys.time()
  engine_all <- run_engine_comparison(
    df, "All", cfg, sel, val$nested_cv, val$nested_cv$folds
  )
  engine_sec <- elapsed_seconds(t0)

  metric <- cfg$ENGINE_METRIC
  if (!metric %in% names(engine_all)) metric <- "CV_NLL"
  good <- is.finite(engine_all[[metric]])

  engine_best <- if (any(good)) {
    engine_all[
      which.min(ifelse(good, engine_all[[metric]], Inf)),
      ,
      drop = FALSE
    ]
  } else {
    engine_all[0, , drop = FALSE]
  }

  t0 <- Sys.time()

  sel_stab <- selection_stability_table(val$selection_table, sel)

  summary_tbl <- build_summary_table_all(
    diag, ex, boot, sel, cfg, val,
    nrow(df), min(df$Age), max(df$Age), sample_sd, sel_stab
  )

  boot_detail <- build_bootstrap_detail_all(ex, boot, cfg)
  family_comparison <- sel$comparison

  sex_int <- test_age_by_sex_interaction(df, sel, cfg)

  if (!is.null(sex_int)) {
    cat(sprintf(
      " Sex x age sensitivity test: LR=%.3f, df=%d, p=%.4f\n",
      sex_int$LR, sex_int$DF, sex_int$P
    ))
  }

  summary_tbl$AgeSex_P <- if (!is.null(sex_int)) sex_int$P else NA_real_
  summary_tbl$AgeSex_q_BH <- NA_real_

  for (a0 in c(20, 40, 60, 75)) {
    summary_tbl[[paste0("IQR_Age", a0)]] <- tryCatch(
      approx(curve$Age, curve$iqr, xout=a0, rule=2)$y,
      error=function(e) NA_real_
    )
    summary_tbl[[paste0("CondSD_Age", a0)]] <- tryCatch(
      approx(curve$Age, curve$cond_sd, xout=a0, rule=2)$y,
      error=function(e) NA_real_
    )
  }

  centile_curves <- build_centile_curve_sheet(curve, cfg)
  derivative_curve <- build_derivative_curve_sheet(
    curve, boot, sample_sd, cfg
  )
  variability_curve <- build_variability_curve_sheet(curve, cfg)
  age_group_deviation <- build_age_group_deviation(norm, cfg)
  centile_calibration <- build_centile_calibration(norm, cfg)

  postprocess_sec <- elapsed_seconds(t0)

  results <- list(
    All = list(
      Age=curve$Age,
      central=curve$central,
      lo=curve$lo,
      hi=curve$hi,
      p25=curve$p25,
      p75=curve$p75,
      iqr=curve$iqr,
      centiles=curve$centiles,
      sigma=curve$cond_sd,
      scale_param=curve$scale_param,
      dcentral=curve$dcentral,
      mu=curve$mu,
      nu=curve$nu,
      tau=curve$tau,
      best_family=sel$best_family,
      best_model=sel$best_model,
      extrema=ex,
      resid=qres,
      bootstrap=boot,
      normative=norm
    ),
    normative_deviation=list(All=norm),
    summary_table=summary_tbl,
    family_comparison=family_comparison,
    bootstrap_detail=boot_detail,
    residual_tests=residual_tests,
    validation_summary=validation_summary,
    validation=list(All=val),
    selection_stability=sel_stab,
    centile_curves=centile_curves,
    derivative_curve=derivative_curve,
    variability_curve=variability_curve,
    age_group_deviation=age_group_deviation,
    centile_calibration=centile_calibration,
    engine_comparison_best=engine_best,
    engine_comparison_all=engine_all,
    sex_interaction=sex_int,
    config_used=cfg
  )

  results$run_metadata <- build_run_metadata(df, results, cfg)

  runtime <- runtime_record(
    group = "All",
    cfg = cfg,
    backend = backend,
    start_time = run_start,
    search_sec = search_sec,
    bootstrap_sec = bootstrap_sec,
    validation_sec = validation_sec,
    engine_sec = engine_sec,
    postprocess_sec = postprocess_sec,
    end_time = Sys.time()
  )

  # Keep runtime in RunMetadata so it appears in the ordinary result workbook.
  for (nm in names(runtime)) {
    if (!identical(nm, "Group")) {
      results$run_metadata[[nm]] <- runtime[[nm]][1]
    }
  }
  results$runtime <- runtime

  t_write <- Sys.time()
  plots <- create_q1_main_plot(df, results, cfg)
  save_results(results, plots, cfg)
  output_write_sec <- elapsed_seconds(t_write)
  wallclock_sec <- elapsed_seconds(run_start)

  runtime$OutputWrite_Seconds <- output_write_sec
  runtime$WallClock_Runtime_Seconds <- wallclock_sec
  runtime$WallClock_Runtime_Minutes <- wallclock_sec / 60
  runtime$WallClock_Runtime_Hours <- wallclock_sec / 3600
  results$runtime <- runtime

  runtime_sidecar <- file.path(cfg$OUT_DIR, paste0(cfg$OUTCOME_COL, "_All_runtime_final.csv"))
  try(utils::write.csv(runtime, runtime_sidecar, row.names = FALSE), silent = TRUE)

  cat(sprintf(
    "\n Wall-clock runtime: %.2f minutes (backend=%s, workers=%d)\n",
    runtime$WallClock_Runtime_Minutes,
    runtime$ComputeMode_Used,
    runtime$Workers
  ))

  cat("\n============================================================\n Pipeline completed successfully.\n============================================================\n")

  invisible(results)
}

cat("\n[BrainNormR v5.5.1 CORRECTED] publication/GitHub layer active.\n")

options(warn = 1)

# IMPORTANT: source() only loads functions.
# Run main(CONFIG) explicitly only for a deliberate single-outcome analysis.
# Production runs should use 01_run_structural_atlas.R or
# 02_run_cognitive_normative.R, which inject outcome-specific settings.
