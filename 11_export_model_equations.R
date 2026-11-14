##############################################################################
# 11_export_model_equations_WITH_WORD_PDF_v3.R
#
# BrainNormR v5.5.x compatible
#
# PURPOSE
# -------
# Export the mathematical specification of every already-saved final GAMLSS
# model. This script:
#   1) DOES NOT refit, reselect, or modify any model.
#   2) Exports Excel + CSV as before.
#   3) Adds LaTeX equation fields to the tabular export.
#   4) Generates a Word report in which equations are native Word equations
#      (OMML, produced by Pandoc from LaTeX math).
#   5) Generates a PDF report with typeset mathematical equations.
#
# IMPORTANT
# ---------
# - Equations are written exactly in terms of variables stored in the fitted
#   model (for example Age_s, Sex_c, edu_c, eTIV_c).
# - The exporter does NOT guess how Age_s was constructed from raw Age.
# - GAMLSS sigma is a family-specific scale parameter and is NOT universally
#   equal to the conditional standard deviation.
# - The response-scale equations are obtained by applying the inverse link.
##############################################################################

suppressPackageStartupMessages({
  pkgs <- c("gamlss", "dplyr", "writexl", "rmarkdown", "knitr")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(
        "Missing package: ", p,
        ". Install it first, e.g. install.packages('", p, "')."
      )
    }
  }
})

library(gamlss)
library(dplyr)
library(writexl)

# =========================
# USER SETTINGS
# =========================
RESULTS_ROOT <- "Results"
OUT_DIR <- file.path(RESULTS_ROOT, "Model_Equations")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# PDF creation needs a LaTeX engine.
# Recommended on Windows:
#   install.packages("tinytex")
#   tinytex::install_tinytex()
PDF_LATEX_ENGINE <- "xelatex"

# If TRUE, stop with an informative error when PDF cannot be generated.
# If FALSE, Word/Excel/CSV will still be produced and PDF will be skipped.
REQUIRE_PDF <- FALSE

# Keep the intermediate Rmd because it is useful for reproducibility.
KEEP_RMD <- TRUE

# =========================
# FIND FINAL MODELS
# =========================
model_files <- list.files(
  RESULTS_ROOT,
  pattern = "_final_GAMLSS_model\\.rds$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (!length(model_files)) {
  stop(
    "No saved final GAMLSS model objects were found under Results/. ",
    "Run the modelling pipeline first."
  )
}

# =========================
# SAFE MODEL ACCESSORS
# =========================
safe_formula <- function(model, parameter) {
  tryCatch(
    paste(deparse(formula(model, what = parameter)), collapse = " "),
    error = function(e) NA_character_
  )
}

safe_coef_vector <- function(model, parameter) {
  tryCatch(coef(model, what = parameter), error = function(e) NULL)
}

safe_coef_text <- function(model, parameter) {
  cf <- safe_coef_vector(model, parameter)
  if (is.null(cf) || !length(cf)) return(NA_character_)
  paste(names(cf), "=", signif(as.numeric(cf), 8), collapse = " ; ")
}

safe_link <- function(model, parameter) {
  x <- tryCatch(model[[paste0(parameter, ".link")]], error = function(e) NULL)
  if (is.null(x) || !length(x)) return(NA_character_)
  as.character(x[1])
}

safe_rhs_terms <- function(model, parameter) {
  f <- tryCatch(formula(model, what = parameter), error = function(e) NULL)
  if (is.null(f)) return(character())
  tryCatch(attr(terms(f), "term.labels"), error = function(e) character())
}

# =========================
# PLAIN-TEXT EQUATION
# =========================
linear_predictor_equation <- function(model, parameter) {
  cf <- safe_coef_vector(model, parameter)
  if (is.null(cf) || !length(cf)) return(NA_character_)

  pieces <- character()

  for (i in seq_along(cf)) {
    nm <- names(cf)[i]
    val <- signif(as.numeric(cf[i]), 8)

    if (identical(nm, "(Intercept)")) {
      pieces <- c(pieces, as.character(val))
    } else {
      sign_txt <- if (val >= 0) "+" else "-"
      pieces <- c(
        pieces,
        paste(sign_txt, abs(val), "*", nm)
      )
    }
  }

  paste0(
    "g_", parameter, "(", parameter, ") = ",
    paste(pieces, collapse = " ")
  )
}

# =========================
# LATEX / MATHEMATICAL EQUATIONS
# =========================

fmt_num <- function(x, digits = 8) {
  if (!is.finite(x)) return("NA")
  # Keep scientific notation only when genuinely useful.
  z <- format(signif(x, digits), scientific = FALSE, trim = TRUE)
  if (grepl("e", z, ignore.case = TRUE)) {
    z <- format(signif(x, digits), scientific = TRUE, trim = TRUE)
  }
  z
}

latex_escape_text <- function(x) {
  if (is.na(x) || !nzchar(x)) return("")
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x
}

# Convert common BrainNormR/GAMLSS term names to readable mathematical notation.
r_term_to_latex <- function(term) {
  if (is.na(term) || !nzchar(term)) return("")

  # Remove the outer I(...) wrapper when it encloses the entire term.
  z <- trimws(term)
  if (grepl("^I\\(.*\\)$", z)) {
    z <- sub("^I\\(", "", z)
    z <- sub("\\)$", "", z)
  }

  # Exact BrainNormR fractional-polynomial forms.
  # Repeated-power FP2: Age_s^p * log(Age_s)
  m_rep <- regexec(
    "^\\(Age_s\\)\\^\\(([-+0-9.]+)\\)\\s*\\*\\s*log\\(Age_s\\)$",
    z,
    perl = TRUE
  )
  g_rep <- regmatches(z, m_rep)[[1]]
  if (length(g_rep) == 2) {
    p <- g_rep[2]
    return(
      paste0(
        "\\mathrm{Age}_{s}^{", p, "}",
        "\\,\\log\\!\\left(\\mathrm{Age}_{s}\\right)"
      )
    )
  }

  # FP1 / non-repeated component: Age_s^p
  m_pow <- regexec(
    "^\\(Age_s\\)\\^\\(([-+0-9.]+)\\)$",
    z,
    perl = TRUE
  )
  g_pow <- regmatches(z, m_pow)[[1]]
  if (length(g_pow) == 2) {
    p <- g_pow[2]
    return(paste0("\\mathrm{Age}_{s}^{", p, "}"))
  }

  # p = 0 representation.
  if (z %in% c("log(Age_s)", "log((Age_s))")) {
    return("\\log\\!\\left(\\mathrm{Age}_{s}\\right)")
  }

  # Repeated p = 0 if it ever appears explicitly.
  if (grepl(
    "^log\\(Age_s\\)\\s*\\*\\s*log\\(Age_s\\)$",
    z,
    perl = TRUE
  )) {
    return(
      "\\left[\\log\\!\\left(\\mathrm{Age}_{s}\\right)\\right]^2"
    )
  }

  # Common covariates.
  known <- c(
    "Age_s" = "\\mathrm{Age}_{s}",
    "Sex_c" = "\\mathrm{Sex}_{c}",
    "edu_c" = "\\mathrm{Edu}_{c}",
    "eTIV_c" = "\\mathrm{eTIV}_{c}"
  )
  if (z %in% names(known)) return(unname(known[z]))

  # Generic fallback: preserve the fitted term literally as math-safe text.
  paste0("\\mathrm{", latex_escape_text(z), "}")
}

parameter_latex <- function(parameter) {
  switch(
    parameter,
    mu = "\\mu",
    sigma = "\\sigma",
    nu = "\\nu",
    tau = "\\tau",
    paste0("\\mathrm{", latex_escape_text(parameter), "}")
  )
}

linear_predictor_latex <- function(model, parameter) {
  cf <- safe_coef_vector(model, parameter)
  if (is.null(cf) || !length(cf)) return(NA_character_)

  par_tex <- parameter_latex(parameter)
  eta <- paste0("\\eta_{", par_tex, "}")
  pieces <- character()

  for (i in seq_along(cf)) {
    nm <- names(cf)[i]
    val <- as.numeric(cf[i])

    if (!is.finite(val)) next

    if (identical(nm, "(Intercept)")) {
      pieces <- c(pieces, fmt_num(val))
    } else {
      sign_txt <- if (val >= 0) "+" else "-"
      term_tex <- r_term_to_latex(nm)
      pieces <- c(
        pieces,
        paste0(
          sign_txt, " ",
          fmt_num(abs(val)),
          "\\,", term_tex
        )
      )
    }
  }

  if (!length(pieces)) return(NA_character_)

  paste0(
    eta, " = ",
    paste(pieces, collapse = " ")
  )
}

linear_predictor_rhs_latex <- function(model, parameter) {
  eq <- linear_predictor_latex(model, parameter)
  if (is.na(eq)) return(NA_character_)
  sub("^.*?=\\s*", "", eq, perl = TRUE)
}

inverse_link_rhs_latex <- function(link, rhs, parameter) {
  if (is.na(rhs) || !nzchar(rhs)) return(NA_character_)

  par_tex <- parameter_latex(parameter)

  if (is.na(link) || !nzchar(link)) {
    return(
      paste0(
        par_tex,
        " = g_{", par_tex, "}^{-1}\\!\\left(", rhs, "\\right)"
      )
    )
  }

  lk <- tolower(trimws(link))

  if (lk == "identity") {
    return(paste0(par_tex, " = ", rhs))
  }

  if (lk == "log") {
    return(
      paste0(
        par_tex,
        " = \\exp\\!\\left(", rhs, "\\right)"
      )
    )
  }

  if (lk == "logit") {
    return(
      paste0(
        par_tex,
        " = \\left[1+\\exp\\!\\left(-\\left(", rhs,
        "\\right)\\right)\\right]^{-1}"
      )
    )
  }

  if (lk == "inverse") {
    return(
      paste0(
        par_tex,
        " = \\left(", rhs, "\\right)^{-1}"
      )
    )
  }

  if (lk == "sqrt") {
    return(
      paste0(
        par_tex,
        " = \\left(", rhs, "\\right)^2"
      )
    )
  }

  # Safe generic expression for uncommon/custom GAMLSS links.
  paste0(
    par_tex,
    " = g_{", par_tex, "}^{-1}\\!\\left(", rhs, "\\right)"
  )
}

response_scale_latex <- function(model, parameter) {
  rhs <- linear_predictor_rhs_latex(model, parameter)
  if (is.na(rhs)) return(NA_character_)

  inverse_link_rhs_latex(
    safe_link(model, parameter),
    rhs,
    parameter
  )
}

active_parameters <- function(model) {
  pars <- c("mu", "sigma", "nu", "tau")
  pars[
    vapply(
      pars,
      function(p) {
        cf <- safe_coef_vector(model, p)
        !is.null(cf) && length(cf) > 0
      },
      logical(1)
    )
  ]
}

distribution_latex <- function(model, family) {
  pars <- active_parameters(model)
  if (!length(pars)) return(NA_character_)

  fam_tex <- latex_escape_text(family)

  paste0(
    "Y\\mid\\mathbf{x} \\sim \\mathrm{", fam_tex, "}\\!\\left(",
    paste(paste0("\\", pars), collapse = ",\\,"),
    "\\right)"
  )
}

# =========================
# PATH METADATA
# =========================
infer_group <- function(path) {
  b <- basename(path)

  if (grepl(
    "_Female_final_GAMLSS_model\\.rds$",
    b,
    ignore.case = TRUE
  )) return("Female")

  if (grepl(
    "_Male_final_GAMLSS_model\\.rds$",
    b,
    ignore.case = TRUE
  )) return("Male")

  if (grepl(
    "_All_final_GAMLSS_model\\.rds$",
    b,
    ignore.case = TRUE
  )) return("All")

  "All"
}

infer_outcome <- function(path, group) {
  b <- sub(
    "_final_GAMLSS_model\\.rds$",
    "",
    basename(path),
    ignore.case = TRUE
  )

  if (group %in% c("Female", "Male", "All")) {
    b <- sub(
      paste0("_", group, "$"),
      "",
      b,
      ignore.case = TRUE
    )
  }

  b
}

# =========================
# EXTRACT ALL FINAL MODELS
# =========================
rows <- list()
errors <- list()

for (f in model_files) {

  model <- tryCatch(readRDS(f), error = function(e) e)

  if (inherits(model, "error")) {
    errors[[length(errors) + 1L]] <- data.frame(
      ModelFile = normalizePath(
        f,
        winslash = "/",
        mustWork = FALSE
      ),
      Error = conditionMessage(model),
      stringsAsFactors = FALSE
    )
    next
  }

  if (!inherits(model, "gamlss")) {
    errors[[length(errors) + 1L]] <- data.frame(
      ModelFile = normalizePath(
        f,
        winslash = "/",
        mustWork = FALSE
      ),
      Error = "Object is not a gamlss model",
      stringsAsFactors = FALSE
    )
    next
  }

  group <- infer_group(f)
  outcome <- infer_outcome(f, group)
  fam <- tryCatch(
    as.character(model$family[1]),
    error = function(e) NA_character_
  )

  rows[[length(rows) + 1L]] <- data.frame(
    Outcome = outcome,
    Group = group,
    Family = fam,

    Mu_link = safe_link(model, "mu"),
    Mu_formula = safe_formula(model, "mu"),
    Mu_coefficients = safe_coef_text(model, "mu"),
    Mu_equation = linear_predictor_equation(model, "mu"),

    Sigma_link = safe_link(model, "sigma"),
    Sigma_formula = safe_formula(model, "sigma"),
    Sigma_coefficients = safe_coef_text(model, "sigma"),
    Sigma_equation = linear_predictor_equation(model, "sigma"),

    Nu_link = safe_link(model, "nu"),
    Nu_formula = safe_formula(model, "nu"),
    Nu_coefficients = safe_coef_text(model, "nu"),
    Nu_equation = linear_predictor_equation(model, "nu"),

    Tau_link = safe_link(model, "tau"),
    Tau_formula = safe_formula(model, "tau"),
    Tau_coefficients = safe_coef_text(model, "tau"),
    Tau_equation = linear_predictor_equation(model, "tau"),

    Distribution_LaTeX = distribution_latex(model, fam),

    Mu_eta_LaTeX = linear_predictor_latex(model, "mu"),
    Mu_response_LaTeX = response_scale_latex(model, "mu"),

    Sigma_eta_LaTeX = linear_predictor_latex(model, "sigma"),
    Sigma_response_LaTeX = response_scale_latex(model, "sigma"),

    Nu_eta_LaTeX = linear_predictor_latex(model, "nu"),
    Nu_response_LaTeX = response_scale_latex(model, "nu"),

    Tau_eta_LaTeX = linear_predictor_latex(model, "tau"),
    Tau_response_LaTeX = response_scale_latex(model, "tau"),

    ModelFile = normalizePath(
      f,
      winslash = "/",
      mustWork = FALSE
    ),

    stringsAsFactors = FALSE
  )
}

Model_Equations <- dplyr::bind_rows(rows)

if (!nrow(Model_Equations)) {
  stop("No readable final GAMLSS model objects were found.")
}

Model_Equations <- Model_Equations %>%
  arrange(
    Outcome,
    factor(Group, levels = c("All", "Female", "Male"))
  )

# =========================
# NOTES
# =========================
Notes <- data.frame(
  Item = c(
    "Purpose",
    "Location parameter",
    "Scale parameter",
    "Nu/Tau",
    "Link-scale equations",
    "Response-scale equations",
    "Age_s",
    "Three-group mode",
    "Word equations",
    "PDF equations"
  ),
  Note = c(
    "Documents already-selected final models; no model is refitted or reselected.",
    "Mu_eta_LaTeX is the fitted linear predictor for the location parameter on the mu link scale.",
    "Sigma_response_LaTeX is the fitted scale-parameter equation after inverse-link transformation. GAMLSS sigma is family-specific scale and is not universally the conditional SD.",
    "Nu and tau are exported when supported by the fitted family. In the current BrainNormR design they are intercept-only unless the saved fitted model itself says otherwise.",
    "Columns ending in *_eta_LaTeX are link-scale predictors eta.",
    "Columns ending in *_response_LaTeX apply the inverse link and therefore show the fitted distributional parameter on its response scale.",
    "Age_s is reported exactly as used by the fitted model. This exporter does not guess or reconstruct the preprocessing definition of Age_s from raw Age.",
    "All, Female and Male are separate fitted models and may therefore have different families and functional forms.",
    "The DOCX report is produced by Pandoc from display LaTeX equations; Pandoc converts these to native Word OMML equations.",
    "The PDF report is typeset by LaTeX. A working LaTeX engine such as xelatex is required."
  ),
  stringsAsFactors = FALSE
)

# =========================
# EXCEL + CSV
# =========================
out_xlsx <- file.path(
  OUT_DIR,
  "GAMLSS_Model_Equations.xlsx"
)

out_csv <- file.path(
  OUT_DIR,
  "GAMLSS_Model_Equations.csv"
)

sheets <- list(
  Model_Equations = Model_Equations,
  Notes = Notes
)

if (length(errors)) {
  sheets$Read_Errors <- dplyr::bind_rows(errors)
}

writexl::write_xlsx(
  sheets,
  out_xlsx
)

write.csv(
  Model_Equations,
  out_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# =========================
# WORD / PDF REPORT
# =========================

# Markdown-safe inline text.
md_code <- function(x) {
  if (is.na(x)) return("NA")
  x <- gsub("`", "'", as.character(x), fixed = TRUE)
  paste0("`", x, "`")
}

write_display_math <- function(con, tex) {
  if (is.na(tex) || !nzchar(tex)) return(invisible(NULL))
  writeLines(
    c("", "$$", tex, "$$", ""),
    con = con,
    sep = "\n"
  )
}

out_rmd <- file.path(
  OUT_DIR,
  "GAMLSS_Model_Equations_Report.Rmd"
)

con <- file(
  out_rmd,
  open = "wt",
  encoding = "UTF-8"
)

writeLines(
  c(
    "---",
    'title: "BrainNormR Final GAMLSS Model Equations"',
    'author: "BrainNormR"',
    'date: "`r format(Sys.Date(), \'%Y-%m-%d\')`"',
    "output:",
    "  word_document:",
    "    toc: true",
    "  pdf_document:",
    paste0("    latex_engine: ", PDF_LATEX_ENGINE),
    "    toc: true",
    "fontsize: 10pt",
    "geometry: margin=1in",
    "---",
    "",
    "# Interpretation notes",
    "",
    paste0(
      "- These equations document **already-selected final models**; ",
      "no model is refitted or reselected."
    ),
    paste0(
      "- Equations written as $\\eta_\\mu$, $\\eta_\\sigma$, ",
      "$\\eta_\\nu$, and $\\eta_\\tau$ are on the fitted link scale."
    ),
    paste0(
      "- The response-scale equations explicitly apply each parameter's ",
      "inverse link."
    ),
    paste0(
      "- GAMLSS $\\sigma$ is a family-specific scale parameter; it is ",
      "not universally the conditional standard deviation."
    ),
    paste0(
      "- `Age_s`, `Sex_c`, `edu_c`, and `eTIV_c` are written exactly ",
      "as stored in the fitted model. This report does not invent a ",
      "raw-age back-transformation."
    ),
    "",
    "\\newpage"
  ),
  con = con,
  sep = "\n"
)

for (i in seq_len(nrow(Model_Equations))) {

  r <- Model_Equations[i, , drop = FALSE]

  writeLines(
    c(
      "",
      paste0(
        "# ",
        i,
        ". ",
        as.character(r$Outcome),
        " - ",
        as.character(r$Group)
      ),
      "",
      paste0(
        "**Family:** ",
        md_code(r$Family)
      ),
      "",
      paste0(
        "**Saved model:** ",
        md_code(r$ModelFile)
      ),
      "",
      "## Distribution",
      ""
    ),
    con = con,
    sep = "\n"
  )

  write_display_math(
    con,
    r$Distribution_LaTeX
  )

  param_info <- list(
    mu = list(
      title = "Location parameter (mu)",
      link = r$Mu_link,
      formula = r$Mu_formula,
      eta = r$Mu_eta_LaTeX,
      response = r$Mu_response_LaTeX
    ),
    sigma = list(
      title = "Scale parameter (sigma)",
      link = r$Sigma_link,
      formula = r$Sigma_formula,
      eta = r$Sigma_eta_LaTeX,
      response = r$Sigma_response_LaTeX
    ),
    nu = list(
      title = "Shape parameter (nu)",
      link = r$Nu_link,
      formula = r$Nu_formula,
      eta = r$Nu_eta_LaTeX,
      response = r$Nu_response_LaTeX
    ),
    tau = list(
      title = "Shape parameter (tau)",
      link = r$Tau_link,
      formula = r$Tau_formula,
      eta = r$Tau_eta_LaTeX,
      response = r$Tau_response_LaTeX
    )
  )

  for (p in names(param_info)) {

    q <- param_info[[p]]

    if (is.na(q$eta) || !nzchar(q$eta)) next

    writeLines(
      c(
        "",
        paste0("## ", q$title),
        "",
        paste0(
          "**Link:** ",
          md_code(q$link)
        ),
        "",
        paste0(
          "**R formula:** ",
          md_code(q$formula)
        ),
        "",
        "**Linear predictor:**"
      ),
      con = con,
      sep = "\n"
    )

    write_display_math(
      con,
      q$eta
    )

    writeLines(
      c(
        "",
        "**Response-scale parameter equation:**"
      ),
      con = con,
      sep = "\n"
    )

    write_display_math(
      con,
      q$response
    )
  }

  writeLines(
    c(
      "",
      "\\newpage",
      ""
    ),
    con = con,
    sep = "\n"
  )
}

close(con)

# Pandoc is required for both Word and PDF.
if (!rmarkdown::pandoc_available()) {
  stop(
    "Pandoc is not available. Run this script from RStudio or install Pandoc. ",
    "Excel/CSV were created, but Word/PDF could not be rendered."
  )
}

out_docx <- file.path(
  OUT_DIR,
  "GAMLSS_Model_Equations_Report.docx"
)

out_pdf <- file.path(
  OUT_DIR,
  "GAMLSS_Model_Equations_Report.pdf"
)

# Word: Pandoc converts display LaTeX equations to native OMML equations.
word_result <- tryCatch(
  rmarkdown::render(
    input = out_rmd,
    output_format = rmarkdown::word_document(toc = TRUE),
    output_file = basename(out_docx),
    output_dir = OUT_DIR,
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  ),
  error = function(e) e
)

if (inherits(word_result, "error")) {
  warning(
    "Word report could not be created: ",
    conditionMessage(word_result)
  )
}

# PDF: use real LaTeX mathematical typesetting.
latex_path <- Sys.which(PDF_LATEX_ENGINE)
pdf_result <- NULL

if (!nzchar(latex_path)) {

  msg <- paste0(
    "PDF report was not created because '",
    PDF_LATEX_ENGINE,
    "' was not found. ",
    "Recommended Windows setup: ",
    "install.packages('tinytex'); tinytex::install_tinytex()"
  )

  if (isTRUE(REQUIRE_PDF)) {
    stop(msg)
  } else {
    warning(msg)
  }

} else {

  pdf_result <- tryCatch(
    rmarkdown::render(
      input = out_rmd,
      output_format = rmarkdown::pdf_document(
        toc = TRUE,
        latex_engine = PDF_LATEX_ENGINE
      ),
      output_file = basename(out_pdf),
      output_dir = OUT_DIR,
      quiet = TRUE,
      envir = new.env(parent = globalenv())
    ),
    error = function(e) e
  )

  if (inherits(pdf_result, "error")) {
    warning(
      "PDF report could not be created: ",
      conditionMessage(pdf_result)
    )
  }
}

if (!isTRUE(KEEP_RMD) && file.exists(out_rmd)) {
  unlink(out_rmd)
}

# =========================
# FINAL LOG
# =========================
cat("\n============================================================\n")
cat(" Final-model equation export completed\n")
cat(" Models exported: ", nrow(Model_Equations), "\n", sep = "")
cat(" Excel: ", out_xlsx, "\n", sep = "")
cat(" CSV:   ", out_csv, "\n", sep = "")

if (file.exists(out_docx)) {
  cat(" Word:  ", out_docx, "\n", sep = "")
} else {
  cat(" Word:  NOT CREATED - see warning above\n")
}

if (file.exists(out_pdf)) {
  cat(" PDF:   ", out_pdf, "\n", sep = "")
} else {
  cat(" PDF:   NOT CREATED - see warning above\n")
}

if (isTRUE(KEEP_RMD) && file.exists(out_rmd)) {
  cat(" Rmd:   ", out_rmd, "\n", sep = "")
}

if (length(errors)) {
  cat(
    " Read errors: ",
    nrow(dplyr::bind_rows(errors)),
    "\n",
    sep = ""
  )
}

cat("============================================================\n")
