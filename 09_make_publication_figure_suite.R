# BrainNormR publication figure suite — COMPLETE publication suite v12
# Comprehensive manuscript + supplement figure generator.
# No BrainNormR models are refitted.

suppressPackageStartupMessages({
  pkgs <- c("readxl","writexl","dplyr","tidyr","ggplot2","patchwork","scales","viridis","ggseg")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly=TRUE)) install.packages(p, repos="https://cloud.r-project.org")
  }
  library(readxl); library(writexl); library(dplyr); library(tidyr)
  library(ggplot2); library(patchwork); library(scales); library(viridis); library(ggseg)
})

# =========================
# USER SETTINGS
# =========================
RESULTS_ROOT <- "Results"
THESIS_DIR <- file.path(RESULTS_ROOT, "Thesis_Outputs")
MASTER_RESULTS_FILE <- file.path(THESIS_DIR, "MASTER_RESULTS.xlsx")
MASTER_CURVES_FILE  <- file.path(THESIS_DIR, "MASTER_CURVES.xlsx")
BIOMARKER_FILE <- file.path(RESULTS_ROOT, "Biomarker",
                            "MASTER_NORMATIVE_DEVIATIONS_EXPANDED.xlsx")

OUT_ROOT <- file.path(RESULTS_ROOT, "Publication_Figures")
MAIN_DIR <- file.path(OUT_ROOT, "Main")
SUPP_DIR <- file.path(OUT_ROOT, "Supplement")
TABLE_DIR <- file.path(OUT_ROOT, "Tables")
dir.create(MAIN_DIR, recursive=TRUE, showWarnings=FALSE)
dir.create(SUPP_DIR, recursive=TRUE, showWarnings=FALSE)
dir.create(TABLE_DIR, recursive=TRUE, showWarnings=FALSE)

REFERENCE_AGE <- NULL
MEASURE_LEVELS <- c("Volume","Area","Thickness","Curvature","FD")
MACRO_LEVELS <- c("WholeBrain","Lobe")
MILESTONE_LEVELS <- c("WholeBrain","Hemisphere","Lobe")
VAR_AGE_EARLY <- 20
VAR_AGE_LATE  <- 75
FIG_DPI <- 600

# Publication-style age periods: deliberately subtle neutral bands.
# These match the visual hierarchy of the reference figure and do not compete
# with the trajectories.
AGE_BANDS <- data.frame(
  Band=c("Young adulthood","Middle adulthood","Late adulthood"),
  xmin=c(20,40,60),
  xmax=c(40,60,80),
  xmid=c(30,50,70),
  stringsAsFactors=FALSE
)

AGE_BAND_COLORS <- c(
  "Young adulthood"="#F4F4F4",
  "Middle adulthood"="#E9E9E9",
  "Late adulthood"="#F2F2F2"
)

# Muted cortical-scale palette. Whole cortex is ALWAYS black.
CORTICAL_SCALE_COLORS <- c(
  "Whole cortex"="#111111",
  "Frontal"="#D95F59",
  "Temporal"="#3E95C8",
  "Parietal"="#8C6BB1",
  "Occipital"="#45A36F",
  "Cingulate"="#D98C2B",
  "Insula"="#2A9D8F"
)

HEMISPHERE_COLORS <- c(
  "Left hemisphere"="#4C78A8",
  "Right hemisphere"="#D55E5E"
)

AGE_PERIOD_TEXT <- paste0(
  "Age periods: Young adulthood (20-40)  |  ",
  "Middle adulthood (40-60)  |  ",
  "Late adulthood (60-80)"
)

AGE_PERIOD_TEXT_TOP <- paste0(
  "Young adulthood (20-40)   ·   ",
  "Middle adulthood (40-60)   ·   ",
  "Late adulthood (60-80)"
)

AGE_PERIOD_STRIP_TEXT <- paste0(
  "Young adulthood (20-40)  |  ",
  "Middle adulthood (40-60)  |  ",
  "Late adulthood (60-80)"
)

measure_age_box_labeller <- ggplot2::as_labeller(function(x) x)

YEO7_COLORS <- c(
  "Visual"="#4C78A8",
  "Somatomotor"="#72A0C1",
  "DorsalAttention"="#6B9E78",
  "VentralAttention"="#B08B57",
  "Limbic"="#A8758A",
  "Frontoparietal"="#8172B2",
  "DefaultMode"="#5F6B6D"
)

MILESTONE_COLORS <- c(
  "Peak"="#6E9270",
  "Trough"="#B9874C",
  "Maximal age-related change"="#6D7F9B"
)

MILESTONE_SHAPES <- c(
  "Peak"=21,
  "Trough"=22,
  "Maximal age-related change"=24
)

# Muted publication-friendly heatmap palettes
HEATMAP_DIVERGING_LOW  <- "#6F8FB8"
HEATMAP_DIVERGING_MID  <- "#F7F7F7"
HEATMAP_DIVERGING_HIGH <- "#C46E6E"
HEATMAP_SEQUENTIAL_COLORS <- c("#6F8FB8", "#D7E0EA", "#F4F4F4", "#E7CFCD", "#C46E6E")

AGE_X_BREAKS <- c(20,30,40,50,60,70,80)

BURDEN_POINT_COLOR <- "#4F7894"
BURDEN_LINE_COLOR  <- "#B9C4CB"
BURDEN_MEDIAN_COLOR <- "#A65A5A"
BURDEN_IQR_FILL <- "#E7ECEF"

COGNITION_LINE_COLOR <- "#4F6F8F"
COGNITION_PEAK_COLOR <- "#6E9270"
COGNITION_MAXCHANGE_COLOR <- "#6D7F9B"

# Muted palette for the three modelling engines used in sensitivity comparison.
ENGINE_COLORS <- c(
  "GAMLSS"="#4F6F8F",
  "GAM"="#6E9270",
  "FP/POLY"="#B9874C"
)

# =========================
# HELPERS
# =========================
safe_num <- function(x) suppressWarnings(as.numeric(x))
truthy <- function(x) {
  if (!length(x) || is.na(x[1])) return(FALSE)
  z <- x[1]
  if (is.logical(z)) return(isTRUE(z))
  if (is.numeric(z)) return(is.finite(z) && z != 0)
  tolower(trimws(as.character(z))) %in% c("true","t","yes","y","1")
}
safe_approx <- function(x,y,xout) {
  x <- safe_num(x); y <- safe_num(y)
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok)<2) return(NA_real_)
  approx(x[ok],y[ok],xout=xout,rule=2)$y
}
norm_level <- function(x) {
  z <- trimws(as.character(x)); out <- z
  out[tolower(z) %in% c("roi","region","regional")] <- "ROI"
  out[tolower(z) %in% c("lobe","lobar")] <- "Lobe"
  out[tolower(z) %in% c("hemisphere","hemispheric")] <- "Hemisphere"
  out[tolower(z) %in% c("wholebrain","whole brain","global","whole-cortex","whole cortex")] <- "WholeBrain"
  out[tolower(z) %in% c("yeo7","network","yeo")] <- "Yeo7"
  out[tolower(z) %in% c("cognition","cognitive")] <- "Cognition"
  out
}
detect_measure <- function(outcome,outcome_type=NA_character_) {
  txt <- paste(outcome,outcome_type,collapse=" | ")
  if (grepl("thickness",txt,ignore.case=TRUE)) return("Thickness")
  if (grepl("curvature|curv",txt,ignore.case=TRUE)) return("Curvature")
  if (grepl("volume|_vol\\b|\\bvol\\b",txt,ignore.case=TRUE)) return("Volume")
  if (grepl("area",txt,ignore.case=TRUE)) return("Area")
  if (grepl("(^|[^A-Za-z])FD([^A-Za-z]|$)|fractal",txt,ignore.case=TRUE)) return("FD")
  NA_character_
}
add_measure <- function(df) {
  if (!nrow(df)) return(df)
  if (!"Measure" %in% names(df)) {
    df$Measure <- mapply(detect_measure,
                         if ("Outcome" %in% names(df)) df$Outcome else "",
                         if ("OutcomeType" %in% names(df)) df$OutcomeType else NA_character_,
                         USE.NAMES=FALSE)
  } else {
    bad <- is.na(df$Measure) | !nzchar(trimws(as.character(df$Measure)))
    if (any(bad)) {
      df$Measure[bad] <- mapply(detect_measure,df$Outcome[bad],
                                if ("OutcomeType" %in% names(df)) df$OutcomeType[bad] else NA_character_,
                                USE.NAMES=FALSE)
    }
  }
  df
}
read_sheet <- function(path,sheet) {
  if (!file.exists(path)) return(NULL)
  sh <- tryCatch(excel_sheets(path),error=function(e) character())
  if (!sheet %in% sh) return(NULL)
  tryCatch(as.data.frame(read_excel(path,sheet=sheet)),error=function(e) NULL)
}
save_pair <- function(p,name,dir,width=11,height=8) {
  png <- file.path(dir,paste0(name,".png"))
  pdf <- file.path(dir,paste0(name,".pdf"))
  ggsave(png,p,width=width,height=height,dpi=FIG_DPI,bg="white")
  ggsave(pdf,p,width=width,height=height,bg="white")
  c(PNG=png,PDF=pdf)
}
add_bands <- function(p, legend=FALSE, labels=FALSE) {
  p +
    geom_rect(
      data=AGE_BANDS,
      aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf,fill=Band),
      inherit.aes=FALSE,
      alpha=1,
      color=NA,
      show.legend=FALSE
    ) +
    geom_vline(
      xintercept=c(40,60),
      color="#BDBDBD",
      linewidth=.45,
      linetype="dashed",
      inherit.aes=FALSE,
      show.legend=FALSE
    ) +
    scale_fill_manual(
      values=AGE_BAND_COLORS,
      breaks=AGE_BANDS$Band,
      drop=FALSE,
      guide="none"
    )
}

add_age_axis <- function(p) {
  p +
    scale_x_continuous(
      breaks=AGE_X_BREAKS,
      minor_breaks=NULL,
      limits=c(19,80),
      expand=expansion(mult=c(0.01,0.02))
    ) +
    theme(
      axis.title.x=element_text(face="plain",margin=margin(t=7)),
      axis.text.x=element_text(color="#222222"),
      axis.ticks.x=element_line(color="#333333")
    )
}

make_age_header_df <- function(df, facet_col, y_col) {
  if (is.null(df) || !nrow(df) || !(facet_col %in% names(df)) ||
      !(y_col %in% names(df))) return(data.frame())

  keep <- is.finite(safe_num(df[[y_col]]))
  if ("Age" %in% names(df)) keep <- keep & is.finite(safe_num(df$Age))
  dd <- df[keep, , drop=FALSE]
  if (!nrow(dd)) return(data.frame())

  parts <- split(dd, as.character(dd[[facet_col]]))
  out <- list()

  for (nm in names(parts)) {
    z <- parts[[nm]]
    yy <- safe_num(z[[y_col]])
    yy <- yy[is.finite(yy)]
    if (!length(yy)) next
    ymin0 <- min(yy, na.rm=TRUE)
    ymax0 <- max(yy, na.rm=TRUE)
    span <- ymax0 - ymin0
    if (!is.finite(span) || span <= 0) span <- max(abs(ymax0), 1)

    y1 <- ymax0 + 0.05 * span
    y2 <- ymax0 + 0.19 * span
    ym <- (y1 + y2) / 2

    tmp <- AGE_BANDS
    tmp$ymin <- y1
    tmp$ymax <- y2
    tmp$y    <- ym
    tmp[[facet_col]] <- nm
    out[[length(out)+1L]] <- tmp
  }

  if (!length(out)) return(data.frame())
  dplyr::bind_rows(out)
}

add_age_headers <- function(p, df, facet_col, y_col) {
  hdr <- make_age_header_df(df, facet_col, y_col)
  if (is.null(hdr) || !nrow(hdr)) return(p)

  p +
    geom_rect(
      data=hdr,
      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
      inherit.aes=FALSE,
      fill="white",
      color="#9A9A9A",
      linewidth=.35,
      show.legend=FALSE
    ) +
    geom_text(
      data=hdr,
      aes(x=xmid, y=y, label=Band),
      inherit.aes=FALSE,
      size=2.7,
      fontface="bold",
      color="#333333",
      lineheight=.9,
      show.legend=FALSE
    ) +
    coord_cartesian(clip="off") +
    theme(plot.margin=margin(26,18,8,8))
}

publication_curve_theme <- function(base_size=10.5) {
  theme_classic(base_size=base_size) +
    theme(
      strip.background=element_rect(fill="white",color="#333333",linewidth=.6),
      strip.text=element_text(
        face="bold",
        color="#222222",
        lineheight=.9,
        size=base_size*0.88,
        margin=margin(t=2,r=2,b=2,l=2)
      ),
      plot.title=element_text(face="bold",color="#111111"),
      plot.subtitle=element_text(color="#444444"),
      plot.caption=element_text(
        color="#666666",
        size=base_size*0.78,
        hjust=0,
        margin=margin(t=7)
      ),
      axis.title=element_text(color="#222222"),
      axis.text=element_text(color="#222222"),
      legend.position="bottom",
      legend.direction="horizontal",
      legend.box="vertical",
      legend.box.just="left",
      legend.title=element_text(face="bold",size=base_size*0.9),
      legend.text=element_text(size=base_size*0.86),
      legend.key.width=grid::unit(1.15,"lines"),
      legend.key.height=grid::unit(0.95,"lines"),
      legend.spacing.y=grid::unit(2,"pt"),
      panel.spacing=grid::unit(0.7,"lines"),
      plot.margin=margin(6,26,6,6)
    )
}

pretty_hemisphere <- function(x) {
  z <- trimws(as.character(x))
  out <- z
  out[tolower(z) %in% c("left","lh","left hemisphere")] <- "Left hemisphere"
  out[tolower(z) %in% c("right","rh","right hemisphere")] <- "Right hemisphere"
  out
}

resolve_hemisphere <- function(hemisphere=NULL, region=NULL, outcome=NULL) {
  n <- max(length(hemisphere), length(region), length(outcome), 1L)

  recycle_chr <- function(x, n) {
    if (is.null(x) || !length(x)) return(rep("", n))
    rep(as.character(x), length.out=n)
  }

  h <- recycle_chr(hemisphere, n)
  r <- recycle_chr(region, n)
  o <- recycle_chr(outcome, n)

  key <- tolower(trimws(paste(h, r, o, sep=" | ")))

  dplyr::case_when(
    grepl("(^|[^a-z])right([^a-z]|$)|(^|[^a-z])rh[_ -]", key) ~ "Right hemisphere",
    grepl("(^|[^a-z])left([^a-z]|$)|(^|[^a-z])lh[_ -]",  key) ~ "Left hemisphere",
    TRUE ~ NA_character_
  )
}

# Keep only whole-cortex and genuinely bilateral/combined lobe outputs.
# Some master files store whole-lobe, left-lobe and right-lobe rows with the
# same Region label.  Without this filter they are drawn as several curves
# with the same colour and legend label.
keep_bilateral_macro <- function(df) {
  if (is.null(df) || !nrow(df) || !"Level" %in% names(df)) return(df)

  n <- nrow(df)
  get_chr <- function(nm) {
    if (nm %in% names(df)) as.character(df[[nm]]) else rep("", n)
  }

  key <- tolower(trimws(paste(
    get_chr("Hemisphere"), get_chr("Region"), get_chr("Outcome"),
    sep=" | "
  )))

  has_left <- grepl(
    "(^|[^a-z])(left|lh)([^a-z]|$)", key, perl=TRUE
  )
  has_right <- grepl(
    "(^|[^a-z])(right|rh)([^a-z]|$)", key, perl=TRUE
  )
  explicitly_combined <- grepl(
    "(^|[^a-z])(bilateral|both|combined)([^a-z]|$)",
    key,
    perl=TRUE
  )

  # A combined Outcome may legitimately contain both lh and rh tokens.
  is_side_specific <- xor(has_left,has_right) & !explicitly_combined

  keep <- df$Level == "WholeBrain" |
    (df$Level == "Lobe" & !is_side_specific)

  df[!is.na(keep) & keep, , drop=FALSE]
}

# Fail early if filtering still leaves more than one plotted value for the
# same lobe/measure/age. This prevents silent over-plotting or zig-zag lines.
assert_one_macro_curve <- function(df, value_col, figure_code) {
  if (is.null(df) || !nrow(df)) return(invisible(TRUE))
  dup <- df %>%
    filter(is.finite(safe_num(.data[[value_col]]))) %>%
    count(Measure, Unit, Age, name="n") %>%
    filter(n > 1)
  if (nrow(dup)) {
    stop(
      figure_code,
      ": more than one bilateral value remains for a Measure × Unit × Age. ",
      "Check Hemisphere/Region/Outcome labels in MASTER_CURVES.xlsx."
    )
  }
  invisible(TRUE)
}

cortical_color_scale <- function() {
  scale_color_manual(
    values=CORTICAL_SCALE_COLORS,
    breaks=names(CORTICAL_SCALE_COLORS),
    drop=FALSE
  )
}

hemisphere_color_scale <- function() {
  scale_color_manual(
    values=HEMISPHERE_COLORS,
    limits=c("Left hemisphere","Right hemisphere"),
    breaks=c("Left hemisphere","Right hemisphere"),
    drop=FALSE
  )
}

yeo7_color_scale <- function() {
  scale_color_manual(
    values=YEO7_COLORS,
    breaks=names(YEO7_COLORS),
    drop=FALSE
  )
}

# Build peak and maximal-change markers from Summary, then interpolate the
# corresponding plotted curve value. This avoids using raw outcome units on
# standardized plots.
make_curve_markers <- function(curve_df, summary_df, eligible_levels,
                               y_col, include_peak=TRUE,
                               include_max_change=TRUE) {

  if (is.null(curve_df) || !nrow(curve_df) ||
      is.null(summary_df) || !nrow(summary_df) ||
      !"Outcome" %in% names(curve_df) ||
      !"Outcome" %in% names(summary_df)) {
    return(data.frame())
  }

  rows <- list()

  for (i in seq_len(nrow(summary_df))) {
    r <- summary_df[i,,drop=FALSE]

    if (!"Level" %in% names(r) ||
        !as.character(r$Level) %in% eligible_levels ||
        !"Measure" %in% names(r) ||
        !as.character(r$Measure) %in% MEASURE_LEVELS) {
      next
    }

    out <- as.character(r$Outcome)
    cc <- curve_df[curve_df$Outcome == out,,drop=FALSE]

    if (!nrow(cc) || !"Age" %in% names(cc) || !y_col %in% names(cc))
      next

    unit <- if (as.character(r$Level)=="WholeBrain") {
      "Whole cortex"
    } else if (as.character(r$Level)=="Hemisphere" &&
               "Hemisphere" %in% names(r)) {
      pretty_hemisphere(r$Hemisphere)
    } else {
      as.character(r$Region)
    }

    add_marker <- function(age, milestone) {
      age <- safe_num(age)
      if (!is.finite(age)) return(NULL)

      yy <- safe_approx(
        safe_num(cc$Age),
        safe_num(cc[[y_col]]),
        age
      )
      if (!is.finite(yy)) return(NULL)

      data.frame(
        Outcome=out,
        Measure=as.character(r$Measure),
        Unit=unit,
        Age=age,
        Y=yy,
        Milestone=milestone,
        stringsAsFactors=FALSE
      )
    }

    if (include_peak &&
        all(c("InternalPeak","PeakAge") %in% names(r)) &&
        truthy(r$InternalPeak)) {
      rows[[length(rows)+1L]] <- add_marker(r$PeakAge,"Peak")
    }

    if (include_max_change &&
        all(c("InternalMaxChange","MaxChangeAge") %in% names(r)) &&
        truthy(r$InternalMaxChange)) {
      rows[[length(rows)+1L]] <- add_marker(
        r$MaxChangeAge,
        "Maximal age-related change"
      )
    }
  }

  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())

  bind_rows(rows) %>%
    mutate(
      Measure=factor(Measure,levels=MEASURE_LEVELS),
      Milestone=factor(
        Milestone,
        levels=c("Peak","Maximal age-related change")
      )
    )
}

add_curve_markers <- function(p, markers, color_scale="cortical") {
  if (is.null(markers) || !nrow(markers)) return(p)

  p +
    geom_point(
      data=markers,
      aes(x=Age,y=Y,color=Unit,shape=Milestone),
      inherit.aes=FALSE,
      size=3.0,
      stroke=.7,
      show.legend=c(color=FALSE,shape=TRUE)
    ) +
    scale_shape_manual(
      values=c(
        "Peak"=21,
        "Maximal age-related change"=24
      ),
      name="Milestone"
    )
}

make_end_labels <- function(df, y_col) {
  if (is.null(df) || !nrow(df) || !(y_col %in% names(df))) return(data.frame())
  if (!all(c("Age","Unit","Outcome") %in% names(df))) return(data.frame())
  labs <- df %>%
    filter(is.finite(Age), is.finite(.data[[y_col]])) %>%
    group_by(Unit, Outcome) %>%
    slice_max(order_by=Age, n=1, with_ties=FALSE) %>%
    ungroup() %>%
    mutate(
      Label=dplyr::case_when(
        as.character(Unit)=="Left hemisphere" ~ "LH",
        as.character(Unit)=="Right hemisphere" ~ "RH",
        TRUE ~ as.character(Unit)
      ),
      Y=.data[[y_col]],
      X=pmin(Age + 0.9, 79.4)
    )
  labs
}

add_end_labels <- function(p, labs_df) {
  if (is.null(labs_df) || !nrow(labs_df)) return(p)
  p +
    geom_label(
      data=labs_df,
      aes(x=X, y=Y, label=Label, color=Unit),
      inherit.aes=FALSE,
      size=3.0,
      fontface="bold",
      fill=scales::alpha("white", 0.9),
      label.size=.15,
      label.padding=grid::unit(0.10, "lines"),
      label.r=grid::unit(0.08, "lines"),
      show.legend=FALSE
    )
}

first_existing_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit)) hit[1] else NA_character_
}

make_cognition_markers <- function(curve_df, summary_df, y_col) {
  if (is.null(curve_df) || !nrow(curve_df) ||
      is.null(summary_df) || !nrow(summary_df) ||
      !"Outcome" %in% names(curve_df) ||
      !"Outcome" %in% names(summary_df) ||
      !(y_col %in% names(curve_df))) {
    return(data.frame())
  }

  ss <- summary_df
  if ("Level" %in% names(ss)) ss$Level <- norm_level(ss$Level)
  ss <- ss %>% filter(Level=="Cognition")

  rows <- list()

  for (i in seq_len(nrow(ss))) {
    r <- ss[i,,drop=FALSE]
    out <- as.character(r$Outcome)
    cc <- curve_df[curve_df$Outcome==out,,drop=FALSE]
    if (!nrow(cc)) next

    add_one <- function(age, milestone) {
      age <- safe_num(age)
      if (!is.finite(age)) return(NULL)
      yy <- safe_approx(cc$Age, cc[[y_col]], age)
      if (!is.finite(yy)) return(NULL)
      data.frame(
        Outcome=out,
        Age=age,
        Y=yy,
        Milestone=milestone,
        stringsAsFactors=FALSE
      )
    }

    if (all(c("InternalPeak","PeakAge") %in% names(r)) &&
        truthy(r$InternalPeak)) {
      rows[[length(rows)+1L]] <- add_one(r$PeakAge, "Peak")
    }

    if (all(c("InternalMaxChange","MaxChangeAge") %in% names(r)) &&
        truthy(r$InternalMaxChange)) {
      rows[[length(rows)+1L]] <- add_one(
        r$MaxChangeAge,
        "Maximal age-related change"
      )
    }
  }

  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())

  bind_rows(rows) %>%
    mutate(
      Milestone=factor(
        Milestone,
        levels=c("Peak","Maximal age-related change")
      )
    )
}

cognition_age_box_labeller <- ggplot2::as_labeller(function(x) x)
idx <- list(); logs <- character()
regfig <- function(code,title,files=NULL,status="CREATED",note="") {
  idx[[length(idx)+1L]] <<- data.frame(
    FigureCode=code,Title=title,Status=status,
    PNG=if (!is.null(files)) unname(files["PNG"]) else NA_character_,
    PDF=if (!is.null(files)) unname(files["PDF"]) else NA_character_,
    Note=note,stringsAsFactors=FALSE)
  logs <<- c(logs,paste(code,status,title,note,sep=" | "))
}

# =========================
# LOAD MASTER OUTPUTS
# =========================
S <- read_sheet(MASTER_RESULTS_FILE,"Summary")
C <- read_sheet(MASTER_CURVES_FILE,"CentileCurves")
D <- read_sheet(MASTER_CURVES_FILE,"DerivativeCurve")
V <- read_sheet(MASTER_CURVES_FILE,"VariabilityCurve")
VAL <- read_sheet(MASTER_RESULTS_FILE,"Validation")
if (is.null(S)||is.null(C)||is.null(D)||is.null(V)) {
  stop("MASTER_RESULTS.xlsx / MASTER_CURVES.xlsx missing. Run 04_make_thesis_outputs.R first.")
}
S <- add_measure(S); C <- add_measure(C); D <- add_measure(D); V <- add_measure(V)
for (nm in c("S","C","D","V")) {
  x <- get(nm)
  if ("Level" %in% names(x)) x$Level <- norm_level(x$Level)
  assign(nm,x)
}

# =========================
# STANDARDIZED P50 TRAJECTORIES
# =========================
p50_candidates <- c("P50","P50_0","P50.0","Median")
p50 <- intersect(p50_candidates,names(C))
if (!length(p50)) {
  nn <- toupper(gsub("[^A-Za-z0-9]","",names(C)))
  ii <- which(nn=="P50")
  if (!length(ii)) stop("P50 column not found in CentileCurves.")
  p50 <- names(C)[ii[1]]
} else p50 <- p50[1]

sdtab <- S %>% select(Outcome,SampleSD) %>% distinct()
CS <- C %>% left_join(sdtab,by="Outcome")
CS$Age <- safe_num(CS$Age); CS$P50_raw <- safe_num(CS[[p50]]); CS$SampleSD <- safe_num(CS$SampleSD)
CS <- CS %>% group_by(Outcome) %>% group_modify(function(.x,.y){
  aa <- .x$Age[is.finite(.x$Age)]
  if (!length(aa)) return(.x)
  ref_age <- if (is.null(REFERENCE_AGE)) min(aa) else as.numeric(REFERENCE_AGE)
  ref <- safe_approx(.x$Age,.x$P50_raw,ref_age)
  .x$ReferenceAge <- ref_age
  .x$StandardizedTrajectory <- (.x$P50_raw-ref)/.x$SampleSD
  .x
}) %>% ungroup()

# Derivative standardized
DS <- D
DS$Age <- safe_num(DS$Age)
if (!"Derivative_per_SD" %in% names(DS)) {
  DS <- DS %>% left_join(sdtab,by="Outcome")
  DS$Derivative_per_SD <- safe_num(DS$Derivative)/safe_num(DS$SampleSD)
}

# Variability standardized
VS <- V %>% left_join(sdtab,by="Outcome")
VS$Age <- safe_num(VS$Age)
VS$Conditional_SD <- safe_num(VS$Conditional_SD)
VS$SampleSD <- safe_num(VS$SampleSD)
VS$Conditional_SD_per_SampleSD <- VS$Conditional_SD/VS$SampleSD

# A single, shared source for all main whole-cortex/lobar figures.
# Hemisphere-specific lobe records are reserved for supplementary S1/S2.
S_MACRO  <- keep_bilateral_macro(S)
CS_MACRO <- keep_bilateral_macro(CS)
DS_MACRO <- keep_bilateral_macro(DS)
VS_MACRO <- keep_bilateral_macro(VS)

# =========================
# M1: MULTISCALE TRAJECTORIES
# =========================
dat <- CS_MACRO %>% filter(Measure %in% MEASURE_LEVELS,
                     is.finite(StandardizedTrajectory)) %>%
  mutate(
    Unit=ifelse(Level=="WholeBrain","Whole cortex",as.character(Region)),
    Measure=factor(Measure,levels=MEASURE_LEVELS)
  )
assert_one_macro_curve(dat,"StandardizedTrajectory","M1")

M1_MARK <- make_curve_markers(
  curve_df=CS_MACRO,
  summary_df=S_MACRO,
  eligible_levels=MACRO_LEVELS,
  y_col="StandardizedTrajectory",
  include_peak=TRUE,
  include_max_change=TRUE
)

if (nrow(dat)) {
  p <- add_bands(ggplot(),legend=FALSE,labels=TRUE) +
    geom_hline(yintercept=0,linetype="dashed",linewidth=.35,color="#555555") +
    geom_line(
      data=dat,
      aes(Age,StandardizedTrajectory,color=Unit,
          group=interaction(Measure,Unit)),
      linewidth=.95,
      lineend="round"
    ) +
    facet_wrap(~Measure,ncol=2,scales="free_y",labeller=measure_age_box_labeller) +
    cortical_color_scale() +
    labs(
      title="Multiscale standardized normative trajectories",
      subtitle="Whole-cortex and lobar trajectories in outcome SD units",
      x="Age (years)",
      y="Standardized change from reference age (SD)",
      color="Cortical scale"
    ) +
    publication_curve_theme(base_size=10.8)

  p <- add_curve_markers(p,M1_MARK)
  p <- p + guides(
    color=guide_legend(order=1,nrow=1,byrow=TRUE,
                       override.aes=list(linewidth=1.2)),
    shape=guide_legend(order=2,nrow=1,byrow=TRUE),
    fill="none"
  )
  p <- add_age_axis(p)
  p <- add_age_headers(p, dat, "Measure", "StandardizedTrajectory")

  regfig(
    "M1",
    "Multiscale standardized normative trajectories",
    save_pair(p,"M1_Multiscale_Standardized_Trajectories",MAIN_DIR,12,8.7)
  )
} else {
  regfig(
    "M1",
    "Multiscale standardized normative trajectories",
    status="SKIPPED",
    note="No macro trajectory data."
  )
}

# =========================
# M2: MULTISCALE AGE GRADIENTS
# =========================
dat <- DS_MACRO %>% filter(Measure %in% MEASURE_LEVELS,
                     is.finite(safe_num(Derivative_per_SD))) %>%
  mutate(
    Unit=ifelse(Level=="WholeBrain","Whole cortex",as.character(Region)),
    Measure=factor(Measure,levels=MEASURE_LEVELS)
  )
assert_one_macro_curve(dat,"Derivative_per_SD","M2")

M2_MARK <- make_curve_markers(
  curve_df=DS_MACRO,
  summary_df=S_MACRO,
  eligible_levels=MACRO_LEVELS,
  y_col="Derivative_per_SD",
  include_peak=FALSE,
  include_max_change=TRUE
)

if (nrow(dat)) {
  p <- add_bands(ggplot(),legend=FALSE,labels=TRUE) +
    geom_hline(yintercept=0,linetype="dashed",linewidth=.35,color="#555555") +
    geom_line(
      data=dat,
      aes(Age,Derivative_per_SD,color=Unit,
          group=interaction(Measure,Unit)),
      linewidth=.95,
      lineend="round"
    ) +
    facet_wrap(~Measure,ncol=2,scales="free_y",labeller=measure_age_box_labeller) +
    cortical_color_scale() +
    labs(
      title="Multiscale standardized cross-sectional age gradients",
      subtitle="Derivative of conditional median in SD/year; triangles mark maximal age-related change",
      x="Age (years)",
      y="Standardized age gradient (SD/year)",
      color="Cortical scale"
    ) +
    publication_curve_theme(base_size=10.8)

  p <- add_curve_markers(p,M2_MARK)
  p <- p + guides(
    color=guide_legend(order=1,nrow=1,byrow=TRUE,
                       override.aes=list(linewidth=1.2)),
    shape=guide_legend(order=2,nrow=1,byrow=TRUE),
    fill="none"
  )
  p <- add_age_axis(p)
  p <- add_age_headers(p, dat, "Measure", "Derivative_per_SD")

  regfig(
    "M2",
    "Multiscale standardized cross-sectional age gradients",
    save_pair(p,"M2_Multiscale_Standardized_Age_Gradients",MAIN_DIR,12,8.7)
  )
} else {
  regfig(
    "M2",
    "Multiscale standardized cross-sectional age gradients",
    status="SKIPPED",
    note="No macro derivative data."
  )
}

# =========================
# M3: PEAK / TROUGH / MAX CHANGE TIMING
# =========================
rows <- list()

for (i in seq_len(nrow(S_MACRO))) {
  r <- S_MACRO[i,,drop=FALSE]

  if (!r$Level %in% MACRO_LEVELS ||
      !r$Measure %in% MEASURE_LEVELS) next

  unit <- if (r$Level=="WholeBrain") {
    "Whole cortex"
  } else if (r$Level=="Hemisphere") {
    pretty_hemisphere(
      if ("Hemisphere" %in% names(r)) r$Hemisphere else r$Region
    )
  } else {
    as.character(r$Region)
  }

  if (all(c("InternalPeak","PeakAge") %in% names(r)) &&
      truthy(r$InternalPeak)) {
    rows[[length(rows)+1L]] <- data.frame(
      Measure=r$Measure,
      Unit=unit,
      Milestone="Peak",
      Age=safe_num(r$PeakAge),
      L95=if("PeakAge_L95"%in%names(r)) safe_num(r$PeakAge_L95) else NA_real_,
      U95=if("PeakAge_U95"%in%names(r)) safe_num(r$PeakAge_U95) else NA_real_,
      Stable=if("PeakStable"%in%names(r)) truthy(r$PeakStable) else NA
    )
  }

  if (all(c("InternalTrough","TroughAge") %in% names(r)) &&
      truthy(r$InternalTrough)) {
    rows[[length(rows)+1L]] <- data.frame(
      Measure=r$Measure,
      Unit=unit,
      Milestone="Trough",
      Age=safe_num(r$TroughAge),
      L95=if("TroughAge_L95"%in%names(r)) safe_num(r$TroughAge_L95) else NA_real_,
      U95=if("TroughAge_U95"%in%names(r)) safe_num(r$TroughAge_U95) else NA_real_,
      Stable=if("TroughStable"%in%names(r)) truthy(r$TroughStable) else NA
    )
  }

  if (all(c("InternalMaxChange","MaxChangeAge") %in% names(r)) &&
      truthy(r$InternalMaxChange)) {
    rows[[length(rows)+1L]] <- data.frame(
      Measure=r$Measure,
      Unit=unit,
      Milestone="Maximal age-related change",
      Age=safe_num(r$MaxChangeAge),
      L95=if("MaxChangeAge_L95"%in%names(r)) safe_num(r$MaxChangeAge_L95) else NA_real_,
      U95=if("MaxChangeAge_U95"%in%names(r)) safe_num(r$MaxChangeAge_U95) else NA_real_,
      Stable=if("MaxChangeStable"%in%names(r)) truthy(r$MaxChangeStable) else NA
    )
  }
}

MILE <- if(length(rows)) bind_rows(rows) else data.frame()

if (nrow(MILE)) {
  MILE$Measure <- factor(MILE$Measure,levels=MEASURE_LEVELS)
  MILE$Milestone <- factor(
    MILE$Milestone,
    levels=c("Peak","Trough","Maximal age-related change")
  )
  MILE$Stability <- ifelse(
    is.na(MILE$Stable),
    "Unknown",
    ifelse(MILE$Stable,"Stable","Unstable")
  )

  p <- add_bands(ggplot(),legend=FALSE,labels=TRUE)

  ci <- MILE %>% filter(is.finite(L95),is.finite(U95))
  if (nrow(ci)) {
    p <- p +
      geom_errorbarh(
        data=ci,
        aes(y=Unit,xmin=L95,xmax=U95,color=Milestone),
        height=0,
        linewidth=.55,
        alpha=.55,
        show.legend=FALSE
      )
  }

  p <- p +
    geom_point(
      data=MILE,
      aes(Age,Unit,color=Milestone,shape=Milestone,alpha=Stability),
      size=3.1,
      stroke=.55
    ) +
    scale_color_manual(
      values=MILESTONE_COLORS,
      breaks=c("Peak","Trough","Maximal age-related change"),
      name="Milestone"
    ) +
    scale_shape_manual(
      values=MILESTONE_SHAPES,
      breaks=c("Peak","Trough","Maximal age-related change"),
      name="Milestone"
    ) +
    scale_alpha_manual(
      values=c("Stable"=1,"Unstable"=.35,"Unknown"=.65),
      name="Bootstrap stability"
    ) +
    facet_wrap(~Measure,ncol=2,scales="free_y",labeller=measure_age_box_labeller) +
    labs(
      title="Timing of internal extrema and maximal age-related change",
      x="Age (years)",
      y=NULL
    ) +
    guides(
      color=guide_legend(order=1,nrow=1,byrow=TRUE),
      shape=guide_legend(order=1,nrow=1,byrow=TRUE),
      alpha=guide_legend(order=2,nrow=1,byrow=TRUE)
    ) +
    publication_curve_theme(base_size=10) +
    coord_cartesian(clip="off") +
    theme(
      plot.margin=margin(30,8,8,8)
    )

  p <- add_age_axis(p)

  regfig(
    "M3",
    "Timing of internal extrema and maximal age-related change",
    save_pair(p,"M3_Milestone_Timing",MAIN_DIR,12,10)
  )

  write.csv(
    MILE,
    file.path(TABLE_DIR,"M3_Milestone_Timing_Data.csv"),
    row.names=FALSE
  )
} else {
  regfig(
    "M3",
    "Timing of internal extrema and maximal age-related change",
    status="SKIPPED",
    note="No macro milestones."
  )
}

# =========================
# ggseg DK COMPATIBILITY LAYER
# Supports both legacy ggseg (dk object + "side") and newer ggseg
# (dk() atlas + "view").
# =========================

get_dk_atlas_compat <- function() {
  dk_obj <- get("dk", envir = asNamespace("ggseg"))
  if (is.function(dk_obj)) {
    dk_obj()
  } else {
    dk_obj
  }
}

get_dk_position_compat <- function() {
  # ggseg < 2 used "side"; current ggseg uses "view".
  ver <- tryCatch(
    utils::packageVersion("ggseg"),
    error = function(e) numeric_version("0")
  )

  if (ver >= numeric_version("2.0.0")) {
    ggseg::position_brain(hemi ~ view)
  } else {
    ggseg::position_brain(hemi ~ side)
  }
}

DK_ATLAS <- get_dk_atlas_compat()
DK_POSITION <- get_dk_position_compat()

message(
  "ggseg compatibility: version=", as.character(utils::packageVersion("ggseg")),
  " | atlas interface=", if (is.function(get("dk", envir=asNamespace("ggseg")))) "dk()" else "dk object",
  " | position column=", if (utils::packageVersion("ggseg") >= numeric_version("2.0.0")) "view" else "side"
)

# =========================
# M4/M5: DK68 BRAIN MAPS
# Explicit FreeSurfer DK68 -> ggseg DK mapping.
# ggseg expects atlas region names such as "insula", "precentral",
# "superior parietal" plus hemisphere information.
# =========================

fs_dk_to_ggseg <- c(
  "bankssts"="bankssts",
  "caudalanteriorcingulate"="caudal anterior cingulate",
  "caudalmiddlefrontal"="caudal middle frontal",
  "cuneus"="cuneus",
  "entorhinal"="entorhinal",
  "fusiform"="fusiform",
  "inferiorparietal"="inferior parietal",
  "inferiortemporal"="inferior temporal",
  "isthmuscingulate"="isthmus cingulate",
  "lateraloccipital"="lateral occipital",
  "lateralorbitofrontal"="lateral orbitofrontal",
  "lingual"="lingual",
  "medialorbitofrontal"="medial orbitofrontal",
  "middletemporal"="middle temporal",
  "parahippocampal"="parahippocampal",
  "paracentral"="paracentral",
  "parsopercularis"="pars opercularis",
  "parsorbitalis"="pars orbitalis",
  "parstriangularis"="pars triangularis",
  "pericalcarine"="pericalcarine",
  "postcentral"="postcentral",
  "posteriorcingulate"="posterior cingulate",
  "precentral"="precentral",
  "precuneus"="precuneus",
  "rostralanteriorcingulate"="rostral anterior cingulate",
  "rostralmiddlefrontal"="rostral middle frontal",
  "superiorfrontal"="superior frontal",
  "superiorparietal"="superior parietal",
  "superiortemporal"="superior temporal",
  "supramarginal"="supramarginal",
  "frontalpole"="frontal pole",
  "temporalpole"="temporal pole",
  "transversetemporal"="transverse temporal",
  "insula"="insula"
)

convert_fs_roi_to_ggseg <- function(region_name) {
  z <- as.character(region_name)
  hemi <- ifelse(grepl("^lh_", z, ignore.case=TRUE), "left",
                 ifelse(grepl("^rh_", z, ignore.case=TRUE), "right", NA_character_))
  base <- sub("^(lh_|rh_)", "", z, ignore.case=TRUE)
  atlas_region <- unname(fs_dk_to_ggseg[base])
  data.frame(
    hemi=hemi,
    region=atlas_region,
    FS_Region=z,
    stringsAsFactors=FALSE
  )
}

ROI <- S %>%
  filter(Level=="ROI",Measure %in% MEASURE_LEVELS)

if (nrow(ROI)) {

  map_parts <- lapply(ROI$Region, convert_fs_roi_to_ggseg)
  map_info <- bind_rows(map_parts)

  ROI$hemi <- map_info$hemi
  ROI$region <- map_info$region

  if ("InternalMaxChange" %in% names(ROI)) {
    ROI$MaxChangeAge_plot <- ifelse(
      vapply(ROI$InternalMaxChange, truthy, logical(1)),
      safe_num(ROI$MaxChangeAge),
      NA_real_
    )
  } else {
    ROI$MaxChangeAge_plot <- NA_real_
  }

  ROI$MaxChangePerSD <- safe_num(ROI$DerivativeAtMaxChange) / safe_num(ROI$SampleSD)

  unmapped <- ROI %>%
    filter(is.na(hemi) | is.na(region)) %>%
    distinct(Region)

  if (nrow(unmapped)) {
    write.csv(
      unmapped,
      file.path(TABLE_DIR,"DK68_Unmapped_ROIs.csv"),
      row.names=FALSE
    )
    warning(
      "Some ROI names could not be mapped to ggseg DK. See Tables/DK68_Unmapped_ROIs.csv"
    )
  }

  d4 <- ROI %>%
    filter(!is.na(hemi),!is.na(region),is.finite(MaxChangeAge_plot)) %>%
    select(hemi,region,Measure,MaxChangeAge_plot)

  if(nrow(d4)) {
    p <- ggplot(d4) +
      geom_brain(
        atlas=DK_ATLAS,
        position=DK_POSITION,
        aes(fill=MaxChangeAge_plot),
        color="grey40"
      ) +
      facet_wrap(~Measure,ncol=1) +
      scale_fill_viridis_c(
        option="plasma",
        na.value="grey90",
        name="Age (years)"
      ) +
      labs(
        title="DK68 cortical map of age at maximal age-related change",
        subtitle="Grey = no eligible internal maximal-change milestone"
      ) +
      theme_void(base_size=10) +
      theme(
        strip.text=element_text(face="bold"),
        plot.title=element_text(face="bold"),
        legend.position="bottom"
      )

    regfig(
      "M4",
      "DK68 cortical map of age at maximal age-related change",
      save_pair(p,"M4_DK68_Map_MaxChange_Age",MAIN_DIR,11,13)
    )
  } else {
    regfig(
      "M4",
      "DK68 cortical map of age at maximal age-related change",
      status="SKIPPED",
      note="No mapped ROI maximal-change ages."
    )
  }

  d5 <- ROI %>%
    filter(!is.na(hemi),!is.na(region),is.finite(MaxChangePerSD)) %>%
    select(hemi,region,Measure,MaxChangePerSD)

  if(nrow(d5)) {
    lim <- max(abs(d5$MaxChangePerSD),na.rm=TRUE)

    p <- ggplot(d5) +
      geom_brain(
        atlas=DK_ATLAS,
        position=DK_POSITION,
        aes(fill=MaxChangePerSD),
        color="grey40"
      ) +
      facet_wrap(~Measure,ncol=1) +
      scale_fill_gradient2(
        low=HEATMAP_DIVERGING_LOW,
        mid=HEATMAP_DIVERGING_MID,
        high=HEATMAP_DIVERGING_HIGH,
        midpoint=0,
        limits=c(-lim,lim),
        na.value="grey90",
        name="SD/year"
      ) +
      labs(
        title="DK68 cortical map of maximal standardized age-related change",
        subtitle="Blue = decrease; red = increase"
      ) +
      theme_void(base_size=10) +
      theme(
        strip.text=element_text(face="bold"),
        plot.title=element_text(face="bold"),
        legend.position="bottom"
      )

    regfig(
      "M5",
      "DK68 cortical map of maximal standardized age-related change",
      save_pair(p,"M5_DK68_Map_MaxChange_Magnitude",MAIN_DIR,11,13)
    )
  } else {
    regfig(
      "M5",
      "DK68 cortical map of maximal standardized age-related change",
      status="SKIPPED",
      note="No mapped ROI maximal-change magnitudes."
    )
  }
} else {
  regfig("M4","DK68 cortical map of age at maximal age-related change",
         status="SKIPPED",note="No ROI summary data.")
  regfig("M5","DK68 cortical map of maximal standardized age-related change",
         status="SKIPPED",note="No ROI summary data.")
}

# =========================
# M6: CONDITIONAL VARIABILITY HEATMAP
# =========================
MV <- VS_MACRO %>% filter(Measure %in% MEASURE_LEVELS) %>%
  group_by(Outcome,Measure,Level,Region) %>%
  summarise(SD_early=safe_approx(Age,Conditional_SD_per_SampleSD,VAR_AGE_EARLY),
            SD_late=safe_approx(Age,Conditional_SD_per_SampleSD,VAR_AGE_LATE),.groups="drop") %>%
  mutate(Delta_ConditionalSD=SD_late-SD_early,
         Unit=ifelse(Level=="WholeBrain","Whole cortex",as.character(Region)))
mv_dup <- MV %>% count(Measure,Unit,name="n") %>% filter(n>1)
if(nrow(mv_dup)) {
  stop("M6: more than one bilateral value remains for a Measure × Unit tile.")
}
if(nrow(MV) && any(is.finite(MV$Delta_ConditionalSD))) {
  lim <- max(abs(MV$Delta_ConditionalSD),na.rm=TRUE)
  p <- ggplot(MV,aes(Measure,Unit,fill=Delta_ConditionalSD)) +
    geom_tile(color="white",linewidth=.4) +
    geom_text(
      aes(label=ifelse(is.finite(Delta_ConditionalSD),
                       sprintf("%.3f",Delta_ConditionalSD),"")),
      color="#202020",size=3.2,fontface="bold"
    ) +
    scale_fill_gradient2(low=HEATMAP_DIVERGING_LOW,mid=HEATMAP_DIVERGING_MID,high=HEATMAP_DIVERGING_HIGH,midpoint=0,
                         limits=c(-lim,lim),
                         name=paste0("Δ Conditional SD\n(age ",VAR_AGE_LATE,"-",VAR_AGE_EARLY,")\n/sample SD")) +
    labs(title="Age-related change in conditional variability",
         subtitle="Positive = greater heterogeneity at the later age",x=NULL,y=NULL) +
    theme_minimal(base_size=11) +
    theme(panel.grid=element_blank(),plot.title=element_text(face="bold"),
          axis.text.x=element_text(angle=30,hjust=1))
  regfig("M6","Age-related change in conditional variability",
         save_pair(p,"M6_Conditional_Variability_Heatmap",MAIN_DIR,9,7))
  write.csv(MV,file.path(TABLE_DIR,"M6_Conditional_Variability_Data.csv"),row.names=FALSE)
}

# =========================
# S1/S2: HEMISPHERE TRAJECTORY + GRADIENT
# =========================
HT <- CS %>%
  filter(Level=="Hemisphere",Measure %in% MEASURE_LEVELS,
         is.finite(StandardizedTrajectory)) %>%
  mutate(
    Unit=resolve_hemisphere(
      hemisphere=if ("Hemisphere" %in% names(.)) Hemisphere else NULL,
      region=if ("Region" %in% names(.)) Region else NULL,
      outcome=if ("Outcome" %in% names(.)) Outcome else NULL
    ),
    Unit=factor(Unit,levels=c("Left hemisphere","Right hemisphere")),
    Measure=factor(Measure,levels=MEASURE_LEVELS)
  ) %>%
  filter(!is.na(Unit))

S1_LABS <- make_end_labels(HT, "StandardizedTrajectory")

S1_MARK <- make_curve_markers(
  curve_df=CS,
  summary_df=S,
  eligible_levels="Hemisphere",
  y_col="StandardizedTrajectory",
  include_peak=TRUE,
  include_max_change=TRUE
)

if(nrow(HT)) {
  p <- add_bands(ggplot(),legend=FALSE,labels=TRUE) +
    geom_hline(yintercept=0,linetype="dashed",linewidth=.35,color="#555555") +
    geom_line(
      data=HT,
      aes(Age,StandardizedTrajectory,color=Unit,
          linetype=Unit,
          group=interaction(Unit,Outcome)),
      linewidth=1.0,
      lineend="round"
    ) +
    facet_wrap(~Measure,ncol=2,scales="free_y",labeller=measure_age_box_labeller) +
    hemisphere_color_scale() +
    scale_linetype_manual(
      values=c("Left hemisphere"="solid","Right hemisphere"="solid"),
      guide="none"
    ) +
    labs(
      title="Hemisphere standardized normative trajectories",
      x="Age (years)",
      y="Standardized change (SD)",
      color="Hemisphere"
    ) +
    publication_curve_theme(base_size=10.5)

  p <- add_curve_markers(p,S1_MARK)
  p <- add_end_labels(p,S1_LABS)
  p <- p + guides(
    color=guide_legend(order=1,nrow=1,byrow=TRUE,
                       override.aes=list(linewidth=1.2)),
    shape=guide_legend(order=2,nrow=1,byrow=TRUE),
    fill="none"
  )
  p <- add_age_axis(p)
  p <- add_age_headers(p, HT, "Measure", "StandardizedTrajectory")

  regfig(
    "S1",
    "Hemisphere standardized normative trajectories",
    save_pair(p,"S1_Hemisphere_Standardized_Trajectories",SUPP_DIR,11,8.5)
  )
}

HD <- DS %>%
  filter(Level=="Hemisphere",Measure %in% MEASURE_LEVELS,
         is.finite(safe_num(Derivative_per_SD))) %>%
  mutate(
    Unit=resolve_hemisphere(
      hemisphere=if ("Hemisphere" %in% names(.)) Hemisphere else NULL,
      region=if ("Region" %in% names(.)) Region else NULL,
      outcome=if ("Outcome" %in% names(.)) Outcome else NULL
    ),
    Unit=factor(Unit,levels=c("Left hemisphere","Right hemisphere")),
    Measure=factor(Measure,levels=MEASURE_LEVELS)
  ) %>%
  filter(!is.na(Unit))

S2_LABS <- make_end_labels(HD, "Derivative_per_SD")

S2_MARK <- make_curve_markers(
  curve_df=DS,
  summary_df=S,
  eligible_levels="Hemisphere",
  y_col="Derivative_per_SD",
  include_peak=FALSE,
  include_max_change=TRUE
)

if(nrow(HD)) {
  p <- add_bands(ggplot(),legend=FALSE,labels=TRUE) +
    geom_hline(yintercept=0,linetype="dashed",linewidth=.35,color="#555555") +
    geom_line(
      data=HD,
      aes(Age,Derivative_per_SD,color=Unit,
          linetype=Unit,
          group=interaction(Unit,Outcome)),
      linewidth=1.0,
      lineend="round"
    ) +
    facet_wrap(~Measure,ncol=2,scales="free_y",labeller=measure_age_box_labeller) +
    hemisphere_color_scale() +
    scale_linetype_manual(
      values=c("Left hemisphere"="solid","Right hemisphere"="solid"),
      guide="none"
    ) +
    labs(
      title="Hemisphere standardized cross-sectional age gradients",
      x="Age (years)",
      y="Standardized age gradient (SD/year)",
      color="Hemisphere"
    ) +
    publication_curve_theme(base_size=10.5)

  p <- add_curve_markers(p,S2_MARK)
  p <- add_end_labels(p,S2_LABS)
  p <- p + guides(
    color=guide_legend(order=1,nrow=1,byrow=TRUE,
                       override.aes=list(linewidth=1.2)),
    shape=guide_legend(order=2,nrow=1,byrow=TRUE),
    fill="none"
  )
  p <- add_age_axis(p)
  p <- add_age_headers(p, HD, "Measure", "Derivative_per_SD")

  regfig(
    "S2",
    "Hemisphere standardized cross-sectional age gradients",
    save_pair(p,"S2_Hemisphere_Standardized_Age_Gradients",SUPP_DIR,11,8.5)
  )
}

# =========================
# S3/S4: YEO7 TRAJECTORY + GRADIENT
# =========================
YT <- CS %>%
  filter(Level=="Yeo7",Measure %in% MEASURE_LEVELS,
         is.finite(StandardizedTrajectory)) %>%
  mutate(
    Unit=as.character(Region),
    Measure=factor(Measure,levels=MEASURE_LEVELS)
  )

S3_MARK <- make_curve_markers(
  curve_df=CS,
  summary_df=S,
  eligible_levels="Yeo7",
  y_col="StandardizedTrajectory",
  include_peak=TRUE,
  include_max_change=TRUE
)

if(nrow(YT)) {
  p <- add_bands(ggplot(),legend=FALSE,labels=TRUE) +
    geom_hline(yintercept=0,linetype="dashed",linewidth=.35,color="#555555") +
    geom_line(
      data=YT,
      aes(Age,StandardizedTrajectory,color=Unit,
          group=interaction(Unit,Outcome)),
      linewidth=.9,
      lineend="round"
    ) +
    facet_wrap(~Measure,ncol=2,scales="free_y",labeller=measure_age_box_labeller) +
    yeo7_color_scale() +
    labs(
      title="Yeo7 standardized normative trajectories",
      x="Age (years)",
      y="Standardized change (SD)",
      color="Yeo7 network"
    ) +
    publication_curve_theme(base_size=10)

  p <- add_curve_markers(p,S3_MARK)
  p <- p + guides(
    color=guide_legend(order=1,nrow=2,byrow=TRUE),
    shape=guide_legend(order=2,nrow=1,byrow=TRUE),
    fill="none"
  )
  p <- add_age_axis(p)
  p <- add_age_headers(p, YT, "Measure", "StandardizedTrajectory")

  regfig(
    "S3",
    "Yeo7 standardized normative trajectories",
    save_pair(p,"S3_Yeo7_Standardized_Trajectories",SUPP_DIR,11,8.5)
  )
}

YD <- DS %>%
  filter(Level=="Yeo7",Measure %in% MEASURE_LEVELS,
         is.finite(safe_num(Derivative_per_SD))) %>%
  mutate(
    Unit=as.character(Region),
    Measure=factor(Measure,levels=MEASURE_LEVELS)
  )

S4_MARK <- make_curve_markers(
  curve_df=DS,
  summary_df=S,
  eligible_levels="Yeo7",
  y_col="Derivative_per_SD",
  include_peak=FALSE,
  include_max_change=TRUE
)

if(nrow(YD)) {
  p <- add_bands(ggplot(),legend=FALSE,labels=TRUE) +
    geom_hline(yintercept=0,linetype="dashed",linewidth=.35,color="#555555") +
    geom_line(
      data=YD,
      aes(Age,Derivative_per_SD,color=Unit,
          group=interaction(Unit,Outcome)),
      linewidth=.9,
      lineend="round"
    ) +
    facet_wrap(~Measure,ncol=2,scales="free_y",labeller=measure_age_box_labeller) +
    yeo7_color_scale() +
    labs(
      title="Yeo7 standardized cross-sectional age gradients",
      x="Age (years)",
      y="Standardized age gradient (SD/year)",
      color="Yeo7 network"
    ) +
    publication_curve_theme(base_size=10)

  p <- add_curve_markers(p,S4_MARK)
  p <- p + guides(
    color=guide_legend(order=1,nrow=2,byrow=TRUE),
    shape=guide_legend(order=2,nrow=1,byrow=TRUE),
    fill="none"
  )
  p <- add_age_axis(p)
  p <- add_age_headers(p, YD, "Measure", "Derivative_per_SD")

  regfig(
    "S4",
    "Yeo7 standardized cross-sectional age gradients",
    save_pair(p,"S4_Yeo7_Standardized_Age_Gradients",SUPP_DIR,11,8.5)
  )
}

# =========================
# S5: CONDITIONAL SD TRAJECTORIES
# =========================
CV <- VS_MACRO %>%
  filter(
    Measure %in% MEASURE_LEVELS,
    is.finite(Conditional_SD_per_SampleSD)
  ) %>%
  mutate(
    Unit=ifelse(Level=="WholeBrain","Whole cortex",as.character(Region)),
    Measure=factor(Measure,levels=MEASURE_LEVELS)
  )
assert_one_macro_curve(CV,"Conditional_SD_per_SampleSD","S5")

if(nrow(CV)) {
  p <- add_bands(ggplot(),legend=FALSE,labels=TRUE) +
    geom_line(
      data=CV,
      aes(
        Age,
        Conditional_SD_per_SampleSD,
        color=Unit,
        group=interaction(Measure,Unit)
      ),
      linewidth=.9,
      lineend="round"
    ) +
    facet_wrap(~Measure,ncol=2,scales="free_y",labeller=measure_age_box_labeller) +
    cortical_color_scale() +
    labs(
      title="Conditional variability across age",
      subtitle="Conditional SD standardized by outcome sample SD",
      x="Age (years)",
      y="Conditional SD / sample SD",
      color="Cortical scale"
    ) +
    publication_curve_theme(base_size=10)

  p <- p + guides(
    color=guide_legend(order=1,nrow=2,byrow=TRUE),
    fill="none"
  )
  p <- add_age_axis(p)
  p <- add_age_headers(p, CV, "Measure", "Conditional_SD_per_SampleSD")

  regfig(
    "S5",
    "Conditional variability across age",
    save_pair(p,"S5_Conditional_SD_Trajectories",SUPP_DIR,11,8.5)
  )
}

# =========================
# S6: ROI MILESTONE HEATMAP
# One integrated heatmap: ROI names shown once, with three 5-column blocks
# (Maximal age-related change, Peak, Trough) separated by dashed vertical lines.
# =========================
rr <- list()

for(i in seq_len(nrow(S))) {
  r <- S[i,,drop=FALSE]
  if(r$Level!="ROI" || !r$Measure%in%MEASURE_LEVELS) next

  region_label <- as.character(r$Region)

  if(all(c("InternalMaxChange","MaxChangeAge")%in%names(r)) &&
     truthy(r$InternalMaxChange)) {
    rr[[length(rr)+1L]] <- data.frame(
      Region=region_label,
      Measure=r$Measure,
      Milestone="Maximal age-related change",
      Age=safe_num(r$MaxChangeAge)
    )
  }

  if(all(c("InternalPeak","PeakAge")%in%names(r)) && truthy(r$InternalPeak)) {
    rr[[length(rr)+1L]] <- data.frame(
      Region=region_label,
      Measure=r$Measure,
      Milestone="Peak",
      Age=safe_num(r$PeakAge)
    )
  }

  if(all(c("InternalTrough","TroughAge")%in%names(r)) && truthy(r$InternalTrough)) {
    rr[[length(rr)+1L]] <- data.frame(
      Region=region_label,
      Measure=r$Measure,
      Milestone="Trough",
      Age=safe_num(r$TroughAge)
    )
  }
}

RH <- if(length(rr)) bind_rows(rr) else data.frame()

if(nrow(RH)) {
  RH$Measure <- factor(RH$Measure,levels=MEASURE_LEVELS)
  RH$Milestone <- factor(
    RH$Milestone,
    levels=c("Maximal age-related change","Peak","Trough")
  )

  # Keep a single ROI list and show it only once on the y-axis.
  region_levels <- sort(unique(as.character(RH$Region)), decreasing=TRUE)
  RH$Region <- factor(RH$Region, levels=region_levels)

  # Build a 15-column axis: 5 measures per milestone block.
  x_levels <- c(
    paste0("MAC__", MEASURE_LEVELS),
    paste0("PEAK__", MEASURE_LEVELS),
    paste0("TROUGH__", MEASURE_LEVELS)
  )
  x_labels <- rep(MEASURE_LEVELS, 3)

  RH <- RH %>%
    mutate(
      XBlock=dplyr::case_when(
        Milestone=="Maximal age-related change" ~ paste0("MAC__", Measure),
        Milestone=="Peak" ~ paste0("PEAK__", Measure),
        Milestone=="Trough" ~ paste0("TROUGH__", Measure),
        TRUE ~ NA_character_
      ),
      XBlock=factor(XBlock, levels=x_levels)
    )

  p <- ggplot(RH, aes(XBlock, Region, fill=Age)) +
    geom_tile(color="white", linewidth=.24) +
    # Dashed separators only across the heatmap body, so they do not
    # cross the milestone headings above the plot.
    annotate(
      "segment",
      x=5.5, xend=5.5,
      y=.5, yend=length(region_levels)+.5,
      linetype="dashed",
      color="#777777",
      linewidth=.55
    ) +
    annotate(
      "segment",
      x=10.5, xend=10.5,
      y=.5, yend=length(region_levels)+.5,
      linetype="dashed",
      color="#777777",
      linewidth=.55
    ) +

    # Dedicated header boxes above each milestone block.
    annotate(
      "rect",
      xmin=.55, xmax=5.45,
      ymin=length(region_levels)+1.0,
      ymax=length(region_levels)+2.8,
      fill="white",
      color="#777777",
      linewidth=.55
    ) +
    annotate(
      "rect",
      xmin=5.55, xmax=10.45,
      ymin=length(region_levels)+1.0,
      ymax=length(region_levels)+2.8,
      fill="white",
      color="#777777",
      linewidth=.55
    ) +
    annotate(
      "rect",
      xmin=10.55, xmax=15.45,
      ymin=length(region_levels)+1.0,
      ymax=length(region_levels)+2.8,
      fill="white",
      color="#777777",
      linewidth=.55
    ) +
    annotate(
      "text",
      x=3, y=length(region_levels)+1.9,
      label="Maximal age-related change",
      fontface="bold",
      size=3.0,
      color="#222222"
    ) +
    annotate(
      "text",
      x=8, y=length(region_levels)+1.9,
      label="Peak",
      fontface="bold",
      size=3.0,
      color="#222222"
    ) +
    annotate(
      "text",
      x=13, y=length(region_levels)+1.9,
      label="Trough",
      fontface="bold",
      size=3.0,
      color="#222222"
    ) +
    scale_x_discrete(labels=x_labels, expand=expansion(add=c(0.2,0.2))) +
    scale_fill_gradientn(
      colors=HEATMAP_SEQUENTIAL_COLORS,
      na.value="#EFEFEF",
      name="Age (years)"
    ) +
    coord_cartesian(clip="off") +
    labs(
      title="ROI milestone timing across morphometric measures",
      x=NULL,
      y=NULL
    ) +
    theme_minimal(base_size=7.8) +
    theme(
      panel.grid=element_blank(),
      panel.border=element_rect(color="#7A7A7A", fill=NA, linewidth=.6),
      plot.title=element_text(face="bold"),
      axis.text.x=element_text(angle=35, hjust=1, vjust=1),
      axis.text.y=element_text(size=5.8),
      legend.position="right",
      legend.title=element_text(face="bold"),
      plot.margin=margin(34,8,6,6)
    )

  regfig(
    "S6",
    "ROI milestone timing heatmap",
    save_pair(p,"S6_ROI_Milestone_Age_Heatmap",SUPP_DIR,16,14)
  )
}
# =========================
# S7: VALIDATION / CALIBRATION HEATMAP
# =========================
VV <- if(!is.null(VAL) && nrow(VAL)) add_measure(VAL) else S
if("Level"%in%names(VV)) VV$Level <- norm_level(VV$Level)
metrics <- intersect(c("CV_RMSE","CV_MAE","CV_R2","CV_NLL","Coverage95","PIT_p",
                       "CrossFit_Z_Mean","CrossFit_Z_SD","BootstrapSuccessProp"),names(VV))
if(length(metrics)) {
  Z <- VV %>% filter(Level %in% c("WholeBrain","Lobe","Hemisphere"),Measure%in%MEASURE_LEVELS) %>%
    mutate(Unit=ifelse(Level=="WholeBrain","Whole cortex",as.character(Region))) %>%
    select(Unit,Measure,all_of(metrics)) %>% pivot_longer(all_of(metrics),names_to="Metric",values_to="Value") %>%
    mutate(Value=safe_num(Value)) %>% group_by(Metric) %>%
    mutate(Metric_Z=if(sum(is.finite(Value))>=2 && sd(Value,na.rm=TRUE)>0)as.numeric(scale(Value))else NA_real_) %>% ungroup()
  if(nrow(Z)) {
    p <- ggplot(Z,aes(Metric,interaction(Unit,Measure,sep=" | "),fill=Metric_Z))+
      geom_tile(color="white",linewidth=.2)+
      scale_fill_gradient2(low=HEATMAP_DIVERGING_LOW,mid=HEATMAP_DIVERGING_MID,high=HEATMAP_DIVERGING_HIGH,midpoint=0,name="Within-metric\nstandardized value")+
      labs(title="Model validation and calibration overview",
           subtitle="Heatmap values standardized within metric for display only",x=NULL,y=NULL)+
      theme_minimal(base_size=8)+theme(panel.grid=element_blank(),plot.title=element_text(face="bold"),
                                      axis.text.x=element_text(angle=45,hjust=1))
    regfig("S7","Model validation and calibration overview",
           save_pair(p,"S7_Validation_Calibration_Heatmap",SUPP_DIR,12,10))
    write.csv(Z,file.path(TABLE_DIR,"S7_Validation_Calibration_Data.csv"),row.names=FALSE)
  }
}

# =========================
# S8: SELECTED FAMILY DISTRIBUTION
# =========================
if("Family"%in%names(S)) {
  FAM <- S %>% filter(Measure%in%MEASURE_LEVELS,!is.na(Family),nzchar(trimws(as.character(Family)))) %>%
    count(Measure,Family,name="N")
  if(nrow(FAM)) {
    p <- ggplot(FAM,aes(Family,N,fill=Measure))+geom_col(position="dodge")+
      labs(title="Selected GAMLSS family distribution",x="Selected family",y="Number of outcomes",fill="Measure")+
      theme_classic(base_size=10)+theme(plot.title=element_text(face="bold"),axis.text.x=element_text(angle=45,hjust=1),
                                        legend.position="bottom")
    regfig("S8","Selected GAMLSS family distribution",
           save_pair(p,"S8_Selected_Family_Distribution",SUPP_DIR,11,6))
  }
}

# =========================
# S9: AGE x SEX SENSITIVITY
# =========================
if("AgeSex_P"%in%names(S)) {
  AX <- S %>% filter(Measure%in%MEASURE_LEVELS) %>%
    mutate(p=safe_num(AgeSex_P),
           q=if("AgeSex_q_BH"%in%names(S))safe_num(AgeSex_q_BH)else NA_real_,
           Significant=ifelse(is.finite(q),q<.05,p<.05)) %>%
    count(Level,Measure,Significant,name="N")
  if(nrow(AX)) {
    p <- ggplot(AX,aes(Measure,N,fill=Significant))+geom_col()+
      facet_wrap(~Level,scales="free_y")+
      scale_fill_manual(values=c("FALSE"="grey75","TRUE"="#B2182B"))+
      labs(title="Age-by-Sex sensitivity-analysis summary",x=NULL,y="Number of outcomes",fill="Significant")+
      theme_classic(base_size=10)+theme(plot.title=element_text(face="bold"),axis.text.x=element_text(angle=30,hjust=1),
                                        legend.position="bottom")
    regfig("S9","Age-by-Sex sensitivity-analysis summary",
           save_pair(p,"S9_AgeSex_Sensitivity_Summary",SUPP_DIR,11,7))
  }
}

# =========================
# S10: BOOTSTRAP MILESTONE SUPPORT
# =========================
sc <- intersect(c("PeakBootstrapSupport","TroughBootstrapSupport","MaxChangeBootstrapSupport"),names(S))
if(length(sc)) {
  BS <- S %>% filter(Measure%in%MEASURE_LEVELS) %>% select(Outcome,Measure,Level,Region,all_of(sc)) %>%
    pivot_longer(all_of(sc),names_to="Milestone",values_to="Support") %>%
    mutate(Support=safe_num(Support),
           Milestone=recode(Milestone,
                            PeakBootstrapSupport="Peak",
                            TroughBootstrapSupport="Trough",
                            MaxChangeBootstrapSupport="Maximal age-related change")) %>%
    filter(is.finite(Support))
  if(nrow(BS)) {
    p <- ggplot(BS,aes(Support,fill=Milestone))+geom_histogram(bins=20,position="identity",alpha=.45)+
      geom_vline(xintercept=.80,linetype="dashed",linewidth=.7)+
      facet_grid(Measure~Level,scales="free_y")+
      labs(title="Bootstrap support of age-related milestones",
           subtitle="Dashed line = 0.80 support threshold",x="Bootstrap support proportion",
           y="Number of outcomes",fill="Milestone")+
      theme_classic(base_size=8)+theme(plot.title=element_text(face="bold"),legend.position="bottom")
    regfig("S10","Bootstrap support of age-related milestones",
           save_pair(p,"S10_Milestone_Bootstrap_Stability",SUPP_DIR,13,10))
  }
}

# =========================
# S11/S12: BIOMARKER BURDEN
# S11 is a ranked participant-level burden plot (main descriptive display).
# S12 compares burden across analysis levels.
# =========================
BA <- read_sheet(BIOMARKER_FILE,"IndividualBurden_All")
BL <- read_sheet(BIOMARKER_FILE,"IndividualBurden_Level")

if(!is.null(BA) && nrow(BA) && "Prop_absZ_ge_2"%in%names(BA)) {

  BA$Prop_absZ_ge_2 <- safe_num(BA$Prop_absZ_ge_2)

  BAP <- BA %>%
    filter(is.finite(Prop_absZ_ge_2)) %>%
    arrange(Prop_absZ_ge_2) %>%
    mutate(
      Rank=row_number(),
      BurdenPct=100*Prop_absZ_ge_2
    )

  if(nrow(BAP)) {

    med_b <- median(BAP$BurdenPct,na.rm=TRUE)
    q1_b  <- quantile(BAP$BurdenPct,.25,na.rm=TRUE,names=FALSE)
    q3_b  <- quantile(BAP$BurdenPct,.75,na.rm=TRUE,names=FALSE)
    min_b <- min(BAP$BurdenPct,na.rm=TRUE)
    max_b <- max(BAP$BurdenPct,na.rm=TRUE)

    stats_txt <- paste0(
      "N = ",nrow(BAP),
      "\nMedian = ",sprintf("%.2f",med_b),"%",
      "\nIQR = ",sprintf("%.2f",q1_b),"–",sprintf("%.2f",q3_b),"%",
      "\nRange = ",sprintf("%.2f",min_b),"–",sprintf("%.2f",max_b),"%"
    )

    p <- ggplot(BAP,aes(x=BurdenPct,y=Rank)) +
      annotate(
        "rect",
        xmin=q1_b,xmax=q3_b,
        ymin=-Inf,ymax=Inf,
        fill=BURDEN_IQR_FILL,
        alpha=.75
      ) +
      geom_segment(
        aes(x=0,xend=BurdenPct,y=Rank,yend=Rank),
        color=BURDEN_LINE_COLOR,
        linewidth=.35
      ) +
      geom_point(
        shape=21,
        size=2.0,
        stroke=.35,
        fill=BURDEN_POINT_COLOR,
        color="#35434B"
      ) +
      geom_vline(
        xintercept=med_b,
        linetype="dashed",
        linewidth=.7,
        color=BURDEN_MEDIAN_COLOR
      ) +
      annotate(
        "label",
        x=max_b,
        y=max(BAP$Rank)*.18,
        label=stats_txt,
        hjust=1,
        vjust=1,
        size=3.1,
        fill=scales::alpha("white",.94),
        color="#222222",
        label.size=.25
      ) +
      scale_x_continuous(
        labels=function(x) paste0(sprintf("%.1f",x),"%"),
        expand=expansion(mult=c(.01,.06))
      ) +
      scale_y_continuous(
        breaks=pretty(BAP$Rank,n=6),
        expand=expansion(mult=c(.01,.02))
      ) +
      labs(
        title="Individual structural normative-deviation burden",
        subtitle="Participants ranked by the percentage of structural outcomes with |cross-fitted normative z| ≥ 2",
        x="Deviation burden (%)",
        y="Participants (ranked)",
        caption="Shaded band = interquartile range; dashed line = median burden"
      ) +
      theme_classic(base_size=11) +
      theme(
        plot.title=element_text(face="bold"),
        plot.subtitle=element_text(color="#444444"),
        plot.caption=element_text(color="#666666",hjust=0),
        axis.title=element_text(face="bold")
      )

    regfig(
      "S11",
      "Ranked individual normative-deviation burden",
      save_pair(p,"S11_Individual_Deviation_Burden_Ranked",SUPP_DIR,8.8,6.5)
    )

    write.csv(
      BAP,
      file.path(TABLE_DIR,"S11_Individual_Deviation_Burden_Ranked.csv"),
      row.names=FALSE
    )
  }

} else {
  regfig(
    "S11",
    "Ranked individual normative-deviation burden",
    status="SKIPPED",
    note="IndividualBurden_All or Prop_absZ_ge_2 unavailable."
  )
}

if(!is.null(BL) && nrow(BL) &&
   all(c("Level","Prop_absZ_ge_2")%in%names(BL))) {

  BL$Level <- norm_level(BL$Level)
  BL$Prop_absZ_ge_2 <- safe_num(BL$Prop_absZ_ge_2)

  BLP <- BL %>%
    filter(
      is.finite(Prop_absZ_ge_2),
      tolower(Level) != "cognition"
    ) %>%
    mutate(BurdenPct=100*Prop_absZ_ge_2)

  if(nrow(BLP)) {
    p <- ggplot(BLP,aes(Level,BurdenPct)) +
      geom_boxplot(
        width=.58,
        outlier.shape=NA,
        fill="#E7ECEF",
        color="#53636C",
        linewidth=.55
      ) +
      geom_jitter(
        width=.12,
        alpha=.22,
        size=.75,
        color=BURDEN_POINT_COLOR
      ) +
      scale_y_continuous(
        labels=function(x) paste0(sprintf("%.1f",x),"%")
      ) +
      labs(
        title="Structural normative-deviation burden by analysis level",
        subtitle="Participant-level percentage of structural outcomes with |cross-fitted normative z| ≥ 2",
        x=NULL,
        y="Deviation burden (%)"
      ) +
      theme_classic(base_size=11) +
      theme(
        plot.title=element_text(face="bold"),
        plot.subtitle=element_text(color="#444444"),
        axis.text.x=element_text(angle=25,hjust=1)
      )

    regfig(
      "S12",
      "Normative-deviation burden by analysis level",
      save_pair(p,"S12_Deviation_Burden_By_Level",SUPP_DIR,9,6)
    )
  }
}


# =========================
# S13: GAMLSS vs GAM vs FP/POLY NESTED-CV COMPARISON
# Two-panel figure:
#   A) ΔCV-NLL relative to the best engine within each outcome
#   B) Engine win rate by analysis level and morphometric measure
# This is a sensitivity/comparison figure only; it does not change the
# pre-specified primary normative engine (GAMLSS) and does not refit models.
# =========================
ECA <- read_sheet(MASTER_RESULTS_FILE,"EngineComparisonAll")

if(!is.null(ECA) && nrow(ECA) &&
   all(c("Outcome","Engine","CV_NLL") %in% names(ECA))) {

  ECA <- add_measure(ECA)

  if("Level" %in% names(ECA))
    ECA$Level <- norm_level(ECA$Level)

  ECA$CV_NLL <- safe_num(ECA$CV_NLL)

  # Robustly harmonize possible engine labels exported by different core versions.
  ECA$Engine3 <- dplyr::case_when(
    grepl("gamlss", as.character(ECA$Engine), ignore.case=TRUE) ~ "GAMLSS",
    grepl("^gam$|spline", as.character(ECA$Engine), ignore.case=TRUE) ~ "GAM",
    grepl("fp|poly", as.character(ECA$Engine), ignore.case=TRUE) ~ "FP/POLY",
    TRUE ~ as.character(ECA$Engine)
  )

  ECA$Engine3 <- factor(
    ECA$Engine3,
    levels=c("GAMLSS","GAM","FP/POLY")
  )

  ENG <- ECA %>%
    filter(
      Measure %in% MEASURE_LEVELS,
      !is.na(Engine3),
      is.finite(CV_NLL)
    ) %>%
    group_by(Outcome) %>%
    mutate(
      Best_CV_NLL = min(CV_NLL,na.rm=TRUE),
      Delta_CV_NLL = CV_NLL - Best_CV_NLL,
      Winner = Delta_CV_NLL <= 1e-12
    ) %>%
    ungroup()

  if(nrow(ENG)) {

    # ---------------- Panel A: ΔCV-NLL ----------------
    p_delta <- ggplot(
      ENG,
      aes(
        x=Engine3,
        y=Delta_CV_NLL,
        fill=Engine3
      )
    ) +
      geom_boxplot(
        width=.68,
        outlier.alpha=.20,
        outlier.size=.8,
        linewidth=.45
      ) +
      geom_hline(
        yintercept=0,
        linetype="dashed",
        linewidth=.35,
        color="#666666"
      ) +
      facet_grid(
        Level ~ Measure,
        scales="free_y"
      ) +
      scale_fill_manual(
        values=ENGINE_COLORS,
        breaks=c("GAMLSS","GAM","FP/POLY"),
        drop=FALSE
      ) +
      labs(
        title="A. Cross-validated distributional performance",
        subtitle="ΔCV-NLL relative to the best engine within each outcome; lower is better",
        x=NULL,
        y="Δ CV-NLL"
      ) +
      theme_classic(
        base_size=8.4
      ) +
      theme(
        plot.title=element_text(face="bold"),
        plot.subtitle=element_text(color="#444444"),
        axis.text.x=element_text(angle=28,hjust=1),
        strip.text=element_text(face="bold",size=7.4),
        legend.position="none"
      )

    # ---------------- Panel B: win rates ----------------
    WIN <- ENG %>%
      group_by(Level,Measure,Engine3) %>%
      summarise(
        N_Outcomes=n_distinct(Outcome),
        Wins=sum(Winner,na.rm=TRUE),
        WinRate=Wins/N_Outcomes,
        Median_Delta_CV_NLL=median(Delta_CV_NLL,na.rm=TRUE),
        .groups="drop"
      ) %>%
      mutate(
        Measure=factor(Measure,levels=MEASURE_LEVELS),
        Engine3=factor(Engine3,levels=c("GAMLSS","GAM","FP/POLY"))
      )

    p_win <- ggplot(
      WIN,
      aes(
        x=Measure,
        y=100*WinRate,
        fill=Engine3
      )
    ) +
      geom_col(
        position=position_dodge(width=.78),
        width=.70,
        linewidth=.15
      ) +
      facet_wrap(
        ~Level,
        ncol=3
      ) +
      scale_fill_manual(
        values=ENGINE_COLORS,
        breaks=c("GAMLSS","GAM","FP/POLY"),
        drop=FALSE,
        name="Engine"
      ) +
      scale_y_continuous(
        limits=c(0,100),
        breaks=c(0,25,50,75,100),
        labels=function(x) paste0(x,"%"),
        expand=expansion(mult=c(0,.04))
      ) +
      labs(
        title="B. Outcome-level engine win rates",
        subtitle="Winner = lowest CV-NLL within each outcome; ties are retained",
        x=NULL,
        y="Win rate (%)"
      ) +
      theme_classic(base_size=9.2) +
      theme(
        plot.title=element_text(face="bold"),
        plot.subtitle=element_text(color="#444444"),
        strip.text=element_text(face="bold"),
        axis.text.x=element_text(angle=28,hjust=1),
        legend.position="bottom",
        legend.direction="horizontal"
      ) +
      guides(
        fill=guide_legend(nrow=1,byrow=TRUE)
      )

    p <- p_delta / p_win +
      patchwork::plot_layout(heights=c(1.55,1)) +
      patchwork::plot_annotation(
        title="Three-engine sensitivity comparison: GAMLSS, GAM and FP/POLY",
        subtitle="Cross-validated comparison; GAMLSS remains the pre-specified primary distributional normative engine",
        theme=theme(
          plot.title=element_text(face="bold",size=13),
          plot.subtitle=element_text(color="#444444",size=9.5)
        )
      )

    regfig(
      "S13",
      "Three-engine nested-CV comparison of GAMLSS, GAM and FP/POLY",
      save_pair(
        p,
        "S13_Three_Engine_Comparison_NestedCV",
        SUPP_DIR,
        14,
        13
      )
    )

    write.csv(
      ENG,
      file.path(TABLE_DIR,"S13_Three_Engine_Delta_CVNLL.csv"),
      row.names=FALSE
    )

    write.csv(
      WIN,
      file.path(TABLE_DIR,"S13_Engine_Win_Rates.csv"),
      row.names=FALSE
    )
  }

} else {
  regfig(
    "S13",
    "Three-engine nested-CV comparison of GAMLSS, GAM and FP/POLY",
    status="SKIPPED",
    note="EngineComparisonAll master sheet unavailable."
  )
}

# =========================
# S14: CROSS-FITTED CENTILE CALIBRATION
# =========================
CAL <- read_sheet(MASTER_RESULTS_FILE,"Calibration")

if(!is.null(CAL) && nrow(CAL) &&
   all(c("ExpectedProportion","ObservedProportion","Outcome") %in% names(CAL))) {

  CAL <- add_measure(CAL)

  if("Level" %in% names(CAL))
    CAL$Level <- norm_level(CAL$Level)

  CAL$ExpectedProportion <- safe_num(CAL$ExpectedProportion)
  CAL$ObservedProportion <- safe_num(CAL$ObservedProportion)

  CALP <- CAL %>%
    filter(
      Measure %in% MEASURE_LEVELS,
      is.finite(ExpectedProportion),
      is.finite(ObservedProportion)
    )

  if(nrow(CALP)) {

    CAL_MEAN <- CALP %>%
      group_by(Measure,ExpectedProportion) %>%
      summarise(
        MeanObserved=mean(ObservedProportion,na.rm=TRUE),
        .groups="drop"
      )

    p <- ggplot(
      CALP,
      aes(
        x=ExpectedProportion,
        y=ObservedProportion,
        group=Outcome
      )
    ) +
      geom_abline(
        slope=1,
        intercept=0,
        linetype="dashed",
        color="grey35"
      ) +
      geom_line(
        alpha=.12,
        linewidth=.35
      ) +
      geom_line(
        data=CAL_MEAN,
        aes(
          x=ExpectedProportion,
          y=MeanObserved,
          group=1
        ),
        inherit.aes=FALSE,
        linewidth=1.15
      ) +
      geom_point(
        data=CAL_MEAN,
        aes(
          x=ExpectedProportion,
          y=MeanObserved
        ),
        inherit.aes=FALSE,
        size=1.7
      ) +
      facet_wrap(
        ~Measure,
        ncol=2
      ) +
      coord_equal() +
      scale_x_continuous(
        limits=c(0,1),
        breaks=c(0,.25,.5,.75,1)
      ) +
      scale_y_continuous(
        limits=c(0,1),
        breaks=c(0,.25,.5,.75,1)
      ) +
      labs(
        title="Cross-fitted centile calibration",
        subtitle="Thin lines = individual outcomes; thick line = mean calibration; dashed line = perfect calibration",
        x="Expected cumulative proportion",
        y="Observed cumulative proportion"
      ) +
      theme_classic(
        base_size=10
      ) +
      theme(
        strip.text=element_text(face="bold"),
        plot.title=element_text(face="bold")
      )

    regfig(
      "S14",
      "Cross-fitted centile calibration",
      save_pair(
        p,
        "S14_Centile_Calibration",
        SUPP_DIR,
        10,
        9
      )
    )

    CALERR <- CALP %>%
      mutate(
        CalibrationError=ObservedProportion-ExpectedProportion
      ) %>%
      group_by(
        Outcome,
        OutcomeType,
        Level,
        Region,
        Measure
      ) %>%
      summarise(
        MeanAbsCalibrationError=mean(abs(CalibrationError),na.rm=TRUE),
        MaxAbsCalibrationError=max(abs(CalibrationError),na.rm=TRUE),
        RMSE_CalibrationError=sqrt(mean(CalibrationError^2,na.rm=TRUE)),
        .groups="drop"
      )

    write.csv(
      CALERR,
      file.path(TABLE_DIR,"S14_Centile_Calibration_Error.csv"),
      row.names=FALSE
    )
  }

} else {
  regfig(
    "S14",
    "Cross-fitted centile calibration",
    status="SKIPPED",
    note="Calibration master sheet unavailable."
  )
}

# =========================
# S15: MODEL-SELECTION STABILITY
# =========================
STAB <- read_sheet(MASTER_RESULTS_FILE,"SelectionStability")

if(!is.null(STAB) && nrow(STAB) &&
   all(c("Component","Outcome") %in% names(STAB))) {

  STAB <- add_measure(STAB)

  if("Level" %in% names(STAB))
    STAB$Level <- norm_level(STAB$Level)

  stability_col <- if(
    "MatchFullProportion" %in% names(STAB)
  ) {
    "MatchFullProportion"
  } else if(
    "ModalProportion" %in% names(STAB)
  ) {
    "ModalProportion"
  } else {
    NA_character_
  }

  if(!is.na(stability_col)) {

    STAB$Stability <- safe_num(
      STAB[[stability_col]]
    )

    STAB_SUM <- STAB %>%
      filter(
        Measure %in% MEASURE_LEVELS,
        is.finite(Stability)
      ) %>%
      group_by(Level,Measure,Component) %>%
      summarise(
        MedianStability=median(Stability,na.rm=TRUE),
        Q25=quantile(Stability,.25,na.rm=TRUE,names=FALSE),
        Q75=quantile(Stability,.75,na.rm=TRUE,names=FALSE),
        N=n(),
        .groups="drop"
      )

    if(nrow(STAB_SUM)) {

      p <- ggplot(
        STAB_SUM,
        aes(
          x=Component,
          y=Measure,
          fill=MedianStability
        )
      ) +
        geom_tile(
          color="white",
          linewidth=.4
        ) +
        geom_text(
          aes(
            label=sprintf("%.2f",MedianStability)
          ),
          size=3
        ) +
        facet_wrap(
          ~Level
        ) +
        scale_fill_viridis_c(
          limits=c(0,1),
          option="viridis",
          name="Median\nstability"
        ) +
        labs(
          title="Model-selection stability across nested folds",
          subtitle=paste0("Cell value = median ",stability_col," across outcomes"),
          x=NULL,
          y=NULL
        ) +
        theme_minimal(
          base_size=10
        ) +
        theme(
          panel.grid=element_blank(),
          plot.title=element_text(face="bold"),
          axis.text.x=element_text(angle=30,hjust=1)
        )

      regfig(
        "S15",
        "Model-selection stability",
        save_pair(
          p,
          "S15_Model_Selection_Stability",
          SUPP_DIR,
          12,
          7
        )
      )

      write.csv(
        STAB_SUM,
        file.path(TABLE_DIR,"S15_Model_Selection_Stability.csv"),
        row.names=FALSE
      )
    }
  }

} else {
  regfig(
    "S15",
    "Model-selection stability",
    status="SKIPPED",
    note="SelectionStability master sheet unavailable."
  )
}

# =========================
# S16: CROSS-FITTED NORMATIVE DEVIATION ACROSS AGE GROUPS
# =========================
AGD <- read_sheet(MASTER_RESULTS_FILE,"AgeGroupDeviation")

if(!is.null(AGD) && nrow(AGD) &&
   all(c("Group","Median_Z","Q25_Z","Q75_Z","Outcome") %in% names(AGD))) {

  AGD <- add_measure(AGD)

  if("Level" %in% names(AGD))
    AGD$Level <- norm_level(AGD$Level)

  AGD$Median_Z <- safe_num(AGD$Median_Z)
  AGD$Q25_Z <- safe_num(AGD$Q25_Z)
  AGD$Q75_Z <- safe_num(AGD$Q75_Z)

  AGP <- AGD %>%
    filter(
      Level %in% c("WholeBrain","Lobe"),
      Measure %in% MEASURE_LEVELS,
      is.finite(Median_Z)
    ) %>%
    mutate(
      Unit=ifelse(
        Level=="WholeBrain",
        "Whole cortex",
        as.character(Region)
      )
    )

  if(nrow(AGP)) {

    p <- ggplot(
      AGP,
      aes(
        x=Median_Z,
        y=Unit,
        color=Group
      )
    ) +
      geom_vline(
        xintercept=0,
        color="grey60",
        linewidth=.45
      ) +
      geom_errorbarh(
        aes(
          xmin=Q25_Z,
          xmax=Q75_Z
        ),
        height=.12,
        position=position_dodge(width=.5)
      ) +
      geom_point(
        size=2.2,
        position=position_dodge(width=.5)
      ) +
      facet_wrap(
        ~Measure,
        ncol=2,
        scales="free_y"
      ) +
      labs(
        title="Cross-fitted normative deviation across age groups",
        subtitle="Median normative z with interquartile range; values near zero indicate good age calibration",
        x="Median cross-fitted normative deviation (z)",
        y=NULL,
        color="Age group"
      ) +
      theme_classic(
        base_size=10
      ) +
      theme(
        strip.text=element_text(face="bold"),
        plot.title=element_text(face="bold"),
        legend.position="bottom"
      )

    regfig(
      "S16",
      "Cross-fitted normative deviation across age groups",
      save_pair(
        p,
        "S16_AgeGroup_CrossFitted_Deviation",
        SUPP_DIR,
        11,
        9
      )
    )
  }

} else {
  regfig(
    "S16",
    "Cross-fitted normative deviation across age groups",
    status="SKIPPED",
    note="AgeGroupDeviation master sheet unavailable."
  )
}



cognition_publication_theme <- function(base_size=9.2) {
  publication_curve_theme(base_size=base_size) +
    theme(
      strip.background=element_rect(
        fill="white",
        color="#333333",
        linewidth=.65
      ),
      strip.text=element_text(
        face="bold",
        color="#222222",
        size=base_size*0.78,
        lineheight=1.05,
        margin=margin(t=4,r=3,b=4,l=3)
      ),
      panel.spacing.x=grid::unit(0.95,"lines"),
      panel.spacing.y=grid::unit(1.15,"lines")
    )
}

# =========================
# S17: COGNITIVE NORMATIVE TRAJECTORIES
# =========================
COG_T <- CS %>%
  filter(
    Level=="Cognition",
    is.finite(StandardizedTrajectory)
  ) %>%
  mutate(
    Outcome=factor(Outcome,levels=unique(as.character(Outcome)))
  )

COG_T_MARK <- make_cognition_markers(
  curve_df=CS,
  summary_df=S,
  y_col="StandardizedTrajectory"
)

if(nrow(COG_T)) {
  p <- add_bands(ggplot(),legend=FALSE,labels=FALSE) +
    geom_hline(
      yintercept=0,
      linetype="dashed",
      linewidth=.35,
      color="#666666"
    ) +
    geom_line(
      data=COG_T,
      aes(Age,StandardizedTrajectory,group=Outcome),
      linewidth=.9,
      color=COGNITION_LINE_COLOR,
      lineend="round"
    )

  if(nrow(COG_T_MARK)) {
    p <- p +
      geom_point(
        data=COG_T_MARK,
        aes(Age,Y,shape=Milestone),
        inherit.aes=FALSE,
        size=2.6,
        stroke=.55,
        color="#333333",
        fill="white"
      ) +
      scale_shape_manual(
        values=c("Peak"=21,"Maximal age-related change"=24),
        name="Milestone"
      )
  }

  p <- p +
    facet_wrap(
      ~Outcome,
      ncol=2,
      scales="free_y",
      labeller=cognition_age_box_labeller
    ) +
    labs(
      title="Cognitive normative trajectories across adulthood",
      subtitle="Each cognitive outcome standardized to its own sample SD",
      x="Age (years)",
      y="Standardized change from reference age (SD)"
    ) +
    cognition_publication_theme(base_size=9.2)

  p <- add_age_axis(p)
  p <- add_age_headers(p, COG_T, "Outcome", "StandardizedTrajectory")

  regfig(
    "S17",
    "Cognitive standardized normative trajectories",
    save_pair(p,"S17_Cognitive_Standardized_Trajectories",SUPP_DIR,12,16)
  )
} else {
  regfig(
    "S17",
    "Cognitive standardized normative trajectories",
    status="SKIPPED",
    note="No cognitive trajectory data in MASTER_CURVES."
  )
}

# =========================
# S18: COGNITIVE AGE GRADIENTS
# =========================
COG_D <- DS %>%
  filter(
    Level=="Cognition",
    is.finite(safe_num(Derivative_per_SD))
  ) %>%
  mutate(
    Outcome=factor(Outcome,levels=unique(as.character(Outcome)))
  )

COG_D_MARK <- make_cognition_markers(
  curve_df=DS,
  summary_df=S,
  y_col="Derivative_per_SD"
)

if(nrow(COG_D)) {
  p <- add_bands(ggplot(),legend=FALSE,labels=FALSE) +
    geom_hline(
      yintercept=0,
      linetype="dashed",
      linewidth=.35,
      color="#666666"
    ) +
    geom_line(
      data=COG_D,
      aes(Age,Derivative_per_SD,group=Outcome),
      linewidth=.9,
      color=COGNITION_LINE_COLOR,
      lineend="round"
    )

  if(nrow(COG_D_MARK)) {
    mm <- COG_D_MARK %>%
      filter(Milestone=="Maximal age-related change")

    if(nrow(mm)) {
      p <- p +
        geom_point(
          data=mm,
          aes(Age,Y),
          inherit.aes=FALSE,
          shape=24,
          size=2.7,
          stroke=.55,
          color="#333333",
          fill=COGNITION_MAXCHANGE_COLOR
        )
    }
  }

  p <- p +
    facet_wrap(
      ~Outcome,
      ncol=2,
      scales="free_y",
      labeller=cognition_age_box_labeller
    ) +
    labs(
      title="Cognitive cross-sectional age gradients",
      subtitle="Derivative of the conditional central trajectory in outcome SD/year",
      x="Age (years)",
      y="Standardized age gradient (SD/year)"
    ) +
    cognition_publication_theme(base_size=9.2)

  p <- add_age_axis(p)
  p <- add_age_headers(p, COG_D, "Outcome", "Derivative_per_SD")

  regfig(
    "S18",
    "Cognitive standardized cross-sectional age gradients",
    save_pair(p,"S18_Cognitive_Standardized_Age_Gradients",SUPP_DIR,12,16)
  )
} else {
  regfig(
    "S18",
    "Cognitive standardized cross-sectional age gradients",
    status="SKIPPED",
    note="No cognitive derivative data in MASTER_CURVES."
  )
}

# =========================
# S19: COGNITIVE MILESTONE TIMING
# =========================
CSUM <- S
if("Level" %in% names(CSUM)) CSUM$Level <- norm_level(CSUM$Level)

cog_rows <- list()

if(nrow(CSUM)) {
  for(i in seq_len(nrow(CSUM))) {
    r <- CSUM[i,,drop=FALSE]
    if(!identical(as.character(r$Level),"Cognition")) next

    out <- as.character(r$Outcome)

    if(all(c("InternalPeak","PeakAge")%in%names(r)) &&
       truthy(r$InternalPeak)) {
      cog_rows[[length(cog_rows)+1L]] <- data.frame(
        Outcome=out,
        Milestone="Peak",
        Age=safe_num(r$PeakAge),
        L95=if("PeakAge_L95"%in%names(r))safe_num(r$PeakAge_L95)else NA_real_,
        U95=if("PeakAge_U95"%in%names(r))safe_num(r$PeakAge_U95)else NA_real_
      )
    }

    if(all(c("InternalTrough","TroughAge")%in%names(r)) &&
       truthy(r$InternalTrough)) {
      cog_rows[[length(cog_rows)+1L]] <- data.frame(
        Outcome=out,
        Milestone="Trough",
        Age=safe_num(r$TroughAge),
        L95=if("TroughAge_L95"%in%names(r))safe_num(r$TroughAge_L95)else NA_real_,
        U95=if("TroughAge_U95"%in%names(r))safe_num(r$TroughAge_U95)else NA_real_
      )
    }

    if(all(c("InternalMaxChange","MaxChangeAge")%in%names(r)) &&
       truthy(r$InternalMaxChange)) {
      cog_rows[[length(cog_rows)+1L]] <- data.frame(
        Outcome=out,
        Milestone="Maximal age-related change",
        Age=safe_num(r$MaxChangeAge),
        L95=if("MaxChangeAge_L95"%in%names(r))safe_num(r$MaxChangeAge_L95)else NA_real_,
        U95=if("MaxChangeAge_U95"%in%names(r))safe_num(r$MaxChangeAge_U95)else NA_real_
      )
    }
  }
}

COG_M <- if(length(cog_rows))bind_rows(cog_rows)else data.frame()

if(nrow(COG_M)) {
  COG_M$Milestone <- factor(
    COG_M$Milestone,
    levels=c("Peak","Trough","Maximal age-related change")
  )

  p <- add_bands(ggplot(),legend=FALSE,labels=FALSE)

  ci <- COG_M %>% filter(is.finite(L95),is.finite(U95))
  if(nrow(ci)) {
    p <- p +
      geom_errorbarh(
        data=ci,
        aes(y=Outcome,xmin=L95,xmax=U95,color=Milestone),
        height=0,
        linewidth=.5,
        alpha=.45,
        show.legend=FALSE
      )
  }

  p <- p +
    # Three age-period labels, centered over their corresponding bands.
    annotate(
      "label",
      x=30, y=Inf,
      label="Young adulthood (20–40)",
      vjust=-0.35,
      size=3.1,
      fontface="bold",
      fill="white",
      color="#444444",
      label.size=.25,
      label.padding=grid::unit(.12,"lines")
    ) +
    annotate(
      "label",
      x=50, y=Inf,
      label="Middle adulthood (40–60)",
      vjust=-0.35,
      size=3.1,
      fontface="bold",
      fill="white",
      color="#444444",
      label.size=.25,
      label.padding=grid::unit(.12,"lines")
    ) +
    annotate(
      "label",
      x=70, y=Inf,
      label="Late adulthood (60–80)",
      vjust=-0.35,
      size=3.1,
      fontface="bold",
      fill="white",
      color="#444444",
      label.size=.25,
      label.padding=grid::unit(.12,"lines")
    ) +
    geom_point(
      data=COG_M,
      aes(Age,Outcome,shape=Milestone,color=Milestone),
      size=3.0
    ) +
    scale_color_manual(
      values=MILESTONE_COLORS,
      name="Milestone"
    ) +
    scale_shape_manual(
      values=MILESTONE_SHAPES,
      name="Milestone"
    ) +
    labs(
      title="Timing of cognitive age-related milestones",
      x="Age (years)",
      y=NULL
    ) +
    guides(
      color=guide_legend(order=1,nrow=1),
      shape=guide_legend(order=1,nrow=1)
    ) +
    publication_curve_theme(base_size=10)

  p <- add_age_axis(p)

  regfig(
    "S19",
    "Timing of cognitive age-related milestones",
    save_pair(p,"S19_Cognitive_Milestone_Timing",SUPP_DIR,10,8)
  )
}

# =========================
# S20: CROSS-FITTED NORMATIVE-Z DISTRIBUTION
# =========================
NS_ALL <- read_sheet(BIOMARKER_FILE,"NormativeScores_All")

if(!is.null(NS_ALL) && nrow(NS_ALL)) {

  z_col <- first_existing_col(
    NS_ALL,
    c(
      "Primary_Norm_Z",
      "PrimaryNormZ",
      "Primary_Z",
      "CF_Norm_Z",
      "Norm_Z"
    )
  )

  if(!is.na(z_col)) {

    NS_ALL$Z_plot <- safe_num(NS_ALL[[z_col]])

    if("Level" %in% names(NS_ALL)) {
      NS_ALL$Level <- norm_level(NS_ALL$Level)
    } else {
      NS_ALL$Level <- "All outcomes"
    }

    ZD <- NS_ALL %>%
      filter(is.finite(Z_plot))

    if(nrow(ZD)) {
      p <- ggplot(ZD,aes(Z_plot)) +
        geom_histogram(
          aes(y=after_stat(density)),
          bins=35,
          fill="#D7E0E5",
          color="white",
          linewidth=.25
        ) +
        stat_function(
          fun=dnorm,
          args=list(mean=0,sd=1),
          linewidth=.85,
          color="#5C7485"
        ) +
        geom_vline(
          xintercept=c(-2,2),
          linetype="dashed",
          linewidth=.45,
          color="#9A6666"
        ) +
        facet_wrap(~Level,scales="free_y") +
        labs(
          title="Distribution of participant-level normative deviations",
          subtitle=paste0(
            "Pooled descriptive diagnostic using ",z_col,
            "; reference curve = N(0,1)"
          ),
          x="Normative deviation (z)",
          y="Density"
        ) +
        theme_classic(base_size=10) +
        theme(
          plot.title=element_text(face="bold"),
          plot.subtitle=element_text(color="#555555")
        )

      regfig(
        "S20",
        "Distribution of participant-level normative deviations",
        save_pair(p,"S20_Normative_Z_Distribution",SUPP_DIR,11,8)
      )
    }
  }
}


# =========================
# FINAL INDEX / LOG
# =========================
IDX <- if(length(idx))bind_rows(idx)else data.frame()
write.csv(IDX,file.path(OUT_ROOT,"FIGURE_INDEX.csv"),row.names=FALSE)
writeLines(c("BrainNormR Publication Figure Suite",
             "===================================",
             paste0("Generated: ",format(Sys.time())),"",logs),
           file.path(OUT_ROOT,"PUBLICATION_FIGURE_LOG.txt"))
cat("\nBrainNormR publication figure suite completed.\n")
cat("Output: ",OUT_ROOT,"\n",sep="")
cat("Check FIGURE_INDEX.csv for created/skipped figures.\n")
