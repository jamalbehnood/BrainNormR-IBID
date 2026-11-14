###############################################################################
# 13_run_age_period_analysis.R — UPDATED: structural/cognition split + cognition heatmap
# BrainNormR — model-derived age-period change + confirmatory ANCOVA contrasts
###############################################################################

suppressPackageStartupMessages({
  pkgs <- c("readxl","writexl","dplyr","tidyr","ggplot2","emmeans","sandwich")
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]
  if (length(miss)) stop("Install required package(s): ", paste(miss, collapse=", "))
  library(readxl); library(writexl); library(dplyr); library(tidyr)
  library(ggplot2); library(emmeans); library(sandwich)
})

# -----------------------------------------------------------------------------
# 1) SETTINGS
# -----------------------------------------------------------------------------
MASTER_CURVES_CANDIDATES <- c(
  file.path("Results","Thesis_Outputs","MASTER_CURVES.xlsx"),
  file.path("BrainNormR_Batch_Results","Thesis_Outputs","MASTER_CURVES.xlsx")
)
GROUPED_DATA_FILE  <- file.path("DerivedData","BrainNormR_grouped_structural_dataset.xlsx")
DK68_CONFIG_FILE   <- file.path("Config","DK68_structural_config.csv")
GROUPED_CONFIG_FILE<- file.path("Config","Grouped_structural_config.csv")
OUT_DIR <- file.path("Results","AgePeriod_Analysis")
dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)
OUT_XLSX <- file.path(OUT_DIR,"AGE_PERIOD_ANALYSIS_RESULTS.xlsx")

AGE_COL <- "Age"; SEX_COL <- "sex"; ETIV_COL <- "eTIV"
FEMALE_CODE <- 1; MALE_CODE <- 0
MIN_PER_AGE_GROUP <- 15L
USE_HC3 <- TRUE
MAKE_MODEL_DERIVED_HEATMAPS <- TRUE
PLOT_ROI_HEATMAP <- FALSE
FIG_DPI <- 600
CODE_RELEASE <- "A4_HEATMAP_FIX_V6_2026-09-06"
message("Running code release: ", CODE_RELEASE)

# Multi-panel publication figures are exported on a portrait A4 canvas.  The
# lower part of the page is deliberately left blank for approximately five
# lines of thesis/article title and a short caption.  Single-panel figures keep
# their original dimensions and are not passed through this helper.
A4_WIDTH_IN <- 8.27
A4_HEIGHT_IN <- 11.69
A4_CAPTION_SPACE_IN <- 1.35
HEAT_LOW <- "#08306B"
HEAT_MID <- "#E1E5EB"
HEAT_HIGH <- "#99000D"
HEAT_TILE_BORDER <- "#6F7782"

AGE_PERIODS <- data.frame(
  PeriodLabel=c("19–29","30–39","40–49","50–59","60–77"),
  StartAge=c(19,30,40,50,60),
  EndAge=c(30,40,50,60,77),
  stringsAsFactors=FALSE
)
AGE_GROUP_LEVELS <- AGE_PERIODS$PeriodLabel

# -----------------------------------------------------------------------------
# 2) HELPERS
# -----------------------------------------------------------------------------
find_preferred_file <- function(candidates){
  x <- candidates[file.exists(candidates)]
  if (!length(x)) stop("MASTER_CURVES not found. Checked:\n",paste(candidates,collapse="\n"))
  if (length(x)>1L){
    x <- x[order(file.info(x)$mtime,decreasing=TRUE)]
    warning("More than one MASTER_CURVES found; using newest: ",x[1])
  }
  x[1]
}
safe_num <- function(x) suppressWarnings(as.numeric(x))

save_a4_multipanel <- function(filename, plot, dpi = FIG_DPI,
                               caption_space_in = A4_CAPTION_SPACE_IN){
  if(!is.finite(caption_space_in) || caption_space_in <= 0 ||
     caption_space_in >= A4_HEIGHT_IN){
    stop("caption_space_in must be between 0 and the A4 page height.")
  }

  plot_fraction <- (A4_HEIGHT_IN - caption_space_in) / A4_HEIGHT_IN
  page_grob <- grid::grobTree(
    grid::rectGrob(gp = grid::gpar(fill = "white", col = NA)),
    grid::grobTree(
      ggplot2::ggplotGrob(plot),
      vp = grid::viewport(
        x = 0.5, y = 1,
        width = 1, height = plot_fraction,
        just = c("center", "top")
      )
    )
  )

  is_raster <- grepl("\\.(png|jpe?g|tiff?)$", filename, ignore.case = TRUE)
  ggplot2::ggsave(
    filename = filename,
    plot = page_grob,
    width = A4_WIDTH_IN,
    height = A4_HEIGHT_IN,
    units = "in",
    dpi = if(is_raster) dpi else 300,
    bg = "white",
    limitsize = FALSE
  )
  invisible(filename)
}
normalise_measure <- function(x){
  z <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    z=="volume" ~ "Volume", z=="area" ~ "Area",
    z=="thickness" ~ "Thickness", z=="curvature" ~ "Curvature",
    z %in% c("fd","fractal","fractal dimension") ~ "FD",
    TRUE ~ as.character(x)
  )
}
same_or_na <- function(x,value){
  if (is.na(value) || !nzchar(as.character(value))) is.na(x) | !nzchar(as.character(x))
  else as.character(x)==as.character(value)
}
interp_safe <- function(age,y,xout){
  age<-safe_num(age); y<-safe_num(y); ok<-is.finite(age)&is.finite(y)
  age<-age[ok]; y<-y[ok]
  if(length(age)<2L) return(NA_real_)
  ord<-order(age); age<-age[ord]; y<-y[ord]
  if(xout<min(age)||xout>max(age)) return(NA_real_)
  approx(age,y,xout=xout,rule=1,ties="ordered")$y
}
trapz_safe <- function(x,y){
  x<-safe_num(x); y<-safe_num(y); ok<-is.finite(x)&is.finite(y)
  x<-x[ok]; y<-y[ok]
  if(length(x)<2L) return(NA_real_)
  ord<-order(x); x<-x[ord]; y<-y[ord]
  sum(diff(x)*(head(y,-1)+tail(y,-1))/2)
}
curve_segment <- function(df,start_age,end_age,value_cols){
  age<-safe_num(df$Age)
  if(!any(is.finite(age))) return(data.frame())
  if(start_age<min(age,na.rm=TRUE)||end_age>max(age,na.rm=TRUE)) return(data.frame())
  inside<-df[is.finite(age)&age>=start_age&age<=end_age,,drop=FALSE]
  out<-data.frame(Age=sort(unique(c(start_age,safe_num(inside$Age),end_age))))
  for(nm in value_cols){
    out[[nm]]<-if(nm %in% names(df)) vapply(out$Age,function(a)interp_safe(df$Age,df[[nm]],a),numeric(1)) else NA_real_
  }
  out
}
make_group_key <- function(df,cols){
  if(!length(cols)) return(rep("ALL",nrow(df)))
  p<-lapply(cols,function(nm){x<-as.character(df[[nm]]);x[is.na(x)|!nzchar(x)]<-"<NA>";x})
  do.call(paste,c(p,sep="||"))
}
make_age_group <- function(age){
  cut(safe_num(age),breaks=c(19,30,40,50,60,77+1e-8),labels=AGE_GROUP_LEVELS,
      right=FALSE,include.lowest=TRUE,ordered_result=FALSE)
}
pretty_region <- function(level,region,hemisphere){
  if(identical(as.character(level),"Lobe") && !is.na(hemisphere) && nzchar(as.character(hemisphere)) && hemisphere!="Bilateral")
    return(paste0(region," (",hemisphere,")"))
  as.character(region)
}

# -----------------------------------------------------------------------------
# 3) PRIMARY: POST-PROCESS FINAL BRAINNORMR CURVES — NO REFIT
# -----------------------------------------------------------------------------
MASTER_CURVES_FILE <- find_preferred_file(MASTER_CURVES_CANDIDATES)
sh <- readxl::excel_sheets(MASTER_CURVES_FILE)
if(!all(c("CentileCurves","DerivativeCurve") %in% sh))
  stop("MASTER_CURVES must contain CentileCurves and DerivativeCurve.")

centile<-as.data.frame(readxl::read_excel(MASTER_CURVES_FILE,sheet="CentileCurves"),check.names=FALSE)
deriv  <-as.data.frame(readxl::read_excel(MASTER_CURVES_FILE,sheet="DerivativeCurve"),check.names=FALSE)
p50_candidates<-names(centile)[grepl("^P50($|_)",names(centile),ignore.case=TRUE)]
if(!length(p50_candidates)) stop("P50 column not found in CentileCurves.")
P50_COL<-p50_candidates[1]

req_c<-c("Outcome","Level","Region","Age",P50_COL)
req_d<-c("Outcome","Level","Region","Age","Derivative")
if(length(setdiff(req_c,names(centile)))) stop("CentileCurves missing: ",paste(setdiff(req_c,names(centile)),collapse=", "))
if(length(setdiff(req_d,names(deriv)))) stop("DerivativeCurve missing: ",paste(setdiff(req_d,names(deriv)),collapse=", "))

meta_cols<-intersect(c("Outcome","OutcomeType","Level","Region","Hemisphere","Lobe"),names(centile))
centile$Age<-safe_num(centile$Age); centile[[P50_COL]]<-safe_num(centile[[P50_COL]])
deriv$Age<-safe_num(deriv$Age); deriv$Derivative<-safe_num(deriv$Derivative)
for(nm in intersect(c("Derivative_L95","Derivative_U95","Derivative_per_SD"),names(deriv))) deriv[[nm]]<-safe_num(deriv[[nm]])

ckey<-make_group_key(centile,meta_cols); keys<-unique(ckey); md_rows<-list(); rr<-0L
for(k in keys){
  ii<-which(ckey==k); csub<-centile[ii,,drop=FALSE]; md<-csub[1,meta_cols,drop=FALSE]
  dkeep<-rep(TRUE,nrow(deriv))
  for(nm in intersect(meta_cols,names(deriv))) dkeep<-dkeep & same_or_na(deriv[[nm]],md[[nm]][1])
  dsub<-deriv[dkeep,,drop=FALSE]
  cmin<-min(csub$Age,na.rm=TRUE); cmax<-max(csub$Age,na.rm=TRUE)
  for(j in seq_len(nrow(AGE_PERIODS))){
    p<-AGE_PERIODS[j,]; rr<-rr+1L
    supported<-is.finite(cmin)&&is.finite(cmax)&&p$StartAge>=cmin&&p$EndAge<=cmax&&nrow(dsub)>1L
    base<-cbind(md,PeriodLabel=p$PeriodLabel,StartAge=p$StartAge,EndAge=p$EndAge,IntervalYears=p$EndAge-p$StartAge)
    if(!supported){
      md_rows[[rr]]<-cbind(base,CurveAgeMin=cmin,CurveAgeMax=cmax,Predicted_P50_Start=NA_real_,Predicted_P50_End=NA_real_,
                           P50_Change=NA_real_,MeanAnnual_P50_Change=NA_real_,MeanDerivative=NA_real_,MeanDerivative_per_SD=NA_real_,
                           PeriodChange_per_SD=NA_real_,MaxAbsDerivativeAge=NA_real_,DerivativeAtMaxAbs=NA_real_,
                           DerivativeAtMaxAbs_L95=NA_real_,DerivativeAtMaxAbs_U95=NA_real_,Direction=NA_character_,
                           SupportStatus="OUTSIDE_CURVE_SUPPORT")
      next
    }
    p0<-interp_safe(csub$Age,csub[[P50_COL]],p$StartAge); p1<-interp_safe(csub$Age,csub[[P50_COL]],p$EndAge)
    vcols<-intersect(c("Derivative","Derivative_L95","Derivative_U95","Derivative_per_SD"),names(dsub))
    seg<-curve_segment(dsub,p$StartAge,p$EndAge,vcols)
    mean_d<-trapz_safe(seg$Age,seg$Derivative)/(p$EndAge-p$StartAge)
    pc_sd<-if("Derivative_per_SD" %in% names(seg)) trapz_safe(seg$Age,seg$Derivative_per_SD) else NA_real_
    mean_d_sd<-if(is.finite(pc_sd)) pc_sd/(p$EndAge-p$StartAge) else NA_real_
    imax<-if(any(is.finite(seg$Derivative))) which.max(abs(seg$Derivative)) else NA_integer_
    maxage<-if(is.finite(imax)) seg$Age[imax] else NA_real_; maxd<-if(is.finite(imax)) seg$Derivative[imax] else NA_real_
    maxlo<-if(is.finite(imax)&&"Derivative_L95"%in%names(seg)) seg$Derivative_L95[imax] else NA_real_
    maxhi<-if(is.finite(imax)&&"Derivative_U95"%in%names(seg)) seg$Derivative_U95[imax] else NA_real_
    delta<-p1-p0
    direction<-if(!is.finite(delta)) NA_character_ else if(abs(delta)<1e-12) "Minimal/Flat" else if(delta>0) "Increasing" else "Decreasing"
    md_rows[[rr]]<-cbind(base,CurveAgeMin=cmin,CurveAgeMax=cmax,Predicted_P50_Start=p0,Predicted_P50_End=p1,
                         P50_Change=delta,MeanAnnual_P50_Change=delta/(p$EndAge-p$StartAge),MeanDerivative=mean_d,
                         MeanDerivative_per_SD=mean_d_sd,PeriodChange_per_SD=pc_sd,MaxAbsDerivativeAge=maxage,
                         DerivativeAtMaxAbs=maxd,DerivativeAtMaxAbs_L95=maxlo,DerivativeAtMaxAbs_U95=maxhi,
                         Direction=direction,SupportStatus="SUPPORTED")
  }
}
model_period<-bind_rows(md_rows) %>%
  mutate(Measure=normalise_measure(if("OutcomeType"%in%names(.)) OutcomeType else Outcome),
         AbsMeanDerivative_per_SD=abs(MeanDerivative_per_SD)) %>%
  group_by(across(all_of(meta_cols))) %>%
  mutate(PeriodRank_by_AbsMeanRate=ifelse(SupportStatus=="SUPPORTED"&is.finite(AbsMeanDerivative_per_SD),
                                          rank(-AbsMeanDerivative_per_SD,ties.method="min",na.last="keep"),NA_real_)) %>% ungroup()
fastest_period<-model_period %>% filter(SupportStatus=="SUPPORTED",is.finite(AbsMeanDerivative_per_SD)) %>%
  group_by(across(all_of(meta_cols))) %>% slice_max(AbsMeanDerivative_per_SD,n=1,with_ties=FALSE) %>% ungroup()

# Split model-derived outputs for paper-specific use while retaining the original
# combined tables for backward compatibility.  Cognition is identified ONLY by
# Level == "Cognition"; all other levels are treated as structural here.
is_cognition_level <- function(x) {
  tolower(trimws(as.character(x))) == "cognition"
}

model_period_cognition <- model_period %>%
  filter(is_cognition_level(Level))
model_period_structural <- model_period %>%
  filter(!is_cognition_level(Level))

fastest_period_cognition <- fastest_period %>%
  filter(is_cognition_level(Level))
fastest_period_structural <- fastest_period %>%
  filter(!is_cognition_level(Level))

# -----------------------------------------------------------------------------
# 4) CONFIRMATORY: ONE ANCOVA PER STRUCTURAL OUTCOME + ADJACENT EMM CONTRASTS
# -----------------------------------------------------------------------------
for(f in c(GROUPED_DATA_FILE,DK68_CONFIG_FILE,GROUPED_CONFIG_FILE)) if(!file.exists(f)) stop("Required file not found: ",f)
df<-as.data.frame(readxl::read_excel(GROUPED_DATA_FILE,sheet=1),check.names=FALSE)
if(length(setdiff(c(AGE_COL,SEX_COL),names(df)))) stop("Grouped dataset missing Age and/or sex columns.")
df[[AGE_COL]]<-safe_num(df[[AGE_COL]])
df$SexFactor<-case_when(df[[SEX_COL]]==FEMALE_CODE~"Female",df[[SEX_COL]]==MALE_CODE~"Male",TRUE~NA_character_)
df$SexFactor<-factor(df$SexFactor,levels=c("Female","Male")); df$AgeGroup<-factor(make_age_group(df[[AGE_COL]]),levels=AGE_GROUP_LEVELS)

dk<-read.csv(DK68_CONFIG_FILE,stringsAsFactors=FALSE,check.names=FALSE)
if(length(setdiff(c("ROI","Measure","Column"),names(dk)))) stop("DK68 config missing ROI/Measure/Column.")
dk_map<-data.frame(Level="ROI",Region=dk$ROI,
                   Hemisphere=ifelse(grepl("^lh_",dk$ROI),"Left",ifelse(grepl("^rh_",dk$ROI),"Right",NA)),
                   Lobe=if("Lobe"%in%names(dk)) dk$Lobe else NA_character_,Measure=normalise_measure(dk$Measure),Column=dk$Column,
                   stringsAsFactors=FALSE)

gc<-read.csv(GROUPED_CONFIG_FILE,stringsAsFactors=FALSE,check.names=FALSE)
if(length(setdiff(c("Level","Group","Hemisphere","Measure","Column"),names(gc)))) stop("Grouped config missing required columns.")
gc_map<-data.frame(Level=gc$Level,Region=gc$Group,Hemisphere=gc$Hemisphere,
                   Lobe=ifelse(tolower(gc$Level)=="lobe",gc$Group,NA_character_),
                   Measure=normalise_measure(gc$Measure),Column=gc$Column,stringsAsFactors=FALSE)

outcome_map<-bind_rows(dk_map,gc_map) %>% filter(Measure%in%c("Volume","Area","Thickness","Curvature","FD")) %>%
  distinct(Level,Region,Hemisphere,Lobe,Measure,Column,.keep_all=TRUE) %>% mutate(ColumnExists=Column%in%names(df))
missing_outcomes<-outcome_map %>% filter(!ColumnExists); outcome_map_run<-outcome_map %>% filter(ColumnExists)

adjacent_contrast_list<-list(
  "30–39 minus 19–29"=c(-1,1,0,0,0),
  "40–49 minus 30–39"=c(0,-1,1,0,0),
  "50–59 minus 40–49"=c(0,0,-1,1,0),
  "60–77 minus 50–59"=c(0,0,0,-1,1)
)

run_one_ancova<-function(m){
  dat<-data.frame(OutcomeValue=safe_num(df[[m$Column]]),AgeGroup=df$AgeGroup,SexFactor=df$SexFactor)
  use_etiv<-m$Measure%in%c("Volume","Area")
  if(use_etiv){if(!ETIV_COL%in%names(df)) return(list(status="SKIP_MISSING_eTIV")); dat$eTIV<-safe_num(df[[ETIV_COL]])}
  needed<-c("OutcomeValue","AgeGroup","SexFactor",if(use_etiv)"eTIV")
  dat<-droplevels(dat[complete.cases(dat[,needed,drop=FALSE]),,drop=FALSE])
  counts<-setNames(rep(0L,length(AGE_GROUP_LEVELS)),AGE_GROUP_LEVELS); tb<-table(dat$AgeGroup);counts[names(tb)]<-as.integer(tb)
  if(length(unique(dat$AgeGroup))!=5L||any(counts<MIN_PER_AGE_GROUP))
    return(list(status="SKIP_INSUFFICIENT_AGE_GROUP_N",qc=data.frame(N=nrow(dat),MinAgeGroupN=min(counts),Counts=paste(names(counts),counts,sep="=",collapse="; "))))
  form<-if(use_etiv) OutcomeValue~AgeGroup+SexFactor+eTIV else OutcomeValue~AgeGroup+SexFactor
  mod<-tryCatch(lm(form,data=dat),error=function(e)NULL); if(is.null(mod)) return(list(status="SKIP_MODEL_ERROR"))
  V<-if(USE_HC3) tryCatch(sandwich::vcovHC(mod,type="HC3"),error=function(e)vcov(mod)) else vcov(mod)
  emm<-tryCatch(emmeans::emmeans(mod,~AgeGroup,weights="equal",vcov.=V),error=function(e)NULL)
  if(is.null(emm)) return(list(status="SKIP_EMMEANS_ERROR"))
  emm_df<-as.data.frame(summary(emm,infer=c(TRUE,FALSE),level=.95))
  ctr<-tryCatch(emmeans::contrast(emm,method=adjacent_contrast_list,adjust="none"),error=function(e)NULL)
  ctr_df<-if(!is.null(ctr)) as.data.frame(summary(ctr,infer=c(TRUE,TRUE),level=.95,adjust="none")) else data.frame()
  jt<-tryCatch(as.data.frame(emmeans::joint_tests(mod,vcov.=V)),error=function(e)data.frame())
  omni<-data.frame(); if(nrow(jt)){tc<-intersect(c("model term","model.term","term"),names(jt));if(length(tc)){hit<-which(as.character(jt[[tc[1]]])=="AgeGroup");if(length(hit))omni<-jt[hit[1],,drop=FALSE]}}
  rsd<-sigma(mod); if(nrow(ctr_df)&&"estimate"%in%names(ctr_df)) ctr_df$Std_Adjusted_Difference<-ctr_df$estimate/rsd
  qc<-data.frame(N=nrow(dat),MinAgeGroupN=min(counts),Counts=paste(names(counts),counts,sep="=",collapse="; "),
                 Residual_SD=rsd,Adjusted_R2=summary(mod)$adj.r.squared,
                 Max_CooksD=suppressWarnings(max(cooks.distance(mod),na.rm=TRUE)),
                 SE_Type=if(USE_HC3)"HC3 robust" else "Model-based",Covariates=if(use_etiv)"Sex + eTIV" else "Sex",Status="OK")
  list(status="OK",omnibus=omni,emmeans=emm_df,contrasts=ctr_df,qc=qc)
}

ol<-list(); el<-list(); cl<-list(); ql<-list()
for(i in seq_len(nrow(outcome_map_run))){
  m<-outcome_map_run[i,,drop=FALSE]; r<-run_one_ancova(m); md<-m[,c("Level","Region","Hemisphere","Lobe","Measure","Column"),drop=FALSE]
  if(!is.null(r$omnibus)&&nrow(r$omnibus)) ol[[length(ol)+1L]]<-cbind(md,r$omnibus)
  if(!is.null(r$emmeans)&&nrow(r$emmeans)) el[[length(el)+1L]]<-cbind(md,r$emmeans)
  if(!is.null(r$contrasts)&&nrow(r$contrasts)) cl[[length(cl)+1L]]<-cbind(md,r$contrasts)
  if(!is.null(r$qc)&&nrow(r$qc)) ql[[length(ql)+1L]]<-cbind(md,r$qc) else ql[[length(ql)+1L]]<-cbind(md,data.frame(Status=r$status))
}
ancova_omnibus<-bind_rows(ol); ancova_emmeans<-bind_rows(el); ancova_contrasts<-bind_rows(cl); ancova_qc<-bind_rows(ql)

# -----------------------------------------------------------------------------
# 5) FDR
# -----------------------------------------------------------------------------
if(nrow(ancova_omnibus)){
  pc<-intersect(c("p.value","p.value."),names(ancova_omnibus)); if(length(pc)) names(ancova_omnibus)[names(ancova_omnibus)==pc[1]]<-"p_raw"
  if("p_raw"%in%names(ancova_omnibus)) ancova_omnibus<-ancova_omnibus %>% mutate(p_raw=safe_num(p_raw)) %>%
    group_by(Level,Measure) %>% mutate(q_BH_LevelMeasure=p.adjust(p_raw,"BH")) %>% ungroup() %>%
    mutate(q_BH_Global=p.adjust(p_raw,"BH"),Significant_FDR=is.finite(q_BH_LevelMeasure)&q_BH_LevelMeasure<.05)
}
if(nrow(ancova_contrasts)){
  if("p.value"%in%names(ancova_contrasts)) names(ancova_contrasts)[names(ancova_contrasts)=="p.value"]<-"p_raw"
  ancova_contrasts<-ancova_contrasts %>% mutate(p_raw=safe_num(p_raw)) %>%
    group_by(Level,Measure,contrast) %>% mutate(q_BH_LevelMeasureContrast=p.adjust(p_raw,"BH")) %>% ungroup() %>%
    mutate(q_BH_Global=p.adjust(p_raw,"BH"),Significant_FDR=is.finite(q_BH_LevelMeasureContrast)&q_BH_LevelMeasureContrast<.05,
           Direction=case_when(!is.finite(estimate)~NA_character_,estimate>0~"Higher in older group",estimate<0~"Lower in older group",TRUE~"No difference"))
}
ancova_significant<-if(nrow(ancova_contrasts)) ancova_contrasts %>% filter(Significant_FDR) else data.frame()

# -----------------------------------------------------------------------------
# 6) OPTIONAL MODEL-DERIVED HEATMAPS
# -----------------------------------------------------------------------------
fig_idx<-list()
if(MAKE_MODEL_DERIVED_HEATMAPS&&nrow(model_period)){
  levs<-unique(as.character(model_period$Level));levs<-levs[!is.na(levs)&nzchar(levs)]
  # Cognition gets its own dedicated heatmap below; exclude it from the generic
  # structural-level loop to avoid a duplicate/less readable figure.
  levs<-levs[!is_cognition_level(levs)]
  if(!PLOT_ROI_HEATMAP) levs<-setdiff(levs,"ROI")
  for(lev in levs){
    pd<-model_period %>% filter(Level==lev,SupportStatus=="SUPPORTED",is.finite(MeanDerivative_per_SD))
    if(!nrow(pd)) next
    heat_lim<-max(abs(pd$MeanDerivative_per_SD),na.rm=TRUE)
    if(!is.finite(heat_lim)||heat_lim<=0) heat_lim<-1
    hemi_vec<-if("Hemisphere"%in%names(pd)) pd$Hemisphere else rep(NA_character_,nrow(pd))
    pd$RegionDisplay<-mapply(pretty_region,pd$Level,pd$Region,hemi_vec,USE.NAMES=FALSE)
    pd<-pd %>% mutate(
      PeriodLabel=factor(PeriodLabel,levels=AGE_GROUP_LEVELS),
      HeatmapLabel=sprintf("%.3f",MeanDerivative_per_SD),
      LabelColor=ifelse(abs(MeanDerivative_per_SD)>=0.55*heat_lim,"white","#202020")
    )
    pd$RegionDisplay<-factor(pd$RegionDisplay,levels=rev(sort(unique(pd$RegionDisplay))))
    p<-ggplot(pd,aes(PeriodLabel,RegionDisplay,fill=MeanDerivative_per_SD))+
      geom_tile(color=HEAT_TILE_BORDER,linewidth=.32)
    # Numerical labels are useful for the compact aggregate heatmaps, but are
    # deliberately omitted from a potentially dense ROI heatmap.
    if(!lev%in%c("ROI","Lobe")){
      p<-p+geom_text(aes(label=HeatmapLabel,color=LabelColor),size=2.55,
                     fontface="plain",family="sans")+
        scale_color_identity()
    }
    plot_title<-if(lev=="Lobe") {
      "Model-derived age-period change: whole-lobe and hemisphere-specific results"
    } else {
      paste0("Model-derived age-period change: ",lev)
    }
    p<-p+
      scale_fill_gradient2(low=HEAT_LOW,mid=HEAT_MID,high=HEAT_HIGH,midpoint=0,
                           limits=c(-heat_lim,heat_lim),name="Mean annual\nchange (SD/year)")+
      facet_wrap(~Measure,scales="free_y",ncol=2)+
      labs(title=plot_title,subtitle="Final BrainNormR P50 trajectory; descriptive post-processing",x="Age period",y=NULL)+
      theme_minimal(base_size=10)+theme(
        panel.grid=element_blank(),
        axis.text.x=element_text(angle=0,hjust=.5,vjust=1,margin=margin(t=6),size=8),
        axis.title.x=element_text(margin=margin(t=12)),
        strip.text=element_text(face="bold"),
        legend.position="bottom",
        plot.margin=margin(8,10,18,10)
      )
    safelev<-gsub("[^A-Za-z0-9_]+","_",lev);png<-file.path(OUT_DIR,paste0("ModelDerived_AgePeriod_Heatmap_",safelev,"_A4_FIXED_V6.png"));pdf<-sub("\\.png$",".pdf",png)
    save_a4_multipanel(png,p)
    save_a4_multipanel(pdf,p)
    fig_idx[[length(fig_idx)+1L]]<-data.frame(
      Level=lev,Figure=basename(png),
      Description=if(lev=="Lobe")
        "Whole-lobe plus left/right results; fill = mean derivative per SD; no cell labels"
      else "Mean derivative per SD within age periods; cell values printed",
      stringsAsFactors=FALSE
    )

    # Additional paper-ready lobar summary: retain bilateral/combined lobe rows
    # only and print the exact standardized gradients inside the cells.
    if(lev=="Lobe"){
      pd_bilateral<-pd %>%
        filter(!grepl("\\((Left|Right)\\)|(^|[[:space:]])(Left|Right)([[:space:]]|$)",
                      as.character(RegionDisplay),ignore.case=TRUE))

      if(nrow(pd_bilateral)){
        p_lobe_bilateral<-ggplot(
          pd_bilateral,
          aes(PeriodLabel,RegionDisplay,fill=MeanDerivative_per_SD)
        )+
          geom_tile(color=HEAT_TILE_BORDER,linewidth=.36)+
          geom_text(
            aes(label=HeatmapLabel,color=LabelColor),
            size=2.55,fontface="plain",family="sans"
          )+
          scale_color_identity()+
          scale_fill_gradient2(
            low=HEAT_LOW,mid=HEAT_MID,high=HEAT_HIGH,midpoint=0,
            limits=c(-heat_lim,heat_lim),
            name="Mean annual\nchange (SD/year)"
          )+
          facet_wrap(~Measure,scales="free_y",ncol=2)+
          labs(
            title="Bilateral lobar age-period gradients across morphometric measures",
            subtitle="Combined whole-lobe models; cell value = mean standardized age gradient (SD/year)",
            x="Age period",y=NULL
          )+
          theme_minimal(base_size=10)+
          theme(
            panel.grid=element_blank(),
            axis.text.x=element_text(angle=0,hjust=.5,vjust=1,margin=margin(t=6),size=8),
            axis.title.x=element_text(margin=margin(t=12)),
            strip.text=element_text(face="bold"),
            plot.title=element_text(face="bold"),
            legend.position="bottom",
            plot.margin=margin(8,10,18,10)
          )

        lobe_num_png<-file.path(
          OUT_DIR,"ModelDerived_AgePeriod_Heatmap_Lobe_Bilateral_Numeric_A4_FIXED_V6.png"
        )
        lobe_num_pdf<-sub("\\.png$",".pdf",lobe_num_png)
        save_a4_multipanel(lobe_num_png,p_lobe_bilateral)
        save_a4_multipanel(lobe_num_pdf,p_lobe_bilateral)
        fig_idx[[length(fig_idx)+1L]]<-data.frame(
          Level="Lobe_Bilateral",
          Figure=basename(lobe_num_png),
          Description="Bilateral/combined lobes only; cell value = mean derivative per SD",
          stringsAsFactors=FALSE
        )
      }
    }
  }

  # Dedicated cognition heatmap: rows = cognitive outcomes, columns = age periods,
  # fill = mean standardized age gradient (SD/year).  This is intended for Paper 2.
  if(nrow(model_period_cognition)){
    cog_pd<-model_period_cognition %>%
      filter(SupportStatus=="SUPPORTED",is.finite(MeanDerivative_per_SD)) %>%
      mutate(
        CognitiveOutcome=ifelse(
          !is.na(Region) & nzchar(trimws(as.character(Region))),
          as.character(Region),
          as.character(Outcome)
        ),
        PeriodLabel=factor(PeriodLabel,levels=AGE_GROUP_LEVELS)
      )

    if(nrow(cog_pd)){
      cog_lim<-max(abs(cog_pd$MeanDerivative_per_SD),na.rm=TRUE)
      if(!is.finite(cog_lim)||cog_lim<=0) cog_lim<-1
      cog_order<-cog_pd %>%
        group_by(CognitiveOutcome) %>%
        summarise(PeakRate=max(abs(MeanDerivative_per_SD),na.rm=TRUE),.groups="drop") %>%
        arrange(PeakRate) %>%
        pull(CognitiveOutcome)
      cog_pd<-cog_pd %>% mutate(
        CognitiveOutcome=factor(CognitiveOutcome,levels=cog_order),
        HeatmapLabel=sprintf("%.3f",MeanDerivative_per_SD),
        LabelColor=ifelse(abs(MeanDerivative_per_SD)>=0.55*cog_lim,"white","#202020")
      )

      p_cog<-ggplot(cog_pd,aes(PeriodLabel,CognitiveOutcome,fill=MeanDerivative_per_SD))+
        geom_tile(color=HEAT_TILE_BORDER,linewidth=.36)+
        geom_text(aes(label=HeatmapLabel,color=LabelColor),size=2.7,fontface="bold")+
        scale_color_identity()+
        scale_fill_gradient2(
          low=HEAT_LOW,mid=HEAT_MID,high=HEAT_HIGH,midpoint=0,
          name="Mean age gradient\n(SD/year)"
        )+
        labs(
          title="Cognitive model-derived age-period gradients",
          subtitle="Final BrainNormR cognitive P50 trajectories; descriptive post-processing of the continuous model",
          x="Age period",y=NULL
        )+
        theme_minimal(base_size=10)+
        theme(
          panel.grid=element_blank(),
          axis.text.x=element_text(angle=35,hjust=1),
          plot.title=element_text(face="bold")
        )

      cog_png<-file.path(OUT_DIR,"ModelDerived_AgePeriod_Heatmap_Cognition.png")
      cog_pdf<-file.path(OUT_DIR,"ModelDerived_AgePeriod_Heatmap_Cognition.pdf")
      cog_h<-max(6,0.42*length(unique(cog_pd$CognitiveOutcome))+2.2)
      ggsave(cog_png,p_cog,width=10,height=cog_h,dpi=FIG_DPI,bg="white")
      ggsave(cog_pdf,p_cog,width=10,height=cog_h,bg="white")
      fig_idx[[length(fig_idx)+1L]]<-data.frame(
        Level="Cognition",
        Figure=basename(cog_png),
        Description="Cognitive outcomes × age periods; fill = mean derivative per SD (SD/year)",
        stringsAsFactors=FALSE
      )
    }
  }
}

# -----------------------------------------------------------------------------
# 6B) CONFIRMATORY ADJACENT AGE-GROUP CONTRAST HEATMAPS
# Fill and cell value = adjusted difference standardized by residual SD.
# Stars = BH-FDR significance within Level x Measure x contrast.
# These are cross-sectional age-group differences, not longitudinal changes.
# -----------------------------------------------------------------------------
make_sig_mark<-function(q){
  q<-safe_num(q)
  ifelse(!is.finite(q),"",
         ifelse(q<.001,"***",ifelse(q<.01,"**",ifelse(q<.05,"*",""))))
}

prepare_contrast_heatmap<-function(x){
  if(is.null(x)||!nrow(x)) return(data.frame())
  x<-x %>%
    mutate(
      Std_Adjusted_Difference=safe_num(Std_Adjusted_Difference),
      q_BH_LevelMeasureContrast=safe_num(q_BH_LevelMeasureContrast),
      Significance=make_sig_mark(q_BH_LevelMeasureContrast),
      CellLabel=ifelse(
        is.finite(Std_Adjusted_Difference),
        paste0(sprintf("%.2f",Std_Adjusted_Difference),Significance),
        ""
      ),
      ContrastLabel=factor(
        as.character(contrast),
        levels=names(adjacent_contrast_list),
        labels=c(
          "19–29 → 30–39","30–39 → 40–49",
          "40–49 → 50–59","50–59 → 60–77"
        )
      ),
      PlotContrastLabel=factor(
        as.character(contrast),
        levels=names(adjacent_contrast_list),
        labels=c(
          "19–29\n→ 30–39","30–39\n→ 40–49",
          "40–49\n→ 50–59","50–59\n→ 60–77"
        )
      )
    ) %>%
    filter(is.finite(Std_Adjusted_Difference),!is.na(ContrastLabel))

  if(!nrow(x)) return(x)
  lim<-max(abs(x$Std_Adjusted_Difference),na.rm=TRUE)
  if(!is.finite(lim)||lim<=0) lim<-1
  x %>% mutate(
    PlotLimit=lim,
    LabelColor=ifelse(abs(Std_Adjusted_Difference)>=.55*lim,"white","#202020")
  )
}

wb_contrast<-data.frame()
lobe_contrast<-data.frame()

if(nrow(ancova_contrasts)&&
   all(c("Std_Adjusted_Difference","q_BH_LevelMeasureContrast")%in%names(ancova_contrasts))){

  contrast_base<-ancova_contrasts %>%
    mutate(LevelKey=gsub("[^a-z]","",tolower(as.character(Level))))

  # Whole-cortex confirmatory heatmap for the main text.
  wb_contrast<-prepare_contrast_heatmap(
    contrast_base %>% filter(LevelKey%in%c("wholebrain","wholecortex","global"))
  )

  if(nrow(wb_contrast)){
    wb_dup<-wb_contrast %>% count(Measure,ContrastLabel,name="n") %>% filter(n>1)
    if(nrow(wb_dup)) stop("WholeBrain contrast heatmap has duplicate Measure × contrast cells.")
    wb_lim<-unique(wb_contrast$PlotLimit)[1]
    wb_contrast$Measure<-factor(
      wb_contrast$Measure,
      levels=rev(c("Volume","Area","Thickness","Curvature","FD"))
    )

    p_wb_contrast<-ggplot(
      wb_contrast,
      aes(PlotContrastLabel,Measure,fill=Std_Adjusted_Difference)
    )+
      geom_tile(color=HEAT_TILE_BORDER,linewidth=.45)+
      geom_text(
        aes(label=CellLabel,color=LabelColor),
        size=3.5,fontface="bold"
      )+
      scale_color_identity()+
      scale_fill_gradient2(
        low=HEAT_LOW,mid=HEAT_MID,high=HEAT_HIGH,midpoint=0,
        limits=c(-wb_lim,wb_lim),
        name="Standardized adjusted\ndifference"
      )+
      labs(
        title="Adjusted differences between adjacent age groups: whole cortex",
        subtitle="Cell value = standardized adjusted difference; * qBH<.05, ** qBH<.01, *** qBH<.001",
        x="Adjacent age-group contrast (older minus younger)",y=NULL
      )+
      theme_minimal(base_size=10.5)+
      theme(
        panel.grid=element_blank(),
        axis.text.x=element_text(angle=0,hjust=.5,linespacing=.9),
        plot.title=element_text(face="bold"),
        legend.position="right"
      )

    wb_png<-file.path(OUT_DIR,"ANCOVA_Adjacent_Contrast_Heatmap_WholeBrain.png")
    wb_pdf<-sub("\\.png$",".pdf",wb_png)
    ggsave(wb_png,p_wb_contrast,width=10,height=5.8,dpi=FIG_DPI,bg="white")
    ggsave(wb_pdf,p_wb_contrast,width=10,height=5.8,bg="white")
    write.csv(wb_contrast,file.path(OUT_DIR,"ANCOVA_Adjacent_Contrast_Heatmap_WholeBrain_Data.csv"),row.names=FALSE)
    fig_idx[[length(fig_idx)+1L]]<-data.frame(
      Level="WholeBrain_ANCOVA",
      Figure=basename(wb_png),
      Description="FDR-annotated standardized adjacent age-group contrasts",
      stringsAsFactors=FALSE
    )
  }

  # Bilateral/combined lobes only; left/right-specific rows are excluded.
  lobe_source<-contrast_base %>% filter(LevelKey=="lobe")
  if(nrow(lobe_source)){
    hemi_key<-tolower(trimws(ifelse(is.na(lobe_source$Hemisphere),"",as.character(lobe_source$Hemisphere))))
    keep_bilateral<-hemi_key==""|grepl("bilateral|both|combined|all",hemi_key)
    lobe_source<-lobe_source[keep_bilateral,,drop=FALSE]
  }
  lobe_contrast<-prepare_contrast_heatmap(lobe_source)

  if(nrow(lobe_contrast)){
    lobe_dup<-lobe_contrast %>% count(Measure,Region,ContrastLabel,name="n") %>% filter(n>1)
    if(nrow(lobe_dup)) stop("Bilateral lobe contrast heatmap has duplicate Measure × Region × contrast cells.")
    lobe_lim<-unique(lobe_contrast$PlotLimit)[1]
    lobe_contrast$Measure<-factor(
      lobe_contrast$Measure,
      levels=c("Volume","Area","Thickness","Curvature","FD")
    )
    lobe_contrast$Region<-factor(
      lobe_contrast$Region,
      levels=rev(sort(unique(as.character(lobe_contrast$Region))))
    )

    p_lobe_contrast<-ggplot(
      lobe_contrast,
      aes(PlotContrastLabel,Region,fill=Std_Adjusted_Difference)
    )+
      geom_tile(color=HEAT_TILE_BORDER,linewidth=.42)+
      geom_text(
        aes(label=CellLabel,color=LabelColor),
        size=2.65,fontface="plain",family="sans"
      )+
      scale_color_identity()+
      scale_fill_gradient2(
        low=HEAT_LOW,mid=HEAT_MID,high=HEAT_HIGH,midpoint=0,
        limits=c(-lobe_lim,lobe_lim),
        name="Standardized adjusted\ndifference"
      )+
      facet_wrap(~Measure,ncol=2,scales="free_y")+
      labs(
        title="Adjusted differences between adjacent age groups: bilateral lobes",
        subtitle="Cell value = standardized adjusted difference; * qBH<.05, ** qBH<.01, *** qBH<.001",
        x=NULL,y=NULL
      )+
      theme_minimal(base_size=9.5)+
      theme(
        panel.grid=element_blank(),
        axis.text.x=element_text(angle=0,hjust=.5,vjust=1,margin=margin(t=7),
                                 size=8,linespacing=.9),
        strip.text=element_text(face="bold"),
        plot.title=element_text(face="bold"),
        legend.position="bottom",
        plot.margin=margin(8,10,24,10)
      )

    lobe_sig_png<-file.path(OUT_DIR,"ANCOVA_Adjacent_Contrast_Heatmap_Lobe_Bilateral_A4_FIXED_V6.png")
    lobe_sig_pdf<-sub("\\.png$",".pdf",lobe_sig_png)
    save_a4_multipanel(lobe_sig_png,p_lobe_contrast)
    save_a4_multipanel(lobe_sig_pdf,p_lobe_contrast)
    write.csv(lobe_contrast,file.path(OUT_DIR,"ANCOVA_Adjacent_Contrast_Heatmap_Lobe_Bilateral_Data.csv"),row.names=FALSE)
    fig_idx[[length(fig_idx)+1L]]<-data.frame(
      Level="Lobe_Bilateral_ANCOVA",
      Figure=basename(lobe_sig_png),
      Description="Bilateral lobar FDR-annotated standardized adjacent age-group contrasts",
      stringsAsFactors=FALSE
    )
  }
}
figure_index_df<-bind_rows(fig_idx)

# -----------------------------------------------------------------------------
# 7) EXPORT
# -----------------------------------------------------------------------------
readme<-data.frame(Item=c("Primary analysis","Primary input","Primary trajectory","Primary age-period metric","Confirmatory analysis","Confirmatory input",
                          "Volume/Area model","Thickness/Curvature/FD model","Sex interaction","Robust SE","Adjusted means","FDR omnibus family","FDR contrast family","Cross-sectional caution"),
                   Description=c("Model-derived summaries from final continuous BrainNormR trajectory",normalizePath(MASTER_CURVES_FILE,winslash="/",mustWork=FALSE),
                                 "P50 median + d(P50)/dAge","P50 change, annual change, mean derivative, SD-standardized rate, within-period maximal |derivative|",
                                 "One ANCOVA per outcome + pre-specified adjacent EMM contrasts",normalizePath(GROUPED_DATA_FILE,winslash="/",mustWork=FALSE),
                                 "Outcome ~ AgeGroup + Sex + eTIV","Outcome ~ AgeGroup + Sex","Not included",if(USE_HC3)"HC3 robust" else "Model-based",
                                 "Equal weighting across Female/Male; eTIV at mean","BH across units within Level × Measure","BH across units within Level × Measure × Contrast",
                                 "Age-group contrasts are cross-sectional differences, not within-person longitudinal decline"),stringsAsFactors=FALSE)
period_def<-AGE_PERIODS %>% mutate(ANCOVA_Definition=c("[19,30)","[30,40)","[40,50)","[50,60)","[60,77]"),ModelDerived_Interval=paste0(StartAge,"→",EndAge))

sheets<-list(
  README=readme,
  AgePeriod_Definition=period_def,
  # Combined model-derived outputs retained for backward compatibility.
  ModelDerived_PeriodChange=model_period,
  ModelDerived_FastestPeriod=fastest_period,
  # Paper-ready split outputs.
  Structural_PeriodChange=model_period_structural,
  Structural_FastestPeriod=fastest_period_structural,
  Cognition_PeriodChange=model_period_cognition,
  Cognition_FastestPeriod=fastest_period_cognition,
  # ANCOVA remains structural-only by design.
  ANCOVA_Omnibus=ancova_omnibus,
  ANCOVA_AdjustedMeans=ancova_emmeans,
  ANCOVA_AdjacentContrasts=ancova_contrasts,
  ANCOVA_Significant_FDR=ancova_significant,
  ANCOVA_WholeBrain_Heatmap=wb_contrast,
  ANCOVA_BilateralLobe_Heatmap=lobe_contrast,
  ANCOVA_QC=ancova_qc,
  QC_MissingConfiguredColumns=missing_outcomes,
  Figure_Index=figure_index_df
)
sheets<-sheets[vapply(sheets,function(x)is.data.frame(x)&&ncol(x)>0,logical(1))]
writexl::write_xlsx(sheets,OUT_XLSX)
write.csv(model_period,file.path(OUT_DIR,"ModelDerived_PeriodChange.csv"),row.names=FALSE)
write.csv(fastest_period,file.path(OUT_DIR,"ModelDerived_FastestPeriod.csv"),row.names=FALSE)
write.csv(model_period_structural,file.path(OUT_DIR,"Structural_PeriodChange.csv"),row.names=FALSE)
write.csv(fastest_period_structural,file.path(OUT_DIR,"Structural_FastestPeriod.csv"),row.names=FALSE)
write.csv(model_period_cognition,file.path(OUT_DIR,"Cognition_PeriodChange.csv"),row.names=FALSE)
write.csv(fastest_period_cognition,file.path(OUT_DIR,"Cognition_FastestPeriod.csv"),row.names=FALSE)
write.csv(ancova_omnibus,file.path(OUT_DIR,"ANCOVA_Omnibus.csv"),row.names=FALSE)
write.csv(ancova_contrasts,file.path(OUT_DIR,"ANCOVA_AdjacentContrasts.csv"),row.names=FALSE)
write.csv(ancova_qc,file.path(OUT_DIR,"ANCOVA_QC.csv"),row.names=FALSE)
if(nrow(figure_index_df)) write.csv(figure_index_df,file.path(OUT_DIR,"AGE_PERIOD_FIGURE_INDEX.csv"),row.names=FALSE)
capture.output(sessionInfo(),file=file.path(OUT_DIR,"sessionInfo_AgePeriodAnalysis.txt"))

cat("\n============================================================\n")
cat("Age-period analysis completed successfully.\n")
cat("Output workbook: ",OUT_XLSX,"\n",sep="")
cat("Model-derived rows (all): ",nrow(model_period),"\n",sep="")
cat("  Structural period rows: ",nrow(model_period_structural),"\n",sep="")
cat("  Cognition period rows: ",nrow(model_period_cognition),"\n",sep="")
cat("Fastest-period rows (all): ",nrow(fastest_period),"\n",sep="")
cat("  Structural fastest rows: ",nrow(fastest_period_structural),"\n",sep="")
cat("  Cognition fastest rows: ",nrow(fastest_period_cognition),"\n",sep="")
cat("ANCOVA omnibus rows: ",nrow(ancova_omnibus),"\n",sep="")
cat("ANCOVA adjacent-contrast rows: ",nrow(ancova_contrasts),"\n",sep="")
cat("FDR-significant contrasts: ",nrow(ancova_significant),"\n",sep="")
cat("============================================================\n")
