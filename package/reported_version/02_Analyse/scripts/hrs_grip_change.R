# User-run exploratory association analysis. Only aggregate outputs are saved.
fit_change <- function(d,dist='weibull') {
  f <- survival::Surv(lo,hi,type='interval2') ~ splines::ns(age,df=3)+sex+smoke+health+
    splines::ns(grip10,df=3)+splines::ns(change_kg,df=3)
  # A constant interval is already held fixed; it has no estimable coefficient.
  if(length(unique(d$gap_years))>1L) f <- update(f,. ~ . + gap_years)
  fit <- withCallingHandlers(survival::survreg(f,data=d,dist=dist,na.action=na.fail,
    control=survival::survreg.control(maxiter=100)),
    warning=function(w) stop('Change model warning; no detailed condition exported.'))
  if(any(!is.finite(coef(fit))) || any(!is.finite(fit$var)) ||
     !is.finite(fit$scale) || fit$scale<=0) stop('Invalid change model fit.')
  fit
}

change_grid <- function(d) {
  bounds <- quantile(d$change_kg,c(.1,.9),names=FALSE)
  lower <- ceiling(bounds[1]); upper <- floor(bounds[2])
  if(!all(is.finite(bounds)) || lower>=0 || upper<=0)
    stop('Insufficient central support for loss, zero change and gain.')
  seq(lower,upper,by=1)
}

change_target <- function(d,grid) {
  stopifnot(all(is.finite(grid)),0 %in% grid)
  # Standardize every point to the SAME people. Do not change the denominator
  # along the curve or assign impossible current grip values to weak baselines.
  d[d$grip10+min(grid)>0 & d$grip10+max(grid)<=100,,drop=FALSE]
}

change_curve <- function(fit,d,grid) {
  target <- change_target(d,grid)
  if(nrow(target)<100) stop('Insufficient standardization target.')
  values <- vapply(grid,function(x) {
    z <- target; z$change_kg <- x
    mean(risk5(fit,z))
  },0)
  if(any(!is.finite(values)) || any(values<0 | values>1)) stop('Invalid curve.')
  data.frame(change_kg=grid,risk5=values,risk_difference=values-values[grid==0])
}

change_bootstrap <- function(d,grid,B=500L,fit_fun=fit_change,curve_fun=change_curve) {
  stopifnot(length(B)==1L,is.finite(B),B>=20,B==floor(B),!anyDuplicated(grid))
  original <- curve_fun(fit_fun(d),d,grid)
  draws <- array(NA_real_,c(B,length(grid),2L))
  for(b in seq_len(B)) {
    if(b==1 || b%%25==0) message('Change association household bootstrap ',b,'/',B)
    boot <- sample_households(d)
    attempt <- tryCatch(curve_fun(fit_fun(boot),boot,grid),error=function(e) NULL)
    if(!is.null(attempt)) draws[b,,] <- as.matrix(attempt[,c('risk5','risk_difference')])
  }
  good <- apply(draws,1,function(x) all(is.finite(x)))
  stable <- sum(good)>=ceiling(.9*B)
  limits <- function(k) {
    if(!stable) return(matrix(NA_real_,2,length(grid)))
    apply(matrix(draws[good,,k],ncol=length(grid)),2,quantile,probs=c(.025,.975))
  }
  risk_ci <- limits(1); rd_ci <- limits(2)
  data.frame(original,risk_ci_lower=risk_ci[1,],risk_ci_upper=risk_ci[2,],
    rd_ci_lower=rd_ci[1,],rd_ci_upper=rd_ci[2,],bootstrap_success=sum(good),
    bootstrap_requested=B,status=if(stable) 'OK' else 'UNSTABLE')
}

plot_change_curve <- function(curve,path,synthetic=FALSE,title='Adjustierte Risikokurve') {
  if(!all(curve$status=='OK')) stop('No confidence-band plot for unstable bootstrap.')
  grDevices::png(path,width=1800,height=900,res=160)
  on.exit(grDevices::dev.off())
  par(mfrow=c(1,2),mar=c(5,5,4,1),oma=c(2,0,1,0))
  x <- curve$change_kg
  panel <- function(value,lower,upper,label,title,zero=FALSE) {
    ylim <- range(c(100*lower,100*upper,if(zero) 0),finite=TRUE)
    plot(x,100*value,type='n',ylim=ylim,xlab='Handkraftveraenderung 2010-2014 (kg)',
      ylab=label,main=title)
    polygon(c(x,rev(x)),100*c(lower,rev(upper)),col='grey85',border=NA)
    if(zero) abline(h=0,lty=2,col='grey45')
    abline(v=0,lty=3,col='grey45')
    lines(x,100*value,lwd=2)
  }
  panel(curve$risk5,curve$risk_ci_lower,curve$risk_ci_upper,
    'Fuenfjahres-Sterberisiko (%)',title)
  panel(curve$risk_difference,curve$rd_ci_lower,curve$rd_ci_upper,
    'Risikodifferenz (Prozentpunkte)','Vergleich mit unveraenderter Handkraft',TRUE)
  mtext(if(synthetic) 'SYNTHETISCHE TESTDATEN, KEIN HRS-ERGEBNIS' else
    'Explorativer Zusammenhang; punktweise 95%-Intervalle; kein kausaler Effekt',
    outer=TRUE,side=1,cex=.8)
}

prepare_change_data <- function(imported) {
  core <- imported$core; t <- imported$tracker
  a <- wave_summary(core$h10i_r,'M'); b <- wave_summary(core$h14i_r,'O')
  keys <- intersect(a$key[a$valid_trials>0],b$key[b$valid_trials>0])
  counts <- c(valid_grip_pairs=length(keys))
  keys <- keys[keys %in% t$key]; t <- t[match(keys,t$key),]
  counts['tracker_matched'] <- length(keys)
  good <- !is.na(t$OAGE)&t$OAGE>=50&t$OAGE<=120&t$OAGE==floor(t$OAGE)&t$OVHHID=='000000'
  t <- t[good,,drop=FALSE]; keys <- keys[good]
  counts['age_and_overlap_eligible'] <- length(keys)
  c <- core$h14c_r[match(keys,core$h14c_r$key),]
  pr <- core$h14pr_r[match(keys,core$h14pr_r$key),]
  origin <- month_index(t$OIWYEAR,t$OIWMONTH)
  early <- month_index(t$MIWYEAR,t$MIWMONTH)
  d <- data.frame(household=t$HHID,age=t$OAGE,
    sex=factor(ifelse(t$SEX %in% 1:2,t$SEX,NA),levels=1:2),
    smoke=factor(smoking_status(c$OC117,c$OC116,pr$OZ076,pr$OZ205),
      levels=c('never','former','current')),
    health=factor(ifelse(c$OC001 %in% 1:5,c$OC001,NA),levels=1:5),
    grip14=b$grip_max[match(keys,b$key)],grip10=a$grip_max[match(keys,a$key)],
    gap_years=(origin-early)/12)
  d$change_kg <- d$grip14-d$grip10
  follow <- make_followup(t,origin+.5,'ndi_first')
  ordered <- !is.na(origin)&!is.na(early)&early<origin
  counts['ordered_interview_months'] <- sum(ordered)
  eligible <- ordered & follow$reason=='included'
  counts['followup_eligible'] <- sum(eligible)
  for(reason in setdiff(unique(follow$reason),'included'))
    counts[paste0('followup_excluded_',reason)] <- sum(ordered & follow$reason==reason)
  covok <- complete.cases(d)
  prediction_covok <- complete.cases(d[c('household','age','sex','smoke','health','grip14','grip10')])
  counts['covariate_exclusions'] <- sum(eligible & !covok)
  d <- cbind(d[eligible&covok,,drop=FALSE],follow[eligible&covok,c('lo','hi'),drop=FALSE])
  counts['analyzed'] <- nrow(d)
  counts['interval_events'] <- sum(is.finite(d$hi))
  counts['early_censored'] <- sum(!is.finite(d$hi)&d$lo<5)
  date <- core$h14a_r[match(keys,core$h14a_r$key),]
  list(data=d,counts=counts,keys=keys[eligible&covok],tracker=t[eligible&covok,,drop=FALSE],
    flow_context=list(tracker=t,origin=origin,early=early,covok=prediction_covok,
      core_month=month_index(date$OA501,date$OA500)))
}

change_main <- function(args,script_dir) {
  if(length(args)<2L) stop('Usage: Rscript hrs_grip_change.R DATA_DIR OUTPUT_DIR [BOOTSTRAPS>=20]')
  if(!requireNamespace('survival',quietly=TRUE)) stop('R package survival is required.')
  B <- if(length(args)>=3L) suppressWarnings(as.numeric(args[3])) else 500L
  if(length(B)!=1L || !is.finite(B) || B<20 || B!=floor(B)) stop('Use an integer bootstrap count >=20.')
  out <- file.path(args[2],'grip_change_v1')
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  status <- file.path(out,'STATUS.txt')
  writeLines('INCOMPLETE: outputs from previous runs are stale until SUCCESS or PARTIAL.',status)
  source(file.path(script_dir,'hrs_feasibility.R'),local=TRUE)
  # Keep even refreshed import diagnostics inside the new output directory.
  imported <- stage2_main(c(args[1],file.path(out,'import_checks')),
    file.path(script_dir,'hrs_feasibility.R'))
  prepared <- prepare_change_data(imported); d <- prepared$data
  write.csv(diagnostic_report(prepared$counts),file.path(out,'sample_flow.csv'),row.names=FALSE)
  if(nrow(d)<100 || sum(is.finite(d$hi))<50) {
    writeLines('INCOMPLETE: insufficient cohort/events for the planned model; no curve fitted.',status)
    return(invisible(NULL))
  }
  grid <- change_grid(d); target <- change_target(d,grid)
  prepared$counts['curve_standardization_target'] <- nrow(target)
  prepared$counts['curve_target_exclusions'] <- nrow(d)-nrow(target)
  write.csv(diagnostic_report(prepared$counts),file.path(out,'sample_flow.csv'),row.names=FALSE)
  result <- change_bootstrap(d,grid,B)
  write.csv(result,file.path(out,'risk_curve.csv'),row.names=FALSE)
  sensitivity <- tryCatch(change_curve(fit_change(d,'lognormal'),d,grid),error=function(e) NULL)
  write.csv(if(is.null(sensitivity)) data.frame(status='MODEL_FAILED') else
    data.frame(sensitivity,status='FIT_OK_POINT_ESTIMATES_ONLY'),
    file.path(out,'lognormal_sensitivity.csv'),row.names=FALSE)
  writeLines(c('Exploratory association analysis, planned after the prediction results were known.',
    paste('R:',getRversion()),paste('survival:',utils::packageVersion('survival')),
    paste('Bootstrap repetitions:',B),'Seed: 20260927',
    'Exposure: grip2014 minus grip2010, kg; positive means gain. No change categories.',
    'Weibull interval-survival model; natural cubic splines (3 df) for change, grip2010 and age.',
    'Further covariates: sex, smoking, self-rated health in 2014 and linear interview gap in years.',
    paste('Interview gap:',if(length(unique(d$gap_years))>1L) 'adjusted' else 'constant; held fixed'),
    'Current grip is not additionally adjusted, since current = baseline + change.',
    'Same primary cohort/follow-up rules as analysis_v1; covariates complete-case, no imputation.',
    'Deaths remain interval-censored; start is interview-month midpoint; horizon-crossing intervals censored at lower bound.',
    'No NDI record alone does not establish survival. Early censoring remains in the likelihood.',
    'Curve grid: integer kg inside observed 10th-90th change percentiles; zero must be interior.',
    'Every curve point averages predictions over the same target subset: baseline+every grid value must be >0 and <=100 kg.',
    'This physiological range screen does NOT prove joint covariate support; extrapolation within covariate strata remains possible.',
    'All eligible people fit the model; target subset only affects curve averaging and is reported in sample_flow.csv.',
    'Risk differences compare each grid value with zero change using paired bootstrap predictions.',
    'Pointwise 95% percentile CIs from household resampling, refitting splines and restandardizing in each draw.',
    'The grid is held fixed across draws. These are not simultaneous bands or an overall significance test.',
    'At least 90% successful draws required for CIs; otherwise UNSTABLE. 500 draws are an initial run, not a Monte Carlo stability guarantee.',
    'Sensitivity: lognormal model, point estimates only; no validated prediction claim or bootstrap CIs for that model.',
    'Risks are model-based associations, not causal effects or observed risks in change groups.',
    'Unweighted selected repeated-measurement cohort, not population-representative estimates.',
    'Limitations: residual confounding, reverse causation, baseline measurement error/regression to the mean, selection and informative censoring.',
    '2014 health covariates can themselves follow strength change; adjustment does not identify a total causal effect.',
    'Model distribution, interview-time proxy and source rules need substantive assessment.',
    'Counts rounded/suppressed; no individual predictions, records, coefficients or model objects exported.',
    'Separate model-equivalence work remains pending independently justified relevance margins.'),file.path(out,'READ_ME.txt'))
  stable <- all(result$status=='OK')
  if(stable) plot_change_curve(result,file.path(out,'risk_curve.png'))
  writeLines(if(!stable) 'INCOMPLETE: bootstrap unstable; do not interpret intervals or old plots.' else
    if(is.null(sensitivity)) 'PARTIAL: main curve complete; lognormal sensitivity failed.' else
    'SUCCESS: exploratory association curve and sensitivity completed; inspect limitations.',status)
  message('Aggregate change reports written to grip_change_v1. Inspect STATUS.txt.')
  invisible(NULL)
}

if(sys.nframe()==0L) {
  script <- sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  directory <- dirname(normalizePath(script))
  for(name in c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R',
               'hrs_analysis.R','hrs_prediction_validation.R')) source(file.path(directory,name))
  set.seed(20260927)
  tryCatch(change_main(commandArgs(trailingOnly=TRUE),directory),error=function(e) {
    message('Change analysis stopped. Inspect STATUS.txt and aggregate import_checks; detailed conditions are not exported.')
    quit(status=1L)
  })
}
