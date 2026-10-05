# Question 1.1: earlier grip varies while current grip stays fixed.
# Reuse the ORIGINAL two-measurement model, not a new change-spline model.
fit_history <- function(d,dist='weibull') {
  fit <- fit_pair(d,dist)$prior
  if(any(!is.finite(coef(fit))) || any(!is.finite(fit$var)) ||
     !is.finite(fit$scale) || fit$scale<=0) stop('Invalid history model fit.')
  fit
}

history_target <- function(d,grid) {
  stopifnot(all(is.finite(grid)),!anyDuplicated(grid),0 %in% grid)
  # Earlier = current - change. One common target for every contrast.
  d[d$grip14-max(grid)>0 & d$grip14-min(grid)<=100,,drop=FALSE]
}

history_curve <- function(fit,d,grid) {
  target <- history_target(d,grid)
  if(nrow(target)<100) stop('Insufficient history standardization target.')
  values <- vapply(grid,function(x) {
    z <- target
    z$grip10 <- z$grip14-x
    z$change_kg <- x
    mean(risk5(fit,z))
  },0)
  if(any(!is.finite(values)) || any(values<0 | values>1)) stop('Invalid history curve.')
  data.frame(change_kg=grid,risk5=values,risk_difference=values-values[grid==0])
}

history_main <- function(args,script_dir) {
  if(length(args)<2L) stop('Usage: Rscript hrs_grip_history.R DATA_DIR OUTPUT_DIR [BOOTSTRAPS>=20]')
  if(!requireNamespace('survival',quietly=TRUE)) stop('R package survival is required.')
  B <- if(length(args)>=3L) suppressWarnings(as.numeric(args[3])) else 500L
  if(!is.finite(B) || B<20 || B!=floor(B)) stop('Use an integer bootstrap count >=20.')
  out <- file.path(args[2],'grip_history_v1')
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  status <- file.path(out,'STATUS.txt')
  writeLines('INCOMPLETE: older outputs in this directory are stale until SUCCESS or PARTIAL.',status)
  imported <- stage2_main(c(args[1],file.path(out,'import_checks')),
    file.path(script_dir,'hrs_feasibility.R'))
  prepared <- prepare_change_data(imported); d <- prepared$data
  write.csv(diagnostic_report(prepared$counts),file.path(out,'sample_flow.csv'),row.names=FALSE)
  if(nrow(d)<100 || sum(is.finite(d$hi))<50) {
    writeLines('INCOMPLETE: insufficient cohort/events; no history curve fitted.',status)
    return(invisible(NULL))
  }
  grid <- change_grid(d)
  target <- history_target(d,grid)
  prepared$counts['curve_standardization_target'] <- nrow(target)
  prepared$counts['curve_target_exclusions'] <- nrow(d)-nrow(target)
  write.csv(diagnostic_report(prepared$counts),file.path(out,'sample_flow.csv'),row.names=FALSE)
  result <- change_bootstrap(d,grid,B,fit_fun=fit_history,curve_fun=history_curve)
  write.csv(result,file.path(out,'risk_curve.csv'),row.names=FALSE)
  sensitivity <- tryCatch(history_curve(fit_history(d,'lognormal'),d,grid),error=function(e) NULL)
  write.csv(if(is.null(sensitivity)) data.frame(status='MODEL_FAILED') else
    data.frame(sensitivity,status='FIT_OK_POINT_ESTIMATES_ONLY'),
    file.path(out,'lognormal_sensitivity.csv'),row.names=FALSE)
  writeLines(c('Question 1.1: history association at the SAME CURRENT grip.',
    'Exploratory analysis planned after prediction and baseline-adjusted change results were known.',
    paste('R:',getRversion()),paste('survival:',utils::packageVersion('survival')),
    paste('Household bootstrap repetitions:',B),'Seed: 20260928',
    'Original two-measurement model: Weibull interval survival, ns(age,3), sex, smoking, self-rated health, ns(grip2014,3), ns(grip2010,3).',
    'No extra change spline or gap term: this is the original main-question model, refitted for uncertainty.',
    'change_kg = grip2014 - grip2010 is ONLY a display/contrast coordinate.',
    'For each person, current grip and all other covariates stay fixed across the curve; earlier grip = current grip - change_kg.',
    'A -5 kg contrast compares earlier=current+5 with earlier=current, at the same current grip.',
    'Curves average over a common selected target; current grip varies BETWEEN people, not BETWEEN contrasts for a person.',
    'Target: every implied earlier grip across the fixed grid must be >0 and <=100 kg. All eligible people fit the model.',
    'Grid: integer kg within observed 10th-90th change percentiles; zero must be interior.',
    'This range screen is not evidence of joint support; conditional extrapolation remains possible.',
    'Risk differences compare each point with zero change. At zero, difference and its interval are zero by construction.',
    'Pointwise 95% household-bootstrap percentile intervals; refit splines and restandardize each draw; fixed grid.',
    'At least 90% successful draws for CIs; otherwise UNSTABLE. No simultaneous band or global test.',
    '500 draws are an initial run, not a Monte Carlo stability guarantee.',
    'Same primary cohort and endpoint rules: month-midpoint start, death intervals retained, crossing-five-year intervals censored at lower bound.',
    'Early censoring retained; missing NDI alone never establishes survival; complete-case covariates.',
    'Lognormal sensitivity: point estimates only, not separately bootstrapped.',
    'Unweighted selected repeated-measurement cohort; no causal or population-representative claim.',
    'Limitations: residual confounding, measurement error, selection, reverse causation, informative censoring and parametric/time-proxy assumptions.',
    'Conditional association is not prediction improvement and does not establish practical equivalence.',
    'No individual predictions, records, coefficients or model objects saved. Counts rounded/suppressed.'),
    file.path(out,'READ_ME.txt'))
  stable <- all(result$status=='OK')
  if(stable) plot_change_curve(result,file.path(out,'risk_curve.png'),title='Gleiche aktuelle Handkraft')
  writeLines(if(!stable) 'INCOMPLETE: bootstrap unstable; do not use intervals or older plots.' else
    if(is.null(sensitivity)) 'PARTIAL: history curve complete; sensitivity failed.' else
    'SUCCESS: fixed-current history curve and sensitivity completed; inspect limitations.',status)
  message('Aggregate history reports written to grip_history_v1. Inspect STATUS.txt.')
  invisible(NULL)
}

if(sys.nframe()==0L) {
  script <- sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  directory <- dirname(normalizePath(script))
  for(name in c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R',
               'hrs_analysis.R','hrs_prediction_validation.R','hrs_grip_change.R'))
    source(file.path(directory,name))
  set.seed(20260928)
  tryCatch(history_main(commandArgs(trailingOnly=TRUE),directory),error=function(e) {
    message('History analysis stopped. Inspect STATUS.txt and import_checks; detailed conditions are not exported.')
    quit(status=1L)
  })
}
