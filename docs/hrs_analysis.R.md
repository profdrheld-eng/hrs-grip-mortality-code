# Annotated source: hrs_analysis.R

Read-only explanation of the executed source. Run the materialized package, not this Markdown view. Only REVIEW NOTE comments are added.

Source: `package/reported_version/02_Analyse/scripts/hrs_analysis.R`

Source SHA-256: `8b9fc506934f22099306b227e3c57f8767ea25ad720b744e5dc1fb9d111e6dc2`

```r
# User-run first interval-survival analysis. No person-level exports.
# REVIEW NOTE: Build interval-censored follow-up in years from calendar-month bounds. Death evidence without usable timing must not become a survivor. Intervals crossing five years are censored at their lower bound, an explicit approximation rather than an exact death date.
make_followup <- function(t,start,strategy='ndi_first') {
  s <- select_death_source(t,strategy)
  alive <- month_index(t$LASTALIVEYR,t$LASTALIVEMO)
  alive[!t$LASTALIVESOURCE %in% 1:3] <- NA_real_
  lo <- (s$lower-start)/12; hi <- (s$upper+1-start)/12
  reason <- rep('included',nrow(t))
  reason[is.na(start)] <- 'missing_start'
  reason[!is.na(lo)&lo<=0] <- 'death_interval_at_or_before_start'
  reason[s$requires_review|s$excluded] <- 'unresolved_conflict'
  absent <- is.na(s$lower)
  # A death indication with no eligible source must not become a survivor.
  death_hint <- !is.na(t$KNOWNDECEASEDYR)|!is.na(t$EXDEATHYR)|!is.na(t$NYEAR)
  censor <- pmin(5,(alive-start)/12)
  lo[absent] <- censor[absent]; hi[absent] <- Inf
  reason[absent & (death_hint|is.na(censor)|censor<=0) & reason=='included'] <- 'insufficient_followup'
  # Administratively coarsen intervals crossing 5 years at their lower bound.
  later <- !absent & !is.na(lo) & lo>=5
  lo[later] <- 5; hi[later] <- Inf
  crosses <- !absent & !is.na(hi) & hi>5 & !later
  hi[crosses] <- Inf
  data.frame(lo=lo,hi=hi,reason=reason,source=s$source)
}

scenario_flow <- function(follow,timing_ok,covok) {
  stopifnot(length(timing_ok)==nrow(follow),length(covok)==nrow(follow),
    !anyNA(timing_ok),!anyNA(covok))
  eligible <- timing_ok & follow$reason=='included'
  counts <- c(timing_eligible=sum(timing_ok),timing_excluded=sum(!timing_ok),
    before_covariate_exclusions=sum(eligible),covariate_exclusions=sum(eligible & !covok))
  for(reason in unique(follow$reason)) counts[reason] <- sum(timing_ok & follow$reason==reason)
  counts['analyzed'] <- sum(eligible & covok)
  counts['interval_events'] <- sum(eligible & covok & is.finite(follow$hi))
  counts
}

# REVIEW NOTE: Both models use the same rows and common adjustment set. The prior model adds 2010 grip to current grip. A spline is refitted in each bootstrap training sample. Model warnings are failures.
fit_pair <- function(d,dist='weibull') {
  f <- survival::Surv(lo,hi,type='interval2') ~ splines::ns(age,df=3)+sex+smoke+health+splines::ns(grip14,df=3)
  g <- update(f,. ~ . + splines::ns(grip10,df=3))
  fit <- function(form) {
    # Warnings are failures, not silently accepted convergence problems.
    withCallingHandlers(survival::survreg(form,data=d,dist=dist,
      na.action=na.fail,control=survival::survreg.control(maxiter=100)),
      warning=function(w) stop('Model warning; no records or detailed condition exported.'))
  }
  list(current=fit(f),prior=fit(g))
}

# REVIEW NOTE: Score the probability of the observed interval, or survival beyond the censoring bound. The expm1 identity avoids cancellation for narrow intervals. This score selects ML configurations and does not use IPCW.
interval_score <- function(fit,d) {
  lp <- as.numeric(predict(fit,newdata=d,type='lp'))
  logs <- function(time) {
    if(fit$dist=='weibull') pweibull(time,shape=1/fit$scale,scale=exp(lp),lower.tail=FALSE,log.p=TRUE)
    else plnorm(time,meanlog=lp,sdlog=fit$scale,lower.tail=FALSE,log.p=TRUE)
  }
  a <- logs(d$lo); b <- logs(d$hi)
  score <- a
  event <- is.finite(d$hi)
  score[event] <- a[event]+log(-expm1(b[event]-a[event]))
  if(any(!is.finite(score))) stop('Nonfinite interval score.')
  mean(score)
}

# REVIEW NOTE: Resample households, not individual rows. Optimism is training minus original-sample performance for each refitted model. This earlier routine has point estimates only. The extended validation is implemented in clinical_validation.
validate_pair <- function(d,pair,B) {
  apparent <- vapply(pair,interval_score,0,d=d)
  groups <- unique(d$household)
  optimism <- matrix(NA_real_,B,2)
  for(b in seq_len(B)) {
    draws <- sample(groups,length(groups),replace=TRUE)
    indices <- unlist(lapply(draws,function(h) which(d$household==h)),use.names=FALSE)
    boot <- d[indices,,drop=FALSE]
    attempt <- tryCatch({
      p <- fit_pair(boot)
      vapply(p,interval_score,0,d=boot)-vapply(p,interval_score,0,d=d)
    },error=function(e) NULL)
    if(!is.null(attempt)) optimism[b,] <- attempt
  }
  good <- complete.cases(optimism)
  corrected <- if(sum(good)>=ceiling(.9*B)) apparent-colMeans(optimism[good,,drop=FALSE]) else rep(NA_real_,2)
  list(apparent=apparent,corrected=corrected,successful=sum(good),requested=B)
}

# REVIEW NOTE: CLI orchestrator. Real data must be handled in a private output root. Historical output names are fixed, so always choose a new output parent. STATUS is marked incomplete before work begins.
analysis_main <- function(args,script_dir,extended=FALSE) {
  if(length(args)<2L) stop('Usage: Rscript hrs_analysis.R DATA_DIR OUTPUT_DIR [BOOTSTRAPS>=20]')
  if(!requireNamespace('survival',quietly=TRUE)) stop('R package survival is required; no automatic installation.')
  B <- if(length(args)>=3L) suppressWarnings(as.integer(args[3])) else 200L
  if(is.na(B)||B<20) stop('At least 20 bootstrap repetitions required; use 200 for the first substantive run.')
  inner <- if(length(args)>=4L) suppressWarnings(as.integer(args[4])) else 50L
  if(extended && (is.na(inner)||inner<20)) stop('At least 20 inner bootstrap repetitions required; 50 recommended.')
  out <- file.path(args[2],if(extended) 'validation_v2' else 'analysis_v1')
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  writeLines('INCOMPLETE: previous outputs in this directory are stale until this status changes.',file.path(out,'STATUS.txt'))
  source(file.path(script_dir,'hrs_feasibility.R'),local=TRUE)
  # stage2 performs all structural checks and returns in-memory data only.
  imported <- stage2_main(args[1:2],file.path(script_dir,'hrs_feasibility.R'))
  core <- imported$core; t <- imported$tracker
  a <- wave_summary(core$h10i_r,'M'); b <- wave_summary(core$h14i_r,'O')
  keys <- intersect(a$key[a$valid_trials>0],b$key[b$valid_trials>0])
  keys <- keys[keys %in% t$key]; t <- t[match(keys,t$key),]
  good <- !is.na(t$OAGE)&t$OAGE>=50&t$OAGE<=120&t$OAGE==floor(t$OAGE)&t$OVHHID=='000000'
  t <- t[good,,drop=FALSE]; keys <- keys[good]
  c <- core$h14c_r[match(keys,core$h14c_r$key),]
  pr <- core$h14pr_r[match(keys,core$h14pr_r$key),]
  date <- core$h14a_r[match(keys,core$h14a_r$key),]
  tracker_month <- month_index(t$OIWYEAR,t$OIWMONTH)
  core_month <- month_index(date$OA501,date$OA500)
  early <- month_index(t$MIWYEAR,t$MIWMONTH)
  d <- data.frame(household=t$HHID,age=t$OAGE,
    sex=factor(ifelse(t$SEX %in% 1:2,t$SEX,NA),levels=1:2),
    smoke=factor(smoking_status(c$OC117,c$OC116,pr$OZ076,pr$OZ205),levels=c('never','former','current')),
    health=factor(ifelse(c$OC001 %in% 1:5,c$OC001,NA),levels=1:5),
    grip14=b$grip_max[match(keys,b$key)],grip10=a$grip_max[match(keys,a$key)])
  scenarios <- data.frame(name=c('primary','core_month','agree_dates_only','month_start','month_end',
    'interview_first','exclude_conflicts','ndi_only','lognormal'),
    strategy=c(rep('ndi_first',5),'interview_first','exclude_conflicts','ndi_only','ndi_first'),stringsAsFactors=FALSE)
  results <- list(); flow <- numeric(); clinical_rows <- list()
  for(i in seq_len(nrow(scenarios))) {
    name <- scenarios$name[i]
    origin <- if(name=='core_month') core_month else tracker_month
    offset <- if(name=='month_start') 0 else if(name=='month_end') 1 else .5
    follow <- make_followup(t,origin+offset,scenarios$strategy[i])
    timing_ok <- !is.na(origin)&!is.na(early)&early<origin
    if(name=='agree_dates_only') timing_ok <- timing_ok & !is.na(core_month)&tracker_month==core_month
    eligible <- timing_ok & follow$reason=='included'
    covok <- complete.cases(d)
    counts <- scenario_flow(follow,timing_ok,covok)
    flow[paste0(name,'_',names(counts))] <- counts
    data <- cbind(d[eligible&covok,,drop=FALSE],follow[eligible&covok,c('lo','hi'),drop=FALSE])
    # Technical minimum, not a claim that the study is adequately powered.
    if(nrow(data)<100 || sum(is.finite(data$hi))<50) {
      results[[name]] <- data.frame(scenario=name,status='INSUFFICIENT_FOR_PLANNED_MODEL',
        current_score=NA,prior_score=NA,delta_score=NA,corrected_delta=NA,bootstrap_success=NA)
      next
    }
    ans <- tryCatch({
      p <- fit_pair(data,if(name=='lognormal') 'lognormal' else 'weibull')
      scores <- vapply(p,interval_score,0,d=data)
      val <- if(name=='primary' && !extended) validate_pair(data,p,B) else NULL
      validation_ok <- TRUE
      if(extended) {
        for(position in c('lower','midpoint','upper')) {
          metrics <- pair_metrics(p,data,position)
          for(k in 1:2) clinical_rows[[length(clinical_rows)+1]] <- data.frame(
            scenario=name,censor_time_position=position,model=names(p)[k],
            metric=rownames(metrics),apparent=metrics[,k])
        }
        if(name=='primary') {
          clinical <- clinical_validation(data,p,B,inner)
          write.csv(clinical,file.path(out,'validated_metrics.csv'),row.names=FALSE)
          validation_ok <- all(clinical$status=='OK')
        }
      }
      data.frame(scenario=name,status=if(!is.null(val)&&anyNA(val$corrected)) 'BOOTSTRAP_UNSTABLE' else 'FIT_OK',
        current_score=scores[1],prior_score=scores[2],delta_score=scores[2]-scores[1],
        corrected_delta=if(is.null(val)) NA else val$corrected[2]-val$corrected[1],
        bootstrap_success=if(is.null(val)) NA else val$successful) -> row
      if(!validation_ok) row$status <- 'CLINICAL_VALIDATION_UNSTABLE'
      row
    },error=function(e) data.frame(scenario=name,status='MODEL_OR_VALIDATION_FAILED',
      current_score=NA,prior_score=NA,delta_score=NA,corrected_delta=NA,bootstrap_success=NA))
    results[[name]] <- ans
  }
  write.csv(diagnostic_report(flow),file.path(out,'sample_flow.csv'),row.names=FALSE)
  write.csv(do.call(rbind,results),file.path(out,'model_comparison.csv'),row.names=FALSE)
  if(extended && length(clinical_rows)) write.csv(do.call(rbind,clinical_rows),
    file.path(out,'sensitivity_metrics.csv'),row.names=FALSE)
  writeLines(c(paste('R:',getRversion()),paste('survival:',utils::packageVersion('survival')),
    paste('Bootstrap repetitions:',B),'Seed: 20260926',
    'Primary: Weibull interval survival; unweighted selected repeated-measurement cohort.',
    'Start: midpoint of interview month, an approximation. Month start/end and Core month are sensitivities.',
    'Death intervals retained. Intervals crossing five years right-censored at their lower bound.',
    'Early censoring retained. Missing NDI alone is never treated as survival.',
    'Both models use the same complete-covariate sample; exclusions are reported, no automatic imputation.',
    'Higher mean interval log score is better; delta is prior minus current model.',
    if(extended) 'validated_metrics.csv contains optimism-corrected metrics and nested household-bootstrap percentile 95% intervals.' else
      'Primary corrected delta subtracts household-bootstrap optimism. No confidence interval is claimed.',
    if(extended) 'IPCW Brier, cumulative/dynamic AUC, calibration intercept/slope and interval log score are separate metrics.' else
      'This score evaluates observed interval/censoring likelihood, not AUC, calibration or five-year Brier score.',
    'Sensitivity rows are apparent scores, not validated performance; compare model deltas within each row only.',
    'Bootstrap repeats spline estimation and fitting. Independent censoring and parametric distribution remain assumptions.',
    'Counts rounded/suppressed. No individual predictions, model objects or records exported.',
    if(extended) paste('Inner bootstrap repetitions:',inner) else 'First model comparison only; calibration, discrimination and publication-readiness need further work.',
    if(extended) 'IPCW assumes marginal independent censoring. Interval midpoints used ONLY to estimate censoring weights; lower/upper variants are apparent sensitivities.' else '',
    if(extended) 'Administrative censoring at 5 is excluded from G(5-). G<0.05 stops evaluation; weights are not silently clipped.' else '',
    if(extended) 'Early censored persons receive zero direct outcome weight; their follow-up contributes to the censoring distribution.' else '',
    if(extended) 'Calibration intercept target 0, slope target 1. Brier lower is better, AUC and log score higher are better. All deltas are prior minus current.' else '',
    if(extended) 'Sensitivity performance is apparent, not optimism-corrected. CIs cover sampling variation, not errors in examination dates or source choice.' else '',
    if(extended) 'Assessment needed: distribution fit, informative censoring, complete-case selection, and interval-time approximation. No equivalence claim from nonsignificance.' else ''),file.path(out,'READ_ME.txt'))
  primary_ok <- identical(results$primary$status,'FIT_OK')
  all_ok <- all(vapply(results,function(x) identical(x$status,'FIT_OK'),TRUE))
  writeLines(if(all_ok) 'SUCCESS: first model comparison completed; inspect limitations.' else
    if(primary_ok) 'PARTIAL: primary fit complete; inspect failed sensitivity rows.' else
    'INCOMPLETE: primary model or validation failed; do not interpret as a completed analysis.',file.path(out,'STATUS.txt'))
  message('Analysis reports written to ',basename(out),'. Inspect STATUS.txt. No person-level outputs saved.')
}

if(sys.nframe()==0L) {
  script <- sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  directory <- dirname(normalizePath(script))
  source(file.path(directory,'hrs_stage2.R'))
  source(file.path(directory,'hrs_source_hierarchy.R'))
  set.seed(20260926)
  analysis_main(commandArgs(trailingOnly=TRUE),directory)
}
```
