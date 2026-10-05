# Local diagnostic run. Aggregate-only exports; unchanged analysis rules.
grip_quality_counts <- function(raw,effort) {
  valid <- is.finite(raw)&raw>0&raw<=100
  zeros <- is.finite(raw)&raw==0
  counts <- c(people=nrow(raw),zero_readings=sum(zeros),
    people_with_zero=sum(rowSums(zeros)>0),
    people_any_positive_valid=sum(rowSums(valid)>0),
    people_valid_only_if_zero_allowed=sum(rowSums(valid)==0 & rowSums(zeros)>0),
    nonmeasurement_codes=sum(raw %in% c(993,998,999)),
    other_outside_range=sum(is.finite(raw)&(raw<0|(raw>100 & !raw %in% c(993,998,999)))),
    effort_full=sum(effort %in% 1),effort_limited_or_unclear=sum(effort %in% c(2,3)),
    effort_missing_or_invalid=sum(!effort %in% 1:3))
  for(k in 0:4) counts[paste0('people_valid_trials_',k)] <- sum(rowSums(valid)==k)
  counts
}

calibration_groups <- function(d,risk) {
  weights <- horizon_weights(d)
  group <- cut(risk,c(0,.05,.1,.2,.4,1),include.lowest=TRUE,right=FALSE)
  rows <- lapply(levels(group),function(level) {
    use <- group==level; event <- weights$y[use]==1; w <- weights$weight[use]
    n <- sum(use); deaths <- sum(event); survivors <- sum(!event & w>0)
    safe <- n>=50 & deaths>=10 & survivors>=10
    shown <- diagnostic_report(c(n=n,events=deaths,known_survivors=survivors))$count_rounded_10
    data.frame(risk_band=level,n_rounded=shown[1],events_rounded=shown[2],
      known_survivors_rounded=shown[3],
      predicted_risk=if(safe) mean(risk[use]) else NA_real_,
      observed_risk=if(safe) sum(w*event)/sum(w) else NA_real_,
      status=if(safe) 'OK' else 'SUPPRESSED_SPARSE')
  })
  do.call(rbind,rows)
}

corrected_flows <- function(context) {
  z <- context; out <- numeric()
  scenarios <- c('primary','core_month','agree_dates_only','month_start','month_end',
                'interview_first','exclude_conflicts','ndi_only','lognormal')
  for(name in scenarios) {
    origin <- if(name=='core_month') z$core_month else z$origin
    offset <- if(name=='month_start') 0 else if(name=='month_end') 1 else .5
    strategy <- if(name %in% c('interview_first','exclude_conflicts','ndi_only')) name else 'ndi_first'
    follow <- make_followup(z$tracker,origin+offset,strategy)
    timing <- !is.na(origin)&!is.na(z$early)&z$early<origin
    if(name=='agree_dates_only') timing <- timing & !is.na(z$core_month)&z$core_month==z$origin
    counts <- scenario_flow(follow,timing,z$covok)
    out[paste0(name,'_',names(counts))] <- counts
  }
  diagnostic_report(out)
}

plot_model_diagnostics <- function(cal,surv,path) {
  grDevices::png(path,width=1800,height=900,res=160)
  on.exit(grDevices::dev.off())
  par(mfrow=c(1,2),mar=c(5,5,4,1),oma=c(2,0,0,0))
  models <- unique(cal$model); colours <- c('black','steelblue4','grey50','darkorange3')
  plot(c(0,1),c(0,1),type='n',xlab='Mittlere vorhergesagte Wahrscheinlichkeit',
    ylab='IPCW-gewichteter beobachteter Anteil',main='Kalibrierungsdiagnostik (apparent)')
  abline(0,1,lty=2,col='grey60')
  if(!any(cal$status=='OK')) text(.5,.4,'Keine Risikobaender mit ausreichender Fallzahl',cex=.8)
  for(i in seq_along(models)) {
    z <- cal[cal$model==models[i]&cal$status=='OK',]
    points(z$predicted_risk,z$observed_risk,type='b',pch=i,col=colours[i])
  }
  legend('topleft',models,col=colours[seq_along(models)],pch=seq_along(models),cex=.7,bty='n')
  plot(c(0,5),c(0,1),type='n',xlab='Jahre seit angenaehertem Beginn',
    ylab='Ueberlebenswahrscheinlichkeit',main='Marginale Modellpassung')
  for(i in seq_along(models)) {
    z <- surv[surv$model==models[i],]
    lines(c(0,z$year),c(1,z$predicted_survival),col=colours[i],lty=i,lwd=2)
  }
  z <- surv[!duplicated(surv$year),]
  points(z$year,z$turnbull_survival,pch=16)
  legend('bottomleft','Punkte: Turnbull-Schaetzung',pch=16,bty='n',cex=.8)
  mtext('Diagnostik ohne Konfidenzintervalle; keine externe Validierung oder kausale Aussage',
    side=1,outer=TRUE,cex=.8)
}

diagnostics_main <- function(args,script_dir) {
  if(length(args)!=2L) stop('Usage: Rscript hrs_diagnostics.R DATA_DIR OUTPUT_DIR')
  if(!requireNamespace('survival',quietly=TRUE)) stop('R package survival is required.')
  out <- file.path(args[2],'diagnostics_v1'); dir.create(out,recursive=TRUE,showWarnings=FALSE)
  status <- file.path(out,'STATUS.txt')
  writeLines('INCOMPLETE: previous outputs are stale until SUCCESS or PARTIAL.',status)
  imported <- stage2_main(c(args[1],file.path(out,'import_checks')),
    file.path(script_dir,'hrs_feasibility.R'))
  prepared <- prepare_change_data(imported); d <- prepared$data
  write.csv(diagnostic_report(prepared$counts),file.path(out,'sample_flow.csv'),row.names=FALSE)
  write.csv(corrected_flows(prepared$flow_context),file.path(out,'corrected_scenario_flow.csv'),row.names=FALSE)
  quality <- numeric()
  for(wave in c('10','14')) {
    prefix <- if(wave=='10') 'M' else 'O'
    core <- imported$core[[paste0('h',wave,'i_r')]]
    for(scope in c('all_core','final_cohort')) {
      x <- if(scope=='all_core') core else core[match(prepared$keys,core$key),,drop=FALSE]
      raw <- as.matrix(x[paste0(prefix,'I',c('816','851','852','853'))])
      counts <- grip_quality_counts(raw,x[[paste0(prefix,'I817')]])
      quality[paste0('wave',wave,'_',scope,'_',names(counts))] <- counts
    }
  }
  write.csv(diagnostic_report(quality),file.path(out,'grip_quality.csv'),row.names=FALSE)
  if(nrow(d)<100 || sum(is.finite(d$hi))<50) {
    writeLines('INCOMPLETE: cohort too small for model diagnostics; counts only.',status)
    return(invisible(NULL))
  }
  t <- prepared$tracker; origin <- month_index(t$OIWYEAR,t$OIWMONTH)+.5
  source <- select_death_source(t)
  raw_hi <- (source$upper+1-origin)/12
  raw_lo <- (source$lower-origin)/12
  crossing <- !is.na(raw_lo)&raw_lo<5 & !is.na(raw_hi)&raw_hi>5
  early <- !is.finite(d$hi)&d$lo<5
  counts <- c(analyzed=nrow(d),interval_events=sum(is.finite(d$hi)),
    early_censored=sum(early),early_censored_horizon_overlap=sum(early&crossing),
    early_censored_last_alive=sum(early&is.na(source$lower)),
    early_censored_other=sum(early&!crossing&!is.na(source$lower)),
    observed_beyond_horizon=sum(!is.finite(d$hi)&d$lo>=5))
  write.csv(diagnostic_report(counts),file.path(out,'censoring_counts.csv'),row.names=FALSE)
  groups <- list(age=cut(d$age,c(50,65,80,121),right=FALSE),sex=d$sex,
    current_grip=cut(d$grip14,c(0,20,30,40,101),right=FALSE),
    health=d$health)
  rows <- list()
  for(field in names(groups)) for(level in levels(groups[[field]])) {
    use <- groups[[field]]==level; n <- sum(use); k <- sum(early&use)
    shown <- diagnostic_report(c(n=n,early=k))$count_rounded_10
    rows[[length(rows)+1]] <- data.frame(variable=field,group=level,n_rounded=shown[1],
      early_censored_rounded=shown[2],early_fraction=if(n>=50 && k>=10 && n-k>=10) round(k/n,3) else NA_real_)
  }
  write.csv(do.call(rbind,rows),file.path(out,'censoring_groups.csv'),row.names=FALSE)
  weights <- horizon_weights(d); w <- weights$weight[weights$weight>0]
  write.csv(data.frame(metric=c('G_min','G_median','weight_max','weight_median','effective_known_sample_size_rounded_10'),
    value=c(round(min(1/w),4),round(median(1/w),4),round(max(w),4),round(median(w),4),
            10*floor((sum(w)^2/sum(w^2))/10+.5))),file.path(out,'weight_diagnostics.csv'),row.names=FALSE)
  rows <- list()
  change <- cut(d$change_kg,c(-Inf,-5,0,5,Inf),right=FALSE)
  for(field in c('grip10','grip14')) {
    strength <- cut(d[[field]],c(0,20,30,40,101),right=FALSE)
    for(a in levels(strength)) for(b in levels(change)) {
      n <- sum(strength==a & change==b)
      rows[[length(rows)+1]] <- data.frame(strength_wave=field,strength_band=a,
        change_band=b,count_rounded_10=diagnostic_report(c(n=n))$count_rounded_10)
    }
  }
  write.csv(do.call(rbind,rows),file.path(out,'joint_support.csv'),row.names=FALSE)
  # Apparent diagnostic fits only; no expensive bootstrap or model selection.
  fits <- list(); failures <- character()
  for(dist in c('weibull','lognormal')) {
    p <- tryCatch(fit_pair(d,dist),error=function(e) NULL)
    if(is.null(p)) { failures <- c(failures,dist); next }
    for(name in names(p)) fits[[paste(dist,name,sep='_')]] <- p[[name]]
  }
  if(!length(fits)) stop('No diagnostic model fit succeeded.')
  empirical <- tryCatch({
    f <- survival::survfit(survival::Surv(lo,hi,type='interval2')~1,data=d)
    summary(f,times=1:5,extend=TRUE)$surv
  },error=function(e) rep(NA_real_,5))
  if(length(empirical)!=5L || any(!is.finite(empirical))) {
    empirical <- rep(NA_real_,5); failures <- c(failures,'turnbull')
  }
  calibration <- curves <- model_rows <- list()
  for(name in names(fits)) {
    fit <- fits[[name]]; risk <- risk5(fit,d)
    calibration[[name]] <- data.frame(model=name,calibration_groups(d,risk))
    lp <- as.numeric(predict(fit,newdata=d,type='lp'))
    s <- vapply(1:5,function(year) mean(if(fit$dist=='weibull')
      pweibull(year,shape=1/fit$scale,scale=exp(lp),lower.tail=FALSE) else
      plnorm(year,meanlog=lp,sdlog=fit$scale,lower.tail=FALSE)),0)
    curves[[name]] <- data.frame(model=name,year=1:5,predicted_survival=s,turnbull_survival=empirical)
    model_rows[[name]] <- data.frame(model=name,AIC=as.numeric(AIC(fit)),
      mean_interval_log_score=interval_score(fit,d),status='FIT_OK_APPARENT_ONLY')
  }
  cal <- do.call(rbind,calibration); surv <- do.call(rbind,curves)
  write.csv(cal,file.path(out,'calibration_groups.csv'),row.names=FALSE)
  write.csv(surv,file.path(out,'survival_fit.csv'),row.names=FALSE)
  write.csv(do.call(rbind,model_rows),file.path(out,'model_fit_summary.csv'),row.names=FALSE)
  plot_model_diagnostics(cal,surv,file.path(out,'model_diagnostics.png'))
  files <- sort(list.files(script_dir,pattern='\\.R$',full.names=TRUE))
  hashes <- system2('/usr/bin/shasum',c('-a','256',shQuote(files)),stdout=TRUE)
  if(!is.null(attr(hashes,'status'))) stop('Source fingerprint creation failed.')
  writeLines(hashes,file.path(out,'source_sha256.txt'))
  writeLines(c('Targeted diagnostic run; original analysis rules unchanged.',
    paste('R:',getRversion()),paste('survival:',utils::packageVersion('survival')),
    paste('Created:',format(Sys.time(),tz='Europe/Berlin',usetz=TRUE)),
    'No bootstrap; same current/prior models fitted under Weibull and lognormal, plus marginal Turnbull estimate.',
    'Corrected flow: timing filter precedes reason counts; included equals before_covariate_exclusions. No cohort definition change.',
    'all_core quality counts concern each complete wave file, not age-restricted or paired people.',
    'final_cohort quality counts concern modeled people only. Zero-only people excluded earlier cannot appear there.',
    'Zero-grip validity is NOT decided here; hypothetical zero eligibility is diagnostic only.',
    'Joint-support bins are descriptive, not new analysis groups or proof of adequate conditional support.',
    'All small counts (<10, including zero) suppressed; others rounded to 10. This is not formal disclosure control.',
    'Censoring fractions rounded; withheld for n<50 or fewer than 10 early/non-early censored people.',
    'Calibration: fixed probability bands; group mean predicted risk vs Hajek IPCW observed event fraction (sum(w*y)/sum(w)).',
    'Calibration estimates withheld for n<50, fewer than 10 events or fewer than 10 known survivors; no confidence intervals.',
    'Bands may contain different people for different models. Apparent diagnostics, NOT independent validation.',
    'Turnbull marginal survival at fixed years 1-5; no individual event times or step locations exported.',
    'Marginal Turnbull estimation assumes noninformative censoring/coarsening; it is not a gold standard.',
    'Marginal fit cannot verify conditional model fit. IPCW and interval-midpoint limitations remain.',
    'Source/time sensitivities from prior runs are not rerun here; no model selected based on AIC.',
    'No individual rows, predictions, identifiers or model objects saved. Source hashes cover code, not private raw data.',
    paste('Diagnostic fit failures:',if(length(failures)) paste(failures,collapse=', ') else 'none')),
    file.path(out,'READ_ME.txt'))
  writeLines(if(length(failures)) 'PARTIAL: inspect diagnostic model failures in READ_ME.txt.' else
    'SUCCESS: targeted diagnostics completed; assumptions still require review.',status)
  message('Aggregate diagnostics written to diagnostics_v1. Inspect STATUS.txt.')
}

if(sys.nframe()==0L) {
  script <- sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  directory <- dirname(normalizePath(script))
  for(name in c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R',
               'hrs_analysis.R','hrs_prediction_validation.R','hrs_grip_change.R'))
    source(file.path(directory,name))
  tryCatch(diagnostics_main(commandArgs(trailingOnly=TRUE),directory),error=function(e) {
    message('Diagnostics stopped. Inspect STATUS.txt and import_checks; detailed conditions are not exported.')
    quit(status=1L)
  })
}
