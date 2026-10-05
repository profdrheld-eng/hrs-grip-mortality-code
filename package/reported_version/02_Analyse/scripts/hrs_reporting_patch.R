# Targeted user-run reporting patch. No bootstrap, tuning, neural or tree fitting.
report_month <- function(x) sprintf('%04d-%02d',as.integer(x %/% 12),as.integer(x %% 12+1))

report_counts <- function(x,half_up=FALSE) {
  stopifnot(all(is.finite(x)),all(x>=0))
  # Suppress a whole related block if any nonzero small count occurs.
  value <- if(any(x>0 & x<10)) rep('SUPPRESSED_BLOCK',length(x)) else as.character(if(half_up) 10*floor(x/10+.5) else 10*round(x/10))
  data.frame(measure=names(x),count=value)
}

report_followup <- function(d) {
  event <- is.finite(d$hi)
  midpoint <- d$lo;midpoint[event]<-(d$lo[event]+d$hi[event])/2
  groups <- list(event_or_censor_midpoint_proxy=midpoint,
    event_interval_lower=d$lo[event],event_interval_upper=d$hi[event],censoring_time=d$lo[!event])
  do.call(rbind,lapply(names(groups),function(name) {
    x<-groups[[name]];ok<-length(x)>=10
    data.frame(measure=name,n=length(x),mean_years=if(length(x)) mean(x) else NA_real_,
      sd_years=if(length(x)>1) sd(x) else NA_real_,publishable_group=ok)
  }))
}

report_model <- function(fit) {
  terms<-delete.response(terms(fit));environment(terms)<-baseenv()
  list(terms=terms,coefficients=coef(fit),scale=fit$scale,
    xlevels=fit$xlevels,contrasts=fit$contrasts,distribution=fit$dist,horizon_years=5)
}

report_risk <- function(model,d) {
  x<-model.matrix(model$terms,data=d,contrasts.arg=model$contrasts,xlev=model$xlevels)
  stopifnot(identical(colnames(x),names(model$coefficients)),model$distribution=='weibull')
  pweibull(model$horizon_years,shape=1/model$scale,scale=exp(drop(x %*% model$coefficients)))
}

report_main <- function(args,directory) {
  if(length(args)!=2L) stop('Use private data directory and private output parent.')
  for(name in c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R','hrs_analysis.R',
    'hrs_prediction_validation.R','hrs_grip_change.R','hrs_grip_history.R','hrs_ml_extension.R'))
    source(file.path(directory,name),local=.GlobalEnv)
  root<-dirname(dirname(directory))
  expected<-read.csv(file.path(root,'01_Aktuelles_Manuskript','manuskript_v2','table2_source.csv'))
  out<-file.path(args[2],paste0('reporting_patch_',format(Sys.time(),'%Y%m%d_%H%M%S'),'_',Sys.getpid()))
  if(file.exists(out) || !dir.create(out,recursive=TRUE)) stop('Cannot create a fresh output directory.')
  private<-file.path(out,'LOCAL_ONLY');share<-file.path(out,'SEND_BACK')
  dir.create(private);dir.create(share)
  status<-file.path(share,'STATUS.txt');writeLines('INCOMPLETE',status)
  tryCatch({
    message('Reading and checking source files. No models fitted during this step.')
    imported<-stage2_main(c(args[1],file.path(private,'import_checks')),file.path(directory,'hrs_feasibility.R'))
    p<-prepare_change_data(imported);d<-p$data;t<-p$tracker
    # Fail closed if the reconstruction does not match the approved manuscript cohort size.
    stopifnot(nrow(d)==6307L,!anyNA(d),nrow(t)==nrow(d))
    grid<-change_grid(d)
    counts<-c(p$counts,change_target=nrow(change_target(d,grid)),history_target=nrow(history_target(d,grid)))
    write.csv(data.frame(measure=names(counts),count=unname(counts)),file.path(private,'counts_exact.csv'),row.names=FALSE)
    selected<-counts[c('analyzed','interval_events','early_censored','change_target','history_target')]
    write.csv(report_counts(selected,half_up=TRUE),file.path(share,'cohort_counts.csv'),row.names=FALSE)
    dates<-do.call(rbind,lapply(c('M','O'),function(w) {
      x<-month_index(t[[paste0(w,'IWYEAR')]],t[[paste0(w,'IWMONTH')]])
      stopifnot(all(is.finite(x)))
      data.frame(wave=if(w=='M') 2010 else 2014,first_interview_month=report_month(min(x)),
        last_interview_month=report_month(max(x)))
    }))
    write.csv(dates,file.path(share,'interview_month_ranges.csv'),row.names=FALSE)
    follow<-report_followup(d)
    write.csv(follow,file.path(private,'followup_exact.csv'),row.names=FALSE)
    follow$n<-ifelse(follow$publishable_group,as.character(10*floor(follow$n/10+.5)),'SUPPRESSED')
    follow[!follow$publishable_group,c('mean_years','sd_years')]<-NA_real_
    follow[,c('mean_years','sd_years')]<-round(follow[,c('mean_years','sd_years')],3)
    write.csv(follow,file.path(share,'followup_summary.csv'),row.names=FALSE)
    # Recreate fold allocation only. No training, augmentation or tuning.
    cfg<-ml_config('full');fold<-ml_folds(d,cfg$outer,cfg$seed);rows<-list()
    add<-function(x,fraction,outer,inner,role) {
      rows[[length(rows)+1L]]<<-data.frame(fraction=fraction,outer=outer,inner=inner,role=role,
        participants=nrow(x),events=sum(is.finite(x$hi)))
    }
    for(fraction in cfg$fractions) for(k in seq_len(cfg$outer)) {
      train<-d[fold!=k,,drop=FALSE];test<-d[fold==k,,drop=FALSE]
      set.seed(cfg$seed+k);groups<-sample(unique(train$household))
      groups<-head(groups,max(cfg$inner*2,ceiling(length(groups)*fraction)))
      train<-train[train$household %in% groups,,drop=FALSE]
      inner<-ml_folds(train,cfg$inner,cfg$seed+100+k)
      add(train,fraction,k,0,'outer_train');add(test,fraction,k,0,'outer_evaluation')
      for(j in seq_len(cfg$inner)) {
        add(train[inner!=j,,drop=FALSE],fraction,k,j,'inner_train')
        add(train[inner==j,,drop=FALSE],fraction,k,j,'inner_evaluation')
      }
    }
    folds<-do.call(rbind,rows)
    write.csv(folds,file.path(private,'fold_counts_exact.csv'),row.names=FALSE)
    old<-read.csv(file.path(root,'01_Aktuelles_Manuskript','manuskript_v2','tableS7_tuning.csv'))
    for(i in seq_len(nrow(old))) {
      r<-folds[folds$fraction==old$fraction[i]&folds$outer==old$fold[i]&folds$role=='outer_train',]
      stopifnot(nrow(r)==1,10*round(r$participants/10)==old$training_n_rounded[i],
        10*round(r$events/10)==old$training_events_rounded[i])
    }
    for(col in c('participants','events')) folds[[col]]<-report_counts(setNames(folds[[col]],seq_len(nrow(folds))))$count
    write.csv(folds,file.path(share,'fold_counts.csv'),row.names=FALSE)
    writeLines('SUCCESS: descriptive reporting complete. Primary model export pending.',status)
    model_status<-tryCatch({
      message('Fitting only the two conventional primary models. No resampling or ML training.')
      if(!requireNamespace('survival',quietly=TRUE)) stop('Missing survival package.')
      setTimeLimit(elapsed=180,transient=TRUE)
      fits<-fit_pair(d)
      setTimeLimit(cpu=Inf,elapsed=Inf,transient=FALSE)
      metrics<-pair_metrics(fits,d);checks<-list()
      for(name in names(fits)) {
        model<-report_model(fits[[name]])
        stopifnot(max(abs(report_risk(model,d)-risk5(fits[[name]],d)))<1e-10)
        for(metric in c('auc','brier')) {
          prior<-expected[expected$target==name&expected$metric==metric,'apparent']
          stopifnot(length(prior)==1)
          checks[[length(checks)+1L]]<-data.frame(model=name,metric=metric,
            difference=unname(metrics[metric,name]-prior))
        }
      }
      checks<-do.call(rbind,checks)
      write.csv(checks,file.path(share,'primary_replay_checks.csv'),row.names=FALSE)
      stopifnot(all(is.finite(checks$difference)),all(abs(checks$difference)<1e-8))
      models<-lapply(fits,report_model)
      saveRDS(models,file.path(private,'primary_prediction_parameters.rds'))
      for(name in names(models)) {
        write.csv(data.frame(term=names(models[[name]]$coefficients),coefficient=unname(models[[name]]$coefficients)),
          file.path(private,paste0(name,'_coefficients.csv')),row.names=FALSE)
        dput(models[[name]],file=file.path(private,paste0(name,'_prediction_specification.R')))
      }
      'SUCCESS: two primary model exports verified against saved apparent AUC/Brier.'
    },error=function(e) 'NOT_VERIFIED: primary export stopped or did not match. Descriptive outputs remain valid.',
    finally=setTimeLimit(cpu=Inf,elapsed=Inf,transient=FALSE))
    writeLines(model_status,file.path(share,'PRIMARY_MODEL_STATUS.txt'))
    files<-file.path(directory,c('hrs_reporting_patch.R','hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R',
      'hrs_analysis.R','hrs_prediction_validation.R','hrs_grip_change.R','hrs_grip_history.R','hrs_ml_extension.R'))
    write.csv(data.frame(file=basename(files),md5=unname(tools::md5sum(files))),file.path(share,'script_hashes.csv'),row.names=FALSE)
    writeLines(c(paste('R',getRversion()),paste('survival',if(requireNamespace('survival',quietly=TRUE))
      as.character(packageVersion('survival')) else 'unavailable')),file.path(share,'versions.txt'))
    writeLines(c('Only SEND_BACK contains the aggregate reporting package. Keep LOCAL_ONLY private.',
      'Counts are rounded to tens. Blocks with nonzero counts below ten are suppressed.',
      'Dates describe interview months, not confirmed grip-assessment dates.',
      'Follow-up summary uses event-interval midpoints and censoring times. It is not a reverse-KM estimate.',
      'Event-interval lower/upper summaries show the timing uncertainty separately.',
      'The five-year administrative horizon is unchanged. No new subgroup or fairness analysis.',
      'Reconstructed fold counts match saved rounded outer counts, not immutable historical fold identities.',
      'Primary parameters are a verified reconstruction only if PRIMARY_MODEL_STATUS reports SUCCESS.',
      'No neural/XGBoost parameters, bootstrap models or augmented association targets were recovered.',
      'No manuscript claim of completion is made until these outputs have been reviewed.'),file.path(share,'READ_ME.txt'))
    writeLines('SUCCESS: descriptive reporting complete. Read PRIMARY_MODEL_STATUS.txt separately.',status)
    message('Finished. Return only this folder: ',share)
    invisible(out)
  },error=function(e) {
    writeLines('INCOMPLETE: import, cohort or fold checks failed. Do not use partial outputs.',status)
    stop('Reporting stopped. Keep LOCAL_ONLY private. Existing analysis files were not changed.',call.=FALSE)
  })
}

if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  tryCatch(report_main(commandArgs(trailingOnly=TRUE),dirname(normalizePath(script))),
    error=function(e) {message('Reporting run stopped. Inspect the new SEND_BACK/STATUS.txt if present.');quit(status=1L)})
}
