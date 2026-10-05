# User-run IPCW impact audit. No model training, tuning or bootstrap fitting.
audit_legacy <- function(directory) {
  e<-new.env(parent=baseenv())
  for(name in c('hrs_prediction_validation.R','hrs_ml_extension.R'))
    sys.source(file.path(directory,name),envir=e)
  e
}

audit_times <- function(d,position) {
  event<-is.finite(d$hi);time<-d$lo
  time[event]<-switch(position,lower=d$lo[event],midpoint=(d$lo[event]+d$hi[event])/2,
    upper=d$hi[event])
  list(time=time,event=event)
}

audit_ties <- function(d,position) {
  t<-audit_times(d,position)
  any(t$time[t$event] %in% t$time[!t$event & t$time<5])
}

audit_compare <- function(train,test,position,previous,current,ml=FALSE) {
  before<-tryCatch(if(ml) previous(train,test,position) else previous(test,position),
    error=function(e) NULL)
  after<-tryCatch(if(ml) current(train,test,position) else current(test,position),
    error=function(e) NULL)
  valid<-!is.null(before) && !is.null(after) && identical(before$y,after$y) &&
    all(is.finite(before$weight)) && all(is.finite(after$weight))
  delta<-if(valid) abs(after$weight-before$weight) else NA_real_
  data.frame(training_ties_present=audit_ties(train,position),
    weight_status=if(!valid) 'WEIGHT_CHECK_FAILED' else if(any(delta>0)) 'CHANGED' else 'IDENTICAL',
    affected=if(valid) sum(delta>0) else NA_integer_,
    max_difference=if(valid) max(delta) else NA_real_)
}

audit_counts <- function(x) {
  if(any(x>0 & x<10,na.rm=TRUE)) return(rep('SUPPRESSED_BLOCK',length(x)))
  ifelse(is.na(x),'NOT_AVAILABLE',as.character(10*round(x/10)))
}

audit_scenarios <- function(z) {
  names<-c('primary','core_month','agree_dates_only','month_start','month_end',
    'interview_first','exclude_conflicts','ndi_only','lognormal')
  result<-setNames(vector('list',length(names)),names)
  for(name in names) {
    origin<-if(name=='core_month') z$core_month else z$origin
    offset<-if(name=='month_start') 0 else if(name=='month_end') 1 else .5
    strategy<-if(name %in% c('interview_first','exclude_conflicts','ndi_only')) name else 'ndi_first'
    f<-make_followup(z$tracker,origin+offset,strategy)
    timing<-!is.na(origin)&!is.na(z$early)&z$early<origin
    if(name=='agree_dates_only') timing<-timing & !is.na(z$core_month)&z$core_month==z$origin
    result[[name]]<-f[timing & z$covok & f$reason=='included',c('lo','hi'),drop=FALSE]
  }
  result
}

audit_main <- function(args,directory) {
  if(!length(args) %in% 2:3) stop('Use DATA_DIR OUTPUT_PARENT [PRIMARY_PARAMETERS_RDS].')
  for(name in c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R','hrs_analysis.R',
    'hrs_prediction_validation.R','hrs_grip_change.R','hrs_grip_history.R','hrs_ml_extension.R',
    'hrs_reporting_patch.R')) source(file.path(directory,name),local=.GlobalEnv)
  root<-dirname(dirname(directory))
  old<-audit_legacy(file.path(root,'03_Dokumentation','IPCW_Korrektur_2026-10-04','legacy_sources'))
  out<-file.path(args[2],paste0('ipcw_impact_audit_',format(Sys.time(),'%Y%m%d_%H%M%S'),'_',Sys.getpid()))
  if(file.exists(out)||!dir.create(out,recursive=TRUE)) stop('Cannot create a fresh output folder.')
  private<-file.path(out,'LOCAL_ONLY');share<-file.path(out,'SEND_BACK')
  dir.create(private);dir.create(share)
  status<-file.path(share,'STATUS.txt');writeLines('INCOMPLETE',status)
  tryCatch({
    message('Importing locally and reconstructing the approved cohort. No model training.')
    imported<-stage2_main(c(args[1],file.path(private,'import_checks')),file.path(directory,'hrs_feasibility.R'))
    p<-prepare_change_data(imported);d<-p$data
    stopifnot(nrow(d)==6307L,!anyNA(d),nrow(p$tracker)==nrow(d))
    scenarios<-audit_scenarios(p$flow_context)
    stopifnot(identical(unname(as.matrix(scenarios$primary)),unname(as.matrix(d[c('lo','hi')]))))
    rows<-list()
    add<-function(scope,position,x,fraction=NA_real_,fold=NA_integer_) {
      rows[[length(rows)+1L]]<<-data.frame(scope=scope,position=position,fraction=fraction,fold=fold,x)
    }
    for(name in names(scenarios)) for(position in c('lower','midpoint','upper')) {
      x<-scenarios[[name]]
      add(name,position,audit_compare(x,x,position,old$horizon_weights,horizon_weights))
    }
    cfg<-ml_config('full');fold<-ml_folds(d,cfg$outer,cfg$seed)
    expected<-read.csv(file.path(root,'01_Aktuelles_Manuskript','manuskript_v2',
      'reporting_patch_20261004','fold_counts.csv'))
    expected<-expected[expected$role=='outer_train',]
    for(fraction in cfg$fractions) for(k in seq_len(cfg$outer)) {
      train<-d[fold!=k,,drop=FALSE];test<-d[fold==k,,drop=FALSE]
      set.seed(cfg$seed+k);groups<-sample(unique(train$household))
      groups<-head(groups,max(cfg$inner*2,ceiling(length(groups)*fraction)))
      train<-train[train$household %in% groups,,drop=FALSE]
      check<-expected[expected$fraction==fraction & expected$outer==k,]
      stopifnot(nrow(check)==1,check$participants==10*round(nrow(train)/10),
        check$events==10*round(sum(is.finite(train$hi))/10))
      for(position in c('lower','midpoint','upper'))
        add('ml_outer',position,audit_compare(train,test,position,old$ml_weights,ml_weights,TRUE),fraction,k)
    }
    result<-do.call(rbind,rows)
    write.csv(result,file.path(private,'weight_comparison_exact.csv'),row.names=FALSE)
    result$affected<-audit_counts(result$affected)
    # Release technical status only, not event times, identifiers or small-cell maxima.
    result$max_difference<-NULL
    write.csv(result,file.path(share,'weight_comparison.csv'),row.names=FALSE)
    primary<-result[result$scope=='primary' & result$position=='midpoint',]
    primary_status<-if(primary$weight_status=='WEIGHT_CHECK_FAILED') 'UNRESOLVED' else
      if(!primary$training_ties_present && primary$weight_status=='IDENTICAL')
        'NO_TIES: this tie correction cannot change primary midpoint weights, including household resamples.' else
        'TIES_PRESENT: primary validation requires impact assessment. No bootstrap results recomputed.'
    writeLines(primary_status,file.path(share,'PRIMARY_VALIDATION_STATUS.txt'))
    # Optional frozen parameters from the earlier user-run reporting patch. Prediction only.
    replay_status<-'NOT_REQUESTED: no primary parameter file supplied.'
    if(length(args)==3L) {
      replay_status<-tryCatch({
        models<-readRDS(args[3]);stopifnot(setequal(names(models),c('current','prior')))
        reference<-read.csv(file.path(root,'01_Aktuelles_Manuskript','manuskript_v2','table2_source.csv'))
        metrics<-list()
        for(name in names(models)) {
          risk<-report_risk(models[[name]],d)
          stopifnot(length(risk)==nrow(d),all(is.finite(risk)),all(risk>=0 & risk<=1))
          for(position in c('lower','midpoint','upper')) {
            value<-function(fun) {
              w<-fun(d,position)
              c(auc=weighted_auc(risk,w$y,w$weight),brier=sum(w$weight*(w$y-risk)^2)/nrow(d))
            }
            a<-value(old$horizon_weights);b<-value(horizon_weights)
            if(position=='midpoint') for(metric in names(a)) {
              saved<-reference[reference$target==name & reference$metric==metric,'apparent']
              stopifnot(length(saved)==1,abs(a[metric]-saved)<1e-8)
            }
            metrics[[length(metrics)+1L]]<-data.frame(model=name,position=position,metric=names(a),
              legacy=unname(a),corrected=unname(b),difference=unname(b-a))
          }
        }
        write.csv(do.call(rbind,metrics),file.path(share,'primary_apparent_reweighting.csv'),row.names=FALSE)
        'SUCCESS: legacy apparent AUC/Brier reproduced. Corrected values use frozen parameters, not new fits.'
      },error=function(e) 'NOT_VERIFIED: parameter replay failed. Weight audit remains available.')
    }
    writeLines(replay_status,file.path(share,'PRIMARY_REPLAY_STATUS.txt'))
    files<-file.path(directory,c('hrs_ipcw_impact_audit.R','hrs_feasibility.R','hrs_stage2.R',
      'hrs_source_hierarchy.R','hrs_analysis.R','hrs_prediction_validation.R','hrs_grip_change.R',
      'hrs_grip_history.R','hrs_ml_extension.R','hrs_reporting_patch.R'))
    files<-c(files,file.path(root,'03_Dokumentation','IPCW_Korrektur_2026-10-04','legacy_sources',
      c('hrs_prediction_validation.R','hrs_ml_extension.R')))
    write.csv(data.frame(file=sub(paste0(root,'/'),'',files,fixed=TRUE),md5=unname(tools::md5sum(files))),
      file.path(share,'script_hashes.csv'),row.names=FALSE)
    writeLines(c(paste('R',getRversion()),capture.output(RNGkind())),file.path(share,'versions.txt'))
    writeLines(c('Return only SEND_BACK. LOCAL_ONLY stays private. No person-level exports in SEND_BACK.',
      'No model training, tuning, synthesis or bootstrap fitting was performed.',
      'Nine existing data definitions and three event-time positions were checked.',
      'ML checks reconstruct the 15 original outer training/evaluation combinations, at all three positions.',
      'Fold reconstruction matches historical rounded counts, not independently saved historical identities.',
      'IDENTICAL applies to checked weights. A no-tie full cohort additionally excludes ties in row resamples.',
      'CHANGED identifies affected weights, not a measured change in a model conclusion.',
      'Counts are rounded to tens. A block containing a nonzero count below ten is fully suppressed.',
      'Optional apparent reweighting does not update optimism correction, confidence intervals or ML metrics.',
      'Simulation results are checked separately with synthetic data. No full TRIPOD completion is claimed.'),
      file.path(share,'READ_ME.txt'))
    writeLines(if(any(result$weight_status=='WEIGHT_CHECK_FAILED'))
      'COMPLETE_WITH_WEIGHT_FAILURE: inspect statuses. No numerical clearance.' else
      'COMPLETE: weight audit finished. Inspect comparison and primary validation status.',status)
    message('Finished. Return only this folder: ',share)
    invisible(out)
  },error=function(e) {
    writeLines('INCOMPLETE: cohort, import or fold checks failed. Do not use partial outputs.',status)
    stop('Audit stopped. Keep LOCAL_ONLY private. Existing results were not changed.',call.=FALSE)
  })
}

if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  tryCatch(audit_main(commandArgs(trailingOnly=TRUE),dirname(normalizePath(script))),
    error=function(e) {message('Audit stopped. Inspect the new SEND_BACK/STATUS.txt if present.');quit(status=1L)})
}
