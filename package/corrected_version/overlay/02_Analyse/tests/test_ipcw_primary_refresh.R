# Invented aggregates and a mocked analysis_main only. No HRS data or model fits.
source('02_Analyse/scripts/hrs_ipcw_primary_refresh.R')
directory<-normalizePath('02_Analyse/scripts')
fixture<-function() {
  base<-tempfile('primary_refresh_test_');dir.create(base)
  for(name in c('data','out','audit')) dir.create(file.path(base,name))
  audit<-file.path(base,'audit')
  ref<-expand.grid(model=c('current','prior'),position=c('lower','midpoint','upper'),
    metric=c('auc','brier'),stringsAsFactors=FALSE)
  ref$legacy<-ifelse(ref$metric=='auc',.8,.1)+ifelse(ref$model=='prior',.001,0)
  ref$corrected<-ref$legacy
  ref$difference<-0
  write.csv(ref,file.path(audit,'primary_apparent_reweighting.csv'),row.names=FALSE)
  writeLines('COMPLETE: invented weight audit',file.path(audit,'STATUS.txt'))
  writeLines('SUCCESS: invented frozen replay',file.path(audit,'PRIMARY_REPLAY_STATUS.txt'))
  writeLines(paste('R',getRversion()),file.path(audit,'versions.txt'))
  names<-primary_refresh_source_names()
  hashes<-tools::md5sum(file.path(directory,names))
  write.csv(data.frame(file=paste0('02_Analyse/scripts/',names),md5=unname(hashes)),
    file.path(audit,'script_hashes.csv'),row.names=FALSE)
  list(base=base,args=c(file.path(base,'data'),file.path(base,'out'),audit),reference=ref)
}
calls<-0L;mode<-'valid'
real_loader<-primary_refresh_load
primary_refresh_load<-function(directory) {
  env<-real_loader(directory)
  env$analysis_main<-function(args,script_dir,extended=FALSE) {
    calls<<-calls+1L
    stopifnot(identical(unname(args[3:4]),c('200','50')),isTRUE(extended),identical(script_dir,directory))
    # No random draw may intervene after the production seed is set.
    actual<-runif(1);set.seed(20260926);stopifnot(identical(actual,runif(1)))
    if(mode=='thrown_private_error') stop('SYNTHETIC_PRIVATE_MARKER failure')
    out<-file.path(args[2],'validation_v2');dir.create(out,recursive=TRUE)
    z<-expand.grid(metric=c('brier','auc','calibration_intercept','calibration_slope','interval_log_score'),
      target=c('current','prior','prior_minus_current'),stringsAsFactors=FALSE)
    z$apparent<-ifelse(z$metric=='auc',.8,ifelse(z$metric=='brier',.1,0))+
      ifelse(z$target=='prior',.001,0)
    z$apparent[z$target=='prior_minus_current']<-.001
    z$optimism_corrected<-z$apparent;z$ci_lower<-z$apparent-.01;z$ci_upper<-z$apparent+.01
    z$outer_success<-200L;z$nested_success<-200L;z$status<-'OK'
    if(mode=='unstable') z$status[1]<-'UNSTABLE'
    if(mode=='missing_validation') z<-z[-1,]
    if(mode=='extra_column') z$PERSON_ID<-'SYNTHETIC_PRIVATE_MARKER'
    write.csv(z,file.path(out,'validated_metrics.csv'),row.names=FALSE)
    s<-expand.grid(scenario=primary_refresh_scenarios(),censor_time_position=c('lower','midpoint','upper'),
      model=c('current','prior'),metric=unique(z$metric),stringsAsFactors=FALSE)
    s$apparent<-ifelse(s$metric=='auc',.8,ifelse(s$metric=='brier',.1,0))+ifelse(s$model=='prior',.001,0)
    if(mode=='reference_mismatch') s$apparent[1]<-123
    if(mode=='missing_sensitivity') s<-s[-1,]
    write.csv(s,file.path(out,'sensitivity_metrics.csv'),row.names=FALSE)
    m<-data.frame(scenario=primary_refresh_scenarios(),status='FIT_OK',current_score=-1,prior_score=-.9,
      delta_score=.1,corrected_delta=NA_real_,bootstrap_success=NA_real_)
    if(mode=='failed_scenario') m$status[2]<-'MODEL_OR_VALIDATION_FAILED'
    write.csv(m,file.path(out,'model_comparison.csv'),row.names=FALSE)
    writeLines(if(mode=='partial_status') 'PARTIAL: invented' else 'SUCCESS: invented',file.path(out,'STATUS.txt'))
    writeLines('SYNTHETIC_PRIVATE_MARKER',file.path(args[2],'DO_NOT_SHARE.txt'))
    cat('SYNTHETIC_PRIVATE_MARKER stdout\n');message('SYNTHETIC_PRIVATE_MARKER stderr')
  }
  env
}
for(testmode in c('valid','unstable','missing_validation','extra_column','reference_mismatch',
  'missing_sensitivity','failed_scenario','partial_status','thrown_private_error')) {
  mode<-testmode;f<-fixture();before<-calls
  result<-try(primary_refresh_main(f$args,directory),silent=TRUE)
  if(calls!=before+1L) cat(as.character(result),'\n')
  stopifnot(calls==before+1L)
  folders<-list.dirs(f$args[2],recursive=FALSE,full.names=TRUE);stopifnot(length(folders)==1)
  share<-file.path(folders,'SEND_BACK');status<-readLines(file.path(share,'STATUS.txt'))
  if(mode=='valid') {
    if(inherits(result,'try-error')) cat(readLines(file.path(folders,'LOCAL_ONLY','analysis.log')),sep='\n')
    stopifnot(!inherits(result,'try-error'),startsWith(status[1],'SUCCESS:'),
      nrow(read.csv(file.path(share,'validated_metrics.csv')))==15L,
      nrow(read.csv(file.path(share,'sensitivity_metrics.csv')))==270L,
      nrow(read.csv(file.path(share,'margin_sensitivity.csv')))==18L,
      setequal(list.files(share),c('validated_metrics.csv','sensitivity_metrics.csv',
        'model_comparison.csv','margin_sensitivity.csv','versions.txt','source_hashes.csv','STATUS.txt')))
  } else stopifnot(inherits(result,'try-error'),startsWith(status[1],'INCOMPLETE:'),
    identical(list.files(share),'STATUS.txt'))
  shared_text<-paste(vapply(list.files(share,full.names=TRUE),function(p) paste(readLines(p,warn=FALSE),collapse='\n'),''),collapse='\n')
  stopifnot(!grepl('SYNTHETIC_PRIVATE_MARKER',shared_text,fixed=TRUE),
    file.exists(file.path(folders,'LOCAL_ONLY','analysis.log')))
}
for(problem in c('bad_reference','bad_version','bad_hash','bad_audit_status')) {
  f<-fixture();before<-calls
  if(problem=='bad_reference') writeLines('model,position,metric,legacy,corrected,difference',
    file.path(f$args[3],'primary_apparent_reweighting.csv'))
  if(problem=='bad_version') writeLines('R 0.0.0',file.path(f$args[3],'versions.txt'))
  if(problem=='bad_hash') {
    h<-read.csv(file.path(f$args[3],'script_hashes.csv'));h$md5[1]<-'wrong'
    write.csv(h,file.path(f$args[3],'script_hashes.csv'),row.names=FALSE)
  }
  if(problem=='bad_audit_status') writeLines('INCOMPLETE',file.path(f$args[3],'STATUS.txt'))
  stopifnot(inherits(try(primary_refresh_main(f$args,directory),silent=TRUE),'try-error'),
    calls==before,length(list.files(f$args[2]))==0L)
}
cat('PASS: primary-only dispatch, exact seed/200/50, 15/270/9 rows, reference and source checks, fail-closed sharing, 18 margin rows. No model fits.\n')
