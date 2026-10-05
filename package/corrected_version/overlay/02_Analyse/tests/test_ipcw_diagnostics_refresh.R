# Synthetic aggregate fixtures and mocked diagnostics_main. No HRS or model fits.
source('02_Analyse/scripts/hrs_ipcw_diagnostics_refresh.R')
directory<-normalizePath('02_Analyse/scripts');env<-diagnostics_refresh_load(directory)
models<-diagnostics_refresh_models();bands<-diagnostics_refresh_bands()
fixtures<-list(
  weight_diagnostics=data.frame(metric=c('G_min','G_median','weight_max','weight_median',
    'effective_known_sample_size_rounded_10'),value=c(.9,.95,1.1111,1.0526,1000)),
  calibration_groups=expand.grid(model=models,risk_band=bands,stringsAsFactors=FALSE),
  model_fit_summary=data.frame(model=models,AIC=100:103,mean_interval_log_score=seq(-1,-.7,.1),status='FIT_OK_APPARENT_ONLY'),
  survival_fit=expand.grid(model=models,year=1:5,stringsAsFactors=FALSE))
cal<-fixtures$calibration_groups
cal$n_rounded<-100;cal$events_rounded<-20;cal$known_survivors_rounded<-80
cal$predicted_risk<-.2;cal$observed_risk<-.21;cal$status<-'OK'
fixtures$calibration_groups<-cal
fixtures$survival_fit$predicted_survival<-.8;fixtures$survival_fit$turnbull_survival<-.79
for(name in names(fixtures)) fixtures[[name]]<-fixtures[[name]][diagnostics_refresh_columns()[[name]]]
diagnostics_refresh_validate(fixtures,env)
sparse<-fixtures
for(name in c('n_rounded','events_rounded','known_survivors_rounded')) sparse$calibration_groups[1,name]<-'SUPPRESSED'
sparse$calibration_groups[1,c('predicted_risk','observed_risk')]<-NA_real_
sparse$calibration_groups$status[1]<-'SUPPRESSED_SPARSE'
diagnostics_refresh_validate(sparse,env)
bad<-sparse;bad$calibration_groups$status[1]<-'OK'
stopifnot(inherits(try(diagnostics_refresh_validate(bad,env),silent=TRUE),'try-error'))
changed<-fixtures;changed$calibration_groups$observed_risk<-.22
changed$weight_diagnostics$value[1]<-.89
stopifnot(nrow(diagnostics_refresh_compare(changed,fixtures,env))==3L)
bad<-fixtures;bad$calibration_groups$predicted_risk[1]<-.3
stopifnot(inherits(try(diagnostics_refresh_compare(bad,fixtures,env),silent=TRUE),'try-error'))
bad<-fixtures;bad$survival_fit<-bad$survival_fit[-1,]
stopifnot(inherits(try(diagnostics_refresh_validate(bad,env),silent=TRUE),'try-error'))

base<-tempfile('diagnostics_refresh_test_');dir.create(base)
for(name in c('data','out','old')) dir.create(file.path(base,name))
old<-file.path(base,'old')
for(name in names(fixtures)) write.csv(fixtures[[name]],file.path(old,paste0(name,'.csv')),row.names=FALSE)
writeLines('SUCCESS: synthetic historical diagnostics.',file.path(old,'STATUS.txt'))
writeLines(c(paste('R:',getRversion()),paste('survival:',utils::packageVersion('survival'))),file.path(old,'READ_ME.txt'))
source_names<-diagnostics_refresh_source_names()
legacy<-normalizePath('03_Dokumentation/IPCW_Korrektur_2026-10-04/legacy_sources/hrs_prediction_validation.R')
paths<-file.path(directory,source_names);paths[source_names=='hrs_prediction_validation.R']<-legacy
writeLines(paste(diagnostics_refresh_sha256(paths),paste0('/invented/old/',source_names),sep='  '),file.path(old,'source_sha256.txt'))
args<-c(file.path(base,'data'),file.path(base,'out'),old,
  normalizePath('03_Dokumentation/IPCW_Rueckgabe_2026-10-04/SEND_BACK'))
invisible(diagnostics_refresh_preflight(args,directory))
real_load<-diagnostics_refresh_load;calls<-0L;mode<-'valid'
diagnostics_refresh_load<-function(directory) {
  e<-real_load(directory)
  e$diagnostics_main<-function(args,script_dir) {
    calls<<-calls+1L
    stopifnot(length(args)==2L,basename(args[2])=='LOCAL_ONLY',identical(script_dir,directory))
    out<-file.path(args[2],'diagnostics_v1');dir.create(out)
    value<-changed
    if(mode=='mismatch') value$model_fit_summary$AIC[1]<-999
    if(mode=='private_error') stop('SYNTHETIC_PRIVATE_MARKER')
    if(mode=='extra_column') value$calibration_groups$PERSON_ID<-'SYNTHETIC_PRIVATE_MARKER'
    for(name in names(value)) write.csv(value[[name]],file.path(out,paste0(name,'.csv')),row.names=FALSE)
    writeLines(if(mode=='partial') 'PARTIAL: synthetic' else 'SUCCESS: synthetic',file.path(out,'STATUS.txt'))
    writeLines('SYNTHETIC_PRIVATE_MARKER',file.path(out,'source_sha256.txt'))
    if(mode=='changed_input') cat('\n',file=file.path(old,'READ_ME.txt'),append=TRUE)
  }
  e
}
for(current in c('valid','mismatch','partial','extra_column','private_error','changed_input')) {
  mode<-current;before<-calls;out<-file.path(base,paste0('out_',mode));dir.create(out);args[2]<-out
  result<-try(diagnostics_refresh_main(args,directory),silent=TRUE)
  stopifnot(calls==before+1L)
  run<-list.dirs(out,recursive=FALSE,full.names=TRUE);stopifnot(length(run)==1L)
  share<-file.path(run,'SEND_BACK');status<-readLines(file.path(share,'STATUS.txt'))
  if(mode=='valid') {
    stopifnot(!inherits(result,'try-error'),startsWith(status[1],'SUCCESS:'),
      setequal(list.files(share),c(paste0(names(fixtures),'.csv'),'REPLAY_CHECKS.csv',
        'source_hashes.csv','versions.txt','READ_ME.txt','STATUS.txt')))
  } else stopifnot(inherits(result,'try-error'),startsWith(status[1],'INCOMPLETE:'),
    identical(list.files(share),'STATUS.txt'))
  text<-paste(vapply(list.files(share,full.names=TRUE),function(p) paste(readLines(p,warn=FALSE),collapse='\n'),''),collapse='\n')
  stopifnot(!grepl('SYNTHETIC_PRIVATE_MARKER',text,fixed=TRUE),!grepl(base,text,fixed=TRUE))
}
cat('PASS: historical provenance/schema checks, invariant replay, permitted IPCW changes, exact9file export and fail-closed lifecycle. Zero HRS reads or model fits.\n')
