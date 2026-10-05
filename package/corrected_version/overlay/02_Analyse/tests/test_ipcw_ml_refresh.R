# Synthetic aggregates and providers only. No HRS input or fitted models.
source('02_Analyse/scripts/hrs_ipcw_ml_refresh.R')
directory<-normalizePath('02_Analyse/scripts')
env<-ml_refresh_load(directory)
cfg<-ml_refresh_configs(env)$full
tuning<-expand.grid(fraction=cfg$fractions,fold=seq_len(cfg$outer),model=cfg$models,
  target=c('current','prior'),arm=c('original','augmented'),stringsAsFactors=FALSE)
tuning$ratio<-ifelse(tuning$arm=='original',0,.5)
tuning$training_n_rounded<-100L;tuning$training_events_rounded<-30L
tuning$inner_log_score<--1;tuning$failed_fits<-0L
tuning$attempted_fits<-vapply(seq_len(nrow(tuning)),function(i)
  cfg$inner*length(env$ml_grid(tuning$model[i],FALSE))*if(tuning$arm[i]=='original') 1 else length(cfg$ratios),0)
tuning$parameter<-vapply(tuning$model,function(model) {
  p<-env$ml_grid(model,FALSE)[[1]]
  paste(names(p),unlist(p),sep='=',collapse=';')
},'')
tuning<-tuning[ml_refresh_columns()$tuning]
ml_refresh_check_tuning(tuning,cfg,env)
inference<-tuning[tuning$fraction==1,,drop=FALSE]
ml_refresh_match_selection(tuning,inference)
bad<-inference;bad$ratio[bad$arm=='augmented'][1]<-1
stopifnot(inherits(try(ml_refresh_match_selection(tuning,bad),silent=TRUE),'try-error'))
bad<-tuning;bad$parameter[bad$model=='neural'][1]<-'hidden=8;lambda=system(1);maxit=600'
stopifnot(inherits(try(ml_refresh_check_tuning(bad,cfg,env),silent=TRUE),'try-error'))
bad<-tuning[-1,]
stopifnot(inherits(try(ml_refresh_check_tuning(bad,cfg,env),silent=TRUE),'try-error'))

folder<-tempfile('ml_refresh_provider_');dir.create(folder)
fits<-0L
env$ml_fit<-function(...) {fits<<-fits+1L;list()}
env$ml_predict<-function(fit,d) list(risk=rep(.25,nrow(d)),score=rep(-1,nrow(d)))
cache<-ml_refresh_cache(env,folder,'SYNTHETIC_SOURCE_FINGERPRINT')
d<-data.frame(household=1:3,lo=c(1,2,5),hi=c(1.1,Inf,Inf))
p<-list(formula=~1,levels=list(),columns='x',center=0,scale=1,spline_terms='x')
args<-list(train=d,aug=d,test=d,model='weibull',target='current',arm='original',
  parameter=list(),seed=1,preprocess=p,fraction=1,fold=1)
a<-do.call(cache$provider,args);b<-do.call(cache$provider,args)
stopifnot(identical(a,b),fits==1L,cache$counts()$fits==1L,cache$counts()$hits==1L)
bad<-args;bad$test$lo[1]<-1.01
stopifnot(inherits(try(do.call(cache$provider,bad),silent=TRUE),'try-error'),fits==1L)
cache$freeze()
bad<-args;bad$fold<-2
stopifnot(inherits(try(do.call(cache$provider,bad),silent=TRUE),'try-error'),fits==1L)

# Numeric replay tolerates only serialization-level drift, not changed estimates.
x<-data.frame(fraction=1,model='weibull',target='current',arm='original',value=.8)
ml_refresh_compare_table(x,x,'synthetic',c('fraction','model','target','arm'))
y<-x;y$value<-.800001
stopifnot(inherits(try(ml_refresh_compare_table(x,y,'synthetic',
  c('fraction','model','target','arm')),silent=TRUE),'try-error'))
cat('PASS: complete stored-selection grid, mismatches/code-like parameters rejected, cache hit/integrity/freeze and strict legacy numeric replay. No model fits.\n')

# Entire client flow with invented benchmark outputs and mocked fitting/import.
# The real preflight, selection provider, cache, legacy gates and safe exports run.
synthetic_result<-function(cfg,tuning,corrected=FALSE) {
  e<-ml_refresh_expected(cfg);cols<-ml_refresh_columns();delta<-if(corrected) .00001 else 0
  m<-e$metrics
  for(name in setdiff(cols$metrics,c(names(m),'uncertainty','status'))) m[[name]]<-0
  m$auc<-.8+delta;m$auc_lower<-.79+delta;m$auc_upper<-.81+delta
  m$brier<-.1+delta;m$brier_lower<-.09+delta;m$brier_upper<-.11+delta
  m$interval_log_score<--1;m$log_score_lower<--1.1;m$log_score_upper<--.9
  m$calibration_intercept<-0;m$calibration_slope<-1
  m$auc_lower_position<-m$auc;m$auc_upper_position<-m$auc
  m$brier_lower_position<-m$brier;m$brier_upper_position<-m$brier
  m$uncertainty<-'conditional_OOF_household_bootstrap';m$status<-'OK'
  pair<-e$paired;pair$difference<-delta;pair$ci_lower<-delta-.01;pair$ci_upper<-delta+.01
  pair$uncertainty<-'conditional_OOF_household_bootstrap'
  cal<-e$calibration;cal$n_rounded<-20;cal$predicted<-.25;cal$observed<-.1+delta;cal$status<-'OK'
  diag<-e$diagnostics;diag$standardized_mean_difference<-0
  r<-list(metrics=m[cols$metrics],paired=pair[cols$paired],calibration=cal[cols$calibration],
    tuning=tuning,diagnostics=diag[cols$diagnostics])
  if(isTRUE(cfg$export_bootstrap)) {
    boot<-merge(e$paired[e$paired$metric %in% c('auc','brier'),],data.frame(replicate=seq_len(cfg$ci)),by=NULL)
    boot$difference<-sin(boot$replicate)*.0001+delta
    r$bootstrap_differences<-boot[cols$bootstrap_differences]
  }
  r
}
configs<-ml_refresh_configs(env)
synthetic<-list(full=synthetic_result(configs$full,tuning),
  inference=synthetic_result(configs$inference,inference))
for(role in names(synthetic)) ml_refresh_validate_result(synthetic[[role]],configs[[role]],synthetic[[role]]$tuning,env)
base<-tempfile('ml_refresh_main_');dir.create(base)
for(name in c('data','out','full','inference')) dir.create(file.path(base,name))
old_names<-setdiff(ml_refresh_source_names(),c('hrs_ml_equivalence.R','hrs_ml_point_figures.R'))
legacy<-normalizePath('03_Dokumentation/IPCW_Korrektur_2026-10-04/legacy_sources')
actual_versions<-data.frame(component=c('R','survival','rpart','xgboost'),version=c(as.character(getRversion()),
  vapply(c('survival','rpart','xgboost'),function(p) as.character(utils::packageVersion(p)),'')))
for(role in names(synthetic)) {
  folder<-file.path(base,role)
  writeLines('SUCCESS: invented benchmark only.',file.path(folder,'STATUS.txt'))
  dput(configs[[role]],file=file.path(folder,'CONFIG.R'))
  write.csv(actual_versions,file.path(folder,'versions.csv'),row.names=FALSE)
  h<-vapply(old_names,function(name) unname(tools::md5sum(file.path(
    if(name %in% c('hrs_ml_extension.R','hrs_prediction_validation.R')) legacy else directory,name))),'')
  if(role=='full') h[old_names=='hrs_ml_extension.R']<-unname(tools::md5sum(
    '04_Archiv/Berichte/ml_critical_audit_v1/before/hrs_ml_extension.R'))
  write.csv(data.frame(file=old_names,md5=h),file.path(folder,'source_hashes.csv'),row.names=FALSE)
  for(name in names(synthetic[[role]])) write.csv(synthetic[[role]][[name]],
    file.path(folder,paste0('benchmark_',name,'.csv')),row.names=FALSE)
}
args<-c(file.path(base,'data'),file.path(base,'out'),file.path(base,'full'),file.path(base,'inference'),
  normalizePath('03_Dokumentation/IPCW_Rueckgabe_2026-10-04/SEND_BACK'))
checked<-ml_refresh_preflight(args,directory)
stopifnot(nrow(checked$history$full$tuning)==180L,nrow(checked$history$inference$tuning)==60L)
# Distinct historical full/inference sources must remain strictly associated.
for(role in c('full','inference')) {
  manifest<-file.path(base,role,'source_hashes.csv');saved_manifest<-read.csv(manifest)
  wrong<-saved_manifest
  wrong$md5[wrong$file=='hrs_ml_extension.R']<-if(role=='full')
    unname(tools::md5sum(file.path(legacy,'hrs_ml_extension.R'))) else
    unname(tools::md5sum('04_Archiv/Berichte/ml_critical_audit_v1/before/hrs_ml_extension.R'))
  write.csv(wrong,manifest,row.names=FALSE)
  err<-try(ml_refresh_preflight(args,directory),silent=TRUE)
  stopifnot(inherits(err,'try-error'),grepl(paste('Historical source hash mismatch:',role,'hrs_ml_extension.R'),as.character(err),fixed=TRUE))
  write.csv(saved_manifest,manifest,row.names=FALSE)
}
stopifnot('historical_full_source/hrs_ml_extension.R' %in% checked$hashes$file)
cat('PASS: distinct historical full/inference sources accepted, swapped sources rejected and full source tracked.\n')
bad_config<-file.path(base,'inference','CONFIG.R');saved<-readLines(bad_config)
writeLines('stop("SYNTHETIC_ARBITRARY_CODE_MUST_NOT_EXECUTE")',bad_config)
err<-try(ml_refresh_preflight(args,directory),silent=TRUE)
stopifnot(inherits(err,'try-error'),!grepl('SYNTHETIC_ARBITRARY_CODE_MUST_NOT_EXECUTE',as.character(err),fixed=TRUE))
writeLines(saved,bad_config)

real_load<-ml_refresh_load;mode<-'success';dispatch<-list();fit_count<-0L
ml_refresh_import<-function(...) data.frame(row=seq_len(500L))
ml_refresh_load<-function(directory) {
  e<-real_load(directory)
  e$ml_fit<-function(...) {fit_count<<-fit_count+1L;list()}
  e$ml_predict<-function(fit,d) list(risk=rep(.25,nrow(d)),score=rep(-1,nrow(d)))
  e$ml_render_person_points<-function(...) {
    grDevices::pdf(file.path(base,'PRIVATE_failed_figure.pdf'))
    stop('SYNTHETIC_PRIVATE_MARKER optional renderer')
  }
  e$ml_benchmark<-function(d,cfg,out=NULL,selection_provider=NULL,prediction_provider=NULL,weight_function=e$ml_weights) {
    role<-if(length(cfg$fractions)==1L) 'inference' else 'full'
    corrected<-identical(weight_function,e$ml_weights)
    dispatch[[length(dispatch)+1L]]<<-c(role=role,weight=if(corrected) 'corrected' else 'legacy')
    if(mode=='private_error') stop('SYNTHETIC_PRIVATE_MARKER from invented fit')
    selected<-synthetic[[role]]$tuning
    train<-data.frame(household=1:100,lo=1,hi=c(rep(2,30),rep(Inf,70)))
    for(i in seq_len(nrow(selected))) {
      z<-selected[i,];seed<-cfg$seed+1000*z$fold
      chosen<-selection_provider(train,z$model,z$target,z$arm,cfg,rep(1:2,50),seed,z$fraction,z$fold)
      test<-train;rownames(test)<-as.character((z$fold-1L)*100L+seq_len(100L))
      prediction_provider(train,train,test,z$model,z$target,z$arm,chosen$parameter,seed,p,z$fraction,z$fold)
    }
    result<-synthetic_result(cfg,selected,corrected)
    if(mode=='replay_mismatch'&&!corrected) result$metrics$auc[1]<-result$metrics$auc[1]+.001
    for(name in names(result)) write.csv(result[[name]],file.path(out,paste0('benchmark_',name,'.csv')),row.names=FALSE)
    if(mode=='changed_input') cat('\n',file=file.path(base,'full','STATUS.txt'),append=TRUE)
    result
  }
  e
}
before_devices<-grDevices::dev.list()
for(current in c('success','replay_mismatch','private_error','changed_input')) {
  mode<-current;dispatch<-list();fit_count<-0L
  output<-file.path(base,paste0('out_',mode));dir.create(output);args[2]<-output
  result<-try(ml_refresh_main(args,directory),silent=TRUE)
  folders<-list.dirs(output,recursive=FALSE,full.names=TRUE);stopifnot(length(folders)==1L)
  share<-file.path(folders,'SEND_BACK');status<-readLines(file.path(share,'STATUS.txt'))
  if(mode=='success') {
    if(inherits(result,'try-error')) cat(readLines(file.path(folders,'LOCAL_ONLY','recovery.log')),sep='\n')
    stopifnot(!inherits(result,'try-error'),startsWith(status[1],'SUCCESS:'),fit_count==180L,
      identical(vapply(dispatch,function(z) paste(z,collapse='_'),''),
        c('full_legacy','inference_legacy','full_corrected','inference_corrected')),
      identical(grDevices::dev.list(),before_devices))
    expected<-c('STATUS.txt','REPLAY_CHECKS.csv','source_hashes.csv','versions.csv','recovery_metadata.csv','READ_ME.txt',
      paste0('full/',c('benchmark_metrics.csv','benchmark_paired.csv','benchmark_calibration.csv',
        'historical_selected_tuning.csv','benchmark_diagnostics.csv')),
      paste0('inference/',c('benchmark_metrics.csv','benchmark_paired.csv','benchmark_calibration.csv',
        'historical_selected_tuning.csv','benchmark_diagnostics.csv','benchmark_bootstrap_differences.csv')),
      'equivalence/conditional_tost.csv','equivalence/margin_sensitivity.csv')
    stopifnot(setequal(list.files(share,recursive=TRUE),expected),
      nrow(read.csv(file.path(share,'inference','benchmark_bootstrap_differences.csv')))==200000L,
      nrow(read.csv(file.path(share,'REPLAY_CHECKS.csv')))==11L)
  } else stopifnot(inherits(result,'try-error'),startsWith(status[1],'INCOMPLETE:'),
    identical(list.files(share),'STATUS.txt'))
  text<-paste(vapply(list.files(share,full.names=TRUE,recursive=TRUE),function(path)
    paste(readLines(path,warn=FALSE),collapse='\n'),''),collapse='\n')
  stopifnot(!grepl('SYNTHETIC_PRIVATE_MARKER',text,fixed=TRUE),!grepl(base,text,fixed=TRUE))
}
cat('PASS: real preflight rejects executable CONFIG; mocked complete recovery fits180, replays11tables, exports19allowlisted files/200000draws, suppresses private errors, detects changed inputs and cleans failed private graphics.\n')
