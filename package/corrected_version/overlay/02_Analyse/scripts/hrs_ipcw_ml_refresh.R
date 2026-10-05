# User-run selected-model IPCW recovery. Historical tuning is never repeated.
ml_refresh_source_names <- function() c('hrs_feasibility.R','hrs_stage2.R',
  'hrs_source_hierarchy.R','hrs_analysis.R','hrs_prediction_validation.R',
  'hrs_grip_change.R','hrs_grip_history.R','hrs_ml_extension.R','hrs_ml_equivalence.R','hrs_ml_point_figures.R')

ml_refresh_load <- function(directory) {
  .libPaths(c(file.path(dirname(dirname(directory)),'.local-r-library'),.libPaths()))
  env<-new.env(parent=.GlobalEnv)
  for(name in ml_refresh_source_names()) sys.source(file.path(directory,name),envir=env)
  env
}

ml_refresh_configs <- function(env) {
  full<-env$ml_config('full');inference<-full
  inference$fractions<-1;inference$ci<-5000L;inference$export_bootstrap<-TRUE
  list(full=full,inference=inference)
}

ml_refresh_columns <- function() list(
  metrics=c('fraction','model','target','arm','auc','auc_lower','auc_upper','brier',
    'brier_lower','brier_upper','interval_log_score','log_score_lower','log_score_upper',
    'calibration_intercept','calibration_slope','auc_lower_position','auc_upper_position',
    'brier_lower_position','brier_upper_position','uncertainty','status'),
  paired=c('fraction','contrast','model','target','arm','reference_model','metric',
    'difference','ci_lower','ci_upper','uncertainty'),
  calibration=c('fraction','model','target','arm','group','n_rounded','predicted','observed','status'),
  tuning=c('fraction','fold','model','target','arm','ratio','training_n_rounded',
    'training_events_rounded','inner_log_score','failed_fits','attempted_fits','parameter'),
  diagnostics=c('fraction','fold','measure','standardized_mean_difference'),
  bootstrap_differences=c('fraction','contrast','model','target','arm','reference_model',
    'metric','replicate','difference'))

ml_refresh_key <- function(x,keys) do.call(paste,c(x[keys],sep='|'))

ml_refresh_grid <- function(x,expected,keys) {
  if(!all(keys %in% names(x))||nrow(x)!=nrow(expected)||
     anyNA(x[keys])||anyDuplicated(ml_refresh_key(x,keys))||
     !setequal(ml_refresh_key(x,keys),ml_refresh_key(expected,keys)))
    stop('Incomplete or unexpected aggregate key grid.')
}

ml_refresh_read <- function(path,columns=NULL) {
  if(!file.exists(path)||isTRUE(file.info(path)$isdir)||nzchar(Sys.readlink(path)))
    stop('A required regular input file is missing or linked.')
  x<-read.csv(path,stringsAsFactors=FALSE,check.names=FALSE)
  if(!is.null(columns)&&!identical(names(x),columns)) stop('Unexpected aggregate columns.')
  x
}

ml_refresh_parameter <- function(value,model,env) {
  if(length(value)!=1L||is.na(value)) stop('Invalid stored parameter string.')
  p<-list()
  if(nzchar(value)) {
    parts<-strsplit(value,';',fixed=TRUE)[[1]]
    kv<-strsplit(parts,'=',fixed=TRUE)
    if(any(lengths(kv)!=2L)) stop('Invalid stored parameter pairs.')
    names<-vapply(kv,'[','',1)
    if(anyDuplicated(names)||any(!nzchar(names))) stop('Invalid parameter names.')
    p<-setNames(lapply(kv,function(x) suppressWarnings(as.numeric(x[2]))),names)
    if(any(!is.finite(unlist(p)))) stop('Stored parameters must be finite numbers.')
  }
  equal<-function(q) setequal(names(p),names(q))&&
    identical(as.numeric(unlist(p[sort(names(p))])),as.numeric(unlist(q[sort(names(q))])))
  if(!any(vapply(env$ml_grid(model,FALSE),equal,TRUE))) stop('Stored parameters are outside the historical grid.')
  p
}

ml_refresh_check_tuning <- function(tuning,cfg,env) {
  if(!identical(names(tuning),ml_refresh_columns()$tuning)) stop('Unexpected stored-tuning columns.')
  keys<-c('fraction','fold','model','target','arm')
  expected<-expand.grid(fraction=cfg$fractions,fold=seq_len(cfg$outer),model=cfg$models,
    target=c('current','prior'),arm=c('original','augmented'),stringsAsFactors=FALSE)
  ml_refresh_grid(tuning,expected,keys)
  numeric<-c('ratio','training_n_rounded','training_events_rounded','inner_log_score','failed_fits','attempted_fits')
  if(!all(vapply(tuning[numeric],is.numeric,TRUE))||any(!is.finite(as.matrix(tuning[numeric])))||
     any(tuning$ratio[tuning$arm=='original']!=0)||
     any(!tuning$ratio[tuning$arm=='augmented'] %in% cfg$ratios)||
     any(tuning$training_n_rounded<=0)||any(tuning$training_events_rounded<0)||
     any(tuning$training_events_rounded>tuning$training_n_rounded)||
     any(tuning$training_n_rounded%%10!=0)||any(tuning$training_events_rounded%%10!=0)||
     any(tuning$failed_fits<0)||any(tuning$attempted_fits<1)||
     any(tuning$failed_fits>tuning$attempted_fits)||
     any(tuning$failed_fits!=floor(tuning$failed_fits))||
     any(tuning$attempted_fits!=floor(tuning$attempted_fits))) stop('Invalid stored-tuning values.')
  for(i in seq_len(nrow(tuning))) {
    ml_refresh_parameter(tuning$parameter[i],tuning$model[i],env)
    attempts<-cfg$inner*length(env$ml_grid(tuning$model[i],FALSE))*
      if(tuning$arm[i]=='original') 1L else length(cfg$ratios)
    if(tuning$attempted_fits[i]!=attempts) stop('Stored attempted-fit count differs from the historical grid.')
  }
  invisible(TRUE)
}

ml_refresh_compare_table <- function(actual,expected,label,keys,tolerance=1e-8) {
  if(!identical(names(actual),names(expected))) stop('Legacy replay schema mismatch.')
  ml_refresh_grid(actual,expected,keys)
  expected<-expected[match(ml_refresh_key(actual,keys),ml_refresh_key(expected,keys)),,drop=FALSE]
  largest<-0
  for(name in names(actual)) {
    a<-actual[[name]];b<-expected[[name]]
    if(!identical(is.na(a),is.na(b))) stop('Legacy replay missingness mismatch.')
    use<-!is.na(a)
    if(is.numeric(a)&&is.numeric(b)) {
      if(any(!is.finite(a[use]))||any(!is.finite(b[use]))) stop('Non-finite legacy replay values.')
      difference<-if(any(use)) max(abs(a[use]-b[use])) else 0
      largest<-max(largest,difference)
      if(difference>tolerance) stop('Legacy numeric replay exceeds the approved tolerance.')
    } else if(!identical(as.character(a[use]),as.character(b[use]))) stop('Legacy replay labels differ.')
  }
  data.frame(table=label,rows=nrow(actual),max_absolute_difference=largest,tolerance=tolerance,status='PASS')
}

ml_refresh_match_selection <- function(full,inference) {
  x<-full[full$fraction==1,,drop=FALSE]
  ml_refresh_compare_table(x,inference,'fraction1_selection',c('fraction','fold','model','target','arm'),0)
}

ml_refresh_selection <- function(tuning,env) {
  function(train,model,target,arm,cfg,inner,seed,fraction,fold) {
    z<-tuning[tuning$fraction==fraction & tuning$fold==fold & tuning$model==model &
      tuning$target==target & tuning$arm==arm,,drop=FALSE]
    if(nrow(z)!=1L||seed!=cfg$seed+1000*fold||length(inner)!=nrow(train)||
       z$training_n_rounded!=10*round(nrow(train)/10)||
       z$training_events_rounded!=10*round(sum(is.finite(train$hi))/10))
      stop('Selected-configuration or training-count check failed.')
    list(parameter=ml_refresh_parameter(z$parameter,model,env),ratio=z$ratio,
      score=z$inner_log_score,failures=z$failed_fits,attempts=z$attempted_fits)
  }
}

ml_refresh_digest <- function(value,private) {
  path<-tempfile('fingerprint_',tmpdir=private)
  on.exit(unlink(path),add=TRUE)
  saveRDS(value,path,version=2,compress=FALSE)
  hash<-unname(tools::md5sum(path))
  if(is.na(hash)) stop('Prediction-cache fingerprint failed.')
  hash
}

ml_refresh_cache <- function(env,private,source_fingerprint) {
  entries<-new.env(parent=emptyenv());fits<-hits<-0L;frozen<-FALSE
  provider<-function(train,aug,test,model,target,arm,parameter,seed,preprocess,fraction,fold) {
    key<-paste(fraction,fold,model,target,arm,sep='|')
    p<-preprocess;p$formula<-deparse(p$formula)
    fingerprint<-ml_refresh_digest(list(source=source_fingerprint,train=train,aug=aug,test=test,
      model=model,target=target,arm=arm,parameter=parameter,seed=seed,preprocess=p,
      fraction=fraction,fold=fold),private)
    if(exists(key,envir=entries,inherits=FALSE)) {
      x<-get(key,envir=entries,inherits=FALSE)
      if(!identical(fingerprint,x$fingerprint)||
         !identical(ml_refresh_digest(x$prediction,private),x$prediction_hash))
        stop('Prediction-cache input or value integrity mismatch.')
      hits<<-hits+1L
      return(x$prediction)
    }
    if(frozen) stop('Unexpected cache miss after historical selected fits were frozen.')
    fit<-env$ml_fit(aug,model,target,parameter,seed,preprocess)
    prediction<-env$ml_predict(fit,test)
    if(!identical(names(prediction),c('risk','score'))||
       any(lengths(prediction)!=nrow(test))||
       !all(vapply(prediction,is.numeric,TRUE))||any(!is.finite(unlist(prediction)))||
       any(prediction$risk<0 | prediction$risk>1)) stop('Invalid recovered predictions.')
    assign(key,list(fingerprint=fingerprint,prediction=prediction,row_names=rownames(test),
      prediction_hash=ml_refresh_digest(prediction,private)),envir=entries)
    fits<<-fits+1L
    prediction
  }
  risks<-function(d,combos,outer) {
    ids<-rownames(d)
    if(is.null(ids)||anyDuplicated(ids)) stop('Unique private row mapping required for figures.')
    risk<-matrix(NA_real_,nrow(d),nrow(combos))
    for(j in seq_len(nrow(combos))) for(k in seq_len(outer)) {
      z<-combos[j,];key<-paste(1,k,z$model,z$target,z$arm,sep='|')
      if(!exists(key,envir=entries,inherits=FALSE)) stop('Full-fraction figure cache missing.')
      x<-get(key,envir=entries,inherits=FALSE);ix<-match(x$row_names,ids)
      if(anyNA(ix)||anyDuplicated(ix)||any(!is.na(risk[ix,j]))||
         !identical(ml_refresh_digest(x$prediction,private),x$prediction_hash))
        stop('Private figure row mapping or prediction integrity failed.')
      risk[ix,j]<-x$prediction$risk
    }
    if(any(!is.finite(risk))) stop('Incomplete private figure predictions.')
    risk
  }
  list(provider=provider,freeze=function() frozen<<-TRUE,risks=risks,
    counts=function() list(fits=fits,hits=hits,entries=length(ls(entries,all.names=TRUE))))
}

ml_refresh_expected <- function(cfg) {
  combos<-expand.grid(model=cfg$models,target=c('current','prior'),arm=c('original','augmented'),stringsAsFactors=FALSE)
  pair<-list()
  for(i in seq_len(nrow(combos))) {
    z<-combos[i,]
    add<-function(label,reference) pair[[length(pair)+1L]]<<-data.frame(z,contrast=label,reference_model=reference)
    if(z$target=='prior') add('prior_minus_current',z$model)
    if(z$arm=='augmented') add('augmented_minus_original',z$model)
    if(z$model!='weibull') add('model_minus_weibull','weibull')
  }
  list(metrics=merge(data.frame(fraction=cfg$fractions),combos,by=NULL),
    paired=merge(merge(data.frame(fraction=cfg$fractions),do.call(rbind,pair),by=NULL),
      data.frame(metric=c('auc','brier','interval_log_score')),by=NULL),
    calibration=merge(merge(data.frame(fraction=cfg$fractions),combos,by=NULL),data.frame(group=1:5),by=NULL),
    diagnostics=expand.grid(fraction=cfg$fractions,fold=seq_len(cfg$outer),
      measure=c('age','grip10','grip14','gap_years','event_fraction','early_censored_fraction'),stringsAsFactors=FALSE))
}

ml_refresh_keys <- function() list(metrics=c('fraction','model','target','arm'),
  paired=c('fraction','contrast','model','target','arm','reference_model','metric'),
  calibration=c('fraction','model','target','arm','group'),
  tuning=c('fraction','fold','model','target','arm'),diagnostics=c('fraction','fold','measure'),
  bootstrap_differences=c('fraction','contrast','model','target','arm','reference_model','metric','replicate'))

ml_refresh_validate_result <- function(result,cfg,tuning,env) {
  columns<-ml_refresh_columns();keys<-ml_refresh_keys()
  names<-c('metrics','paired','calibration','tuning','diagnostics',
    if(isTRUE(cfg$export_bootstrap)) 'bootstrap_differences')
  if(!setequal(names(result),names)) stop('Incomplete recovered benchmark outputs.')
  for(name in names) if(!is.data.frame(result[[name]])||
    !identical(names(result[[name]]),columns[[name]])) stop('Unexpected recovered benchmark schema.')
  expected<-ml_refresh_expected(cfg)
  for(name in names(expected)) ml_refresh_grid(result[[name]],expected[[name]],keys[[name]])
  ml_refresh_check_tuning(result$tuning,cfg,env)
  ml_refresh_compare_table(result$tuning,tuning,'stored_tuning',keys$tuning)
  finite<-function(x,fields) all(vapply(x[fields],is.numeric,TRUE))&&
    all(is.finite(as.matrix(x[fields])))
  m<-result$metrics;p<-result$paired;c<-result$calibration;g<-result$diagnostics
  if(!finite(m,setdiff(columns$metrics,c(keys$metrics,'uncertainty','status')))||
     anyNA(m$status)||any(m$status!='OK')||
     any(m$uncertainty!='conditional_OOF_household_bootstrap')||
     any(m$auc_lower>m$auc_upper)||any(m$brier_lower>m$brier_upper)||
     any(m$log_score_lower>m$log_score_upper)||
     !finite(p,c('difference','ci_lower','ci_upper'))||any(p$ci_lower>p$ci_upper)||
     any(p$uncertainty!='conditional_OOF_household_bootstrap')||
     !finite(g,'standardized_mean_difference')) stop('Unstable or invalid benchmark metrics.')
  if(!finite(c,'n_rounded')||any(c$n_rounded<0)||any(c$n_rounded%%10!=0)||
     anyNA(c$status)||any(!c$status %in% c('OK','SUPPRESSED'))||
     !is.numeric(c$predicted)||!is.numeric(c$observed)) stop('Invalid calibration groups.')
  ok<-c$status=='OK';suppressed<-!ok
  if(any(!is.finite(c$predicted[ok]))||any(!is.finite(c$observed[ok]))||
     any(c$predicted[ok]<0 | c$predicted[ok]>1)||any(c$observed[ok]<0 | c$observed[ok]>1)||
     any(!is.na(c$predicted[suppressed]))||any(!is.na(c$observed[suppressed]))) stop('Invalid calibration suppression or coordinates.')
  if(isTRUE(cfg$export_bootstrap)) {
    b<-result$bootstrap_differences
    expected<-expected$paired[expected$paired$metric %in% c('auc','brier'),]
    key<-setdiff(keys$bootstrap_differences,'replicate')
    if(!finite(b,c('replicate','difference'))||nrow(b)!=nrow(expected)*cfg$ci||
       any(b$replicate!=floor(b$replicate))||any(!b$replicate %in% seq_len(cfg$ci))||
       anyDuplicated(ml_refresh_key(b,keys$bootstrap_differences))||
       !setequal(ml_refresh_key(b,key),ml_refresh_key(expected,key))||
       any(table(ml_refresh_key(b,key))!=cfg$ci)) stop('Incomplete paired bootstrap draws.')
  }
  invisible(TRUE)
}

ml_refresh_preflight <- function(args,directory) {
  if(length(args)!=5L) stop('Use DATA_DIR OUTPUT_PARENT HISTORICAL_FULL_DIR HISTORICAL_INFERENCE_DIR AUDIT_SEND_BACK.')
  if(!all(dir.exists(args))) stop('All five directories must already exist.')
  args<-vapply(args,normalizePath,'',mustWork=TRUE)
  for(i in c(1,3,4,5)) if(args[2]==args[i]||startsWith(args[2],paste0(args[i],'/')))
    stop('Output parent must be separate from all inputs.')
  if(args[3]==args[4]) stop('Distinct historical full and inference runs required.')
  root<-dirname(dirname(directory))
  legacy<-file.path(root,'03_Dokumentation','IPCW_Korrektur_2026-10-04','legacy_sources')
  full_source<-file.path(root,'04_Archiv','Berichte','ml_critical_audit_v1','before','hrs_ml_extension.R')
  prehook<-file.path(root,'03_Dokumentation','IPCW_Rueckgabe_2026-10-04','source_before_ml_hooks','hrs_ml_extension.R')
  if(unname(tools::md5sum(file.path(directory,'hrs_ml_extension.R')))!='092dadebace530fc41637c261b5519e6')
    stop('ML source is not the reviewed recovery-hook version.')
  audit<-ml_refresh_read(file.path(args[5],'script_hashes.csv'),c('file','md5'))
  if(anyDuplicated(audit$file)||nrow(audit)!=12L||
     !startsWith(readLines(file.path(args[5],'STATUS.txt'),warn=FALSE)[1],'COMPLETE:'))
    stop('The completed IPCW audit with twelve source hashes is required.')
  for(i in seq_len(nrow(audit))) {
    relative<-audit$file[i]
    allowed<-c(paste0('02_Analyse/scripts/',c('hrs_ipcw_impact_audit.R',
      setdiff(ml_refresh_source_names(),c('hrs_ml_equivalence.R','hrs_ml_point_figures.R')),'hrs_reporting_patch.R')),
      paste0('03_Dokumentation/IPCW_Korrektur_2026-10-04/legacy_sources/',
        c('hrs_ml_extension.R','hrs_prediction_validation.R')))
    if(!relative %in% allowed) stop('Unexpected IPCW audit source path.')
    path<-if(relative=='02_Analyse/scripts/hrs_ml_extension.R') prehook else file.path(root,relative)
    if(is.na(tools::md5sum(path))||unname(tools::md5sum(path))!=audit$md5[i])
      stop('Current or preserved sources do not match the returned IPCW audit.')
  }
  env<-ml_refresh_load(directory);env$ml_require()
  if(!all(c('selection_provider','prediction_provider','weight_function') %in% names(formals(env$ml_benchmark))))
    stop('Required benchmark recovery hooks are missing.')
  configs<-ml_refresh_configs(env)
  versions<-data.frame(component=c('R','survival','rpart','xgboost'),version=c(as.character(getRversion()),
    vapply(c('survival','rpart','xgboost'),function(p) as.character(utils::packageVersion(p)),'')))
  rownames(versions)<-NULL
  history<-list();paths<-character();roles<-character()
  old_names<-setdiff(ml_refresh_source_names(),c('hrs_ml_equivalence.R','hrs_ml_point_figures.R'))
  for(role in c('full','inference')) {
    path<-args[if(role=='full') 3 else 4];cfg<-configs[[role]]
    if(!startsWith(readLines(file.path(path,'STATUS.txt'),warn=FALSE)[1],'SUCCESS:')) stop('Successful historical runs required.')
    config<-capture.output(dput(cfg))
    if(!identical(readLines(file.path(path,'CONFIG.R'),warn=FALSE),config)) stop('Stored configuration differs from the exact historical profile.')
    ver<-ml_refresh_read(file.path(path,'versions.csv'),c('component','version'))
    if(!identical(ver,versions)) stop('Historical R/package versions must match the current runtime exactly.')
    h<-ml_refresh_read(file.path(path,'source_hashes.csv'),c('file','md5'))
    if(nrow(h)!=length(old_names)||anyDuplicated(h$file)||!setequal(h$file,old_names)) stop('Unexpected historical source manifest.')
    for(name in old_names) {
      source<-file.path(if(name %in% c('hrs_ml_extension.R','hrs_prediction_validation.R')) legacy else directory,name)
      if(name=='hrs_ml_extension.R' && role=='full') source<-full_source
      observed<-unname(tools::md5sum(source))
      if(is.na(observed)||observed!=h$md5[h$file==name])
        stop(paste('Historical source hash mismatch:',role,name))
    }
    tables<-c('metrics','paired','calibration','tuning','diagnostics',if(role=='inference') 'bootstrap_differences')
    result<-setNames(lapply(tables,function(name) ml_refresh_read(file.path(path,paste0('benchmark_',name,'.csv')),
      ml_refresh_columns()[[name]])),tables)
    ml_refresh_validate_result(result,cfg,result$tuning,env)
    history[[role]]<-result
    files<-c('STATUS.txt','CONFIG.R','versions.csv','source_hashes.csv',paste0('benchmark_',tables,'.csv'))
    paths<-c(paths,file.path(path,files));roles<-c(roles,paste0('historical_',role,'/',files))
  }
  ml_refresh_match_selection(history$full$tuning,history$inference$tuning)
  source_paths<-c(file.path(directory,c(ml_refresh_source_names(),'hrs_ipcw_ml_refresh.R')),
    file.path(legacy,c('hrs_ml_extension.R','hrs_prediction_validation.R')),prehook,full_source,
    file.path(dirname(directory),'tests',c('test_ipcw_ml_refresh.R','test_ml_recovery_hooks.R')),
    file.path(args[5],c('STATUS.txt','script_hashes.csv')))
  source_roles<-c(paste0('scripts/',c(ml_refresh_source_names(),'hrs_ipcw_ml_refresh.R')),
    paste0('legacy/',c('hrs_ml_extension.R','hrs_prediction_validation.R')),'prehook/hrs_ml_extension.R','historical_full_source/hrs_ml_extension.R',
    paste0('tests/',c('test_ipcw_ml_refresh.R','test_ml_recovery_hooks.R')),'audit/STATUS.txt','audit/script_hashes.csv')
  paths<-c(source_paths,paths);roles<-c(source_roles,roles)
  hashes<-data.frame(file=roles,md5=unname(tools::md5sum(paths)))
  if(anyNA(hashes$md5)) stop('Input/source provenance could not be recorded.')
  old<-new.env(parent=baseenv());sys.source(file.path(legacy,'hrs_ml_extension.R'),envir=old)
  list(args=args,env=env,configs=configs,history=history,versions=versions,
    legacy_weight=old$ml_weights,paths=paths,hashes=hashes)
}

ml_refresh_logged <- function(fun,path) {
  con<-file(path,open='wt');sink(con);sink(con,type='message')
  on.exit({sink(type='message');sink();close(con)})
  fun()
}

ml_refresh_import <- function(args,private,directory,env) {
  imported<-env$stage2_main(c(args[1],file.path(private,'import_checks')),file.path(directory,'hrs_feasibility.R'))
  d<-env$prepare_change_data(imported)$data
  env$ml_validate(d)
  if(nrow(d)!=6307L) stop('Recovered analytical cohort size differs from the audited cohort.')
  d
}

ml_refresh_main <- function(args,directory) {
  directory<-normalizePath(directory,mustWork=TRUE)
  checked<-ml_refresh_preflight(args,directory);args<-checked$args;env<-checked$env
  out<-file.path(args[2],paste0('ipcw_ml_refresh_',format(Sys.time(),'%Y%m%d_%H%M%S'),'_',Sys.getpid()))
  if(file.exists(out)||!dir.create(out)) stop('A fresh output directory is required.')
  private<-file.path(out,'LOCAL_ONLY');share<-file.path(out,'SEND_BACK')
  if(!dir.create(private)||!dir.create(share)) stop('Separated output directories could not be created.')
  status<-file.path(share,'STATUS.txt');writeLines('INCOMPLETE: ML recovery has not passed all checks.',status)
  log<-file.path(private,'recovery.log')
  message('Selected-model recovery: 180 planned fits. No tuning, association models or simulation rerun.')
  message('Historical 500/5000-draw evaluations are reproduced before corrected aggregates can be shared.')
  tryCatch({
    result<-ml_refresh_logged(function() {
      d<-ml_refresh_import(args,private,directory,env)
      RNGkind('Mersenne-Twister','Inversion','Rejection')
      cache<-ml_refresh_cache(env,private,paste(checked$hashes$md5,collapse='|'))
      # A missing hook must fail instead of silently triggering the historical grid.
      env$ml_tune<-function(...) stop('Hyperparameter tuning is forbidden in recovery.')
      checks<-list();corrected<-list()
      for(weight in c('legacy','corrected')) for(role in c('full','inference')) {
        cfg<-checked$configs[[role]];old<-checked$history[[role]]
        folder<-file.path(private,paste(weight,role,sep='_'));if(!dir.create(folder)) stop('Fresh evaluation folder required.')
        value<-env$ml_benchmark(d,cfg,out=folder,
          selection_provider=ml_refresh_selection(old$tuning,env),prediction_provider=cache$provider,
          weight_function=if(weight=='legacy') checked$legacy_weight else env$ml_weights)
        ml_refresh_validate_result(value,cfg,old$tuning,env)
        if(weight=='legacy') {
          for(name in names(value)) checks[[length(checks)+1L]]<-cbind(profile=role,
            ml_refresh_compare_table(value[[name]],old[[name]],name,ml_refresh_keys()[[name]]))
          if(role=='full') {
            if(cache$counts()$fits!=180L||cache$counts()$entries!=180L) stop('Unexpected selected-fit count.')
            cache$freeze()
          }
        } else corrected[[role]]<-value
      }
      if(cache$counts()$fits!=180L||cache$counts()$hits!=300L) stop('Unexpected selected-fit or prediction-cache count.')
      inference<-file.path(private,'corrected_inference')
      dput(checked$configs$inference,file=file.path(inference,'CONFIG.R'))
      write.csv(checked$hashes,file.path(inference,'source_hashes.csv'),row.names=FALSE)
      writeLines('SUCCESS: corrected benchmark passed recovery checks. Private intermediate only.',file.path(inference,'STATUS.txt'))
      eq<-file.path(private,'corrected_equivalence');env$ml_equivalence_report(inference,eq)
      tost<-ml_refresh_read(file.path(eq,'conditional_tost.csv'))
      margins<-ml_refresh_read(file.path(eq,'margin_sensitivity.csv'))
      if(nrow(tost)!=360L||nrow(margins)!=360L||
         !startsWith(readLines(file.path(eq,'STATUS.txt'),warn=FALSE)[1],'SUCCESS:')) stop('Conditional aggregate inference incomplete.')
      figures<-file.path(private,'person_figures')
      figure_status<-tryCatch({
        if(!dir.create(figures)) stop('Fresh private figure directory required.')
        combos<-expand.grid(model=checked$configs$inference$models,target=c('current','prior'),
          arm=c('original','augmented'),stringsAsFactors=FALSE)
        risks<-cache$risks(d,combos,checked$configs$inference$outer)
        draw<-function() {
          before<-grDevices::dev.list()
          on.exit(for(id in setdiff(grDevices::dev.list(),before)) try(grDevices::dev.off(id),silent=TRUE),add=TRUE)
          env$ml_render_person_points(risks,combos,corrected$inference$calibration,figures)
        }
        draw()
        if(length(list.files(figures,pattern='[.]png$'))!=3L) stop('Expected three private PNG figures.')
        writeLines('SUCCESS: private figures created from recovered predictions and corrected calibration.',file.path(figures,'STATUS.txt'))
        'SUCCESS: three PNG figures exist only in LOCAL_ONLY/person_figures; visual review remains pending.'
      },error=function(e) {
        if(dir.exists(figures)) writeLines(c('INCOMPLETE: optional private figures failed.',conditionMessage(e)),file.path(figures,'STATUS.txt'))
        'INCOMPLETE: optional private figures failed; numerical aggregates remain independently checked.'
      })
      list(corrected=corrected,checks=do.call(rbind,checks),counts=cache$counts(),tost=tost,margins=margins,figure_status=figure_status)
    },log)
    if(!identical(checked$hashes$md5,unname(tools::md5sum(checked$paths))))
      stop('Sources or historical inputs changed during recovery.')
    for(role in c('full','inference')) {
      folder<-file.path(share,role);if(!dir.create(folder)) stop('Fresh aggregate directory required.')
      for(name in names(result$corrected[[role]])) {
        filename<-if(name=='tuning') 'historical_selected_tuning.csv' else paste0('benchmark_',name,'.csv')
        write.csv(result$corrected[[role]][[name]],file.path(folder,filename),row.names=FALSE)
      }
    }
    eq<-file.path(share,'equivalence');if(!dir.create(eq)) stop('Fresh equivalence directory required.')
    write.csv(result$tost,file.path(eq,'conditional_tost.csv'),row.names=FALSE)
    write.csv(result$margins,file.path(eq,'margin_sensitivity.csv'),row.names=FALSE)
    write.csv(result$checks,file.path(share,'REPLAY_CHECKS.csv'),row.names=FALSE)
    write.csv(checked$hashes,file.path(share,'source_hashes.csv'),row.names=FALSE)
    write.csv(checked$versions,file.path(share,'versions.csv'),row.names=FALSE)
    write.csv(data.frame(selected_fits=result$counts$fits,cache_hits=result$counts$hits,
      full_bootstrap_draws=500L,inference_bootstrap_draws=5000L,current_tuning_fits=0L,
      selection_metadata='historical inner-CV results, not rerun'),file.path(share,'recovery_metadata.csv'),row.names=FALSE)
    writeLines(c('Return only SEND_BACK. LOCAL_ONLY and its detailed logs remain private.',
      'Cache verification temporarily writes private RDS fingerprints under LOCAL_ONLY and removes them after hashing.',
      'Existing configurations were recovered. No new tuning, association fitting or simulation was performed.',
      '180 selected model fits were reused in all four legacy/corrected evaluations.',
      'All stored benchmark tables, including conditional bootstrap differences, reproduced within 1e-8 before release.',
      'full retains three training fractions and 500 conditional resamples. inference retains fraction 1 and 5000 resamples.',
      'historical_selected_tuning.csv records the original tuning scores, attempts and failures, not new searches.',
      'Paired inference remains conditional on recovered held-out predictions, folds, tuning and censoring weights.',
      'Equivalence margins remain hypothetical. No clinical equivalence or full-pipeline coverage is established.',
      'No participant-level prediction tables, fitted objects or individual-point figures are included.',result$figure_status,
      'Figures and manuscript integration remain separate. Primary nested-bootstrap correction is not performed here.'),
      file.path(share,'READ_ME.txt'))
    writeLines('SUCCESS: selected ML configurations recovered, all legacy replay checks passed and IPCW aggregates corrected. No new tuning.',status)
    message('Finished. Return only: ',share)
    invisible(out)
  },error=function(e) {
    cat('\nRecovery failure: ',conditionMessage(e),'\n',file=log,append=TRUE)
    writeLines('INCOMPLETE: ML recovery failed. Do not use partial outputs. Detailed errors remain LOCAL_ONLY.',status)
    stop('ML recovery failed. Inspect LOCAL_ONLY/recovery.log. No successful corrected result is available.',call.=FALSE)
  })
}

if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  tryCatch(ml_refresh_main(commandArgs(trailingOnly=TRUE),dirname(normalizePath(script))),
    error=function(e) {message('ML recovery stopped: ',conditionMessage(e));quit(status=1L)})
}
