# Local conventional-diagnostics refresh. Existing model/statistical code is reused.
diagnostics_refresh_source_names <- function() c('hrs_feasibility.R','hrs_stage2.R',
  'hrs_source_hierarchy.R','hrs_analysis.R','hrs_prediction_validation.R','hrs_grip_change.R','hrs_diagnostics.R')
diagnostics_refresh_models <- function() c('weibull_current','weibull_prior','lognormal_current','lognormal_prior')
diagnostics_refresh_bands <- function() levels(cut(c(0,1),c(0,.05,.1,.2,.4,1),include.lowest=TRUE,right=FALSE))
diagnostics_refresh_columns <- function() list(weight_diagnostics=c('metric','value'),
  calibration_groups=c('model','risk_band','n_rounded','events_rounded','known_survivors_rounded',
    'predicted_risk','observed_risk','status'),
  model_fit_summary=c('model','AIC','mean_interval_log_score','status'),
  survival_fit=c('model','year','predicted_survival','turnbull_survival'))

diagnostics_refresh_load <- function(directory) {
  .libPaths(c(file.path(dirname(dirname(directory)),'.local-r-library'),.libPaths()))
  env<-new.env(parent=.GlobalEnv)
  for(name in c(diagnostics_refresh_source_names(),'hrs_ipcw_primary_refresh.R'))
    sys.source(file.path(directory,name),envir=env)
  env
}
diagnostics_refresh_sha256 <- function(paths) {
  lines<-system2('/usr/bin/shasum',c('-a','256',shQuote(paths)),stdout=TRUE)
  if(!is.null(attr(lines,'status'))||length(lines)!=length(paths)||
     any(!grepl('^[a-f0-9]{64}  ',lines))) stop('Source SHA-256 computation failed.')
  substr(lines,1,64)
}
diagnostics_refresh_read <- function(path) {
  columns<-diagnostics_refresh_columns()
  result<-lapply(names(columns),function(name) {
    file<-file.path(path,paste0(name,'.csv'))
    if(!file.exists(file)||isTRUE(file.info(file)$isdir)||nzchar(Sys.readlink(file))) stop('Regular aggregate file required.')
    x<-read.csv(file,stringsAsFactors=FALSE,check.names=FALSE)
    if(!identical(names(x),columns[[name]])) stop('Unexpected diagnostic aggregate schema.')
    x
  })
  setNames(result,names(columns))
}
diagnostics_refresh_validate <- function(x,env) {
  models<-diagnostics_refresh_models();c<-x$calibration_groups;w<-x$weight_diagnostics
  m<-x$model_fit_summary;s<-x$survival_fit
  for(name in names(diagnostics_refresh_columns()))
    if(!identical(names(x[[name]]),diagnostics_refresh_columns()[[name]])) stop('Unexpected diagnostic columns.')
  env$primary_refresh_grid(m,data.frame(model=models),'model')
  env$primary_refresh_grid(s,expand.grid(model=models,year=1:5),c('model','year'))
  env$primary_refresh_grid(c,expand.grid(model=models,risk_band=diagnostics_refresh_bands()),c('model','risk_band'))
  env$primary_refresh_grid(w,data.frame(metric=c('G_min','G_median','weight_max','weight_median',
    'effective_known_sample_size_rounded_10')),'metric')
  finite<-function(z,fields) all(vapply(z[fields],is.numeric,TRUE))&&all(is.finite(as.matrix(z[fields])))
  if(!finite(m,c('AIC','mean_interval_log_score'))||anyNA(m$status)||any(m$status!='FIT_OK_APPARENT_ONLY')||
     !finite(s,c('year','predicted_survival','turnbull_survival'))||
     any(s$predicted_survival<0 | s$predicted_survival>1)||any(s$turnbull_survival<0 | s$turnbull_survival>1)||
     !finite(w,'value')||any(w$value<=0)) stop('Invalid or incomplete model/weight diagnostics.')
  counts<-unlist(c[c('n_rounded','events_rounded','known_survivors_rounded')],use.names=FALSE)
  number<-suppressWarnings(as.numeric(counts));numeric<-counts!='SUPPRESSED'
  if(anyNA(counts)||any(!is.finite(number[numeric]))||any(number[numeric]<10)||any(number[numeric]%%10!=0)||
     anyNA(c$status)||any(!c$status %in% c('OK','SUPPRESSED_SPARSE'))) stop('Invalid calibration counts or status.')
  for(field in c('n_rounded','events_rounded','known_survivors_rounded')) {
    value<-suppressWarnings(as.numeric(c[[field]][c$status=='OK']))
    if(anyNA(value)||any(value<if(field=='n_rounded') 50 else 10)) stop('Unsuppressed calibration counts are below reporting thresholds.')
  }
  for(field in c('predicted_risk','observed_risk')) {
    v<-c[[field]];ok<-c$status=='OK'
    if(!(is.numeric(v)||(is.logical(v)&&all(is.na(v))))||any(!is.finite(v[ok]))||
       any(v[ok]<0 | v[ok]>1)||any(!is.na(v[!ok]))) stop('Invalid calibration values or suppression.')
  }
  invisible(TRUE)
}
diagnostics_refresh_compare <- function(actual,old,env) {
  diagnostics_refresh_validate(actual,env);diagnostics_refresh_validate(old,env)
  keys<-list(model_fit_summary='model',survival_fit=c('model','year'),calibration_groups=c('model','risk_band'))
  rows<-lapply(names(keys),function(name) {
    a<-actual[[name]];b<-old[[name]];key<-keys[[name]]
    makekey<-function(x) do.call(paste,c(x[key],sep='|'))
    b<-b[match(makekey(a),makekey(b)),,drop=FALSE];maximum<-0
    for(field in setdiff(names(a),if(name=='calibration_groups') 'observed_risk' else character())) {
      av<-a[[field]];bv<-b[[field]]
      if(!identical(is.na(av),is.na(bv))) stop('Unchanged diagnostic missingness failed to reproduce.')
      use<-!is.na(av)
      if(is.numeric(av)&&is.numeric(bv)) {
        delta<-if(any(use)) max(abs(av[use]-bv[use])) else 0
        maximum<-max(maximum,delta)
        if(delta>1e-8) stop('Unchanged diagnostic numeric values failed to reproduce.')
      } else if(!identical(as.character(av[use]),as.character(bv[use]))) stop('Unchanged diagnostic labels/counts failed to reproduce.')
    }
    data.frame(table=name,rows=nrow(a),max_absolute_difference=maximum,tolerance=1e-8,status='PASS')
  })
  do.call(rbind,rows)
}

diagnostics_refresh_preflight <- function(args,directory) {
  if(length(args)!=4L||!all(dir.exists(args))) stop('Use existing DATA_DIR OUTPUT_PARENT HISTORICAL_DIAGNOSTICS_DIR AUDIT_SEND_BACK.')
  args<-vapply(args,normalizePath,'',mustWork=TRUE)
  for(i in c(1,3,4)) if(args[2]==args[i]||startsWith(args[2],paste0(args[i],'/'))) stop('Separate output parent required.')
  if(unname(tools::md5sum(file.path(directory,'hrs_diagnostics.R')))!='c9b0590390a85a45dbe82aacf20be253')
    stop('Diagnostics helper differs from the reviewed unchanged version.')
  env<-diagnostics_refresh_load(directory)
  env$primary_refresh_preflight(args[c(1,2,4)],directory)
  audit<-read.csv(file.path(args[4],'script_hashes.csv'),stringsAsFactors=FALSE)
  h<-audit$md5[audit$file=='02_Analyse/scripts/hrs_grip_change.R']
  if(length(h)!=1L||h!=unname(tools::md5sum(file.path(directory,'hrs_grip_change.R')))) stop('Cohort helper differs from the returned audit.')
  old<-args[3];status<-readLines(file.path(old,'STATUS.txt'),warn=FALSE)
  note<-readLines(file.path(old,'READ_ME.txt'),warn=FALSE)
  if(!length(status)||!startsWith(status[1],'SUCCESS:')||
     !identical(grep('^R: ',note,value=TRUE),paste('R:',getRversion()))||
     !identical(grep('^survival: ',note,value=TRUE),paste('survival:',utils::packageVersion('survival'))))
    stop('Successful historical diagnostics and identical R/survival versions required.')
  names<-diagnostics_refresh_source_names();root<-dirname(dirname(directory))
  legacy<-file.path(root,'03_Dokumentation','IPCW_Korrektur_2026-10-04','legacy_sources','hrs_prediction_validation.R')
  paths<-file.path(directory,names);paths[names=='hrs_prediction_validation.R']<-legacy
  expected<-diagnostics_refresh_sha256(paths)
  historical<-readLines(file.path(old,'source_sha256.txt'),warn=FALSE)
  if(any(!grepl('^[a-f0-9]{64}  ',historical))) stop('Unexpected historical SHA-256 manifest format.')
  basenames<-basename(substring(historical,67))
  for(i in seq_along(names)) {
    row<-which(basenames==names[i])
    if(length(row)!=1L||substr(historical[row],1,64)!=expected[i]) stop('Historical diagnostic helper hash mismatch.')
  }
  reference<-diagnostics_refresh_read(old);diagnostics_refresh_validate(reference,env)
  inputs<-c('STATUS.txt','READ_ME.txt','source_sha256.txt',paste0(names(reference),'.csv'))
  audit_files<-c('STATUS.txt','PRIMARY_REPLAY_STATUS.txt','versions.txt','script_hashes.csv','primary_apparent_reweighting.csv')
  sources<-c(names,'hrs_ipcw_primary_refresh.R','hrs_ipcw_diagnostics_refresh.R')
  paths<-c(file.path(directory,sources),legacy,file.path(dirname(directory),'tests','test_ipcw_diagnostics_refresh.R'),
    file.path(old,inputs),file.path(args[4],audit_files))
  roles<-c(paste0('scripts/',sources),'legacy/hrs_prediction_validation.R','tests/test_ipcw_diagnostics_refresh.R',
    paste0('historical/',inputs),paste0('audit/',audit_files))
  hashes<-data.frame(file=roles,md5=unname(tools::md5sum(paths)))
  if(anyNA(hashes$md5)) stop('Incomplete diagnostic input/source provenance.')
  list(args=args,env=env,reference=reference,paths=paths,hashes=hashes)
}

diagnostics_refresh_main <- function(args,directory) {
  directory<-normalizePath(directory,mustWork=TRUE);checked<-diagnostics_refresh_preflight(args,directory)
  args<-checked$args;env<-checked$env
  out<-file.path(args[2],paste0('ipcw_diagnostics_refresh_',format(Sys.time(),'%Y%m%d_%H%M%S'),'_',Sys.getpid()))
  if(file.exists(out)||!dir.create(out)) stop('A fresh diagnostics output directory is required.')
  private<-file.path(out,'LOCAL_ONLY');share<-file.path(out,'SEND_BACK')
  if(!dir.create(private)||!dir.create(share)) stop('Separated diagnostics directories could not be created.')
  status<-file.path(share,'STATUS.txt');writeLines('INCOMPLETE: diagnostics refresh has not passed all checks.',status)
  log<-file.path(private,'diagnostics.log')
  message('Conventional diagnostics only: four simple survival fits and one Turnbull curve. No bootstrap or ML.')
  tryCatch({
    result<-env$primary_refresh_logged(function() {
      env$diagnostics_main(c(args[1],private),directory)
      folder<-file.path(private,'diagnostics_v1');s<-readLines(file.path(folder,'STATUS.txt'),warn=FALSE)
      if(!length(s)||!startsWith(s[1],'SUCCESS:')) stop('Existing diagnostics did not complete successfully.')
      value<-diagnostics_refresh_read(folder)
      list(value=value,checks=diagnostics_refresh_compare(value,checked$reference,env))
    },log)
    if(!identical(checked$hashes$md5,unname(tools::md5sum(checked$paths)))) stop('Diagnostic sources or historical inputs changed during the run.')
    for(name in names(result$value)) write.csv(result$value[[name]],file.path(share,paste0(name,'.csv')),row.names=FALSE)
    write.csv(result$checks,file.path(share,'REPLAY_CHECKS.csv'),row.names=FALSE)
    write.csv(checked$hashes,file.path(share,'source_hashes.csv'),row.names=FALSE)
    writeLines(c(paste('R:',getRversion()),paste('survival:',utils::packageVersion('survival')),
      'Four Weibull/lognormal fits and one marginal Turnbull curve. No resampling, ML or model selection.'),file.path(share,'versions.txt'))
    writeLines(c('Return only SEND_BACK. All original diagnostics, import reports, graphics and detailed logs remain LOCAL_ONLY.',
      'model_fit_summary.csv and survival_fit.csv reproduced the historical results within 1e-8.',
      'Calibration bands, group counts, status and unweighted predicted risks reproduced historical values.',
      'Only IPCW observed calibration risks and weight diagnostics were allowed to change; rounding can hide a change.',
      'Calibration uses fixed risk bands and Hajek-weighted observed risk. These are apparent diagnostics without confidence intervals.',
      'No main prediction bootstrap, ML benchmark, association analysis or simulation was rerun.',
      'Source hashes cover code and aggregate references, not private raw data or independently saved person identities.',
      'Manuscript integration and visual review of the private diagnostic PNG remain separate.'),file.path(share,'READ_ME.txt'))
    writeLines('SUCCESS: conventional IPCW diagnostics refreshed and unchanged model/calibration quantities verified.',status)
    message('Finished. Return only: ',share);invisible(out)
  },error=function(e) {
    cat('\nDiagnostics refresh failure: ',conditionMessage(e),'\n',file=log,append=TRUE)
    writeLines('INCOMPLETE: diagnostics refresh failed. Do not use partial outputs. Details remain LOCAL_ONLY.',status)
    stop('Diagnostics refresh failed. Inspect LOCAL_ONLY/diagnostics.log. No successful corrected result is available.',call.=FALSE)
  })
}
if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  tryCatch(diagnostics_refresh_main(commandArgs(trailingOnly=TRUE),dirname(normalizePath(script))),
    error=function(e) {message('Diagnostics refresh stopped: ',conditionMessage(e));quit(status=1L)})
}
