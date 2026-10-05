# User-run primary IPCW correction only. Never starts ML or association analyses.
primary_refresh_source_names <- function() c('hrs_feasibility.R','hrs_stage2.R',
  'hrs_source_hierarchy.R','hrs_analysis.R','hrs_prediction_validation.R','hrs_equivalence_sensitivity.R')

primary_refresh_scenarios <- function() c('primary','core_month','agree_dates_only',
  'month_start','month_end','interview_first','exclude_conflicts','ndi_only','lognormal')

primary_refresh_load <- function(directory) {
  env<-new.env(parent=.GlobalEnv)
  for(name in primary_refresh_source_names()) sys.source(file.path(directory,name),envir=env)
  env
}

primary_refresh_grid <- function(x,expected,keys) {
  key<-function(z) do.call(paste,c(z[keys],sep='|'))
  if(nrow(x)!=nrow(expected)||anyDuplicated(key(x))||!setequal(key(x),key(expected)))
    stop('Unexpected aggregate result grid.')
}

primary_refresh_preflight <- function(args,directory) {
  if(length(args)!=3L) stop('Use DATA_DIR OUTPUT_PARENT AUDIT_SEND_BACK.')
  if(!all(dir.exists(args))) stop('All three input/output-parent directories must already exist.')
  args<-vapply(args,normalizePath,'',mustWork=TRUE)
  if(args[2]==args[1]||startsWith(args[2],paste0(args[1],'/'))||
     args[2]==args[3]||startsWith(args[2],paste0(args[3],'/')))
    stop('Output parent must be separate from data and audit inputs.')
  if(!requireNamespace('survival',quietly=TRUE)) stop('Installed survival package required; no automatic installation.')
  audit<-args[3]
  complete<-readLines(file.path(audit,'STATUS.txt'),warn=FALSE)
  replay<-readLines(file.path(audit,'PRIMARY_REPLAY_STATUS.txt'),warn=FALSE)
  version<-readLines(file.path(audit,'versions.txt'),warn=FALSE)
  if(!length(complete)||!startsWith(complete[1],'COMPLETE:')||
     !length(replay)||!startsWith(replay[1],'SUCCESS:')) stop('Completed weight audit and successful primary replay required.')
  if(!length(version)||version[1]!=paste('R',getRversion())) stop('R version must match the supplied audit before the long run.')
  ref<-read.csv(file.path(audit,'primary_apparent_reweighting.csv'),stringsAsFactors=FALSE)
  if(!identical(names(ref),c('model','position','metric','legacy','corrected','difference')))
    stop('Unexpected apparent-reference columns.')
  primary_refresh_grid(ref,expand.grid(model=c('current','prior'),position=c('lower','midpoint','upper'),
    metric=c('auc','brier')),c('model','position','metric'))
  if(!all(vapply(ref[c('legacy','corrected','difference')],is.numeric,TRUE))||
     any(!is.finite(as.matrix(ref[c('legacy','corrected','difference')])) )||
     any(abs(ref$corrected-ref$legacy-ref$difference)>1e-12)) stop('Invalid apparent reference values.')
  historical<-read.csv(file.path(audit,'script_hashes.csv'),stringsAsFactors=FALSE)
  if(!identical(names(historical),c('file','md5'))||anyDuplicated(historical$file)) stop('Invalid audit source manifest.')
  for(name in setdiff(primary_refresh_source_names(),'hrs_equivalence_sensitivity.R')) {
    row<-historical[historical$file==paste0('02_Analyse/scripts/',name),,drop=FALSE]
    if(nrow(row)!=1L||row$md5!=unname(tools::md5sum(file.path(directory,name))))
      stop('Analysis source does not match the supplied audit.')
  }
  list(args=args,reference=ref)
}

primary_refresh_validate <- function(path,reference) {
  status<-readLines(file.path(path,'STATUS.txt'),warn=FALSE)
  if(!length(status)||!startsWith(status[1],'SUCCESS:')) stop('Original analysis did not complete successfully.')
  read<-function(name,columns) {
    x<-read.csv(file.path(path,name),stringsAsFactors=FALSE)
    if(!identical(names(x),columns)) stop('Unexpected columns in aggregate output.')
    x
  }
  metrics<-c('brier','auc','calibration_intercept','calibration_slope','interval_log_score')
  v<-read('validated_metrics.csv',c('metric','target','apparent','optimism_corrected',
    'ci_lower','ci_upper','outer_success','nested_success','status'))
  primary_refresh_grid(v,expand.grid(metric=metrics,target=c('current','prior','prior_minus_current')),c('metric','target'))
  numeric<-c('apparent','optimism_corrected','ci_lower','ci_upper','outer_success','nested_success')
  if(!all(vapply(v[numeric],is.numeric,TRUE))||any(!is.finite(as.matrix(v[numeric])))||
     any(v$status!='OK')||any(v$ci_lower>v$ci_upper)||any(v$nested_success<180)||
     any(v$outer_success<v$nested_success)||any(v$outer_success>200)||
     any(v$outer_success!=floor(v$outer_success))||any(v$nested_success!=floor(v$nested_success)))
    stop('Primary validation is incomplete or unstable.')
  s<-read('sensitivity_metrics.csv',c('scenario','censor_time_position','model','metric','apparent'))
  primary_refresh_grid(s,expand.grid(scenario=primary_refresh_scenarios(),
    censor_time_position=c('lower','midpoint','upper'),model=c('current','prior'),metric=metrics),
    c('scenario','censor_time_position','model','metric'))
  if(!is.numeric(s$apparent)||any(!is.finite(s$apparent))) stop('Invalid sensitivity values.')
  m<-read('model_comparison.csv',c('scenario','status','current_score','prior_score',
    'delta_score','corrected_delta','bootstrap_success'))
  primary_refresh_grid(m,data.frame(scenario=primary_refresh_scenarios()),'scenario')
  if(any(m$status!='FIT_OK')||any(!is.finite(as.matrix(m[c('current_score','prior_score','delta_score')])))||
     any(abs(m$prior_score-m$current_score-m$delta_score)>1e-10)||
     any(!is.na(m$corrected_delta))||any(!is.na(m$bootstrap_success))) stop('Scenario fit checks failed.')
  for(i in seq_len(nrow(reference))) {
    r<-reference[i,]
    value<-s$apparent[s$scenario=='primary' & s$censor_time_position==r$position & s$model==r$model & s$metric==r$metric]
    if(length(value)!=1L||abs(value-r$corrected)>1e-8) stop('New apparent metrics do not match the corrected audit reference.')
  }
  for(metric in metrics) {
    z<-v[v$metric==metric,]
    for(target in c('current','prior')) {
      value<-s$apparent[s$scenario=='primary' & s$censor_time_position=='midpoint' & s$model==target & s$metric==metric]
      if(abs(value-z$apparent[z$target==target])>1e-10) stop('Apparent validation and sensitivity disagree.')
    }
    for(column in c('apparent','optimism_corrected'))
      if(abs(z[z$target=='prior',column]-z[z$target=='current',column]-z[z$target=='prior_minus_current',column])>1e-10)
        stop('Paired validation difference does not match model estimates.')
  }
  list(validated_metrics=v,sensitivity_metrics=s,model_comparison=m)
}

primary_refresh_logged <- function(fun,path) {
  con<-file(path,open='wt');sink(con);sink(con,type='message')
  on.exit({sink(type='message');sink();close(con)})
  fun()
}

primary_refresh_main <- function(args,directory) {
  directory<-normalizePath(directory,mustWork=TRUE)
  checked<-primary_refresh_preflight(args,directory);args<-checked$args
  env<-primary_refresh_load(directory)
  if(!identical(names(formals(env$analysis_main)),c('args','script_dir','extended'))||
     !is.function(env$margin_sensitivity)) stop('Unexpected existing analysis API.')
  scripts<-c(primary_refresh_source_names(),'hrs_ipcw_primary_refresh.R')
  audit_files<-c('STATUS.txt','PRIMARY_REPLAY_STATUS.txt','versions.txt','script_hashes.csv','primary_apparent_reweighting.csv')
  paths<-c(file.path(directory,scripts),file.path(args[3],audit_files))
  hashes<-data.frame(file=c(paste0('scripts/',scripts),paste0('audit/',audit_files)),md5=unname(tools::md5sum(paths)))
  if(anyNA(hashes$md5)) stop('Source provenance could not be computed before the run.')
  out<-file.path(args[2],paste0('ipcw_primary_refresh_',format(Sys.time(),'%Y%m%d_%H%M%S'),'_',Sys.getpid()))
  if(file.exists(out)||!dir.create(out)) stop('Cannot create a fresh run directory.')
  private<-file.path(out,'LOCAL_ONLY');share<-file.path(out,'SEND_BACK')
  if(!dir.create(private)||!dir.create(share)) stop('Cannot create separated output directories.')
  status<-file.path(share,'STATUS.txt');writeLines('INCOMPLETE: primary refresh has not passed all checks.',status)
  log<-file.path(private,'analysis.log')
  message('Primary refresh: 200 outer x (2 + 50 x 2) = 20,400 planned bootstrap survival fits, plus 18 scenario fits and calibration evaluations.')
  message('Seed 20260926. No association, ML or simulation run. Detailed log stays in LOCAL_ONLY.')
  tryCatch({
    result<-primary_refresh_logged(function() {
      RNGkind('Mersenne-Twister','Inversion','Rejection');set.seed(20260926)
      env$analysis_main(c(args[1],private,'200','50'),directory,extended=TRUE)
      primary_refresh_validate(file.path(private,'validation_v2'),checked$reference)
    },log)
    margins<-env$margin_sensitivity(result$validated_metrics)
    if(nrow(margins)!=18L) stop('Incomplete marginal sensitivity grid.')
    if(!identical(hashes$md5,unname(tools::md5sum(paths)))) stop('Sources or audit references changed during the run.')
    # Publish only exact aggregate schemas after every computational gate passed.
    for(name in names(result)) write.csv(result[[name]],file.path(share,paste0(name,'.csv')),row.names=FALSE)
    write.csv(margins,file.path(share,'margin_sensitivity.csv'),row.names=FALSE)
    write.csv(hashes,file.path(share,'source_hashes.csv'),row.names=FALSE)
    writeLines(c(paste('R',getRversion()),paste('survival',utils::packageVersion('survival')),
      'Seed: 20260926','RNG: Mersenne-Twister / Inversion / Rejection',
      'Outer bootstrap repetitions: 200','Inner bootstrap repetitions: 50',
      'Planned bootstrap survival fits: 20400; scenario survival fits: 18',
      'Primary only. No association, ML or simulation analyses.',
      'Margin sensitivity uses existing paired 95% intervals; no TOST or clinical-equivalence claim.',
      'All import reports, original analysis outputs and detailed logs remain in LOCAL_ONLY.'),file.path(share,'versions.txt'))
    writeLines('SUCCESS: primary IPCW refresh, all nine scenarios and corrected apparent-reference checks passed. ML correction remains separate.',status)
    message('Primary refresh finished. Return only: ',share)
    invisible(out)
  },error=function(e) {
    cat('\nRefresh failure: ',conditionMessage(e),'\n',file=log,append=TRUE)
    writeLines('INCOMPLETE: primary refresh failed. Do not use partial outputs. Details remain in LOCAL_ONLY.',status)
    stop('Primary refresh failed; inspect LOCAL_ONLY/analysis.log. No successful corrected result is available.',call.=FALSE)
  })
}

if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  tryCatch(primary_refresh_main(commandArgs(trailingOnly=TRUE),dirname(normalizePath(script))),
    error=function(e) {message('Primary refresh stopped: ',conditionMessage(e));quit(status=1L)})
}
