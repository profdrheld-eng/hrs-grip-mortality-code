# HUMAN-RUN ONLY. Never run against HRS data through an AI tool.
# DATA_DIR OUTPUT_PARENT MATERIALIZED_CORRECTED_SCRIPTS [EXISTING_RUN_TO_RESUME]
review_private_main <- function(args,directory) {
  if(!length(args)||identical(args,'--help')) {
    cat('Human-run only: Rscript --vanilla run_review_private.R DATA_DIR OUTPUT_PARENT CORRECTED_SCRIPTS [RUN_TO_RESUME]\n')
    return(invisible(NULL))
  }
  if(!length(args) %in% 3:4||!all(dir.exists(args[1:3]))) stop('Supply three existing directories, optionally a prior run.')
  args[1:3]<-vapply(args[1:3],normalizePath,'',mustWork=TRUE)
  if(args[2]==args[1]||startsWith(args[2],paste0(args[1],'/'))) stop('Output must be separate from input data.')
  if(!requireNamespace('survival',quietly=TRUE)) stop('Installed survival is required; no installation is automatic.')
  scripts<-args[3]
  dependencies<-c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R','hrs_analysis.R','hrs_prediction_validation.R')
  for(name in dependencies) source(file.path(scripts,name),local=FALSE)
  source(file.path(directory,'hrs_review_analyses.R'),local=FALSE)
  if(!all(vapply(c('survival','rpart','xgboost'),requireNamespace,TRUE,quietly=TRUE))) stop('Required installed packages unavailable; set R_LIBS_USER, no automatic installation.')
  input_sources<-c(file.path(scripts,c(dependencies,'hrs_ml_extension.R','hrs_grip_change.R','hrs_grip_history.R')),file.path(directory,c('hrs_review_analyses.R','hrs_review_oof.R','run_review_private.R')),
    file.path(dirname(directory),c('primary_reference.csv','selected_tuning.csv','oof_reference.csv')))
  hashes<-unname(tools::md5sum(input_sources));if(anyNA(hashes)) stop('Code provenance could not be established.')
  out<-if(length(args)==4) normalizePath(args[4],mustWork=TRUE) else file.path(args[2],paste0('manuel_review_',format(Sys.time(),'%Y%m%d_%H%M%S'),'_',Sys.getpid()))
  if(length(args)==3 && (!dir.create(out)||file.exists(file.path(out,'CONFIG.rds')))) stop('Fresh output could not be created.')
  private<-file.path(out,'LOCAL_ONLY');share<-file.path(out,'SEND_BACK')
  dir.create(private,showWarnings=FALSE);dir.create(share,showWarnings=FALSE)
  config<-list(args=args[1:3],hashes=hashes,R=as.character(getRversion()),survival=as.character(utils::packageVersion('survival')))
  config_path<-file.path(out,'CONFIG.rds')
  if(file.exists(config_path)) {if(!identical(readRDS(config_path),config)) stop('Cannot resume with changed code, runtime or paths.')} else saveRDS(config,config_path)
  cat('Private run directory: ',out,'\n',sep='')
  cat('Planned survival fits: 81638; detailed progress stays in LOCAL_ONLY/analysis.log.\n')
  writeLines('INCOMPLETE: review analysis has not passed its checks.',file.path(share,'STATUS.txt'))
  # Detailed conditions and input import reports remain inside LOCAL_ONLY.
  log<-file(file.path(private,'analysis.log'),open='at');sink(log);sink(log,type='message')
  on.exit({sink(type='message');sink();close(log)},add=TRUE)
  imported<-stage2_main(c(args[1],private),file.path(scripts,'hrs_feasibility.R'))
  core<-imported$core;t<-imported$tracker
  files<-list.files(args[1],recursive=TRUE,full.names=TRUE)
  one<-function(name) {x<-files[tolower(basename(files))==tolower(name)];if(length(x)!=1) stop('Required file absent or duplicated.');x}
  get_core<-function(name,vars) read_hrs(one(paste0(name,'.da')),one(paste0(name,'.sas')),c('HHID','PN',vars))
  baseline<-get_core('h10c_r','MC001')
  physical<-get_core('h14i_r',c('OPMELIG','OI812',paste0('OI813M',1:4)))
  raw<-read.csv(one('trk2022tr_r.csv'),colClasses='character',check.names=FALSE,na.strings=c('','NA'))
  names(raw)<-toupper(names(raw))
  if(anyDuplicated(names(raw))||!all(c('HHID','PN','MAGE','OIWTYPE') %in% names(raw))) stop('Required tracker metadata absent.')
  rk<-paste(raw$HHID,raw$PN,sep=':');if(anyDuplicated(rk)) stop('Duplicate tracker keys.')
  extra<-function(name) {
    x<-trimws(raw[[name]][match(t$key,rk)]);x[x=='']<-NA_character_
    y<-suppressWarnings(as.numeric(x));if(any(!is.na(x)&(!is.finite(y)|is.na(y)))) stop('Invalid numeric tracker field.');y
  }
  t$MAGE<-extra('MAGE');t$OIWTYPE<-extra('OIWTYPE')
  a<-wave_summary(core$h10i_r,'M');b<-wave_summary(core$h14i_r,'O')
  keys<-intersect(a$key[a$valid_trials>0],b$key[b$valid_trials>0]);keys<-keys[keys %in% t$key]
  z<-t[match(keys,t$key),];ok<-!is.na(z$OAGE)&z$OAGE>=50&z$OAGE<=120&z$OAGE==floor(z$OAGE)&z$OVHHID=='000000'
  z<-z[ok,,drop=FALSE];keys<-keys[ok]
  c14<-core$h14c_r[match(keys,core$h14c_r$key),];pr<-core$h14pr_r[match(keys,core$h14pr_r$key),]
  origin<-month_index(z$OIWYEAR,z$OIWMONTH);early<-month_index(z$MIWYEAR,z$MIWMONTH)
  follow<-make_followup(z,origin+.5)
  d<-data.frame(household=z$HHID,age=z$OAGE,sex=factor(ifelse(z$SEX %in% 1:2,z$SEX,NA),levels=1:2),
    smoke=factor(smoking_status(c14$OC117,c14$OC116,pr$OZ076,pr$OZ205),levels=c('never','former','current')),
    health=factor(ifelse(c14$OC001 %in% 1:5,c14$OC001,NA),levels=1:5),
    grip14=b$grip_max[match(keys,b$key)],grip10=a$grip_max[match(keys,a$key)],lo=follow$lo,hi=follow$hi)
  take<-!is.na(origin)&!is.na(early)&early<origin&follow$reason=='included'&complete.cases(d)
  analytical_keys<-keys[take];d<-d[take,,drop=FALSE]
  if(nrow(d)!=6307L) stop('Analytical cohort does not reproduce the exact reference N.')
  ref<-read.csv(file.path(dirname(directory),'primary_reference.csv'),stringsAsFactors=FALSE)
  fit<-fit_pair(d);observed<-pair_metrics(fit,d)
  for(metric in rownames(observed)) for(model in c('current','prior')) {
    expected<-ref$apparent[ref$metric==metric&ref$target==model]
    if(length(expected)!=1||abs(observed[metric,model]-expected)>1e-8) stop('Primary apparent reference did not reproduce.')
  }
  # A1: all valid 2010 measurements, including people outside the later target population.
  ak<-a$key[a$valid_trials>0];zz<-t[match(ak,t$key),];pp<-physical[match(ak,physical$key),]
  matched<-ak %in% t$key;source<-select_death_source(zz,'ndi_first')
  died<-!is.na(source$upper)&source$upper<month_index(2014,1)&!source$requires_review&!source$excluded
  died[is.na(died)]<-FALSE
  during<-!is.na(source$lower)&!is.na(source$upper)&source$lower>=month_index(2014,1)&
    source$upper<month_index(2016,1)&!source$requires_review&!source$excluded
  during[is.na(during)]<-FALSE
  reasons<-as.matrix(pp[paste0('OI813M',1:4)]);unable<-apply(reasons,1,function(v) any(v %in% 4))
  reason<-review_selection_reason(matched,zz$OIWTYPE %in% 1,died,pp$OPMELIG,ifelse(unable,4,NA_real_),ak %in% analytical_keys,death_during_wave=during,
    not_scheduled=ak %in% physical$key & is.na(pp$OPMELIG))
  flow<-review_release_counts(c(total=length(ak),table(factor(reason,levels=c('included','death_before_2014','unmatched_tracker',
    'documented_death_2014_2015_without_core','no_core_interview','physical_measure_ineligible',
    'not_scheduled_for_2014_physical_measures','documented_unable','no_valid_grip_or_other_exclusion')))))
  # Baseline comparisons use 2010 age/SRH, not missing 2014 values for deceased people.
  age<-zz$MAGE;age[!is.finite(age)|age<0|age>120]<-NA_real_
  health<-baseline$MC001[match(ak,baseline$key)];health[!health %in% 1:5]<-NA_real_
  grip<-a$grip_max[match(ak,a$key)];included<-ak %in% analytical_keys
  continuous<-do.call(rbind,lapply(c('age','grip'),function(variable) {
    x<-if(variable=='age') age else grip
    parts<-lapply(c(FALSE,TRUE),function(g) {
      v<-x[included==g];v<-v[is.finite(v)]
      data.frame(variable=variable,group=if(g) 'included' else 'excluded',available_rounded_10=10*floor(length(v)/10+.5),
        mean=if(length(v)>=20) mean(v) else NA_real_,sd=if(length(v)>=20) sd(v) else NA_real_)
    })
    r<-do.call(rbind,parts);r$smd_included_minus_excluded<-(r$mean[2]-r$mean[1])/sqrt(mean(r$sd^2));r
  }))
  categorical<-do.call(rbind,lapply(c(FALSE,TRUE),function(g) {
    counts<-c(total=sum(included==g),table(factor(health[included==g],levels=1:5),useNA='always'))
    names(counts)<-c('total',paste0('srh_',1:5),'missing_srh')
    r<-review_release_counts(counts);r$group<-if(g) 'included' else 'excluded';r
  }))
  correlations<-do.call(rbind,lapply(c('all','1','2'),function(group) {
    v<-if(group=='all') d else d[as.character(d$sex)==group,,drop=FALSE]
    data.frame(group=group,n_rounded_10=10*floor(nrow(v)/10+.5),pearson_r=cor(v$grip10,v$grip14))
  }))
  # Four EWGSOP2 transitions are reported, with the same selected complete cohort.
  transitions<-review_transitions(d$grip10,d$grip14,d$sex)
  trans_counts<-review_release_counts(c(total=nrow(d),table(transitions)))
  # Same paired household draws throughout all model families and sex-specific evaluation.
  RNGkind('Mersenne-Twister','Inversion','Rejection');set.seed(20261019)
  validation<-review_validate(d,B=200,inner=50,checkpoint=file.path(private,'validation_checkpoint.rds'),context=hashes)
  saveRDS(validation,file.path(private,'validation_objects.rds'))
  # A5: refit only the 30 previously selected original-training OOF models; no new tuning.
  source(file.path(scripts,'hrs_ml_extension.R'),local=FALSE)
  for(name in c('hrs_grip_change.R','hrs_grip_history.R')) source(file.path(scripts,name),local=FALSE)
  source(file.path(directory,'hrs_review_oof.R'),local=FALSE)
  dd<-prepare_change_data(imported)$data
  if(nrow(dd)!=nrow(d)||!identical(dd$household,d$household)||
     any(abs(dd$grip10-d$grip10)>1e-12)||any(abs(dd$grip14-d$grip14)>1e-12)) stop('OOF and primary rows do not agree.')
  cfg<-ml_config('full');cfg$fractions<-1
  tuning<-read.csv(file.path(dirname(directory),'selected_tuning.csv'),stringsAsFactors=FALSE)
  replay<-review_oof_replay(dd,cfg,tuning)
  expected<-read.csv(file.path(dirname(directory),'oof_reference.csv'),stringsAsFactors=FALSE)
  replay_checks<-do.call(rbind,lapply(seq_len(nrow(replay$combos)),function(j) {
    combo<-replay$combos[j,];ref<-expected[expected$model==combo$model&expected$target==combo$target&expected$arm=='original'&expected$fraction==1,]
    if(nrow(ref)!=1) stop('OOF reference missing.')
    metrics<-ml_metrics(dd,replay$risk[,j],replay$weights)
    data.frame(model=combo$model,target=combo$target,auc_error=unname(metrics['auc']-ref$auc),brier_error=unname(metrics['brier']-ref$brier))
  }))
  if(any(abs(replay_checks$auc_error)>1e-8)||any(abs(replay_checks$brier_error)>1e-8)) stop('Held-out original-training metrics failed reproduction.')
  saveRDS(replay,file.path(private,'OOF_predictions_LOCAL_ONLY.rds'))
  risk_changes<-review_oof_summary(dd,replay,B=5000)
  outputs<-list(selection_flow=flow,selection_continuous=continuous,selection_srh=categorical,
    grip_correlation=correlations,transitions=trans_counts,validation=validation$metrics,
    sex_increment_contrasts=validation$sex_increment_contrasts,likelihood=validation$likelihood,
    validation_attempts=validation$attempts,oof_risk_change=risk_changes,oof_replay_checks=replay_checks)
  if(!identical(hashes,unname(tools::md5sum(input_sources)))) stop('Sources changed during the run.')
  for(name in names(outputs)) write.csv(outputs[[name]],file.path(share,paste0(name,'.csv')),row.names=FALSE)
  write.csv(data.frame(file=basename(input_sources),md5=hashes),file.path(share,'source_hashes.csv'),row.names=FALSE)
  writeLines(c(paste('R',getRversion()),paste('survival',utils::packageVersion('survival')),
    'Seed 20261019; 200 outer x 50 inner household bootstrap; fixed eight model formulas.',
    'Additional exploratory analyses followed the original results.',
    'A1-A5 outputs provided; A5 uses real held-out predictions with reproduced reference metrics.',
    'No claims about clinical equivalence, screening frequency or causality.'),file.path(share,'README.txt'))
  writeLines(if(all(validation$metrics$status=='OK'))
    'SUCCESS: A1-A5 aggregate computations and reference reproduction checks completed.' else
    'UNSTABLE: one or more validation metrics failed the prespecified success threshold. Report instability.',file.path(share,'STATUS.txt'))
  sink(type='message');sink();close(log);on.exit(NULL)
  cat('Return only the aggregate directory: ',share,'\n',sep='')
  invisible(out)
}
if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  review_private_main(commandArgs(trailingOnly=TRUE),dirname(normalizePath(script)))
}
