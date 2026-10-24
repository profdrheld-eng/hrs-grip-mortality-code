# HUMAN-RUN ONLY. Four residual descriptive counts; no models or checkpoints.
residual_counts <- function(w10,w14,t) {
  if(anyNA(t$key)||anyDuplicated(t$key)) stop('Invalid cohort keys.')
  a<-w10$valid_trials[match(t$key,w10$key)]
  b<-w14$valid_trials[match(t$key,w14$key)]
  if(anyNA(c(a,b))) stop('Missing wave mapping.')
  origin<-month_index(t$OIWYEAR,t$OIWMONTH)+.5
  follow<-make_followup(t,origin)
  if(any(follow$reason!='included')||anyNA(follow$lo)) stop('Follow-up cohort mismatch.')
  source<-select_death_source(t)
  raw_hi<-(source$upper+1-origin)/12
  raw_lo<-(source$lower-origin)/12
  crossing<-!is.na(raw_lo)&raw_lo<5&!is.na(raw_hi)&raw_hi>5
  early<-!is.finite(follow$hi)&follow$lo<5
  c(four_valid_2010=sum(a==4),four_valid_2014=sum(b==4),
    early_last_alive=sum(early&is.na(source$lower)),
    early_horizon_overlap=sum(early&crossing))
}

residual_release <- function(counts,n) {
  expected<-c('four_valid_2010','four_valid_2014','early_last_alive','early_horizon_overlap')
  if(!identical(names(counts),expected)||length(n)!=1L||!is.finite(n)||n<1||n!=floor(n)||
     anyNA(counts)||any(!is.finite(counts))||any(counts<0|counts>n|counts!=floor(counts)))
    stop('Invalid descriptive aggregate counts.')
  # Release no redundant margins. Joint suppression also protects small complements.
  shown<-if(any(counts<10|n-counts<10)) rep('SUPPRESSED',4) else as.character(10*floor(counts/10+.5))
  data.frame(metric=names(counts),count_rounded_10=shown,stringsAsFactors=FALSE)
}

residual_main <- function(args,here) {
  if(length(args)!=3L||!all(dir.exists(args))) stop('Supply DATA_DIR OUTPUT_PARENT CORRECTED_SCRIPTS.')
  args<-vapply(args,normalizePath,'',mustWork=TRUE)
  if(args[2]==args[1]||startsWith(args[2],paste0(args[1],'/'))) stop('Separate output required.')
  helper<-normalizePath(file.path(here,'../a1_sex_20261021/export_a1_sex.R'),mustWork=TRUE)
  if(unname(tools::md5sum(helper))!='a66429be18ac4e81e7e61dec5fce6c7f') stop('A1 helper identity mismatch.')
  env<-new.env(parent=environment(residual_main))
  sys.source(helper,envir=env)
  expected<-env$a1_verify_sources(args[3])
  for(name in names(expected)) sys.source(file.path(args[3],name),envir=env)
  env$fit_pair<-function(...) stop('Model fitting prohibited in this export.')
  environment(residual_counts)<-env
  out<-file.path(args[2],paste0('residual_counts_',format(Sys.time(),'%Y%m%d_%H%M%S'),'_',Sys.getpid()))
  if(!dir.create(out)) stop('Cannot create a fresh output directory.')
  private<-file.path(out,'LOCAL_ONLY');share<-file.path(out,'SEND_BACK')
  stopifnot(dir.create(private),dir.create(share))
  writeLines('INCOMPLETE: do not share.',file.path(share,'STATUS.txt'))
  cat('Private run directory: ',out,'\n',sep='')
  log<-file(file.path(private,'export.log'),'wt');oldwarn<-getOption('warn');options(warn=1)
  sink(log);sink(log,type='message')
  ok<-tryCatch({
    imported<-env$stage2_main(c(args[1],private),file.path(args[3],'hrs_feasibility.R'))
    selected<-env$a1_prepare(imported) # Existing exact N6307 / rounded8690/6310/2380 guards.
    a<-env$wave_summary(imported$core$h10i_r,'M')
    b<-env$wave_summary(imported$core$h14i_r,'O')
    keys<-a$key[a$valid_trials>0][selected$included]
    stopifnot(length(keys)==6307L,!anyNA(keys),!anyDuplicated(keys))
    tracker<-imported$tracker[match(keys,imported$tracker$key),,drop=FALSE]
    counts<-residual_counts(a,b,tracker)
    released<-residual_release(counts,length(keys))
    reference<-c(6020L,5920L,180L,60L)
    released$published_rounded_reference<-as.character(reference)
    released$reference_comparison<-ifelse(released$count_rounded_10=='SUPPRESSED','NOT_ASSESSED',
      ifelse(released$count_rounded_10==released$published_rounded_reference,'MATCH','MISMATCH'))
    write.csv(released,file.path(share,'residual_counts.csv'),row.names=FALSE)
    paths<-c(file.path(args[3],names(expected)),helper,file.path(here,'export_residual_counts.R'))
    hashes<-unname(tools::md5sum(paths));if(anyNA(hashes)) stop('Missing public-code provenance.')
    write.csv(data.frame(file=basename(paths),md5=hashes),file.path(share,'source_hashes.csv'),row.names=FALSE)
    writeLines(c('Four descriptive counts only, same analytical-key definition and exactN6307 guard as the completed A1 export.',
      'Four-trial counts use clean valid_trials==4 separately in each wave, in the final analytical cohort.',
      'Early censoring: infinite upper follow-up bound and lower bound<5years.',
      'Last-alive category: early censoring with no eligible selected death lower bound.',
      'Horizon-overlap category: early censoring with raw death lower<5 and raw death upper>5.',
      'Definitions match public hrs_diagnostics.R. No fit, bootstrap, checkpoint or prediction loading.',
      'Counts rounded to nearest10,half-up. If any count or complement is<10,all four counts are suppressed.',
      'Published numbers are comparison targets only,never substituted for computed results.',
      'SUCCESS means the calculation/export completed. MISMATCH requires review and is not a reporting PASS.',
      'No separate censoring total, percentages or small-category residuals exported.',
      'Suppression is not a universal anonymity guarantee. Review SEND_BACK locally before sharing; LOCAL_ONLY stays private.'),file.path(share,'README.txt'))
    writeLines('PASS: four-count/complement suppression rule applied; no additional margins.',file.path(share,'DISCLOSURE_CHECK.txt'))
    writeLines('SUCCESS: descriptive export completed; inspect MATCH/MISMATCH separately.',file.path(share,'STATUS.txt'))
    TRUE
  },error=function(condition) {
    message(conditionMessage(condition))
    unlink(file.path(share,c('residual_counts.csv','source_hashes.csv','README.txt','DISCLOSURE_CHECK.txt')))
    writeLines('FAILED: no release. Details stay LOCAL_ONLY/export.log.',file.path(share,'STATUS.txt'))
    FALSE
  })
  sink(type='message');sink();close(log);options(warn=oldwarn)
  if(!ok) stop('Export failed. Inspect the private log locally; do not share it.',call.=FALSE)
  cat('SUCCESS. Review and return only: ',share,'\n',sep='')
  invisible(out)
}

if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  residual_main(commandArgs(trailingOnly=TRUE),dirname(normalizePath(script)))
}
