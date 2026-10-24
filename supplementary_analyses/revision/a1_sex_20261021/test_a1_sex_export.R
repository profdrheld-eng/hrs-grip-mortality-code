# Artificial data only. No private paths, files, fits or model objects.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1L)
e <- new.env(parent=globalenv())
sys.source(file.path(args[1],'export_a1_sex.R'),envir=e)
failed <- function(expr) inherits(tryCatch({force(expr);NULL},error=identity),'error')
stopifnot(identical(e$a1_release(c(total=105,male=55,female=50,missing_or_invalid=0))$count_rounded_10,
  c('110','60','SUPPRESSED','SUPPRESSED')))
stopifnot(identical(e$a1_release(c(total=100,male=5,female=90,missing_or_invalid=5))$count_rounded_10,
  c('100','SUPPRESSED','90','SUPPRESSED')))
stopifnot(failed(e$a1_release(c(total=NA,male=1))))
stopifnot(failed(e$a1_release(c(total=2,male=-1))))
x <- e$a1_sex_table(c(rep(1,35),rep(2,40),NA,9), c(rep(TRUE,35),rep(FALSE,42)))
stopifnot(identical(names(x),c('group','metric','count_rounded_10')),
 nrow(x)==8L,all(x$count_rounded_10[x$metric=='missing_or_invalid']=='SUPPRESSED'))
stopifnot(failed(e$a1_sex_table(1:2,c(TRUE,NA))))
stopifnot(failed(e$a1_sex_table(1:2,TRUE)))
stopifnot(failed(e$a1_denominator_guard(100L,70L)))
# Adapter uses real public grip/date/smoking helpers, but invented follow-up flags.
scripts <- file.path(args[1],'../../implementation/analysis-work/02_Analyse/scripts')
for (name in c('hrs_feasibility.R','hrs_stage2.R')) sys.source(file.path(scripts,name),envir=e)
e$make_followup <- function(z,start) data.frame(lo=rep(5,nrow(z)),hi=rep(Inf,nrow(z)),reason='included')
wave <- function(prefix,n) {
 d <- data.frame(key=paste0('fictional_',seq_len(n)))
 for(v in c('I816','I851','I852','I853')) d[[paste0(prefix,v)]] <- rep(30,n)
 d[[paste0(prefix,'I817')]] <- 1
 d
}
t <- data.frame(key=paste0('fictional_',1:8),HHID=paste0('house_',1:8),OAGE=c(60,60,60,60,49,60,60,60),
 OVHHID='000000',SEX=c(1,2,1,2,1,NA,1,2),OIWYEAR=2014,OIWMONTH=6,MIWYEAR=2010,MIWMONTH=6)
c14 <- data.frame(key=t$key,OC117=1,OC116=1,OC001=c(1,2,3,4,2,2,NA,2))
p14 <- data.frame(key=t$key,OZ076=1,OZ205=1)
h14 <- wave('O',8);h14$OI816[8]<-NA;h14$OI851[8]<-NA;h14$OI852[8]<-NA;h14$OI853[8]<-NA
imp <- list(core=list(h10i_r=wave('M',9),h14i_r=h14,h14c_r=c14,h14pr_r=p14),tracker=t)
y <- e$a1_prepare(imp,expected_n=4L,enforce_reference=FALSE)
stopifnot(identical(y$included,c(rep(TRUE,4),rep(FALSE,5))),is.na(y$sex[9]))
stopifnot(failed(e$a1_prepare(imp)))
# Exact reference denominators and source integrity must reject changed inputs.
stopifnot(isTRUE(e$a1_denominator_guard(8690L,6307L)))
stopifnot(failed(e$a1_verify_sources(tempdir())))
# Full release/failure paths use a wholly invented import in temporary directories.
testroot<-tempfile('a1_artificial_');dir.create(testroot)
on.exit_cleanup<-function() unlink(testroot,recursive=TRUE)
for(folder in c('data','out','code')) dir.create(file.path(testroot,folder))
for(name in c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R','hrs_analysis.R'))
 writeLines('# Invented test fixture; not HRS data or code.',file.path(testroot,'code',name))
# Test-only injected public-code check and importer. Production has no bypass argument.
e$a1_verify_sources<-function(scripts) c(hrs_feasibility.R='fixture',hrs_stage2.R='fixture',
 hrs_source_hierarchy.R='fixture',hrs_analysis.R='fixture')
big<-local({
 tracker<-data.frame(key=paste0('invented_',1:8690),HHID=paste0('invented_house_',1:8690),
 OAGE=60,OVHHID='000000',SEX=rep(1:2,length.out=8690),OIWYEAR=2014,OIWMONTH=6,MIWYEAR=2010,MIWMONTH=6)
 oldwave<-wave('M',8690);oldwave$key<-tracker$key
 newwave<-wave('O',6307);newwave$key<-tracker$key[1:6307]
 list(tracker=tracker,core=list(h10i_r=oldwave,h14i_r=newwave,
 h14c_r=data.frame(key=tracker$key,OC117=1,OC116=1,OC001=2),
 h14pr_r=data.frame(key=tracker$key,OZ076=1,OZ205=1)))
})
writeLines('stage2_main<-function(args,helper) get("big",envir=.GlobalEnv)',
 file.path(testroot,'code','hrs_stage2.R'))
success_out<-tryCatch(e$a1_export_main(file.path(testroot,c('data','out','code')),args[1]),error=function(err) {
 for(log in list.files(file.path(testroot,'out'),pattern='export.log',recursive=TRUE,full.names=TRUE)) cat(readLines(log),sep='\n')
 stop(err)
})
stopifnot(setequal(list.files(file.path(success_out,'SEND_BACK')),
 c('selection_sex.csv','source_hashes.csv','README.txt','STATUS.txt','DISCLOSURE_CHECK.txt')))
released<-read.csv(file.path(success_out,'SEND_BACK','selection_sex.csv'),colClasses='character')
stopifnot(nrow(released)==8L,!any(grepl('invented_',unlist(released))),
 startsWith(readLines(file.path(success_out,'SEND_BACK','STATUS.txt')),'SUCCESS'))
# Force a failure after CSV creation; partially written results must be removed.
dir.create(file.path(testroot,'out_fail'))
stopifnot(failed(e$a1_export_main(file.path(testroot,c('data','out_fail','code')),file.path(testroot,'missing_helper'))))
failed_runs<-list.dirs(file.path(testroot,'out_fail'),recursive=FALSE,full.names=TRUE)
stopifnot(length(failed_runs)==1L,
 identical(list.files(file.path(failed_runs,'SEND_BACK')),'STATUS.txt'),
 startsWith(readLines(file.path(failed_runs,'SEND_BACK','STATUS.txt')),'FAILED'))
on.exit_cleanup()
cat('PASS: artificial suppression, rounding, invalid inputs, cohort membership, missing/unmatched sex, exact-N/denominator and provenance guards.\n')
