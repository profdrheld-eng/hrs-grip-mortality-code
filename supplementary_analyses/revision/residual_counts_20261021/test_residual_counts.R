# Only invented observations and public source functions.
a<-commandArgs(trailingOnly=TRUE);stopifnot(length(a)==2L)
e<-new.env(parent=globalenv());sys.source(file.path(a[1],'export_residual_counts.R'),envir=e)
for(name in c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R','hrs_analysis.R')) sys.source(file.path(a[2],name),envir=e)
e$fit_pair<-function(...) stop('A fit must never be called.')
t<-data.frame(key=paste0('invented_',1:4),OIWYEAR=2014,OIWMONTH=6,NYEAR=c(NA,2019,NA,2016),NMONTH=c(NA,2,NA,1),EXDEATHYR=NA_real_,EXDEATHMO=NA_real_,EXDODSOURCE=NA_real_,KNOWNDECEASEDYR=NA_real_,KNOWNDECEASEDMO=NA_real_,KNOWNDECEASEDSOURCE=NA_real_,LASTALIVEYR=c(2018,2018,2020,2015),LASTALIVEMO=6,LASTALIVESOURCE=1)
w10<-data.frame(key=t$key,valid_trials=c(4,4,3,4))
w14<-data.frame(key=t$key,valid_trials=c(4,2,4,3))
x<-e$residual_counts(w10,w14,t)
stopifnot(identical(unname(x),c(3L,2L,1L,1L)))
published<-c(four_valid_2010=6020L,four_valid_2014=5920L,early_last_alive=180L,early_horizon_overlap=60L)
stopifnot(identical(e$residual_release(published,6307L)$count_rounded_10,as.character(published)))
small<-published;small[4]<-5L
stopifnot(all(e$residual_release(small,6307L)$count_rounded_10=='SUPPRESSED'))
near_total<-published;near_total[1]<-6302L
stopifnot(all(e$residual_release(near_total,6307L)$count_rounded_10=='SUPPRESSED'))
invalid<-tryCatch(e$residual_release(c(published,-1L),6307L),error=identity);stopifnot(inherits(invalid,'error'))
# End-to-end release and cleanup with an injected invented importer. Public source
# hashes and all actual preparation/follow-up functions remain in force.
root<-tempfile('invented_residual_');dir.create(root)
for(folder in c('data','out','out_fail','a1_sex_20261021','residual_counts107')) dir.create(file.path(root,folder))
wave<-function(prefix,n) {
 x<-data.frame(key=paste0('invented_',seq_len(n)))
 for(v in c('I816','I851','I852','I853')) x[[paste0(prefix,v)]]<-30
 x[[paste0(prefix,'I817')]]<-1
 x
}
bigtracker<-t[rep(3L,8690L),,drop=FALSE]
bigtracker$key<-paste0('invented_',1:8690);bigtracker$HHID<-paste0('house_',1:8690)
bigtracker$OAGE<-60;bigtracker$OVHHID<-'000000';bigtracker$SEX<-rep(1:2,4345)
bigtracker$MIWYEAR<-2010;bigtracker$MIWMONTH<-6
big<-list(tracker=bigtracker,core=list(h10i_r=wave('M',8690),h14i_r=wave('O',6307),
 h14c_r=data.frame(key=bigtracker$key,OC117=1,OC116=1,OC001=2),
 h14pr_r=data.frame(key=bigtracker$key,OZ076=1,OZ205=1)))
e$sys.source<-function(file,envir,...) {
 base::sys.source(file,envir=envir,...)
 if(basename(file)=='hrs_stage2.R') envir$stage2_main<-function(args,helper) big
}
success<-e$residual_main(c(file.path(root,'data'),file.path(root,'out'),a[2]),a[1])
stopifnot(setequal(list.files(file.path(success,'SEND_BACK')),
 c('STATUS.txt','DISCLOSURE_CHECK.txt','README.txt','source_hashes.csv','residual_counts.csv')))
released<-read.csv(file.path(success,'SEND_BACK','residual_counts.csv'),colClasses='character')
stopifnot(nrow(released)==4L,all(released$count_rounded_10=='SUPPRESSED'),
 all(released$reference_comparison=='NOT_ASSESSED'),!any(grepl('invented_',unlist(released))))
stopifnot(file.copy(file.path(a[1],'../a1_sex_20261021/export_a1_sex.R'),file.path(root,'a1_sex_20261021/export_a1_sex.R')))
failure<-tryCatch(e$residual_main(c(file.path(root,'data'),file.path(root,'out_fail'),a[2]),file.path(root,'residual_counts107')),error=identity)
stopifnot(inherits(failure,'error'))
failed<-list.dirs(file.path(root,'out_fail'),recursive=FALSE,full.names=TRUE)
stopifnot(length(failed)==1L,identical(list.files(file.path(failed,'SEND_BACK')),'STATUS.txt'),
 startsWith(readLines(file.path(failed,'SEND_BACK','STATUS.txt')),'FAILED'))
unlink(root,recursive=TRUE)
cat('PASS: invented four-trial and exact historical censoring formulas, small/complement suppression and invalid input rejection; no fits.\n')
