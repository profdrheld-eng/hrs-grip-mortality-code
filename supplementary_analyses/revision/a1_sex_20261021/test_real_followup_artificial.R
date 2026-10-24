# Real public cohort/follow-up functions, entirely invented observations.
args<-commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1L)
e<-new.env(parent=globalenv())
sys.source(file.path(args[1],'export_a1_sex.R'),envir=e)
scripts<-normalizePath(file.path(args[1],'../../implementation/analysis-work/02_Analyse/scripts'))
e$a1_verify_sources(scripts)
for(name in c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R','hrs_analysis.R'))
  sys.source(file.path(scripts,name),envir=e)
e$fit_pair<-function(...) stop('Unexpected fit in artificial cohort test.')
wave<-function(prefix,n) {
  x<-data.frame(key=paste0('invented_',seq_len(n)))
  for(v in c('I816','I851','I852','I853')) x[[paste0(prefix,v)]]<-30
  x[[paste0(prefix,'I817')]]<-1
  x
}
t<-data.frame(key=paste0('invented_',1:8),HHID=paste0('invented_house_',1:8),
  OAGE=c(60,60,60,60,49,60,60,60),OVHHID='000000',SEX=c(1,2,1,2,1,NA,1,2),
  OIWYEAR=2014,OIWMONTH=6,MIWYEAR=2010,MIWMONTH=6,
  NYEAR=NA_real_,NMONTH=NA_real_,EXDEATHYR=NA_real_,EXDEATHMO=NA_real_,EXDODSOURCE=NA_real_,
  KNOWNDECEASEDYR=NA_real_,KNOWNDECEASEDMO=NA_real_,KNOWNDECEASEDSOURCE=NA_real_,
  LASTALIVEYR=2019,LASTALIVEMO=7,LASTALIVESOURCE=1)
t$NYEAR[2]<-2016;t$NMONTH[2]<-2;t$LASTALIVEYR[2]<-2015
t$LASTALIVESOURCE[3]<-4
t$MIWYEAR[4]<-2015
c14<-data.frame(key=t$key,OC117=1,OC116=1,OC001=c(1,2,3,4,2,2,NA,2))
pr<-data.frame(key=t$key,OZ076=1,OZ205=1)
h14<-wave('O',8)
for(v in c('OI816','OI851','OI852','OI853')) h14[[v]][8]<-NA
imp<-list(core=list(h10i_r=wave('M',9),h14i_r=h14,h14c_r=c14,h14pr_r=pr),tracker=t)
x<-e$a1_prepare(imp,expected_n=2L,enforce_reference=FALSE)
stopifnot(identical(x$included,c(TRUE,TRUE,rep(FALSE,7))),
  is.na(x$sex[6]),is.na(x$sex[9]))
follow<-e$make_followup(t,e$month_index(t$OIWYEAR,t$OIWMONTH)+.5)
stopifnot(is.infinite(follow$hi[1]),is.finite(follow$hi[2]),
  follow$reason[3]=='insufficient_followup')
# A last-alive observation after the selected death interval triggers exclusion.
imp$tracker$LASTALIVEYR[2]<-2019
y<-e$a1_prepare(imp,expected_n=1L,enforce_reference=FALSE)
stopifnot(identical(y$included,c(TRUE,rep(FALSE,8))))
# Existing source identity passes; a changed public code copy must fail before import.
fixture<-tempfile('public_code_identity_');dir.create(fixture)
names<-c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R','hrs_analysis.R')
stopifnot(all(file.copy(file.path(scripts,names),fixture)))
e$a1_verify_sources(fixture)
cat('\n# Invented mutation for identity-guard test.\n',file=file.path(fixture,names[1]),append=TRUE)
stopifnot(inherits(tryCatch(e$a1_verify_sources(fixture),error=identity),'error'))
unlink(fixture,recursive=TRUE)
cat('PASS: actual public follow-up functions, invented survivor/death/insufficient-follow-up/conflict/timing/covariate/grip exclusions, unmatched sex and changed-source rejection.\n')
