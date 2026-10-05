source('scripts/hrs_stage2.R')
source('scripts/hrs_source_hierarchy.R')
source('scripts/hrs_analysis.R')
t <- data.frame(NYEAR=c(2018,NA,2019,2020),NMONTH=c(1,NA,2,1),
 EXDEATHYR=NA_real_,EXDEATHMO=NA_real_,EXDODSOURCE=NA_real_,
 KNOWNDECEASEDYR=NA_real_,KNOWNDECEASEDMO=NA_real_,KNOWNDECEASEDSOURCE=NA_real_,
 LASTALIVEYR=c(2017,2016,2018,2018),LASTALIVEMO=1,LASTALIVESOURCE=1)
f <- make_followup(t,rep(month_index(2014,6)+.5,4))
stopifnot(all(f$reason=='included'),is.finite(f$hi[1]),is.infinite(f$hi[2]),
 f$lo[2]<5,is.infinite(f$hi[3]),f$lo[3]<5,f$lo[4]==5,is.infinite(f$hi[4]))
t$NYEAR[1] <- 2014; t$NMONTH[1] <- 2
stopifnot(make_followup(t,rep(month_index(2014,6)+.5,4))$reason[1]!='included')
set.seed(7); n <- 600
d <- data.frame(household=rep(seq_len(n/2),each=2),age=runif(n,50,85),
 sex=factor(sample(1:2,n,TRUE)),smoke=factor(sample(c('never','former','current'),n,TRUE)),
 health=factor(sample(1:5,n,TRUE)),grip14=rnorm(n,30,6),grip10=rnorm(n,32,6))
time <- rweibull(n,1.3,exp(2-.02*(d$age-65)-.02*(d$grip10-32)))
d$lo <- pmin(5,pmax(.001,floor(time*12)/12)); d$hi <- ifelse(time<5,d$lo+1/12,Inf)
p <- fit_pair(d)
for(fit in p) stopifnot(abs(interval_score(fit,d)*n-as.numeric(logLik(fit)))<1e-5)
v <- validate_pair(d,p,20)
stopifnot(v$successful>=18,all(is.finite(v$corrected)))
pn <- fit_pair(d,'lognormal')
stopifnot(abs(interval_score(pn$current,d)*n-as.numeric(logLik(pn$current)))<1e-5)
cat('Synthetic interval/censoring, model-likelihood and household-bootstrap checks passed.\n')
source('tests/test_stage2.R') # Creates synthetic files only, including a one-person cohort.
t$OAGE <- 60
write.csv(t,file.path(root,'trk2022tr_r.csv'),row.names=FALSE)
analysis_main(c(root,output,'20'),normalizePath('scripts'))
comparison <- read.csv(file.path(output,'analysis_v1','model_comparison.csv'))
stopifnot(nrow(comparison)==9,
 all(comparison$status=='INSUFFICIENT_FOR_PLANNED_MODEL'),
 startsWith(readLines(file.path(output,'analysis_v1','STATUS.txt'))[1],'INCOMPLETE'))
cat('Synthetic full command workflow and insufficient-sample gate passed.\n')
