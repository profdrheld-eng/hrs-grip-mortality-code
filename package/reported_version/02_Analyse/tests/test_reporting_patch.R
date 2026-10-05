source('02_Analyse/scripts/hrs_reporting_patch.R')
stopifnot(identical(report_month(c(12*2010,12*2014+11)),c('2010-01','2014-12')))
d <- data.frame(lo=c(1,2,5),hi=c(2,Inf,Inf))
x <- report_followup(d)
stopifnot(abs(x$mean_years[x$measure=='event_or_censor_midpoint_proxy']-8.5/3)<1e-12,
 x$n[x$measure=='censoring_time']==2)
stopifnot(all(report_counts(c(a=1,b=40))$count=='SUPPRESSED_BLOCK'))
stopifnot(identical(report_counts(c(a=245,b=6305),half_up=TRUE)$count,c('250','6310')),
 identical(report_counts(c(a=245,b=6305))$count,c('240','6300')))
set.seed(41);n<-300
z<-data.frame(age=runif(n,50,85),grip14=runif(n,10,50),grip10=runif(n,10,50),
 sex=factor(sample(1:2,n,TRUE)),smoke=factor(sample(c('never','former','current'),n,TRUE)),
 health=factor(sample(1:5,n,TRUE)))
t<-rweibull(n,2,exp(3-.015*z$age));z$lo<-pmin(t,5);z$hi<-ifelse(t<5,t+.01,Inf)
source('02_Analyse/scripts/hrs_analysis.R');source('02_Analyse/scripts/hrs_prediction_validation.R')
a<-fit_pair(z)
for(f in a) {
 b<-report_model(f)
 stopifnot(max(abs(report_risk(b,z)-risk5(f,z)))<1e-12,
 identical(environment(b$terms),baseenv()),!any(c('y','model','residuals') %in% names(b)))
 q<-tempfile();saveRDS(b,q);stopifnot(max(abs(report_risk(readRDS(q),z)-risk5(f,z)))<1e-12);unlink(q)
}
cat('Synthetic date, follow-up, suppression and model-export round-trip checks passed.\n')
