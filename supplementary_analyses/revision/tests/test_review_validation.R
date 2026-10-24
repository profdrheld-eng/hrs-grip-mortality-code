# Numerical integration against the unchanged corrected primary implementation.
args<-commandArgs(trailingOnly=TRUE);root<-args[1];original<-args[2]
source(file.path(original,'hrs_analysis.R'));source(file.path(original,'hrs_prediction_validation.R'))
source(file.path(root,'scripts','hrs_review_analyses.R'))
set.seed(712);n<-400
sex<-factor(rep(1:2,n/2));g<-rnorm(n,32-7*(as.integer(sex)-1),7)
time<-rweibull(n,1.4,exp(2.3+.018*g-.025*(runif(n,50,90)-65)))
d<-data.frame(lo=ifelse(time<5,pmax(.0001,floor(time*12)/12),5),
 hi=ifelse(time<5,ceiling(time*12)/12,Inf),age=runif(n,50,90),sex=sex,
 smoke=factor(sample(c('never','former','current'),n,TRUE)),health=factor(sample(1:5,n,TRUE)),
 grip14=g,grip10=g+rnorm(n,2,5),household=as.character(rep(seq_len(n/2),each=2)))
d$hi[d$hi>5]<-Inf
allfits<-review_fit(d)
stopifnot(length(allfits)==8,all(is.finite(review_metrics(allfits,d))))
forms<-review_formulas()[c('current','prior')]
set.seed(20260926);old<-clinical_validation(d,fit_pair(d),B=20,inner=20)
set.seed(20260926);new<-review_validate(d,B=20,inner=20,formulas=forms,groups='all')$metrics
mapped<-ifelse(new$target=='prior_vs_current','prior_minus_current',new$target)
for(i in seq_len(nrow(new))) {
 ref<-old[old$metric==new$metric[i]&old$target==mapped[i],]
 stopifnot(nrow(ref)==1)
 for(col in c('apparent','optimism_corrected','ci_lower','ci_upper','outer_success','nested_success'))
   stopifnot(isTRUE(all.equal(new[i,col],ref[[col]],tolerance=1e-12)))
}
cat('PASS: all eight model families fit artificial data; generalized bootstrap matches original paired validation\n')
# Interrupt after one completed outer draw; resuming must preserve the exact RNG trajectory.
checkpoint<-tempfile(fileext='.rds')
original_sampler<-sample_households;calls<-0L
sample_households<-function(d) {calls<<-calls+1L;if(calls==22L) stop('ARTIFICIAL INTERRUPTION');original_sampler(d)}
set.seed(20260926)
interrupted<-tryCatch(review_validate(d,B=20,inner=20,formulas=forms,groups='all',checkpoint=checkpoint),error=identity)
stopifnot(inherits(interrupted,'error'),file.exists(checkpoint),readRDS(checkpoint)$completed==1L)
sample_households<-original_sampler
resumed<-review_validate(d,B=20,inner=20,formulas=forms,groups='all',checkpoint=checkpoint)$metrics
stopifnot(isTRUE(all.equal(resumed,new,tolerance=1e-12)))
modified<-d;modified$age[1]<-modified$age[1]+.1
wrong<-tryCatch(review_validate(modified,B=20,inner=20,formulas=forms,groups='all',checkpoint=checkpoint),error=identity)
stopifnot(inherits(wrong,'error'));unlink(checkpoint)
cat('PASS: interrupted validation resumes exactly and rejects changed input\n')
