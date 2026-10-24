# All fixtures are artificial. No HRS paths are accepted by this test.
args <- commandArgs(trailingOnly=TRUE)
root <- if(length(args)) args[1] else 'revision_20261019'
source(file.path(root,'scripts','hrs_review_analyses.R'))
expect_error <- function(expr) stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),'error'))
forms <- review_formulas()
stopifnot(identical(names(forms),c('covariates','current','prior','joint','current_sex','prior_sex','current_no_srh','prior_no_srh')))
stopifnot(!'grip14' %in% all.vars(forms$covariates),'grip10' %in% all.vars(forms$prior),
          !'health' %in% all.vars(forms$prior_no_srh))
# Model matrices encode 9 joint spline products and sex interactions without changing main effects.
set.seed(19)
n<-240
d<-data.frame(lo=runif(n,.2,4),hi=Inf,age=runif(n,50,90),
 sex=factor(rep(1:2,n/2)),smoke=factor(rep(c('never','former','current'),length.out=n)),
 health=factor(rep(1:5,length.out=n)),grip14=runif(n,8,60),grip10=runif(n,8,65),
 household=as.character(rep(seq_len(n/2),each=2)))
d$hi[seq_len(80)]<-d$lo[seq_len(80)]+.1
cols<-vapply(forms,function(f)ncol(model.matrix(delete.response(terms(f)),d)),0L)
stopifnot(cols['joint']-cols['prior']==9,cols['current_sex']-cols['current']==3,
          cols['prior_sex']-cols['prior']==6)
# Include the recovery group and use strict less-than cutoffs, including boundary values.
z<-review_transitions(c(27,26,16,15),c(26,27,15,16),factor(c(1,1,2,2)))
stopifnot(identical(as.character(z),c('not_low_to_low','low_to_not_low','not_low_to_low','low_to_not_low')))
expect_error(review_transitions(20,20,3))
# Strict greater-than 2 percentage points, not >=; retain signs and denominator.
r<-review_risk_changes(c(.1,.1,.1,.1),c(.12,.121,.079,.1),threshold=.02)
stopifnot(r$n==4,r$n_above==2,r$n_increase==1,r$n_decrease==1,r$proportion==.5)
expect_error(review_risk_changes(c(.1,NA),c(.2,.3)))
expect_error(review_risk_changes(.1,1.1))
# Common paired bootstrap draws, including direct difference between subgroup increments.
a<-array(NA_real_,c(30,2,2),dimnames=list(NULL,c('auc','brier'),c('current','prior')))
a[,1,1]<-.8;a[,1,2]<-seq(.801,.804,length.out=30);a[,2,1]<-.1;a[,2,2]<-.099
s<-review_interval(a[,1,2]-a[,1,1],30)
stopifnot(s$status=='OK',s$lower>0,s$upper<.005)
a[1:4,1,2]<-NA
stopifnot(review_interval(a[,1,2]-a[,1,1],30)$status=='UNSTABLE')
# Category counts alone cannot disclose suppressed small cells through an exact complement.
q<-review_release_counts(c(total=100,a=96,b=4))
stopifnot(q$count_rounded_10[q$metric=='b']=='SUPPRESSED',sum(q$count_rounded_10=='SUPPRESSED')>=2)
# Classify absent examinations conservatively and distinguish structurally ineligible participants.
f<-review_selection_reason(c(TRUE,TRUE,TRUE,TRUE,FALSE),c(TRUE,TRUE,FALSE,TRUE,FALSE),
 c(FALSE,FALSE,FALSE,FALSE,TRUE),c(1,1,1,5,NA),c(NA,4,NA,NA,NA),c(TRUE,FALSE,FALSE,FALSE,FALSE))
stopifnot(identical(f,c('included','documented_unable','no_core_interview','physical_measure_ineligible','death_before_2014')))
cat('PASS: review analysis specifications, transitions, disclosure and uncertainty tests\n')

f<-review_selection_reason(c(TRUE,TRUE),c(FALSE,TRUE),c(FALSE,FALSE),c(NA,NA),c(NA,NA),c(FALSE,FALSE),
 death_during_wave=c(TRUE,FALSE),not_scheduled=c(FALSE,TRUE))
stopifnot(identical(f,c('documented_death_2014_2015_without_core','not_scheduled_for_2014_physical_measures')))
