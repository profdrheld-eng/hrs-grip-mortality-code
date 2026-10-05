source('scripts/hrs_feasibility.R')
source('scripts/hrs_stage2.R')
source('scripts/hrs_source_hierarchy.R')
source('scripts/hrs_analysis.R')
source('scripts/hrs_prediction_validation.R')
if(file.exists('scripts/hrs_grip_change.R')) source('scripts/hrs_grip_change.R')
stopifnot(exists('fit_change',mode='function'))

# A known protective association of increasing strength, with interval events
# and independent early censoring. These are generated records, not HRS data.
set.seed(271)
n <- 1200
d <- data.frame(household=rep(seq_len(n/2),each=2),age=runif(n,50,85),
 sex=factor(sample(1:2,n,TRUE)),smoke=factor(sample(c('never','former','current'),n,TRUE)),
 health=factor(sample(1:5,n,TRUE)),grip10=runif(n,20,45),gap_years=runif(n,3,5),
 change_kg=runif(n,-10,10))
d$grip14 <- d$grip10+d$change_kg
event_time <- rweibull(n,1.4,exp(2.1+.06*d$change_kg-.015*(d$age-65)))
censor_time <- ifelse(runif(n)<.15,runif(n,.5,4.5),5)
observed <- event_time<censor_time
d$lo <- ifelse(observed,pmax(.001,floor(event_time*12)/12),censor_time)
d$hi <- ifelse(observed,pmin(censor_time,d$lo+1/12),Inf)
grid <- c(-5,0,5)
fit <- fit_change(d)
stopifnot(!'grip14' %in% all.vars(formula(fit)),
 all(c('grip10','change_kg','gap_years') %in% all.vars(formula(fit))),
 abs(interval_score(fit,d)*n-as.numeric(logLik(fit)))<1e-5)
curve <- change_curve(fit,d,grid)
stopifnot(curve$risk5[1]>curve$risk5[2],curve$risk5[2]>curve$risk5[3],
 curve$risk_difference[2]==0,curve$risk_difference[1]>.04,
 all(curve$risk5>0 & curve$risk5<1))
flat <- fit; flat$coefficients[grepl('change_kg',names(coef(flat)))] <- 0
stopifnot(max(abs(change_curve(flat,d,grid)$risk_difference))<1e-12)
for(dist in c('weibull','lognormal')) {
 f <- fit_change(d,dist)
 stopifnot(all(is.finite(change_curve(f,d,grid)$risk5)))
}
# Physically impossible grip combinations use the same restricted target at
# every grid point, and the reference cannot silently disappear.
edge <- d; edge$grip10[1:2] <- c(2,98)
stopifnot(nrow(change_target(edge,grid))==n-2L)
stopifnot(inherits(try(change_curve(fit,d,c(1,5)),silent=TRUE),'try-error'))
positive <- d; positive$change_kg <- abs(positive$change_kg)+1
stopifnot(inherits(try(change_grid(positive),silent=TRUE),'try-error'))
g <- change_grid(d)
stopifnot(0 %in% g,all(g==round(g)),min(g)>=quantile(d$change_kg,.1),
 max(g)<=quantile(d$change_kg,.9))
constant_gap <- d; constant_gap$gap_years <- 4
stopifnot(!'gap_years' %in% all.vars(formula(fit_change(constant_gap))))
res <- change_bootstrap(d,grid,20)
stopifnot(all(res$status=='OK'),all(res$bootstrap_success>=18),
 all(res$risk_ci_lower<=res$risk_ci_upper),
 all(res$rd_ci_lower<=res$rd_ci_upper),
 res$rd_ci_lower[res$change_kg==0]==0,res$rd_ci_upper[res$change_kg==0]==0)
original_fit <- fit_change; calls <- 0L
fit_change <- function(...) {
 calls <<- calls+1L
 if(calls>1L) stop('Synthetic bootstrap failure')
 original_fit(...)
}
bad <- change_bootstrap(d,grid,20)
fit_change <- original_fit
stopifnot(all(bad$status=='UNSTABLE'),all(bad$bootstrap_success==0),
 all(is.na(bad$risk_ci_lower)),all(is.na(bad$rd_ci_upper)))
plot_path <- tempfile('hrs_change_synthetic_',fileext='.png')
plot_change_curve(res,plot_path,synthetic=TRUE)
stopifnot(file.info(plot_path)$size>1000)
cat('Synthetic association, reference, support, bootstrap and plot checks passed.\n')

# Real parser pipeline using generated one-person fixed-width fixtures only.
association_fixture <- d
source('tests/test_stage2.R')
t$OAGE <- 60
write.csv(t,file.path(root,'trk2022tr_r.csv'),row.names=FALSE)
old_report <- file.path(output,'stage2_counts.csv')
old_hash <- tools::md5sum(old_report)
change_main(c(root,output,'20'),normalizePath('scripts'))
out <- file.path(output,'grip_change_v1')
stopifnot(identical(old_hash,tools::md5sum(old_report)),
 startsWith(readLines(file.path(out,'STATUS.txt'))[1],'INCOMPLETE'),
 file.exists(file.path(out,'sample_flow.csv')),
 !file.exists(file.path(out,'risk_curve.csv')))
flow <- read.csv(file.path(out,'sample_flow.csv'))
stopifnot(all(flow$count_rounded_10=='SUPPRESSED'))
cat('Synthetic import pipeline, output isolation and small-sample stop passed.\n')

# End-to-end CLI success: expand the generated fixed-width input to a cohort.
d <- association_fixture; n <- nrow(d)
ids <- paste0(sprintf('%06d',as.integer(d$household)),sprintf('%03d',rep(1:2,n/2)))
for(name in names(sets)) {
 values <- matrix(rep(sets[[name]],each=n),nrow=n,dimnames=list(NULL,names(sets[[name]])))
 if(name=='h10i_r') values[,paste0('MI',c('816','851','852','853'))] <- round(d$grip10)
 if(name=='h14i_r') values[,paste0('OI',c('816','851','852','853'))] <- round(d$grip14)
 if(name=='h14c_r') {
   values[,'OC001'] <- as.integer(d$health)
   values[,'OC116'] <- ifelse(d$smoke=='never',5,1)
   values[,'OC117'] <- ifelse(d$smoke=='never',NA,ifelse(d$smoke=='former',5,1))
 }
 if(name=='h14pr_r') { values[,'OZ076'] <- 0; values[,'OZ205'] <- NA }
 lines <- vapply(seq_len(n),function(i)
   paste0(ids[i],paste(ifelse(is.na(values[i,]),'    ',sprintf('%4d',as.integer(values[i,]))),collapse='')),'')
 writeLines(lines,file.path(root,paste0(name,'.da')))
}
t <- t[rep(1,n),,drop=FALSE]
t$HHID <- substr(ids,1,6); t$PN <- substr(ids,7,9)
t$OAGE <- round(d$age); t$SEX <- as.integer(d$sex)
early <- month_index(2014,6)-round(d$gap_years*12)
t$MIWYEAR <- early%/%12; t$MIWMONTH <- early%%12+1
death <- month_index(2014,6)+floor(12*d$lo)
event <- is.finite(d$hi)
t$NYEAR <- ifelse(event,death%/%12,NA)
t$NMONTH <- ifelse(event,(death%%12)%/%3+1,NA)
t$EXDEATHYR <- t$KNOWNDECEASEDYR <- t$NYEAR
t$EXDEATHMO <- t$KNOWNDECEASEDMO <- ifelse(event,death%%12+1,NA)
alive <- ifelse(event,month_index(2014,6),month_index(2014,6)+floor(12*d$lo))
t$LASTALIVEYR <- alive%/%12; t$LASTALIVEMO <- alive%%12+1
write.csv(t,file.path(root,'trk2022tr_r.csv'),row.names=FALSE)
cli <- system2(file.path(R.home('bin'),'Rscript'),
 shQuote(c(normalizePath('scripts/hrs_grip_change.R'),root,output,'20')),
 stdout=TRUE,stderr=TRUE)
stopifnot(is.null(attr(cli,'status')),
 startsWith(readLines(file.path(out,'STATUS.txt'))[1],'SUCCESS'),
 identical(old_hash,tools::md5sum(old_report)))
curve <- read.csv(file.path(out,'risk_curve.csv'))
stopifnot(all(curve$status=='OK'),file.exists(file.path(out,'risk_curve.png')),
 all(c('change_kg','risk_difference','rd_ci_lower','rd_ci_upper') %in% names(curve)),
 !any(c('household','HHID','PN','grip10','grip14') %in% names(curve)))
# A failed rerun must mark even an existing successful result as stale.
t$OAGE[1] <- 'invalid'
write.csv(t,file.path(root,'trk2022tr_r.csv'),row.names=FALSE)
failed <- suppressWarnings(system2(file.path(R.home('bin'),'Rscript'),
 shQuote(c(normalizePath('scripts/hrs_grip_change.R'),root,output,'20')),stdout=TRUE,stderr=TRUE))
stopifnot(attr(failed,'status')==1L,
 startsWith(readLines(file.path(out,'STATUS.txt'))[1],'INCOMPLETE'),
 !any(grepl('invalid',failed,fixed=TRUE)))
cat('Synthetic full CLI success, aggregate output and stale-result failure checks passed.\n')
