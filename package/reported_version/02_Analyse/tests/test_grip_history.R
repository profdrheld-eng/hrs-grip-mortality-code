for(name in c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R',
              'hrs_analysis.R','hrs_prediction_validation.R','hrs_grip_change.R'))
  source(file.path('scripts',name))
if(file.exists('scripts/hrs_grip_history.R')) source('scripts/hrs_grip_history.R')
stopifnot(exists('history_curve',mode='function'))
set.seed(1127)
n <- 1200
d <- data.frame(household=rep(seq_len(n/2),each=2),age=runif(n,50,85),
 sex=factor(sample(1:2,n,TRUE)),smoke=factor(sample(c('never','former','current'),n,TRUE)),
 health=factor(sample(1:5,n,TRUE)),grip14=runif(n,20,45),gap_years=runif(n,3,5),
 change_kg=runif(n,-10,10))
d$grip10 <- d$grip14-d$change_kg
time <- rweibull(n,1.4,exp(2.1+.06*d$change_kg-.015*(d$age-65)))
d$lo <- pmin(5,pmax(.001,floor(time*12)/12))
d$hi <- ifelse(time<5,pmin(5,d$lo+1/12),Inf)
grid <- c(-5,0,5)
fit <- fit_history(d)
stopifnot(identical(coef(fit),coef(fit_pair(d)$prior)),
 all(c('grip10','grip14') %in% all.vars(formula(fit))),
 !any(c('change_kg','gap_years') %in% all.vars(formula(fit))))
curve <- history_curve(fit,d,grid)
stopifnot(curve$risk5[1]>curve$risk5[2],curve$risk5[2]>curve$risk5[3],
 curve$risk_difference[2]==0)
# Current-only oracle: changing history must have EXACTLY zero influence.
flat <- fit; flat$coefficients[grepl('grip10',names(coef(flat)))] <- 0
stopifnot(max(abs(history_curve(flat,d,grid)$risk_difference))<1e-12)
# Verify direction and fixed current value independently of the curve function.
manual <- d; manual$grip10 <- manual$grip14+5
stopifnot(abs(mean(risk5(fit,manual))-curve$risk5[1])<1e-12)
edge <- d; edge$grip14[1:2] <- c(2,98)
stopifnot(nrow(history_target(edge,grid))==n-2L,
 inherits(try(history_curve(fit,d,c(1,5)),silent=TRUE),'try-error'))
result <- change_bootstrap(d,grid,20,fit_fun=fit_history,curve_fun=history_curve)
stopifnot(all(result$status=='OK'),all(result$bootstrap_success>=18),
 result$rd_ci_lower[result$change_kg==0]==0,
 result$rd_ci_upper[result$change_kg==0]==0)
plot_change_curve(result,tempfile('hrs_history_synthetic_',fileext='.png'),synthetic=TRUE,
 title='Gleiche aktuelle Handkraft')
cat('Fixed-current contrast, unchanged original model, null-history oracle and bootstrap passed.\n')
# Reuse the existing generated fixed-width fixtures and regression checks.
source('tests/test_grip_change.R')
t$OAGE <- round(d$age)
write.csv(t,file.path(root,'trk2022tr_r.csv'),row.names=FALSE)
prior_files <- list.files(output,recursive=TRUE,full.names=TRUE)
prior_hashes <- tools::md5sum(prior_files)
cli <- system2(file.path(R.home('bin'),'Rscript'),
 shQuote(c(normalizePath('scripts/hrs_grip_history.R'),root,output,'20')),stdout=TRUE,stderr=TRUE)
out <- file.path(output,'grip_history_v1')
stopifnot(is.null(attr(cli,'status')),
 startsWith(readLines(file.path(out,'STATUS.txt'))[1],'SUCCESS'),
 identical(prior_hashes,tools::md5sum(prior_files)))
curve <- read.csv(file.path(out,'risk_curve.csv'))
stopifnot(all(curve$status=='OK'),file.exists(file.path(out,'risk_curve.png')),
 !any(c('HHID','PN','household','grip10','grip14') %in% names(curve)))
t$OAGE[1] <- 'invalid'
write.csv(t,file.path(root,'trk2022tr_r.csv'),row.names=FALSE)
failed <- suppressWarnings(system2(file.path(R.home('bin'),'Rscript'),
 shQuote(c(normalizePath('scripts/hrs_grip_history.R'),root,output,'20')),stdout=TRUE,stderr=TRUE))
stopifnot(attr(failed,'status')==1L,
 startsWith(readLines(file.path(out,'STATUS.txt'))[1],'INCOMPLETE'))
cat('History CLI, output isolation and failure status passed.\n')
