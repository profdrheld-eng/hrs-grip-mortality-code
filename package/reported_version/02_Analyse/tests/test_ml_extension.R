# All inputs generated; never read private HRS data.
source('scripts/hrs_ml_extension.R')
ml_source_existing('scripts')
d <- ml_simulate(600,'small',11)
stopifnot(nrow(d)==600,all(d$lo>0),all(d$hi>d$lo),all(d$grip14-d$grip10==d$change_kg))
f <- ml_folds(d,3,12)
stopifnot(all(vapply(split(f,d$household),function(x) length(unique(x))==1,TRUE)))
# Event-free households must also be balanced after event-positive assignments.
odd <- d;odd$hi <- Inf;odd$hi[1:3] <- pmin(5,odd$lo[1:3]+.01)
odd$lo[1:3] <- pmin(odd$lo[1:3],4.9);odd$hi[1:3] <- odd$lo[1:3]+.01
balanced <- ml_folds(odd,3,12)
stopifnot(diff(range(table(balanced)))<=4)
# Likelihood probability oracle, including short intervals and right censoring.
lo <- c(.2,1,2);hi <- c(.3,1+1e-7,Inf);mu <- c(1,2,1);scale <- .8
v <- ml_weibull(lo,hi,mu,scale)
oracle <- log(pweibull(hi,1/scale,exp(mu),lower.tail=FALSE)*-1+
 pweibull(lo,1/scale,exp(mu),lower.tail=FALSE))
stopifnot(max(abs(v$logp-oracle))<1e-7)
p <- ml_preprocess(d,'prior');x <- ml_matrix(p,d)
set.seed(15);theta <- c(rnorm((ncol(x)+1)*3+4,sd=.04),log(.9))
a <- ml_nn_objective(theta,x,d$lo,d$hi,3,.01)
h <- 1e-5
numeric_grad <- vapply(seq_along(theta),function(j) {
 t1<-t2<-theta;t1[j]<-t1[j]+h;t2[j]<-t2[j]-h
 (ml_nn_objective(t1,x,d$lo,d$hi,3,.01)$value-
 ml_nn_objective(t2,x,d$lo,d$hi,3,.01)$value)/(2*h)
},0)
stopifnot(max(abs(a$gradient-numeric_grad))<1e-4)
syn <- ml_augment(d,1,13)
stopifnot(nrow(syn)==2*nrow(d),all(syn$hi>syn$lo),
 all(syn$grip10>0 & syn$grip10<=100),all(syn$grip14-syn$grip10==syn$change_kg))
# Current-only augmentation must not learn from earlier grip, even indirectly.
alt<-d;alt$grip10<-rev(alt$grip10);alt$change_kg<-alt$grip14-alt$grip10
s1<-ml_augment(d,.5,13,'current');s2<-ml_augment(alt,.5,13,'current')
ix<-(nrow(d)+1):nrow(s1)
stopifnot(identical(s1[ix,c('age','grip14','lo','hi')],s2[ix,c('age','grip14','lo','hi')]))
# Preprocessor frozen to training: shifting evaluation values cannot change it.
changed <- d;changed$age <- changed$age+20
stopifnot(!identical(ml_matrix(p,d),ml_matrix(p,changed)),identical(p[c('columns','center','scale')],ml_preprocess(d,'prior')[c('columns','center','scale')]))
for(model in c('weibull','neural','xgboost')) {
 fit <- ml_fit(d,model,'prior',ml_grid(model,TRUE)[[1]],14)
 risk <- ml_predict(fit,d)$risk
 stopifnot(all(is.finite(risk)),all(risk>0 & risk<1))
 if(model=='weibull') stopifnot(max(abs(risk-risk5(fit_pair(d)$prior,d)))<1e-12)
}
# XGBoost returns log-time margins; zero-tree prediction matches fitted AFT intercept.
p0<-ml_grid('xgboost',TRUE)[[1]];p0$rounds<-0L
f0<-ml_fit(d,'xgboost','prior',p0,14)
mu0<-optimize(function(mu) -mean(ml_weibull(d$lo,d$hi,rep(mu,nrow(d)),p0$scale)$logp),c(-3,8))$minimum
stopifnot(max(abs(ml_predict(f0,d)$risk-pweibull(5,1/p0$scale,exp(mu0))))<1e-6)
# Frozen spline knots retain exact agreement with the pre-existing curve models.
grid<-change_grid(d)
stopifnot(max(abs(change_curve(ml_association_fit(d,d,'change'),d,grid)$risk5-
                 change_curve(fit_change(d),d,grid)$risk5))<1e-9,
 max(abs(history_curve(ml_association_fit(d,d,'history'),d,grid)$risk5-
         history_curve(fit_history(d),d,grid)$risk5))<1e-9)
# An uncensored oracle: IPCW equals ordinary Brier/AUC.
u<-d;u$lo[!is.finite(u$hi)]<-5
w<-ml_weights(u,u);r<-rep(.2,nrow(u))
stopifnot(all(w$weight==1),abs(ml_metrics(u,r,w)['brier']-mean((w$y-r)^2))<1e-12)
cfg<-ml_config('smoke');cfg$models<-c('weibull','neural','xgboost')
out<-file.path(tempdir(),'hrs_ml_smoke');dir.create(out,showWarnings=FALSE)
cfg$fractions<-c(.75,1)
result<-ml_benchmark(d,cfg,out)
stopifnot(nrow(result$metrics)==24,all(is.finite(result$metrics$auc)),
 all(is.finite(result$metrics$brier)),nrow(result$paired)>0)
invisible(ml_associations(d,cfg,out))
invisible(ml_simulation(cfg,out))
stopifnot(file.exists(file.path(out,'association_curves.csv')),
 file.exists(file.path(out,'simulation_summary.csv')))
for(path in list.files(out,pattern='csv$',full.names=TRUE)) {
 z<-read.csv(path)
 stopifnot(!any(c('household','HHID','PN','lo','hi','grip10','grip14') %in% names(z)))
}
bad<-d;bad$hi[1]<-bad$lo[1]
stopifnot(inherits(try(ml_validate(bad),silent=TRUE),'try-error'))
cat('PASS: gradient, likelihood, grouping, augmentation, all models, nested benchmark, curves, simulation, aggregate schemas.\n')
cat('Synthetic outputs: ',out,'\n')
