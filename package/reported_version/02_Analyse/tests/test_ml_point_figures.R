# Synthetic only: validates paired transformations and saved-tuning replay.
source('scripts/hrs_ml_extension.R');ml_source_existing('scripts')
source('scripts/hrs_ml_point_figures.R')
cmb<-expand.grid(model=c('weibull','neural','xgboost'),target=c('current','prior'),arm=c('original','augmented'),stringsAsFactors=FALSE)
r<-matrix(.2,50,12)
r[,cmb$target=='prior']<-r[,cmb$target=='prior']+.03
r[,cmb$arm=='augmented']<-r[,cmb$arm=='augmented']-.01
v<-ml_point_changes(r,cmb)
stopifnot(max(abs(v$history-3))<1e-12,max(abs(v$synthetic+1))<1e-12)
stopifnot(inherits(try(ml_point_changes(r[-1,],cmb[-1,]),silent=TRUE),'try-error'))
d<-ml_simulate(600,'small',11);cfg<-ml_config('smoke');cfg$models<-'weibull';cfg$ci<-0;cfg$ratios<-.5
b<-ml_benchmark(d,cfg)
a<-ml_replay_predictions(d,cfg,b$tuning)
for(j in seq_len(nrow(a$combos))) {
 z<-b$metrics[b$metrics$target==a$combos$target[j]&b$metrics$arm==a$combos$arm[j],]
 m<-ml_metrics(d,a$risk[,j],a$weights)
 stopifnot(abs(z$auc-m['auc'])<1e-12,abs(z$brier-m['brier'])<1e-12)
}
message('PASS: paired risk changes and replay match fresh synthetic benchmark.')
# Exercise both renderers with fake predictions, never private records.
set.seed(3);base<-rbeta(300,2,12);r<-sapply(1:12,function(j) pmin(1,pmax(0,base+rnorm(300,0,.01))))
cal<-do.call(rbind,lapply(1:12,function(j) data.frame(cmb[rep(j,5),],group=1:5,predicted=seq(.02,.4,length.out=5),observed=seq(.02,.4,length.out=5),row.names=NULL)))
render_out<-file.path(tempdir(),'hrs_point_render_test');dir.create(render_out,showWarnings=FALSE)
ml_render_person_points(r,cmb,cal,render_out)
stopifnot(setequal(list.files(render_out,pattern='png$'),c('P02a_prior_risk_changes.png','P02b_synthetic_risk_changes.png','P03_calibration_with_points.png')))
message('PASS: three synthetic-only renderers.')
