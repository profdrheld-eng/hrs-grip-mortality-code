# Generated data only. Regression checks for optional replay providers.
source('scripts/hrs_ml_extension.R');ml_source_existing('scripts')
d<-ml_simulate(600,'small',11)
cfg<-ml_config('smoke');cfg$fractions<-c(.75,1)
cfg$export_bootstrap<-TRUE
baseline<-ml_benchmark(d,cfg)
key<-function(fraction,fold,model,target,arm)
  paste(fraction,fold,model,target,arm,sep='|')
selections<-new.env(parent=emptyenv());predictions<-new.env(parent=emptyenv())
select<-function(train,model,target,arm,cfg,inner,seed,fraction,fold) {
  name<-key(fraction,fold,model,target,arm)
  stopifnot(!exists(name,selections,inherits=FALSE))
  value<-ml_tune(train,model,target,arm,cfg,inner,seed)
  assign(name,value,selections);value
}
predict_provider<-function(train,aug,test,model,target,arm,parameter,seed,preprocess,fraction,fold) {
  name<-key(fraction,fold,model,target,arm)
  stopifnot(!exists(name,predictions,inherits=FALSE),
    !any(test$household %in% train$household))
  value<-ml_predict(ml_fit(aug,model,target,parameter,seed,preprocess),test)
  assign(name,list(value=value,train=train,test=test,parameter=parameter),predictions);value
}
weight_calls<-0L
weight_provider<-function(train,test,position) {
  stopifnot(!any(test$household %in% train$household),position %in% c('lower','midpoint','upper'))
  weight_calls<<-weight_calls+1L;ml_weights(train,test,position)
}
replay<-ml_benchmark(d,cfg,selection_provider=select,prediction_provider=predict_provider,weight_function=weight_provider)
stopifnot(identical(baseline,replay),length(ls(selections))==48L,
  length(ls(predictions))==48L,weight_calls==12L)
select_cached<-function(train,model,target,arm,cfg,inner,seed,fraction,fold)
  get(key(fraction,fold,model,target,arm),selections,inherits=FALSE)
predict_cached<-function(train,aug,test,model,target,arm,parameter,seed,preprocess,fraction,fold) {
  value<-get(key(fraction,fold,model,target,arm),predictions,inherits=FALSE)
  stopifnot(identical(train,value$train),identical(test,value$test),identical(parameter,value$parameter))
  value$value
}
fit_original<-ml_fit;tune_original<-ml_tune
ml_fit<-function(...) stop('Unexpected fit during cached scoring.')
ml_tune<-function(...) stop('Unexpected tuning during replay.')
cached<-ml_benchmark(d,cfg,selection_provider=select_cached,prediction_provider=predict_cached)
stopifnot(identical(baseline,cached))
scaled_weights<-function(train,test,position) {
  w<-ml_weights(train,test,position);w$weight<-w$weight*.9;w
}
rescored<-ml_benchmark(d,cfg,selection_provider=select_cached,prediction_provider=predict_cached,
  weight_function=scaled_weights)
stopifnot(max(abs(rescored$metrics$brier-.9*baseline$metrics$brier))<1e-12,
  identical(rescored$metrics$interval_log_score,baseline$metrics$interval_log_score))
reject<-function(fun,pattern) {
  error<-tryCatch({fun();NULL},error=identity)
  stopifnot(inherits(error,'error'),grepl(pattern,conditionMessage(error),fixed=TRUE))
}
reject(function() ml_benchmark(d,cfg,selection_provider=1),'selection_provider')
reject(function() ml_benchmark(d,cfg,prediction_provider=1),'prediction_provider')
reject(function() ml_benchmark(d,cfg,weight_function=NULL),'weight_function')
reject(function() ml_benchmark(d,cfg,selection_provider=select_cached,
  prediction_provider=function(...) list(risk=.2,score=-1)),'Invalid replay predictions')
reject(function() ml_benchmark(d,cfg,selection_provider=select_cached,
  prediction_provider=function(test,...) list(risk=rep(2,nrow(test)),score=rep(-1,nrow(test)))),
  'Invalid replay predictions')
reject(function() ml_benchmark(d,cfg,selection_provider=select_cached,
  prediction_provider=function(test,...) list(risk=rep(.2,nrow(test)),score=rep(NA_real_,nrow(test)))),
  'Invalid replay predictions')
ml_fit<-fit_original;ml_tune<-tune_original
cat('PASS: default/provider equality, complete explicit keys, no held-out household leakage, cached no-fit/no-tune scoring, weight-only rescore and invalid-provider rejection.\n')
