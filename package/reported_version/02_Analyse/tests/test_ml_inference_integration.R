source('scripts/hrs_ml_extension.R');ml_source_existing('scripts')
source('scripts/hrs_ml_equivalence.R')
d<-ml_simulate(600,'small',11);cfg<-ml_config('smoke');cfg$ci<-1000L
# Instrument the actual nested pipeline: every scoring call must be on unseen households.
original_fit<-ml_fit;original_predict<-ml_predict;scoring_calls<-0L
ml_fit<-function(d,model,target,parameter,seed,preprocess=NULL) {
 fit<-original_fit(d,model,target,parameter,seed,preprocess)
 fit$training_households<-unique(d$household);fit
}
ml_predict<-function(fit,d) {
 stopifnot(!any(d$household %in% fit$training_households))
 scoring_calls<<-scoring_calls+1L;original_predict(fit,d)
}
a<-ml_benchmark(d,cfg)
cfg$export_bootstrap<-TRUE
out<-tempfile('ml_inference_');dir.create(out)
b<-ml_benchmark(d,cfg,out)
stopifnot(identical(a$metrics,b$metrics),identical(a$paired,b$paired))
stopifnot(scoring_calls>24L)
draws<-b$bootstrap_differences
stopifnot(nrow(draws)==40000L,
 identical(names(draws),c('fraction','contrast','model','target','arm','reference_model','metric','replicate','difference')))
for(i in seq_len(nrow(b$paired))) {
 z<-b$paired[i,];if(!z$metric %in% c('auc','brier')) next
 use<-rep(TRUE,nrow(draws))
 for(k in c('fraction','contrast','model','target','arm','reference_model','metric')) use<-use & draws[[k]]==z[[k]]
 ci<-quantile(draws$difference[use],c(.025,.975),names=FALSE)
 stopifnot(max(abs(ci-c(z$ci_lower,z$ci_upper)))<1e-12)
}
writeLines('SUCCESS: synthetic integration test only.',file.path(out,'STATUS.txt'))
dput(cfg,file=file.path(out,'CONFIG.R'));writeLines('synthetic fixture',file.path(out,'source_hashes.csv'))
report<-tempfile('ml_tost_');ml_equivalence_report(out,report)
z<-read.csv(file.path(report,'conditional_tost.csv'))
stopifnot(nrow(z)==360L,all(z$p_holm>=z$p_tost),all(z$bootstrap_B==1000),
 all(z$equivalent_holm==(z$p_holm<.05)))
cat('PASS: opt-in export leaves estimates unchanged; paired quantiles match; all 40 metric-contrasts and 360 margin tests exported.\n')
