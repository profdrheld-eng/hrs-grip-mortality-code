# Original-training held-out predictions only, with fixed historical tuning.
review_oof_replay <- function(d,cfg,tuning) {
  ml_validate(d);stopifnot(identical(as.numeric(cfg$fractions),1))
  fold<-ml_folds(d,cfg$outer,cfg$seed)
  combos<-expand.grid(model=cfg$models,target=c('current','prior'),arm='original',stringsAsFactors=FALSE)
  risk<-matrix(NA_real_,nrow(d),nrow(combos));w<-data.frame(y=rep(NA_real_,nrow(d)),weight=NA_real_)
  for(k in seq_len(cfg$outer)) {
    train<-d[fold!=k,,drop=FALSE];test<-d[fold==k,,drop=FALSE]
    set.seed(cfg$seed+k);groups<-sample(unique(train$household))
    groups<-head(groups,max(cfg$inner*2,ceiling(length(groups))))
    train<-train[train$household %in% groups,,drop=FALSE]
    w[fold==k,]<-ml_weights(train,test,'midpoint')
    for(j in seq_len(nrow(combos))) {
      combo<-combos[j,];z<-tuning[tuning$fraction==1&tuning$fold==k&tuning$model==combo$model&
        tuning$target==combo$target&tuning$arm=='original',,drop=FALSE]
      if(nrow(z)!=1||!is.finite(z$ratio)||z$ratio!=0) stop('Original-training fixed tuning is missing or invalid.')
      parameter<-list()
      if(!is.na(z$parameter)&&nzchar(z$parameter)) {
        kv<-strsplit(strsplit(z$parameter,';',fixed=TRUE)[[1]],'=',fixed=TRUE)
        if(!all(lengths(kv)==2)) stop('Invalid fixed tuning syntax.')
        parameter<-setNames(lapply(kv,function(x)as.numeric(x[2])),vapply(kv,'[','',1))
        if(any(!is.finite(unlist(parameter)))) stop('Invalid fixed tuning values.')
      }
      seed<-cfg$seed+1000*k;prep<-ml_preprocess(train,combo$target)
      # Ratio zero deliberately preserves the historical original-training pipeline.
      aug<-ml_augment(train,0,seed,combo$target)
      fit<-ml_fit(aug,combo$model,combo$target,parameter,seed,prep)
      risk[fold==k,j]<-ml_predict(fit,test)$risk
    }
  }
  if(any(!is.finite(risk))) stop('Incomplete held-out predictions.')
  list(risk=risk,combos=combos,weights=w,fold=fold)
}

review_oof_summary <- function(d,replayed,B=5000L,seed=20261020L) {
  if(B<20) stop('Insufficient descriptive resamples.')
  combos<-replayed$combos;rows<-list();groups<-split(seq_len(nrow(d)),d$household)
  models<-unique(combos$model)
  indicators<-list();point<-list()
  for(model in models) {
    r<-which(combos$model==model&combos$target=='current');a<-which(combos$model==model&combos$target=='prior')
    if(length(r)!=1||length(a)!=1) stop('Ambiguous OOF comparison.')
    point[[model]]<-review_risk_changes(replayed$risk[,r],replayed$risk[,a])
    delta<-replayed$risk[,a]-replayed$risk[,r];tol<-16*.Machine$double.eps
    indicators[[model]]<-cbind(above=abs(delta)>.02+tol,increase=delta>.02+tol,decrease=delta< -.02-tol)
  }
  size<-lengths(groups);aggregate<-lapply(indicators,function(z)t(vapply(groups,function(ix)colSums(z[ix,,drop=FALSE]),numeric(3))))
  boot<-array(NA_real_,c(B,length(models),3));set.seed(seed)
  for(b in seq_len(B)) {
    selected<-sample.int(length(groups),length(groups),replace=TRUE);denominator<-sum(size[selected])
    for(m in seq_along(models)) boot[b,m,]<-colSums(aggregate[[m]][selected,,drop=FALSE])/denominator
  }
  for(m in seq_along(models)) for(k in seq_len(3)) {
    model<-models[m];value<-point[[model]];n<-value$n;counts<-c(value$n_above,value$n_increase,value$n_decrease)
    # Absolute change equals increase plus decrease. Hide all three linked
    # summaries when one numerator/complement is small, including its CI.
    available<-all(pmin(counts,n-counts)>=10)
    ci<-quantile(boot[,m,k],c(.025,.975),names=FALSE)
    rows[[length(rows)+1]]<-data.frame(model=model,comparison=c('absolute','increase','decrease')[k],threshold_probability=.02,
      n_rounded_10=10*floor(n/10+.5),count_rounded_10=if(available) as.character(10*floor(counts[k]/10+.5)) else 'SUPPRESSED',
      proportion=if(available) counts[k]/n else NA_real_,ci_lower=if(available) ci[1] else NA_real_,ci_upper=if(available) ci[2] else NA_real_,
      bootstrap_repetitions=B,uncertainty='conditional_fixed_OOF_household_bootstrap')
  }
  do.call(rbind,rows)
}
