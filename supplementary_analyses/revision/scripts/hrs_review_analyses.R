# Exploratory analyses. Pure functions, no data import or automatic execution.
# Source existing corrected survival helpers before calling model/validation functions.
review_formulas <- function() {
  base <- survival::Surv(lo,hi,type='interval2') ~ splines::ns(age,df=3)+sex+smoke+health
  current <- update(base,. ~ . + splines::ns(grip14,df=3))
  prior <- update(current,. ~ . + splines::ns(grip10,df=3))
  list(covariates=base,current=current,prior=prior,
    joint=update(prior,. ~ . + splines::ns(grip14,df=3):splines::ns(grip10,df=3)),
    current_sex=update(current,. ~ . + sex:splines::ns(grip14,df=3)),
    prior_sex=update(prior,. ~ . + sex:splines::ns(grip14,df=3)+sex:splines::ns(grip10,df=3)),
    current_no_srh=update(current,. ~ . - health),prior_no_srh=update(prior,. ~ . - health))
}

review_transitions <- function(prior,current,sex) {
  sex<-as.character(sex)
  if(length(prior)!=length(current)||length(prior)!=length(sex)||
     anyNA(c(prior,current))||any(!is.finite(c(prior,current)))||!all(sex %in% c('1','2')))
    stop('Invalid transition inputs; no participant values exported.')
  cutoff<-ifelse(sex=='1',27,16)
  levels<-c('not_low_to_not_low','not_low_to_low','low_to_not_low','low_to_low')
  factor(paste0(ifelse(prior<cutoff,'low','not_low'),'_to_',ifelse(current<cutoff,'low','not_low')),levels=levels)
}

review_risk_changes <- function(current,prior,threshold=.02) {
  if(length(current)!=length(prior)||!length(current)||anyNA(c(current,prior))||
     any(!is.finite(c(current,prior)))||any(c(current,prior)<0|c(current,prior)>1)||
     length(threshold)!=1||!is.finite(threshold)||threshold<=0) stop('Invalid risk-change inputs.')
  delta<-prior-current
  # Values equal up to floating-point arithmetic are on, not above, the boundary.
  tol<-16*.Machine$double.eps
  up<-sum(delta>threshold+tol);down<-sum(delta< -threshold-tol)
  data.frame(n=length(delta),threshold=threshold,n_above=up+down,n_increase=up,n_decrease=down,
    proportion=(up+down)/length(delta),proportion_increase=up/length(delta),proportion_decrease=down/length(delta))
}

review_interval <- function(samples,requested) {
  good<-is.finite(samples);enough<-sum(good)>=ceiling(.9*requested)
  ci<-if(enough) quantile(samples[good],c(.025,.975),names=FALSE) else c(NA_real_,NA_real_)
  list(lower=ci[1],upper=ci[2],success=sum(good),status=if(enough) 'OK' else 'UNSTABLE')
}

review_release_counts <- function(counts) {
  if(!length(counts)||is.null(names(counts))||anyNA(counts)||any(!is.finite(counts))||any(counts<0)||
     any(counts!=floor(counts))) stop('Invalid aggregate counts.')
  suppressed<-counts<10
  # Do not release an otherwise identifiable single small category.
  if(sum(suppressed)==1 && length(counts)>1) {
    candidates<-which(!suppressed & names(counts)!='total')
    if(!length(candidates)) candidates<-which(!suppressed)
    suppressed[candidates[which.min(counts[candidates])]]<-TRUE
  }
  shown<-as.character(10*floor(counts/10+.5));shown[suppressed]<-'SUPPRESSED'
  data.frame(metric=names(counts),count_rounded_10=shown,stringsAsFactors=FALSE)
}

review_selection_reason <- function(tracker_match,core_interview,death_before_2014,pmelig,reason,included,death_during_wave=rep(FALSE,length(included)),
    not_scheduled=rep(FALSE,length(included))) {
  n<-length(included)
  if(any(lengths(list(tracker_match,core_interview,death_before_2014,pmelig,reason,death_during_wave,not_scheduled))!=n)||
     anyNA(c(tracker_match,core_interview,death_before_2014,included,death_during_wave,not_scheduled))) stop('Invalid selection flags.')
  out<-rep('no_valid_grip_or_other_exclusion',n)
  out[not_scheduled]<-'not_scheduled_for_2014_physical_measures'
  out[reason %in% 4]<-'documented_unable'
  out[pmelig %in% c(5,6,7)]<-'physical_measure_ineligible'
  out[!core_interview]<-'no_core_interview'
  out[death_during_wave & !core_interview]<-'documented_death_2014_2015_without_core'
  out[included]<-'included'
  out[!tracker_match]<-'unmatched_tracker'
  out[death_before_2014]<-'death_before_2014'
  if(any(included & (death_before_2014|!tracker_match|!core_interview))) stop('Contradictory inclusion flags.')
  out
}

review_fit <- function(d,formulas=review_formulas()) {
  lapply(formulas,function(f) withCallingHandlers({
    fit<-survival::survreg(f,data=d,dist='weibull',na.action=na.fail,
      control=survival::survreg.control(maxiter=100))
    if(any(!is.finite(coef(fit)))||!is.finite(fit$scale)||fit$scale<=0) stop('Invalid survival fit.')
    fit
  },warning=function(w) stop('Survival fit warning; details remain private.')))
}

review_metrics <- function(fits,d,groups=c('all','1','2')) {
  metrics<-c('brier','auc','calibration_intercept','calibration_slope','interval_log_score')
  out<-array(NA_real_,c(length(metrics),length(fits),length(groups)),
    dimnames=list(metrics,names(fits),groups))
  for(group in groups) {
    take<-if(group=='all') rep(TRUE,nrow(d)) else as.character(d$sex)==group
    if(sum(take)<2) next
    z<-d[take,,drop=FALSE]
    for(model in names(fits)) out[,model,group]<-tryCatch(
      c(clinical_metrics(z,risk5(fits[[model]],z)),interval_log_score=interval_score(fits[[model]],z)),
      error=function(e) rep(NA_real_,length(metrics)))
  }
  out
}

review_contrasts <- function() data.frame(
  id=c('current_vs_covariates','prior_vs_current','joint_vs_current','joint_vs_additive',
       'sex_current_vs_current','sex_prior_vs_sex_current','sex_prior_vs_additive','prior_vs_current_no_srh'),
  reference=c('covariates','current','current','prior','current','current_sex','prior','current_no_srh'),
  added=c('current','prior','joint','joint','current_sex','prior_sex','prior_sex','prior_no_srh'))

review_validate <- function(d,B=200L,inner=50L,formulas=review_formulas(),groups=c('all','1','2'),checkpoint=NULL,context=NULL) {
  if(B<20||inner<20||B!=floor(B)||inner!=floor(inner)) stop('At least 20 outer and inner resamples required.')
  fits<-review_fit(d,formulas);apparent<-review_metrics(fits,d,groups)
  bias<-array(NA_real_,c(B,dim(apparent)));nested<-bias
  attempts<-data.frame(repetition=seq_len(B),outer_fit=FALSE,inner_fit_success=0L)
  signature<-serialize(list(data=d,formulas=lapply(formulas,deparse),B=B,inner=inner,groups=groups,
    R=as.character(getRversion()),survival=as.character(utils::packageVersion('survival')),context=context),NULL)
  first<-1L
  if(!is.null(checkpoint) && file.exists(checkpoint)) {
    saved<-readRDS(checkpoint)
    if(!identical(saved$signature,signature)) stop('Checkpoint belongs to different inputs, code or settings.')
    bias<-saved$bias;nested<-saved$nested;attempts<-saved$attempts
    assign('.Random.seed',saved$rng,envir=.GlobalEnv);first<-saved$completed+1L
  }
  persist<-function(completed) {
    if(is.null(checkpoint)) return(invisible(NULL))
    tmp<-paste0(checkpoint,'.tmp')
    saveRDS(list(signature=signature,bias=bias,nested=nested,attempts=attempts,completed=completed,
      rng=get('.Random.seed',envir=.GlobalEnv)),tmp)
    if(!file.rename(tmp,checkpoint)) stop('Cannot atomically save private checkpoint.')
  }
  remaining<-if(first<=B) seq.int(first,B) else integer()
  for(b in remaining) {
    if(b==1||b%%5==0) message('Review validation outer ',b,'/',B)
    boot<-sample_households(d)
    attempt<-tryCatch({f<-review_fit(boot,formulas);list(train=review_metrics(f,boot,groups),
      test=review_metrics(f,d,groups))},error=function(e) NULL)
    if(is.null(attempt)) {persist(b);next}
    attempts$outer_fit[b]<-TRUE;bias[b,,,]<-attempt$train-attempt$test
    ibias<-array(NA_real_,c(inner,dim(apparent)))
    for(j in seq_len(inner)) {
      ib<-sample_households(boot)
      value<-tryCatch({f<-review_fit(ib,formulas);review_metrics(f,ib,groups)-review_metrics(f,boot,groups)},error=function(e) NULL)
      if(!is.null(value)) {ibias[j,,,]<-value;attempts$inner_fit_success[b]<-attempts$inner_fit_success[b]+1L}
    }
    for(m in seq_len(dim(apparent)[1])) for(g in seq_len(dim(apparent)[3])) {
      mat<-matrix(ibias[,m,,g],nrow=inner,ncol=length(formulas))
      good<-apply(is.finite(mat),1,all)
      if(sum(good)>=ceiling(.9*inner)) nested[b,m,,g]<-attempt$train[m,,g]-colMeans(mat[good,,drop=FALSE])
    }
    persist(b)
  }
  corrected<-apparent;rows<-list();contrasts<-review_contrasts()
  contrasts<-contrasts[contrasts$reference %in% names(formulas)&contrasts$added %in% names(formulas),,drop=FALSE]
  for(m in seq_len(dim(apparent)[1])) for(g in seq_len(dim(apparent)[3])) {
    mat<-matrix(bias[,m,,g],nrow=B,ncol=length(formulas));good<-apply(is.finite(mat),1,all)
    corrected[m,,g]<-if(sum(good)>=ceiling(.9*B)) apparent[m,,g]-colMeans(mat[good,,drop=FALSE]) else NA_real_
    model_rows<-function(name,estimate,obs,samples) {
      ci<-review_interval(samples,B)
      data.frame(metric=dimnames(apparent)[[1]][m],group=groups[g],target=name,apparent=obs,
        optimism_corrected=estimate,ci_lower=ci$lower,ci_upper=ci$upper,
        outer_success=sum(good),nested_success=ci$success,status=if(is.finite(estimate)) ci$status else 'UNSTABLE')
    }
    for(k in seq_along(formulas)) rows[[length(rows)+1]]<-model_rows(names(formulas)[k],corrected[m,k,g],apparent[m,k,g],nested[,m,k,g])
    for(i in seq_len(nrow(contrasts))) {
      a<-match(contrasts$added[i],names(formulas));r<-match(contrasts$reference[i],names(formulas))
      rows[[length(rows)+1]]<-model_rows(contrasts$id[i],corrected[m,a,g]-corrected[m,r,g],
        apparent[m,a,g]-apparent[m,r,g],nested[,m,a,g]-nested[,m,r,g])
    }
  }
  # Direct sex difference in paired increments; same household draws in both groups.
  sex_rows<-list()
  if(all(c('1','2') %in% groups)) for(id in c('prior_vs_current','sex_prior_vs_sex_current')) {
    con<-contrasts[contrasts$id==id,,drop=FALSE];if(!nrow(con)) next
    a<-match(con$added,names(formulas));r<-match(con$reference,names(formulas))
    x<-match('1',groups);y<-match('2',groups)
    for(m in seq_len(dim(apparent)[1])) {
      ci<-review_interval((nested[,m,a,y]-nested[,m,r,y])-(nested[,m,a,x]-nested[,m,r,x]),B)
      sex_rows[[length(sex_rows)+1]]<-data.frame(metric=dimnames(apparent)[[1]][m],contrast=id,
        definition='female_increment_minus_male_increment',estimate=(corrected[m,a,y]-corrected[m,r,y])-
          (corrected[m,a,x]-corrected[m,r,x]),ci_lower=ci$lower,ci_upper=ci$upper,success=ci$success,status=ci$status)
    }
  }
  likelihood<-do.call(rbind,lapply(seq_len(nrow(contrasts)),function(i) {
    r<-fits[[contrasts$reference[i]]];a<-fits[[contrasts$added[i]]]
    llr<-logLik(r);lla<-logLik(a);df<-attr(lla,'df')-attr(llr,'df')
    data.frame(contrast=contrasts$id[i],lr_chisq=2*(as.numeric(lla)-as.numeric(llr)),df=df,
      delta_aic=AIC(a)-AIC(r))
  }))
  list(metrics=do.call(rbind,rows),sex_increment_contrasts=if(length(sex_rows)) do.call(rbind,sex_rows) else data.frame(),
    likelihood=likelihood,attempts=attempts,local_fits=fits,local_bootstrap=nested)
}
