# Annotated source: hrs_prediction_validation.R

Read-only explanation of the executed source. Run the materialized package, not this Markdown view. Only REVIEW NOTE comments are added.

Source: `package/corrected_version/overlay/02_Analyse/scripts/hrs_prediction_validation.R`

Source SHA-256: `bfc3539a8caf5bc27eb67ae45de290ffca823fa101459825720275a5c8a62fcd`

```r
# Five-year IPCW metrics. Interval model fits remain unchanged.
# REVIEW NOTE: Finite hi denotes an event entirely within the five-year horizon. lo/hi must already satisfy cohort construction. Estimate censoring survival using the death-before-censoring tie convention. Events use G(T-), horizon survivors use G(5), early-censored rows get weight zero. Reject weak positivity instead of clipping.
horizon_weights <- function(d,position='midpoint') {
  stopifnot(position %in% c('lower','midpoint','upper'))
  event <- is.finite(d$hi)
  time <- d$lo
  time[event] <- switch(position,lower=d$lo[event],upper=d$hi[event],
                        midpoint=(d$lo[event]+d$hi[event])/2)
  censor_times <- sort(unique(time[!event & time<5]))
  g <- rep(1,nrow(d))
  evaluation <- ifelse(event,time,5)
  for(ct in censor_times) {
    # Deaths at ct precede censoring, consistent with evaluation at G(T-).
    at_risk <- sum(time>ct | (!event & time==ct))
    jump <- 1-sum(!event & time==ct)/at_risk
    g[evaluation>ct] <- g[evaluation>ct]*jump
  }
  known <- event | d$lo>=5
  if(any(!is.finite(g[known]) | g[known]<.05)) stop('IPCW positivity insufficient; no silent weight clipping.')
  data.frame(y=as.numeric(event),weight=ifelse(known,1/g,0))
}

# REVIEW NOTE: Weighted case-control concordance. Sorted score groups include all exact ties. Each tied case-control pair contributes one half. The denominator uses weighted case and control mass. Return NA when either is absent.
weighted_auc <- function(risk,y,w) {
  a <- order(risk); risk <- risk[a]; y <- y[a]; w <- w[a]
  group <- cumsum(c(TRUE,diff(risk)!=0))
  cases <- as.numeric(rowsum(w*(y==1),group,reorder=FALSE))
  controls <- as.numeric(rowsum(w*(y==0),group,reorder=FALSE))
  denominator <- sum(cases)*sum(controls)
  if(denominator<=0) return(NA_real_)
  sum(cases*(cumsum(controls)-controls/2))/denominator
}

# REVIEW NOTE: The Brier denominator is the full sample size, not the sum of IPCW weights. Calibration fits use known outcomes and weights. Only the calibration logit is numerically bounded. AUC and Brier retain original probabilities.
clinical_metrics <- function(d,risk,position='midpoint') {
  stopifnot(length(risk)==nrow(d),all(is.finite(risk)),all(risk>=0 & risk<=1))
  v <- horizon_weights(d,position); use <- v$weight>0
  # Numerical bounding is for the calibration logit only, not Brier or AUC.
  lp <- qlogis(pmin(1-1e-8,pmax(1e-8,risk)))
  cal <- data.frame(y=v$y[use],w=v$weight[use],lp=lp[use])
  fitcal <- function(form) tryCatch(withCallingHandlers({
    m <- glm(form,data=cal,weights=w,family=quasibinomial())
    if(!m$converged || any(!is.finite(coef(m)))) stop('Calibration fit invalid.')
    coef(m)
  },warning=function(w) stop('Calibration warning.')),error=function(e) NULL)
  intercept <- fitcal(y~1+offset(lp)); slope <- fitcal(y~lp)
  c(brier=sum(v$weight*(v$y-risk)^2)/nrow(d),
    auc=weighted_auc(risk,v$y,v$weight),
    calibration_intercept=if(is.null(intercept)) NA_real_ else unname(intercept[1]),
    calibration_slope=if(is.null(slope)) NA_real_ else unname(slope[2]))
}

# REVIEW NOTE: survreg predicts log-time location, not log hazard. Weibull shape is 1/scale and time scale is exp(lp). The lognormal sensitivity model has a different CDF.
risk5 <- function(fit,d) {
  lp <- as.numeric(predict(fit,newdata=d,type='lp'))
  if(fit$dist=='weibull') pweibull(5,shape=1/fit$scale,scale=exp(lp))
  else plnorm(5,meanlog=lp,sdlog=fit$scale)
}

# REVIEW NOTE: Keep the same evaluation rows and censoring convention for both models. Return five metrics in a stable order required by the bootstrap arrays.
pair_metrics <- function(pair,d,position='midpoint') {
  vapply(pair,function(f) c(clinical_metrics(d,risk5(f,d),position),
                           interval_log_score=interval_score(f,d)),numeric(5))
}

# REVIEW NOTE: Bootstrap whole households. A repeated draw gets a new cluster label so the nested bootstrap treats duplicate draws as separate bootstrap sampling units.
sample_households <- function(d) {
  clusters <- split(seq_len(nrow(d)),d$household)
  draws <- sample(seq_along(clusters),length(clusters),replace=TRUE)
  pieces <- lapply(seq_along(draws),function(i) {
    z <- d[clusters[[draws[i]]],,drop=FALSE]
    z$household <- as.character(i) # Keep repeated draws distinct in nested resampling.
    z
  })
  do.call(rbind,pieces)
}

# REVIEW NOTE: Outer resamples estimate optimism. Inner resamples correct each outer-sample apparent estimate for interval construction. Require paired finite values for both models and at least 90% successful resamples. Paired intervals use within-draw differences, not subtraction of marginal interval endpoints.
clinical_validation <- function(d,pair,B=200L,inner=50L) {
  apparent <- pair_metrics(pair,d)
  optimism <- array(NA_real_,c(B,5,2))
  corrected_samples <- array(NA_real_,c(B,5,2))
  for(b in seq_len(B)) {
    if(b==1 || b%%5==0) message('Validation outer bootstrap ',b,'/',B,'; inner repetitions: ',inner)
    boot <- sample_households(d)
    outer <- tryCatch({
      fits <- fit_pair(boot)
      train <- pair_metrics(fits,boot); test <- pair_metrics(fits,d)
      list(fits=fits,train=train,test=test)
    },error=function(e) NULL)
    if(is.null(outer)) next
    optimism[b,,] <- outer$train-outer$test
    inner_bias <- array(NA_real_,c(inner,5,2))
    for(j in seq_len(inner)) {
      ib <- sample_households(boot)
      bias <- tryCatch({
        f <- fit_pair(ib)
        pair_metrics(f,ib)-pair_metrics(f,boot)
      },error=function(e) NULL)
      if(!is.null(bias)) inner_bias[j,,] <- bias
    }
    for(m in 1:5) {
      good <- is.finite(inner_bias[,m,1])&is.finite(inner_bias[,m,2])
      if(sum(good)>=ceiling(.9*inner))
        corrected_samples[b,m,] <- outer$train[m,]-colMeans(matrix(inner_bias[good,m,],ncol=2))
    }
    if(b%%25==0) message('Validation bootstrap ',b,'/',B,' completed.')
  }
  rows <- list()
  for(m in 1:5) {
    good <- is.finite(optimism[,m,1])&is.finite(optimism[,m,2])
    corrected <- if(sum(good)>=ceiling(.9*B)) apparent[m,]-colMeans(matrix(optimism[good,m,],ncol=2)) else c(NA_real_,NA_real_)
    valid_ci <- is.finite(corrected_samples[,m,1])&is.finite(corrected_samples[,m,2])
    enough <- sum(valid_ci)>=ceiling(.9*B)
    for(k in 1:3) {
      samples <- if(k<=2) corrected_samples[,m,k] else corrected_samples[,m,2]-corrected_samples[,m,1]
      ci <- if(enough) quantile(samples[valid_ci],c(.025,.975),names=FALSE) else c(NA_real_,NA_real_)
      rows[[length(rows)+1]] <- data.frame(metric=rownames(apparent)[m],
        target=c('current','prior','prior_minus_current')[k],
        apparent=if(k<=2) apparent[m,k] else apparent[m,2]-apparent[m,1],
        optimism_corrected=if(k<=2) corrected[k] else corrected[2]-corrected[1],
        ci_lower=ci[1],ci_upper=ci[2],outer_success=sum(good),nested_success=sum(valid_ci),
        status=if(enough&&all(is.finite(corrected))) 'OK' else 'UNSTABLE')
    }
  }
  do.call(rbind,rows)
}

if(sys.nframe()==0L) {
  script <- sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  directory <- dirname(normalizePath(script))
  source(file.path(directory,'hrs_stage2.R'))
  source(file.path(directory,'hrs_source_hierarchy.R'))
  source(file.path(directory,'hrs_analysis.R'))
  set.seed(20260926)
  analysis_main(commandArgs(trailingOnly=TRUE),directory,extended=TRUE)
}
```
