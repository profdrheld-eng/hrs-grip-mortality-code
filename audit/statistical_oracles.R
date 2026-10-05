# Invented records only. Run with a materialized code tree as the sole argument.
# Quadratic concordance below is deliberately independent of the sorting algorithm.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1L)
scripts <- file.path(normalizePath(args[1],mustWork=TRUE),'02_Analyse','scripts')
source(file.path(scripts,'hrs_prediction_validation.R'))
source(file.path(scripts,'hrs_ml_extension.R'))
set.seed(7102026)
for (b in seq_len(200L)) {
  n <- sample(8:40,1)
  risk <- sample(seq(0,1,length.out=8),n,replace=TRUE)
  y <- sample(c(0,1),n,replace=TRUE);y[1:2] <- c(0,1)
  w <- runif(n,0,3);w[sample.int(n,2)] <- 0
  i <- which(y==1);j <- which(y==0)
  den <- sum(w[i])*sum(w[j])
  if(den==0) next
  concordance <- outer(risk[i],risk[j],'>') + .5*outer(risk[i],risk[j],'==')
  oracle <- sum(concordance*outer(w[i],w[j]))/den
  stopifnot(abs(weighted_auc(risk,y,w)-oracle)<1e-12)
  # Reordering records must not affect concordance, including zero weights/ties.
  perm <- sample.int(n)
  stopifnot(abs(weighted_auc(risk[perm],y[perm],w[perm])-oracle)<1e-12)
}
for (b in seq_len(100L)) {
  n <- 80L;event <- sample(c(TRUE,FALSE),n,replace=TRUE)
  lo <- sample(c(.25,.5,1,2,3,4),n,replace=TRUE)
  hi <- ifelse(event,pmin(lo+.25,5),Inf)
  # Retain known horizon survivors, preventing artificial positivity collapse.
  lo[1:20] <- 5;hi[1:20] <- Inf
  d <- data.frame(lo=lo,hi=hi)
  for(position in c('lower','midpoint','upper')) {
    one <- horizon_weights(d,position);two <- ml_weights(d,d,position)
    stopifnot(identical(one$y,two$y),max(abs(one$weight-two$weight))<1e-12)
    time <- lo;finite <- is.finite(hi)
    time[finite] <- switch(position,lower=lo[finite],midpoint=(lo[finite]+hi[finite])/2,upper=hi[finite])
    # Independent step-product definition: death at a time leaves before censoring.
    ct <- sort(unique(time[!finite & time<5]))
    eval <- ifelse(finite,time,5)
    ref <- vapply(seq_len(n),function(i) {
      if(!finite[i] && lo[i]<5)return(0)
      survival <- prod(vapply(ct[ct<eval[i]],function(t) {
        censor_count <- sum(!finite & time==t)
        denominator <- n-sum(time<t)-sum(finite & time==t)
        1-censor_count/denominator
      },0))
      1/survival
    },0)
    stopifnot(max(abs(one$weight-ref))<1e-12)
  }
}
cat('PASS: 200 randomized weighted-AUC oracles and 300 tied-time censoring-weight comparisons. Artificial data only.\n')
