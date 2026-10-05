source('scripts/hrs_analysis.R')
source('scripts/hrs_prediction_validation.R')
d <- data.frame(lo=c(1,2,5,5),hi=c(1.2,2.2,Inf,Inf))
r <- c(.8,.6,.3,.1)
m <- clinical_metrics(d,r)
stopifnot(abs(m['brier']-mean((c(1,1,0,0)-r)^2))<1e-12,m['auc']==1)
stopifnot(weighted_auc(c(.5,.5),c(1,0),c(1,1))==.5)
# Early censoring receives zero outcome weight, rather than a survivor label.
x <- data.frame(lo=c(1,2,5),hi=c(Inf,2.1,Inf))
w <- horizon_weights(x)
stopifnot(w$weight[1]==0,all(abs(w$weight[2:3]-1.5)<1e-12))
# Censoring at the administrative horizon must not make G(5-) zero.
stopifnot(all(horizon_weights(d)$weight==1))
cat('Synthetic Brier, weighted AUC, early censoring and horizon ties passed.\n')
# Independent brute-force AUC check, including ties and unequal weights.
p <- c(.1,.4,.4,.7,.9); y <- c(1,0,1,0,1); w <- c(2,1,3,2,1)
num <- 0
for(i in which(y==1)) for(j in which(y==0))
 num <- num+w[i]*w[j]*((p[i]>p[j])+.5*(p[i]==p[j]))
stopifnot(abs(weighted_auc(p,y,w)-num/(sum(w[y==1])*sum(w[y==0])))<1e-12)
set.seed(91); n <- 500
data <- data.frame(household=rep(seq_len(n/2),each=2),age=runif(n,50,85),
 sex=factor(sample(1:2,n,TRUE)),smoke=factor(sample(c('never','former','current'),n,TRUE)),
 health=factor(sample(1:5,n,TRUE)),grip14=rnorm(n,30,6),grip10=rnorm(n,32,6))
tt <- rweibull(n,1.4,exp(2-.02*(data$age-65)))
data$lo <- pmin(5,pmax(.001,floor(tt*12)/12)); data$hi <- ifelse(tt<5,data$lo+1/12,Inf)
pair <- fit_pair(data)
stopifnot(all(is.finite(pair_metrics(pair,data))))
validation <- clinical_validation(data,pair,B=6,inner=3)
stopifnot(nrow(validation)==15,all(validation$status=='OK'),
 all(validation$ci_lower<=validation$ci_upper))
# Group sizes remain paired even when the same source household is drawn twice.
sampled <- sample_households(data)
stopifnot(length(unique(sampled$household))==n/2,all(table(sampled$household)==2))
cat('Synthetic weighted-rank, prediction, nested-bootstrap and cluster-resampling checks passed.\n')
