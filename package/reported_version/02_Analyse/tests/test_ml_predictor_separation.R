# Generated positive control: history adds a deliberately strong signal.
source('scripts/hrs_ml_extension.R');ml_source_existing('scripts')
d<-ml_simulate(900,'small',20260930)
set.seed(42);mu<-log(8)+.10*(d$grip10-d$grip14)
t<-rweibull(nrow(d),1.4,exp(mu));event<-t<5
d$lo<-ifelse(event,pmax(.00001,t-.005),5)
d$hi<-ifelse(event,t+.005,Inf);d$hi[d$hi>5]<-Inf
pc<-ml_preprocess(d,'current');pp<-ml_preprocess(d,'prior')
stopifnot(!'grip10' %in% pc$columns,'grip10' %in% pp$columns)
shift<-d;shift$grip10<-pmin(99,shift$grip10+10);shift$change_kg<-shift$grip14-shift$grip10
stopifnot(identical(ml_matrix(pc,d),ml_matrix(pc,shift)),
 !identical(ml_matrix(pp,d),ml_matrix(pp,shift)))
for(model in c('weibull','neural','xgboost')) {
 par<-ml_grid(model,FALSE)[[1]]
 if(model=='neural') par$lambda<-.001
 if(model=='xgboost') {par$depth<-2;par$rounds<-200}
 current<-ml_fit(d,model,'current',par,37)
 prior<-ml_fit(d,model,'prior',par,37)
 a<-ml_predict(current,d)$risk;b<-ml_predict(prior,d)$risk
 stopifnot(identical(a,ml_predict(current,shift)$risk),
   max(abs(b-ml_predict(prior,shift)$risk))>.001,max(abs(a-b))>.001)
 cat('PASS: prior-grip perturbation changes only extended predictions:',model,'\n')
}
# Independent weighted pairwise AUC oracle including ties and zero weights.
r<-c(.2,.2,.6,.8,.9);y<-c(0,1,1,0,1);w<-c(1,2,0,3,4)
oracle<-sum(outer(seq_along(r),seq_along(r),Vectorize(function(i,j)
 w[i]*w[j]*(y[i]==1)*(y[j]==0)*((r[i]>r[j])+.5*(r[i]==r[j])))))/
 (sum(w[y==1])*sum(w[y==0]))
stopifnot(abs(weighted_auc(r,y,w)-oracle)<1e-12)
cat('PASS: independent weighted pairwise AUC oracle.\n')
# Reverse-KM oracle: one censoring at t=1 among three at risk gives G=2/3.
train<-data.frame(lo=c(.5,1,3,5),hi=c(.6,Inf,3.1,Inf))
test<-data.frame(lo=c(.5,2,5,.8),hi=c(.6,2.1,Inf,Inf))
weights<-ml_weights(train,test)
stopifnot(max(abs(weights$weight-c(1,1.5,1.5,0)))<1e-12)
cat('PASS: independent censoring-weight oracle with an early censored test observation.\n')
