# Figure contains model-level aggregate estimates, no participant-level points.
args<-commandArgs(trailingOnly=TRUE)
if(!length(args) %in% c(2,3)) stop('Supply PAIRED_95_CSV NEW_PDF.')
if(file.exists(args[2])) stop('Output exists.')
d<-read.csv(args[1],stringsAsFactors=FALSE)
stopifnot(nrow(d)==6,all(d$confidence_level==.95),all(d$interval_method=='basic'))
draws<-if(length(args)==3) read.csv(args[3],stringsAsFactors=FALSE) else NULL
options(scipen=999)
models<-c('weibull','neural','xgboost');labels<-c('Weibull splines','Neural AFT','XGBoost AFT')
cols<-c('#087F8C','#7956A5','#D47735')
if(grepl('[.]png$',args[2],ignore.case=TRUE)) png(args[2],width=2050,height=900,res=250,type='cairo') else
  pdf(args[2],width=8.2,height=3.6,family='Helvetica',useDingbats=FALSE)
par(mfrow=c(1,2),mar=c(4.5,7,2.2,1),oma=c(1.5,0,0,0),bty='n',las=1,cex=.9)
for(metric in c('auc','brier')) {
 z<-d[d$metric==metric,];z<-z[match(models,z$model),]
 xlim<-range(c(0,z$ci_lower,z$ci_upper));xlim<-xlim+c(-1,1)*diff(xlim)*.08
 plot(z$difference,3:1,type='n',xlim=xlim,ylim=c(.5,3.5),yaxt='n',
   ylab='',xlab=if(metric=='auc') 'Change in five-year AUC' else 'Change in five-year Brier score',
   main=if(metric=='auc') 'A  Discrimination' else 'B  Prediction error')
 if(!is.null(draws)) for(m in seq_along(models)) {
   v<-draws$difference[draws$model==models[m]&draws$metric==metric]
   stopifnot(length(v)==500,all(is.finite(v)))
   set.seed(19+m);points(v,4-m+runif(length(v),-.12,.12),pch=16,cex=.22,col=adjustcolor(cols[m],alpha.f=.18))
 }
 axis(2,at=3:1,labels=labels,tick=FALSE);abline(v=0,col='gray55',lty=2)
 segments(z$ci_lower,3:1,z$ci_upper,3:1,col=cols,lwd=2)
 points(z$difference,3:1,pch=19,col=cols,cex=1.1)
}
mtext('Current plus prior minus current grip; original training; conditional basic 95% confidence intervals',side=1,outer=TRUE,cex=.72)
dev.off()
