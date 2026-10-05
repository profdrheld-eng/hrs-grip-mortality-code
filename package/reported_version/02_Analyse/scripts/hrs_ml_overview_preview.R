# Aggregate-only four-panel alternative to Figure 1. Does not refit models.
a<-commandArgs(trailingOnly=TRUE)
if(length(a)!=2) stop('Usage: Rscript hrs_ml_overview_preview.R INFERENCE_DIR NEW_OUTPUT_DIR')
if(file.exists(a[2])) stop('Choose a new output directory.')
m<-read.csv(file.path(a[1],'benchmark_metrics.csv'))
e<-read.csv(file.path(a[1],'exploratory_equivalence','conditional_tost.csv'))
b<-read.csv(file.path(a[1],'benchmark_bootstrap_differences.csv'))
stopifnot(nrow(m)==12,all(m$status=='OK'))
dir.create(a[2],recursive=TRUE)
co<-c(weibull='#087F8C',neural='#7956A5',xgboost='#D47735')
models<-names(co);labels<-c('Weibull','Neural AFT','XGBoost')
options(scipen=999)
whisker<-function(x,l,u,y,col,pch) {
 segments(l,y,u,y,lwd=2,col=col);points(x,y,pch=pch,cex=1,col=col)
}
draw<-function() {
 set.seed(20260930)
 par(mfrow=c(2,2),mar=c(4.7,8.8,2,1),oma=c(2.8,0,0,0),family='Helvetica',
   font=2,font.axis=2,font.lab=2,cex=.9,las=1,mgp=c(3,.7,0),bty='n')
 for(metric in c('auc','brier')) {
  lim<-range(m[[paste0(metric,'_lower')]],m[[paste0(metric,'_upper')]])
  lim<-lim+c(-1,1)*diff(lim)*.06
  plot(NA,xlim=lim,ylim=c(.5,6.5),axes=FALSE,xlab=if(metric=='auc') 'Five-year AUC' else 'Five-year Brier score',ylab='')
  at<-pretty(lim,4);axis(1,at,formatC(at,format='f',digits=3))
  labs<-as.vector(t(outer(labels,c('Current','Current + prior'),paste,sep='\n')))
  axis(2,6:1,labs,tick=FALSE,cex.axis=.85)
  for(j in 1:3) for(t in 1:2) for(ar in 1:2) {
   z<-m[m$model==models[j]&m$target==c('current','prior')[t]&m$arm==c('original','augmented')[ar],]
   stopifnot(nrow(z)==1)
   y<-7-((j-1)*2+t)+c(.13,-.13)[ar]
   whisker(z[[metric]],z[[paste0(metric,'_lower')]],z[[paste0(metric,'_upper')]],y,co[j],c(16,17)[ar])
  }
  mtext(if(metric=='auc') 'A' else 'B',3,adj=0,line=.3,font=2)
 }
 for(metric in c('auc','brier')) {
  z<-e[e$contrast=='prior_minus_current'&e$metric==metric,]
  z<-z[!duplicated(paste(z$model,z$arm)),]
  bb<-b[b$contrast=='prior_minus_current'&b$metric==metric,]
  lim<-range(c(0,bb$difference,z$ci90_lower,z$ci90_upper));lim<-lim+c(-1,1)*diff(lim)*.05
  plot(NA,xlim=lim,ylim=c(.5,6.5),axes=FALSE,xlab=if(metric=='auc') 'AUC change from adding prior grip' else 'Brier change from adding prior grip',ylab='')
  at<-pretty(lim,4);axis(1,at,formatC(at,format='f',digits=if(metric=='auc') 3 else 4))
  labs<-as.vector(t(outer(labels,c('Original','+ synthetic'),paste,sep='\n')))
  axis(2,6:1,labs,tick=FALSE,cex.axis=.85);abline(v=0,lwd=1.4,col='#8C939B')
  for(j in 1:3) for(ar in 1:2) {
   arm<-c('original','augmented')[ar];v<-z[z$model==models[j]&z$arm==arm,]
   cloud<-bb[bb$model==models[j]&bb$arm==arm,];cloud<-cloud[order(cloud$replicate),]
   stopifnot(nrow(cloud)==5000,nrow(v)==1,all(is.finite(cloud$difference)))
   cloud<-cloud[seq(1,5000,length.out=500),];y<-7-((j-1)*2+ar)
   points(cloud$difference,y+runif(500,-.26,.26),pch=16,cex=.45,col=adjustcolor(co[j],alpha.f=.2))
   whisker(v$estimate,v$ci90_lower,v$ci90_upper,y,co[j],c(16,17)[ar])
  }
  mtext(if(metric=='auc') 'C' else 'D',3,adj=0,line=.3,font=2)
 }
 par(oma=c(0,0,0,0),fig=c(0,1,0,.04),new=TRUE,mar=c(0,0,0,0));plot.new()
 legend('center',c('Original training','Original + synthetic'),pch=c(16,17),horiz=TRUE,bty='n',text.font=2)
}
pdf(file.path(a[2],'FIGURE_1_PROPOSAL.pdf'),width=13,height=10.5,family='Helvetica',useDingbats=FALSE);draw();dev.off()
png(file.path(a[2],'FIGURE_1_PROPOSAL.png'),width=3120,height=2520,res=240,type='cairo');draw();dev.off()
writeLines(c('A/B: all 12 model variants; conditional 95% percentile intervals.',
 'C/D: current-plus-prior minus current grip; points show 500 deterministic selections from 5,000 conditional household bootstrap draws, not people.',
 'C/D: large symbols show the observed paired difference; bars show unadjusted 90% basic bootstrap intervals. No equivalence bounds or formal equivalence decisions are displayed.',
 'Higher AUC and lower Brier are better; positive C or negative D favors adding prior grip.',
 'Intervals are conditional on fixed out-of-fold predictions, not full training uncertainty.',
 'Current/prior refer to grip predictors; adjustment covariates remain included. This layout is a preview alternative, not an extra independent analysis.'),file.path(a[2],'LEGEND.txt'))
h<-tools::md5sum(file.path(a[1],c('benchmark_metrics.csv','benchmark_bootstrap_differences.csv','exploratory_equivalence/conditional_tost.csv')))
write.csv(data.frame(source=names(h),md5=unname(h)),file.path(a[2],'source_hashes.csv'),row.names=FALSE)
