# Aggregate-only publication figures; base R, no new packages or person-level reads.
args<-commandArgs(trailingOnly=TRUE)
if(!length(args) %in% c(3,4)) stop('Usage: Rscript hrs_ml_figures.R ORIGINAL_ML_DIR INFERENCE_DIR NEW_OUTPUT_DIR')
old<-normalizePath(args[1]);new<-normalizePath(args[2]);out<-args[3]
if(dir.exists(out)) stop('Use a new output directory; previous figures are preserved.')
for(p in c(old,new,file.path(new,'exploratory_equivalence')))
 stopifnot(startsWith(readLines(file.path(p,'STATUS.txt'),warn=FALSE)[1],'SUCCESS:'))
read<-function(p,n) read.csv(file.path(p,n),stringsAsFactors=FALSE)
m<-read(new,'benchmark_metrics.csv');lc<-read(old,'benchmark_metrics.csv')
cal<-read(new,'benchmark_calibration.csv');sim<-read(old,'simulation_repetitions.csv')
eq<-read(file.path(new,'exploratory_equivalence'),'conditional_tost.csv')
draws<-read(new,'benchmark_bootstrap_differences.csv')
stopifnot(nrow(m)==12,nrow(eq)==360,all(m$status=='OK'),all(cal$status=='OK'))
key<-function(x) paste(x$model,x$target,x$arm)
a<-lc[lc$fraction==1,];ix<-match(key(m),key(a))
stopifnot(!anyNA(ix),max(abs(m$auc-a$auc[ix]))<1e-12,max(abs(m$brier-a$brier[ix]))<1e-12)
dir.create(out,recursive=TRUE)
colours<-c(weibull='#087F8C',neural='#7956A5',xgboost='#D47735')
models<-names(colours);names_model<-c('Weibull','Neural AFT','XGBoost')
options(scipen=999)
setup<-function(rows=1,cols=2,mar=c(4.5,9,1.5,1)) {
 par(mfrow=c(rows,cols),mar=mar,oma=c(3.5,0,0,0),family='Helvetica',font=2,font.axis=2,font.lab=2,
     pty='m',cex=.96,cex.axis=.86,cex.lab=1,mgp=c(3.6,.7,0),tcl=-.25,bty='n',las=1)
}
axisx<-function(x,digits=3) {at<-pretty(x,4);at<-at[at>=min(x)&at<=max(x)];axis(1,at,formatC(at,format='f',digits=digits))}
axisy<-function(x,digits=3) {at<-pretty(x,4);at<-at[at>=min(x)&at<=max(x)];axis(2,at,formatC(at,format='f',digits=digits))}
letter<-function(x) mtext(x,3,adj=0,line=.25,font=2,cex=1.05)
# Use ASCII-compatible symbol legends through a dedicated overlay, not PDF glyphs.
legendarm<-function() {par(pty='m',oma=c(0,0,0,0),fig=c(0,1,0,.05),new=TRUE,mar=c(0,0,0,0));plot.new();legend('center',c(names_model,'Original training','Original + synthetic'),col=c(colours,'#202833','#202833'),pch=c(15,15,15,16,17),horiz=TRUE,bty='n',text.font=2,cex=.8)}
whisker<-function(x,l,u,y,col,pch=16) {segments(l,y,u,y,col=col,lwd=2);segments(c(l,u),y-.045,c(l,u),y+.045,col=col,lwd=1.4);points(x,y,pch=pch,col=col,cex=1.1)}
labels6<-as.vector(t(outer(names_model,c('Current','Current + prior'),paste,sep='\n')))
F1<-function() {
 setup();for(metric in c('auc','brier')) {
  lim<-range(m[[paste0(metric,'_lower')]],m[[paste0(metric,'_upper')]]);lim<-lim+c(-1,1)*diff(lim)*.06
  plot(NA,xlim=lim,ylim=c(.5,6.5),axes=FALSE,xlab=if(metric=='auc') 'Five-year AUC' else 'Five-year Brier score',ylab='')
  axisx(lim);axis(2,6:1,labels6,tick=FALSE,cex.axis=.79)
  for(j in 1:3) for(t in 1:2) for(arm in c('original','augmented')) {
   z<-m[m$model==models[j]&m$target==c('current','prior')[t]&m$arm==arm,];y<-7-((j-1)*2+t)+if(arm=='original') .12 else -.12
   whisker(z[[metric]],z[[paste0(metric,'_lower')]],z[[paste0(metric,'_upper')]],y,colours[j],if(arm=='original') 16 else 17)
  };letter(if(metric=='auc') 'A' else 'B')
 };legendarm()
}
F2<-function() {
 set.seed(20260930)
 setup(mar=c(4.5,9,1.5,1));labs<-as.vector(t(outer(names_model,c('Original','+ synthetic'),paste,sep=' | ')))
 positions<-as.vector(t(outer(c(5.5,3.5,1.5),c(.25,-.25),'+')))
 for(metric in c('auc','brier')) {
  z<-eq[eq$contrast=='prior_minus_current'&eq$metric==metric,];z<-z[!duplicated(key(z)),]
  bounds<-if(metric=='auc') c(.001,.002,.005) else c(.0002,.0005,.001)
  bd<-draws$difference[draws$contrast=='prior_minus_current'&draws$metric==metric]
  limit<-max(abs(c(z$ci90_lower,z$ci90_upper,bd,bounds)))*1.05
  plot(NA,xlim=c(-limit,limit),ylim=c(.5,6.5),axes=FALSE,xlab=if(metric=='auc') 'AUC difference' else 'Brier score difference',ylab='')
  for(i in 3:1) rect(-bounds[i],par('usr')[3],bounds[i],par('usr')[4],col=c('#E2E7EC','#EDF0F3','#F6F7F9')[i],border=NA)
  abline(v=0,col='#66707A',lwd=1.4)
  axisx(c(-limit,limit),if(metric=='auc') 3 else 4);axis(2,positions,labs,tick=FALSE,cex.axis=.8)
  mtext(if(metric=='auc') 'Favors current only' else 'Favors current + prior',1,line=-1,adj=0,font=2,cex=.70)
  mtext(if(metric=='auc') 'Favors current + prior' else 'Favors current only',1,line=-1,adj=1,font=2,cex=.70)
  for(j in 1:3) for(ar in 1:2) {
   v<-z[z$model==models[j]&z$arm==c('original','augmented')[ar],]
   cloud<-draws[draws$contrast=='prior_minus_current'&draws$metric==metric&draws$model==models[j]&draws$arm==c('original','augmented')[ar],]
   stopifnot(nrow(cloud)==5000,all(is.finite(cloud$difference)))
   # Deterministic display sample; intervals and tests still use all 5,000 draws.
   cloud<-cloud[order(cloud$replicate),];cloud<-cloud[seq(1,nrow(cloud),length.out=min(500,nrow(cloud))),]
   y<-positions[(j-1)*2+ar]
   points(cloud$difference,y+runif(nrow(cloud),-.17,.17),pch=if(ar==1) 16 else 17,cex=.4,col=adjustcolor(colours[j],alpha.f=if(ar==1) .24 else .10))
   whisker(v$estimate,v$ci90_lower,v$ci90_upper,y,adjustcolor(colours[j],alpha.f=if(ar==1) 1 else .55),if(ar==1) 16 else 17)
  };letter(if(metric=='auc') 'A' else 'B')
 }
}
F3<-function() {
 on.exit(par(pty='m'))
 setup(2,3,c(4.8,5.5,2.2,.8));lim<-c(0,ceiling(max(cal$predicted,cal$observed)*100/5)*5)
 par(pty='s')
 for(target in c('current','prior')) for(j in 1:3) {
  plot(NA,xlim=lim,ylim=lim,axes=FALSE,xlab=if(target=='prior') 'Predicted risk (%)' else '',ylab=if(j==1) 'Observed risk (%)' else '',asp=1)
  axis(1);axis(2);abline(0,1,col='#8C939B',lty=2,lwd=1.5)
  for(ar in 1:2) {
   z<-cal[cal$model==models[j]&cal$target==target&cal$arm==c('original','augmented')[ar],];z<-z[order(z$group),]
   colour<-adjustcolor(colours[j],alpha.f=if(ar==1) 1 else .5)
   lines(100*z$predicted,100*z$observed,col=colour,lty=ar,lwd=1.1)
   points(100*z$predicted,100*z$observed,col=colour,pch=if(ar==1) 16 else 17,cex=1)
  }
  mtext(paste(names_model[j],if(target=='current') '| Current' else '| Current + prior'),3,line=.5,cex=.85,font=2)
 };legendarm()
}
F4<-function() {
 setup(2,3,c(2.5,5.6,1.8,.6));set.seed(20260930)
 z<-sim[sim$contrast=='prior_minus_current'&sim$metric %in% c('auc','brier'),]
 for(metric in c('auc','brier')) {
  lim<-range(z$difference[z$metric==metric]);lim<-lim+c(-1,1)*diff(lim)*.08
  for(sc in c('null','small','interaction')) {
   plot(NA,xlim=c(.55,3.45),ylim=lim,axes=FALSE,xlab='',ylab=if(sc=='null') {if(metric=='auc') 'AUC difference' else 'Brier score difference'} else '')
   axis(1,1:3,names_model,tick=FALSE,cex.axis=.78);axisy(lim,if(metric=='auc') 3 else 4);abline(h=0,col='#8C939B',lwd=1.3)
   for(j in 1:3) for(ar in 1:2) {
    v<-z[z$metric==metric&z$scenario==sc&z$model==models[j]&z$arm==c('original','augmented')[ar],]
    stopifnot(nrow(v)==100,length(unique(v$repetition))==100)
    x<-j+if(ar==1) -.16 else .16
    points(x+runif(100,-.095,.095),v$difference,pch=if(ar==1) 16 else 17,col=adjustcolor(colours[j],alpha.f=if(ar==1) .30 else .16),cex=.7)
    segments(x-.085,mean(v$difference),x+.085,mean(v$difference),lwd=3,col='#202833')
   }
   if(metric=='auc') mtext(c(null='No history signal',small='Small history signal',interaction='Age-by-history signal')[sc],3,line=.45,font=2,cex=.9)
   if(metric=='brier') mtext('Favors current + prior: AUC > 0; Brier < 0',1,line=1.7,font=2,cex=.58)
  }
 };legendarm()
}
F5<-function() {
 setup(2,2,c(2.5,6,1.8,.8));par(oma=c(5,0,0,0))
 for(metric in c('auc','brier')) {
  lim<-range(lc[[paste0(metric,'_lower')]],lc[[paste0(metric,'_upper')]]);lim<-lim+c(-1,1)*diff(lim)*.05
  for(target in c('current','prior')) {
   plot(NA,xlim=c(46,104),ylim=lim,axes=FALSE,xlab='',ylab=if(target=='current') {if(metric=='auc') 'Five-year AUC' else 'Five-year Brier score'} else '')
   axis(1,c(50,75,100));axisy(lim)
   for(j in 1:3) for(ar in 1:2) {
    z<-lc[lc$model==models[j]&lc$target==target&lc$arm==c('original','augmented')[ar],];z<-z[order(z$fraction),];x<-100*z$fraction
    segments(x,z[[paste0(metric,'_lower')]],x,z[[paste0(metric,'_upper')]],col=adjustcolor(colours[j],alpha.f=.2),lwd=2)
    colour<-adjustcolor(colours[j],alpha.f=if(ar==1) 1 else .45)
    lines(x,z[[metric]],col=colour,lwd=2,lty=ar);points(x,z[[metric]],pch=if(ar==1) 16 else 17,col=colour,cex=.9)
   };if(metric=='auc') mtext(if(target=='current') 'Current grip' else 'Current + prior grip',3,line=.45,font=2,cex=.9)
  }
 };mtext('Available training households (%)',1,outer=TRUE,line=.6,font=2,cex=.9);legendarm()
}
F6<-function() {
 setup(1,2,c(5.5,9,1.6,.6));labs<-as.vector(t(outer(names_model,c('Original','+ synthetic'),paste,sep=' | ')))
 for(metric in c('auc','brier')) {
  z<-eq[eq$contrast=='prior_minus_current'&eq$metric==metric,];bounds<-sort(unique(z$margin))
  plot(NA,xlim=c(.5,9.5),ylim=c(.5,6.5),axes=FALSE,xlab='',ylab='')
  for(j in 1:3) for(ar in 1:2) for(k in 1:9) {
   v<-z[z$model==models[j]&z$arm==c('original','augmented')[ar]&z$margin==bounds[k],];stopifnot(nrow(v)==1)
   y<-7-((j-1)*2+ar);rect(k-.43,y-.37,k+.43,y+.37,col=if(v$equivalent_holm) colours[j] else '#EDF0F3',border=NA)
   if(v$equivalent_holm) points(k,y,pch=16,col='white',cex=.6)
  }
  axis(1,1:9,format(bounds,scientific=FALSE,trim=TRUE),las=2,tick=FALSE,cex.axis=.8);axis(2,6:1,labs,tick=FALSE,cex.axis=.8)
  mtext(if(metric=='auc') 'Hypothetical AUC margin (+/-)' else 'Hypothetical Brier margin (+/-)',1,line=4.3,font=2,cex=.9)
  letter(if(metric=='auc') 'A' else 'B')
 }
}
figures<-list(F01_model_comparison=F1,F02_history_equivalence=F2,F03_calibration=F3,F04_simulation_clouds=F4,F05_learning_curves=F5,F06_equivalence_grid=F6)
comparison_cloud<-function(contrast) {
 setup();set.seed(20260930)
 for(metric in c('auc','brier')) {
  z<-eq[eq$contrast==contrast&eq$metric==metric,];z<-z[!duplicated(key(z)),]
  z<-z[order(match(z$model,models),z$target,z$arm),]
  b<-draws[draws$contrast==contrast&draws$metric==metric,]
  lim<-range(c(b$difference,z$ci90_lower,z$ci90_upper,0));lim<-lim+c(-1,1)*diff(lim)*.03
  plot(NA,xlim=lim,ylim=c(.5,nrow(z)+.5),axes=FALSE,xlab=if(metric=='auc') 'AUC difference' else 'Brier score difference',ylab='')
  abline(v=0,col='#8C939B',lwd=1.3);axisx(lim,if(metric=='auc') 3 else 4)
  labs<-paste(names_model[match(z$model,models)],ifelse(z$target=='current','Current','Current + prior'),sep='\n')
  if(contrast=='model_minus_weibull') labs<-paste0(labs,ifelse(z$arm=='original',' | Original',' | + synthetic'))
  axis(2,rev(seq_len(nrow(z))),labs,tick=FALSE,cex.axis=.68)
  for(i in seq_len(nrow(z))) {
   v<-b[b$model==z$model[i]&b$target==z$target[i]&b$arm==z$arm[i],];v<-v[order(v$replicate),]
   stopifnot(nrow(v)==5000,all(is.finite(v$difference)))
   v<-v[seq(1,nrow(v),length.out=500),];y<-nrow(z)+1-i;co<-colours[z$model[i]]
   points(v$difference,y+runif(nrow(v),-.24,.24),pch=16,cex=.4,col=adjustcolor(co,alpha.f=.16))
   whisker(z$estimate[i],z$ci90_lower[i],z$ci90_upper[i],y,co)
  };letter(if(metric=='auc') 'A' else 'B')
 }
}
figures$F07_synthetic_training_clouds<-function() comparison_cloud('augmented_minus_original')
figures$F08_model_difference_clouds<-function() comparison_cloud('model_minus_weibull')
# Three distinct paired questions, with AUC and Brier in adjacent columns.
Fcombined<-function(rows=1:3,training=NULL,full_background=FALSE) {
 setup(length(rows),2,c(4.8,if(length(rows)==1) 11 else 9,2.8,1));par(oma=c(0,0,0,0),cex=.92,mgp=c(2.5,.7,0))
 set.seed(20260930)
 contrasts<-c('prior_minus_current','augmented_minus_original','model_minus_weibull')
 titles<-c('Added prior grip','Synthetic augmentation','Model comparison')
 if(!is.null(training)) titles[1]<-paste('Added prior grip |',if(training=='original') 'Original training' else 'Augmented training')
 for(r in rows) for(metric in c('auc','brier')) {
  contrast<-contrasts[r]
  z<-eq[eq$contrast==contrast&eq$metric==metric,];z<-z[!duplicated(key(z)),]
  z<-z[order(match(z$model,models),match(z$target,c('current','prior')),match(z$arm,c('original','augmented'))),]
  stopifnot(nrow(z)==c(6,6,8)[r])
  if(!is.null(training)) z<-z[z$arm==training,]
  positions<-if(!is.null(training)) 3:1 else rep(rev(seq_len(nrow(z)/2))*1.5,each=2)+rep(c(.20,-.20),nrow(z)/2)
  b<-draws[draws$contrast==contrast&draws$metric==metric,]
  if(!is.null(training)) b<-b[b$arm==training,]
  bounds<-if(metric=='auc') c(.001,.002,.005) else c(.0002,.0005,.001)
  scale_draws<-if(r==1) draws$difference[draws$contrast==contrast & draws$metric==metric] else b$difference
  scale_ci<-if(r==1) unlist(eq[eq$contrast==contrast & eq$metric==metric,c('ci90_lower','ci90_upper')]) else c(z$ci90_lower,z$ci90_upper)
  lim<-range(c(scale_draws,scale_ci,0,if(r==1) c(-bounds,bounds)))
  lim<-lim+c(-1,1)*diff(lim)*.04
  if(r==1) lim<-c(-1,1)*max(abs(lim))
  plot(NA,xlim=lim,ylim=c(.75,max(positions)+.45),axes=FALSE,xlab=if(metric=='auc') 'AUC difference' else 'Brier score difference',ylab='')
  if(r==1) {
   if(full_background) rect(par('usr')[1],par('usr')[3],par('usr')[2],par('usr')[4],col='#F6F7F9',border=NA)
   for(k in (if(full_background) 2 else 3):1) rect(-bounds[k],par('usr')[3],bounds[k],par('usr')[4],col=c('#E2E7EC','#EDF0F3','#F6F7F9')[k],border=NA)
  }
  abline(v=0,col='#7C858E',lwd=1.3);axisx(lim,if(metric=='auc') 3 else 4)
  labs<-names_model[match(z$model,models)]
  if(r==1 && is.null(training)) labs<-paste(labs,ifelse(z$arm=='original','Original','+ synthetic'),sep=' | ')
  if(r==2) labs<-paste(labs,ifelse(z$target=='current','Current','Current + prior'),sep=' | ')
  if(r==3) labs<-paste(labs,ifelse(z$target=='current','C','C+P'),ifelse(z$arm=='original','Original','+ synth.'),sep=' | ')
  axis(2,positions,labs,tick=FALSE,cex.axis=if(length(rows)==1) .82 else .68)
  first<-c('current + prior','augmented','ML model')[r];reference<-c('current only','original','Weibull')[r]
  mtext(paste('Favors',if(metric=='auc') reference else first),1,line=-1,adj=0,cex=.65,font=2)
  mtext(paste('Favors',if(metric=='auc') first else reference),1,line=-1,adj=1,cex=.65,font=2)
  for(i in seq_len(nrow(z))) {
   v<-b[b$model==z$model[i]&b$target==z$target[i]&b$arm==z$arm[i],];v<-v[order(v$replicate),]
   stopifnot(nrow(v)==5000,all(is.finite(v$difference)))
   v<-v[seq(1,nrow(v),length.out=500),]
   # An augmentation contrast contains both arms; do not label it as one arm.
   light<-r!=2 && z$arm[i]=='augmented';co<-colours[z$model[i]];pch<-if(light) 17 else 16
   points(v$difference,positions[i]+runif(500,-.14,.14),pch=pch,cex=.32,col=adjustcolor(co,alpha.f=if(light) .16 else .23))
   whisker(z$estimate[i],z$ci90_lower[i],z$ci90_upper[i],positions[i],adjustcolor(co,alpha.f=if(light) .65 else 1),pch)
  }
  mtext(paste(LETTERS[if(length(rows)==1) {if(metric=='auc') 1 else 2} else (r-1)*2+if(metric=='auc') 1 else 2],titles[r],sep='  '),3,line=.8,adj=0,cex=.88,font=2)
 }
}
if(length(args)==4 && args[4]=='combined') {
 figures<-list(F02_combined_comparisons=Fcombined,F04_simulation_clouds=F4,F05_learning_curves=F5)
 tab<-eq[eq$contrast=='prior_minus_current',]
 tab<-tab[order(tab$metric,match(tab$model,models),match(tab$arm,c('original','augmented')),tab$margin),]
 stopifnot(nrow(tab)==108,!anyDuplicated(tab[c('model','arm','metric','margin')]))
 write.csv(tab,file.path(out,'SUPPLEMENT_equivalence.csv'),row.names=FALSE)
 fmt<-function(x) formatC(x,format='f',digits=6)
 lines<-c('# Supplementary table: exploratory equivalence of added prior grip','',
 'Current plus prior minus current grip. Bounds are hypothetical and post hoc, not clinically established. Intervals are unadjusted conditional 90% basic bootstrap intervals; tests use fixed out-of-fold predictions and 5,000 household bootstrap draws. Holm adjustment covers all 20 contrasts within each metric and margin, although this table shows only the six prior-grip contrasts. Not established does not imply a meaningful difference. No exact equality claim.','',
 '| Metric | Model | Training | Estimate | 90% CI lower | 90% CI upper | Bound (+/-) | TOST p | Holm p | Equivalence established |',
 '|---|---|---|---:|---:|---:|---:|---:|---:|---|')
 for(i in seq_len(nrow(tab))) {v<-tab[i,];lines<-c(lines,paste0('| ',paste(c(toupper(v$metric),names_model[match(v$model,models)],if(v$arm=='original') 'Original' else 'Original + synthetic',fmt(v$estimate),fmt(v$ci90_lower),fmt(v$ci90_upper),fmt(v$margin),fmt(v$p_tost),fmt(v$p_holm),if(v$equivalent_holm) 'Yes' else 'Not established'),collapse=' | '),' |'))}
 writeLines(lines,file.path(out,'SUPPLEMENT_equivalence.md'))
 writeLines(c('# Combined comparison figure','',
 'Rows: added prior grip (current plus prior minus current), synthetic augmentation (augmented minus original), model comparison (neural AFT or XGBoost minus Weibull). Columns: AUC and Brier differences. All comparisons hold the other settings fixed. Current/prior labels abbreviate grip terms; adjustment variables remain included. In panels E/F, C = current grip, C+P = current plus prior grip, and + synth. = original plus synthetic training.',
 'Small points show 500 deterministically selected household bootstrap replicates from 5,000, not participants. Large points are paired estimates; bars are unadjusted conditional 90% basic bootstrap intervals. Uncertainty does not include full retraining and tuning variation. Jitter is decorative.',
 'Within rows 1 and 3, original training is opaque/circular and augmented training lighter/triangular. Row 2 contrasts both training arms, so opacity does not encode arm there; paired rows distinguish predictor sets.',
 'Gray bands in row 1 are hypothetical bounds: AUC +/-0.001, 0.002, 0.005; Brier +/-0.0002, 0.0005, 0.001, dark to light. They do not establish Holm-adjusted or clinical equivalence. See the supplementary table. Axis ranges differ between questions.',
 'Placement: combined comparison in main text; F04 simulations, F05 learning curves, and equivalence table in supplement. Earlier individual figures remain archived; the equivalence heatmap is omitted from this selected set.'),file.path(out,'FIGURE_LEGENDS.md'))
}

if(length(args)==4 && args[4]=='split') {
 figures<-list(MAIN_prior_grip= function() Fcombined(1),
               SUPP_synthetic_training=function() Fcombined(2))
 writeLines(c('# Approved figure allocation, 2026-09-30','',
 'Main text: MAIN_prior_grip, two panels, prior plus current versus current grip. Three model pairs: original training first, original plus synthetic training second, lighter and triangular.',
 'Supplement: SUPP_synthetic_training, augmented versus original training within each model and predictor set. The separate prior-grip augmented figure is redundant and omitted; its results are now in the main figure.',
 'Direct comparisons of neural AFT and XGBoost against Weibull belong in a supplementary table, not another figure. Exploratory equivalence belongs in a supplementary table. Absolute model performance is intended for a main-text table.',
 'This decision replaces the previous six-panel main-text proposal. Previous files are retained. Manuscript integration and final numbering remain pending.'),file.path(out,'FIGURE_DECISION.md'))
 writeLines(c('# Figure legends','',
 'MAIN_prior_grip: Added prior grip under original and augmented training, shown as closely spaced pairs within model. Original is first and opaque/circular; original plus synthetic is second and lighter/triangular. Each estimate compares current plus prior with current grip, holding adjustment variables and training procedure fixed. Both arms are evaluated on real held-out participants.',
 'SUPP_synthetic_training: Augmented minus original training, holding model and predictor set fixed. Closely paired rows distinguish current and current plus prior grip. Each contrast includes both training arms, so opacity does not encode arm here.',
 'In every figure, A is AUC difference and B is Brier difference. Small points are 500 deterministic bootstrap draws selected from 5,000 household draws, not participants. Large points are estimates; bars are unadjusted conditional 90% basic bootstrap intervals. Uncertainty does not include full retraining or tuning. Vertical jitter has no quantitative meaning.',
 'Gray bands in the prior-grip figures denote hypothetical bounds, dark to light: AUC +/-0.001, 0.002, 0.005; Brier +/-0.0002, 0.0005, 0.001. They are not clinically established and do not substitute for Holm-adjusted equivalence tests.'),file.path(out,'FIGURE_LEGENDS.md'))
 tab<-eq[eq$contrast=='model_minus_weibull',]
 tab<-tab[!duplicated(tab[c('model','target','arm','metric')]),c('model','target','arm','metric','estimate','ci90_lower','ci90_upper','scope')]
 stopifnot(nrow(tab)==16)
 write.csv(tab,file.path(out,'SUPPLEMENT_model_differences.csv'),row.names=FALSE)
 lines<-c('# Direct model differences versus Weibull','', 'Matched predictor set and training arm. Unadjusted conditional 90% basic intervals, fixed OOF predictions; no full-pipeline uncertainty.', '', '| Model | Predictors | Training | Metric | Difference | 90% lower | 90% upper |','|---|---|---|---|---:|---:|---:|')
 for(i in seq_len(nrow(tab))) {v<-tab[i,];lines<-c(lines,paste0('| ',paste(c(names_model[match(v$model,models)],if(v$target=='current') 'Current' else 'Current + prior',if(v$arm=='original') 'Original' else 'Original + synthetic',toupper(v$metric),formatC(c(v$estimate,v$ci90_lower,v$ci90_upper),format='f',digits=6)),collapse=' | '),' |'))}
 writeLines(lines,file.path(out,'SUPPLEMENT_model_differences.md'))
}

for(name in names(figures)) {
 height<-if(length(args)==4 && args[4]=='split') 5 else if(name=='F02_combined_comparisons') 12 else if(name %in% c('F03_calibration','F04_simulation_clouds','F05_learning_curves')) 6.6 else 7.8
 pdf(file.path(out,paste0(name,'.pdf')),width=13,height=height,family='Helvetica',useDingbats=FALSE);figures[[name]]();dev.off()
 png(file.path(out,paste0(name,'.png')),width=3120,height=round(240*height),res=240,type='cairo');figures[[name]]();dev.off()
}
pdf(file.path(out,if(length(args)==4 && args[4]=='combined') 'SUPPLEMENT_FIGURES.pdf' else 'ALL_ML_FIGURES.pdf'),width=13,height=7.8,family='Helvetica',useDingbats=FALSE)
for(name in names(figures)) {
 if(name=='F02_combined_comparisons') next
 figures[[name]]()
}
dev.off()
paths<-c(file.path(old,c('benchmark_metrics.csv','simulation_repetitions.csv')),file.path(new,c('benchmark_metrics.csv','benchmark_calibration.csv','benchmark_bootstrap_differences.csv')),file.path(new,'exploratory_equivalence/conditional_tost.csv'))
h<-tools::md5sum(paths);write.csv(data.frame(source=names(h),md5=unname(h)),file.path(out,'source_hashes.csv'),row.names=FALSE)
writeLines(c(paste0('SUCCESS: ',length(figures),' aggregate-only figures; no model refits. Combined figure has its own full-height PDF.'), '12 variants; unchanged point estimates between runs; 100 repetitions per simulation group.', 'Intervals are conditional; margins hypothetical; heatmap uses Holm-adjusted tests.', 'Clouds display 500 deterministically selected bootstrap draws per contrast, not people.'),file.path(out,'CHECKS.txt'))
message('Figures saved: ',normalizePath(out))
