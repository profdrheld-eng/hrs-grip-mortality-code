# User-run local person-point figures. No identifiers or prediction tables exported.
ml_point_changes <- function(risk,combos) {
  stopifnot(ncol(risk)==nrow(combos),all(is.finite(risk)),all(risk>=0 & risk<=1),
    !anyDuplicated(paste(combos$model,combos$target,combos$arm)))
  delta<-function(kind) {
    rows<-combos[if(kind=='history') combos$target=='prior' else combos$arm=='augmented',]
    values<-lapply(seq_len(nrow(rows)),function(i) {
      z<-rows[i,];j<-which(combos$model==z$model & combos$target==z$target & combos$arm==z$arm)
      k<-which(combos$model==z$model & combos$target==(if(kind=='history') 'current' else z$target) &
        combos$arm==(if(kind=='synthetic') 'original' else z$arm))
      stopifnot(length(j)==1,length(k)==1)
      100*(risk[,j]-risk[,k])
    })
    do.call(cbind,values)
  }
  list(history=delta('history'),synthetic=delta('synthetic'))
}

ml_replay_predictions <- function(d,cfg,tuning) {
  ml_validate(d);stopifnot(identical(as.numeric(cfg$fractions),1))
  fold<-ml_folds(d,cfg$outer,cfg$seed)
  combos<-expand.grid(model=cfg$models,target=c('current','prior'),arm=c('original','augmented'),stringsAsFactors=FALSE)
  risk<-matrix(NA_real_,nrow(d),nrow(combos));w<-data.frame(y=rep(NA_real_,nrow(d)),weight=NA_real_)
  for(k in seq_len(cfg$outer)) {
    message('Recreating selected models: fold ',k,'/',cfg$outer)
    train<-d[fold!=k,,drop=FALSE];test<-d[fold==k,,drop=FALSE]
    # Preserve benchmark row selection and RNG sequence exactly.
    set.seed(cfg$seed+k);groups<-sample(unique(train$household))
    groups<-head(groups,max(cfg$inner*2,ceiling(length(groups))))
    train<-train[train$household %in% groups,,drop=FALSE]
    w[fold==k,]<-ml_weights(train,test,'midpoint')
    for(j in seq_len(nrow(combos))) {
      c<-combos[j,];z<-tuning[tuning$fraction==1 & tuning$fold==k & tuning$model==c$model &
        tuning$target==c$target & tuning$arm==c$arm,]
      stopifnot(nrow(z)==1,is.finite(z$ratio),z$ratio>=0)
      parameter<-list()
      if(nzchar(z$parameter)) {
        parts<-strsplit(z$parameter,';',fixed=TRUE)[[1]]
        kv<-strsplit(parts,'=',fixed=TRUE);stopifnot(all(lengths(kv)==2))
        parameter<-setNames(lapply(kv,function(x) as.numeric(x[2])),vapply(kv,'[','',1))
        stopifnot(all(is.finite(unlist(parameter))))
      }
      seed<-cfg$seed+1000*k;prep<-ml_preprocess(train,c$target)
      aug<-ml_augment(train,z$ratio,seed,c$target)
      fit<-ml_fit(aug,c$model,c$target,parameter,seed,prep)
      risk[fold==k,j]<-ml_predict(fit,test)$risk
    }
  }
  stopifnot(all(is.finite(risk)))
  list(risk=risk,combos=combos,weights=w)
}

ml_render_person_points <- function(risk,combos,cal,out) {
  changes<-ml_point_changes(risk,combos)
  models<-c('weibull','neural','xgboost');co<-c('#087F8C','#7956A5','#D47735');names_model<-c('Weibull','Neural AFT','XGBoost')
  options(scipen=999)
  setup<-function(rows,cols,mar=c(4.5,5.7,2,.8)) par(mfrow=c(rows,cols),mar=mar,oma=c(2,0,0,0),
    family='Helvetica',font=2,font.axis=2,font.lab=2,las=1,bty='n',mgp=c(3.6,.7,0),cex=.95)
  label<-function(x) mtext(x,3,line=.5,font=2,cex=.9)
  f2<-function(kind) {
    setup(1,2,c(4.2,5.7,2,.8));par(oma=c(4,0,0,0));set.seed(19)
    {
      full<-range(c(0,changes[[kind]]));full<-full+c(-1,1)*max(diff(full),.01)*.05
      # Reuse identical horizontal jitter in full-range and zoom panels.
      values<-xs<-list()
      for(j in 1:3) for(a in 1:2) {
        ix<-which(combos$model==models[j]&combos$target==(if(kind=='history') 'prior' else c('current','prior')[a])&combos$arm==(if(kind=='history') c('original','augmented')[a] else 'augmented'))
        ref<-which(combos$model==models[j]&combos$target==(if(kind=='history') 'current' else c('current','prior')[a])&combos$arm==(if(kind=='history') c('original','augmented')[a] else 'original'))
        i<-(j-1)*2+a;values[[i]]<-100*(risk[,ix]-risk[,ref])
        xs[[i]]<-j+c(-.17,.17)[a]+runif(nrow(risk),-.12,.12)
      }
      for(zoom in c(FALSE,TRUE)) {
        plot(NA,xlim=c(.55,3.45),ylim=if(zoom) c(-5,5) else full,xaxt='n',xlab='',ylab='Risk change (percentage points)',yaxs='i')
        axis(1,1:3,names_model,tick=FALSE,cex.axis=.85);abline(h=0,col='#8C939B',lwd=1.4)
        for(j in 1:3) for(a in 1:2) {
          i<-(j-1)*2+a;v<-values[[i]];x<-j+c(-.17,.17)[a]
          points(xs[[i]],v,col=adjustcolor(co[j],alpha.f=if(a==1) .20 else .09),pch=if(a==1) 16 else 17,cex=.38)
          segments(x-.10,median(v),x+.10,median(v),col='#202833',lwd=2.5)
        }
        label(paste(if(kind=='history') 'Prior grip added' else 'Synthetic training',if(zoom) '| Zoom: -5 to +5 pp' else '| Full range'))

      }
    }
    par(pty='m',oma=c(0,0,0,0),fig=c(0,1,0,.07),new=TRUE,mar=rep(0,4));plot.new()
    legend('center',if(kind=='history') c('Original training','Original + synthetic') else c('Current grip','Current + prior grip'),pch=c(16,17),horiz=TRUE,bty='n',cex=.85,text.font=2,col=c('#303943','#899098'))
  }
  f3<-function() {
    setup(1,1);par(oma=c(4,0,0,0));set.seed(20)
    layout(matrix(c(1,3,5,2,4,6,7,9,11,8,10,12),nrow=4,byrow=TRUE),heights=c(3,1.3,3,1.3))
    for(target in c('current','prior')) for(j in 1:3) {
      par(mar=c(3,5.2,2,.8),mgp=c(3,.6,0),pty='s')
      plot(NA,xlim=c(0,45),ylim=c(0,45),xlab='',ylab=if(j==1) 'Observed risk (%)' else '',axes=FALSE,xaxs='i',yaxs='i')
      axis(1,seq(0,40,10));axis(2,seq(0,40,10));abline(0,1,col='#8C939B',lty=2,lwd=1.3)
      for(a in 1:2) {
        arm<-c('original','augmented')[a]
        z<-cal[cal$model==models[j]&cal$target==target&cal$arm==arm,];z<-z[order(z$group),]
        stopifnot(nrow(z)==5,all(is.finite(z$predicted)),all(is.finite(z$observed)),all(z$predicted<=.45),all(z$observed<=.45))
        colour<-adjustcolor(co[j],alpha.f=if(a==1) 1 else .5)
        lines(100*z$predicted,100*z$observed,col=colour,lwd=1.1,lty=a)
        points(100*z$predicted,100*z$observed,col=colour,pch=if(a==1) 16 else 17)
      }
      label(paste(names_model[j],if(target=='current') '| Current' else '| Current + prior'))
      mtext('Predicted risk (%) | 0-45% detail',1,line=1.8,font=2,cex=.65)
      par(mar=c(3.4,5.2,.6,.8),pty='m')
      plot(NA,xlim=c(0,100),ylim=c(.5,2.5),xlab='',ylab='',axes=FALSE,xaxs='i',yaxs='i')
      axis(1,seq(0,100,20));axis(2,2:1,c('Original','+ synthetic'),tick=FALSE,cex.axis=.6)
      for(a in 1:2) {
        ix<-which(combos$model==models[j]&combos$target==target&combos$arm==c('original','augmented')[a])
        points(100*risk[,ix],3-a+runif(nrow(risk),-.20,.20),col=adjustcolor(co[j],alpha.f=if(a==1) .16 else .07),pch=if(a==1) 16 else 17,cex=.38)
      }
      mtext('Individual predicted risk (%) | Full range',1,line=2.1,font=2,cex=.65)
    }
    par(pty='m',oma=c(0,0,0,0),fig=c(0,1,0,.045),new=TRUE,mar=rep(0,4));plot.new()
    legend('center',c('Original training','Original + synthetic'),pch=c(16,17),lty=c(1,2),horiz=TRUE,bty='n',text.font=2,col=c('#303943','#899098'),cex=.9)
  }
  figures<-list(P02a_prior_risk_changes=function() f2('history'),P02b_synthetic_risk_changes=function() f2('synthetic'),P03_calibration_with_points=f3)
  # Raster images only: no vector coordinates or participant identifiers embedded.
  for(n in names(figures)) {
    png(file.path(out,paste0(n,'.png')),width=3120,height=if(n=='P03_calibration_with_points') 2400 else 1320,res=240,type='cairo');figures[[n]]();dev.off()
  }
  writeLines(c('Individual points are real held-out predictions; repeated panels show the same people.',
    'P02a = prior minus current; P02b = augmented minus original. Left = full range, right = -5 to +5 percentage-point zoom of the same people. Points outside the zoom remain in the full panel. Bars are medians; positive values mean higher predicted risk, not improved accuracy. Within each model, circles/left = original training (P02a) or current grip (P02b); triangles/right = augmented training (P02a) or current plus prior grip (P02b). Jitter has no quantitative meaning.',
    'P03: calibration uses a 0-45% detail scale; all five calibration summaries must lie within that range. Separate strips below each panel use a different, full 0-100% x-scale and show individual risks. Circles/solid = original; triangles/dashed = augmented. Upper strip original, lower strip augmented. Strip vertical positions do not represent observed risks. No uncertainty bands are available for the five descriptive IPCW group points.',
    'Calibration summaries remain IPCW weighted; person clouds are unweighted descriptions of the analyzed cohort.',
    'No person-level table or model object saved. PNGs still contain individual predictions visually; keep local and review disclosure rules before sharing.'),file.path(out,'PERSON_FIGURE_LEGENDS.txt'))
}
