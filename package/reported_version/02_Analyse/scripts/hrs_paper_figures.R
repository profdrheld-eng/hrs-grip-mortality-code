# User-run publication graphics. Person-level values remain in memory and local
# figures only; no individual tables, model objects or prediction files are saved.
paper_numbers <- function(x) {
  vapply(x,function(value) {
    if(!is.finite(value)) stop('Nonfinite axis value.')
    if(abs(value)<1e-14) return('0')
    format(value,scientific=FALSE,trim=TRUE,digits=10,big.mark=',',decimal.mark='.')
  },'')
}

paper_style <- function() {
  par(family='Helvetica',font=2,font.axis=2,font.lab=2,font.main=2,font.sub=2,
      bg='white',fg='black',col.axis='black',col.lab='black',col.main='black',
      cex=.9,cex.axis=.85,cex.lab=.93,mgp=c(2.15,.55,0),tcl=-.22,
      mar=c(4,4.7,1.5,1),oma=c(0,0,0,0),las=1,bty='l',xaxs='i',yaxs='i')
}

paper_ticks <- function(limits,n=5) {
  ticks <- pretty(limits,n=n)
  ticks <- ticks[ticks>=limits[1]&ticks<=limits[2]]
  if(length(ticks)<3L) {
    ticks <- pretty(limits,n=2*n)
    ticks <- ticks[ticks>=limits[1]&ticks<=limits[2]]
  }
  ticks
}

paper_axis <- function(side,limits,n=5) {
  ticks <- paper_ticks(limits,n)
  axis(side,at=ticks,labels=paper_numbers(ticks),font=2)
}

paper_plot <- function(xlim,ylim,xlab,ylab,letter=NULL,...) {
  plot(NA_real_,NA_real_,type='n',xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,
       axes=FALSE,...)
  paper_axis(1,xlim);paper_axis(2,ylim);box(bty='l',col='#555555')
  if(!is.null(letter)) mtext(letter,side=3,adj=0,line=.25,font=2,cex=1)
}

paper_range <- function(x,zero=FALSE,symmetric=FALSE) {
  if(any(!is.finite(x))) stop('Nonfinite plotting coordinate.')
  if(symmetric) {
    extent <- max(abs(x));if(extent==0) extent <- .01
    return(c(-1,1)*extent*1.08)
  }
  r <- range(c(x,if(zero) 0))
  if(diff(r)==0) r <- r+c(-1,1)*max(abs(r[1])*.05,.01)
  pad <- diff(r)*.06
  if(zero && all(x>=0)) c(0,r[2]+pad) else r+c(-pad,pad)
}

paper_point_coordinates <- function(d,current,prior) {
  stopifnot(nrow(d)==length(current),length(current)==length(prior),nrow(d)>0,
            all(is.finite(current)),all(is.finite(prior)),
            all(current>=0 & current<=1),all(prior>=0 & prior<=1),
            all(is.finite(d$grip10)),all(is.finite(d$grip14)),
            all(d$grip10>0 & d$grip10<=100),all(d$grip14>0 & d$grip14<=100))
  grip_range <- c(0,ceiling(max(d$grip10,d$grip14)/10)*10)
  risk_range <- c(0,min(100,ceiling(max(current,prior)*100/5)*5))
  if(risk_range[2]==0) risk_range[2] <- 1
  delta <- 100*(prior-current)
  list(grip=list(x=d$grip10,y=d$grip14,xlim=grip_range,ylim=grip_range),
       agreement=list(x=100*current,y=100*prior,xlim=risk_range,ylim=risk_range),
       difference=list(x=50*(current+prior),y=delta,xlim=risk_range,
                       ylim=paper_range(delta,symmetric=TRUE)))
}

paper_ba_stats <- function(difference) {
  stopifnot(length(difference)>=2L,all(is.finite(difference)))
  bias <- mean(difference);spread <- sd(difference)
  c(bias=bias,lower_loa=bias-1.96*spread,upper_loa=bias+1.96*spread)
}

paper_bland_altman <- function(d,pair) {
  delta <- 100*(risk5(pair$prior,d)-risk5(pair$current,d))
  estimate <- paper_ba_stats(delta)
  data.frame(statistic=names(estimate),estimate=unname(estimate),
    observed_fraction_inside_limits=mean(delta>=estimate['lower_loa'] & delta<=estimate['upper_loa']))
}

paper_exact_flow <- function(counts) {
  stages <- c('valid_grip_pairs','tracker_matched','age_and_overlap_eligible',
              'ordered_interview_months','followup_eligible','analyzed')
  included <- unname(counts[stages])
  stopifnot(length(included)==6L,all(is.finite(included)),all(included>=0),
            all(included==floor(included)),all(diff(included)<=0))
  list(included=included,excluded=-diff(included))
}

paper_flow_layout <- function(flow) {
  labels <- c('Valid grip pairs','Tracker matched','Age 50-120, no overlap',
              'Ordered examination dates','Eligible follow-up','Analysis cohort')
  reasons <- c('No tracker match','Age / overlap criteria','Missing / unordered dates',
               'Unusable / excluded\nfollow-up','Incomplete model variables')
  date_note <- flow$excluded[3]==0
  keep <- if(date_note) c(1L,2L,4L,5L,6L) else seq_len(6L)
  if(date_note) labels[4] <- labels[3]
  list(included=flow$included[keep],excluded=-diff(flow$included[keep]),
       labels=labels[keep],reasons=reasons[if(date_note) c(1,2,4,5) else 1:5],
       date_note=date_note)
}

paper_run_directory <- function(parent) {
  base <- file.path(parent,paste0('paper_figures_v4_',format(Sys.time(),'%Y%m%d_%H%M%S')))
  out <- base;k <- 1L
  while(file.exists(out)) {out <- paste0(base,'_',k);k <- k+1L}
  if(!dir.create(out,recursive=TRUE)) stop('Cannot create a new output directory.')
  out
}

paper_read_reports <- function(parent) {
  spec <- c(flow='diagnostics_v1/sample_flow.csv',support='diagnostics_v1/joint_support.csv',
    calibration='diagnostics_v1/calibration_groups.csv',
    metrics='validation_v2/validated_metrics.csv',
    comparison='validation_v2/model_comparison.csv',history='grip_history_v1/risk_curve.csv',
    change='grip_change_v1/risk_curve.csv')
  for(folder in unique(dirname(spec))) {
    status <- file.path(parent,folder,'STATUS.txt')
    if(!file.exists(status) || !startsWith(readLines(status,warn=FALSE)[1],'SUCCESS:'))
      stop('A required aggregate report is missing or incomplete.')
  }
  files <- file.path(parent,spec)
  if(!all(file.exists(files))) stop('Missing aggregate input.')
  reports <- lapply(files,read.csv,stringsAsFactors=FALSE)
  names(reports) <- names(spec)
  if(any(reports$metrics$status!='OK')) stop('Unstable validation results.')
  for(key in c('history','change')) {
    d <- reports[[key]]
    stopifnot(all(d$status=='OK'),all(is.finite(d$risk5)),
      all(d$risk_ci_lower<=d$risk_ci_upper),all(d$rd_ci_lower<=d$rd_ci_upper),
      !anyDuplicated(d$change_kg),all(diff(d$change_kg)>0))
  }
  stopifnot(identical(reports$history$change_kg,reports$change$change_kg))
  attr(reports,'files') <- files
  reports
}

paper_check_refit <- function(d,pair,reports) {
  old <- reports$comparison[reports$comparison$scenario=='primary',]
  if(nrow(old)!=1L || old$status!='FIT_OK') stop('Primary model reference unavailable.')
  scores <- vapply(pair,interval_score,0,d=d)
  expected <- c(old$current_score,old$prior_score)
  if(any(!is.finite(scores)) || max(abs(scores-expected))>1e-7)
    stop('Refitted models differ from frozen reports.')
  # Check apparent risks against the frozen metrics, never against corrected values.
  metrics <- pair_metrics(pair,d,'midpoint')
  for(model in c('current','prior')) for(metric in rownames(metrics)) {
    row <- reports$metrics[reports$metrics$target==model & reports$metrics$metric==metric,]
    if(nrow(row)!=1L || !is.finite(row$apparent) ||
       abs(metrics[metric,model]-row$apparent)>1e-7)
      stop('Refitted predictions differ from frozen reports.')
  }
  shown <- diagnostic_report(c(analyzed=nrow(d)))$count_rounded_10
  expected_n <- reports$flow$count_rounded_10[reports$flow$metric=='analyzed']
  if(length(expected_n)!=1L || shown!=expected_n) stop('Cohort reference mismatch.')
  invisible(TRUE)
}

paper_pages <- function(d,current,prior,r,agreement,counts) {
  blue <- '#087F8C';purple <- '#7956A5';gray <- '#626773'
  coords <- paper_point_coordinates(d,current,prior)
  exact_flow <- paper_exact_flow(counts)
  stopifnot(identical(as.character(agreement$statistic),c('bias','lower_loa','upper_loa')),
            all(is.finite(agreement$estimate)))
  letter <- function(x) mtext(x,side=3,adj=0,line=.25,font=2,cex=1)
  split2 <- function() {par(mfrow=c(1,2));par(mar=c(4,4.8,1.5,.8),pty='s')}
  list(
    F01_grip_scatter=function() {
      # Shared strength limits; every participant is drawn once, without jitter.
      z <- coords$grip
      # Explicit physical geometry keeps the scatter square and the marginal
      # histograms aligned exactly. asp=1 alone would expand one axis silently.
      size <- par('din');side <- min(size[1]*.46,size[2]*.69)
      xb <- c(.14,.14+side/size[1]);yb <- c(.14,.14+side/size[2])
      par(fig=c(xb,yb),mar=rep(0,4),pty='m')
      paper_plot(z$xlim,z$ylim,'','')
      abline(0,1,col=gray,lty=2,lwd=1.1)
      points(z$x,z$y,pch=16,cex=.5,col=adjustcolor(blue,.19))
      breaks <- seq(z$xlim[1],z$xlim[2],length.out=21)
      hx <- hist(z$x,breaks=breaks,plot=FALSE,include.lowest=TRUE)
      hy <- hist(z$y,breaks=breaks,plot=FALSE,include.lowest=TRUE)
      par(fig=c(xb,yb[2]+.04,.985),mar=rep(0,4),new=TRUE)
      plot(NA,NA,xlim=z$xlim,ylim=c(0,max(hx$counts)*1.08),axes=FALSE,xlab='',ylab='')
      rect(head(breaks,-1),0,tail(breaks,-1),hx$counts,col=adjustcolor(blue,.65),border='white')
      paper_axis(2,c(0,max(hx$counts)),3);mtext('n',side=2,line=2.8,font=2,cex=.8)
      par(fig=c(xb[2]+.04,xb[2]+.22,yb),mar=rep(0,4),new=TRUE)
      plot(NA,NA,xlim=c(0,max(hy$counts)*1.08),ylim=z$ylim,axes=FALSE,xlab='',ylab='')
      rect(0,head(breaks,-1),hy$counts,tail(breaks,-1),col=adjustcolor(blue,.65),border='white')
      paper_axis(1,c(0,max(hy$counts)),3);mtext('n',side=1,line=2.4,font=2,cex=.8)
      par(fig=c(0,1,0,1),mar=rep(0,4),new=TRUE)
      plot.new();plot.window(xlim=c(0,1),ylim=c(0,1),xaxs='i',yaxs='i')
      text(mean(xb),.035,'Grip strength 2010 (kg)',font=2,cex=.9)
      text(.035,mean(yb),'Grip strength 2014 (kg)',font=2,cex=.9,srt=90)
    },
    F02_prediction_scatter=function() {
      split2();z <- coords$agreement
      paper_plot(z$xlim,z$ylim,'Current-grip model: risk (%)','Two-measurement model: risk (%)','A',asp=1)
      abline(0,1,col=gray,lty=2,lwd=1.1)
      points(z$x,z$y,pch=16,cex=.48,col=adjustcolor(blue,.18))
      z <- coords$difference
      z$ylim <- paper_range(c(z$y,agreement$estimate[2:3]),symmetric=TRUE)
      paper_plot(z$xlim,z$ylim,'Mean predicted risk (%)','Risk difference (percentage points)','B')
      points(z$x,z$y,pch=16,cex=.48,col=adjustcolor(purple,.18))
      abline(h=agreement$estimate[2:3],col=gray,lty=2,lwd=1.4)
      legend('topright','95% limits of agreement',lty=2,col=gray,
        text.font=2,text.col='black',bty='n',cex=.6)
    },
    F04_risk_contrasts=function() {
      split2();ylim <- paper_range(100*c(r$change$rd_ci_lower,r$change$rd_ci_upper,r$history$rd_ci_lower,r$history$rd_ci_upper),zero=TRUE)
      for(i in 1:2) {
        z <- list(r$change,r$history)[[i]];col <- '#52616B'
        paper_plot(range(z$change_kg),ylim,'Grip change 2014 - 2010 (kg)','Risk difference (percentage points)',c('A','B')[i])
        abline(h=0,col=gray,lty=2);abline(v=0,col=gray,lty=3)
        polygon(c(z$change_kg,rev(z$change_kg)),100*c(z$rd_ci_lower,rev(z$rd_ci_upper)),col=adjustcolor(col,.16),border=NA)
        lines(z$change_kg,100*z$risk_difference,lwd=2,col=col)
        mtext(c('Earlier grip fixed','Current grip fixed')[i],side=3,adj=1,line=.25,font=2,cex=.74)
        # Model estimates are not represented as participant dots.
      }
    },
    S01_cohort_flow=function() {
      par(mfrow=c(1,1),mar=rep(0,4),pty='m',lend='butt')
      plot.new();plot.window(xlim=c(0,1),ylim=c(0,1))
      flow <- paper_flow_layout(exact_flow)
      ys <- seq(.91,.10,length.out=length(flow$included))
      half <- if(length(ys)==5L) .067 else .054
      ink <- '#35434B'
      # Filled arrowheads and 2-point shafts at the native export size.
      down <- function(x,top,bottom) {
        segments(x,top,x,bottom+.015,col=ink,lwd=2.7)
        polygon(c(x-.009,x+.009,x),c(bottom+.018,bottom+.018,bottom),col=ink,border=NA)
      }
      right <- function(left,y,right) {
        segments(left,y,right-.011,y,col=ink,lwd=2.7)
        polygon(c(right-.014,right-.014,right),c(y-.012,y+.012,y),col=ink,border=NA)
      }
      for(i in seq_along(ys)) {
        y <- ys[i];last <- i==length(ys)
        note <- flow$date_note && i==3L
        rect(.07,y-half,.49,y+half,border=if(i==1L || last) blue else ink,
             col=if(last) blue else 'white',lwd=if(i==1L || last) 2.7 else 1.8)
        text(.28,y+if(note) .037 else .023,flow$labels[i],font=2,cex=.84,
             col=if(last) 'white' else ink)
        text(.28,y+if(note) -.005 else -.023,paste0('n = ',paper_numbers(flow$included[i])),
             font=2,cex=1.25,col=if(last) 'white' else ink)
        if(note) text(.28,y-.044,'Date check: 0 excluded',font=2,cex=.67,col=ink)
        if(!last) {
          middle <- (y+ys[i+1])/2
          down(.28,y-half,ys[i+1]+half)
          right(.28,middle,.62)
          rect(.62,middle-.055,.96,middle+.055,border=ink,col='white',lwd=1.8)
          text(.79,middle+.022,paste0(paper_numbers(flow$excluded[i]),' excluded'),
               font=2,cex=.97,col=ink)
          text(.79,middle-.022,flow$reasons[i],font=2,cex=.71,col=ink)
        }
      }
    },
    S02_calibration=function() {
      split2();cols <- c(blue,purple)
      all <- r$calibration[r$calibration$model %in% c('weibull_current','weibull_prior') & r$calibration$status=='OK',]
      if(!nrow(all)) stop('No usable calibration groups.')
      mx <- max(as.numeric(all$n_rounded))
      limits <- c(0,min(100,ceiling(max(all$predicted_risk,all$observed_risk)*100/10)*10))
      for(i in 1:2) {
        z <- all[all$model==c('weibull_current','weibull_prior')[i],]
        paper_plot(limits,limits,'Mean predicted risk (%)','Weighted observed risk (%)',c('A','B')[i],asp=1)
        abline(0,1,col=gray,lty=2)
        points(100*z$predicted_risk,100*z$observed_risk,pch=21,cex=3.5*sqrt(as.numeric(z$n_rounded)/mx),bg=adjustcolor(cols[i],.35),col=cols[i])
      }
    },
    S03_data_support=function() {
      par(mfrow=c(1,1))
      mx <- max(suppressWarnings(as.numeric(r$support$count_rounded_10)),na.rm=TRUE)
      stopifnot(is.finite(mx),mx>0)
      colours <- colorRampPalette(c('#F4FAFA','#C7E4E5','#78B9C0'))(101)
      for(i in 1:2) {
        par(fig=c((i-1)*.5,i*.5,.17,1),mar=c(4,5.5,2,.8),new=i>1L,pty='s')
        z <- r$support[r$support$strength_wave==c('grip10','grip14')[i],]
        xx <- match(z$strength_band,c('[0,20)','[20,30)','[30,40)','[40,101)'))
        yy <- match(z$change_band,c('[-Inf,-5)','[-5,0)','[0,5)','[5, Inf)'))
        nn <- suppressWarnings(as.numeric(z$count_rounded_10))
        stopifnot(!anyNA(xx),!anyNA(yy))
        plot(NA,NA,xlim=c(.5,4.5),ylim=c(.5,4.5),axes=FALSE,
          xlab=c('Grip strength 2010 (kg)','Grip strength 2014 (kg)')[i],ylab='')
        mtext('Grip change (kg)',side=2,line=3.8,font=2,cex=.84,las=0)
        fill <- rep('#EAEAEA',nrow(z));known <- !is.na(nn)
        fill[known] <- colours[1+floor(100*nn[known]/mx)]
        rect(xx-.5,yy-.5,xx+.5,yy+.5,col=fill,border='white',lwd=2)
        text(xx,yy,ifelse(is.na(nn),'<10',z$count_rounded_10),font=2,cex=.83)
        axis(1,1:4,c('<20','20 to <30','30 to <40','40+'),font=2,cex.axis=.64,tick=FALSE,gap.axis=0)
        axis(2,1:4,c('< -5','-5 to <0','0 to <5','5+'),font=2,cex.axis=.78,las=1,tick=FALSE)
        letter(c('A','B')[i])
      }
      par(fig=c(0,1,0,1),mar=rep(0,4),new=TRUE,pty='m')
      plot.new();plot.window(xlim=c(0,1),ylim=c(0,1),xaxs='i',yaxs='i')
      edges <- seq(.32,.78,length.out=102)
      rect(head(edges,-1),.08,tail(edges,-1),.105,col=colours,border=NA)
      at <- paper_ticks(c(0,mx),4)
      text(.32+.46*at/mx,.055,paper_numbers(at),font=2,cex=.7,xpd=NA)
      text(.29,.0925,'n',font=2,cex=.85,xpd=NA)
    })
}

paper_export <- function(pages,out,synthetic=FALSE) {
  draw <- function(fun) {
    paper_style();fun()
    if(synthetic) mtext('SYNTHETIC TEST DATA',side=3,outer=TRUE,line=-1,font=2,cex=.75)
  }
  device <- function(path,type,fun) {
    if(type=='png') png(path,width=7.5,height=5,units='in',res=600,bg='white')
    else pdf(path,width=7.5,height=5,family='Helvetica',useDingbats=FALSE,bg='white',version='1.4')
    on.exit(dev.off());draw(fun)
  }
  for(name in names(pages)) {
    device(file.path(out,paste0(name,'.png')),'png',pages[[name]])
    device(file.path(out,paste0(name,'.pdf')),'pdf',pages[[name]])
  }
  pdf(file.path(out,'ALL_FIGURES.pdf'),width=7.5,height=5,family='Helvetica',useDingbats=FALSE,bg='white',version='1.4')
  on.exit(dev.off())
  for(fun in pages) draw(fun)
}

paper_main <- function(args,script_dir) {
  if(length(args)!=2L) stop('Usage: Rscript hrs_paper_figures.R PRIVATE_DATA_DIR RESULTS_DIR')
  if(!requireNamespace('survival',quietly=TRUE)) stop('Existing survival package required.')
  out <- paper_run_directory(args[2]);status <- file.path(out,'STATUS.txt')
  writeLines('INCOMPLETE: do not use figures until SUCCESS.',status)
  stage <- 'checking frozen aggregate reports'
  tryCatch({
    reports <- paper_read_reports(args[2])
    stage <- 'importing private data locally'
    imported <- stage2_main(c(args[1],file.path(out,'import_checks')),
                           file.path(script_dir,'hrs_feasibility.R'))
    prepared <- prepare_change_data(imported);d <- prepared$data
    if(nrow(d)<100 || sum(is.finite(d$hi))<50) stop('Insufficient cohort for original models.')
    stage <- 'refitting and checking the original models'
    message('Refitting the original models and checking frozen results.')
    pair <- fit_pair(d)
    paper_check_refit(d,pair,reports)
    current <- risk5(pair$current,d);prior <- risk5(pair$prior,d)
    stage <- 'calculating descriptive agreement limits'
    agreement <- paper_bland_altman(d,pair)
    write.csv(agreement,file.path(out,'agreement_summary.csv'),row.names=FALSE)
    stage <- 'rendering publication figures'
    pages <- paper_pages(d,current,prior,reports,agreement,prepared$counts)
    paper_export(pages,out)
    write.csv(diagnostic_report(prepared$counts),file.path(out,'sample_flow.csv'),row.names=FALSE)
    files <- attr(reports,'files')
    code <- file.path(script_dir,c('hrs_paper_figures.R','hrs_feasibility.R','hrs_stage2.R',
      'hrs_source_hierarchy.R','hrs_analysis.R','hrs_prediction_validation.R','hrs_grip_change.R'))
    hashes <- tools::md5sum(c(files,code))
    write.csv(data.frame(file=names(hashes),md5=unname(hashes)),file.path(out,'source_hashes.csv'),row.names=FALSE)
    writeLines(c('Publication graphic exports. No titles, captions or background grid lines in the figures.',
      'All figure text is bold; ordinary decimal labels; white background; teal/purple palette.',
      'PNG: 600 dpi, 7.5 x 5 inches. PDF: vector graphics with transparent points.',
      'Local point-cloud files contain individual measurements/predictions visually; keep private until disclosure/release review.',
      'No individual CSV, identifiers, model objects or prediction-pair files were exported.',
      'All records are plotted without jitter or sampling; overlapping points can hide multiplicity.',
      'F01: earlier/current grip scatter with 20 equal-width marginal histogram bins; diagonal means equal grip.',
      'F02 A: current-only vs two-measurement model risk. B: difference (two minus current) vs mean of the two risks.',
      'F02 B: only normal-theory 95% limits of agreement (mean +/- 1.96 SD), shown as grey dashed lines. No confidence bands or bias line.',
      'These are global descriptive limits: non-normality and risk-dependent spread mean that 95% coverage is not guaranteed, especially conditionally.',
      'Observed in-sample coverage is in agreement_summary.csv. Agreement does not establish accuracy, external validity or interchangeability.',
      'Method: Bland and Altman (1999), https://pubmed.ncbi.nlm.nih.gov/10501650/. CI = confidence interval; limits of agreement are not CIs.',
      'F02 uses APPARENT fitted predictions from the original models, not held-out, external or optimism-corrected individual predictions.',
      'F04 A: earlier grip held fixed; B: current grip held fixed. Pointwise 95% bands, separate exploratory models and target subsets.',
      'F04: no participant points are superimposed on model-standardized risks; at zero, difference and interval are zero by construction.',
      'S01: exact sequential counts computed locally; exclusions are exact consecutive-stage differences, never differences of rounded reports.',
      'S01: date screening is combined with the preceding stage only when its exclusion count is zero, explicitly noted in the box; otherwise all six stages remain.',
      'S01 contains exact small counts for local manuscript review; the separately exported sample_flow.csv remains rounded/suppressed.',
      'S02 A/B: current/two-measurement Weibull model; apparent Hajek IPCW calibration, group points, no confidence intervals.',
      'S03: heatmap of aggregated support bins. A: earlier strength; B: current strength. Common colour scale, rounded counts, suppressed cells grey.',
      'Limits: selected unweighted cohort, interval/time approximations, censoring and model assumptions, no causal inference.',
      'Original apparent metrics and interval log-scores were reproduced within 0.0000001 before export.',
      'Paper styling is implemented; final journal dimensions, disclosure eligibility and substantive figure selection still require review.',
      paste('R:',getRversion()),paste('survival:',utils::packageVersion('survival'))),file.path(out,'READ_ME.txt'))
    writeLines('SUCCESS: six figure sets exported; original models reproduced; inspect READ_ME.txt.',status)
    message('Figures saved to: ',out)
    invisible(out)
  },error=function(e) {
    writeLines(paste('INCOMPLETE: stopped while',stage,'. No detailed person-level condition exported.'),status)
    stop('Paper figure run stopped. Inspect STATUS.txt in the newest paper_figures directory.',call.=FALSE)
  })
}

if(sys.nframe()==0L) {
  script <- sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  directory <- dirname(normalizePath(script))
  for(name in c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R',
               'hrs_analysis.R','hrs_prediction_validation.R','hrs_grip_change.R'))
    source(file.path(directory,name))
  tryCatch(paper_main(commandArgs(trailingOnly=TRUE),directory),error=function(e) {
    message('Figure run stopped. Check STATUS.txt in the newest output folder; detailed conditions are not exported.')
    quit(status=1L)
  })
}
