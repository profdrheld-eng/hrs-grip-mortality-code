# Separate exploratory extension. Private rows and fitted models stay in memory.
ml_source_existing <- function(directory) {
  .libPaths(c(file.path(dirname(normalizePath(directory)),'.local-r-library'),.libPaths()))
  for(name in c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R',
                'hrs_analysis.R','hrs_prediction_validation.R','hrs_grip_change.R','hrs_grip_history.R'))
    source(file.path(directory,name),local=.GlobalEnv)
}

ml_validate <- function(d,min_n=40L) {
  needed <- c('household','age','sex','smoke','health','grip14','grip10','gap_years','change_kg','lo','hi')
  stopifnot(all(needed %in% names(d)),nrow(d)>=min_n,!anyNA(d[needed]),
    all(is.finite(d$lo)),all(d$lo>0),all(d$hi>d$lo),all(d$lo<=5),
    all(d$hi[is.finite(d$hi)]<=5),all(d$age>=50 & d$age<=120),
    all(d$grip10>0 & d$grip10<=100),all(d$grip14>0 & d$grip14<=100),
    all(abs(d$change_kg-(d$grip14-d$grip10))<1e-10),all(d$gap_years>0))
  invisible(TRUE)
}

ml_config <- function(profile='full') {
  stopifnot(profile %in% c('full','smoke'))
  quick <- profile=='smoke'
  list(profile=profile,seed=20260929L,outer=if(quick) 2L else 5L,
    inner=if(quick) 2L else 3L,ci=if(quick) 20L else 500L,
    association_B=if(quick) 20L else 500L,simulation_R=if(quick) 2L else 100L,
    simulation_n=if(quick) 600L else 6300L,
    fractions=if(quick) 1 else c(.5,.75,1),ratios=if(quick) .5 else c(.5,1),
    models=c('weibull','neural','xgboost'))
}

ml_folds <- function(d,k,seed) {
  groups <- unique(as.character(d$household));stopifnot(length(groups)>=k*2)
  events <- vapply(groups,function(g) sum(is.finite(d$hi[d$household==g])),0)
  sizes <- vapply(groups,function(g) sum(d$household==g),0)
  set.seed(seed);order <- order(-events,runif(length(groups)))
  assignment <- integer(length(groups));totals <- counts <- numeric(k)
  for(i in order) {
    choices <- if(events[i]==0) which(counts==min(counts)) else which(totals==min(totals))
    choices <- choices[counts[choices]==min(counts[choices])]
    j <- choices[sample.int(length(choices),1)]
    assignment[i] <- j;totals[j] <- totals[j]+events[i];counts[j] <- counts[j]+sizes[i]
  }
  assignment[match(d$household,groups)]
}

ml_spline_term <- function(name,d) {
  basis <- splines::ns(d[[name]],df=3)
  numbers <- function(x) paste(format(x,digits=17,scientific=FALSE,trim=TRUE),collapse=',')
  paste0('splines::ns(',name,',knots=c(',numbers(attr(basis,'knots')),
         '),Boundary.knots=c(',numbers(attr(basis,'Boundary.knots')),'))')
}

ml_preprocess <- function(d,target) {
  vars <- c('age','sex','smoke','health','grip14',if(target=='prior') 'grip10')
  levels <- lapply(d[vars],function(x) if(is.factor(x)) levels(x) else NULL)
  f <- reformulate(vars);x <- model.matrix(f,d)[,-1,drop=FALSE]
  s <- apply(x,2,sd);s[!is.finite(s)|s<1e-8] <- 1
  list(formula=f,levels=levels,columns=colnames(x),center=colMeans(x),scale=s,
       spline_terms=vapply(c('age','grip14',if(target=='prior') 'grip10'),ml_spline_term,'',d=d))
}

ml_matrix <- function(p,d) {
  for(v in names(p$levels)) if(!is.null(p$levels[[v]]))
    d[[v]] <- factor(d[[v]],levels=p$levels[[v]])
  x <- model.matrix(p$formula,d)[,-1,drop=FALSE]
  stopifnot(identical(colnames(x),p$columns),nrow(x)==nrow(d),all(is.finite(x)))
  sweep(sweep(x,2,p$center,'-'),2,p$scale,'/')
}

# Weibull interval probability and derivatives with respect to mu and log(scale).
ml_weibull <- function(lo,hi,mu,scale) {
  zl <- (log(lo)-mu)/scale;a <- exp(zl)
  logp <- -a;dm <- a/scale;ds <- a*zl
  event <- is.finite(hi)
  if(any(event)) {
    dz <- log1p((hi[event]-lo[event])/lo[event])/scale
    delta <- a[event]*expm1(dz)
    zh <- zl[event]+dz;b <- a[event]+delta
    inv <- exp(-delta)/(-expm1(-delta))
    logp[event] <- -a[event]+log(-expm1(-delta))
    dm[event] <- (a[event]-delta*inv)/scale
    ds[event] <- a[event]*zl[event]-(delta*zl[event]+b*dz)*inv
  }
  if(any(!is.finite(c(logp,dm,ds)))) stop('Nonfinite interval likelihood.')
  list(logp=logp,dm=dm,ds=ds)
}

ml_nn_unpack <- function(theta,p,h) {
  k <- (p+1)*h
  list(w=matrix(theta[seq_len(k)],p+1,h),v=theta[k+seq_len(h)],
       bias=theta[k+h+1],scale=exp(theta[k+h+2]))
}

ml_nn_objective <- function(theta,x,lo,hi,h,lambda) {
  n <- nrow(x);z <- cbind(1,x);q <- ml_nn_unpack(theta,ncol(x),h)
  hidden <- tanh(z%*%q$w);mu <- as.numeric(hidden%*%q$v+q$bias)
  v <- ml_weibull(lo,hi,mu,q$scale)
  gmu <- -v$dm/n
  gw <- crossprod(z,(gmu%o%q$v)*(1-hidden^2))
  gv <- as.numeric(crossprod(hidden,gmu));gb <- sum(gmu);gs <- -mean(v$ds)
  penalized <- seq_len(length(theta)-2L)
  grad <- c(as.numeric(gw),gv,gb,gs)
  grad[penalized] <- grad[penalized]+2*lambda*theta[penalized]
  list(value=-mean(v$logp)+lambda*sum(theta[penalized]^2),gradient=grad)
}

ml_grid <- function(model,quick=FALSE) {
  if(model=='weibull') return(list(list()))
  if(model=='neural') {
    z <- expand.grid(hidden=if(quick) 3L else c(4L,8L),lambda=if(quick) .02 else c(.01,.1))
    return(lapply(seq_len(nrow(z)),function(i) list(hidden=z$hidden[i],lambda=z$lambda[i],maxit=600L)))
  }
  z <- expand.grid(depth=if(quick) 2L else c(1L,2L),rounds=if(quick) 15L else c(80L,200L),
                   scale=if(quick) 1 else c(.7,1.3))
  lapply(seq_len(nrow(z)),function(i) as.list(z[i,]))
}

ml_fit <- function(d,model,target,parameter,seed,preprocess=NULL) {
  stopifnot(model %in% c('weibull','neural','xgboost'),target %in% c('current','prior'))
  if(sum(is.finite(d$hi))<10) stop('Too few training events.')
  if(model=='weibull') {
    if(is.null(preprocess)) preprocess <- ml_preprocess(d,target)
    f <- reformulate(c(preprocess$spline_terms,'sex','smoke','health'),
                     response="survival::Surv(lo,hi,type='interval2')")
    fit <- withCallingHandlers(survival::survreg(f,d,dist='weibull',
      control=survival::survreg.control(maxiter=100)),warning=function(w) stop('Baseline fit warning.'))
    if(any(!is.finite(coef(fit)))||!is.finite(fit$scale)) stop('Invalid baseline fit.')
    return(list(model=model,fit=fit,scale=fit$scale))
  }
  if(is.null(preprocess)) preprocess <- ml_preprocess(d,target)
  x <- ml_matrix(preprocess,d);set.seed(seed)
  if(model=='xgboost') {
    if(!requireNamespace('xgboost',quietly=TRUE)) stop('xgboost is required.')
    mat <- xgboost::xgb.DMatrix(x)
    xgboost::setinfo(mat,'label_lower_bound',d$lo)
    xgboost::setinfo(mat,'label_upper_bound',d$hi)
    initial_mu <- optimize(function(mu) -mean(ml_weibull(d$lo,d$hi,rep(mu,nrow(d)),parameter$scale)$logp),
                           interval=c(-3,8))$minimum
    fit <- xgboost::xgb.train(params=list(objective='survival:aft',eval_metric='aft-nloglik',
      base_score=exp(initial_mu),
      aft_loss_distribution='extreme',aft_loss_distribution_scale=parameter$scale,
      max_depth=parameter$depth,eta=.05,lambda=1,min_child_weight=5,tree_method='hist',
      nthread=1,seed=seed),data=mat,nrounds=parameter$rounds,verbose=0)
    return(list(model=model,fit=fit,preprocess=preprocess,scale=parameter$scale))
  }
  h <- parameter$hidden;k <- (ncol(x)+1)*h+h
  initial <- c(rnorm(k,sd=.05),log(10),0)
  objective <- function(t) ml_nn_objective(t,x,d$lo,d$hi,h,parameter$lambda)
  fit <- optim(initial,function(t) objective(t)$value,function(t) objective(t)$gradient,
    method='L-BFGS-B',lower=c(rep(-3,k),-3,log(.2)),upper=c(rep(3,k),8,log(3)),
    control=list(maxit=parameter$maxit,factr=1e8))
  if(fit$convergence!=0 || !is.finite(fit$value)) stop('Neural optimization failed.')
  list(model=model,fit=fit$par,preprocess=preprocess,hidden=h,scale=exp(tail(fit$par,1)))
}

ml_predict <- function(fit,d) {
  mu <- switch(fit$model,
    weibull=as.numeric(predict(fit$fit,d,type='lp')),
    xgboost=as.numeric(predict(fit$fit,xgboost::xgb.DMatrix(ml_matrix(fit$preprocess,d)),outputmargin=TRUE)),
    neural={q<-ml_nn_unpack(fit$fit,length(fit$preprocess$columns),fit$hidden)
      as.numeric(tanh(cbind(1,ml_matrix(fit$preprocess,d))%*%q$w)%*%q$v+q$bias)})
  risk <- pweibull(5,shape=1/fit$scale,scale=exp(mu))
  stopifnot(length(risk)==nrow(d),all(is.finite(risk)))
  list(risk=risk,score=ml_weibull(d$lo,d$hi,mu,fit$scale)$logp)
}

# CART synthesis uses only the supplied training frame. Outcomes remain joint pairs.
ml_augment <- function(d,ratio,seed,target='prior') {
  if(ratio==0) return(d)
  set.seed(seed);m <- ceiling(nrow(d)*ratio);n <- nrow(d)
  vars <- c('age','sex','smoke','health','grip14',if(target!='current') 'grip10',
            if(target=='association') 'gap_years')
  syn <- d[sample.int(n,m,replace=TRUE),,drop=FALSE]
  bounds <- list(age=c(50,120),grip14=c(.1,100),grip10=c(.1,100),gap_years=c(.1,30))
  control <- rpart::rpart.control(minbucket=max(10L,floor(n/40)),cp=.005,maxdepth=4,xval=0)
  for(j in seq_along(vars)) {
    name <- vars[j];v <- d[[name]]
    if(j==1L || length(unique(v))==1L) {syn[[name]]<-sample(v,m,replace=TRUE);next}
    tree <- rpart::rpart(reformulate(vars[seq_len(j-1)],response=name),d,
                         method=if(is.factor(v)) 'class' else 'anova',control=control)
    if(is.factor(v)) {
      probs <- predict(tree,syn,type='prob')
      draw <- vapply(seq_len(m),function(i) sample(colnames(probs),1,prob=probs[i,]),'')
      syn[[name]] <- factor(draw,levels=levels(v))
    } else {
      fitted <- as.numeric(predict(tree,d));pred <- as.numeric(predict(tree,syn))
      draw <- vapply(pred,function(value) {
        pool <- which(abs(fitted-value)<1e-8)
        if(!length(pool)) stop('Generator leaf mismatch.')
        v[pool[sample.int(length(pool),1)]]+rnorm(1,sd=if(length(pool)>1) .05*sd(v[pool]) else 0)
      },0)
      syn[[name]] <- pmin(bounds[[name]][2],pmax(bounds[[name]][1],draw))
    }
  }
  td <- d;td$observed_lower <- log1p(td$lo)
  tree <- rpart::rpart(reformulate(vars,'observed_lower'),td,method='anova',control=control)
  fitted <- as.numeric(predict(tree,td));pred <- as.numeric(predict(tree,syn))
  donors <- vapply(pred,function(value) {
    pool<-which(abs(fitted-value)<1e-8);if(!length(pool)) stop('Generator outcome leaf mismatch.')
    pool[sample.int(length(pool),1)]
  },1L)
  syn$lo <- d$lo[donors];syn$hi <- d$hi[donors]
  syn$household <- paste0('synthetic_',seq_len(m))
  if(target=='current') syn$grip10 <- syn$grip14 # Never use earlier grip in this generator.
  syn$change_kg <- syn$grip14-syn$grip10
  ml_validate(syn,min_n=1L)
  rbind(d,syn)
}

ml_weights <- function(train,test,position='midpoint') {
  time <- function(d) {
    event <- is.finite(d$hi);t<-d$lo
    t[event]<-switch(position,lower=d$lo[event],upper=d$hi[event],midpoint=(d$lo[event]+d$hi[event])/2)
    t
  }
  tt <- time(train);event <- is.finite(test$hi);eval <- ifelse(event,time(test),5)
  censor <- !is.finite(train$hi);g <- rep(1,nrow(test))
  for(ct in sort(unique(tt[censor & tt<5])))
    g[eval>ct] <- g[eval>ct]*(1-sum(censor & tt==ct)/sum(tt>=ct))
  known <- event | test$lo>=5
  if(any(g[known]<.05)) stop('IPCW positivity insufficient.')
  data.frame(y=as.numeric(event),weight=ifelse(known,1/g,0))
}

ml_metrics <- function(d,risk,weights) {
  y <- weights$y;w <- weights$weight;use <- w>0
  lp <- qlogis(pmin(1-1e-8,pmax(1e-8,risk)))
  cal <- function(slope=FALSE) tryCatch({
    z<-data.frame(y=y[use],w=w[use],lp=lp[use])
    fit<-suppressWarnings(glm(if(slope) y~lp else y~1+offset(lp),data=z,
                              weights=w,family=quasibinomial()))
    if(!fit$converged) return(NA_real_)
    unname(coef(fit)[if(slope) 2 else 1])
  },error=function(e) NA_real_)
  c(auc=weighted_auc(risk,y,w),brier=sum(w*(y-risk)^2)/nrow(d),
    calibration_intercept=cal(),calibration_slope=cal(TRUE))
}

ml_tune <- function(d,model,target,arm,cfg,fold,seed) {
  grid <- ml_grid(model,cfg$profile=='smoke')
  ratios <- if(arm=='original') 0 else cfg$ratios
  best <- NULL;best_score <- -Inf;failed <- 0L;attempts <- 0L
  for(ratio in ratios) for(p in grid) {
    scores <- rep(NA_real_,max(fold))
    for(k in seq_len(max(fold))) {
      attempts<-attempts+1L
      scores[k] <- tryCatch({
        tr<-d[fold!=k,,drop=FALSE];te<-d[fold==k,,drop=FALSE]
        prep<-ml_preprocess(tr,target)
        aug<-ml_augment(tr,ratio,seed+k,target)
        fit<-ml_fit(aug,model,target,p,seed+k,prep)
        mean(ml_predict(fit,te)$score)
      },error=function(e) {failed<<-failed+1L;NA_real_})
    }
    if(all(is.finite(scores)) && mean(scores)>best_score) {
      best_score<-mean(scores);best<-list(parameter=p,ratio=ratio)
    }
  }
  if(is.null(best)) stop('No valid complete inner-CV candidate.')
  best$failures<-failed;best$attempts<-attempts;best$score<-best_score
  best
}

ml_write <- function(x,out,name) {
  if(!is.null(out)) write.csv(x,file.path(out,name),row.names=FALSE,na='NA')
}

ml_diagnostics <- function(real,aug,fold) {
  syn<-aug[-seq_len(nrow(real)),,drop=FALSE]
  rows<-lapply(c('age','grip10','grip14','gap_years'),function(v)
    data.frame(fold=fold,measure=v,standardized_mean_difference=
      if(sd(real[[v]])>0) (mean(syn[[v]])-mean(real[[v]]))/sd(real[[v]]) else 0))
  rbind(do.call(rbind,rows),data.frame(fold=fold,measure=c('event_fraction','early_censored_fraction'),
    standardized_mean_difference=c(mean(is.finite(syn$hi))-mean(is.finite(real$hi)),
      mean(!is.finite(syn$hi)&syn$lo<5)-mean(!is.finite(real$hi)&real$lo<5))))
}

ml_benchmark <- function(d,cfg,out=NULL) {
  ml_validate(d);fold<-ml_folds(d,cfg$outer,cfg$seed)
  combos<-expand.grid(model=cfg$models,target=c('current','prior'),arm=c('original','augmented'),stringsAsFactors=FALSE)
  metrics<-paired<-calibration<-tuning<-diagnostics<-bootstrap_differences<-list()
  for(fraction in cfg$fractions) {
    risk<-score<-matrix(NA_real_,nrow(d),nrow(combos))
    weights<-lapply(c('lower','midpoint','upper'),function(x) data.frame(y=rep(NA_real_,nrow(d)),weight=NA_real_))
    names(weights)<-c('lower','midpoint','upper')
    for(k in seq_len(cfg$outer)) {
      message('Benchmark: fraction ',fraction,', outer fold ',k,'/',cfg$outer)
      train<-d[fold!=k,,drop=FALSE];test<-d[fold==k,,drop=FALSE]
      set.seed(cfg$seed+k);groups<-sample(unique(train$household))
      groups<-head(groups,max(cfg$inner*2,ceiling(length(groups)*fraction)))
      train<-train[train$household %in% groups,,drop=FALSE]
      inner<-ml_folds(train,cfg$inner,cfg$seed+100+k)
      for(position in names(weights)) weights[[position]][fold==k,]<-ml_weights(train,test,position)
      for(j in seq_len(nrow(combos))) {
        c<-combos[j,];seed<-cfg$seed+1000*k
        selected<-ml_tune(train,c$model,c$target,c$arm,cfg,inner,seed)
        prep<-ml_preprocess(train,c$target);aug<-ml_augment(train,selected$ratio,seed,c$target)
        fit<-ml_fit(aug,c$model,c$target,selected$parameter,seed,prep)
        pred<-ml_predict(fit,test)
        risk[fold==k,j]<-pred$risk;score[fold==k,j]<-pred$score
        tuning[[length(tuning)+1]]<-data.frame(fraction=fraction,fold=k,c,
          ratio=selected$ratio,training_n_rounded=10*round(nrow(train)/10),
          training_events_rounded=10*round(sum(is.finite(train$hi))/10),inner_log_score=selected$score,
          failed_fits=selected$failures,attempted_fits=selected$attempts,
          parameter=paste(names(selected$parameter),unlist(selected$parameter),sep='=',collapse=';'))
        if(c$model==cfg$models[1] && c$target=='prior' && c$arm=='augmented')
          diagnostics[[length(diagnostics)+1]]<-data.frame(fraction=fraction,ml_diagnostics(train,aug,k))
      }
    }
    stopifnot(all(is.finite(risk)),all(is.finite(score)))
    fast <- function(index,j,w) c(auc=weighted_auc(risk[index,j],w$y[index],w$weight[index]),
      brier=sum(w$weight[index]*(w$y[index]-risk[index,j])^2)/length(index),
      interval_log_score=mean(score[index,j]))
    mainw<-weights$midpoint;index<-seq_len(nrow(d))
    boot<-array(NA_real_,c(cfg$ci,3,nrow(combos)))
    if(cfg$ci>0) {
      clusters<-split(index,d$household);set.seed(cfg$seed+987)
      for(b in seq_len(cfg$ci)) {
        ix<-unlist(clusters[sample.int(length(clusters),length(clusters),replace=TRUE)],use.names=FALSE)
        for(j in seq_len(nrow(combos))) boot[b,,j]<-fast(ix,j,mainw)
      }
    }
    limits <- function(v) {
      good<-is.finite(v)
      if(length(v)<20 || sum(good)<ceiling(.9*length(v))) return(c(NA_real_,NA_real_))
      quantile(v[good],c(.025,.975),names=FALSE)
    }
    for(j in seq_len(nrow(combos))) {
      m<-ml_metrics(d,risk[,j],mainw);point<-fast(index,j,mainw)
      ci<-vapply(1:3,function(t) limits(boot[,t,j]),numeric(2))
      metrics[[length(metrics)+1]]<-data.frame(fraction=fraction,combos[j,],
        auc=m['auc'],auc_lower=ci[1,1],auc_upper=ci[2,1],
        brier=m['brier'],brier_lower=ci[1,2],brier_upper=ci[2,2],
        interval_log_score=point[3],log_score_lower=ci[1,3],log_score_upper=ci[2,3],
        calibration_intercept=m[3],calibration_slope=m[4],
        auc_lower_position=fast(index,j,weights$lower)[1],auc_upper_position=fast(index,j,weights$upper)[1],
        brier_lower_position=fast(index,j,weights$lower)[2],brier_upper_position=fast(index,j,weights$upper)[2],
        uncertainty='conditional_OOF_household_bootstrap',
        status=if(any(!is.finite(m))) 'UNSTABLE_CALIBRATION' else if(cfg$ci==0) 'POINT_ESTIMATES_ONLY' else
          if(any(!is.finite(ci))) 'UNSTABLE_INTERVALS' else 'OK')
      # Equal-count display bins; weights estimated without outer-test outcomes.
      group<-pmin(5L,ceiling(rank(risk[,j],ties.method='first')/nrow(d)*5))
      for(g in 1:5) {
        ix<-which(group==g);usable<-sum(mainw$weight[ix]>0)>=10
        calibration[[length(calibration)+1]]<-data.frame(fraction=fraction,combos[j,],group=g,
          n_rounded=round(length(ix)/10)*10,
          predicted=if(usable) weighted.mean(risk[ix,j],mainw$weight[ix]) else NA_real_,
          observed=if(usable) weighted.mean(mainw$y[ix],mainw$weight[ix]) else NA_real_,
          status=if(usable) 'OK' else 'SUPPRESSED')
      }
    }
    add_pair <- function(j,i,label) {
      delta<-fast(index,j,mainw)-fast(index,i,mainw)
      for(t in 1:3) {
        if(isTRUE(cfg$export_bootstrap) && cfg$ci>0 && t<=2)
          bootstrap_differences[[length(bootstrap_differences)+1]]<<-data.frame(
            fraction=fraction,contrast=label,model=combos$model[j],target=combos$target[j],
            arm=combos$arm[j],reference_model=combos$model[i],metric=names(delta)[t],
            replicate=seq_len(cfg$ci),difference=boot[,t,j]-boot[,t,i])
        ci<-limits(boot[,t,j]-boot[,t,i])
        paired[[length(paired)+1]]<<-data.frame(fraction=fraction,contrast=label,
          model=combos$model[j],target=combos$target[j],arm=combos$arm[j],
          reference_model=combos$model[i],metric=names(delta)[t],difference=delta[t],
          ci_lower=ci[1],ci_upper=ci[2],uncertainty='conditional_OOF_household_bootstrap')
      }
    }
    for(j in seq_len(nrow(combos))) {
      c<-combos[j,]
      if(c$target=='prior') add_pair(j,which(combos$model==c$model & combos$target=='current' & combos$arm==c$arm),'prior_minus_current')
      if(c$arm=='augmented') add_pair(j,which(combos$model==c$model & combos$target==c$target & combos$arm=='original'),'augmented_minus_original')
      if(c$model!='weibull' && 'weibull' %in% cfg$models)
        add_pair(j,which(combos$model=='weibull' & combos$target==c$target & combos$arm==c$arm),'model_minus_weibull')
    }
  }
  result<-list(metrics=do.call(rbind,metrics),paired=do.call(rbind,paired),
    calibration=do.call(rbind,calibration),tuning=do.call(rbind,tuning),diagnostics=do.call(rbind,diagnostics))
  if(length(bootstrap_differences)) result$bootstrap_differences<-do.call(rbind,bootstrap_differences)
  for(n in names(result)) ml_write(result[[n]],out,paste0('benchmark_',n,'.csv'))
  result
}

ml_association_fit <- function(aug,real,kind) {
  vars <- c('age',if(kind=='change') c('grip10','change_kg') else c('grip14','grip10'))
  terms <- c(vapply(vars,ml_spline_term,'',d=real),'sex','smoke','health')
  if(kind=='change' && length(unique(real$gap_years))>1) terms<-c(terms,'gap_years')
  f<-reformulate(terms,response="survival::Surv(lo,hi,type='interval2')")
  fit<-withCallingHandlers(survival::survreg(f,aug,dist='weibull',
    control=survival::survreg.control(maxiter=100)),warning=function(w) stop('Association fit warning.'))
  if(any(!is.finite(coef(fit)))||!is.finite(fit$scale)) stop('Invalid association fit.')
  fit
}

ml_associations <- function(d,cfg,out=NULL) {
  grid<-change_grid(d);rows<-list()
  for(kind in c('change','history')) {
    curvefun<-if(kind=='change') change_curve else history_curve
    point<-lapply(c(0,1),function(ratio) curvefun(ml_association_fit(ml_augment(d,ratio,cfg$seed,if(kind=='change') 'association' else 'prior'),d,kind),d,grid))
    draws<-array(NA_real_,c(cfg$association_B,length(grid),2,2))
    set.seed(cfg$seed+321)
    # Store seeds first so generator RNG cannot alter household bootstrap draws.
    seeds<-sample.int(1e8,cfg$association_B)
    for(b in seq_len(cfg$association_B)) {
      if(b==1 || b%%25==0) message('Association ',kind,': ',b,'/',cfg$association_B)
      set.seed(seeds[b]);boot<-sample_households(d)
      for(a in 1:2) {
        attempt<-tryCatch(curvefun(ml_association_fit(ml_augment(boot,a-1,seeds[b]+1,if(kind=='change') 'association' else 'prior'),boot,kind),boot,grid),error=function(e) NULL)
        if(!is.null(attempt)) draws[b,,a,]<-as.matrix(attempt[,c('risk5','risk_difference')])
      }
    }
    for(a in 1:3) {
      values<-if(a<=2) as.matrix(point[[a]][,c('risk5','risk_difference')]) else
        as.matrix(point[[2]][,c('risk5','risk_difference')]-point[[1]][,c('risk5','risk_difference')])
      samples<-if(a<=2) draws[,,a,,drop=FALSE] else draws[,,2,,drop=FALSE]-draws[,,1,,drop=FALSE]
      dim(samples)<-c(cfg$association_B,length(grid),2)
      good<-apply(samples,1,function(x) all(is.finite(x)))
      for(g in seq_along(grid)) {
        ci<-if(sum(good)>=ceiling(.9*cfg$association_B))
          apply(matrix(samples[good,g,],ncol=2),2,quantile,c(.025,.975),names=FALSE) else matrix(NA_real_,2,2)
        rows[[length(rows)+1]]<-data.frame(kind=kind,arm=c('original','augmented','augmented_minus_original')[a],
          change_kg=grid[g],risk5=values[g,1],risk_difference=values[g,2],
          risk_lower=ci[1,1],risk_upper=ci[2,1],rd_lower=ci[1,2],rd_upper=ci[2,2],
          successful=sum(good),requested=cfg$association_B,
          status=if(sum(good)>=ceiling(.9*cfg$association_B)) 'OK' else 'UNSTABLE')
      }
    }
  }
  result<-do.call(rbind,rows);ml_write(result,out,'association_curves.csv');result
}

ml_simulate <- function(n,scenario,seed) {
  stopifnot(scenario %in% c('null','small','interaction'),n>=100)
  set.seed(seed);household<-as.character(rep(seq_len(ceiling(n/2)),each=2)[seq_len(n)])
  shared<-rnorm(ceiling(n/2))[as.integer(household)]
  age<-pmin(90,pmax(50,66+8*rnorm(n)+2*shared));sex<-factor(sample(1:2,n,TRUE),levels=1:2)
  smoke<-factor(sample(c('never','former','current'),n,TRUE,c(.45,.4,.15)),levels=c('never','former','current'))
  health<-factor(sample(1:5,n,TRUE,c(.15,.3,.3,.2,.05)),levels=1:5)
  grip14<-pmin(70,pmax(5,36-8*(sex=='2')-.3*(age-65)+6*rnorm(n)))
  residual<-rnorm(n);grip10<-pmin(80,pmax(2,grip14+2+5*residual))
  history<-(grip10-grip14-2)/5
  extra<-switch(scenario,null=rep(0,n),small=.12*history,interaction=.28*history*(age-66)/8)
  mu<-log(12)-.035*(age-65)+.018*(grip14-30)-.2*(smoke=='current')-.12*(as.numeric(health)-3)+extra
  death<-rweibull(n,shape=1.4,scale=exp(mu))
  censor<-ifelse(runif(n)<.2,runif(n,.5,5),5)
  event<-death<=censor & death<5
  lo<-pmin(censor,5);hi<-rep(Inf,n)
  lo[event]<-pmax(1e-5,floor(death[event]*12)/12)
  hi[event]<-(floor(death[event]*12)+1)/12
  hi[hi>5]<-Inf
  d<-data.frame(household,age,sex,smoke,health,grip14,grip10,
    gap_years=rep(4,n),change_kg=grip14-grip10,lo,hi)
  ml_validate(d);d
}

ml_simulation <- function(cfg,out=NULL) {
  rows<-status<-list();simcfg<-cfg;simcfg$fractions<-1;simcfg$ci<-0L
  for(s in c('null','small','interaction')) for(b in seq_len(cfg$simulation_R)) {
    message('Simulation ',s,': ',b,'/',cfg$simulation_R)
    simcfg$seed<-cfg$seed+10000*match(s,c('null','small','interaction'))+b
    result<-tryCatch(ml_benchmark(ml_simulate(cfg$simulation_n,s,simcfg$seed),simcfg),error=function(e) NULL)
    status[[length(status)+1]]<-data.frame(scenario=s,repetition=b,status=if(is.null(result)) 'FAILED' else 'OK')
    if(!is.null(result)) rows[[length(rows)+1]]<-data.frame(scenario=s,repetition=b,result$paired)
  }
  states<-do.call(rbind,status);ml_write(states,out,'simulation_status.csv')
  if(!length(rows)) stop('All simulations failed.')
  raw<-do.call(rbind,rows);ml_write(raw,out,'simulation_repetitions.csv')
  keys<-c('scenario','contrast','model','target','arm','reference_model','metric')
  groups<-split(seq_len(nrow(raw)),interaction(raw[keys],drop=TRUE))
  summary<-do.call(rbind,lapply(groups,function(ix) {
    z<-raw[ix,,drop=FALSE];v<-z$difference[is.finite(z$difference)]
    data.frame(z[1,keys],mean_difference=mean(v),mcse=if(length(v)>1) sd(v)/sqrt(length(v)) else NA_real_,
      empirical_q025=if(length(v)) quantile(v,.025,names=FALSE) else NA_real_,
      empirical_q975=if(length(v)) quantile(v,.975,names=FALSE) else NA_real_,
      successful=length(v),requested=cfg$simulation_R,
      status=if(length(v)>=ceiling(.9*cfg$simulation_R)) 'OK' else 'UNSTABLE')
  }))
  ml_write(summary,out,'simulation_summary.csv');summary
}

ml_plot_benchmark <- function(result,out,synthetic=FALSE) {
  pdf(file.path(out,'benchmark_curves.pdf'),width=8,height=5,family='Helvetica',useDingbats=FALSE)
  on.exit(dev.off())
  colours<-c(weibull='#626773',neural='#7556B8',xgboost='#087F8C')
  style<-function() par(mfrow=c(1,2),font=2,font.axis=2,font.lab=2,mar=c(4.5,4.7,3,1),
                        bty='l',las=1,cex=.9,mgp=c(2.7,.6,0))
  combinations<-expand.grid(model=names(colours),arm=c('original','augmented'),stringsAsFactors=FALSE)
  legend_labels<-paste(combinations$model,ifelse(combinations$arm=='original','original','augmented'))
  for(target in c('current','prior')) {
    style()
    for(metric in c('auc','brier')) {
      z<-result$metrics[result$metrics$target==target,,drop=FALSE]
      range_y<-range(c(z[[paste0(metric,'_lower')]],z[[paste0(metric,'_upper')]],z[[metric]]),finite=TRUE)
      pad<-max(diff(range_y)*.1,.005);range_y<-range_y+c(-pad,pad)
      plot(NA,NA,xlim=c(min(z$fraction)*100-3,103),ylim=range_y,axes=FALSE,
        xlab='Outer training households (%)',ylab=if(metric=='auc') 'Five-year AUC' else 'Five-year Brier score')
      axis(1,at=sort(unique(z$fraction))*100,font=2)
      ticks<-pretty(range_y,5);axis(2,at=ticks,labels=format(ticks,scientific=FALSE,trim=TRUE),font=2)
      for(j in seq_len(nrow(combinations))) {
        c<-combinations[j,];v<-z[z$model==c$model & z$arm==c$arm,,drop=FALSE]
        v<-v[order(v$fraction),];x<-v$fraction*100+(if(c$arm=='original') -.5 else .5)
        lines(x,v[[metric]],col=colours[c$model],lwd=2,lty=if(c$arm=='original') 1 else 2)
        segments(x,v[[paste0(metric,'_lower')]],x,v[[paste0(metric,'_upper')]],col=adjustcolor(colours[c$model],.5))
        points(x,v[[metric]],pch=if(c$arm=='original') 16 else 17,col=colours[c$model],cex=.8)
      }
      mtext(if(target=='current') 'Current grip' else 'Current + earlier grip',side=3,line=.7,adj=0,font=2)
      if(metric=='auc') legend('bottomright',legend_labels,col=colours[combinations$model],
        lty=ifelse(combinations$arm=='original',1,2),cex=.57,bty='n',text.font=2)
    }
    if(synthetic) mtext('SYNTHETIC TEST DATA',side=3,outer=TRUE,line=-1,font=2,cex=.7)
  }
  style()
  for(target in c('current','prior')) {
    z<-result$calibration[result$calibration$target==target & result$calibration$fraction==max(result$calibration$fraction),]
    plot(NA,NA,xlim=c(0,100),ylim=c(0,100),xlab='Predicted five-year risk (%)',ylab='Observed five-year risk (%)')
    abline(0,1,lty=2,col='#626773')
    for(j in seq_len(nrow(combinations))) {
      c<-combinations[j,];v<-z[z$model==c$model & z$arm==c$arm & z$status=='OK',]
      lines(100*v$predicted,100*v$observed,col=colours[c$model],lwd=2,lty=if(c$arm=='original') 1 else 2)
      points(100*v$predicted,100*v$observed,col=colours[c$model],pch=if(c$arm=='original') 16 else 17)
    }
    mtext(if(target=='current') 'Current grip' else 'Current + earlier grip',side=3,line=.7,adj=0,font=2)
    if(target=='current') legend('topleft',legend_labels,col=colours[combinations$model],
      lty=ifelse(combinations$arm=='original',1,2),cex=.57,bty='n',text.font=2)
  }
  if(synthetic) mtext('SYNTHETIC TEST DATA',side=3,outer=TRUE,line=-1,font=2,cex=.7)
}

ml_preflight <- function(d,out) {
  ml_validate(d)
  counts<-c(analyzed=nrow(d),households=length(unique(d$household)),
    interval_events=sum(is.finite(d$hi)),early_censored=sum(!is.finite(d$hi)&d$lo<5),
    known_survivors_at_5=sum(!is.finite(d$hi)&d$lo>=5))
  ml_write(diagnostic_report(counts),out,'preflight_counts.csv')
  if(counts['interval_events']<100 || counts['known_survivors_at_5']<100)
    stop('Insufficient event/control support for the planned benchmark.')
  invisible(counts)
}

ml_require <- function() {
  needed<-c('survival','rpart','xgboost')
  missing<-needed[!vapply(needed,requireNamespace,TRUE,quietly=TRUE)]
  if(length(missing)) stop(paste('Missing packages:',paste(missing,collapse=', ')))
}

ml_main <- function(args,directory) {
  modes<-c('preflight','benchmark','inference','associations','simulation','all','smoke')
  if(length(args)!=3L || !args[1] %in% modes)
    stop('Usage: Rscript hrs_ml_extension.R MODE PRIVATE_DATA_DIR OUTPUT_PARENT')
  ml_source_existing(directory);ml_require()
  mode<-args[1];cfg<-ml_config(if(mode=='smoke') 'smoke' else 'full')
  if(mode=='inference') {cfg$fractions<-1;cfg$ci<-5000L;cfg$export_bootstrap<-TRUE}
  prefix<-if(mode=='smoke') 'hrs_ml_SYNTHETIC_' else paste0('hrs_ml_',mode,'_')
  base<-file.path(args[3],paste0(prefix,format(Sys.time(),'%Y%m%d_%H%M%S')))
  out<-base;k<-1L
  while(file.exists(out)) {out<-paste0(base,'_',k);k<-k+1L}
  if(!dir.create(out,recursive=TRUE)) stop('Cannot create output directory.')
  status<-file.path(out,'STATUS.txt');writeLines('INCOMPLETE: run not completed.',status)
  phase<-'configuration'
  tryCatch({
    dput(cfg,file=file.path(out,'CONFIG.R'))
    scripts<-file.path(directory,c('hrs_ml_extension.R','hrs_feasibility.R','hrs_stage2.R',
      'hrs_source_hierarchy.R','hrs_analysis.R','hrs_prediction_validation.R','hrs_grip_change.R','hrs_grip_history.R'))
    hash<-tools::md5sum(scripts)
    ml_write(data.frame(file=basename(names(hash)),md5=unname(hash)),out,'source_hashes.csv')
    versions<-data.frame(component=c('R','survival','rpart','xgboost'),
      version=c(as.character(getRversion()),vapply(c('survival','rpart','xgboost'),function(p) as.character(packageVersion(p)),'')))
    ml_write(versions,out,'versions.csv')
    writeLines(c('Exploratory ML extension, planned after earlier results were known.',
      if(mode=='smoke') 'SYNTHETIC TEST DATA ONLY. NOT A SCIENTIFIC RESULT.' else 'Local HRS execution. No individual records or fitted model objects exported.',
      'Nested household cross-validation. All synthesis and preprocessing fitted only on training.',
      'All families use Weibull AFT survival likelihood with interval or right censoring.',
      'AFT XGBoost uses extreme noise and outputmargin for log-time; neural model is a custom tanh AFT.',
      'Original and augmented arms tuned separately. Real validation rows only; synthetic rows do not increase independent N.',
      'Generator is sequential CART and joint interval-pair donor sampling, not a validated latent event-time generator.',
      'OOF CIs are conditional household resampling of existing held-out predictions; no full-pipeline refit in those CIs.',
      'IPCW nuisance estimates use outer training data. Lower/midpoint/upper interval positions are scoring sensitivities only.',
      'Pooled held-out predictions come from different fitted folds. No external validation or deployment claim.',
      'Association CIs instead refit both generator and model in each real-household bootstrap.',
      'Association augmentation is a generator-dependent sensitivity analysis, not additional observed evidence.',
      'Simulation summary quantiles are empirical repetition distributions, not confidence intervals for mean differences.',
      'Simulation MCSE reflects independent repetitions. Smoke repetitions cannot establish power or type-I error.',
      'No causal or equivalence claim. Selected complete-case cohort, not survey-population inference.',
      'Review failures, calibration, positivity, learning curves and Monte Carlo precision before manuscript use.'),
      file.path(out,'READ_ME.txt'))
    if(mode!='simulation') {
      phase<-'local import'
      if(mode=='smoke') d<-ml_simulate(600,'small',cfg$seed) else {
        imported<-stage2_main(c(args[2],file.path(out,'import_checks')),file.path(directory,'hrs_feasibility.R'))
        d<-prepare_change_data(imported)$data
      }
      phase<-'preflight';ml_preflight(d,out)
    }
    stable<-TRUE
    if(mode %in% c('benchmark','inference','all','smoke')) {
      phase<-'nested prediction benchmark';result<-ml_benchmark(d,cfg,out)
      ml_plot_benchmark(result,out,mode=='smoke')
      stable<-stable && all(result$metrics$status=='OK')
    }
    if(mode %in% c('associations','all','smoke')) {
      phase<-'association augmentation';curves<-ml_associations(d,cfg,out)
      stable<-stable && all(curves$status=='OK')
    }
    if(mode %in% c('simulation','all','smoke')) {
      phase<-'controlled simulation';sim<-ml_simulation(cfg,out)
      stable<-stable && all(sim$status=='OK')
    }
    writeLines(if(stable) paste('SUCCESS:',mode,'completed; read limitations in READ_ME.txt.') else
      'PARTIAL: unstable results; inspect status fields before use.',status)
    message('Aggregate outputs: ',out)
    invisible(out)
  },error=function(e) {
    writeLines(paste('INCOMPLETE: stopped during',phase,'; no private error values exported.'),status)
    stop('ML run failed; inspect newest STATUS.txt. Detailed person-level errors suppressed.',call.=FALSE)
  })
}

if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  directory<-dirname(normalizePath(script))
  tryCatch(ml_main(commandArgs(trailingOnly=TRUE),directory),error=function(e) {
    message('ML extension stopped. Check dependencies, invocation and latest STATUS.txt.');quit(status=1L)
  })
}
