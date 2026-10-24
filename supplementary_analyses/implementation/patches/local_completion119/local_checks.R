# HUMAN-RUN ONLY for HRS data. Codex tests this file with invented records only.
# Two fixed primary Weibull fits; no bootstrap, tuning, model selection or manuscript writes.
protected_count_block <- function(x) {
  stopifnot(is.numeric(x),!anyNA(x),all(is.finite(x)),all(x>=0),all(x==floor(x)))
  if(any(x<10)) rep('SUPPRESSED',length(x)) else as.character(10*floor(x/10+.5))
}
matrix_diagnostics <- function(x) {
  stopifnot(is.matrix(x),all(is.finite(x)),nrow(x)>ncol(x))
  scales <- sqrt(colSums(x*x));scaled <- sweep(x,2,ifelse(scales>0,scales,1),'/')
  rank <- qr(scaled,tol=1e-10)$rank;full <- rank==ncol(x)
  condition <- if(full) kappa(scaled,exact=TRUE) else NA_real_
  data.frame(columns=ncol(x),rank=rank,rank_status=if(full)'FULL_RANK'else'RANK_DEFICIENT',
    condition=signif(condition,4),condition_status=if(!full)'NOT_INTERPRETABLE'else
      if(!is.finite(condition)||condition>1e8)'REVIEW'else'COMPUTED',stringsAsFactors=FALSE)
}
influence_summary <- function(dfb,household,event) {
  stopifnot(nrow(dfb)==length(household),length(event)==length(household))
  # Sum signed approximations within household before taking absolute magnitudes.
  h <- rowsum(dfb,household);case_flag <- apply(abs(dfb),1,max)>.5
  house_flag <- apply(abs(h),1,max)>.5
  counts <- c(sum(case_flag),sum(!case_flag),sum(house_flag),sum(!house_flag))
  shown <- if(nrow(dfb)<200||nrow(h)<200||sum(event)<10||sum(!event)<10)
    rep('SUPPRESSED',4) else protected_count_block(counts)
  data.frame(metric=c('case_over_half_SE','case_other','household_over_half_SE','household_other'),
    value=shown,stringsAsFactors=FALSE)
}
weibull_scores <- function(lo,hi,eta,sigma) {
  # Analytic first derivatives of interval/right-censored Weibull probabilities.
  # No dependence on residuals.survreg's interval log-scale derivative.
  stopifnot(length(sigma)==1,is.finite(sigma),sigma>0,length(lo)==length(hi),
    length(lo)==length(eta),all(is.finite(lo)),all(lo>0),all(is.finite(eta)),
    all(!is.na(hi)),all(hi>lo))
  z<-(log(lo)-eta)/sigma;h<-exp(z);event<-is.finite(hi)
  location<-h/sigma;scale<-z*h
  if(any(event)) {
    zu<-(log(hi[event])-eta[event])/sigma;hu<-exp(zu)
    delta<-hu-h[event];den<--expm1(-delta);q<-exp(-delta)
    stopifnot(all(is.finite(delta)),all(delta>0),all(den>0))
    location[event]<-(h[event]-hu*q)/(sigma*den)
    scale[event]<-(z[event]*h[event]-zu*hu*q)/den
  }
  stopifnot(all(is.finite(location)),all(is.finite(scale)))
  cbind(location=location,log_scale=scale)
}
model_diagnostics <- function(fit,d,name) {
  x <- model.matrix(stats::delete.response(fit$terms),data=d,contrasts.arg=fit$contrasts)
  stopifnot(identical(colnames(x),names(coef(fit))),
    max(abs(drop(x%*%coef(fit))-fit$linear.predictors))<1e-8)
  fit$x <- x;fit$y <- survival::Surv(d$lo,d$hi,type='interval2')
  stopifnot(identical(fit$dist,'weibull'),ncol(fit$var)==ncol(x)+1,
    is.null(fit$weights)||all(fit$weights==1))
  first <- weibull_scores(d$lo,d$hi,fit$linear.predictors,fit$scale)
  location <- first[,1];scale_score <- first[,2]
  full_score <- cbind(x*location,scale_score)
  dfb <- sweep(full_score%*%fit$var,2,sqrt(diag(fit$var)),'/')
  stopifnot(all(is.finite(dfb)))
  h <- rowsum(dfb,d$household)
  # Only first derivatives are populated. No unverified second derivatives retained.
  score <- cbind(NA_real_,location,NA_real_,scale_score,NA_real_,NA_real_)
  standardized <- function(v) {s<-sd(v);if(!is.finite(s)||s<=0)stop('Degenerate score.');v/s}
  scores <- list(location_score=standardized(location),scale_score=standardized(scale_score))
  quantities <- c(scores,list(case_dfbeta=apply(abs(dfb),1,max),household_dfbeta=apply(abs(h),1,max)))
  enough <- nrow(d)>=200 && nrow(h)>=200 && sum(is.finite(d$hi))>=10 && sum(!is.finite(d$hi))>=10
  qs <- do.call(rbind,lapply(names(quantities),function(k) data.frame(diagnostic=k,
    quantile=c('q50','q90','q95'),value=if(enough)signif(as.numeric(quantile(abs(quantities[[k]]),c(.5,.9,.95))),3)else NA_real_)))
  groups <- list(age=d$age,current_grip=d$grip14,prior_grip=d$grip10,linear_predictor=fit$linear.predictors)
  grouped <- list()
  for(variable in names(groups)) {
    v <- groups[[variable]];cuts <- unique(quantile(v,c(0,1/3,2/3,1),names=FALSE))
    if(length(cuts)<3) next
    bin <- cut(v,cuts,include.lowest=TRUE,labels=FALSE)
    ids <- sort(unique(bin));valid <- vapply(ids,function(g) {
      take<-bin==g;sum(take)>=200&&length(unique(d$household[take]))>=50&&
        sum(is.finite(d$hi[take]))>=10&&sum(!is.finite(d$hi[take]))>=10
    },TRUE)
    for(g in ids) for(k in names(scores)) grouped[[length(grouped)+1]] <- data.frame(
      variable=variable,band=paste0('ordered_band_',g),diagnostic=k,
      n_rounded_10=if(all(valid))as.character(10*floor(sum(bin==g)/10+.5))else'SUPPRESSED',
      standardized_mean=if(all(valid))as.character(signif(mean(scores[[k]][bin==g]),3))else'SUPPRESSED')
  }
  eigenvalues <- eigen(fit$var,symmetric=TRUE,only.values=TRUE)$values
  matrix <- matrix_diagnostics(x);matrix$variance_status <- if(all(is.finite(eigenvalues))&&min(eigenvalues)>0)'POSITIVE_DEFINITE'else'REVIEW'
  list(matrix=matrix,quantiles=qs,grouped=do.call(rbind,grouped),
    influence=influence_summary(dfb,d$household,is.finite(d$hi)),
    private=list(score=score,dfbetas=dfb,household_dfbetas=h,fit=fit))
}
release_diagnostics <- function(results) {
  types <- c('matrix','quantiles','grouped','influence')
  setNames(lapply(types,function(k)do.call(rbind,lapply(names(results),function(n)
    data.frame(model=n,results[[n]][[k]],row.names=NULL,stringsAsFactors=FALSE)))),types)
}
cohort_adapter <- function(prepare) {
  # Preserve the reviewed cohort algorithm, changing only its final return value.
  b <- body(prepare);last <- length(b)
  stopifnot(identical(b[[last]],quote(list(sex = sex, included = included))))
  b[[last]] <- quote(list(sex=sex,included=included,data=d,keys=analytical_keys))
  body(prepare) <- b;prepare
}
version_status <- function(paths,env,stem,field,expected) {
  one <- function(ext) paths[tolower(basename(paths))==paste0(stem,ext)]
  da<-one('.da');sas<-one('.sas')
  if(length(da)!=1||length(sas)!=1) return('NOT_AVAILABLE')
  tryCatch({v<-env$read_hrs(da,sas,c('HHID','PN',field))[[field]]
    if(!anyNA(v)&&length(v)>0&&all(v==expected))'MATCH'else'NOT_CONFIRMED'},
    error=function(e)'NOT_AVAILABLE')
}
local_main <- function(args,here) {
  if(length(args)!=3)stop('Expected private data directory, private results parent and implementation directory.')
  args<-vapply(args,normalizePath,'',mustWork=TRUE)
  stopifnot(all(dir.exists(args)),startsWith(args[2],paste0(dirname(args[1]),'/')),args[2]!=args[1],
    !startsWith(args[2],paste0(args[1],'/')))
  if(!requireNamespace('survival',quietly=TRUE))stop('survival missing; no installation attempted.')
  impl<-args[3];scripts<-file.path(impl,'analysis-work/02_Analyse/scripts')
  auxiliary<-c('../revision/a1_sex_20261021/export_a1_sex.R'='a66429be18ac4e81e7e61dec5fce6c7f',
    '../revision/scripts/hrs_review_analyses.R'='98bebbca32c991f7d10208c6003b37fb',
    'verification/check_existing_primary_parameters.R'='a2cdea29cdb90302e7dbfe0c5f85ed3f')
  stopifnot(identical(unname(tools::md5sum(file.path(impl,names(auxiliary)))),unname(auxiliary)))
  env<-new.env(parent=globalenv());sys.source(file.path(impl,names(auxiliary)[1]),envir=env)
  expected<-env$a1_verify_sources(scripts)
  for(n in names(expected))sys.source(file.path(scripts,n),envir=env)
  sys.source(file.path(impl,names(auxiliary)[2]),envir=env)
  sys.source(file.path(impl,names(auxiliary)[3]),envir=env)
  out<-file.path(args[2],paste0('completion_checks_corrected119_',format(Sys.time(),'%Y%m%d_%H%M%S'),'_',Sys.getpid()))
  if(!dir.create(out))stop('Fresh output directory could not be created.')
  private<-file.path(out,'LOCAL_ONLY');share<-file.path(out,'SEND_BACK')
  stopifnot(dir.create(private),dir.create(share));writeLines('INCOMPLETE; do not share.',file.path(share,'STATUS.txt'))
  cat('Lokaler Pruefordner: ',out,'\nImport und zwei feste Primaerfits; keine Bootstraps/ML-Suche.\n',sep='')
  log<-file(file.path(private,'checks.log'),'wt');sink(log);sink(log,type='message')
  ok<-tryCatch({
    imported<-env$stage2_main(c(args[1],private),file.path(scripts,'hrs_feasibility.R'))
    original<-env$a1_prepare(imported);adapt<-cohort_adapter(env$a1_prepare);prepared<-adapt(imported)
    stopifnot(identical(original$included,prepared$included),identical(original$sex,prepared$sex),
      nrow(prepared$data)==6307L,!anyDuplicated(prepared$keys))
    d<-prepared$data
    paths<-list.files(args[1],recursive=TRUE,full.names=TRUE)
    t<-imported$tracker;a<-env$wave_summary(imported$core$h10i_r,'M');b<-env$wave_summary(imported$core$h14i_r,'O')
    keys<-intersect(a$key[a$valid_trials>0],b$key[b$valid_trials>0]);z<-t[match(intersect(keys,t$key),t$key),]
    flagged<-sum(!is.na(z$OAGE)&z$OAGE>=50&z$OAGE<=120&z$OAGE==floor(z$OAGE)&z$OVHHID!='000000',na.rm=TRUE)
    statuses<-data.frame(check=c('cohort_reference_N6307','tracker_VERSION1','core2010_MVERSION6',
      'core2014_OVERSION2','pre_overlap_flag_count_reported10'),status=c('MATCH',
      if(!anyNA(t$VERSION)&&all(t$VERSION==1))'MATCH'else'NOT_CONFIRMED',
      version_status(paths,env,'h10pr_r','MVERSION',6),version_status(paths,env,'h14pr_r','OVERSION',2),
      if(flagged==10)'MATCH'else'NOT_CONFIRMED'))
    # Fingerprints of private inputs stay private; they do not prove historical download identity.
    input_paths<-paths[tolower(basename(paths))%in%c('trk2022tr_r.csv',paste0(c('h10i_r','h14i_r','h14a_r','h14c_r','h14pr_r','h10pr_r'),'.da'),paste0(c('h10i_r','h14i_r','h14a_r','h14c_r','h14pr_r','h10pr_r'),'.sas'))]
    write.csv(data.frame(file=input_paths,md5=unname(tools::md5sum(input_paths))),file.path(private,'current_input_manifest.csv'),row.names=FALSE)
    formulas<-env$review_formulas()
    matrices<-do.call(rbind,lapply(names(formulas),function(n)data.frame(model=n,
      matrix_diagnostics(model.matrix(formulas[[n]],data=d)),row.names=NULL)))
    pair<-env$fit_pair(d);stopifnot(identical(names(pair),c('current','prior')))
    reference<-env$expected_primary_parameters();comparisons<-list();diagnostics<-list()
    for(n in names(pair)) {
      fit<-pair[[n]];ref<-reference[reference$model==n,]
      beta<-ref[ref$component=='coefficient',];sc<-ref$value[ref$component=='scale']
      matches<-identical(names(coef(fit)),beta$term)&&all(abs(unname(coef(fit))-beta$value)<=1e-8)&&abs(fit$scale-sc)<=1e-8
      comparisons[[n]]<-data.frame(check=paste0(n,'_fresh_fit_vs_published_parameters'),status=if(matches)'MATCH'else'NOT_CONFIRMED')
      diagnostics[[n]]<-model_diagnostics(fit,d,n)
    }
    released<-release_diagnostics(diagnostics)
    saveRDS(list(models=lapply(diagnostics,function(x)x$private),household=d$household),file.path(private,'diagnostics_private.rds'))
    # A private diagnostic PDF is generated directly by R; no Word or printer is used.
    grDevices::pdf(file.path(private,'diagnostics_private.pdf'),width=10,height=8)
    for(n in names(diagnostics)) {
      par(mfrow=c(2,2));rr<-diagnostics[[n]]$private$score
      for(k in c(2,4))for(v in c('age','grip14'))plot(d[[v]],rr[,k],pch=20,cex=.3,
        main=paste('PRIVATE',n,if(k==2)'location score'else'scale score'),xlab=v,ylab='Likelihood score')
    }
    for(n in names(diagnostics)) {
      par(mfrow=c(1,2));p<-diagnostics[[n]]$private
      plot(apply(abs(p$dfbetas),1,max),pch=20,cex=.3,main=paste('PRIVATE',n,'case influence'),
        xlab='Local row index',ylab='Largest absolute approximate DFBETA, SE units');abline(h=.5,col='red')
      plot(apply(abs(p$household_dfbetas),1,max),pch=20,cex=.3,main=paste('PRIVATE',n,'household influence'),
        xlab='Local household index',ylab='Largest absolute approximate DFBETA, SE units');abline(h=.5,col='red')
    }
    grDevices::dev.off()
    for(k in names(released))write.csv(released[[k]],file.path(share,paste0(k,'.csv')),row.names=FALSE,na='SUPPRESSED')
    write.csv(matrices,file.path(share,'model_matrices.csv'),row.names=FALSE,na='NOT_ASSESSED')
    write.csv(rbind(statuses,do.call(rbind,comparisons)),file.path(share,'checks.csv'),row.names=FALSE)
    write.csv(data.frame(component=c('R','survival'),version=c(as.character(getRversion()),as.character(utils::packageVersion('survival')))),file.path(share,'runtime.csv'),row.names=FALSE)
    source_paths<-c(file.path(scripts,names(expected)),file.path(impl,names(auxiliary)),file.path(here,'local_checks.R'))
    write.csv(data.frame(file=basename(source_paths),md5=unname(tools::md5sum(source_paths))),file.path(share,'source_hashes.csv'),row.names=FALSE)
    capture.output(sessionInfo(),file=file.path(private,'sessionInfo.txt'))
    writeLines(c('Completed local technical/diagnostic export; NOT a scientific PASS.',
      'Current input versions and two fresh fixed primary fits are checked, not historical input identity.',
      'Primary coefficients/scales compared with public S3 values, absolute tolerance1e-8 fixed before run.',
      'Overlap comparison is BEFORE timing/followup/covariate restrictions; it is not a counterfactual final-cohort exclusion count.',
      'Version matches identify current input fields, not the exact historical files or download date.',
      'Eight existing model matrices checked without refitting eight models. Only two primary Weibull models fitted.',
      'Location/log-scale scores use analytic Weibull interval/right probability derivatives, independently tested against pweibull finite differences; no residuals.survreg scale scores.',
      'Quantiles q50/q90/q95 are of absolute standardized scores or absolute DFBETA magnitudes; no extrema exported.',
      'Household influence sums SIGNED approximate DFBETAs before magnitudes; includes log-scale parameter.',
      'Influence threshold0.5SE is a descriptive review flag, not a validated exclusion rule. No observations removed.',
      'Counts<10 including0 suppress the whole influence block. Other counts rounded to10; no exact sample/case records exported.',
      'Tertile boundaries are empirical and ties can yield fewer bands; band labels preserve order, not common ranges across predictors.',
      'Grouped means require every band n>=200,>=50households,>=10interval events and>=10censored; otherwise all bands of that variable suppressed.',
      'Grouped score patterns are descriptive in-sample diagnostics, not calibrated hypothesis tests or validation.',
      'Quantiles require>=200persons/households and>=10events/censored. No minimum, maximum, IDs or per-case values in SEND_BACK.',
      'No bootstrap, ML tuning, model-selection search, automatic repairs or manuscript changes.',
      'Review SEND_BACK locally before sharing; LOCAL_ONLY includes participant-derived diagnostics and must stay private.',
      'Suppression is not an anonymity guarantee. Original full-source/nativeWord/rights/author decisions remain separate.'),file.path(share,'README.txt'))
    writeLines('CHECKED: narrow aggregate release rules applied. Review locally before sharing.',file.path(share,'DISCLOSURE_CHECK.txt'))
    allowed<-c(paste0(names(released),'.csv'),'model_matrices.csv','checks.csv','runtime.csv','source_hashes.csv','README.txt','DISCLOSURE_CHECK.txt','STATUS.txt')
    stopifnot(setequal(list.files(share),allowed))
    writeLines('SUCCESS: execution completed; MATCH/NOT_CONFIRMED and diagnostics require separate review.',file.path(share,'STATUS.txt'))
    TRUE
  },error=function(e){message(conditionMessage(e));unlink(list.files(share,full.names=TRUE));writeLines('FAILED: no release. Details only in LOCAL_ONLY/checks.log.',file.path(share,'STATUS.txt'));FALSE})
  sink(type='message');sink();close(log)
  if(!ok)stop('Lokale Pruefung fehlgeschlagen. Privates Log lokal ansehen, nicht weitergeben.',call.=FALSE)
  cat('Fertig. Bitte lokal pruefen und ausschliesslich diesen Ordner zurueckgeben:\n',share,'\n',sep='')
  invisible(out)
}
if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  local_main(commandArgs(trailingOnly=TRUE),dirname(normalizePath(script)))
}
