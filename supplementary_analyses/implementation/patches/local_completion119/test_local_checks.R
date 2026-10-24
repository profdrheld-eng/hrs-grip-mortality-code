# Invented records only. Never calls the private-data main routine.
args <- commandArgs(trailingOnly=TRUE)
e <- new.env(parent=globalenv())
sys.source(file.path(args[1],'local_checks.R'),envir=e)
stopifnot(identical(e$protected_count_block(c(0,40,60)),rep('SUPPRESSED',3)),
 identical(e$protected_count_block(c(10,40,60)),c('10','40','60')))
x <- cbind(1,seq_len(30),seq_len(30))
r <- e$matrix_diagnostics(x)
stopifnot(r$rank_status=='RANK_DEFICIENT',r$condition_status=='NOT_INTERPRETABLE')
x <- cbind(1,seq(-1,1,length.out=60),sin(seq_len(60)))
stopifnot(e$matrix_diagnostics(x)$rank_status=='FULL_RANK')
stopifnot(all(e$influence_summary(matrix(0,400,3),rep(seq_len(200),each=2),rep(FALSE,400))$value=='SUPPRESSED'))
set.seed(118)
d <- data.frame(age=runif(800,50,90),sex=factor(rep(1:2,400)),
 smoke=factor(rep(c('never','former','current','never'),200)),
 health=factor(rep(1:5,160)),grip14=rnorm(800,30,7),grip10=rnorm(800,31,8),
 household=rep(seq_len(400),each=2))
t <- exp(2+.01*(d$age-60)+.4*rnorm(800));d$lo <- pmin(floor(t*12)/12,5)
d$hi <- ifelse(t>=5,Inf,d$lo+1/12);d$lo[d$lo==0]<-1/1200
fit <- survival::survreg(survival::Surv(lo,hi,type='interval2')~age+sex+grip14,data=d,
 dist='weibull',x=TRUE,y=TRUE)
z <- e$model_diagnostics(fit,d,'invented')
stopifnot(nrow(z$matrix)==1,nrow(z$quantiles)==12,all(is.finite(z$quantiles$value)),
 all(c('location_score','scale_score')%in%z$grouped$diagnostic),
 ncol(residuals(fit,type='matrix'))==6)
# Independent probability oracle: differentiate pweibull interval probabilities.
log_probability <- function(eta,logs) {
 a<-pweibull(d$lo,shape=1/exp(logs),scale=exp(eta),lower.tail=FALSE,log.p=TRUE)
 b<-pweibull(d$hi,shape=1/exp(logs),scale=exp(eta),lower.tail=FALSE,log.p=TRUE)
 event<-is.finite(d$hi);out<-a
 out[event]<-a[event]+log(-expm1(b[event]-a[event]));out
}
check_oracle <- function(fit) {
 out<-e$model_diagnostics(fit,d,'oracle');eta<-fit$linear.predictors;logs<-log(fit$scale);eps<-1e-5
 expected_location<-(log_probability(eta+eps,logs)-log_probability(eta-eps,logs))/(2*eps)
 expected_scale<-(log_probability(eta,logs+eps)-log_probability(eta,logs-eps))/(2*eps)
 actual<-out$private$score
 stopifnot(max(abs(actual[,2]-expected_location))<1e-6,
   max(abs(actual[,4]-expected_scale))<1e-6,
   abs(sum(log_probability(eta,logs))-fit$loglik[2])<1e-7)
 x<-model.matrix(stats::delete.response(fit$terms),data=d,contrasts.arg=fit$contrasts)
 full<-cbind(x*expected_location,expected_scale)
 expected_dfb<-sweep(full%*%fit$var,2,sqrt(diag(fit$var)),'/')
 stopifnot(max(abs(out$private$dfbetas-expected_dfb))<1e-6,
   isTRUE(all.equal(unname(out$private$household_dfbetas),unname(rowsum(out$private$dfbetas,d$household)),tolerance=1e-10)))
 cat('PASS independent probability derivatives, likelihood and case/household influence.\n')
}
check_oracle(fit)
# Signed household collapse is checked against the independent derivatives above.
# Narrow release excludes extrema, IDs, models, subject vectors and unrounded cells.
share <- e$release_diagnostics(list(invented=z))
stopifnot(setequal(names(share),c('matrix','quantiles','grouped','influence')),
 !any(grepl('max|minimum|household_id|subject_id',unlist(lapply(share,names)))))
# Production uses natural-spline terms; verify fitted spline matrices and diagnostics.
core <- file.path(args[1],'../../analysis-work/02_Analyse/scripts')
sys.source(file.path(core,'hrs_analysis.R'),envir=e)
pair <- e$fit_pair(d)
for(name in names(pair)) {
  check_oracle(pair[[name]])
  q <- e$model_diagnostics(pair[[name]],d,name)
  stopifnot(q$matrix$rank_status=='FULL_RANK',all(is.finite(q$quantiles$value)))
}
# With only20distinct artificial households, all distributional exports are withheld.
d_small_groups <- d;d_small_groups$household<-rep(seq_len(20),length.out=nrow(d))
g <- e$model_diagnostics(pair$current,d_small_groups,'invented_small_groups')
stopifnot(all(is.na(g$quantiles$value)),all(g$grouped$standardized_mean=='SUPPRESSED'),
 all(g$influence$value=='SUPPRESSED'))
# AST adapter must preserve prior outputs and add only local data/key return fields.
f <- function(x) {sex<-x;included<-x>0;d<-data.frame(x=x);analytical_keys<-seq_along(x);list(sex=sex,included=included)}
adapted <- e$cohort_adapter(f)
a <- f(c(1,-1));b <- adapted(c(1,-1))
stopifnot(identical(a$sex,b$sex),identical(a$included,b$included),identical(b$data,data.frame(x=c(1,-1))))
cat('PASS: invented interval-censoring, score/scale diagnostics, signed household influence, rank deficiency and block suppression.\n')
