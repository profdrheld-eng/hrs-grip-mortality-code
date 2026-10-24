# Artificial OOF records and a hand-computed risk-change summary, no HRS inputs.
args<-commandArgs(trailingOnly=TRUE);root<-args[1];original<-args[2]
source(file.path(root,'scripts','hrs_review_analyses.R'));source(file.path(root,'scripts','hrs_review_oof.R'))
n<-80;d<-data.frame(household=as.character(rep(1:40,each=2)))
risk<-matrix(c(rep(.1,n),rep(c(.1,.13,.07,.12),each=20)),ncol=2)
a<-list(risk=risk,combos=data.frame(model='weibull',target=c('current','prior'),arm='original'))
s<-review_oof_summary(d,a,B=50)
stopifnot(s$proportion[s$comparison=='absolute']==.5,
          s$proportion[s$comparison=='increase']==.25,
          s$proportion[s$comparison=='decrease']==.25,
          all(s$ci_lower<=s$proportion),all(s$ci_upper>=s$proportion))
# Recreate selected original models using historical pipeline and compare to its replay.
source(file.path(original,'hrs_ml_extension.R'));ml_source_existing(original)
source(file.path(original,'hrs_ml_point_figures.R'))
set.seed(817);n<-160;g<-rnorm(n,30,6);t<-rweibull(n,1.4,8)
z<-data.frame(household=as.character(rep(1:80,each=2)),age=runif(n,50,90),sex=factor(rep(1:2,n/2)),
 smoke=factor(sample(c('never','former','current'),n,TRUE)),health=factor(sample(1:5,n,TRUE)),grip14=g,grip10=g+rnorm(n,2,4),
 gap_years=4,lo=ifelse(t<5,pmax(.0001,floor(t*12)/12),5),hi=ifelse(t<5,ceiling(t*12)/12,Inf))
z$change_kg<-z$grip14-z$grip10;z$hi[z$hi>5]<-Inf
cfg<-ml_config('smoke');cfg$models<-'weibull'
tuning<-expand.grid(fraction=1,fold=1:2,model='weibull',target=c('current','prior'),arm=c('original','augmented'),stringsAsFactors=FALSE)
tuning$ratio<-0;tuning$parameter<-''
old<-ml_replay_predictions(z,cfg,tuning);new<-review_oof_replay(z,cfg,tuning)
ix<-which(old$combos$arm=='original')
stopifnot(isTRUE(all.equal(old$risk[,ix],new$risk,tolerance=1e-12)),
          isTRUE(all.equal(old$weights,new$weights,tolerance=1e-12)))
cat('PASS: OOF proportions, signed changes and paired household intervals; original OOF replay reproduced\n')
