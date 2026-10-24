# Synthetic category counts only. No HRS input.
args<-commandArgs(trailingOnly=TRUE)
source(file.path(args[1],'scripts','hrs_review_analyses.R'));source(args[2])
check<-function(up,down,suppressed) {
 n<-1000L;current<-rep(.1,n);prior<-current
 prior[seq_len(up)]<-.13
 prior[up+seq_len(down)]<-.07
 replay<-list(risk=cbind(current,prior),combos=data.frame(model='weibull',target=c('current','prior'),arm='original'))
 d<-data.frame(household=as.character(rep(1:500,each=2)))
 s<-review_oof_summary(d,replay,B=20)
 if(suppressed) stopifnot(all(s$count_rounded_10=='SUPPRESSED'),all(is.na(s$proportion)),all(is.na(s$ci_lower)),all(is.na(s$ci_upper)))
 else stopifnot(all(s$count_rounded_10!='SUPPRESSED'),all(is.finite(s$proportion)),
  isTRUE(all.equal(s$proportion[s$comparison=='absolute'],s$proportion[s$comparison=='increase']+s$proportion[s$comparison=='decrease'])))
}
check(4,100,TRUE)
check(100,4,TRUE)
check(900,96,TRUE)
check(100,200,FALSE)
cat('PASS: small directional and complementary cells cannot be reconstructed from additive proportions or intervals\n')
