a<-commandArgs(trailingOnly=TRUE);source(file.path(a[1],'release_oof_safely.R'))
run<-tempfile('artificial_release_');dir.create(run)
dir.create(file.path(run,'SEND_BACK'));dir.create(file.path(run,'LOCAL_ONLY'))
p<-file.path(run,'SEND_BACK','oof_risk_change.csv')
z<-data.frame(model=rep(c('weibull','neural'),each=3),comparison=rep(c('absolute','increase','decrease'),2),
 count_rounded_10=c('100','SUPPRESSED','100','300','100','200'),proportion=c(.104,NA,.100,.3,.1,.2),
 ci_lower=c(.08,NA,.08,.2,.05,.1),ci_upper=c(.13,NA,.13,.4,.15,.3))
write.csv(z,p,row.names=FALSE);original<-readBin(p,'raw',n=file.info(p)$size)
writeLines('INCOMPLETE',file.path(run,'SEND_BACK','STATUS.txt'))
stopifnot(inherits(tryCatch(release_oof_safely(run),error=identity),'error'))
writeLines('SUCCESS: synthetic test only',file.path(run,'SEND_BACK','STATUS.txt'))
release_oof_safely(run);out<-read.csv(p)
stopifnot(all(out$count_rounded_10[1:3]=='SUPPRESSED'),all(is.na(out$proportion[1:3])),
 all(is.na(out$ci_lower[1:3])),all(is.na(out$ci_upper[1:3])),identical(out[4:6,],z[4:6,]),
 identical(readBin(file.path(run,'LOCAL_ONLY','oof_risk_change_before_complementary_suppression.csv'),'raw',length(original)),original))
release_oof_safely(run);stopifnot(identical(read.csv(p),out))
unlink(run,recursive=TRUE)
cat('PASS: completed-output repair, private original preservation, idempotence, and active-run rejection\n')
