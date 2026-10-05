# Synthetic checks only. Run from the repository root.
source('02_Analyse/scripts/hrs_prediction_validation.R')
source('02_Analyse/scripts/hrs_ml_extension.R')
source('02_Analyse/scripts/hrs_ipcw_impact_audit.R')
old<-audit_legacy('03_Dokumentation/IPCW_Korrektur_2026-10-04/legacy_sources')
d<-data.frame(lo=c(.9,1,1.9,5),hi=c(1.1,Inf,2.1,Inf))
a<-audit_compare(d,d,'midpoint',old$horizon_weights,horizon_weights)
stopifnot(a$training_ties_present,a$weight_status=='CHANGED',a$affected==2,
  abs(a$max_difference-1/6)<1e-12)
z<-d;z$lo[2]<-1.1
b<-audit_compare(z,z,'midpoint',old$horizon_weights,horizon_weights)
stopifnot(!b$training_ties_present,b$weight_status=='IDENTICAL',b$affected==0)
c<-audit_compare(d,d[3:4,],'midpoint',old$ml_weights,ml_weights,ml=TRUE)
stopifnot(c$training_ties_present,c$weight_status=='CHANGED',c$affected==2)
stopifnot(identical(audit_counts(c(0,2,12)),rep('SUPPRESSED_BLOCK',3)),
  identical(audit_counts(c(0,10,22)),c('0','10','20')))
failed<-function(...) stop('SYNTHETIC_PRIVATE_ERROR_MARKER')
f<-audit_compare(d,d,'midpoint',old$horizon_weights,failed)
stopifnot(f$weight_status=='WEIGHT_CHECK_FAILED',is.na(f$affected))
stopifnot(!grepl('PRIVATE_ERROR',paste(unlist(f),collapse=' ')))
known<-data.frame(lo=c(.5,5,5),hi=c(1,Inf,Inf))
k<-audit_compare(known,known,'midpoint',old$horizon_weights,horizon_weights)
stopifnot(!k$training_ties_present,k$weight_status=='IDENTICAL')
cat('PASS: synthetic tie, no-tie, train/test, suppression, failure and horizon checks.\n')
