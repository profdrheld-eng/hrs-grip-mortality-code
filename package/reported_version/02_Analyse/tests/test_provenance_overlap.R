source('02_Analyse/scripts/hrs_provenance_overlap_audit.R')
t <- data.frame(HHID=c('000001','000002','000003'),PN=c('010','010','010'),
 OVHHID=c('000000','000001','999999'),OVPN=c('000','010','010'),
 OVRESULT=c(0,3,1),OVYEAR=c(NA,2012,NA),OAGE=c(60,70,80))
t$key <- paste(t$HHID,t$PN,sep=':')
w10 <- data.frame(key=c('000001:010','000002:010'),valid_trials=c(4,0))
w14 <- data.frame(key=t$key,valid_trials=c(4,4,4))
z <- overlap_checks(t,w10,w14)
stopifnot(z['flagged_before_current_link_rule']==2,
 z['old_id_has_prior_grip_new_id_has_current_grip']==1,
 z['old_id_absent_from_tracker']==1,
 z['flagged_in_existing_pre_overlap_sample']==0)
bad <- t;bad$OVRESULT[2] <- 9
stopifnot(inherits(try(overlap_checks(bad,w10,w14),silent=TRUE),'try-error'))
bad <- t;bad$OVPN[2] <- 'wrong'
stopifnot(inherits(try(overlap_checks(bad,w10,w14),silent=TRUE),'try-error'))
r <- protected_counts(c(a=10,b=1,c=0))
stopifnot(all(r$value=='SUPPRESSED_BLOCK'))
r <- protected_counts(c(a=14,b=20,c=0))
stopifnot(identical(r$value,c('10','20','0')))
q <- tempfile();dir.create(q)
writeLines(c('HRS 2010 Final Release Version 6','INPUT','HHID $ 1-6'),file.path(q,'h10i_r.sas'))
h <- release_headers(file.path(q,'h10i_r.sas'))
stopifnot(nrow(h)==1,grepl('Final Release Version 6',h$header_evidence))
stopifnot(nrow(release_headers(character()))==0)
unlink(q,recursive=TRUE)
cat('Synthetic overlap, invalid-code, privacy and release-header checks passed.\n')
