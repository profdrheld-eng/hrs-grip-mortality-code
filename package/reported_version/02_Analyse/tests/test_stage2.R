source('scripts/hrs_stage2.R')
sm <- smoking_status(c(NA,NA,5,1,NA,NA,5),c(NA,NA,NA,NA,5,NA,5),
                     c(1,1,1,1,5,5,1),c(5,1,1,5,NA,5,1))
stopifnot(identical(sm,c('never','unresolved','former','current','never','unresolved','conflict')))
stopifnot(identical(stroke_status(c(1,2,3,4,5,8,9,NA,7)),
 c('yes','possible_or_TIA','yes','no','no','missing','missing','missing','invalid')))
stopifnot(is.na(month_index(2020,13)),is.na(month_index(9999,1)))
base <- month_index(2014,6)
d <- data.frame(NYEAR=c(2018,2019,2020,NA,2013),NMONTH=c(1,2,1,NA,1),
 LASTALIVEYR=c(2017,2018,2019,2020,2012),LASTALIVEMO=c(1,1,7,1,1),
 KNOWNDECEASEDYR=NA_real_,KNOWNDECEASEDMO=NA_real_,KNOWNDECEASEDSOURCE=NA_real_)
r <- timing_flags(d,rep(base,5))
stopifnot(r$ndi_within[1],r$boundary_uncertain[2],r$beyond[3],
          r$beyond[4],r$prebaseline[5])
d$LASTALIVEYR[1] <- 2020
stopifnot(timing_flags(d,rep(base,5))$conflict[1])
e <- data.frame(NYEAR=c(2018,NA,2019,NA,NA),NMONTH=c(1,NA,2,NA,NA),
 EXDEATHYR=c(NA,2018,NA,NA,NA),EXDEATHMO=c(NA,2,NA,NA,NA),
 KNOWNDECEASEDYR=NA_real_,KNOWNDECEASEDMO=NA_real_,KNOWNDECEASEDSOURCE=NA_real_,
 LASTALIVEYR=c(2017,2017,2018,2020,2016),LASTALIVEMO=1)
stopifnot(identical(endpoint_candidate(e,rep(base,5)),
 c('death_within_5y','death_within_5y','horizon_uncertain','alive_at_5y','censored_before_5y')))
e$LASTALIVEYR[1] <- 2020
stopifnot(endpoint_candidate(e,rep(base,5))[1]=='source_conflict')
e$LASTALIVESOURCE <- 1
b <- conflict_breakdown(e)
stopifnot(b$alive_after_ndi[1], !b$ndi_vs_exit[1],
          identical(b$any_conflict,endpoint_candidate(e,rep(base,5))=='source_conflict'))
e$NYEAR[2] <- 2019; e$NMONTH[2] <- 1
stopifnot(conflict_breakdown(e)$ndi_vs_exit[2])
e$LASTALIVESOURCE[1] <- 4
stopifnot(conflict_breakdown(e)$conflict_with_imputed_alive[1])
d$LASTALIVEYR[1] <- 2017; d$KNOWNDECEASEDYR[1] <- 2019
d$KNOWNDECEASEDMO[1] <- 1; d$KNOWNDECEASEDSOURCE[1] <- 1
stopifnot(timing_flags(d,rep(base,5))$conflict[1])
cat('Synthetic month, horizon and source-conflict checks passed.\n')
root <- tempfile(); output <- tempfile(); dir.create(root)
sets <- list(h10i_r=setNames(c(rep(20,4),1,1),paste0('MI',c('816','851','852','853','817','818'))),
 h14i_r=setNames(c(rep(22,4),1,1,65,150),paste0('OI',c('816','851','852','853','817','818','834','841'))),
 h14a_r=c(OA500=6,OA501=2014),
 h14c_r=c(OC001=2,OC005=5,OC010=5,OC018=5,OC030=5,OC036=5,OC053=5,OC116=5,OC117=NA),
 h14pr_r=c(OZ076=1,OZ205=5))
for(name in names(sets)) {
 vals <- sets[[name]]; n <- length(vals); end <- 9+4*n
 writeLines(c(paste0('INFILE "synthetic" LRECL = ',end,';'),'INPUT',
   ' HHID $ 1 - 6',' PN $ 7 - 9',
   paste(names(vals),10+4*(seq_len(n)-1),'-',13+4*(seq_len(n)-1)),';'),file.path(root,paste0(name,'.sas')))
 text <- paste0('000001010',paste(ifelse(is.na(vals),'    ',sprintf('%4d',as.integer(vals))),collapse=''))
 writeLines(text,file.path(root,paste0(name,'.da')))
}
t <- data.frame(HHID='000001',PN='010',OVHHID='000000',OVPN='000',OVYEAR=0,OVRESULT=0,
 NYEAR=2018,NMONTH=1,NSCORE=80,EXDEATHYR=2018,EXDEATHMO=2,EXDODSOURCE=1,
 KNOWNDECEASEDYR=2018,KNOWNDECEASEDMO=2,KNOWNDECEASEDSOURCE=1,
 LASTALIVEYR=2017,LASTALIVEMO=1,LASTALIVESOURCE=1,VERSION=1,OAGE=60,SEX=1,
 MIWYEAR=2010,MIWMONTH=6,OIWYEAR=2014,OIWMONTH=6,OPMWGTR=1,STRATUM=1,SECU=1)
write.csv(t,file.path(root,'trk2022tr_r.csv'),row.names=FALSE)
stage2_main(c(root,output),normalizePath('scripts/hrs_feasibility.R'))
counts <- read.csv(file.path(output,'stage2_counts.csv'))
stopifnot(all(counts$count_rounded_10=='SUPPRESSED'),
          'flow_06_agreed_ordered_interview_months' %in% counts$metric,
          startsWith(readLines(file.path(output,'stage2_READ_ME.txt'))[1],'SUCCESS'))
# Failure preserves the earlier report but marks it as stale.
t$OAGE <- 'unexpected'
write.csv(t,file.path(root,'trk2022tr_r.csv'),row.names=FALSE)
stopifnot(inherits(try(stage2_main(c(root,output),normalizePath('scripts/hrs_feasibility.R')),silent=TRUE),'try-error'),
          startsWith(readLines(file.path(output,'stage2_READ_ME.txt'))[1],'INCOMPLETE'))
cat('Synthetic full Stage2 run, suppressed output and failure-status checks passed.\n')
