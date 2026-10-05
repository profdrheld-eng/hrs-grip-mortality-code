source('scripts/hrs_stage2.R')
source('scripts/hrs_source_hierarchy.R')
t <- data.frame(NYEAR=c(2018,NA,2018,NA),NMONTH=c(1,NA,1,NA),
 EXDEATHYR=c(2019,2018,NA,NA),EXDEATHMO=c(1,2,NA,NA),EXDODSOURCE=c(1,1,NA,NA),
 KNOWNDECEASEDYR=c(2019,2018,NA,2018),KNOWNDECEASEDMO=c(1,2,NA,2),
 KNOWNDECEASEDSOURCE=c(1,1,NA,4),LASTALIVEYR=c(2017,2017,2020,2017),
 LASTALIVEMO=1,LASTALIVESOURCE=1)
p <- select_death_source(t,'ndi_first')
stopifnot(identical(p$source,c('NDI','EXIT','NDI','NONE')),
 p$lower[1]==12*2018,p$upper[1]==12*2018+2,
 p$death_source_conflict[1],p$alive_conflict_selected[3],p$requires_review[3],
 is.na(p$lower[4]))
a <- select_death_source(t,'interview_first')
stopifnot(a$source[1]=='EXIT',a$lower[1]==12*2019)
e <- select_death_source(t,'exclude_conflicts')
stopifnot(e$excluded[1],e$excluded[3],is.na(e$lower[1]),!e$excluded[2])
n <- select_death_source(t,'ndi_only')
stopifnot(n$source[2]=='NONE',is.na(n$lower[2]))
t$EXDODSOURCE[2] <- 9; t$KNOWNDECEASEDSOURCE[2] <- 4
stopifnot(select_death_source(t)$source[2]=='NONE')
cat('Synthetic source priority, conflicts, restricted fallback and imputation checks passed.\n')
