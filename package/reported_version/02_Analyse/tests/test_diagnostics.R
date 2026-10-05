for(name in c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R',
              'hrs_analysis.R','hrs_prediction_validation.R','hrs_grip_change.R'))
  source(file.path('scripts',name))
if(file.exists('scripts/hrs_diagnostics.R')) source('scripts/hrs_diagnostics.R')
stopifnot(exists('scenario_flow',mode='function'),exists('diagnostics_main',mode='function'))
f <- data.frame(reason=c('included','included','unresolved_conflict'),hi=c(1,Inf,Inf))
counts <- scenario_flow(f,c(TRUE,FALSE,TRUE),c(TRUE,TRUE,TRUE))
stopifnot(counts['included']==1,counts['before_covariate_exclusions']==1,
 counts['analyzed']==1,counts['unresolved_conflict']==1,counts['timing_excluded']==1)
counts <- scenario_flow(f,c(TRUE,TRUE,TRUE),c(FALSE,TRUE,TRUE))
stopifnot(counts['included']==2,counts['analyzed']==1,counts['covariate_exclusions']==1)

raw <- matrix(c(0,0,NA,993,0,20,999,998,110,NA,NA,NA),nrow=3,byrow=TRUE)
q <- grip_quality_counts(raw,c(1,2,NA))
stopifnot(q['zero_readings']==3,q['people_valid_only_if_zero_allowed']==1,
 q['people_any_positive_valid']==1,q['other_outside_range']==1,
 q['effort_full']==1,q['effort_limited_or_unclear']==1,q['effort_missing_or_invalid']==1)
# Sparse calibration rows must not expose estimates even if the group is large.
d <- data.frame(lo=rep(5,100),hi=Inf)
cal <- calibration_groups(d,rep(.1,100))
stopifnot(all(is.na(cal$observed_risk)),all(is.na(cal$predicted_risk)))
d <- data.frame(lo=c(rep(1,20),rep(5,80)),hi=c(rep(1.2,20),rep(Inf,80)))
cal <- calibration_groups(d,rep(.2,100))
ok <- cal$status=='OK'
stopifnot(sum(ok)==1,abs(cal$observed_risk[ok]-.2)<1e-12,
 abs(cal$predicted_risk[ok]-.2)<1e-12)
cat('Sequential flow, zero-grip diagnostics and calibration suppression checks passed.\n')

# Generate a full synthetic fixed-width cohort through the existing test fixture.
source('tests/test_grip_change.R')
t$OAGE <- round(d$age)
# Ensure known five-year survivors despite the half-month origin approximation.
t$LASTALIVEYR[!is.finite(d$hi)] <- 2020
t$LASTALIVEMO[!is.finite(d$hi)] <- 6
write.csv(t,file.path(root,'trk2022tr_r.csv'),row.names=FALSE)
imported <- stage2_main(c(root,tempfile('diagnostic_flow_')),normalizePath('scripts/hrs_feasibility.R'))
prepared <- prepare_change_data(imported)
# A missing Tracker month must not remove a valid core-month sensitivity case.
candidate <- match(prepared$keys[1],imported$tracker$key)
imported$tracker$OIWMONTH[candidate] <- NA_real_
missing_tracker <- prepare_change_data(imported)
stopifnot(identical(prepared$flow_context$covok,missing_tracker$flow_context$covok))
flow_missing <- corrected_flows(missing_tracker$flow_context)
flow_original <- corrected_flows(prepared$flow_context)
stopifnot(identical(flow_missing$count_rounded_10[flow_missing$metric=='core_month_analyzed'],
 flow_original$count_rounded_10[flow_original$metric=='core_month_analyzed']))
prior <- list.files(output,recursive=TRUE,full.names=TRUE)
hash <- tools::md5sum(prior)
cli <- system2(file.path(R.home('bin'),'Rscript'),
 shQuote(c(normalizePath('scripts/hrs_diagnostics.R'),root,output)),stdout=TRUE,stderr=TRUE)
if(!is.null(attr(cli,'status'))) cat(paste(cli,collapse='\n'),'\n')
out <- file.path(output,'diagnostics_v1')
stopifnot(is.null(attr(cli,'status')),
 startsWith(readLines(file.path(out,'STATUS.txt'))[1],'SUCCESS'),
 identical(hash,tools::md5sum(prior)))
required <- c('corrected_scenario_flow.csv','grip_quality.csv','censoring_counts.csv',
 'censoring_groups.csv','weight_diagnostics.csv','joint_support.csv',
 'calibration_groups.csv','survival_fit.csv','model_diagnostics.png','source_sha256.txt','READ_ME.txt')
stopifnot(all(file.exists(file.path(out,required))))
calibration <- read.csv(file.path(out,'calibration_groups.csv'))
stopifnot(any(calibration$status=='OK'),
 all(is.finite(calibration$observed_risk[calibration$status=='OK'])))
file.copy(file.path(out,'model_diagnostics.png'),tempfile('hrs_diagnostics_synthetic_',fileext='.png'),overwrite=TRUE)
for(path in list.files(out,pattern='\\.csv$',full.names=TRUE)) {
 names <- names(read.csv(path))
 stopifnot(!any(c('HHID','PN','key','household') %in% names))
}
flow <- read.csv(file.path(out,'corrected_scenario_flow.csv'))
stopifnot(flow$count_rounded_10[flow$metric=='primary_included']==
 flow$count_rounded_10[flow$metric=='primary_before_covariate_exclusions'])
t$OAGE[1] <- 'invalid'
write.csv(t,file.path(root,'trk2022tr_r.csv'),row.names=FALSE)
failed <- suppressWarnings(system2(file.path(R.home('bin'),'Rscript'),
 shQuote(c(normalizePath('scripts/hrs_diagnostics.R'),root,output)),stdout=TRUE,stderr=TRUE))
stopifnot(attr(failed,'status')==1L,
 startsWith(readLines(file.path(out,'STATUS.txt'))[1],'INCOMPLETE'))
cat('Full diagnostic CLI, output privacy schema, provenance and failure status passed.\n')
