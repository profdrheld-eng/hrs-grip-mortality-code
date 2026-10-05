# Synthetic-only regression checks; never use HRS person-level files here.
for(name in c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R',
              'hrs_analysis.R','hrs_prediction_validation.R','hrs_grip_change.R'))
  source(file.path('scripts',name))
if(file.exists('scripts/hrs_paper_figures.R')) source('scripts/hrs_paper_figures.R')
stopifnot(exists('paper_numbers',mode='function'))
stopifnot(exists('paper_ba_stats',mode='function'),exists('paper_exact_flow',mode='function'))
ba_oracle <- paper_ba_stats(c(-2,-1,0,1,2))
stopifnot(abs(ba_oracle['bias'])<1e-12,
 abs(ba_oracle['upper_loa']-1.96*sd(c(-2,-1,0,1,2)))<1e-12,
 abs(ba_oracle['lower_loa']+1.96*sd(c(-2,-1,0,1,2)))<1e-12)
flow_input <- c(valid_grip_pairs=103,tracker_matched=101,age_and_overlap_eligible=95,
 ordered_interview_months=94,followup_eligible=90,analyzed=88)
flow_test <- paper_exact_flow(flow_input)
stopifnot(identical(flow_test$excluded,c(2,6,1,4,2)),
 sum(flow_test$excluded)+tail(flow_test$included,1)==head(flow_test$included,1))
layout <- paper_flow_layout(flow_test)
stopifnot(length(layout$included)==6,identical(layout$excluded,c(2,6,1,4,2)))
flow_zero <- flow_test;flow_zero$included[4] <- flow_zero$included[3]
flow_zero$excluded <- -diff(flow_zero$included)
layout_zero <- paper_flow_layout(flow_zero)
stopifnot(length(layout_zero$included)==5,
 identical(layout_zero$excluded,c(2,6,5,2)),layout_zero$date_note,
 sum(layout_zero$excluded)+tail(layout_zero$included,1)==head(layout_zero$included,1))
flow_input['tracker_matched'] <- 104
stopifnot(inherits(try(paper_exact_flow(flow_input),silent=TRUE),'try-error'))
stopifnot(identical(paper_numbers(c(-.0002,0,.0002)),c('-0.0002','0','0.0002')),
          !any(grepl('[eE]',paper_numbers(c(1e-7,1e6)))),
          !anyDuplicated(paper_numbers(c(.00001,.00002,.00003))))
stopifnot(length(paper_ticks(c(-.046,.046),3))>=3)
d <- data.frame(grip10=c(20,30,40),grip14=c(18,30,45))
p <- paper_point_coordinates(d,c(.1,.2,.3),c(.12,.19,.3))
stopifnot(identical(p$grip$x,d$grip10),identical(p$grip$y,d$grip14),
          max(abs(p$agreement$x-c(10,20,30)))<1e-12,
          max(abs(p$difference$x-c(11,19.5,30)))<1e-12,
          max(abs(p$difference$y-c(2,-1,0)))<1e-12,
          identical(p$grip$xlim,p$grip$ylim),identical(p$agreement$xlim,p$agreement$ylim),
          p$difference$ylim[1]<0,p$difference$ylim[2]>0)
stopifnot(inherits(try(paper_point_coordinates(d,c(.1,NA,.2),c(.1,.2,.3)),silent=TRUE),'try-error'))
# Output paths must never overwrite an earlier successful or failed run.
a <- tempfile('paper_runs_');dir.create(a)
one <- paper_run_directory(a);two <- paper_run_directory(a)
stopifnot(one!=two,dir.exists(one),dir.exists(two))
# Style contract is checked on a real graphics device.
pdf(tempfile(fileext='.pdf'));paper_style()
stopifnot(par('font')==2,par('font.axis')==2,par('font.lab')==2,par('font.main')==2)
dev.off()
cat('Numeric labels, paired coordinates, unit conversion, range checks, output isolation and bold typography passed.\n')

# Reuse the existing synthetic fixed-width cohort and importer regression fixture.
source('tests/test_grip_change.R')
t$OAGE <- round(d$age)
t$LASTALIVEYR[!is.finite(d$hi)] <- 2020
t$LASTALIVEMO[!is.finite(d$hi)] <- 6
write.csv(t,file.path(root,'trk2022tr_r.csv'),row.names=FALSE)
source('scripts/hrs_diagnostics.R')
imported <- stage2_main(c(root,tempfile('paper_import_')),normalizePath('scripts/hrs_feasibility.R'))
prepared <- prepare_change_data(imported);data <- prepared$data
pair <- fit_pair(data);pm <- pair_metrics(pair,data)
# Synthetic layout reports: intervals below are test fixtures, not inferential results.
r <- list(flow=diagnostic_report(prepared$counts))
r$comparison <- data.frame(scenario='primary',status='FIT_OK',
 current_score=interval_score(pair$current,data),prior_score=interval_score(pair$prior,data))
r$metrics <- do.call(rbind,lapply(rownames(pm),function(metric) {
 values <- c(pm[metric,],pm[metric,'prior']-pm[metric,'current'])
 width <- c(.015,.015,.001)
 data.frame(metric=metric,target=c('current','prior','prior_minus_current'),apparent=values,
 optimism_corrected=values,ci_lower=values-width,ci_upper=values+width,
 outer_success=200,nested_success=200,status='OK')
}))
stopifnot(isTRUE(paper_check_refit(data,pair,r)))
bad_r <- r;bad_r$metrics$apparent[1] <- bad_r$metrics$apparent[1]+.001
stopifnot(inherits(try(paper_check_refit(data,pair,bad_r),silent=TRUE),'try-error'))
z <- read.csv(file.path(output,'grip_change_v1','risk_curve.csv'))
r$change <- z;r$history <- z
r$history$risk_difference <- z$risk_difference/8
r$history$rd_ci_lower <- z$rd_ci_lower/8;r$history$rd_ci_upper <- z$rd_ci_upper/8
r$calibration <- do.call(rbind,lapply(c('weibull_current','weibull_prior'),function(model) {
 risk <- risk5(pair[[sub('weibull_','',model)]],data)
 data.frame(model=model,calibration_groups(data,risk))
}))
r$support <- expand.grid(strength_wave=c('grip10','grip14'),
 strength_band=c('[0,20)','[20,30)','[30,40)','[40,101)'),
 change_band=c('[-Inf,-5)','[-5,0)','[0,5)','[5, Inf)'),stringsAsFactors=FALSE)
r$support$count_rounded_10 <- rep(c('SUPPRESSED','20','100','400'),length.out=nrow(r$support))
r$sensitivity <- expand.grid(scenario=c('primary','core_month','agree_dates_only','month_start','month_end',
 'interview_first','exclude_conflicts','ndi_only','lognormal'),censor_time_position=c('lower','midpoint','upper'),
 model=c('current','prior'),metric=c('auc','brier'),stringsAsFactors=FALSE)
r$sensitivity$apparent <- ifelse(r$sensitivity$metric=='auc',.8,.08)+
 ifelse(r$sensitivity$model=='prior',.0001,0)
r$survival <- expand.grid(model=c('weibull_current','weibull_prior','lognormal_current','lognormal_prior'),year=1:5)
r$survival$predicted_survival <- exp(-.025*r$survival$year)
r$survival$turnbull_survival <- exp(-.026*r$survival$year)
# Every report has the same schema and location as its production counterpart.
report_output <- tempfile('paper_reports_');dir.create(report_output)
spec <- c(flow='diagnostics_v1/sample_flow.csv',support='diagnostics_v1/joint_support.csv',
 calibration='diagnostics_v1/calibration_groups.csv',survival='diagnostics_v1/survival_fit.csv',
 metrics='validation_v2/validated_metrics.csv',sensitivity='validation_v2/sensitivity_metrics.csv',
 comparison='validation_v2/model_comparison.csv',history='grip_history_v1/risk_curve.csv',change='grip_change_v1/risk_curve.csv')
for(folder in unique(dirname(spec))) {
 dir.create(file.path(report_output,folder));writeLines('SUCCESS: synthetic fixture.',file.path(report_output,folder,'STATUS.txt'))
}
for(key in names(spec)) write.csv(r[[key]],file.path(report_output,spec[[key]]),row.names=FALSE)
frozen <- list.files(report_output,recursive=TRUE,full.names=TRUE);hashes <- tools::md5sum(frozen)
readback <- paper_read_reports(report_output)
stopifnot(isTRUE(paper_check_refit(data,pair,readback)))
preview <- tempfile('hrs_paper_figures_v4_synthetic_')
dir.create(preview,showWarnings=FALSE)
agreement <- paper_bland_altman(data,pair)
expected_ba <- paper_ba_stats(100*(risk5(pair$prior,data)-risk5(pair$current,data)))
stopifnot(max(abs(agreement$estimate-unname(expected_ba)))<1e-12,
 !any(c('ci_lower','ci_upper','bootstrap_requested') %in% names(agreement)))
pages <- paper_pages(data,risk5(pair$current,data),risk5(pair$prior,data),readback,agreement,prepared$counts)
stopifnot(length(pages)==6, !'F03_prediction_performance' %in% names(pages))
paper_export(pages,preview,synthetic=TRUE)
stopifnot(length(list.files(preview,pattern='\\.png$'))==6,
 all(file.info(list.files(preview,pattern='\\.png$',full.names=TRUE))$size>10000))
# End-to-end CLI uses only generated input; actual private HRS files are never read.
command <- shQuote(c(normalizePath('scripts/hrs_paper_figures.R'),root,report_output))
cli <- system2(file.path(R.home('bin'),'Rscript'),command,stdout=TRUE,stderr=TRUE)
if(!is.null(attr(cli,'status'))) cat(paste(cli,collapse='\n'),'\n')
stopifnot(is.null(attr(cli,'status')),identical(hashes,tools::md5sum(frozen)))
runs <- list.dirs(report_output,recursive=FALSE,full.names=TRUE)
run <- runs[startsWith(basename(runs),'paper_figures_')]
stopifnot(length(run)==1,startsWith(readLines(file.path(run,'STATUS.txt'))[1],'SUCCESS:'),
 length(list.files(run,pattern='\\.png$'))==6,
 !any(grepl('\\.(rds|RData)$',list.files(run,recursive=TRUE))))
for(path in list.files(run,pattern='\\.csv$',recursive=TRUE,full.names=TRUE))
 stopifnot(!any(c('HHID','PN','household','grip10','grip14','current','prior') %in% names(read.csv(path))))
# Failed rerun creates a separate INCOMPLETE directory and preserves success.
old <- list.files(run,recursive=TRUE,full.names=TRUE);old_hash <- tools::md5sum(old)
t$OAGE[1] <- 'invalid';write.csv(t,file.path(root,'trk2022tr_r.csv'),row.names=FALSE)
failed <- suppressWarnings(system2(file.path(R.home('bin'),'Rscript'),command,stdout=TRUE,stderr=TRUE))
stopifnot(attr(failed,'status')==1L,identical(old_hash,tools::md5sum(old)),
 !any(grepl('invalid',failed,fixed=TRUE)))
# Missing/incomplete upstream reports must be rejected before rendering.
writeLines('INCOMPLETE: synthetic',file.path(report_output,'validation_v2','STATUS.txt'))
stopifnot(inherits(try(paper_read_reports(report_output),silent=TRUE),'try-error'))
cat('Synthetic six-figure rendering, original-refit check, complete CLI, output isolation, privacy schemas and failure handling passed.\n')
