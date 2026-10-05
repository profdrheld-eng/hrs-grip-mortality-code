# User-run cohort description only. Reuses cohort selection; fits no models.
table1_summary <- function(d) {
  continuous <- c(age='Age, years',grip10='Grip strength 2010, kg',
    grip14='Grip strength 2014, kg',change_kg='Grip change 2014 minus 2010, kg',
    gap_years='Interview interval, years')
  required <- c(names(continuous),'sex','smoke','health','lo','hi')
  if(!all(required %in% names(d)) || nrow(d)<10 ||
     anyNA(d[required]) || any(!is.finite(as.matrix(d[names(continuous)]))))
    stop('Incomplete or insufficient cohort.')
  if(any(d$lo<=0 | d$lo>5 | d$hi<=d$lo) ||
     any(is.finite(d$hi)&d$hi>5) ||
     any(abs(d$change_kg-(d$grip14-d$grip10))>1e-8))
    stop('Invalid follow-up or change definition.')
  n <- nrow(d); nr <- as.numeric(diagnostic_report(c(n=n))$count_rounded_10)
  row <- function(variable,label,level='',count=NA_character_,pct=NA_real_,
                  avg=NA_real_,spread=NA_real_) {
    data.frame(variable=variable,label=label,level=level,
      denominator_rounded_10=nr,count_rounded_10=count,percent_approx=pct,
      mean=avg,sd=spread,stringsAsFactors=FALSE)
  }
  result <- list(row('n','Analysed participants',count=as.character(nr)))
  for(v in names(continuous)) result[[length(result)+1L]] <-
    row(v,continuous[v],avg=round(mean(d[[v]]),1),spread=round(sd(d[[v]]),1))
  categorical <- function(v,label,values,labels) {
    if(any(!as.character(d[[v]]) %in% values)) stop('Unexpected category.')
    counts <- vapply(values,function(k) sum(as.character(d[[v]])==k),0)
    shown <- diagnostic_report(counts)$count_rounded_10
    # Prevent a single suppressed category being recovered by subtraction.
    if(sum(shown=='SUPPRESSED')==1L && any(shown!='SUPPRESSED')) {
      candidate <- which(shown!='SUPPRESSED')
      shown[candidate[which.min(counts[candidate])]] <- 'SUPPRESSED'
    }
    for(j in seq_along(values)) {
      pct <- if(shown[j]=='SUPPRESSED') NA_real_ else round(100*as.numeric(shown[j])/nr,1)
      result[[length(result)+1L]] <<- row(v,label,labels[j],shown[j],pct)
    }
  }
  categorical('sex','Sex',c('1','2'),c('Male','Female'))
  categorical('smoke','Smoking status',c('never','former','current'),c('Never','Former','Current'))
  categorical('health','Self-rated health',as.character(1:5),
    c('Excellent','Very good','Good','Fair','Poor'))
  d$followup <- ifelse(is.finite(d$hi),'death',ifelse(d$lo<5,'early','survived'))
  categorical('followup','Five-year outcome classification',c('death','early','survived'),
    c('Death interval within five years','Censored before five years','Known survival to five years'))
  do.call(rbind,result)
}

table1_markdown <- function(x) {
  lines <- c('# Table 1. Characteristics of the analytical cohort','',
    '| Characteristic | Mean ± SD or approximately n (%) |','|---|---|')
  for(i in seq_len(nrow(x))) {
    z <- x[i,]
    label <- paste0(z$label,if(nzchar(z$level)) paste0(': ',z$level) else '')
    value <- if(!is.na(z$mean)) sprintf('%.1f ± %.1f',z$mean,z$sd) else
      if(z$count_rounded_10=='SUPPRESSED') 'Suppressed' else
        if(is.na(z$percent_approx)) z$count_rounded_10 else
          sprintf('%s (%.1f)',z$count_rounded_10,z$percent_approx)
    lines <- c(lines,paste0('| ',label,' | ',value,' |'))
  }
  c(lines,'','SD, sample standard deviation. Continuous variables are arithmetic mean ± SD, rounded to one decimal.',
    'Counts are rounded to tens; percentages are approximate and calculated from the displayed rounded counts and denominator.',
    'Counts below ten (including zero) are suppressed. One additional category is suppressed if needed to avoid a lone suppressed category.',
    'Rounded counts or percentages may not add exactly to their total. No subgroup comparisons or hypothesis tests were performed.',
    'Grip change is 2014 minus 2010; negative values indicate loss. The interview interval approximates the measurement interval.',
    'The cohort has complete model variables by construction. Outcome rows describe ascertainment, not Kaplan–Meier mortality estimates.',
    'Unweighted selected repeated-measurement cohort; no population-representative claim.')
}

table1_main <- function(args,script_dir) {
  if(length(args)!=2L) stop('Usage: Rscript hrs_table1.R DATA_DIR OUTPUT_DIR')
  out <- file.path(args[2],paste0('table1_',format(Sys.time(),'%Y%m%d_%H%M%S')))
  if(dir.exists(out) || !dir.create(out,recursive=TRUE)) stop('Output directory unavailable.')
  status <- file.path(out,'STATUS.txt')
  writeLines('INCOMPLETE: descriptive export not finished.',status)
  imported <- stage2_main(c(args[1],file.path(out,'import_checks')),
    file.path(script_dir,'hrs_feasibility.R'))
  prepared <- prepare_change_data(imported); d <- prepared$data
  z <- prepared$flow_context
  primary <- scenario_flow(make_followup(z$tracker,z$origin+.5,'ndi_first'),
    !is.na(z$origin)&!is.na(z$early)&z$early<z$origin,z$covok)
  stopifnot(unname(primary['analyzed'])==nrow(d),
    unname(primary['interval_events'])==sum(is.finite(d$hi)))
  # Match rounded main-cohort counts to the frozen diagnostic reference.
  reference <- file.path(args[2],'diagnostics_v1','sample_flow.csv')
  if(!file.exists(reference)) stop('Frozen cohort reference unavailable.')
  ref <- read.csv(reference,stringsAsFactors=FALSE)
  actual <- diagnostic_report(prepared$counts)
  for(k in c('analyzed','interval_events','early_censored')) {
    a <- actual$count_rounded_10[actual$metric==k]
    b <- ref$count_rounded_10[ref$metric==k]
    if(length(a)!=1L || length(b)!=1L || a!=b) stop('Cohort reference mismatch.')
  }
  table <- table1_summary(d)
  write.csv(table,file.path(out,'table1.csv'),row.names=FALSE,na='')
  writeLines(table1_markdown(table),file.path(out,'TABLE1.md'))
  write.csv(actual,file.path(out,'sample_flow.csv'),row.names=FALSE)
  sources <- file.path(script_dir,c('hrs_table1.R','hrs_feasibility.R','hrs_stage2.R',
    'hrs_source_hierarchy.R','hrs_analysis.R','hrs_grip_change.R'))
  write.csv(data.frame(file=c(sources,reference),md5=unname(tools::md5sum(c(sources,reference)))),
    file.path(out,'source_md5.csv'),row.names=FALSE)
  writeLines(c(paste('R:',getRversion()),
    'Descriptive statistics only; no prediction models or bootstrap runs fitted.',
    'Same cohort-selection function as history/change/paper figures; primary selection count checked in memory.',
    'Rounded cohort, event and early-censor counts match diagnostics_v1. Count agreement is not proof of individual identity.',
    'No individual rows, IDs, predictions, coefficients or model objects exported.',
    'Counts rounded to tens and small categories suppressed; see TABLE1.md for percentage rules.',
    'Input data were not modified. New timestamped directory; prior outputs untouched.'),
    file.path(out,'READ_ME.txt'))
  writeLines('SUCCESS: aggregate Table 1 exported; inspect TABLE1.md and READ_ME.txt.',status)
  message('Aggregate Table 1 written to ',out)
}

if(sys.nframe()==0L) {
  script <- sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  directory <- dirname(normalizePath(script))
  for(name in c('hrs_feasibility.R','hrs_stage2.R','hrs_source_hierarchy.R',
               'hrs_analysis.R','hrs_grip_change.R')) source(file.path(directory,name))
  tryCatch(table1_main(commandArgs(trailingOnly=TRUE),directory),error=function(e) {
    message('Table 1 stopped. Inspect STATUS.txt and import_checks; detailed conditions are not exported.')
    quit(status=1L)
  })
}
