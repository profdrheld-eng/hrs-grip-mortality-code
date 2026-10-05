# Local, user-run diagnostics only. No person-level output or model fitting.
smoking_status <- function(current,ever,reinterview,prior_ever) {
  out <- rep('unresolved',length(current))
  # HRS 2014 C/PR routing: never-smoking carry-forward only for reinterviews.
  history_yes <- ever %in% 1 | (reinterview %in% 1 & prior_ever %in% 1)
  out[current %in% 5 & history_yes] <- 'former'
  out[is.na(current) & ((reinterview %in% 1 & prior_ever %in% 5 & is.na(ever)) |
      (reinterview %in% c(0,5) & ever %in% 5))] <- 'never'
  out[current %in% 1] <- 'current'
  # A current response can update old history; an explicit contradictory current
  # ever-smoking response, however, needs adjudication.
  out[ever %in% 5 & (current %in% c(1,5) |
      (reinterview %in% 1 & prior_ever %in% 1))] <- 'conflict'
  out
}

stroke_status <- function(x) {
  out <- rep('invalid',length(x))
  out[is.na(x) | x %in% c(8,9)] <- 'missing'
  out[x %in% c(1,3)] <- 'yes'
  out[x %in% c(4,5)] <- 'no'
  out[x %in% 2] <- 'possible_or_TIA'
  out
}

month_index <- function(year,month) {
  valid <- !is.na(year) & !is.na(month) & year==floor(year) &
    year>=1900 & year<=2026 & month %in% 1:12
  ifelse(valid,12*year+month-1,NA_real_)
}

death_bounds <- function(year,month,quarter=FALSE) {
    ok <- !is.na(year) & year==floor(year) & year>=1900 & year<=2023
    m <- if(quarter) month %in% 1:4 else month %in% 1:12
    lo <- if(quarter) 3*(month-1) else month-1
    hi <- if(quarter) 3*month-1 else month-1
    cbind(ifelse(ok,12*year+ifelse(m,lo,0),NA_real_),
          ifelse(ok,12*year+ifelse(m,hi,11),NA_real_))
}

conflict_breakdown <- function(t) {
  ndi <- death_bounds(t$NYEAR,t$NMONTH,TRUE)
  ex <- death_bounds(t$EXDEATHYR,t$EXDEATHMO)
  known <- death_bounds(t$KNOWNDECEASEDYR,t$KNOWNDECEASEDMO)
  known[!t$KNOWNDECEASEDSOURCE %in% 1:3,] <- NA_real_
  yes <- function(x) !is.na(x) & x
  disjoint <- function(a,b) yes(a[,2]<b[,1] | b[,2]<a[,1])
  alive <- month_index(t$LASTALIVEYR,t$LASTALIVEMO)
  out <- data.frame(ndi_vs_exit=disjoint(ndi,ex),ndi_vs_known=disjoint(ndi,known),
    exit_vs_known=disjoint(ex,known),alive_after_ndi=yes(alive>ndi[,2]),
    alive_after_exit=yes(alive>ex[,2]),alive_after_known=yes(alive>known[,2]))
  out$any_conflict <- rowSums(out)>0
  out$multiple_conflict_types <- rowSums(out[,1:6,drop=FALSE])>1
  out$conflict_with_imputed_alive <- out$any_conflict & t$LASTALIVESOURCE %in% 4
  out$conflict_with_core_alive <- out$any_conflict & t$LASTALIVESOURCE %in% 1
  out$conflict_with_partner_alive <- out$any_conflict & t$LASTALIVESOURCE %in% 2
  out$conflict_with_field_alive <- out$any_conflict & t$LASTALIVESOURCE %in% 3
  out$conflict_with_unknown_alive_source <- out$any_conflict & !t$LASTALIVESOURCE %in% 1:4
  out
}

endpoint_candidate <- function(t,start) {
  # Provisional combined-source classification conditional on the start-month proxy.
  ndi <- death_bounds(t$NYEAR,t$NMONTH,TRUE)
  ex <- death_bounds(t$EXDEATHYR,t$EXDEATHMO)
  known <- death_bounds(t$KNOWNDECEASEDYR,t$KNOWNDECEASEDMO)
  known[!t$KNOWNDECEASEDSOURCE %in% 1:3,] <- NA_real_
  alive <- month_index(t$LASTALIVEYR,t$LASTALIVEMO)
  out <- rep('unknown_status',nrow(t))
  for(i in seq_len(nrow(t))) {
    if(is.na(start[i])) { out[i] <- 'start_unknown'; next }
    intervals <- rbind(ndi[i,],ex[i,],known[i,])
    intervals <- intervals[!is.na(intervals[,1]),,drop=FALSE]
    if(nrow(intervals)) {
      lo <- max(intervals[,1]); hi <- min(intervals[,2])
      if(lo>hi || (!is.na(alive[i]) && alive[i]>hi)) out[i] <- 'source_conflict'
      else if(hi<start[i]) out[i] <- 'death_before_start'
      else if(lo<=start[i]) out[i] <- 'start_overlap'
      else if(hi<start[i]+60) out[i] <- 'death_within_5y'
      else if(lo>start[i]+60) out[i] <- 'alive_at_5y'
      else out[i] <- 'horizon_uncertain'
    } else if(t$KNOWNDECEASEDSOURCE[i] %in% 4 && !is.na(t$KNOWNDECEASEDYR[i])) {
      out[i] <- 'imputed_death_review'
    } else if(!is.na(alive[i])) {
      if(alive[i]>start[i]+60) out[i] <- 'alive_at_5y'
      else if(alive[i]==start[i]+60) out[i] <- 'horizon_uncertain'
      else if(alive[i]>=start[i]) out[i] <- 'censored_before_5y'
    }
  }
  out
}

timing_flags <- function(t,start) {
  y <- t$NYEAR
  valid <- !is.na(y) & y==floor(y) & y>=1900 & y<=2023
  q <- t$NMONTH
  lo <- ifelse(valid,12*y+ifelse(q %in% 1:4,3*(q-1),0),NA_real_)
  hi <- ifelse(valid,12*y+ifelse(q %in% 1:4,3*q-1,11),NA_real_)
  alive <- month_index(t$LASTALIVEYR,t$LASTALIVEMO)
  ky <- t$KNOWNDECEASEDYR
  kvalid <- !is.na(ky) & ky==floor(ky) & ky>=1900 & ky<=2023
  km <- t$KNOWNDECEASEDMO
  klo <- ifelse(kvalid,12*ky+ifelse(km %in% 1:12,km-1,0),NA_real_)
  khi <- ifelse(kvalid,12*ky+ifelse(km %in% 1:12,km-1,11),NA_real_)
  yes <- function(v) !is.na(v) & v
  conflict <- yes(alive>hi) |
    yes(t$KNOWNDECEASEDSOURCE %in% 1:3 & (khi<lo | klo>hi))
  known_before <- yes(t$KNOWNDECEASEDSOURCE %in% 1:3 & khi<start)
  if(all(c('EXDEATHYR','EXDEATHMO') %in% names(t))) {
    ey <- t$EXDEATHYR; em <- t$EXDEATHMO
    ev <- !is.na(ey) & ey==floor(ey) & ey>=1900 & ey<=2023
    elo <- ifelse(ev,12*ey+ifelse(em %in% 1:12,em-1,0),NA_real_)
    ehi <- ifelse(ev,12*ey+ifelse(em %in% 1:12,em-1,11),NA_real_)
    conflict <- conflict | yes(ehi<lo | elo>hi) | yes(alive>ehi)
    known_before <- known_before | yes(ehi<start)
  }
  # Monthly intervals: equality at the horizon is uncertain, not an exact event.
  data.frame(
    ndi_within=yes(lo>start & hi<start+60) & !conflict,
    beyond=(yes(lo>start+60) | yes(alive>start+60)) & !conflict,
    boundary_uncertain=yes(lo<=start+60 & hi>=start+60),
    prebaseline=yes(hi<start) | known_before,
    baseline_uncertain=yes(lo<=start & hi>=start),
    conflict=conflict,
    early_last_contact=yes(alive>=start & alive<start+60) & !valid,
    interview_death_without_ndi=kvalid & !valid,
    imputed_death=kvalid & t$KNOWNDECEASEDSOURCE %in% 4,
    missing_last_contact=is.na(alive))
}

stage2_main <- function(args,helper) {
  source(helper,local=TRUE)
  if(length(args)!=2L || !dir.exists(args[1])) stop('Supply private data and output directories.')
  dir.create(args[2],recursive=TRUE,showWarnings=FALSE)
  note <- file.path(args[2],'stage2_READ_ME.txt')
  writeLines('INCOMPLETE RUN. Earlier stage2 reports are stale until this file reports success.',note)
  files <- list.files(args[1],recursive=TRUE,full.names=TRUE)
  one <- function(name) {
    hits <- files[tolower(basename(files))==tolower(name)]
    if(length(hits)!=1L) stop('Required file missing or duplicated: ',name)
    hits
  }
  specs <- list(h10i_r=c('HHID','PN',paste0('MI',c('816','851','852','853','817','818'))),
    h14i_r=c('HHID','PN',paste0('OI',c('816','851','852','853','817','818','834','841'))),
    h14a_r=c('HHID','PN','OA500','OA501'),
    h14c_r=c('HHID','PN','OC001','OC005','OC010','OC018','OC030','OC036','OC053','OC116','OC117'),
    h14pr_r=c('HHID','PN','OZ076','OZ205'))
  paths <- lapply(names(specs),function(n) c(one(paste0(n,'.da')),one(paste0(n,'.sas'))))
  csv <- one('trk2022tr_r.csv')
  audit <- read_tracker_csv(csv)
  report <- audit$report
  for(i in seq_along(specs)) {
    a <- audit_import(paths[[i]][1],paths[[i]][2],specs[[i]])
    a$report$check <- paste(names(specs)[i],a$report$check,sep=':')
    report <- rbind(report,a$report)
  }
  write.csv(report,file.path(args[2],'stage2_import_audit.csv'),row.names=FALSE)
  if(any(report$value=='FAIL') || !audit$ok ||
     any(report$value %in% c('READER_ERROR','UNRECOGNIZED')))
    stop('Stage2 import check failed; share stage2_import_audit.csv only.')
  # Extra named Tracker fields; values never enter diagnostic error messages.
  wanted <- c('HHID','PN','OVPN','OVYEAR','OVRESULT','OAGE','SEX','MIWYEAR','MIWMONTH',
    'OIWYEAR','OIWMONTH','EXDEATHMO','EXDODSOURCE','KNOWNDECEASEDMO',
    'KNOWNDECEASEDSOURCE','LASTALIVEMO','LASTALIVESOURCE','OPMWGTR','STRATUM','SECU')
  raw <- suppressWarnings(read.csv(csv,colClasses='character',check.names=FALSE,
    na.strings=c('','NA'),strip.white=TRUE,fill=FALSE,row.names=NULL))
  names(raw) <- toupper(names(raw))
  valid <- !anyDuplicated(names(raw)) && all(wanted %in% names(raw))
  add_audit <- function(k,ok) {
    report <<- rbind(report,data.frame(check=k,value=if(ok) 'PASS' else 'FAIL'))
    write.csv(report,file.path(args[2],'stage2_import_audit.csv'),row.names=FALSE)
  }
  add_audit('extra_tracker_columns',valid)
  if(!valid) stop('Extra Tracker columns absent or duplicated; see stage2_import_audit.csv.')
  t <- audit$data
  # Same file and row order as the validated named import. Do not merge on row numbers across files.
  for(field in setdiff(wanted,c('HHID','PN'))) {
    v <- trimws(raw[[field]]); v[v==''] <- NA_character_
    if(field=='OVPN') { t[[field]] <- v; next }
    n <- suppressWarnings(as.numeric(v))
    ok <- all(is.na(v) | (!is.na(n) & is.finite(n)))
    add_audit(paste0(field,'_numeric'),ok)
    if(!ok) stop('Extra Tracker numeric check failed; no field values exported.')
    t[[field]] <- n
  }
  core <- lapply(seq_along(specs),function(i) read_hrs(paths[[i]][1],paths[[i]][2],specs[[i]]))
  names(core) <- names(specs)
  counts <- numeric()
  add <- function(k,v) counts[k] <<- sum(v,na.rm=TRUE)
  w10 <- wave_summary(core$h10i_r,'M'); w14 <- wave_summary(core$h14i_r,'O')
  keys <- w14$key[w14$valid_trials>0]
  add('flow_01_valid_grip_2014',length(keys))
  add('unmatched_tracker_before_age',!keys %in% t$key)
  keys <- keys[keys %in% t$key]
  add('flow_02_tracker_matched',length(keys))
  z <- t[match(keys,t$key),]
  ageok <- !is.na(z$OAGE) & z$OAGE==floor(z$OAGE) & z$OAGE>=50 & z$OAGE<=120
  add('age_missing_or_outside_50_120',!ageok)
  keys <- keys[ageok]; add('flow_03_age_50_120',length(keys))
  keys <- keys[keys %in% w10$key[w10$valid_trials>0]]
  add('flow_04_repeated_grip',length(keys))
  z <- t[match(keys,t$key),]
  overlap <- z$OVHHID!='000000'
  add('overlap_requires_local_review',overlap)
  # No automatic overlap remapping; flag and hold aside for this provisional audit.
  keys <- keys[!overlap]; add('flow_05_no_flagged_overlap',length(keys))
  z <- t[match(keys,t$key),]
  a <- core$h14a_r[match(keys,core$h14a_r$key),]
  start <- month_index(z$OIWYEAR,z$OIWMONTH)
  early <- month_index(z$MIWYEAR,z$MIWMONTH)
  other <- month_index(a$OA501,a$OA500)
  add('missing_core_A_match',!keys %in% core$h14a_r$key)
  add('dates_missing_or_invalid',is.na(start)|is.na(early)|is.na(other))
  add('dates_core_tracker_disagree',!is.na(start)&!is.na(other)&start!=other)
  # Describe discrepancies; do not claim either source is the measurement date.
  delta <- other-start
  add('dates_core_later_1_month',delta==1)
  add('dates_core_later_over_1_month',delta>1)
  add('dates_core_earlier',delta<0)
  ok <- !is.na(start)&!is.na(other)&!is.na(early)&start==other&early<start
  add('dates_nonpositive_measurement_interval',!is.na(start)&!is.na(early)&early>=start)
  keys <- keys[ok]; z <- z[ok,,drop=FALSE]; start <- start[ok]
  add('flow_06_agreed_ordered_interview_months',length(keys))
  flags <- timing_flags(z,start)
  endpoint <- endpoint_candidate(z,start)
  conflicts <- conflict_breakdown(z)
  if(!identical(conflicts$any_conflict,endpoint=='source_conflict'))
    stop('Conflict diagnostics disagree with endpoint classification; previous counts are stale.')
  for(field in names(conflicts)) add(paste0('conflict_detail_',field),conflicts[[field]])
  for(level in c('death_within_5y','alive_at_5y','censored_before_5y','horizon_uncertain',
      'source_conflict','death_before_start','start_overlap','start_unknown',
      'imputed_death_review','unknown_status'))
    add(paste0('endpoint_candidate_',level),endpoint==level)
  for(field in names(flags)) add(paste0('timing_',field),flags[[field]])
  add('timing_status_unresolved',!(flags$ndi_within|flags$beyond) |
      flags$prebaseline|flags$baseline_uncertain|flags$conflict)
  add('flow_07_no_prebaseline_or_conflicting_NDI',
      !(flags$prebaseline|flags$baseline_uncertain|flags$conflict))
  # Coverage is reported on flow_06, before outcome-based exclusions.
  c <- core$h14c_r[match(keys,core$h14c_r$key),]
  pr <- core$h14pr_r[match(keys,core$h14pr_r$key),]
  pm <- core$h14i_r[match(keys,core$h14i_r$key),]
  add('covariate_denominator',length(keys))
  add('missing_core_C_match',!keys %in% core$h14c_r$key)
  add('missing_core_PR_match',!keys %in% core$h14pr_r$key)
  add('sex_not_1_or_2',!z$SEX %in% c(1,2))
  add('self_rated_health_unusable',!c$OC001 %in% 1:5)
  for(field in c('OC005','OC010','OC018','OC030','OC036','OC053')) {
    add(paste0(field,'_blank'),is.na(c[[field]]))
    add(paste0(field,'_DK_RF'),c[[field]] %in% c(8,9))
    add(paste0(field,'_disputed_history'),c[[field]] %in% c(3,4))
    allowed <- if(field=='OC053') c(1,2,3,4,5,8,9) else c(1,3,4,5,8,9)
    add(paste0(field,'_other_code'),!is.na(c[[field]]) & !c[[field]] %in% allowed)
  }
  stroke <- stroke_status(c$OC053)
  for(level in c('yes','no','possible_or_TIA','missing','invalid'))
    add(paste0('stroke_',level),stroke==level)
  smoke <- smoking_status(c$OC117,c$OC116,pr$OZ076,pr$OZ205)
  for(level in c('never','former','current','unresolved','conflict'))
    add(paste0('smoking_reconstructed_',level),smoke==level)
  add('smoking_current_answer_blank',is.na(c$OC117))
  add('smoking_prior_ever_blank',is.na(pr$OZ205))
  add('smoking_blank_with_prior_never_candidate',is.na(c$OC117)&pr$OZ205 %in% 5)
  add('smoking_current_yes_with_prior_never_review',c$OC117 %in% 1 & pr$OZ205 %in% 5)
  for(field in c('OI834','OI841')) add(paste0(field,'_raw_nonmissing_not_yet_cleaned'),!is.na(pm[[field]]))
  add('physical_measure_weight_missing_or_nonpositive',is.na(z$OPMWGTR)|z$OPMWGTR<=0)
  add('physical_measure_weight_missing',is.na(z$OPMWGTR))
  add('physical_measure_weight_zero',z$OPMWGTR==0)
  add('physical_measure_weight_negative',z$OPMWGTR<0)
  add('design_strata_or_cluster_missing',is.na(z$STRATUM)|is.na(z$SECU))
  out <- diagnostic_report(counts)
  write.csv(out,file.path(args[2],'stage2_counts.csv'),row.names=FALSE)
  writeLines(c('SUCCESS: technical and aggregate Stage2 diagnostics refreshed.',
    'Counts rounded to nearest 10, all counts below 10 suppressed including zero.',
    'Flow is provisional: ages above 120 held for review; overlap cases not remapped.',
    'Timing uses INTERVIEW MONTH proxies, not confirmed physical-examination dates.',
    'Timing flags are overlapping diagnostics, NOT final event labels or exclusive groups.',
    'endpoint_candidate categories are exclusive but conditional on the unverified interview-month proxy.',
    'Horizon comparisons are conservative month/quarter interval comparisons.',
    'No NDI record is not proof of survival; non-NDI deaths still require adjudication.',
    'Known death conflicts compare non-imputed known-deceased dates with NDI intervals.',
    'Covariate denominator is flow_06, before outcome-based exclusions.',
    'Smoking is reconstructed with C/PR routing; unresolved and contradictory cases remain separate.',
    'Stroke code 2 is possible stroke/TIA, not invalid and not silently combined with confirmed stroke.',
    'Height and weight coverage is RAW nonmissing, not valid BMI.',
    'Further codebook checks, detailed source adjudication and overlap review remain.',
    'No models fitted, no person-level outputs saved. Review before sharing.'),note)
  message('Stage2 aggregate reports written. No person-level outputs saved.')
  invisible(list(core=core,tracker=t))
}

if(sys.nframe()==0L) {
  script <- sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  stage2_main(commandArgs(trailingOnly=TRUE),file.path(dirname(normalizePath(script)),'hrs_feasibility.R'))
}
