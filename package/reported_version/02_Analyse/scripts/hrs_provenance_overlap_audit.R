# Local user-run audit of HRS input provenance and historical Tracker overlaps.
# Reads source files. Writes identifier-level review only under HRS_private.
# No model fitting, network access, or changes to source files.
# Usage: Rscript hrs_provenance_overlap_audit.R PRIVATE_DATA_DIR PRIVATE_RESULTS_DIR

protected_counts <- function(counts) {
  metric_names <- names(counts)
  counts <- as.numeric(counts)
  names(counts) <- metric_names
  if (any(!is.finite(counts) | counts<0)) stop('Counts must be finite and nonnegative.')
  value <- ifelse(counts==0,'0',
    ifelse(counts<10,'SUPPRESSED_BLOCK',as.character(10*floor(counts/10+0.5))))
  # A small nonzero cell suppresses the whole small table to prevent recovery
  # by subtraction from a known total.
  if (any(counts>0 & counts<10)) value[] <- 'SUPPRESSED_BLOCK'
  data.frame(metric=names(counts),value=value,stringsAsFactors=FALSE)
}

release_headers <- function(files) {
  rows <- list()
  for (f in files) {
    if (!file.exists(f)) next
    lines <- tryCatch(readLines(f,n=40,warn=FALSE),error=function(e) character())
    evidence <- lines[grepl('HRS|release|version|wave',lines,ignore.case=TRUE)]
    if (!length(evidence)) next
    rows[[length(rows)+1L]] <- data.frame(file=basename(f),
      header_evidence=paste(trimws(evidence),collapse=' | '),stringsAsFactors=FALSE)
  }
  if (!length(rows)) return(data.frame(file=character(),header_evidence=character()))
  do.call(rbind,rows)
}

overlap_checks <- function(t,w10,w14) {
  required <- c('HHID','PN','OVHHID','OVPN','OVRESULT','OAGE')
  if (!all(required %in% names(t)) || !all(c('key','valid_trials') %in% names(w10)) ||
      !all(c('key','valid_trials') %in% names(w14))) stop('Required overlap fields are absent.')
  pad <- function(x,width,allow_na=FALSE) {
    x <- trimws(as.character(x)); x[x==''] <- NA_character_
    valid <- !is.na(x) & grepl(paste0('^[0-9]{1,',width,'}$'),x)
    if ((!allow_na && any(!valid)) || (allow_na && any(!is.na(x) & !valid)))
      stop('Malformed HRS person or household identifier.')
    x[valid] <- paste0(vapply(width-nchar(x[valid]),
      function(n) paste(rep('0',n),collapse=''),''),x[valid])
    x
  }
  hh <- pad(t$HHID,6); pn <- pad(t$PN,3)
  old_hh <- pad(t$OVHHID,6,allow_na=TRUE); old_pn <- pad(t$OVPN,3,allow_na=TRUE)
  if (anyNA(hh) || anyNA(pn) || anyDuplicated(paste(hh,pn,sep=':')))
    stop('Current Tracker identifiers must be complete and unique.')
  code <- suppressWarnings(as.numeric(as.character(t$OVRESULT)))
  if (any(!is.na(t$OVRESULT) & !code %in% 0:3)) stop('Unexpected OVRESULT code.')
  current <- paste(hh,pn,sep=':')
  old <- ifelse(!is.na(old_hh)&old_hh!='000000'&!is.na(old_pn)&old_pn!='000',
                paste(old_hh,old_pn,sep=':'),NA_character_)
  overlap <- !is.na(old_hh)&old_hh!='000000'
  valid10 <- w10$key[w10$valid_trials>0]
  valid14 <- w14$key[w14$valid_trials>0]
  data <- data.frame(current_key=current,old_key=old,overlap=overlap,
    current_has_prior_grip=current %in% valid10,
    current_has_current_grip=current %in% valid14,
    old_has_prior_grip=!is.na(old)&old %in% valid10,
    old_has_current_grip=!is.na(old)&old %in% valid14,
    old_id_in_tracker=ifelse(!is.na(old),old %in% current,FALSE),
    stringsAsFactors=FALSE)
  eligible_age <- !is.na(t$OAGE) & suppressWarnings(as.numeric(as.character(t$OAGE)))>=50 &
    suppressWarnings(as.numeric(as.character(t$OAGE)))<=120
  eligible_age[is.na(eligible_age)] <- FALSE
  c(flagged_before_current_link_rule=sum(overlap),
    old_id_has_prior_grip_new_id_has_current_grip=sum(overlap & data$old_has_prior_grip &
      data$current_has_current_grip),
    old_id_absent_from_tracker=sum(overlap & !is.na(old) & !data$old_id_in_tracker),
    flagged_in_existing_pre_overlap_sample=sum(overlap & data$current_has_prior_grip &
      data$current_has_current_grip & eligible_age))
}

audit_main <- function(args, script_dir) {
  if (length(args) != 2L || !dir.exists(args[1]) || !dir.exists(args[2])) {
    stop('Usage: Rscript hrs_release_overlap_audit.R PRIVATE_DATA_DIR PRIVATE_RESULTS_DIR')
  }
  data_dir <- normalizePath(args[1], mustWork=TRUE)
  results_dir <- normalizePath(args[2], mustWork=TRUE)
  private_root <- dirname(data_dir)
  if (!startsWith(results_dir, paste0(private_root, .Platform$file.sep)) ||
      results_dir == data_dir) {
    stop('Choose a results directory inside the same private HRS root as PRIVATE_DATA_DIR.')
  }

  source(file.path(script_dir, 'hrs_feasibility.R'), local=TRUE)
  source(file.path(script_dir, 'hrs_stage2.R'), local=TRUE)

  paths <- list.files(data_dir, recursive=TRUE, full.names=TRUE)
  locate <- function(name) {
    hit <- paths[tolower(basename(paths)) == tolower(name)]
    if (length(hit) != 1L) stop('Required input missing or duplicated: ', name)
    hit
  }
  specs <- list(
    h10i_r=c('HHID','PN',paste0('MI',c('816','851','852','853','817','818'))),
    h14i_r=c('HHID','PN',paste0('OI',c('816','851','852','853','817','818','834','841'))),
    h14a_r=c('HHID','PN','OA500','OA501'),
    h14c_r=c('HHID','PN','OC001','OC005','OC010','OC018','OC030','OC036',
             'OC053','OC116','OC117'),
    h14pr_r=c('HHID','PN','OZ076','OZ205')
  )
  inputs <- list()
  for (name in names(specs)) {
    inputs[[paste0(name,'_data')]] <- locate(paste0(name,'.da'))
    inputs[[paste0(name,'_layout')]] <- locate(paste0(name,'.sas'))
  }
  tracker_path <- locate('trk2022tr_r.csv')
  inputs[['tracker_csv']] <- tracker_path

  run_name <- paste0('release_overlap_audit_',format(Sys.time(),'%Y%m%d_%H%M%S'),
                     '_',Sys.getpid())
  out <- file.path(results_dir,run_name)
  if (file.exists(out)) stop('Output folder already exists. Run again to create a new timestamped folder.')
  dir.create(out)

  shasum <- Sys.which('shasum')
  release_rows <- lapply(names(inputs),function(role) {
    f <- inputs[[role]]
    info <- file.info(f)
    sha <- NA_character_
    if (nzchar(shasum)) {
      hash_out <- tryCatch(system2(shasum,c('-a','256',shQuote(f)),stdout=TRUE,stderr=FALSE),
                           error=function(e) character())
      if (length(hash_out) && grepl('^[[:xdigit:]]{64}',hash_out[1]))
        sha <- sub('^([[:xdigit:]]{64}).*$','\\1',hash_out[1])
    }
    data.frame(role=role,file=basename(f),bytes=info$size,
      filesystem_mtime=format(info$mtime,tz='UTC',usetz=TRUE),sha256=sha,
      stringsAsFactors=FALSE)
  })
  release <- do.call(rbind,release_rows)
  write.csv(release,file.path(out,'source_file_manifest.csv'),row.names=FALSE,na='')
  template <- data.frame(role=names(inputs),file=basename(unlist(inputs)),
    confirmed_release='',access_date='',access_date_source='',
    stringsAsFactors=FALSE)
  write.csv(template,file.path(out,'release_access_confirmation_template.csv'),
            row.names=FALSE,na='')
  headers <- release_headers(unlist(inputs[grepl('_layout$',names(inputs))]))
  write.csv(headers,file.path(out,'release_header_evidence.csv'),row.names=FALSE)

  tracker_audit <- read_tracker_csv(tracker_path)
  if (!tracker_audit$ok) {
    write.csv(tracker_audit$report,file.path(out,'tracker_import_audit.csv'),row.names=FALSE)
    stop('Tracker import check failed. Review tracker_import_audit.csv locally.')
  }
  # Read only named fields needed for the overlap review, retaining identifiers locally.
  tracker_raw <- suppressWarnings(read.csv(tracker_path,colClasses='character',
    check.names=FALSE,na.strings=c('','NA'),strip.white=TRUE,fill=FALSE,row.names=NULL))
  names(tracker_raw) <- toupper(names(tracker_raw))
  wanted_tracker <- c('HHID','PN','OVHHID','OVPN','OVYEAR','OVRESULT','OAGE')
  if (anyDuplicated(names(tracker_raw)) || !all(wanted_tracker %in% names(tracker_raw)))
    stop('Tracker overlap fields are missing or duplicated.')
  tr <- tracker_audit$data
  if (nrow(tracker_raw)!=nrow(tr)) stop('Tracker readers returned different row counts.')
  raw <- tracker_raw[wanted_tracker]
  raw$HHID <- tr$HHID
  raw$PN <- tr$PN
  raw$current_key <- tr$key
  normalize_id <- function(x,width) {
    x <- trimws(as.character(x)); x[x==''] <- NA_character_
    ok <- !is.na(x) & grepl(paste0('^[0-9]{1,',width,'}$'),x)
    if (any(!is.na(x) & !ok)) stop('Malformed historical Tracker identifier.')
    x[ok] <- paste0(vapply(width-nchar(x[ok]),
      function(n) paste(rep('0',n),collapse=''),''),x[ok])
    x
  }
  raw$OVHHID <- normalize_id(raw$OVHHID,6L)
  raw$OVPN <- normalize_id(raw$OVPN,3L)
  raw$old_key <- ifelse(!is.na(raw$OVHHID) & raw$OVHHID!='000000' &
                          !is.na(raw$OVPN) & raw$OVPN!='000',
                        paste(raw$OVHHID,raw$OVPN,sep=':'),NA_character_)
  tracker_duplicates <- duplicated(tr$key) | duplicated(tr$key,fromLast=TRUE)
  if (any(tracker_duplicates)) stop('Duplicate current Tracker identifiers. No overlap report was written.')

  core <- lapply(names(specs),function(name) {
    read_hrs(inputs[[paste0(name,'_data')]],inputs[[paste0(name,'_layout')]],specs[[name]])
  })
  names(core) <- names(specs)
  grip10 <- wave_summary(core$h10i_r,'M')
  grip14 <- wave_summary(core$h14i_r,'O')
  all_overlap_counts <- overlap_checks(raw,grip10,grip14)
  current_keys <- intersect(grip10$key[grip10$valid_trials>0],
                            grip14$key[grip14$valid_trials>0])
  current_keys <- current_keys[current_keys %in% tr$key]
  tidx <- match(current_keys,tr$key)
  age <- suppressWarnings(as.numeric(raw$OAGE[tidx]))
  eligible_age <- !is.na(age) & age==floor(age) & age>=50 & age<=120
  current_keys <- current_keys[eligible_age]
  tidx <- tidx[eligible_age]
  overlap <- raw[tidx,,drop=FALSE]
  flagged <- !is.na(overlap$OVHHID) & overlap$OVHHID!='000000'
  review <- overlap[flagged,,drop=FALSE]

  valid10 <- grip10$key[grip10$valid_trials>0]
  valid14 <- grip14$key[grip14$valid_trials>0]
  tracker_keys <- tr$key
  review$current_in_2010_core <- review$current_key %in% core$h10i_r$key
  review$current_in_2010_with_grip <- review$current_key %in% valid10
  review$current_in_2014_core <- review$current_key %in% core$h14i_r$key
  review$current_in_2014_with_grip <- review$current_key %in% valid14
  review$old_id_in_2010_core <- !is.na(review$old_key) & review$old_key %in% core$h10i_r$key
  review$old_id_in_2010_with_grip <- !is.na(review$old_key) & review$old_key %in% valid10
  review$old_id_in_2014_core <- !is.na(review$old_key) & review$old_key %in% core$h14i_r$key
  review$old_id_in_2014_with_grip <- !is.na(review$old_key) & review$old_key %in% valid14
  review$old_id_in_tracker <- !is.na(review$old_key) & review$old_key %in% tracker_keys
  review$current_equals_old_id <- !is.na(review$old_key) & review$current_key==review$old_key
  old_counts <- table(review$old_key,useNA='no')
  review$multiple_current_rows_for_old_id <- !is.na(review$old_key) &
    as.integer(old_counts[review$old_key])>1L
  review$review_interpretation <- 'Identifier crosswalk only; HRS-specific adjudication required'
  local_file <- file.path(out,'OVERLAP_REVIEW_LOCAL_ONLY.csv')
  write.csv(review,local_file,row.names=FALSE,na='')

  overlap_type <- ifelse(review$OVRESULT=='1','HRS_AHEAD_overlap_appeared',
    ifelse(review$OVRESULT=='2','HRS_AHEAD_overlap_not_appeared',
      ifelse(review$OVRESULT=='3','interrespondent_household_merge','other_or_missing_code')))
  overlap_type[is.na(overlap_type)] <- 'other_or_missing_code'
  summary_values <- c(
    eligible_paired_grip_participants_age_50_120_before_overlap_rule=length(current_keys),
    flagged_overlap_rows=nrow(review),
    flagged_rows_with_missing_old_person_number=sum(is.na(review$old_key)),
    flagged_rows_with_old_id_matching_any_tracker_record=sum(review$old_id_in_tracker),
    flagged_rows_where_current_and_old_ids_match=sum(review$current_equals_old_id),
    flagged_rows_with_multiple_current_rows_for_old_id=sum(review$multiple_current_rows_for_old_id),
    current_ids_present_in_2010_core=sum(review$current_in_2010_core),
    current_ids_with_valid_2010_grip=sum(review$current_in_2010_with_grip),
    current_ids_present_in_2014_core=sum(review$current_in_2014_core),
    current_ids_with_valid_2014_grip=sum(review$current_in_2014_with_grip),
    old_ids_present_in_2010_core=sum(review$old_id_in_2010_core),
    old_ids_with_valid_2010_grip=sum(review$old_id_in_2010_with_grip),
    old_ids_present_in_2014_core=sum(review$old_id_in_2014_core),
    old_ids_with_valid_2014_grip=sum(review$old_id_in_2014_with_grip))
  type_counts <- table(factor(overlap_type,levels=c('HRS_AHEAD_overlap_appeared',
    'HRS_AHEAD_overlap_not_appeared','interrespondent_household_merge','other_or_missing_code')))
  summary_values <- c(summary_values,setNames(as.numeric(type_counts),
    paste0('overlap_type_',names(type_counts))))
  names(all_overlap_counts) <- paste0('all_tracker_',names(all_overlap_counts))
  summary_values <- c(summary_values,all_overlap_counts)
  summary <- protected_counts(summary_values)
  names(summary) <- c('measure','count_rounded_10')
  write.csv(summary,file.path(out,'overlap_review_summary.csv'),row.names=FALSE)
  versions <- sort(unique(tr$VERSION))
  writeLines(c(
    'SUCCESS: local source inventory and overlap identifier crosswalk completed.',
    paste0('Tracker VERSION values read from the named CSV: ',paste(versions,collapse=', '),'.'),
    'Core release names, UTC filesystem modification times and SHA-256 hashes are in source_file_manifest.csv.',
    'Filesystem timestamps do not establish download or access dates.',
    'Complete release_access_confirmation_template.csv from retained HRS download records.',
    'OVERLAP_REVIEW_LOCAL_ONLY.csv contains direct identifiers and must stay in the private HRS directory.',
    'Share overlap_review_summary.csv only. Counts below ten are suppressed and others rounded to ten.',
    'Rows named all_tracker_* describe the full Tracker crosswalk; other overlap rows describe the matched-grip, age 50-120 review sample.',
    'The identifier crosswalk does not decide which person records should be linked or excluded.',
    'No rows were remapped, included, excluded or used to refit models.'),
    file.path(out,'READ_ME.txt'))
  message('Audit complete. Aggregate summary: ',file.path(out,'overlap_review_summary.csv'))
  message('Keep OVERLAP_REVIEW_LOCAL_ONLY.csv inside the private HRS directory.')
  invisible(out)
}

if (sys.nframe()==0L) {
  script <- sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  audit_main(commandArgs(trailingOnly=TRUE),dirname(normalizePath(script)))
}
