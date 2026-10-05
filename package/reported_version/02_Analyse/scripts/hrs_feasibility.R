# HRS feasibility stage 1. Run manually in a local R session, outside AI tools.
# No packages, network, model fitting, or person-level output.
# Usage: Rscript hrs_feasibility.R /path/to/private/HRS /path/to/private/results

hrs_layout <- function(sas_file, wanted) {
  setup <- readLines(sas_file, warn = FALSE)
  start <- grep('^\\s*INPUT\\s*$', setup, ignore.case = TRUE)
  if (length(start) != 1L) stop('Unrecognized SAS INPUT definition.')
  tail <- setup[(start + 1L):length(setup)]
  end <- grep(';', tail, fixed = TRUE)[1]
  if (is.na(end)) stop('Unterminated SAS INPUT definition.')
  block <- tail[seq_len(end)]
  pattern <- '^\\s*([A-Za-z][A-Za-z0-9_]*)\\s+(\\$\\s*)?([0-9]+)\\s*-\\s*([0-9]+)\\s*;?\\s*$'
  fields <- regmatches(block, regexec(pattern, block))
  fields <- fields[lengths(fields) == 5L]
  if (!length(fields)) stop('No supported fixed-width definitions.')
  meta <- do.call(rbind, fields)
  names <- toupper(meta[, 2])
  if (anyDuplicated(names)) stop('Duplicate variable definitions.')
  if (!all(wanted %in% names)) stop('Required variables absent from SAS metadata.')
  data.frame(name = names, character = nzchar(trimws(meta[, 3])),
             start = as.integer(meta[, 4]), end = as.integer(meta[, 5]))
}

read_hrs <- function(data_file, sas_file, wanted) {
  layout <- hrs_layout(sas_file, wanted)
  pos <- match(wanted, layout$name)
  records <- readLines(data_file, warn = FALSE)
  if (any(nchar(records, type = 'chars') < max(layout$end[pos]))) {
    stop('Records shorter than required layout; no results exported.')
  }
  out <- lapply(pos, function(i) {
    value <- trimws(substr(records, layout$start[i], layout$end[i]))
    value[value == ''] <- NA_character_
    if (layout$character[i]) return(value)
    number <- suppressWarnings(as.numeric(value))
    if (any(!is.na(value) & is.na(number))) stop('Non-numeric value in numeric field.')
    number
  })
  out <- as.data.frame(setNames(out, wanted), stringsAsFactors = FALSE)
  if (anyNA(out[c('HHID', 'PN')])) stop('Missing identifiers.')
  if (any(!grepl('^[0-9]{6}$', out$HHID)) ||
      any(!grepl('^[0-9]{3}$', out$PN))) stop('Unexpected identifier format.')
  out$key <- paste(out$HHID, out$PN, sep = ':')
  if (anyDuplicated(out$key)) stop('Duplicate identifiers; resolve locally.')
  out
}

clean_grip <- function(x) {
  # Preliminary plausibility rule, not a finalized exclusion criterion.
  # Zero and >100 kg flagged; 993/998/999 are nonmeasurement codes.
  x[!is.finite(x) | x <= 0 | x > 100] <- NA_real_
  x
}

wave_summary <- function(x, prefix) {
  vars <- paste0(prefix, c('I816', 'I851', 'I852', 'I853'))
  raw <- as.matrix(x[vars])
  clean <- clean_grip(raw)
  n <- rowSums(!is.na(clean))
  peak <- apply(clean, 1, function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE))
  data.frame(key = x$key, valid_trials = n, grip_max = peak,
             full_effort = x[[paste0(prefix, 'I817')]] == 1,
             flagged_trials = rowSums(!is.na(raw) & is.na(clean)))
}

feasibility_counts <- function(a, b, tracker) {
  wa <- wave_summary(a, 'M'); wb <- wave_summary(b, 'O')
  joined <- merge(wa, wb, by = 'key', suffixes = c('_2010', '_2014'))
  paired <- joined[joined$valid_trials_2010 > 0 & joined$valid_trials_2014 > 0, ]
  k <- match(paired$key, tracker$key)
  t <- tracker[k, , drop = FALSE]
  year <- t$NYEAR
  # Coverage counts only. These are NOT the final five-year outcome labels.
  c(core_2010_rows = nrow(a), core_2014_rows = nrow(b),
    grip_2010_available = sum(wa$valid_trials > 0),
    grip_2014_available = sum(wb$valid_trials > 0),
    shared_ids = nrow(joined), valid_grip_pairs = nrow(paired),
    pairs_all_four_trials_both_waves = sum(paired$valid_trials_2010 == 4 & paired$valid_trials_2014 == 4),
    pairs_full_effort_both_waves = sum(paired$full_effort_2010 & paired$full_effort_2014, na.rm = TRUE),
    tracker_matched_pairs = sum(!is.na(k)),
    ndi_year_recorded_pairs = sum(!is.na(year)),
    ndi_year_2015_2019_pairs = sum(year >= 2015 & year <= 2019, na.rm = TRUE),
    ndi_year_at_or_before_2014_requires_review = sum(year <= 2014, na.rm = TRUE),
    ndi_quarter_missing_or_invalid = sum(!is.na(year) & !(t$NMONTH %in% 1:4)),
    ndi_and_interview_year_disagree = sum(!is.na(year) & !is.na(t$EXDEATHYR) & year != t$EXDEATHYR),
    last_alive_year_missing_pairs = sum(is.na(t$LASTALIVEYR)),
    overlap_mapping_requires_review = sum(!is.na(t$OVHHID) & t$OVHHID != '000000'),
    flagged_grip_trials_2010 = sum(wa$flagged_trials),
    flagged_grip_trials_2014 = sum(wb$flagged_trials))
}

public_report <- function(counts) {
  # Release only four broad counts. Do not export diagnostic or complementary cells.
  metrics <- c('grip_2010_available', 'grip_2014_available',
               'valid_grip_pairs', 'ndi_year_2015_2019_pairs')
  values <- counts[metrics]
  if (anyNA(values) || any(!is.finite(values)) || any(values < 0)) {
    stop('Invalid main report counts.')
  }
  shown <- ifelse(values < 10, 'SUPPRESSED',
                  as.character(10 * floor(values / 10 + 0.5)))
  data.frame(metric = metrics, count_rounded_10 = unname(shown))
}

mortality_diagnostics <- function(pair_keys, tracker) {
  plausible <- function(x) !is.na(x) & is.finite(x) & x == floor(x) & x >= 1900 & x <= 2023
  in_window <- function(x) plausible(x) & x >= 2015 & x <= 2019
  k <- match(pair_keys, tracker$key)
  matched <- tracker[k[!is.na(k)], , drop = FALSE]
  c(tracker_rows = nrow(tracker), tracker_matched_pairs = sum(!is.na(k)),
    tracker_ndi_year_nonmissing = sum(!is.na(tracker$NYEAR)),
    tracker_ndi_year_plausible = sum(plausible(tracker$NYEAR)),
    tracker_ndi_quarter_present = sum(tracker$NMONTH %in% 1:4),
    tracker_ndi_score_present = sum(is.finite(tracker$NSCORE) & tracker$NSCORE > 0, na.rm = TRUE),
    tracker_exit_year_invalid = sum(!is.na(tracker$EXDEATHYR) & !plausible(tracker$EXDEATHYR)),
    pairs_ndi_year_plausible = sum(plausible(matched$NYEAR)),
    pairs_ndi_year_2015_2019 = sum(in_window(matched$NYEAR)),
    pairs_exit_year_plausible = sum(plausible(matched$EXDEATHYR)),
    pairs_exit_year_2015_2019 = sum(in_window(matched$EXDEATHYR)),
    pairs_known_deceased_year_plausible = sum(plausible(matched$KNOWNDECEASEDYR)),
    pairs_known_deceased_year_2015_2019 = sum(in_window(matched$KNOWNDECEASEDYR)),
    pairs_last_alive_2019_or_later = sum(is.finite(matched$LASTALIVEYR) &
      matched$LASTALIVEYR >= 2019 & matched$LASTALIVEYR <= 2026 &
      matched$LASTALIVEYR == floor(matched$LASTALIVEYR), na.rm = TRUE))
}

diagnostic_report <- function(counts) {
  if (anyNA(counts) || any(!is.finite(counts)) || any(counts < 0)) {
    stop('Invalid diagnostic counts.')
  }
  data.frame(metric = names(counts), count_rounded_10 = unname(ifelse(
    counts < 10, 'SUPPRESSED', as.character(10 * floor(counts / 10 + 0.5)))))
}

audit_import <- function(data_file, sas_file, wanted, sps_file = NULL) {
  checks <- character()
  add <- function(name, value) checks[name] <<- as.character(value)
  fail <- function() list(ok = FALSE, report = data.frame(check=names(checks), value=unname(checks)))
  layout <- tryCatch(hrs_layout(sas_file, wanted), error=function(e) NULL)
  if (is.null(layout)) {
    add('SAS_layout_parse', 'FAIL'); return(fail())
  }
  add('SAS_layout_parse', 'PASS')
  setup <- readLines(sas_file, warn=FALSE)
  hits <- regmatches(setup, regexec('LRECL\\s*=\\s*([0-9]+)', setup, ignore.case=TRUE))
  hits <- hits[lengths(hits)==2L]
  if (length(hits)!=1L) {
    add('SAS_LRECL', 'UNRECOGNIZED'); return(fail())
  }
  lrecl <- as.integer(hits[[1]][2])
  add('SAS_LRECL', lrecl)
  add('SAS_last_defined_column', max(layout$end))
  valid_layout <- all(layout$start >= 1 & layout$end >= layout$start) &&
    all(layout$end <= lrecl) &&
    all(layout$start[-1] > head(layout$end, -1))
  add('SAS_column_boundaries', if (valid_layout) 'PASS' else 'FAIL')
  if (!valid_layout) return(fail())
  selected <- layout[layout$name %in% wanted, ]
  for (i in seq_len(nrow(selected))) {
    add(paste0('SAS_columns_',selected$name[i]),paste(selected$start[i],selected$end[i],sep='-'))
  }
  # Compare a second official import definition if it is present.
  # Both definitions could share an upstream error; agreement is not provenance validation.
  metadata_ok <- TRUE
  if (is.null(sps_file)) {
    add('SPSS_layout_agreement', 'NOT_AVAILABLE')
  } else {
    txt <- readLines(sps_file, warn=FALSE)
    pattern <- '^\\s*([A-Za-z][A-Za-z0-9_]*)\\s+([0-9]+)\\s*-\\s*([0-9]+)(.*)$'
    h <- regmatches(txt,regexec(pattern,txt))
    h <- h[lengths(h)==5L]
    if (!length(h)) {
      add('SPSS_layout_agreement','UNRECOGNIZED'); metadata_ok <- FALSE
    } else {
      z <- do.call(rbind,h); zn <- toupper(z[,2]); j <- match(selected$name,zn)
      metadata_ok <- !anyNA(j) && !anyDuplicated(zn) &&
        all(as.integer(z[j,3])==selected$start) && all(as.integer(z[j,4])==selected$end) &&
        all(grepl('\\(A\\)', z[j,5], ignore.case=TRUE)==selected$character)
      add('SPSS_layout_agreement',if (metadata_ok) 'PASS' else 'FAIL')
    }
  }
  lines <- readLines(data_file, warn=FALSE)
  if (!length(lines)) { add('nonempty_file','FAIL'); return(fail()) }
  add('nonempty_file','PASS')
  bytes <- nchar(lines,type='bytes'); chars <- nchar(lines,type='chars')
  add('record_bytes_min',min(bytes)); add('record_bytes_max',max(bytes))
  length_ok <- all(bytes==lrecl)
  add('record_length_matches_LRECL',if(length_ok) 'PASS' else 'FAIL')
  ascii_ok <- all(bytes==chars)
  add('byte_character_width_agreement',if(ascii_ok) 'PASS' else 'FAIL')
  # Structural diagnostics only: no values, identifiers, row indices or counts.
  # A uniform suffix does not establish that the preceding fields are aligned.
  status <- function(x) {
    if (!length(x)) 'NOT_APPLICABLE' else if (all(x)) 'ALL' else if (any(x)) 'MIXED' else 'NONE'
  }
  add('record_excess_bytes_min',min(bytes-lrecl))
  add('record_excess_bytes_max',max(bytes-lrecl))
  add('records_containing_tabs',status(grepl('\t',lines,fixed=TRUE)))
  add('records_containing_control_characters',status(grepl('[[:cntrl:]]',lines)))
  if (ascii_ok) {
    suffix <- substring(lines[bytes>lrecl],lrecl+1L)
    add('extra_suffix_spaces_only',status(grepl('^ +$',suffix)))
    add('extra_suffix_controls_only',status(grepl('^[[:cntrl:]]+$',suffix)))
    add('extra_suffix_whitespace_only',status(grepl('^[[:space:]]+$',suffix)))
    extra <- bytes-lrecl
    prefix <- substring(lines[extra>0],1L,extra[extra>0])
    add('extra_width_prefix_spaces_only',status(grepl('^ +$',prefix)))
  } else {
    add('suffix_and_prefix_checks','SKIPPED_NON_SINGLE_BYTE_WIDTH')
  }
  if (!length_ok || !ascii_ok || !metadata_ok) return(fail())
  # Separate base-R fixed-width reader, retaining all fields as strings first.
  widths <- integer(); previous <- 0L
  for(i in seq_len(nrow(selected))) {
    gap <- selected$start[i]-previous-1L
    if(gap>0) widths <- c(widths,-gap)
    widths <- c(widths,selected$end[i]-selected$start[i]+1L)
    previous <- selected$end[i]
  }
  independent <- tryCatch(suppressWarnings(read.fwf(data_file,widths=widths,
    col.names=selected$name,colClasses='character',strip.white=TRUE,
    comment.char='',blank.lines.skip=FALSE, na.strings=c('','NA'))),
    error=function(e) NULL)
  current <- tryCatch(read_hrs(data_file,sas_file,wanted),error=function(e) NULL)
  if(is.null(independent) || is.null(current)) {
    add('independent_reader_agreement','READER_ERROR'); return(fail())
  }
  same <- nrow(current)==nrow(independent)
  for(i in seq_len(nrow(selected))) {
    v <- trimws(independent[[selected$name[i]]]); v[v==''] <- NA_character_
    if(!selected$character[i]) {
      n <- suppressWarnings(as.numeric(v))
      if(any(!is.na(v) & is.na(n))) same <- FALSE
      v <- n
    }
    same <- same && identical(unname(current[[selected$name[i]]]),unname(v))
  }
  add('independent_reader_agreement',if(same) 'PASS' else 'FAIL')
  # No field contents, IDs, discrepant rows or detailed errors leave this function.
  list(ok=same,report=data.frame(check=names(checks),value=unname(checks)))
}

read_tracker_csv <- function(path) {
  checks <- c(tracker_source='NAMED_CSV')
  add <- function(k,ok) checks[k] <<- if(ok) 'PASS' else 'FAIL'
  result <- function(data=NULL) list(ok=!any(checks=='FAIL'),data=data,
    report=data.frame(check=names(checks),value=unname(checks)))
  x <- tryCatch(suppressWarnings(read.csv(path,colClasses='character',
    check.names=FALSE,na.strings=c('','NA'),strip.white=TRUE,row.names=NULL,
    fill=FALSE)),error=function(e) NULL)
  add('CSV_readable',!is.null(x))
  if(is.null(x)) return(result())
  names(x) <- toupper(names(x))
  required <- c('HHID','PN','OVHHID','NYEAR','NMONTH','NSCORE','EXDEATHYR',
                'KNOWNDECEASEDYR','LASTALIVEYR','VERSION')
  add('CSV_required_unique_columns',!anyDuplicated(names(x)) && all(required %in% names(x)))
  add('CSV_nonempty',nrow(x)>0)
  if(any(checks=='FAIL')) return(result())
  x <- x[required]
  x[] <- lapply(x,function(v) { v <- trimws(v); v[v==''] <- NA_character_; v })
  for(field in c('HHID','PN','OVHHID')) {
    width <- if(field=='PN') 3L else 6L
    v <- x[[field]]
    valid <- !anyNA(v) && all(grepl('^[0-9]+$',v)) && all(nchar(v)<=width)
    add(paste0(field,'_format'),valid)
    if(valid) x[[field]] <- paste0(vapply(width-nchar(v),
      function(n) paste(rep('0',n),collapse=''),''),v)
  }
  for(field in setdiff(required,c('HHID','PN','OVHHID'))) {
    v <- x[[field]]; n <- suppressWarnings(as.numeric(v))
    valid <- all(is.na(v) | (is.finite(n) & !is.na(n)))
    add(paste0(field,'_numeric'),valid)
    x[[field]] <- n
  }
  if(any(checks=='FAIL')) return(result())
  x$key <- paste(x$HHID,x$PN,sep=':')
  add('CSV_unique_person_keys',!anyDuplicated(x$key))
  add('VERSION_is_1',!anyNA(x$VERSION) && all(x$VERSION==1))
  # Conservative review gates, not silent recodes or finalized exclusions.
  for(field in c('NYEAR','EXDEATHYR','KNOWNDECEASEDYR')) {
    v <- x[[field]]
    add(paste0(field,'_range'),all(is.na(v) | (v==floor(v) & v>=1900 & v<=2023)))
  }
  # Public Tracker codebook lists LASTALIVEYR separately as 1992-2026.
  v <- x$LASTALIVEYR
  add('LASTALIVEYR_range',all(is.na(v) | (v==floor(v) & v>=1992 & v<=2026)))
  add('NMONTH_quarter_range',all(is.na(x$NMONTH) | x$NMONTH %in% 1:4))
  add('NSCORE_release_range',all(is.na(x$NSCORE) |
    (x$NSCORE==floor(x$NSCORE) & x$NSCORE>=22 & x$NSCORE<=105)))
  if(any(checks=='FAIL')) return(result())
  result(x)
}

main <- function(args) {
  if (length(args) != 2L) stop('Usage: Rscript hrs_feasibility.R PRIVATE_DATA_DIR PRIVATE_OUTPUT_DIR')
  if (!dir.exists(args[1])) stop('Private data directory does not exist.')
  files <- list.files(args[1], recursive = TRUE, full.names = TRUE)
  locate <- function(pattern) {
    hits <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
    if (length(hits) != 1L) stop('Required file missing or duplicated: ', pattern,
                                '. Extract each distribution once; keep matching .da and .sas files.')
    hits
  }
  # Resolve all files before reading any microdata.
  paths <- lapply(c('^h10i_r\\.da$', '^h10i_r\\.sas$', '^h14i_r\\.da$',
                    '^h14i_r\\.sas$', '^trk2022tr_r\\.csv$'), locate)
  audit <- read_tracker_csv(paths[[5]])
  dir.create(args[2], recursive=TRUE, showWarnings=FALSE)
  write.csv(audit$report, file.path(args[2], 'import_audit.csv'), row.names=FALSE)
  if (!audit$ok) {
    writeLines('IMPORT AUDIT FAILED. Earlier feasibility and mortality CSV files are stale; do not interpret them.',
               file.path(args[2], 'READ_ME.txt'))
    stop('Import audit failed. Share only import_audit.csv; no raw records. Previous result CSVs were not refreshed.')
  }
  grip <- c('816', '851', '852', '853', '817')
  a <- read_hrs(paths[[1]], paths[[2]], c('HHID', 'PN', paste0('MI', grip)))
  b <- read_hrs(paths[[3]], paths[[4]], c('HHID', 'PN', paste0('OI', grip)))
  t <- audit$data
  counts <- feasibility_counts(a, b, t)
  report <- public_report(counts)
  wa <- wave_summary(a, 'M'); wb <- wave_summary(b, 'O')
  pair_keys <- intersect(wa$key[wa$valid_trials > 0], wb$key[wb$valid_trials > 0])
  diagnostics <- diagnostic_report(mortality_diagnostics(pair_keys, t))
  dir.create(args[2], recursive = TRUE, showWarnings = FALSE)
  write.csv(report, file.path(args[2], 'feasibility_counts.csv'), row.names = FALSE)
  write.csv(diagnostics, file.path(args[2], 'mortality_diagnostics.csv'), row.names = FALSE)
  writeLines(c('Stage 1 only: no survival model or final event labels.',
               'Tracker imported by CSV column names; 2010/2014 grip remains fixed-width.',
               'import_audit.csv checks tracker columns, identifiers, VERSION=1 and numeric ranges.',
               'Blank and NA tokens are missing; other unrecognized codes block import, without silent recoding.',
               'Range and version checks do not establish provenance or validate the final mortality endpoint.',
               'Four main counts rounded to nearest 10; all counts below 10 suppressed, including zero.',
               'Additional rounded coverage counts are in mortality_diagnostics.csv; no individual rows or cross-tabulations.',
               'Small counts including zero are suppressed; rounding is not formal privacy protection.',
               'Diagnostic plausible death years: integer 1900-2023 for this tracker release.',
               'KNOWNDECEASEDYR includes imputed dates; do not treat these as confirmed exact death dates.',
               'A last-alive year >=2019 does not by itself establish complete individual five-year follow-up.',
               'All-tracker totals include people outside our paired sample; death sources overlap and must not be added.',
               '2015-2019 is a calendar-year coverage count, not the final five-year mortality endpoint.',
               'Review overlaps, interview dates, censoring, missingness and plausibility locally.',
               'No NDI match must not be interpreted as alive.',
               'Manually review all outputs before sharing; no microdata or identifiers.'),
             file.path(args[2], 'READ_ME.txt'))
  message('Local aggregate report written. No person-level outputs saved.')
}
if (sys.nframe() == 0L) main(commandArgs(trailingOnly = TRUE))
