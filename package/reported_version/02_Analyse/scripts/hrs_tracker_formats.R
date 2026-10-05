# Run manually on the user's computer. Outputs technical comparisons only.
# Never repairs or writes person-level data. No model fitting or network access.
compare_tracker_formats <- function(ascii_file, sas_file, csv_file) {
  checks <- character()
  add <- function(k,v) checks[k] <<- as.character(v)
  result <- function() data.frame(check=names(checks),value=unname(checks))
  setup <- readLines(sas_file,warn=FALSE)
  begin <- which(grepl('^\\s*INPUT\\s*$',setup,ignore.case=TRUE))
  if(length(begin)!=1L) stop('Unrecognized INPUT definition.')
  ending <- which(seq_along(setup)>begin & grepl('^\\s*;',setup))[1]
  if(is.na(ending)) stop('Unrecognized INPUT terminator.')
  h <- regmatches(setup[(begin+1L):(ending-1L)],
    regexec('^\\s*([A-Za-z][A-Za-z0-9_]*)\\s+(\\$\\s*)?([0-9]+)\\s*-\\s*([0-9]+)',
            setup[(begin+1L):(ending-1L)]))
  h <- h[lengths(h)==5L]
  if(!length(h)) stop('No field definitions found.')
  z <- do.call(rbind,h)
  layout <- data.frame(name=toupper(z[,2]),character=grepl('\\$',z[,3]),
                       start=as.integer(z[,4]),end=as.integer(z[,5]))
  # CSV is a separately supplied named export, not a repair of the ASCII file.
  named <- read.csv(csv_file,colClasses='character',check.names=FALSE,
                    na.strings=c('','NA'),strip.white=TRUE)
  names(named) <- toupper(names(named))
  if(anyDuplicated(names(named)) || !all(c('HHID','PN') %in% names(named)))
    stop('CSV has missing or duplicated column names.')
  lines <- readLines(ascii_file,warn=FALSE)
  if(!length(lines) || !nrow(named)) stop('Empty input.')
  add('single_byte_records',if(all(nchar(lines,type='bytes')==nchar(lines))) 'PASS' else 'FAIL')
  if(tail(checks,1)!='PASS') return(result())
  ids <- layout[match(c('HHID','PN'),layout$name),]
  if(anyNA(ids$start)) stop('Missing ID definitions.')
  pad <- function(x,width) {
    x <- trimws(x)
    if(anyNA(x) || any(!grepl('^[0-9]+$',x)) || any(nchar(x)>width))
      stop('Invalid identifier structure; no values exported.')
    paste0(vapply(width-nchar(x),function(n) paste(rep('0',n),collapse=''),''),x)
  }
  akey <- paste(pad(substr(lines,ids$start[1],ids$end[1]),6),
                pad(substr(lines,ids$start[2],ids$end[2]),3),sep=':')
  ckey <- paste(pad(named$HHID,6),pad(named$PN,3),sep=':')
  same <- !anyDuplicated(akey) && !anyDuplicated(ckey) && setequal(akey,ckey)
  add('same_person_keys',if(same) 'PASS' else 'FAIL')
  if(!same) return(result())
  named <- named[match(akey,ckey),,drop=FALSE]
  fields <- intersect(c('EXDEATHYR','HHBASEWT','KNOWNDECEASEDYR','LASTALIVEYR',
                        'NMONTH','NSCORE','NYEAR','VERSION'),layout$name)
  for(field in fields) {
    if(!field %in% names(named)) { add(paste0(field,'_CSV_present'),'FAIL'); next }
    reference <- suppressWarnings(as.numeric(named[[field]]))
    raw <- trimws(named[[field]])
    if(any(!is.na(raw) & raw!='' & is.na(reference))) {
      add(paste0(field,'_CSV_numeric'),'FAIL'); next
    }
    if(all(is.na(reference))) { add(paste0(field,'_comparison'),'UNINFORMATIVE'); next }
    k <- match(field,layout$name)
    for(offset in 0:2) {
      raw <- trimws(substr(lines,layout$start[k]+offset,layout$end[k]+offset))
      raw[raw==''] <- NA_character_
      candidate <- suppressWarnings(as.numeric(raw))
      equal <- !any(!is.na(raw) & is.na(candidate)) &&
        identical(is.na(candidate),is.na(reference)) &&
        all(candidate[!is.na(reference)]==reference[!is.na(reference)])
      add(paste0(field,'_offset_',offset),if(equal) 'PASS' else 'FAIL')
    }
  }
  add('interpretation','DIAGNOSTIC_ONLY_NO_AUTOMATIC_REPAIR')
  result()
}

if(sys.nframe()==0L) {
  args <- commandArgs(trailingOnly=TRUE)
  if(length(args)!=2L) stop('Usage: Rscript hrs_tracker_formats.R DATA_DIR OUTPUT_DIR')
  files <- list.files(args[1],recursive=TRUE,full.names=TRUE)
  one <- function(name) {
    hit <- files[tolower(basename(files))==tolower(name)]
    if(length(hit)!=1L) stop('Required format file missing or duplicated.')
    hit
  }
  report <- compare_tracker_formats(one('TRK2022TR_R.da'),one('TRK2022TR_R.sas'),
                                    one('trk2022tr_r.csv'))
  dir.create(args[2],recursive=TRUE,showWarnings=FALSE)
  write.csv(report,file.path(args[2],'tracker_format_comparison.csv'),row.names=FALSE)
  cat('Technical report written: tracker_format_comparison.csv. No data modified.\n')
}
