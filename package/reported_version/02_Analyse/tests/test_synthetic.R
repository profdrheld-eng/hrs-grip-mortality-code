source('scripts/hrs_feasibility.R')
p <- tempfile(); s <- tempfile()
writeLines(c('INPUT', ' HHID $ 1 - 6', ' PN $ 7 - 9', ' MI816 10 - 13', ';'), s)
writeLines(c('00000101020.5', '000002020 993'), p)
x <- read_hrs(p, s, c('HHID','PN','MI816'))
stopifnot(identical(x$HHID, c('000001','000002')), x$MI816[1] == 20.5,
          is.na(clean_grip(x$MI816)[2]))
writeLines(c('00000101020.5', '00000101030.0'), p)
stopifnot(inherits(try(read_hrs(p,s,c('HHID','PN','MI816')), silent=TRUE), 'try-error'))
a <- data.frame(key=c('a','b'), MI816=c(20,993), MI851=c(22,999), MI852=c(21,998), MI853=c(23,0), MI817=c(1,2))
b <- a; names(b) <- sub('^M','O',names(b))
t <- data.frame(key='a', NYEAR=2017, NMONTH=2, EXDEATHYR=2017, LASTALIVEYR=2016, OVHHID='000000')
cnt <- feasibility_counts(a,b,t)
stopifnot(cnt['valid_grip_pairs']==1, cnt['ndi_year_2015_2019_pairs']==1,
          cnt['flagged_grip_trials_2010']==4)
cat('Synthetic parser, duplicate-ID, grip and merge checks passed.\n')
# Reporting must not suppress large main counts because a diagnostic cell is small.
report <- public_report(c(grip_2010_available=1234, grip_2014_available=1116,
                         valid_grip_pairs=1001, ndi_year_2015_2019_pairs=9,
                         overlap_mapping_requires_review=1))
stopifnot(nrow(report)==4L,
          identical(report$count_rounded_10, c('1230','1120','1000','SUPPRESSED')),
          !any(grepl('overlap', report$metric)))
for (small in c(0,1,9)) {
  z <- public_report(c(grip_2010_available=small, grip_2014_available=10,
                      valid_grip_pairs=10, ndi_year_2015_2019_pairs=10))
  stopifnot(z$count_rounded_10[1]=='SUPPRESSED', z$count_rounded_10[2]=='10')
}
cat('Synthetic rounded-report and small-cell checks passed.\n')
# An empty NDI field must not hide successful linkage and interview deaths.
d <- data.frame(key=c('a','b'), NYEAR=c(NA,NA), NMONTH=c(NA,NA),
                NSCORE=c(40,NA), EXDEATHYR=c(2017,9999),
                KNOWNDECEASEDYR=c(2017,2018), LASTALIVEYR=c(2016,2017))
dc <- mortality_diagnostics(c('a','b','unmatched'), d)
stopifnot(dc['tracker_matched_pairs']==2, dc['tracker_ndi_year_nonmissing']==0,
          dc['pairs_exit_year_2015_2019']==1,
          dc['pairs_known_deceased_year_2015_2019']==2,
          dc['tracker_exit_year_invalid']==1)
d$NYEAR <- c(2018,9999)
dc <- mortality_diagnostics(c('a'), d)
stopifnot(dc['tracker_ndi_year_nonmissing']==2,
          dc['tracker_ndi_year_plausible']==1,
          dc['pairs_ndi_year_2015_2019']==1)
dr <- diagnostic_report(dc)
stopifnot(all(dr$count_rounded_10 == 'SUPPRESSED'))
cat('Synthetic mortality-diagnostic checks passed.\n')
# Import audit: synthetic records only, never HRS data.
writeLines(c('INFILE "synthetic.da" LRECL = 13;', 'INPUT',
             ' HHID $ 1 - 6', ' PN $ 7 - 9', ' NYEAR 10 - 13', ';'), s)
writeLines(c('0000010102017', '000002020    '), p, useBytes=TRUE)
aud <- audit_import(p, s, c('HHID','PN','NYEAR'))
stopifnot(aud$ok, aud$report$value[aud$report$check=='independent_reader_agreement']=='PASS')
writeLines(c('0000010102017extra'), p)
aud <- audit_import(p, s, c('HHID','PN','NYEAR'))
stopifnot(!aud$ok, aud$report$value[aud$report$check=='record_length_matches_LRECL']=='FAIL')
writeLines('00000101020', p)
stopifnot(!audit_import(p,s,c('HHID','PN','NYEAR'))$ok)
writeLines('000001010abcd',p)
stopifnot(!audit_import(p,s,c('HHID','PN','NYEAR'))$ok)
# Independent metadata with a conflicting field boundary must be reported.
sp <- tempfile(fileext='.sps')
writeLines(c('DATA LIST FILE="synthetic.da" FIXED /',
 ' HHID 1-6 (A)', ' PN 7-9 (A)', ' NYEAR 10-12', '.'),sp)
writeLines('0000010102017',p)
aud <- audit_import(p,s,c('HHID','PN','NYEAR'),sp)
stopifnot(!aud$ok, aud$report$value[aud$report$check=='SPSS_layout_agreement']=='FAIL')
cat('Synthetic import-audit checks passed.\n')
# Matching second metadata source and skipped columns must also work.
writeLines(c('INFILE "synthetic.da" LRECL = 16;', 'INPUT',
             ' HHID $ 1 - 6', ' PN $ 7 - 9', ' UNUSED 10 - 12',
             ' NYEAR 13 - 16', ';'), s)
writeLines(c('0000010101232017', '000002020456    '),p)
writeLines(c('DATA LIST FILE="synthetic.da" FIXED /',
             ' HHID 1-6 (A)', ' PN 7-9 (A)', ' NYEAR 13-16', '.'),sp)
aud <- audit_import(p,s,c('NYEAR','HHID','PN'),sp)
stopifnot(aud$ok, aud$report$value[aud$report$check=='SPSS_layout_agreement']=='PASS')
writeLines(character(), p)
stopifnot(!audit_import(p,s,c('HHID','PN','NYEAR'))$ok)
cat('Synthetic skipped-column, metadata-agreement and empty-file checks passed.\n')
# Format diagnostics must describe structure without releasing field contents.
value <- function(a, key) a$report$value[match(key, a$report$check)]
writeLines(c('0000010101232017  ', '0000020204562018  '), p)
aud <- audit_import(p,s,c('HHID','PN','NYEAR'))
stopifnot(!aud$ok, identical(value(aud,'extra_suffix_spaces_only'),'ALL'),
          identical(value(aud,'record_excess_bytes_min'),'2'),
          identical(value(aud,'record_excess_bytes_max'),'2'))
writeLines(c('0000010101232017XY', '0000020204562018  '), p)
aud <- audit_import(p,s,c('HHID','PN','NYEAR'))
stopifnot(!aud$ok, identical(value(aud,'extra_suffix_spaces_only'),'MIXED'),
          !any(grepl('2017|2018|000001|000002|XY',aud$report$value)))
writeLines('0000010101232017\t\t',p)
aud <- audit_import(p,s,c('HHID','PN','NYEAR'))
stopifnot(!aud$ok, identical(value(aud,'records_containing_tabs'),'ALL'),
          identical(value(aud,'extra_suffix_controls_only'),'ALL'))
writeLines('0000010101232017',p)
aud <- audit_import(p,s,c('HHID','PN','NYEAR'))
stopifnot(aud$ok, identical(value(aud,'extra_suffix_spaces_only'),'NOT_APPLICABLE'))
cat('Synthetic structural-format and output-privacy checks passed.\n')
