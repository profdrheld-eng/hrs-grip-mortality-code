source('scripts/hrs_tracker_formats.R')
p <- tempfile(); s <- tempfile(); csv <- tempfile(); out <- tempfile()
writeLines(c('INPUT',' HHID $ 1 - 6',' PN $ 7 - 9',' NYEAR 10 - 13',' VERSION 14 - 14',';'),s)
writeLines(c('000001010  20171','000002020  20181'),p)
write.csv(data.frame(HHID=c('000001','000002'),PN=c('010','020'),
                     NYEAR=c('2017','2018'),VERSION=c('1','1')),csv,row.names=FALSE)
r <- compare_tracker_formats(p,s,csv)
stopifnot(r$value[r$check=='same_person_keys']=='PASS',
          r$value[r$check=='NYEAR_offset_0']=='FAIL',
          r$value[r$check=='NYEAR_offset_2']=='PASS',
          !any(grepl('000001|000002|2017|2018',r$value)))
write.csv(data.frame(HHID='000003',PN='010',NYEAR='2019',VERSION='1'),csv,row.names=FALSE)
r <- compare_tracker_formats(p,s,csv)
stopifnot(r$value[r$check=='same_person_keys']=='FAIL',
          !any(grepl('offset',r$check)))
cat('Synthetic format comparison and mismatched-key checks passed.\n')
