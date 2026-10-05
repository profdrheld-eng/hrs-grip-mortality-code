source('scripts/hrs_feasibility.R')
p <- tempfile(fileext='.csv')
x <- data.frame(HHID=c('1','2'),PN=c('10','20'),OVHHID=c('0','010417'),
 NYEAR=c('2018',''),NMONTH=c('2',''),NSCORE=c('80',''),
 EXDEATHYR=c('2018',''),KNOWNDECEASEDYR=c('2018',''),
 LASTALIVEYR=c('2017','2022'),VERSION=c('1','1'))
run <- function(x) { write.csv(x,p,row.names=FALSE,na=''); read_tracker_csv(p) }
a <- run(x)
stopifnot(a$ok,identical(a$data$key,c('000001:010','000002:020')),
          a$data$OVHHID[1]=='000000',is.na(a$data$NYEAR[2]))
for(field in c('VERSION','NMONTH','NYEAR','NSCORE','HHID')) {
 bad <- x; bad[[field]][1] <- '9999999'
 stopifnot(!run(bad)$ok,is.null(run(bad)$data))
}
bad <- x; bad$NYEAR[1] <- 'not_numeric'
stopifnot(!run(bad)$ok)
bad <- x; bad[2,] <- bad[1,]
stopifnot(!run(bad)$ok)
bad <- x; bad$VERSION <- NULL
stopifnot(!run(bad)$ok)
bad <- x; bad$NYEAR[1] <- '.'
stopifnot(!run(bad)$ok) # Unverified special codes must not silently become missing.
stopifnot(!any(grepl('2018|010417|000001',a$report$value)))
cat('Synthetic CSV import, missingness, version, range and ID checks passed.\n')
for(year in c('1992','2024','2025','2026')) {
  valid <- x; valid$LASTALIVEYR[1] <- year
  stopifnot(run(valid)$ok)
}
for(year in c('1991','2027','2025.5')) {
  invalid <- x; invalid$LASTALIVEYR[1] <- year
  stopifnot(!run(invalid)$ok)
}
invalid <- x; invalid$EXDEATHYR[1] <- '2026'
stopifnot(!run(invalid)$ok)
# End-to-end: tracker ASCII and setup are deliberately absent.
root <- tempfile(); output <- tempfile(); dir.create(root)
for(wave in c('10','14')) {
  prefix <- if(wave=='10') 'M' else 'O'
  writeLines(c('INPUT',' HHID $ 1 - 6',' PN $ 7 - 9',
    paste0(' ',prefix,'I816 10 - 11'),paste0(' ',prefix,'I851 12 - 13'),
    paste0(' ',prefix,'I852 14 - 15'),paste0(' ',prefix,'I853 16 - 17'),
    paste0(' ',prefix,'I817 18 - 18'),';'),
    file.path(root,paste0('h',wave,'i_r.sas')))
  writeLines(c('000001010202122231','000002020303132331'),
    file.path(root,paste0('h',wave,'i_r.da')))
}
write.csv(x,file.path(root,'trk2022tr_r.csv'),row.names=FALSE)
main(c(root,output))
stopifnot(all(file.exists(file.path(output,c('import_audit.csv',
  'feasibility_counts.csv','mortality_diagnostics.csv','READ_ME.txt')))))
prior <- readLines(file.path(output,'feasibility_counts.csv'))
bad <- x; bad$VERSION <- '2'
write.csv(bad,file.path(root,'trk2022tr_r.csv'),row.names=FALSE)
stopifnot(inherits(try(main(c(root,output)),silent=TRUE),'try-error'),
          identical(prior,readLines(file.path(output,'feasibility_counts.csv'))),
          grepl('stale',readLines(file.path(output,'READ_ME.txt'))))
cat('Synthetic full workflow and stale-output protection checks passed.\n')
