if(file.exists('scripts/hrs_equivalence_sensitivity.R')) source('scripts/hrs_equivalence_sensitivity.R')
stopifnot(exists('margin_sensitivity',mode='function'))
metrics <- data.frame(metric=c('auc','brier'),target='prior_minus_current',
 optimism_corrected=c(.001,-.0001),ci_lower=c(-.002,-.0003),ci_upper=c(.003,.0002),
 outer_success=200,nested_success=200,status='OK')
margins <- list(auc=c(.001,.002,.003,.004),brier=c(.0001,.0002,.0003,.0004))
r <- margin_sensitivity(metrics,margins)
stopifnot(identical(r$interval_contained,c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE)),
 all(r$boundary_infimum[r$metric=='auc']==.003),
 all(r$boundary_infimum[r$metric=='brier']==.0003))
bad <- metrics; bad$status[1] <- 'UNSTABLE'
stopifnot(inherits(try(margin_sensitivity(bad,margins),silent=TRUE),'try-error'))
bad <- metrics; bad$ci_lower[1] <- NA
stopifnot(inherits(try(margin_sensitivity(bad,margins),silent=TRUE),'try-error'))
bad <- metrics; bad$ci_lower[1] <- .004
stopifnot(inherits(try(margin_sensitivity(bad,margins),silent=TRUE),'try-error'))
stopifnot(inherits(try(margin_sensitivity(rbind(metrics,metrics[1,]),margins),silent=TRUE),'try-error'),
 inherits(try(margin_sensitivity(metrics,list(auc=c(0,.01),brier=.001)),silent=TRUE),'try-error'))
# Include a nonzero interval fully within the bounds: equivalence is not the
# same question as whether zero is in the confidence interval.
positive <- metrics; positive$ci_lower[1] <- .001
positive_result <- margin_sensitivity(positive,margins)
stopifnot(positive_result$interval_contained[positive_result$metric=='auc' & positive_result$delta==.004])
input <- tempfile(); output <- tempfile(); dir.create(input)
write.csv(metrics,file.path(input,'validated_metrics.csv'),row.names=FALSE)
writeLines('SUCCESS: synthetic run',file.path(input,'STATUS.txt'))
writeLines(c('Bootstrap repetitions: 200','95% intervals'),file.path(input,'READ_ME.txt'))
before <- tools::md5sum(list.files(input,full.names=TRUE))
equivalence_main(c(input,output))
stopifnot(identical(before,tools::md5sum(list.files(input,full.names=TRUE))),
 startsWith(readLines(file.path(output,'STATUS.txt'))[1],'SUCCESS'),
 file.exists(file.path(output,'BERICHT.md')))
writeLines('INCOMPLETE: synthetic failed source',file.path(input,'STATUS.txt'))
stopifnot(inherits(try(equivalence_main(c(input,output)),silent=TRUE),'try-error'),
 startsWith(readLines(file.path(output,'STATUS.txt'))[1],'INCOMPLETE'))
cat('Interval boundaries, invalid inputs, source preservation and failure status passed.\n')
