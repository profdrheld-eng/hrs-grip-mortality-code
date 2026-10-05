source('scripts/hrs_feasibility.R')
source('scripts/hrs_table1.R')
x <- data.frame(age=seq(50,89),grip10=seq(20,59),grip14=seq(18,57),
 change_kg=rep(-2,40),gap_years=rep(4,40),sex=factor(rep(1:2,each=20)),
 smoke=factor(rep(c('never','former'),each=20),levels=c('never','former','current')),
 health=factor(rep(1:5,each=8)),lo=c(rep(2,10),rep(5,20),rep(3,10)),
 hi=c(rep(2.2,10),rep(Inf,30)))
a <- table1_summary(x)
stopifnot(a$mean[a$variable=='age']==69.5,
 a$sd[a$variable=='age']==round(sd(x$age),1),
 a$mean[a$variable=='change_kg']==-2,
 all(a$mean[a$variable=='gap_years']==4),
 all(a$count_rounded_10[a$variable=='sex']=='20'),
 all(a$percent_approx[a$variable=='sex']==50),
 all(is.na(a$percent_approx[a$variable=='health'])),
 a$count_rounded_10[a$variable=='smoke' & a$level=='Current']=='SUPPRESSED')
# One small category triggers complementary suppression of another category.
y <- x;y$sex <- factor(c(rep(1,37),rep(2,3)))
b <- table1_summary(y)
stopifnot(all(b$count_rounded_10[b$variable=='sex']=='SUPPRESSED'))
y <- x;y$age[1] <- NA
stopifnot(inherits(try(table1_summary(y),silent=TRUE),'try-error'))
y <- x;y$hi[1] <- 6
stopifnot(inherits(try(table1_summary(y),silent=TRUE),'try-error'))
# Formatting uses rounded public cells, never extra exact counts in text.
lines <- table1_markdown(a)
stopifnot(any(grepl('69.5 ±',lines,fixed=TRUE)),any(grepl('approximately',lines,fixed=TRUE)))
cat('PASS: mean/sample SD, change sign, categories, early censoring, primary/complementary suppression, missing-data and horizon guards, Markdown.\n')
# Exercise the export wrapper using synthetic in-memory input and a temporary reference.
source('scripts/hrs_analysis.R')
fixture <- x
fixture$household <- rep(seq_len(nrow(x)/2),each=2)
counts <- c(analyzed=nrow(x),interval_events=sum(is.finite(x$hi)),
 early_censored=sum(!is.finite(x$hi)&x$lo<5))
stage2_main <- function(...) list()
prepare_change_data <- function(...) list(data=fixture,counts=counts,
 flow_context=list(tracker=fixture,origin=rep(100,nrow(x)),early=rep(52,nrow(x)),covok=rep(TRUE,nrow(x))))
make_followup <- function(t,...) data.frame(lo=t$lo,hi=t$hi,reason='included')
fit_pair <- function(...) stop('Model fitting must not occur.')
workspace <- tempfile('table1_synthetic_');dir.create(workspace)
dir.create(file.path(workspace,'diagnostics_v1'))
write.csv(diagnostic_report(counts),file.path(workspace,'diagnostics_v1','sample_flow.csv'),row.names=FALSE)
table1_main(c('SYNTHETIC_NO_DATA_DIRECTORY',workspace),'scripts')
exports <- list.dirs(workspace,recursive=FALSE,full.names=TRUE)
export <- exports[grepl('/table1_[0-9]',exports)]
stopifnot(length(export)==1L,
 grepl('^SUCCESS',readLines(file.path(export,'STATUS.txt'))),
 all(c('table1.csv','TABLE1.md','sample_flow.csv','READ_ME.txt','source_md5.csv','STATUS.txt') %in% list.files(export)))
out <- read.csv(file.path(export,'table1.csv'))
stopifnot(nrow(out)==nrow(a),!any(c('household','key','HHID','PN') %in% names(out)))
cat('PASS: synthetic export wrapper, reference check, aggregate-only schema and SUCCESS status; no model fit.\n')
