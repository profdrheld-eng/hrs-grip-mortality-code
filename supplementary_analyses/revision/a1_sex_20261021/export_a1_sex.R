# HUMAN-RUN ONLY for HRS inputs. Tests use invented records only.
# No fits, resampling, checkpoint/model loading, predictions or network operations.
a1_release <- function(counts) {
  if(!length(counts)||is.null(names(counts))||anyNA(counts)||any(!is.finite(counts))||any(counts<0)||
     any(counts!=floor(counts))) stop('Invalid aggregate counts.')
  suppressed<-counts<10
  # Do not release an otherwise identifiable single small category.
  if(sum(suppressed)==1 && length(counts)>1) {
    candidates<-which(!suppressed & names(counts)!='total')
    if(!length(candidates)) candidates<-which(!suppressed)
    suppressed[candidates[which.min(counts[candidates])]]<-TRUE
  }
  shown<-as.character(10*floor(counts/10+.5));shown[suppressed]<-'SUPPRESSED'
  data.frame(metric=names(counts),count_rounded_10=shown,stringsAsFactors=FALSE)
}

a1_denominator_guard <- function(baseline_n,included_n) {
  rounded <- function(x) 10*floor(x/10+.5)
  if(length(baseline_n)!=1L || length(included_n)!=1L || anyNA(c(baseline_n,included_n)) ||
     included_n!=6307L || rounded(baseline_n)!=8690L ||
     rounded(included_n)!=6310L || rounded(baseline_n-included_n)!=2380L)
    stop('Cohort reference mismatch; no release.')
  TRUE
}

a1_sex_table <- function(sex,included) {
  if(length(sex)!=length(included) || !is.logical(included) || anyNA(included) ||
     !length(sex) || !is.numeric(sex)) stop('Invalid aggregate inputs.')
  do.call(rbind,lapply(c(TRUE,FALSE),function(g) {
    v <- sex[included==g]
    counts <- c(total=length(v),male=sum(v %in% 1),female=sum(v %in% 2),
                missing_or_invalid=sum(!v %in% 1:2))
    stopifnot(sum(counts[-1])==counts[1])
    released <- a1_release(counts)
    data.frame(group=if(g) 'included' else 'excluded',released,stringsAsFactors=FALSE)
  }))
}

a1_prepare <- function(imported,expected_n=6307L,enforce_reference=TRUE) {
  core<-imported$core;t<-imported$tracker
  if(anyDuplicated(t$key) || anyNA(t$key)) stop('Invalid tracker keys.')
  a<-wave_summary(core$h10i_r,'M');b<-wave_summary(core$h14i_r,'O')
  keys<-intersect(a$key[a$valid_trials>0],b$key[b$valid_trials>0]);keys<-keys[keys %in% t$key]
  z<-t[match(keys,t$key),];ok<-!is.na(z$OAGE)&z$OAGE>=50&z$OAGE<=120&z$OAGE==floor(z$OAGE)&z$OVHHID=='000000'
  z<-z[ok,,drop=FALSE];keys<-keys[ok]
  c14<-core$h14c_r[match(keys,core$h14c_r$key),];pr<-core$h14pr_r[match(keys,core$h14pr_r$key),]
  origin<-month_index(z$OIWYEAR,z$OIWMONTH);early<-month_index(z$MIWYEAR,z$MIWMONTH)
  follow<-make_followup(z,origin+.5)
  d<-data.frame(household=z$HHID,age=z$OAGE,sex=factor(ifelse(z$SEX %in% 1:2,z$SEX,NA),levels=1:2),
    smoke=factor(smoking_status(c14$OC117,c14$OC116,pr$OZ076,pr$OZ205),levels=c('never','former','current')),
    health=factor(ifelse(c14$OC001 %in% 1:5,c14$OC001,NA),levels=1:5),
    grip14=b$grip_max[match(keys,b$key)],grip10=a$grip_max[match(keys,a$key)],lo=follow$lo,hi=follow$hi)
  take<-!is.na(origin)&!is.na(early)&early<origin&follow$reason=='included'&complete.cases(d)
  analytical_keys<-keys[take];d<-d[take,,drop=FALSE]
  if(nrow(d)!=expected_n) stop('Analytical cohort does not reproduce the exact reference N.')
  ak<-a$key[a$valid_trials>0]
  if(anyNA(ak) || anyDuplicated(ak) || anyNA(analytical_keys) ||
     anyDuplicated(analytical_keys) || !all(analytical_keys %in% ak))
    stop('Invalid cohort key mapping.')
  included<-ak %in% analytical_keys
  if(enforce_reference) a1_denominator_guard(length(ak),sum(included))
  sex<-t$SEX[match(ak,t$key)]
  if(any(!sex[included] %in% 1:2)) stop('Included-cohort sex consistency failed.')
  list(sex=sex,included=included)
}

a1_verify_sources <- function(scripts) {
  expected <- c(hrs_feasibility.R='87da5316011268f839bcec76924bccb9',
    hrs_stage2.R='f8b2263cbeb949029389767eb979c336',
    hrs_source_hierarchy.R='17d004980078fc1f072c791824944daa',
    hrs_analysis.R='aef9cff44f38cd394f4d632b39ec0b8b')
  paths<-file.path(scripts,names(expected))
  if(!all(file.exists(paths))) stop('Required public code absent.')
  actual<-unname(tools::md5sum(paths))
  if(anyNA(actual)||!identical(actual,unname(expected))) stop('Public source identity mismatch; no import.')
  invisible(expected)
}

a1_export_main <- function(args,helper_dir) {
  if(length(args)!=3L || !all(dir.exists(args))) stop('Supply DATA_DIR OUTPUT_PARENT CORRECTED_SCRIPTS.')
  args<-vapply(args,normalizePath,'',mustWork=TRUE)
  if(args[2]==args[1]||startsWith(args[2],paste0(args[1],'/'))) stop('Output must be separate from input data.')
  expected<-a1_verify_sources(args[3])
  out<-file.path(args[2],paste0('a1_sex_',format(Sys.time(),'%Y%m%d_%H%M%S'),'_',Sys.getpid()))
  if(!dir.create(out)) stop('Cannot create fresh output directory.')
  private<-file.path(out,'LOCAL_ONLY');share<-file.path(out,'SEND_BACK')
  stopifnot(dir.create(private),dir.create(share))
  writeLines('INCOMPLETE: no approved release.',file.path(share,'STATUS.txt'))
  cat('Private run directory: ',out,'\n',sep='')
  log<-file(file.path(private,'export.log'),open='wt')
  warn_before<-getOption('warn');options(warn=1)
  sink(log);sink(log,type='message')
  success<-tryCatch({
    env<-new.env(parent=environment(a1_export_main))
    for(name in names(expected)) sys.source(file.path(args[3],name),envir=env)
    # Trap every exposed model entry point; none is used by the cohort adapter.
    env$fit_pair<-function(...) stop('Model fitting is prohibited in this export.')
    environment(a1_prepare)<-env
    imported<-env$stage2_main(c(args[1],private),file.path(args[3],'hrs_feasibility.R'))
    prepared<-a1_prepare(imported)
    result<-a1_sex_table(prepared$sex,prepared$included)
    # Only allowlisted aggregate outputs enter SEND_BACK. No participant keys.
    stopifnot(identical(names(result),c('group','metric','count_rounded_10')),nrow(result)==8L,
      all(result$count_rounded_10=='SUPPRESSED' | grepl('^[0-9]+0$',result$count_rounded_10)))
    write.csv(result,file.path(share,'selection_sex.csv'),row.names=FALSE,na='')
    provenance<-data.frame(file=c(names(expected),'export_a1_sex.R'),
      md5=c(unname(expected),unname(tools::md5sum(file.path(helper_dir,'export_a1_sex.R')))))
    if(anyNA(provenance$md5)) stop('Export provenance unavailable; no release.')
    write.csv(provenance,file.path(share,'source_hashes.csv'),row.names=FALSE)
    writeLines(c('A1 included/excluded coded-sex comparison; baseline: all valid 2010 grip measurements.',
      'Same analytical-key definition as the existing review runner; exact analytical N6307 guard passed.',
      'Rounded reference guards passed: baseline8690, included6310, excluded2380.',
      'SEX1=male; SEX2=female. These are Tracker coded-sex categories, not self-reported gender.',
      'Missing_or_invalid includes absent Tracker match, missing SEX and codes outside1/2; no imputation.',
      'Counts rounded to nearest10, half-up. Counts<10 including0 suppressed.',
      'If only one category is suppressed, a second non-total category is also suppressed.',
      'No separate valid-sex or unmatched margin is exported, to avoid redundant reconstruction.',
      'Rounded categories may not sum to rounded totals. No percentages or hidden counts inferred.',
      'Suppression is the existing project rule, not a universal anonymisation guarantee.',
      'No fits, bootstrap, OOF replay, model objects, checkpoints or individual predictions.',
      'Import audits and detailed errors remain LOCAL_ONLY. Review SEND_BACK before sharing.'),file.path(share,'README.txt'))
    stopifnot(setequal(list.files(share),c('STATUS.txt','selection_sex.csv','source_hashes.csv','README.txt')))
    writeLines('PASS: reference denominators and existing small/complementary suppression rules checked.',file.path(share,'DISCLOSURE_CHECK.txt'))
    writeLines('SUCCESS: A1 sex aggregate export completed. No model fits.',file.path(share,'STATUS.txt'))
    TRUE
  },error=function(condition) {
    message('Private export error: ',conditionMessage(condition))
    # No partially generated counts remain in the release folder.
    unlink(file.path(share,c('selection_sex.csv','source_hashes.csv','README.txt','DISCLOSURE_CHECK.txt')))
    writeLines('FAILED: no release. Details remain LOCAL_ONLY/export.log.',file.path(share,'STATUS.txt'))
    FALSE
  })
  sink(type='message');sink();close(log);options(warn=warn_before)
  if(!success) stop('A1 export failed. Do not share outputs; inspect the private log locally.',call.=FALSE)
  cat('SUCCESS. Review and return only: ',share,'\n',sep='')
  invisible(out)
}

if(sys.nframe()==0L) {
  script<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
  a1_export_main(commandArgs(trailingOnly=TRUE),dirname(normalizePath(script)))
}
