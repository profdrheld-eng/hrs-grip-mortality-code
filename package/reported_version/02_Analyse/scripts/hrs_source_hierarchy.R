# Requires death_bounds() from hrs_stage2.R. Does not read or write files.
select_death_source <- function(t,strategy='ndi_first') {
  stopifnot(strategy %in% c('ndi_first','interview_first','exclude_conflicts','ndi_only'))
  ndi <- death_bounds(t$NYEAR,t$NMONTH,TRUE)
  ex <- death_bounds(t$EXDEATHYR,t$EXDEATHMO)
  ex[!t$EXDODSOURCE %in% 1:2,] <- NA_real_
  known <- death_bounds(t$KNOWNDECEASEDYR,t$KNOWNDECEASEDMO)
  known[!t$KNOWNDECEASEDSOURCE %in% 1:3,] <- NA_real_
  yes <- function(x) !is.na(x)&x
  disjoint <- function(a,b) yes(a[,2]<b[,1]|b[,2]<a[,1])
  conflict <- disjoint(ndi,ex)|disjoint(ndi,known)|disjoint(ex,known)
  alive <- month_index(t$LASTALIVEYR,t$LASTALIVEMO)
  # Imputed contacts cannot override observed mortality.
  alive[!t$LASTALIVESOURCE %in% 1:3] <- NA_real_
  any_alive <- yes(alive>ndi[,2])|yes(alive>ex[,2])|yes(alive>known[,2])
  sources <- list(NDI=ndi,EXIT=ex,KNOWN=known)
  order <- if(strategy=='interview_first') c('EXIT','KNOWN','NDI') else if(strategy=='ndi_only') 'NDI' else names(sources)
  out <- data.frame(lower=rep(NA_real_,nrow(t)),upper=NA_real_,source='NONE',
    death_source_conflict=conflict,alive_conflict_selected=FALSE,requires_review=FALSE,excluded=FALSE)
  for(s in order) {
    take <- is.na(out$lower)&!is.na(sources[[s]][,1])
    out$lower[take] <- sources[[s]][take,1]; out$upper[take] <- sources[[s]][take,2]
    out$source[take] <- s
  }
  out$alive_conflict_selected <- yes(alive>out$upper)
  out$requires_review <- out$alive_conflict_selected
  if(strategy=='exclude_conflicts') {
    out$excluded <- conflict|any_alive
    out[out$excluded,c('lower','upper')] <- NA_real_
  }
  out
}
