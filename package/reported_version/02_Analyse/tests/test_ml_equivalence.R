# Synthetic aggregate bootstrap draws only; no HRS records.
if(file.exists('scripts/hrs_ml_equivalence.R')) source('scripts/hrs_ml_equivalence.R')
stopifnot(exists('ml_tost',mode='function'))
B<-9999L;se<-.001;estimate<-.0002
draws<-estimate+qnorm(seq_len(B)/(B+1))*se
z<-ml_tost(estimate,draws,.003)
stopifnot(z$p_tost<.05,z$equivalent,
 abs(z$p_lower-pnorm((estimate+.003)/se,lower.tail=FALSE))<.0002,
 abs(z$p_upper-pnorm((estimate-.003)/se))<.0002,
 abs(z$ci90_lower-(estimate-qnorm(.95)*se))<.00001)
# A difference can be nonzero yet equivalent; nonsignificance is not equivalence.
stopifnot(ml_tost(.002,.002+qnorm(seq_len(B)/(B+1))*.0001,.003)$equivalent)
stopifnot(!ml_tost(0,draws-estimate,.0001)$equivalent)
stopifnot(!ml_tost(.004,draws-estimate+.004,.003)$equivalent)
for(args in list(list(0,rep(0,20),.001),list(0,c(draws,NA),.001),
 list(0,draws,0),list(0,rep(0,B),.001)))
 stopifnot(inherits(try(do.call(ml_tost,args),silent=TRUE),'try-error'))
# Exact equality at an interval boundary does not pass the strict CI rule.
z<-ml_tost(0,draws-estimate,.003)
stopifnot(!ml_tost(0,draws-estimate,z$ci90_upper)$interval_contained)
cat('PASS: bootstrap TOST tails, normal oracle, nonzero equivalence, failure guards, strict bounds.\n')
