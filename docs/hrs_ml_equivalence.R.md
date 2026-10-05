# Annotated source: hrs_ml_equivalence.R

Read-only explanation of the executed source. Run the materialized package, not this Markdown view. Only REVIEW NOTE comments are added.

Source: `package/reported_version/02_Analyse/scripts/hrs_ml_equivalence.R`

Source SHA-256: `be13ceb54a04d2ad69f6a540da712068f72b32aa2a9bfdbdcd08ab000adf6639`

```r
# Exploratory, approximate, conditional bootstrap TOST. Not exact equality.
# Inputs are aggregate performance differences, never person-level predictions.
# REVIEW NOTE: Hypothetical sensitivity margins are not clinically justified equivalence thresholds. Keep the full grid rather than selecting a favorable margin.
ml_equivalence_margins <- function() list(
  auc=c(.0001,.0002,.0005,.001,.002,.005,.01,.02,.05),
  brier=c(.00001,.00002,.00005,.0001,.0002,.0005,.001,.002,.005))

# REVIEW NOTE: Center paired fixed-prediction bootstrap errors at the observed estimate. Boundary tests use plus-one tail probabilities. Basic 90% intervals and percentile 95% intervals are different constructions. This is approximate conditional inference, not a full model-development equivalence test.
ml_tost <- function(estimate,draws,margin,alpha=.05) {
  stopifnot(length(estimate)==1L,is.finite(estimate),length(margin)==1L,
    is.finite(margin),margin>0,length(alpha)==1L,alpha>0,alpha<.5,
    length(draws)>=1000L,all(is.finite(draws)),sd(draws)>0)
  # Boundary-null distributions shift centered bootstrap errors to -/+ margin.
  # Basic bootstrap intervals invert these tests; do not mix with percentile CIs.
  error<-draws-estimate;B<-length(error)
  lower<-(1+sum(error>=estimate+margin))/(B+1)
  upper<-(1+sum(error<=estimate-margin))/(B+1)
  ci<-estimate-quantile(error,c(1-alpha,alpha),names=FALSE)
  contained<-ci[1]> -margin && ci[2]<margin
  data.frame(estimate=estimate,margin=margin,ci90_lower=ci[1],ci90_upper=ci[2],
    p_lower=lower,p_upper=upper,p_tost=max(lower,upper),
    equivalent=max(lower,upper)<alpha,interval_contained=contained,
    bootstrap_B=B,p_resolution=1/(B+1),
    scope='Exploratory conditional OOF inference; no full-pipeline refit')
}

# REVIEW NOTE: Join draws to contrasts by all identifying fields. Holm adjustment is within metric and margin. No joint guarantee over both metrics and the entire margin grid is asserted. Never reconstruct missing draws from interval endpoints.
ml_equivalence_report <- function(input,output) {
  input<-normalizePath(input,mustWork=TRUE);output<-normalizePath(output,mustWork=FALSE)
  if(input==output || startsWith(input,paste0(output,'/'))) stop('Separate output required.')
  if(file.exists(output)) stop('Use a new output directory to preserve previous reports.')
  if(!startsWith(readLines(file.path(input,'STATUS.txt'),warn=FALSE)[1],'SUCCESS:'))
    stop('Successful source benchmark required.')
  paired<-read.csv(file.path(input,'benchmark_paired.csv'),stringsAsFactors=FALSE)
  paired<-paired[paired$fraction==1 & paired$metric %in% c('auc','brier'),]
  key<-c('fraction','contrast','model','target','arm','reference_model','metric')
  stopifnot(nrow(paired)>0,!anyDuplicated(paired[key]),
    all(is.finite(as.matrix(paired[c('difference','ci_lower','ci_upper')]))),
    all(paired$ci_lower<=paired$ci_upper))
  margins<-ml_equivalence_margins();rows<-list();tests<-list()
  path<-file.path(input,'benchmark_bootstrap_differences.csv')
  draws<-if(file.exists(path)) read.csv(path,stringsAsFactors=FALSE) else NULL
  for(i in seq_len(nrow(paired))) {
    z<-paired[i,];v<-NULL
    if(!is.null(draws)) {
      keep<-rep(TRUE,nrow(draws))
      for(k in key) keep<-keep & draws[[k]]==z[[k]]
      subset<-draws[keep,];stopifnot(!anyDuplicated(subset$replicate))
      v<-subset$difference
    }
    for(delta in margins[[z$metric]]) {
      rows[[length(rows)+1]]<-data.frame(z,margin=delta,
        interval95_contained=z$ci_lower> -delta & z$ci_upper<delta,
        boundary_infimum=max(abs(c(z$ci_lower,z$ci_upper))),
        interpretation='Hypothetical margin sensitivity; not a clinical equivalence claim')
      if(!is.null(v)) tests[[length(tests)+1]]<-cbind(z[key],ml_tost(z$difference,v,delta))
    }
  }
  dir.create(output,recursive=TRUE)
  write.csv(do.call(rbind,rows),file.path(output,'margin_sensitivity.csv'),row.names=FALSE)
  if(length(tests)) {
    z<-do.call(rbind,tests);z$p_holm<-NA_real_
    for(ix in split(seq_len(nrow(z)),interaction(z$metric,z$margin,drop=TRUE)))
      z$p_holm[ix]<-p.adjust(z$p_tost[ix],method='holm')
    z$equivalent_holm<-z$p_holm<.05
    write.csv(z,file.path(output,'conditional_tost.csv'),row.names=FALSE)
  }
  paths<-file.path(input,c('STATUS.txt','CONFIG.R','source_hashes.csv','benchmark_paired.csv'))
  if(!is.null(draws)) paths<-c(paths,path)
  h<-tools::md5sum(paths)
  write.csv(data.frame(file=names(h),md5=unname(h)),file.path(output,'source_hashes.csv'),row.names=FALSE)
  writeLines(c('Exploratory equivalence analysis; margins not clinically validated or prespecified.',
    'All existing 1-2-5 margins retained. No favorable margin selected.',
    'Historical 95% percentile intervals are retained as descriptive sensitivity only.',
    if(length(tests)) 'Approximate conditional bootstrap TOST computed from actual paired resamples.' else
      'TOST NOT COMPUTED: source did not save bootstrap draws; no reconstruction from 95% intervals.',
    'Boundary-null tests use bootstrap errors centered at the observed difference, plus-one tail probabilities.',
    '90% basic bootstrap intervals correspond to centered tests; finite-sample quantile discretization may differ near a boundary.',
    'Holm adjustment is across all contrasts within each metric and each margin; no global cross-margin or cross-metric equivalence claim.',
    'Bootstrap holds fitted models, fold membership, preprocessing, synthesis and censoring weights fixed.',
    'Overlapping CV training sets and training/tuning uncertainty limit coverage; p-values are approximate and conditional.',
    'B>=1000 required; recommended B=5000. Results near 0.05 need Monte Carlo sensitivity checks.',
    'No external validation, exact equality, clinical interchangeability or algorithm-wide superiority claim.'),
    file.path(output,'READ_ME.txt'))
  writeLines(if(length(tests)) 'SUCCESS: exploratory conditional TOST and margin sensitivity.' else
    'PARTIAL: margin sensitivity complete; formal TOST pending new benchmark bootstrap export.',
    file.path(output,'STATUS.txt'))
  invisible(output)
}

if(sys.nframe()==0L) {
  args<-commandArgs(trailingOnly=TRUE)
  if(length(args)!=2L) stop('Usage: Rscript hrs_ml_equivalence.R ML_RESULT_DIR NEW_REPORT_DIR')
  ml_equivalence_report(args[1],args[2])
}
```
