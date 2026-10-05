# Run locally by the user. Microdata are never sent to an external service.
script<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]);directory<-dirname(normalizePath(script))
source(file.path(directory,'hrs_ml_extension.R'));source(file.path(directory,'hrs_ml_point_figures.R'))
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=3) stop('Usage: Rscript hrs_ml_person_figures_local.R PRIVATE_DATA_DIR INFERENCE_DIR OUTPUT_PARENT')
ml_source_existing(directory);ml_require()
previous<-normalizePath(args[2]);stopifnot(startsWith(readLines(file.path(previous,'STATUS.txt'))[1],'SUCCESS:'))
# Config is compared against the known full profile, not evaluated from disk.
cfg<-ml_config('full');cfg$fractions<-1;cfg$ci<-5000L;cfg$export_bootstrap<-TRUE
saved_cfg<-readLines(file.path(previous,'CONFIG.R'));tmp<-tempfile();dput(cfg,file=tmp)
stopifnot(identical(saved_cfg,readLines(tmp)));unlink(tmp)
hashes<-read.csv(file.path(previous,'source_hashes.csv'),stringsAsFactors=FALSE)
stopifnot(all(unname(tools::md5sum(file.path(directory,hashes$file)))==hashes$md5))
versions<-read.csv(file.path(previous,'versions.csv'),stringsAsFactors=FALSE)
current<-c(as.character(getRversion()),vapply(c('survival','rpart','xgboost'),function(p) as.character(packageVersion(p)),''))
stopifnot(identical(unname(current),versions$version))
out<-file.path(args[3],paste0('hrs_ml_person_figures_',format(Sys.time(),'%Y%m%d_%H%M%S')))
if(file.exists(out)) stop('Output already exists.')
stopifnot(dir.create(out,recursive=TRUE));writeLines('INCOMPLETE',file.path(out,'STATUS.txt'))
tryCatch({
 imported<-stage2_main(c(args[1],file.path(out,'import_checks')),file.path(directory,'hrs_feasibility.R'))
 d<-prepare_change_data(imported)$data
 tuning<-read.csv(file.path(previous,'benchmark_tuning.csv'),stringsAsFactors=FALSE)
 a<-ml_replay_predictions(d,cfg,tuning)
 expected<-read.csv(file.path(previous,'benchmark_metrics.csv'),stringsAsFactors=FALSE)
 checks<-lapply(seq_len(nrow(a$combos)),function(j) {
   c<-a$combos[j,];z<-expected[expected$model==c$model&expected$target==c$target&expected$arm==c$arm&expected$fraction==1,]
   stopifnot(nrow(z)==1);v<-ml_metrics(d,a$risk[,j],a$weights)
   data.frame(c,auc_difference=unname(v['auc']-z$auc),brier_difference=unname(v['brier']-z$brier))
 })
 checks<-do.call(rbind,checks);write.csv(checks,file.path(out,'REPLAY_CHECKS.csv'),row.names=FALSE)
 if(any(abs(checks$auc_difference)>1e-8)|any(abs(checks$brier_difference)>1e-8)) stop('Recreated metrics differ from the saved run.')
 cal<-read.csv(file.path(previous,'benchmark_calibration.csv'),stringsAsFactors=FALSE)
 ml_render_person_points(a$risk,a$combos,cal,out)
 paths<-c(file.path(directory,c('hrs_ml_person_figures_local.R','hrs_ml_point_figures.R')),file.path(previous,c('CONFIG.R','benchmark_tuning.csv','benchmark_metrics.csv','benchmark_calibration.csv')))
 h<-tools::md5sum(paths);write.csv(data.frame(source=names(h),md5=unname(h)),file.path(out,'source_hashes.csv'),row.names=FALSE)
 writeLines('SUCCESS: selected models recreated; AUC/Brier matched; local PNG figures only.',file.path(out,'STATUS.txt'))
 message('Person-point figures saved: ',out)
},error=function(e) {writeLines('INCOMPLETE: recreation or figure check failed. No prediction tables exported.',file.path(out,'STATUS.txt'));stop('Local figure run stopped; inspect aggregate REPLAY_CHECKS.csv if present.',call.=FALSE)})
