# Redraw Figure 3 from saved aggregate inference outputs, without model fitting.
args<-commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==2)
script<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE))
root<-normalizePath(file.path(dirname(script),'../..'))
source_file<-file.path(root,'02_Analyse/scripts/hrs_ml_figures.R')
# Load only plotting helpers, not the full figure script's execution block.
helpers<-c('setup','axisx','whisker','Fcombined')
for(expr in parse(source_file)) {
 if(is.call(expr) && identical(expr[[1]],as.name('<-')) &&
    is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% helpers) eval(expr)
}
stopifnot(all(vapply(helpers,exists,logical(1))))
eq<-read.csv(file.path(args[1],'exploratory_equivalence/conditional_tost.csv'))
draws<-read.csv(file.path(args[1],'benchmark_bootstrap_differences.csv'))
stopifnot(nrow(eq)==360)
key<-function(x) paste(x$model,x$target,x$arm)
colours<-c(weibull='#087F8C',neural='#7956A5',xgboost='#D47735')
models<-names(colours);names_model<-c('Weibull','Neural AFT','XGBoost')
options(scipen=999)
dir.create(args[2],recursive=TRUE,showWarnings=FALSE)
png(file.path(args[2],'Figure_3.png'),width=3120,height=1200,res=240,type='cairo')
Fcombined(1,full_background=TRUE)
dev.off()
pdf(file.path(args[2],'Figure_3.pdf'),width=13,height=5,family='Helvetica',useDingbats=FALSE)
Fcombined(1,full_background=TRUE)
dev.off()
