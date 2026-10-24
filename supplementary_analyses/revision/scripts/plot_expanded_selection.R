# Released aggregate CSVs only. No HRS individual records or model fitting.
args<-commandArgs(trailingOnly=TRUE);root<-normalizePath(args[1],mustWork=TRUE)
input<-file.path(root,'results')
stopifnot(length(args)==2,!dir.exists(args[2]));out<-args[2];dir.create(out)
flow<-read.csv(file.path(input,'selection_flow.csv'),stringsAsFactors=FALSE)
srh<-read.csv(file.path(input,'selection_srh.csv'),stringsAsFactors=FALSE)
counts<-setNames(flow$count_rounded_10,flow$metric)
excluded<-srh$count_rounded_10[srh$metric=='total'&srh$group=='excluded']
show<-function(s) if(s=='SUPPRESSED') 'Suppressed' else paste0('≈',format(as.integer(s),big.mark=',',trim=TRUE))
render<-function() {
 par(mar=c(.2,.2,.2,.2),family='sans',xaxs='i',yaxs='i');plot.new();plot.window(c(0,1),c(0,1))
 box<-function(x,y,w,h,label,fill='white',cex=.95) {rect(x,y,x+w,y+h,col=fill,border='black',lwd=.8);text(x+w/2,y+h/2,label,cex=cex)}
 box(.12,.85,.76,.12,paste('At least one valid grip assessment in 2010',paste(show(counts['total']),'participants (rounded)'),sep='\n'),cex=.95)
 box(.02,.65,.45,.14,'Included in the analytical cohort\n6,307 participants (exact)\nValid grip in both waves',fill='#edf3f7',cex=.91)
 box(.53,.65,.45,.14,paste('Excluded from analytical cohort',paste(show(excluded),'participants (rounded)'),sep='\n'),fill='#f5f5f5')
 arrows(c(.38,.62,.75),c(.84,.84,.64),c(.25,.75,.75),c(.80,.80,.61),length=.09,lwd=.9)
 rect(.02,.14,.98,.60,col='white',border='black',lwd=.8)
 text(.04,.576,'Mutually exclusive reasons (fixed priority rules)',adj=c(0,.5),cex=.92,font=2)
 keys<-c('death_before_2014','unmatched_tracker','documented_death_2014_2015_without_core','no_core_interview','physical_measure_ineligible','not_scheduled_for_2014_physical_measures','documented_unable','no_valid_grip_or_other_exclusion')
 labels<-c('Documented death before calendar 2014','No matched Tracker record','Documented death in 2014–2015 without a Core interview','No qualifying 2014 Core interview','Ineligible for physical measurement','Not scheduled for 2014 physical measurements','Documented inability to complete grip testing','No valid grip or another analytical exclusion')
 ys<-seq(.524,.18,length.out=8)
 for(i in seq_along(keys)) {text(.04,ys[i],paste(strwrap(labels[i],width=52),collapse='\n'),adj=c(0,.5),cex=.86);text(.96,ys[i],show(counts[keys[i]]),adj=c(1,.5),cex=.9)}


}
pngfile<-file.path(out,'FigureS1_expanded_selection.png');pdffile<-file.path(out,'FigureS1_expanded_selection.pdf')
stopifnot(!file.exists(pngfile),!file.exists(pdffile))
png(pngfile,width=1430,height=1650,res=220,type='cairo');render();dev.off()
cairo_pdf(pdffile,width=6.5,height=7.5);render();dev.off()
cat('Expanded aggregate selection diagram generated, no model fitting.\n')
