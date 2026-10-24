# Repairs only completed aggregate output. Never opens microdata, fits or checkpoints.
release_oof_safely <- function(run) {
 share<-file.path(run,'SEND_BACK');private<-file.path(run,'LOCAL_ONLY')
 if(!dir.exists(share)||!dir.exists(private)) stop('Expected completed private run directories.')
 status<-readLines(file.path(share,'STATUS.txt'),warn=FALSE)
 if(length(status)!=1||!grepl('^(SUCCESS|UNSTABLE):',status)) stop('Run is not terminal; wait for completion.')
 path<-file.path(share,'oof_risk_change.csv');z<-read.csv(path,stringsAsFactors=FALSE)
 needed<-c('model','comparison','count_rounded_10','proportion','ci_lower','ci_upper')
 if(!all(needed %in% names(z))||anyNA(z$model)||!nrow(z)) stop('Invalid aggregate schema.')
 for(model in unique(z$model)) {
  ix<-which(z$model==model)
  if(length(ix)!=3||!setequal(z$comparison[ix],c('absolute','increase','decrease'))) stop('Invalid linked categories.')
  if(any(z$count_rounded_10[ix]=='SUPPRESSED'|is.na(z$proportion[ix]))) {
   z$count_rounded_10[ix]<-'SUPPRESSED'
   z$proportion[ix]<-NA_real_;z$ci_lower[ix]<-NA_real_;z$ci_upper[ix]<-NA_real_
  }
 }
 backup<-file.path(private,'oof_risk_change_before_complementary_suppression.csv')
 if(!file.exists(backup)&&!file.copy(path,backup,overwrite=FALSE)) stop('Cannot preserve original aggregate privately.')
 temp<-tempfile('safe_oof_',tmpdir=share,fileext='.csv')
 write.csv(z,temp,row.names=FALSE)
 if(!file.rename(temp,path)) stop('Cannot atomically replace released aggregate.')
 writeLines(c('PASS: complementary suppression checked for all three linked risk-change categories.',
 'Counts, proportions and intervals are jointly suppressed when any original linked category was suppressed.',
 'No model refit or HRS individual-data access was needed.'),file.path(share,'OOF_DISCLOSURE_CHECK.txt'))
 invisible(TRUE)
}
if(sys.nframe()==0L) {
 args<-commandArgs(trailingOnly=TRUE)
 if(length(args)!=1) stop('Usage: Rscript release_oof_safely.R COMPLETED_RUN_DIRECTORY')
 release_oof_safely(args[1])
 cat('Aggregate suppression repair completed. Original is preserved in LOCAL_ONLY.\n')
}
