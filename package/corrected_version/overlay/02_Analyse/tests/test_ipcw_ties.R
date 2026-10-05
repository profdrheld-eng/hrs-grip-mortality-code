# Invented records only. No HRS input or prediction-model fitting.
source('scripts/hrs_prediction_validation.R')
source('scripts/hrs_ml_extension.R')

failures <- character();passed <- 0L
check <- function(label,expr) {
  tryCatch({force(expr);passed<<-passed+1L;cat('PASS:',label,'\n')},
    error=function(e) {
      failures<<-c(failures,label)
      cat('FAIL:',label,':',conditionMessage(e),'\n')
    })
}
near <- function(actual,expected) {
  if(length(actual)!=length(expected)||any(!is.finite(actual))||
     max(abs(actual-expected))>1e-12)
    stop('expected [',paste(expected,collapse=', '),'] but got [',
         paste(signif(actual,12),collapse=', '),']')
}
fixture <- function(time,event,position) {
  lo<-time;hi<-rep(Inf,length(time))
  lo[event]<-time[event]-switch(position,lower=0,midpoint=.125,upper=.25)
  hi[event]<-time[event]+switch(position,lower=.25,midpoint=.125,upper=0)
  data.frame(lo=lo,hi=hi)
}
reference <- function(time,event) {
  d<-data.frame(time=time,event=event)
  as.numeric(survival::rttright(survival::Surv(time,event)~1,
    data=d,times=5,renorm=FALSE))
}
has_survival <- requireNamespace('survival',quietly=TRUE)
if(has_survival) {
  cat('Independent oracle: survival',as.character(packageVersion('survival')),'rttright\n')
} else cat('SKIP: survival::rttright not installed; hand-calculated references remain active.\n')

for(position in c('midpoint','lower','upper')) {
  time<-c(1,1,2,5);event<-c(TRUE,FALSE,TRUE,FALSE)
  d<-fixture(time,event,position);expected<-c(1,0,1.5,1.5)
  check(paste(position,'single tie primary'),near(horizon_weights(d,position)$weight,expected))
  check(paste(position,'single tie ML'),near(ml_weights(d,d,position)$weight,expected))
  check(paste(position,'constant-risk Brier arithmetic'),
    near(sum(horizon_weights(d,position)$weight*.25)/nrow(d),.25))
  if(has_survival) {
    check(paste(position,'primary versus survival'),
      near(horizon_weights(d,position)$weight,reference(time,event)))
    check(paste(position,'ML versus survival'),
      near(ml_weights(d,d,position)$weight,reference(time,event)))
  }

  multiple_time<-c(1,1,1,1,2,2,2,5,5)
  multiple_event<-c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE)
  multiple<-fixture(multiple_time,multiple_event,position)
  multiple_expected<-c(1,1,0,0,7/5,0,0,14/5,14/5)
  check(paste(position,'multiple tied events and censors primary'),
    near(horizon_weights(multiple,position)$weight,multiple_expected))
  check(paste(position,'multiple tied events and censors ML'),
    near(ml_weights(multiple,multiple,position)$weight,multiple_expected))
  if(has_survival) {
    check(paste(position,'multiple primary versus survival'),
      near(horizon_weights(multiple,position)$weight,reference(multiple_time,multiple_event)))
    check(paste(position,'multiple ML versus survival'),
      near(ml_weights(multiple,multiple,position)$weight,reference(multiple_time,multiple_event)))
  }

  untied<-fixture(c(.75,1.25,2,5),event,position)
  check(paste(position,'no ties primary'),near(horizon_weights(untied,position)$weight,expected))
  check(paste(position,'no ties ML'),near(ml_weights(untied,untied,position)$weight,expected))

  # A separate test frame cannot contribute to the censoring risk set.
  heldout<-fixture(c(.5,1,1.5,2,3,5),c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE),position)
  heldout_expected<-c(1,1,1.5,1.5,0,1.5)
  check(paste(position,'ML uses only training censoring distribution'),
    near(ml_weights(d,heldout,position)$weight,heldout_expected))
  expanded<-rbind(heldout,heldout[c(1,2,5),])
  check(paste(position,'changing test-frame composition preserves weights'),
    near(ml_weights(d,expanded,position)$weight[seq_len(nrow(heldout))],heldout_expected))

  horizon<-fixture(c(1,2,5,5),c(TRUE,TRUE,FALSE,FALSE),position)
  check(paste(position,'administrative horizon primary'),near(horizon_weights(horizon,position)$weight,rep(1,4)))
  check(paste(position,'administrative horizon ML'),near(ml_weights(horizon,horizon,position)$weight,rep(1,4)))
}

# Event upper bound at the horizon does not receive an administrative censoring jump.
boundary<-data.frame(lo=c(4.75,5),hi=c(5,Inf))
check('event at upper horizon primary',near(horizon_weights(boundary,'upper')$weight,c(1,1)))
check('event at upper horizon ML',near(ml_weights(boundary,boundary,'upper')$weight,c(1,1)))
# Removing the tied event leaves no uncensored training subject beyond time 1.
train<-data.frame(lo=c(.875,1),hi=c(1.125,Inf))
test<-data.frame(lo=1.875,hi=2.125)
check('ML preserves positivity failure after death-first tie handling',{
  message<-tryCatch({ml_weights(train,test);''},error=function(e) conditionMessage(e))
  stopifnot(identical(message,'IPCW positivity insufficient.'))
})

cat('IPCW tie regression:',passed,'passed;',length(failures),'failed.\n')
if(length(failures)) stop('IPCW tie regression failed: ',paste(failures,collapse='; '))
