# Human-run comparison of an EXISTING parameter export against published S3.
# No HRS input data, fitting, prediction, source/eval, network or writes.
# Only model/component check counts and PASS/FAIL are printed.
expected_primary_parameters <- function() {
  read.csv(text = '
model,component,term,value
current,coefficient,(Intercept),2.9837303009403477
prior,coefficient,(Intercept),3.2255643236380531
current,coefficient,"splines::ns(age, df = 3)1",-0.94662581857449912
prior,coefficient,"splines::ns(age, df = 3)1",-0.95821339131259176
current,coefficient,"splines::ns(age, df = 3)2",-2.4498049295762172
prior,coefficient,"splines::ns(age, df = 3)2",-2.477613109787026
current,coefficient,"splines::ns(age, df = 3)3",-2.4923063887409973
prior,coefficient,"splines::ns(age, df = 3)3",-2.5203628477670081
current,coefficient,sex2,0.70600562337070416
prior,coefficient,sex2,0.67715729668219082
current,coefficient,smokeformer,-0.23975046621668222
prior,coefficient,smokeformer,-0.23436582831057967
current,coefficient,smokecurrent,-0.56093017083567143
prior,coefficient,smokecurrent,-0.55553596908315761
current,coefficient,health2,-0.22699225288157882
prior,coefficient,health2,-0.22758587373603056
current,coefficient,health3,-0.43753129096441395
prior,coefficient,health3,-0.43906458134565912
current,coefficient,health4,-0.70292951321631392
prior,coefficient,health4,-0.70674387215748236
current,coefficient,health5,-1.2041191056814677
prior,coefficient,health5,-1.1995487780803682
current,coefficient,"splines::ns(grip14, df = 3)1",1.5008898667013748
prior,coefficient,"splines::ns(grip14, df = 3)1",1.5790180534207778
current,coefficient,"splines::ns(grip14, df = 3)2",6.6097281188727406
prior,coefficient,"splines::ns(grip14, df = 3)2",7.8815225074175572
current,coefficient,"splines::ns(grip14, df = 3)3",8.3745914752899129
prior,coefficient,"splines::ns(grip14, df = 3)3",10.123843710494626
prior,coefficient,"splines::ns(grip10, df = 3)1",-0.13763694265781265
prior,coefficient,"splines::ns(grip10, df = 3)2",-1.8331656464410169
prior,coefficient,"splines::ns(grip10, df = 3)3",-2.1198823481493863
current,scale,Weibull AFT scale σ,0.69733953458459874
prior,scale,Weibull AFT scale σ,0.69675696316755875
', stringsAsFactors = FALSE, check.names = FALSE)
}

check_primary_parameters <- function(models) {
  stopifnot(is.list(models), !is.object(models),
            identical(names(models), c('current', 'prior')))
  expected <- expected_primary_parameters()
  allowed <- c('terms', 'coefficients', 'scale', 'xlevels', 'contrasts',
               'distribution', 'horizon_years')
  result <- list()
  for (name in c('current', 'prior')) {
    model <- models[[name]]
    stopifnot(is.list(model), !is.object(model),
              length(names(model)) == length(allowed),
              setequal(names(model), allowed),
              identical(model$distribution, 'weibull'),
              is.numeric(model$horizon_years),
              length(model$horizon_years) == 1L,
              is.finite(model$horizon_years), model$horizon_years == 5)
    for (component in c('coefficient', 'scale')) {
      wanted <- expected[expected$model == name & expected$component == component, ]
      actual <- if (component == 'scale') model$scale else model$coefficients
      stopifnot(is.numeric(actual), !is.object(actual),
                length(actual) == nrow(wanted), !anyNA(actual),
                all(is.finite(actual)))
      if (component == 'coefficient') {
        stopifnot(identical(names(actual), wanted$term))
      } else {
        stopifnot(actual > 0)
      }
      matches <- abs(unname(actual) - wanted$value) <= 1e-12
      result[[length(result) + 1L]] <- data.frame(
        model = name, component = component,
        n_checked = length(actual), n_matching = sum(matches),
        status = if (all(matches)) 'PASS' else 'FAIL',
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, result)
}

if (sys.nframe() == 0L) {
  # Human execution only. The existing RDS must be the report_model export,
  # not a fitted survreg object or participant data. Nothing is written.
  result <- tryCatch({
    args <- commandArgs(trailingOnly = TRUE)
    stopifnot(length(args) == 1L,
              basename(args[1L]) == 'primary_prediction_parameters.rds')
    models <- suppressWarnings(readRDS(args[1L]))
    check_primary_parameters(models)
  }, error = function(e) {
    # Do not print paths, condition messages, parameter values or object names.
    data.frame(model = 'NOT_CHECKED', component = 'STRUCTURE_OR_INPUT',
               n_checked = 0L, n_matching = 0L, status = 'CHECK_FAILED',
               stringsAsFactors = FALSE)
  })
  write.csv(result, stdout(), row.names = FALSE)
  quit(save = 'no', status = if (all(result$status == 'PASS')) 0L else 2L)
}
