# Read-only dependency check. Does not install packages or read HRS inputs.
expected <- c(survival='3.8.6',rpart='4.1.27',xgboost='3.2.1.1')
cat('R:',as.character(getRversion()),' (reported version: 4.6.0)\n')
for(p in names(expected)) {
  actual <- if(requireNamespace(p,quietly=TRUE)) as.character(packageVersion(p)) else 'UNAVAILABLE'
  cat(p,':',actual,' (reported version:',expected[p],')\n')
}
cat('Cairo graphics:',capabilities('cairo'),'\n')
cat('Diagnostics SHA-256 helper:',file.exists('/usr/bin/shasum'),'\n')
missing <- names(expected)[!vapply(names(expected),requireNamespace,TRUE,quietly=TRUE)]
if(length(missing)) quit(status=1)
if(as.character(getRversion())!='4.6.0' || any(vapply(names(expected),function(p)
  as.character(packageVersion(p))!=expected[p],TRUE))) quit(status=2)
