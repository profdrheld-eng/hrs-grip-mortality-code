# Aggregate-only, post hoc sensitivity to hypothetical equivalence margins.
margin_sensitivity <- function(metrics,margins=list(
  auc=c(.0001,.0002,.0005,.001,.002,.005,.01,.02,.05),
  brier=c(.00001,.00002,.00005,.0001,.0002,.0005,.001,.002,.005))) {
  required <- c('metric','target','optimism_corrected','ci_lower','ci_upper',
                'outer_success','nested_success','status')
  if(!all(required %in% names(metrics))) stop('Required metric columns missing.')
  if(!setequal(names(margins),c('auc','brier'))) stop('Both AUC and Brier margins required.')
  rows <- lapply(c('auc','brier'),function(name) {
    z <- metrics[metrics$metric==name & metrics$target=='prior_minus_current',,drop=FALSE]
    if(nrow(z)!=1L || anyNA(z[,required]) || z$status!='OK') stop('Unique valid paired difference required.')
    numeric <- c('optimism_corrected','ci_lower','ci_upper','outer_success','nested_success')
    if(!all(vapply(z[,numeric],is.numeric,TRUE)) ||
       any(!is.finite(as.matrix(z[,numeric]))) || z$ci_lower>z$ci_upper ||
       z$nested_success<1 || z$outer_success<z$nested_success) stop('Invalid metric values.')
    delta <- margins[[name]]
    if(!is.numeric(delta) || !length(delta) || any(!is.finite(delta)) || any(delta<=0) ||
       anyDuplicated(delta)) stop('Margins must be distinct finite positive numbers.')
    data.frame(metric=name,delta=sort(delta),estimate=z$optimism_corrected,
      ci_lower=z$ci_lower,ci_upper=z$ci_upper,
      interval_contained=z$ci_lower> -sort(delta) & z$ci_upper<sort(delta),
      boundary_infimum=max(abs(c(z$ci_lower,z$ci_upper))),
      rule='ci_lower > -delta AND ci_upper < delta',
      interpretation='Hypothetical margin; not a validated clinical threshold')
  })
  do.call(rbind,rows)
}

equivalence_main <- function(args) {
  if(length(args)!=2L) stop('Usage: Rscript hrs_equivalence_sensitivity.R VALIDATION_DIR REPORT_DIR')
  input <- normalizePath(args[1],mustWork=TRUE)
  output <- normalizePath(args[2],mustWork=FALSE)
  # Reports must never overwrite the source directory or its ancestors.
  if(input==output || startsWith(input,paste0(output,'/'))) stop('Use a separate report directory.')
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  status <- file.path(output,'STATUS.txt')
  writeLines('INCOMPLETE: older reports are stale until SUCCESS.',status)
  paths <- file.path(input,c('STATUS.txt','READ_ME.txt','validated_metrics.csv'))
  if(!all(file.exists(paths)) || !startsWith(readLines(paths[1],warn=FALSE)[1],'SUCCESS:'))
    stop('A successful source validation is required.')
  note <- readLines(paths[2],warn=FALSE)
  count_line <- grep('^Bootstrap repetitions: [0-9]+$',note,value=TRUE)
  if(length(count_line)!=1L || !any(grepl('95%',note,fixed=TRUE))) stop('Source CI metadata missing.')
  B <- as.numeric(sub('^Bootstrap repetitions: ','',count_line))
  metrics <- read.csv(paths[3],stringsAsFactors=FALSE)
  result <- margin_sensitivity(metrics)
  paired <- metrics[metrics$target=='prior_minus_current' & metrics$metric %in% c('auc','brier'),]
  if(!is.finite(B) || B<20 || any(paired$nested_success<ceiling(.9*B)) ||
     any(paired$outer_success>B)) stop('Insufficient source bootstrap success.')
  write.csv(result,file.path(output,'margin_sensitivity.csv'),row.names=FALSE)
  hashes <- tools::md5sum(c(paths,if(file.exists('scripts/hrs_equivalence_sensitivity.R'))
    'scripts/hrs_equivalence_sensitivity.R'))
  write.csv(data.frame(path=names(hashes),md5=unname(hashes)),
    file.path(output,'source_hashes.csv'),row.names=FALSE)
  fmt <- function(x) formatC(x,format='f',digits=6)
  text <- c('# Sensitivität gegenüber möglichen Gleichwertigkeitsgrenzen','',
    '## Ergebnis und Bedeutung','',
    'Diese nachträgliche Auswertung zeigt, bei welchen hypothetischen Grenzen die bereits vorhandenen 95%-Intervalle vollständig innerhalb des Bereichs liegen. Sie legt keine medizinisch relevante Grenze fest und erklärt die Modelle nicht pauschal für gleichwertig. Alle untersuchten Grenzen werden berichtet; es wird keine erfolgreiche Grenze ausgewählt.',
    '',paste('Quelle:',input),paste('Auswertung erstellt:',format(Sys.time(),tz='Europe/Berlin',usetz=TRUE)),
    '', 'Verglichen wird das Modell mit früherer und aktueller Handkraft mit dem Modell mit aktueller Handkraft; beide enthalten die übrigen festgelegten Kovariaten. Differenzen bedeuten immer **mit früherer Messung minus ohne**. Positive AUC-Differenzen sind günstig, negative Brier-Differenzen sind günstig.',
    '', '## Grenzenraster','',
    'Das Raster folgt einer einfachen 1-2-5-Abstufung über mehrere Größenordnungen. Es sind transparente Rechenszenarien, keine aus der Literatur validierten Relevanzgrenzen. Die Auswahl und Auswertung erfolgen nach Kenntnis der Ergebnisse. AUC und Brier werden getrennt beurteilt; gleiche Zahlen bedeuten bei beiden Kennzahlen nicht dieselbe praktische Bedeutung.')
  for(name in c('auc','brier')) {
    r <- result[result$metric==name,]
    label <- if(name=='auc') 'AUC' else 'Brier-Score'
    text <- c(text,'',paste('##',label),'',
      paste0('Korrigierte Differenz: **',fmt(r$estimate[1]),'**, 95%-Intervall: **[',
             fmt(r$ci_lower[1]),'; ',fmt(r$ci_upper[1]),']**.'),'',
      '| Hypothetische symmetrische Grenze | Gesamtes 95%-Intervall strikt innerhalb? |',
      '|---|---|',paste0('| ±',fmt(r$delta),' | ',ifelse(r$interval_contained,'Ja','Nein'),' |'),'',
      paste0('Rechnerische Untergrenze der erforderlichen Halbbreite: **',
        formatC(r$boundary_infimum[1],format='f',digits=9),
        '** (gerundet). Für striktes Enthaltensein muss δ größer als der ungerundete Wert sein. Das ist eine aus den Ergebnissen abgeleitete Präzisionsbeschreibung, keine empfohlene Relevanzgrenze.'))
  }
  text <- c(text,'','## Entscheidungsregel und Grenzen','',
    '- Ein Intervall ist enthalten, wenn seine untere Grenze strikt über −δ und seine obere Grenze strikt unter +δ liegt. Grenzberührung zählt nicht als erfüllt. Berechnet wird mit ungerundeten Zahlen.',
    '- „Nein“ bedeutet: Für diese enge Grenze reicht die vorliegende Eingrenzung nicht aus. Es bedeutet nicht, dass ein praktisch wichtiger Unterschied nachgewiesen wurde.',
    '- „Ja“ bedeutet: Bedingt auf genau diese Grenze und die Analyseannahmen ist das Intervall vollständig eingeschlossen. Es begründet die gewählte Grenze nicht.',
    '- Verwendet werden die bestehenden gepaarten, optimismusbereinigten 95%-Bootstrap-Intervalle. Dies ist eine konservative Intervallprüfung, kein neu berechneter TOST mit p-Werten. Beim klassischen TOST mit zwei einseitigen Tests auf dem 5%-Niveau wird ein 90%-Intervall verwendet; dieses wird hier nicht aus dem 95%-Intervall rekonstruiert.',
    paste0('- Die nominale Abdeckung hängt vom ursprünglichen verschachtelten Bootstrap ab. Angeforderte äußere Wiederholungen laut Quellbericht: ',B,'. Weitere Einstellungen stehen im ursprünglichen READ_ME.txt; numerische Stabilität an knappen Grenzen ist nicht garantiert.'),
    '- Es wurden weder Modelle neu angepasst noch Individualdaten gelesen. Die Auswertung verwendet ausschließlich aggregierte Validierungsberichte. Quellberichte bleiben unverändert; ihre MD5-Prüfsummen stehen in source_hashes.csv.',
    '- Die Rasterzeilen sind voneinander abhängige Sensitivitätsszenarien, keine unabhängigen Bestätigungstests. Es wird keine nachträglich ausgewählte Grenze als vorab spezifiziert ausgegeben.',
    '- Gleiche mittlere Modellgüte bedeutet nicht gleiche individuelle Prognosen, gleiche klinische Entscheidungen oder gleiche Leistung in jeder Untergruppe. Die Kennzahlen sind keine Prozentpunkte Sterberisiko.',
    '- Intervallzeit-Annäherung, Zensierungsannahmen, selektierte Kohorte und fehlende externe Validierung bleiben unverändert. Diese Auswertung deckt keine systematischen Fehler ab.',
    '', '## Empfehlung','',
    'Die vollständige Sensitivitätstabelle als explorative Ergänzung berichten. Ohne unabhängig begründete klinische oder wissenschaftliche Relevanzgrenzen lautet die Schlussfolgerung: kein erkennbarer Zusatznutzen bei eng eingegrenzten Kennzahlunterschieden. Für einen künftigen formalen Gleichwertigkeitsanspruch zuerst den Anwendungszweck und die Grenzen begründen; bei Nähe zu Intervallgrenzen die Bootstrap-Stabilität prüfen.',
    '', '## Methodische Quellen','',
    '- [Lakens (2017): Equivalence Tests](https://pmc.ncbi.nlm.nih.gov/articles/PMC5502906/), Begründung von Äquivalenzgrenzen und Unterschied zwischen Nichtsignifikanz und Äquivalenz.',
    '- [Assel, Sjoberg und Vickers (2017): The Brier score does not evaluate the clinical utility of diagnostic tests or prediction models](https://pmc.ncbi.nlm.nih.gov/articles/PMC6460786/), Grenze zwischen statistischer Vorhersagegüte und klinischem Nutzen.')
  writeLines(text,file.path(output,'BERICHT.md'),useBytes=TRUE)
  writeLines('SUCCESS: aggregate margin sensitivity completed; no clinical equivalence claim.',status)
  message('Aggregate sensitivity report written. No models refitted or individual data read.')
  invisible(result)
}

if(sys.nframe()==0L) equivalence_main(commandArgs(trailingOnly=TRUE))
