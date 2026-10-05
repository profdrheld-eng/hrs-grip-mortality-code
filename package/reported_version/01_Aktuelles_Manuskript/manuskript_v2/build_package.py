"""Build manuscript tables from existing aggregate outputs; never read microdata."""
from pathlib import Path
import csv, json, hashlib, shutil, re
import argparse
parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument("old_aggregate_dir")
parser.add_argument("inference_dir")
parser.add_argument("full_ml_dir")
parser.add_argument("new_output_dir")
parser.add_argument("prior_inference_verification")
args=parser.parse_args()
OLD=Path(args.old_aggregate_dir).resolve()
INF=Path(args.inference_dir).resolve()
FULL=Path(args.full_ml_dir).resolve()
OUT=Path(args.new_output_dir).resolve()
OUT.mkdir(parents=True,exist_ok=False)
SOURCES={}
def read(path):
    SOURCES[str(path)]=hashlib.sha256(path.read_bytes()).hexdigest()
    return list(csv.DictReader(path.open()))
def savecsv(name, rows):
    with (OUT/name).open('w',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
def table(title,headers,rows,note):
    return '\n'.join(['## '+title,'','| '+' | '.join(headers)+' |','|'+'|'.join(['---']*len(headers))+'|']+['| '+' | '.join(str(v) for v in r)+' |' for r in rows]+['','**Note.** '+note,''])
def f(v,n=6):return f'{float(v):.{n}f}'
def ci(r,x,lo,hi,n=6,scale=1):return f'{float(r[x])*scale:.{n}f} ({float(r[lo])*scale:.{n}f} to {float(r[hi])*scale:.{n}f})'
MODEL={'weibull':'Weibull splines','neural':'Neural AFT','xgboost':'XGBoost AFT'}
TARGET={'current':'Current','prior':'Current + prior','both':'Both','':'Not applicable'}
ARM={'original':'Original','augmented':'Original + synthetic','both':'Paired arms','augmented_minus_original':'Augmented minus original','':'Not applicable'}
def label(r):return [MODEL.get(r['model'],r['model']),TARGET.get(r['target'],r['target']),ARM.get(r['arm'],r['arm'])]
common='AFT, accelerated failure time; AUC, area under the receiver operating characteristic curve; CI, confidence interval. Current and prior refer to 2014 and 2010 handgrip strength. All models include age, sex, smoking and self-rated health. Higher AUC and lower Brier score indicate better performance.'
conditional='Household bootstrap uncertainty is conditional on fixed out-of-fold predictions, folds, tuning choices and censoring weights; it excludes full retraining uncertainty. Evaluation uses real held-out participants only.'
t1=read(OLD/'table1.csv');savecsv('table1.csv',t1)
t1rows=[]
for r in t1:
    if r['mean'] not in ('','NA'):value=f(r['mean'],1)+' ± '+f(r['sd'],1)
    elif r['count_rounded_10'] not in ('','NA'):value=r['count_rounded_10']+(' ('+f(r['percent_approx'],1)+')' if r['percent_approx'] not in ('','NA') else '')
    else:value=r['denominator_rounded_10'] if r['variable']=='n' else 'Suppressed'
    t1rows.append([r['label']+(': '+r['level'] if r['level'] not in ('','NA') else ''),value])
main=table('Table 1. Characteristics of the analytical cohort',['Characteristic','Mean ± SD or approximately n (%)'],t1rows,'SD, sample standard deviation. Counts are rounded to tens; percentages use rounded counts and denominator and may not sum to 100%. Exact analytical sample size is 6,307. Counts below ten are suppressed, with complementary suppression where needed. Continuous summaries are arithmetic means ± SD. Change is 2014 minus 2010 grip; negative values indicate loss. Outcome rows describe ascertainment, not Kaplan–Meier mortality estimates. This is an unweighted selected complete-case cohort.')
p=read(OLD/'prognosekennzahlen.csv');savecsv('table2_source.csv',p);lk={(r['metric'],r['target']):r for r in p}
metrics=[('auc','AUC'),('brier','Brier score'),('calibration_intercept','Calibration intercept'),('calibration_slope','Calibration slope'),('interval_log_score','Mean interval log score')]
main+=table('Table 2. Original optimism-corrected five-year prediction',['Measure','Current (95% CI)','Current + prior (95% CI)','Paired difference (95% CI)'],[[name]+[ci(lk[k,t],'optimism_corrected','ci_lower','ci_upper') for t in ['current','prior','prior_minus_current']] for k,name in metrics],common+' Differences are current + prior minus current. Both are Weibull spline models. Percentile 95% CIs use 200 outer household bootstrap repetitions with 50 inner repetitions for nested optimism correction. All 200 outer repetitions yielded valid paired optimism estimates and nested-corrected estimates for each reported measure. Calibration targets are zero for intercept and one for slope; higher interval log score is better. These are distinct from the nested cross-validation results in Table 3.')
m=read(INF/'benchmark_metrics.csv');savecsv('table3_source.csv',m)
m.sort(key=lambda r:(list(MODEL).index(r['model']),['current','prior'].index(r['target']),['original','augmented'].index(r['arm'])))
main+=table('Table 3. Five-year performance of all 12 model variants',['Model','Grip predictors','Training','AUC (95% CI)','Brier score (95% CI)','Calibration intercept','Calibration slope'],[label(r)+[ci(r,'auc','auc_lower','auc_upper'),ci(r,'brier','brier_lower','brier_upper'),f(r['calibration_intercept'],3),f(r['calibration_slope'],3)] for r in m],common+' Pooled predictions use five outer and three inner household-grouped folds. Percentile 95% CIs use 5,000 household bootstrap draws. Calibration parameters are point estimates, with targets zero and one. '+conditional+' Synthetic training is original plus generated observations, not synthetic-only training. Six decimal places preserve small between-model differences and do not imply measurement precision.')
(OUT/'TABLES.md').write_text('# Main manuscript tables\n\n'+main)
supp=''
curves=[]
for name,cond in [('veraenderung_kurve.csv','Earlier grip fixed'),('vorgeschichte_kurve.csv','Current grip fixed')]:
    for r in read(OLD/name):curves.append({'conditioning':cond,**r})
savecsv('tableS1_associations.csv',curves)
supp+=table('Table S1. Complete conditional association contrasts',['Conditioning','Change (kg)','Risk, % (95% CI)','Difference from zero change, percentage points (95% CI)'],[[r['conditioning'],r['change_kg'],ci(r,'risk5','risk_ci_lower','risk_ci_upper',2,100),ci(r,'risk_difference','rd_ci_lower','rd_ci_upper',2,100)] for r in curves],'CI, confidence interval. Standardized model predictions with pointwise percentile 95% CIs from 500 household refits. Change is 2014 minus 2010 grip. Target subsets contain approximately 6,290 participants for fixed earlier grip and 6,310 for fixed current grip, rounded to tens. Distinct models and targets preclude causal or decomposition interpretations. Figure 2 uses these original association runs; augmentation sensitivity uses a separate bootstrap run in Table S6.')
e=read(INF/'exploratory_equivalence/conditional_tost.csv');savecsv('tableS3_equivalence.csv',e)
keys=['contrast','model','target','arm','reference_model','metric'];unique={tuple(r[k] for k in keys):r for r in e}
contrast={'prior_minus_current':'Current + prior minus current','model_minus_weibull':'Named model minus Weibull','augmented_minus_original':'Augmented minus original'}
pairrows=[]
for r in unique.values():pairrows.append({'comparison':contrast[r['contrast']],**{k:r[k] for k in keys},**{k:r[k] for k in ['estimate','ci90_lower','ci90_upper']}})
savecsv('tableS2_paired.csv',pairrows)
supp+=table('Table S2. Paired performance differences',['Comparison','Model','Grip predictors','Training','Measure','Difference (90% CI)'],[[r['comparison']]+label(r)+[r['metric'].upper() if r['metric']=='auc' else 'Brier',ci(r,'estimate','ci90_lower','ci90_upper')] for r in pairrows],common+' Unadjusted basic 90% bootstrap CIs use 5,000 paired household draws. '+conditional+' Each comparison holds the other design choices fixed. For a prior-minus-current comparison the two predictor sets are contrasted; for augmented-minus-original the two training arms are contrasted. These are not multiplicity-adjusted simultaneous intervals.')
supp+=table('Table S3. Complete exploratory equivalence grid',['Comparison','Model','Grip predictors','Training','Measure','Margin ±δ','Lower-bound p','Upper-bound p','TOST p','Holm p','Holm criterion met'],[[contrast[r['contrast']]]+label(r)+[r['metric'].upper(),f(r['margin']),f(r['p_lower']),f(r['p_upper']),f(r['p_tost']),f(r['p_holm']),'Yes' if r['equivalent_holm']=='TRUE' else 'No'] for r in e],common+' TOST, two one-sided tests. Hypothetical symmetric margins are not clinically justified thresholds. Approximate centered paired-bootstrap boundary tests use 5,000 draws and a plus-one correction, giving p-value resolution 1/5,001. Holm correction covers 20 contrasts separately within each metric and margin, not all 360 tests together. Criterion: adjusted p < 0.05. '+conditional+' The corresponding unadjusted 90% intervals are in Table S2. Neither passing nor failing establishes exact equality, clinical interchangeability or a clinically meaningful difference. All margins are reported; no preferred margin was chosen from these results.')
learning=read(FULL/'benchmark_metrics.csv');savecsv('tableS4_learning.csv',learning)
supp+=table('Table S4. Learning curves',['Outer-training fraction','Model','Grip predictors','Training','AUC (95% CI)','Brier (95% CI)'],[[r['fraction']]+label(r)+[ci(r,'auc','auc_lower','auc_upper'),ci(r,'brier','brier_lower','brier_upper')] for r in learning],common+' Fractions refer to available outer-training households, approximately 80% of the cohort at fraction 1, not the entire cohort. These original learning-curve runs used 500 conditional percentile bootstrap draws; their full-fraction point estimates match Table 3 but intervals need not. '+conditional+' Three sizes do not establish a performance plateau.')
sim=read(FULL/'simulation_summary.csv');savecsv('tableS5_simulation.csv',sim)
supp+=table('Table S5. Controlled simulation results',['Scenario','Comparison','Model','Grip predictors','Training','Measure','Mean difference','Monte Carlo SE','Empirical 2.5th percentile','Empirical 97.5th percentile','Successful/requested'],[[r['scenario'],contrast[r['contrast']]]+label(r)+[('Interval log score' if r['metric']=='interval_log_score' else r['metric'].upper()),f(r['mean_difference']),f(r['mcse']),f(r['empirical_q025']),f(r['empirical_q975']),r['successful']+'/'+r['requested']] for r in sim],common+' SE, standard error. Higher interval log score indicates better performance. Each scenario comprises 100 independent generated data sets of 6,300 observations. Monte Carlo SE is the repetition standard deviation divided by the square root of successful repetitions. Empirical percentiles describe repetition variability, not a confidence interval for the mean. All 12 variants use nested validation. These simulations evaluate specified mechanisms, not equivalence-test power or type-I error.')
a=read(FULL/'association_curves.csv');savecsv('tableS6_augmentation_associations.csv',a)
supp+=table('Table S6. Synthetic augmentation sensitivity of association curves',['Conditioning','Training/contrast','Change (kg)','Risk difference or paired change, percentage points (95% CI)'],[['Earlier grip fixed' if r['kind']=='change' else 'Current grip fixed',ARM.get(r['arm'],r['arm']),r['change_kg'],ci(r,'risk_difference','rd_lower','rd_upper',2,100)] for r in a],'CI, confidence interval. Pointwise percentile 95% intervals use 500 real-household bootstrap resamples, regenerating synthetic observations and refitting at each repetition. Augmentation uses a 1:1 ratio. For augmented minus original, the row is the paired change in risk contrast. Other rows compare the specified change with zero change within the same conditioning model. Predictions are standardized to real observations. These bootstrap intervals differ from Table S1 because they come from a separate run.')
tune=read(INF/'benchmark_tuning.csv');savecsv('tableS7_tuning.csv',tune)
supp+=table('Table S7. Selected full-training fold configurations',['Outer fold','Model','Grip predictors','Training','Synthetic ratio','Training n, rounded','Training events, rounded','Selected parameters','Failed/attempted inner fits'],[[r['fold']]+label(r)+[r['ratio'],r['training_n_rounded'],r['training_events_rounded'],r['parameter'].replace(';', ', '),r['failed_fits']+'/'+r['attempted_fits']] for r in tune],common+' Each row is one outer-fold fit after three-fold inner selection. The ratio is generated observations per original training observation. Counts are rounded; events describe training data, not held-out performance. Boundary selections do not establish that wider tuning would improve or preserve the observed comparison.')
g=read(OLD/'grenzenraster.csv');savecsv('tableS8_original_margins.csv',g)
supp+=table('Table S8. Original bootstrap interval containment',['Measure','Hypothetical margin ±δ','Estimate','95% CI lower','95% CI upper','Strictly contained'],[[r['metric'].upper(),f(r['delta']),f(r['estimate']),f(r['ci_lower']),f(r['ci_upper']),'Yes' if r['interval_contained']=='TRUE' else 'No'] for r in g],'AUC, area under the receiver operating characteristic curve; CI, confidence interval. Descriptive containment of the original optimism-corrected paired 95% interval (Table 2). TOST denotes two one-sided tests. This is distinct from the conditional cross-validation TOST analysis in Table S3. Margins are hypothetical, not clinically justified.')
supp=re.sub(r'^## Table (S\d+)\.', lambda m: '<a id="table-'+m[1].lower()+'"></a>\n\n'+m[0], supp, flags=re.M)
(OUT/'SUPPLEMENTARY_TABLES.md').write_text('# Supplementary tables\n\n'+supp)
for name in ['benchmark_diagnostics.csv','benchmark_calibration.csv','versions.csv','preflight_counts.csv']:savecsv(name,read(INF/name))
# Check every AUC/Brier contrast against the independently stored absolute estimates.
lookup={(r['model'],r['target'],r['arm']):r for r in m};maxerr=0
for r in unique.values():
    model,target,arm,metric=r['model'],r['target'],r['arm'],r['metric']
    if r['contrast']=='prior_minus_current':a,b=lookup[model,'prior',arm],lookup[model,'current',arm]
    elif r['contrast']=='augmented_minus_original':a,b=lookup[model,target,'augmented'],lookup[model,target,'original']
    else:a,b=lookup[model,target,arm],lookup['weibull',target,arm]
    maxerr=max(maxerr,abs(float(a[metric])-float(b[metric])-float(r['estimate'])))
assert len(m)==12 and len(e)==360 and len(unique)==40 and maxerr<1e-12
assert all(r['status']=='OK' for r in m)
# Full inference independent verification already archived, preserve as provenance.
shutil.copyfile(Path(args.prior_inference_verification),OUT/'prior_inference_verification.json')
# Retain the verified aggregate evidence from the targeted reporting follow-up.
for reporting_source in sorted((OUT/'reporting_patch_20261004').glob('*')):
    if reporting_source.is_file():
        SOURCES[str(reporting_source.resolve())]=hashlib.sha256(reporting_source.read_bytes()).hexdigest()
(OUT/'source_sha256.json').write_text(json.dumps(SOURCES,indent=2)+'\n')
(OUT/'table_checks.json').write_text(json.dumps({'model_variants':len(m),'equivalence_tests':len(e),'paired_metric_contrasts':len(unique),'maximum_absolute_contrast_recalculation_error':maxerr,'simulation_summary_rows':len(sim),'all_model_status_OK':True,'source_files':len(SOURCES),'raw_participant_data_read':False},indent=2)+'\n')
print('Built main and supplementary tables; 40 paired differences verified; 360 equivalence rows retained.')
