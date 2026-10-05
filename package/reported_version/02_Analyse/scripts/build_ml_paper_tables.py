"""Reproduce manuscript tables from aggregate reports only; standard library only."""
import csv
import hashlib
import json
import sys
from pathlib import Path


def build(source, output):
    source, output = Path(source).resolve(), Path(output).resolve()
    if output == source or output in source.parents:
        raise ValueError('Separate manuscript output required')
    assert (source / 'STATUS.txt').read_text().startswith('SUCCESS:')
    names = ['benchmark_metrics', 'benchmark_paired', 'benchmark_tuning',
             'association_curves', 'simulation_summary', 'simulation_status']
    data = {name: list(csv.DictReader((source / (name + '.csv')).open())) for name in names}
    m = [r for r in data['benchmark_metrics'] if float(r['fraction']) == 1]
    assert len(m) == 12
    assert len({(r['model'], r['target'], r['arm']) for r in m}) == 12
    assert all(r['status'] == 'OK' for r in m)
    contrasts = [r for r in data['benchmark_paired'] if float(r['fraction']) == 1 and r['metric'] in ('auc', 'brier')]
    assert len(contrasts) == 40
    lookup = {(r['model'], r['target'], r['arm']): r for r in m}
    for r in contrasts:
        a = lookup[r['model'], r['target'], r['arm']]
        if r['contrast'] == 'prior_minus_current':
            b = lookup[r['model'], 'current', r['arm']]
        elif r['contrast'] == 'augmented_minus_original':
            b = lookup[r['model'], r['target'], 'original']
        else:
            b = lookup[r['reference_model'], r['target'], r['arm']]
        assert abs(float(a[r['metric']]) - float(b[r['metric']]) - float(r['difference'])) < 1e-12
    label = {'weibull': 'Weibull splines', 'neural': 'Neural AFT', 'xgboost': 'XGBoost AFT'}
    def fmt(x):
        return f'{float(x):.6f}'
    def interval(r, value, lo, hi):
        return f'{fmt(r[value])} ({fmt(r[lo])} to {fmt(r[hi])})'
    lines = ['# Exploratory model comparison tables', '',
        '## Table 4. Internally validated five-year mortality prediction across all model variants', '',
        '| Model | Grip predictors | Training | AUC (95% CI) | Brier (95% CI) | Calibration intercept | Calibration slope |',
        '|---|---|---|---|---|---|---|']
    for r in m:
        lines.append('| ' + ' | '.join([label[r['model']], 'Current' if r['target'] == 'current' else 'Current + prior',
            'Original' if r['arm'] == 'original' else 'Original + synthetic',
            interval(r, 'auc', 'auc_lower', 'auc_upper'), interval(r, 'brier', 'brier_lower', 'brier_upper'),
            fmt(r['calibration_intercept']), fmt(r['calibration_slope'])]) + ' |')
    lines += ['', 'AFT, accelerated failure time; AUC, cumulative/dynamic area under the receiver operating characteristic curve; CI, confidence interval. All models also include age, sex, smoking and self-rated health. Five outer household folds and three inner folds were used. “Full training” uses all available outer-training households (approximately 80% of the cohort), not an in-sample fit on the entire cohort. Higher AUC and lower Brier indicate better performance. Calibration targets are 0 and 1. Intervals are 500-resample conditional household-bootstrap intervals of pooled out-of-fold predictions, not full-pipeline uncertainty intervals or external validation. Synthetic records enter training only and do not increase the independent sample size.', '',
        '## Supplementary Table S4. Paired performance differences', '',
        '| Contrast | Model | Grip predictors | Training | Metric | Difference (conditional 95% CI) |',
        '|---|---|---|---|---|---|']
    for r in contrasts:
        lines.append('| ' + ' | '.join([r['contrast'].replace('_', ' '), label[r['model']], r['target'], r['arm'],
            r['metric'], interval(r, 'difference', 'ci_lower', 'ci_upper')]) + ' |')
    lines += ['', 'AUC, area under the receiver operating characteristic curve; CI, confidence interval. Differences use the first term minus the reference term. Positive AUC and negative Brier differences favor the first term. Model contrasts use Weibull splines as reference. These intervals are descriptive, unadjusted and conditional on the existing fitted folds; they do not establish equality or algorithm-wide superiority.', '',
        '## Supplementary Table S5. Prior-grip incremental AUC in controlled simulations without augmentation', '',
        '| Scenario | Model | Mean difference | Monte Carlo SE | Empirical 2.5th percentile | Empirical 97.5th percentile | Successful repetitions |',
        '|---|---|---|---|---|---|---|']
    for r in data['simulation_summary']:
        if r['contrast'] == 'prior_minus_current' and r['arm'] == 'original' and r['metric'] == 'auc':
            lines.append('| ' + ' | '.join([r['scenario'], label[r['model']], fmt(r['mean_difference']), fmt(r['mcse']),
                fmt(r['empirical_q025']), fmt(r['empirical_q975']), r['successful']]) + ' |')
    lines += ['', 'AUC, area under the receiver operating characteristic curve; SE, standard error. Differences compare current + prior grip with current grip. Each scenario generated 100 independent data sets of 6,300 observations using a Weibull event-time mechanism. The null scenario contains no additional history signal, the small scenario a linear signal and the interaction scenario an age-by-history interaction. Empirical percentiles describe variation across simulated data sets, not confidence intervals for the mean. These simulations do not estimate equivalence-test power or type-I error. All augmentation and model-family simulation contrasts remain available in ml_simulation_summary.csv.', '',
        '## Supplementary Table S6. Association sensitivity to synthetic augmentation at a −5 kg change', '',
        '| Conditioning | Training | Standardized five-year risk, % | Risk difference versus zero change, percentage points (95% CI) |',
        '|---|---|---|---|']
    for r in data['association_curves']:
        if float(r['change_kg']) == -5 and r['arm'] in ('original', 'augmented'):
            rd = f"{100*float(r['risk_difference']):.2f} ({100*float(r['rd_lower']):.2f} to {100*float(r['rd_upper']):.2f})"
            lines.append(f"| {'Earlier grip fixed' if r['kind']=='change' else 'Current grip fixed'} | {r['arm']} | {100*float(r['risk5']):.2f} | {rd} |")
    lines += ['', 'CI, confidence interval. Pointwise intervals use 500 real-household resamples with generator and model refitting; augmented training adds one synthetic record per original record. Standardization uses the original real-data target, not the synthetic cohort. These are conditional associations, not causal effects. The two conditioning schemes answer different questions. Synthetic augmentation is a generator-dependent sensitivity analysis, not additional independent evidence.', '']
    output.mkdir(parents=True, exist_ok=True)
    (output / 'ML_TABLES.md').write_text('\n'.join(lines))
    for name in names:
        (output / ('ml_' + name + '.csv')).write_bytes((source / (name + '.csv')).read_bytes())
    manifest = {name + '.csv': hashlib.sha256((source / (name + '.csv')).read_bytes()).hexdigest() for name in names}
    (output / 'ml_source_sha256.json').write_text(json.dumps({'source': str(source), 'sha256': manifest}, indent=2))
    failures = sum(int(r['failed_fits']) for r in data['benchmark_tuning'])
    (output / 'ml_checks.json').write_text(json.dumps({'variants': len(m), 'paired_metric_contrasts': len(contrasts),
        'all_contrasts_recomputed': True, 'inner_fit_failures': failures,
        'simulation_status': {status: sum(r['status'] == status for r in data['simulation_status']) for status in ('OK','FAILED')}}, indent=2))


if __name__ == '__main__':
    if len(sys.argv) != 3:
        raise SystemExit('Usage: build_ml_paper_tables.py AGGREGATE_RESULT_DIR MANUSCRIPT_DIR')
    build(*sys.argv[1:])
