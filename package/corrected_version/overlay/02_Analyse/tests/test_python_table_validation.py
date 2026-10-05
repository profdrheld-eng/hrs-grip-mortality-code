"""Synthetic aggregate fixtures only. Never execute builders on project data."""
import copy
import ast
import csv
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
ML = ROOT / '02_Analyse/scripts/build_ml_paper_tables.py'
PACKAGE = ROOT / '01_Aktuelles_Manuskript/manuskript_v2/build_package.py'
MODES = ('normal', 'flag_O', 'env_PYTHONOPTIMIZE')


def write_csv(path, rows, fields=None):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=fields or list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def fixture():
    metrics = []
    for i, model in enumerate(('weibull', 'neural', 'xgboost')):
        for j, target in enumerate(('current', 'prior')):
            for k, arm in enumerate(('original', 'augmented')):
                auc = .7 + i * .01 + j * .001 + k * .002
                brier = .15 - i * .001 - j * .0001 + k * .0002
                metrics.append(dict(fraction='1', model=model, target=target, arm=arm,
                    status='OK', auc=auc, auc_lower=auc-.02, auc_upper=auc+.02,
                    brier=brier, brier_lower=brier-.01, brier_upper=brier+.01,
                    calibration_intercept=0, calibration_slope=1))
    lookup = {(r['model'], r['target'], r['arm']): r for r in metrics}
    paired = []
    for r in metrics:
        comparisons = []
        if r['target'] == 'prior':
            comparisons.append(('prior_minus_current', lookup[r['model'], 'current', r['arm']]))
        if r['arm'] == 'augmented':
            comparisons.append(('augmented_minus_original', lookup[r['model'], r['target'], 'original']))
        if r['model'] != 'weibull':
            comparisons.append(('model_minus_weibull', lookup['weibull', r['target'], r['arm']]))
        for contrast, ref in comparisons:
            for metric in ('auc', 'brier'):
                delta = r[metric] - ref[metric]
                paired.append(dict(fraction='1', contrast=contrast, model=r['model'],
                    target=r['target'], arm=r['arm'], reference_model=ref['model'],
                    metric=metric, difference=delta, ci_lower=delta-.02, ci_upper=delta+.02))
    equivalence = []
    for p in paired:
        for margin in range(1, 10):
            equivalence.append(dict(p, estimate=p['difference'], ci90_lower=p['ci_lower'],
                ci90_upper=p['ci_upper'], margin=margin*.001, p_lower=.5, p_upper=.5,
                p_tost=.5, p_holm=1, equivalent_holm='FALSE'))
    return metrics, paired, equivalence


def ancillary():
    return {
        'benchmark_tuning': [dict(fold=1, model='weibull', target='current', arm='original',
            ratio=0, training_n_rounded=100, training_events_rounded=30, parameter='',
            failed_fits=0, attempted_fits=3)],
        'association_curves': [dict(change_kg=-5, arm='original', kind='change',
            risk5=.2, risk_difference=.01, rd_lower=-.01, rd_upper=.03)],
        'simulation_summary': [dict(scenario='null', contrast='prior_minus_current',
            model='weibull', target='prior', arm='original', metric='auc',
            mean_difference=0, mcse=.001, empirical_q025=-.01, empirical_q975=.01,
            successful=100, requested=100)],
        'simulation_status': [dict(status='OK')],
    }


def invoke(script, args, mode):
    env = os.environ.copy()
    env.pop('PYTHONOPTIMIZE', None)
    if mode == 'env_PYTHONOPTIMIZE':
        env['PYTHONOPTIMIZE'] = '1'
    command = [sys.executable] + (['-O'] if mode == 'flag_O' else [])
    return subprocess.run(command + [str(script)] + list(map(str, args)),
        cwd=script.parent, env=env, capture_output=True, text=True, timeout=20)


def package_output(entry):
    return entry.parent/'validated_output' if 'args.old_aggregate_dir' in entry.read_text() else entry.parent


def invoke_package(entry, mode):
    root = entry.parents[2]
    private = root/'invented_aggregates'
    args = ()
    if package_output(entry) != entry.parent:
        args = (root/'reports/manuskript_v1', private/'hrs_ml_inference_20260930_120128',
            private/'hrs_ml_all_20260929_160248', package_output(entry),
            root/'reports/ml_critical_audit_v1/inference_verification.json')
    return invoke(entry, args, mode)


class TableValidationTests(unittest.TestCase):
    def test_ml_rejects_invalid_inputs_in_every_mode(self):
        cases = ('missing_status', 'failed_status', 'empty', 'model_count',
                 'duplicate_variants', 'model_status', 'contrast_count', 'contrast_math')
        for case in cases:
            for mode in MODES:
                with self.subTest(case=case, mode=mode), tempfile.TemporaryDirectory() as tmp:
                    source, output = Path(tmp)/'invented_input', Path(tmp)/'output'
                    source.mkdir()
                    m, p, _ = fixture()
                    headers_m, headers_p = list(m[0]), list(p[0])
                    if case != 'missing_status':
                        (source/'STATUS.txt').write_text('FAILED: invented' if case == 'failed_status' else 'SUCCESS: invented')
                    if case == 'empty':
                        m, p = [], []
                    elif case == 'model_count':
                        m.append(copy.deepcopy(m[0]))
                    elif case == 'duplicate_variants':
                        m = [copy.deepcopy(m[0]) for _ in range(12)]
                        p = [dict(p[0], model='weibull', target='current', arm='original',
                            contrast='model_minus_weibull', reference_model='weibull', difference=0) for _ in range(40)]
                    elif case == 'model_status':
                        m[0]['status'] = 'FAILED'
                    elif case == 'contrast_count':
                        p = []
                    elif case == 'contrast_math':
                        p[0]['difference'] = 1
                    write_csv(source/'benchmark_metrics.csv', m, headers_m)
                    write_csv(source/'benchmark_paired.csv', p, headers_p)
                    for name, rows in ancillary().items():
                        write_csv(source/(name+'.csv'), rows)
                    result = invoke(ML, (source, output), mode)
                    self.assertNotEqual(result.returncode, 0)
                    self.assertFalse((output/'ml_checks.json').exists())
                    self.assertFalse((output/'ML_TABLES.md').exists())

    def package_tree(self, root, case):
        # Copy the entrypoint into a completely invented tree. Only its absolute
        # PRIVATE constant is redirected; its algorithms and gates are untouched.
        entry = root/'01_Aktuelles_Manuskript/manuskript_v2/build_package.py'
        entry.parent.mkdir(parents=True)
        original = PACKAGE.read_text()
        private = root/'invented_aggregates'
        assignments = [node for node in ast.parse(original).body if isinstance(node, ast.Assign)
            and any(isinstance(target, ast.Name) and target.id == 'PRIVATE' for target in node.targets)]
        if assignments:
            self.assertEqual(len(assignments), 1)
            self.assertEqual(assignments[0].lineno, assignments[0].end_lineno)
            lines = original.splitlines(keepends=True)
            lines[assignments[0].lineno-1] = 'PRIVATE=Path('+repr(str(private))+')\n'
            original = ''.join(lines)
        else:
            self.assertIn('args.old_aggregate_dir', original)
        entry.write_text(original)
        old = root/'reports/manuskript_v1'
        inference = private/'hrs_ml_inference_20260930_120128'
        full = private/'hrs_ml_all_20260929_160248'
        m, _, e = fixture()
        if case == 'model_count':
            m.append(copy.deepcopy(m[0]))
        elif case == 'equivalence_count':
            e.pop()
        elif case == 'contrast_count':
            e = e[9:]
        elif case == 'model_status':
            m[0]['status'] = 'FAILED'
        elif case == 'contrast_math':
            for row in e[:9]:
                row['estimate'] = 1
        elif case in ('estimate_nan', 'estimate_inf'):
            for row in e:
                row['estimate'] = 'nan' if case == 'estimate_nan' else 'inf'
        elif case in ('earlier_margin_nan', 'earlier_margin_wrong'):
            e[0]['estimate'] = 'nan' if case == 'earlier_margin_nan' else 123
        elif case in ('model_nan', 'model_inf'):
            m[0]['auc'] = 'nan' if case == 'model_nan' else 'inf'
        write_csv(old/'table1.csv', [dict(mean=65, sd=3, count_rounded_10='',
            percent_approx='', denominator_rounded_10='', variable='age', label='Age', level='')])
        pred = [dict(metric=metric, target=target, optimism_corrected=.1, ci_lower=.01, ci_upper=.2)
            for metric in ('auc','brier','calibration_intercept','calibration_slope','interval_log_score')
            for target in ('current','prior','prior_minus_current')]
        write_csv(old/'prognosekennzahlen.csv', pred)
        curve = [dict(change_kg=-5,risk5=.2,risk_ci_lower=.1,risk_ci_upper=.3,
            risk_difference=.01,rd_ci_lower=-.01,rd_ci_upper=.03)]
        for name in ('veraenderung_kurve.csv','vorgeschichte_kurve.csv'):
            write_csv(old/name, curve)
        write_csv(old/'grenzenraster.csv', [dict(metric='auc',delta=.01,estimate=0,
            ci_lower=-.02,ci_upper=.02,interval_contained='FALSE')])
        write_csv(inference/'benchmark_metrics.csv', m)
        write_csv(inference/'exploratory_equivalence/conditional_tost.csv', e)
        write_csv(full/'benchmark_metrics.csv', m)
        for name, rows in ancillary().items():
            write_csv((inference if name == 'benchmark_tuning' else full)/(name+'.csv'), rows)
        for name in ('benchmark_diagnostics','benchmark_calibration','versions','preflight_counts'):
            write_csv(inference/(name+'.csv'), [dict(synthetic_fixture='yes')])
        provenance = root/'reports/ml_critical_audit_v1/inference_verification.json'
        provenance.parent.mkdir(parents=True)
        provenance.write_text('{"synthetic_fixture": true}')
        if case == 'empty':
            (old/'table1.csv').write_text('mean,sd\n')
        return entry

    def test_package_rejects_invalid_inputs_in_every_mode(self):
        cases = ('empty','model_count','equivalence_count','contrast_count','model_status','contrast_math',
                 'estimate_nan','estimate_inf','earlier_margin_nan','earlier_margin_wrong','model_nan','model_inf')
        for case in cases:
            for mode in MODES:
                with self.subTest(case=case, mode=mode), tempfile.TemporaryDirectory() as tmp:
                    entry = self.package_tree(Path(tmp), case)
                    result = invoke_package(entry, mode)
                    self.assertNotEqual(result.returncode, 0)
                    self.assertFalse((package_output(entry)/'table_checks.json').exists())
                    self.assertFalse((package_output(entry)/'source_sha256.json').exists())
                    self.assertNotIn('40 paired differences verified', result.stdout)

    def test_valid_synthetic_inputs_pass_in_every_mode(self):
        for mode in MODES:
            with self.subTest(mode=mode), tempfile.TemporaryDirectory() as tmp:
                root = Path(tmp)
                entry = self.package_tree(root, 'valid')
                result = invoke_package(entry, mode)
                self.assertEqual(result.returncode, 0, result.stderr)
                checks = json.loads((package_output(entry)/'table_checks.json').read_text())
                self.assertEqual(checks['model_variants'], 12)
                self.assertEqual(checks['equivalence_tests'], 360)
                source = root/'ml_input'
                source.mkdir()
                (source/'STATUS.txt').write_text('SUCCESS: invented')
                m, p, _ = fixture()
                for name, rows in dict(ancillary(), benchmark_metrics=m, benchmark_paired=p).items():
                    write_csv(source/(name+'.csv'), rows)
                result = invoke(ML, (source, root/'ml_output'), mode)
                self.assertEqual(result.returncode, 0, result.stderr)
                checks = json.loads((root/'ml_output/ml_checks.json').read_text())
                self.assertEqual(checks['variants'], 12)
                self.assertEqual(checks['paired_metric_contrasts'], 40)


if __name__ == '__main__':
    unittest.main(verbosity=2)
