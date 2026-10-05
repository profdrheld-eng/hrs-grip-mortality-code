"""Local assembly from an allowlist in the source checkout. No HRS inputs read."""
from pathlib import Path
import argparse
import difflib
import hashlib
import json
import tempfile
import zipfile

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
PAIR = ['hrs_prediction_validation.R', 'hrs_ml_extension.R']
SCRIPTS = [
    'build_ml_paper_tables.py', 'hrs_analysis.R', 'hrs_diagnostics.R',
    'hrs_equivalence_sensitivity.R', 'hrs_feasibility.R', 'hrs_grip_change.R',
    'hrs_grip_history.R', 'hrs_ml_equivalence.R', 'hrs_ml_extension.R',
    'hrs_ml_figures.R', 'hrs_ml_inference.R', 'hrs_ml_overview_preview.R',
    'hrs_ml_person_figures_local.R', 'hrs_ml_point_figures.R', 'hrs_paper_figures.R',
    'hrs_prediction_validation.R', 'hrs_provenance_overlap_audit.R',
    'hrs_reporting_patch.R', 'hrs_source_hierarchy.R', 'hrs_stage2.R',
    'hrs_table1.R', 'hrs_tracker_formats.R',
]
TESTS = [
    'test_analysis.R', 'test_diagnostics.R', 'test_equivalence_sensitivity.R',
    'test_grip_change.R', 'test_grip_history.R', 'test_ml_equivalence.R',
    'test_ml_extension.R', 'test_ml_inference_integration.R', 'test_ml_point_figures.R',
    'test_ml_predictor_separation.R', 'test_paper_figures.R',
    'test_prediction_validation.R', 'test_provenance_overlap.R', 'test_reporting_patch.R',
    'test_source_hierarchy.R', 'test_stage2.R', 'test_synthetic.R', 'test_table1.R',
    'test_tracker_csv.R', 'test_tracker_formats.R',
]


def sha(data):
    return hashlib.sha256(data).hexdigest()


def checked_path(base, relative):
    relative = Path(relative)
    if relative.is_absolute() or '..' in relative.parts:
        raise ValueError('Path must stay inside its declared root')
    path = base
    for part in relative.parts:
        path = path / part
        if path.is_symlink():
            raise ValueError('Symlink is not allowed: ' + relative.as_posix())
    return path


def write_file(path, data):
    """Replace one prepared file without following an existing leaf link."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(dir=path.parent, delete=False) as stream:
        temporary = Path(stream.name)
        stream.write(data)
    try:
        temporary.replace(path)
    finally:
        temporary.unlink(missing_ok=True)


def assemble():
    records = []
    prepared = {}
    legacy = checked_path(ROOT, '03_Dokumentation/IPCW_Korrektur_2026-10-04/legacy_sources')
    checked_path(legacy, 'sha256.json')
    expected = {r['saved_as']: r['sha256'] for r in json.loads((legacy / 'sha256.json').read_text())}
    legacy_python = '03_Dokumentation/Codefreigabe_Audit_2026-10-04/legacy_python/'
    python_hashes = json.loads(checked_path(ROOT, legacy_python + 'sha256.json').read_text())
    def add(source, target, transform=None):
        source = checked_path(ROOT, source)
        if source.is_symlink() or not source.is_file():
            raise ValueError('Allowlisted source missing or symlink: ' + str(source.relative_to(ROOT)))
        data = source.read_bytes()
        out = transform(data.decode()).encode() if transform else data
        path = checked_path(HERE, target)
        if path.exists() and not path.is_file():
            raise ValueError('Output must be a regular file: ' + target)
        prepared[target] = out
        records.append({'source': source.relative_to(ROOT).as_posix(), 'bundled_as': target,
                        'source_sha256': sha(data), 'bundle_sha256': sha(out),
                        'transformation': 'CLI path configuration only' if transform else 'none'})
    for name in SCRIPTS:
        source = ('03_Dokumentation/IPCW_Korrektur_2026-10-04/legacy_sources/' + name
                  if name in PAIR else legacy_python + name if name == 'build_ml_paper_tables.py'
                  else '02_Analyse/scripts/' + name)
        if name in PAIR and sha(checked_path(ROOT, source).read_bytes()) != expected[name]:
            raise ValueError('Legacy source hash differs: ' + name)
        if name == 'build_ml_paper_tables.py' and sha(checked_path(ROOT, source).read_bytes()) != python_hashes[name]['sha256']:
            raise ValueError('Legacy Python source hash differs: ' + name)
        add(source, 'reported_version/02_Analyse/scripts/' + name)
    for name in TESTS:
        add('02_Analyse/tests/' + name, 'reported_version/02_Analyse/tests/' + name)
    for name in ['stage2_variables.csv']:
        add('02_Analyse/metadata/' + name, 'reported_version/02_Analyse/metadata/' + name)
    prefix = '01_Aktuelles_Manuskript/manuskript_v2/'
    for name in ['table2_source.csv', 'tableS7_tuning.csv', 'versions.csv',
                 'build_figure3.R', 'build_figure_s1.R']:
        add(('03_Dokumentation/IPCW_Integration_2026-10-05/before/' + name) if name == 'table2_source.csv' else prefix + name, 'reported_version/' + prefix + name)
    def portable_tables(text):
        begin = text.index('ROOT=')
        end = text.index('SOURCES={}')
        settings = (
            'import argparse\n'
            'parser=argparse.ArgumentParser(description=__doc__)\n'
            'parser.add_argument("old_aggregate_dir")\n'
            'parser.add_argument("inference_dir")\n'
            'parser.add_argument("full_ml_dir")\n'
            'parser.add_argument("new_output_dir")\n'
            'parser.add_argument("prior_inference_verification")\n'
            'args=parser.parse_args()\n'
            'OLD=Path(args.old_aggregate_dir).resolve()\n'
            'INF=Path(args.inference_dir).resolve()\n'
            'FULL=Path(args.full_ml_dir).resolve()\n'
            'OUT=Path(args.new_output_dir).resolve()\n'
            'OUT.mkdir(parents=True,exist_ok=False)\n')
        original = "ROOT/'reports/ml_critical_audit_v1/inference_verification.json'"
        replacement = 'Path(args.prior_inference_verification)'
        if text.count(original) != 1:
            raise ValueError('Expected one original table-builder source path')
        changed = text[:begin] + settings + text[end:].replace(original, replacement)
        if changed[changed.index('SOURCES={}'):] != text[end:].replace(original, replacement):
            raise ValueError('Unexpected table-builder change beyond path configuration')
        return changed
    if sha(checked_path(ROOT, legacy_python + 'build_package.py').read_bytes()) != python_hashes['build_package.py']['sha256']:
        raise ValueError('Legacy table-builder source hash differs')
    add(legacy_python + 'build_package.py', 'reported_version/' + prefix + 'build_package.py', portable_tables)
    prior_audit = '04_Archiv/Berichte/ml_critical_audit_v1/inference_verification.json'
    add(prior_audit, 'reported_version/' + prior_audit)
    for name in PAIR:
        add('02_Analyse/scripts/' + name, 'corrected_version/overlay/02_Analyse/scripts/' + name)
    add('02_Analyse/scripts/build_ml_paper_tables.py',
        'corrected_version/overlay/02_Analyse/scripts/build_ml_paper_tables.py')
    add(prefix + 'build_package.py', 'corrected_version/overlay/' + prefix + 'build_package.py', portable_tables)
    add('02_Analyse/tests/test_ipcw_ties.R',
        'corrected_version/overlay/02_Analyse/tests/test_ipcw_ties.R')
    add('02_Analyse/tests/test_python_table_validation.py',
        'corrected_version/overlay/02_Analyse/tests/test_python_table_validation.py')
    for source in ['02_Analyse/scripts/hrs_ipcw_impact_audit.R',
                   '02_Analyse/tests/test_ipcw_impact_audit.R',
                   prefix + 'reporting_patch_20261004/fold_counts.csv']:
        add(source, 'corrected_version/overlay/' + source)
    for name in PAIR + ['sha256.json']:
        source = '03_Dokumentation/IPCW_Korrektur_2026-10-04/legacy_sources/' + name
        add(source, 'corrected_version/overlay/' + source)
    # Exact sources and fixtures required by the verified recovery entry points.
    for name in ['hrs_ipcw_primary_refresh.R','hrs_ipcw_ml_refresh.R','hrs_ipcw_diagnostics_refresh.R']:
        source = '02_Analyse/scripts/' + name
        add(source, 'corrected_version/overlay/' + source)
    for name in ['test_ipcw_primary_refresh.R','test_ipcw_ml_refresh.R','test_ipcw_diagnostics_refresh.R','test_ml_recovery_hooks.R']:
        source = '02_Analyse/tests/' + name
        add(source, 'corrected_version/overlay/' + source)
    for source in [
        '04_Archiv/Berichte/ml_critical_audit_v1/before/hrs_ml_extension.R',
        '03_Dokumentation/IPCW_Rueckgabe_2026-10-04/source_before_ml_hooks/hrs_ml_extension.R']:
        add(source, 'corrected_version/overlay/' + source)
    for name in ['STATUS.txt','PRIMARY_REPLAY_STATUS.txt','versions.txt','script_hashes.csv','primary_apparent_reweighting.csv']:
        source = '03_Dokumentation/IPCW_Rueckgabe_2026-10-04/SEND_BACK/' + name
        add(source, 'corrected_version/overlay/' + source)
    diffs = []
    for name in PAIR:
        old = prepared['reported_version/02_Analyse/scripts/' + name].decode()
        new = prepared['corrected_version/overlay/02_Analyse/scripts/' + name].decode()
        if old == new:
            raise ValueError('Corrected source does not differ: ' + name)
        diffs.extend(difflib.unified_diff(old.splitlines(True), new.splitlines(True),
                     fromfile='a/02_Analyse/scripts/' + name,
                     tofile='b/02_Analyse/scripts/' + name))
    prepared['corrected_version/ipcw_correction.patch'] = ''.join(diffs).encode()
    python_diff = []
    for source in ['02_Analyse/scripts/build_ml_paper_tables.py', prefix + 'build_package.py']:
        old = prepared['reported_version/' + source].decode()
        new = prepared['corrected_version/overlay/' + source].decode()
        python_diff.extend(difflib.unified_diff(old.splitlines(True), new.splitlines(True),
                           fromfile='a/' + source, tofile='b/' + source))
    prepared['corrected_version/runtime_validation.patch'] = ''.join(python_diff).encode()
    prepared['SOURCE_MANIFEST.json'] = (json.dumps(records, indent=2) + '\n').encode()
    # Validate the complete source set and all destinations before the first write.
    for target in prepared:
        path = checked_path(HERE, target)
        if path.exists() and not path.is_file():
            raise ValueError('Output must be a regular file: ' + target)
    for target, data in prepared.items():
        write_file(checked_path(HERE, target), data)
    print('Allowlisted sources copied. Analytical sources are byte-preserved within each version.')


def seal():
    allowed_top = {'README.md', 'DATA_ACCESS.md', 'VERSION_STATUS.md', 'SOURCE_MANIFEST.json',
                   'build_local_package.py', 'materialize_version.py', 'run_synthetic_tests.py',
                   'verify_local_package.py', 'test_release_cli.py', 'test_package_integrity.py'}
    approved_qa = {'static_checks.json', 'environment_check.txt', 'reported_quick.json',
                   'corrected_quick.json', 'corrected_regression.json', 'relocation_check.json',
                   'corrected_all.json', 'release_audit.json'}
    files = []
    for p in sorted(HERE.rglob('*')):
        if p.is_symlink():
            raise ValueError('No symlinks allowed in review bundle')
        if not p.is_file() and not p.is_dir():
            raise ValueError('Unsupported file type in review bundle')
        if not p.is_file() or p == HERE / 'FILE_SHA256.json':
            continue
        rel = p.relative_to(HERE)
        if len(rel.parts) == 1:
            valid = p.name in allowed_top
        elif rel.parts[0] == 'qa':
            valid = len(rel.parts) == 2 and p.name in approved_qa
        elif rel.parts[0] == 'environment':
            valid = len(rel.parts) == 2 and p.name in {'check_environment.R', 'DEPENDENCIES.tsv'}
        elif rel.parts[0] in {'reported_version', 'corrected_version'}:
            listed = {r['bundled_as'] for r in json.loads((HERE / 'SOURCE_MANIFEST.json').read_text())}
            valid = rel.as_posix() in listed or rel.as_posix() in {
                'corrected_version/ipcw_correction.patch', 'corrected_version/runtime_validation.patch'}
        else:
            valid = False
        if not valid:
            raise ValueError('Unexpected file outside allowlist: ' + rel.as_posix())
        content = p.read_text()
        # Check content, not just file extensions. Never include local personal path literals.
        if any(marker in content for marker in ['/Users/' + 'steffenheld', 'Documents/' + 'HRS_private',
                                                'BEGIN ' + 'PRIVATE KEY']):
            raise ValueError('Private path or key marker in ' + rel.as_posix())
        files.append(p)
    hashes = {p.relative_to(HERE).as_posix(): sha(p.read_bytes()) for p in files}
    manifest = HERE / 'FILE_SHA256.json'
    if manifest.is_symlink():
        raise ValueError('Hash manifest must not be a symlink')
    write_file(manifest, (json.dumps(hashes, indent=2) + '\n').encode())
    from verify_local_package import verify_package
    verify_package(HERE)
    files.append(manifest)
    archive = HERE.with_suffix('.zip')
    if archive.is_symlink() or archive.with_suffix('.zip.sha256').is_symlink():
        raise ValueError('Archive targets must not be symlinks')
    with tempfile.NamedTemporaryFile(dir=archive.parent, delete=False) as stream:
        pending_archive = Path(stream.name)
    try:
        with zipfile.ZipFile(pending_archive, 'w', zipfile.ZIP_DEFLATED, compresslevel=9) as z:
            for p in files:
                name = HERE.name + '/' + p.relative_to(HERE).as_posix()
                info = zipfile.ZipInfo(name, date_time=(2026, 10, 4, 0, 0, 0))
                info.compress_type = zipfile.ZIP_DEFLATED
                info.external_attr = 0o644 << 16
                z.writestr(info, p.read_bytes())
        with zipfile.ZipFile(pending_archive) as z:
            if z.testzip() is not None or len(z.namelist()) != len(files):
                raise ValueError('Archive validation failed')
            for name, digest in hashes.items():
                if sha(z.read(HERE.name + '/' + name)) != digest:
                    raise ValueError('Archive content differs: ' + name)
        pending_archive.replace(archive)
    finally:
        pending_archive.unlink(missing_ok=True)
    write_file(archive.with_suffix('.zip.sha256'), (sha(archive.read_bytes()) + '  ' + archive.name + '\n').encode())
    print('Local archive sealed:', archive.name, 'files:', len(files))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--seal', action='store_true')
    args = parser.parse_args()
    if args.seal:
        seal()
    else:
        assemble()
