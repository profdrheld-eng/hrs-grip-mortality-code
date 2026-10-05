"""Package CLI regressions with artificial files, no R fits or HRS access."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import time
import unittest

HERE = Path(__file__).resolve().parent


class ReleaseCliTests(unittest.TestCase):
    def setUp(self):
        self.scratch = tempfile.TemporaryDirectory(prefix="hrs_release_cli_")
        self.root = Path(self.scratch.name)

    def tearDown(self):
        self.scratch.cleanup()

    def runner(self, with_test=True):
        package = self.root / "package"
        package.mkdir()
        shutil.copy2(HERE / "run_synthetic_tests.py", package)
        fixture = self.root / "fixture"
        tests = fixture / "02_Analyse/tests"
        tests.mkdir(parents=True)
        if with_test:
            (tests / "test_artificial.R").write_text("cat('Artificial only')\n")
        # Isolate runner control flow from package-integrity tests.
        (package / "materialize_version.py").write_text(
            "from pathlib import Path\n"
            f"def materialize(version, destination): return Path({str(fixture)!r})\n")
        return package / "run_synthetic_tests.py"

    def run_cli(self, script, report, *extra, executable_dir=None, temporary_dir=None):
        env = os.environ.copy()
        env["PATH"] = str(executable_dir or self.root / "no_executables")
        if temporary_dir is not None:
            env['TMPDIR'] = str(temporary_dir)
        return subprocess.run([sys.executable, "-B", str(script), "--suite", "all",
                               "--report", str(report), *extra], env=env,
                              capture_output=True, text=True, timeout=15)

    def test_existing_report_is_preserved(self):
        runner = self.runner(False)
        report = self.root / "old.json"
        report.write_text('{"status":"OLD"}\n')
        run = self.run_cli(runner, report)
        self.assertNotEqual(run.returncode, 0)
        self.assertEqual(report.read_text(), '{"status":"OLD"}\n')

    def test_empty_suite_fails(self):
        report = self.root / "empty.json"
        run = self.run_cli(self.runner(False), report)
        self.assertNotEqual(run.returncode, 0)
        self.assertEqual(json.loads(report.read_text())["status"], "FAIL")

    def test_missing_rscript_writes_current_failure(self):
        report = self.root / "missing.json"
        run = self.run_cli(self.runner(), report)
        self.assertNotEqual(run.returncode, 0)
        self.assertEqual(json.loads(report.read_text())["status"], "FAIL")

    def test_report_must_not_modify_package(self):
        runner = self.runner(False)
        report = runner.parent / "new.json"
        run = self.run_cli(runner, report)
        self.assertNotEqual(run.returncode, 0)
        self.assertFalse(report.exists())

    def test_unsupported_temporary_path_fails_before_launch(self):
        runner = self.runner()
        temporary = self.root / 'temporary with spaces'
        temporary.mkdir()
        report = self.root / 'unsupported.json'
        run = self.run_cli(runner, report, temporary_dir=temporary)
        self.assertNotEqual(run.returncode, 0)
        self.assertIn('temporary path without whitespace', run.stderr)
        self.assertFalse(report.exists())

    def test_rscript_receives_relative_test_path(self):
        runner = self.runner()
        executables = self.root / 'bin'
        executables.mkdir()
        shim = executables / 'Rscript'
        shim.write_text(f'#!{sys.executable}\nfrom pathlib import Path\nimport sys\n'
                        'p=Path(sys.argv[2])\nraise SystemExit(0 if not p.is_absolute() and p.is_file() else 1)\n')
        shim.chmod(0o700)
        report = self.root / 'relative.json'
        run = self.run_cli(runner, report, executable_dir=executables)
        self.assertEqual(run.returncode, 0, run.stderr)
        self.assertEqual(json.loads(report.read_text())['status'], 'PASS')

    @unittest.skipUnless(os.name == 'posix', 'POSIX process groups')
    def test_timeout_stops_child_processes(self):
        runner = self.runner()
        sentinel = self.root / 'delayed_child.txt'
        executables = self.root / 'bin'
        executables.mkdir()
        shim = executables / 'Rscript'
        child = f"import time; from pathlib import Path; time.sleep(2); Path({str(sentinel)!r}).write_text('orphan')"
        shim.write_text(f'#!{sys.executable}\nimport subprocess,time,sys\n'
                        f'subprocess.Popen([sys.executable,"-c",{child!r}])\ntime.sleep(30)\n')
        shim.chmod(0o700)
        report = self.root / 'timeout.json'
        run = self.run_cli(runner, report, '--timeout', '1', executable_dir=executables)
        self.assertNotEqual(run.returncode, 0)
        self.assertEqual(json.loads(report.read_text())['results'][0]['status'], 'TIMEOUT')
        time.sleep(1.5)
        self.assertFalse(sentinel.exists(), 'Child survived runner timeout')

    def builder(self):
        source = self.root / "source"
        package = source / "02_Analyse/bundle"
        package.mkdir(parents=True)
        shutil.copy2(HERE / "build_local_package.py", package)
        legacy = source / "03_Dokumentation/IPCW_Korrektur_2026-10-04/legacy_sources"
        legacy.mkdir(parents=True)
        (legacy / "sha256.json").write_text("[]\n")
        legacy_python = source / "03_Dokumentation/Codefreigabe_Audit_2026-10-04/legacy_python"
        legacy_python.mkdir(parents=True)
        first = legacy_python / "build_ml_paper_tables.py"
        first.write_text("# Artificial source\n")
        (legacy_python / 'sha256.json').write_text(json.dumps({first.name: {
            'sha256': hashlib.sha256(first.read_bytes()).hexdigest()}}))
        return package, first

    def test_assembly_rejects_symlink_without_external_write(self):
        package, first = self.builder()
        outside = self.root / "outside.txt"
        outside.write_text("PRESERVE\n")
        target = package / "reported_version/02_Analyse/scripts" / first.name
        target.parent.mkdir(parents=True)
        target.symlink_to(outside)
        run = subprocess.run([sys.executable, "-B", str(package / "build_local_package.py")],
                             capture_output=True, timeout=15)
        self.assertNotEqual(run.returncode, 0)
        self.assertEqual(outside.read_text(), "PRESERVE\n")

    def test_incomplete_sources_do_not_partially_replace_bundle(self):
        package, first = self.builder()
        target = package / "reported_version/02_Analyse/scripts" / first.name
        target.parent.mkdir(parents=True)
        target.write_text("PREVIOUS VERSION\n")
        run = subprocess.run([sys.executable, "-B", str(package / "build_local_package.py")],
                             capture_output=True, timeout=15)
        self.assertNotEqual(run.returncode, 0)
        self.assertEqual(target.read_text(), "PREVIOUS VERSION\n")

    def test_sealer_rejects_nested_manifest(self):
        package, _ = self.builder()
        (package / "SOURCE_MANIFEST.json").write_text("[]\n")
        hidden = package / "environment/extra/FILE_SHA256.json"
        hidden.parent.mkdir(parents=True)
        hidden.write_text("unlisted\n")
        run = subprocess.run([sys.executable, "-B", str(package / "build_local_package.py"), "--seal"],
                             capture_output=True, timeout=15)
        self.assertNotEqual(run.returncode, 0)
        self.assertFalse(package.with_suffix('.zip').exists())


if __name__ == "__main__":
    unittest.main()
