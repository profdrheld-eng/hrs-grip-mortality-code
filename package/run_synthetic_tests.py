"""Run only artificial-data fixtures in a disposable tree, never HRS data."""
import argparse
import json
import os
from pathlib import Path
import signal
import subprocess
import tempfile
import time
from materialize_version import materialize

QUICK = [
    "test_synthetic.R", "test_tracker_formats.R", "test_tracker_csv.R",
    "test_stage2.R", "test_source_hierarchy.R", "test_equivalence_sensitivity.R",
    "test_ml_equivalence.R", "test_provenance_overlap.R",
]


def run_test(command, cwd, env, timeout):
    """A timeout or interrupt also stops nested test processes on POSIX."""
    process = subprocess.Popen(command, cwd=cwd, env=env, stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE, text=True, start_new_session=True)
    def terminate_requested(signum, frame):
        raise KeyboardInterrupt('Test runner terminated')
    previous = signal.signal(signal.SIGTERM, terminate_requested)
    try:
        output, errors = process.communicate(timeout=timeout)
        return subprocess.CompletedProcess(command, process.returncode, output, errors)
    except BaseException:
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        process.communicate()
        raise
    finally:
        signal.signal(signal.SIGTERM, previous)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--version", choices=["reported", "corrected"], default="corrected")
    parser.add_argument("--suite", choices=["quick", "regression", "all"], default="quick")
    parser.add_argument("--report", type=Path, required=True)
    parser.add_argument("--r-library", type=Path)
    parser.add_argument("--timeout", type=int, default=1800)
    args = parser.parse_args()
    if os.name != 'posix':
        parser.error('This test runner requires POSIX process-group cleanup')
    if any(char.isspace() for char in tempfile.gettempdir()):
        parser.error('Historical CLI fixtures require a temporary path without whitespace. Set TMPDIR to a suitable private directory.')
    if args.suite == "regression" and args.version != "corrected":
        parser.error("The new regression test belongs to corrected_version")
    if args.timeout <= 0:
        parser.error("Timeout must be positive")
    package = Path(__file__).resolve().parent
    if args.report.exists() or args.report.is_symlink():
        parser.error("Report must be a new file, previous reports are never overwritten")
    args.report = args.report.resolve()
    if args.report.is_relative_to(package):
        parser.error("Write the report outside the code package")
    env = os.environ.copy()
    if args.r_library:
        if not args.r_library.is_dir():
            parser.error("R library directory does not exist")
        if args.report.is_relative_to(args.r_library.resolve()):
            parser.error("Write the report outside the R library")
        env["R_LIBS_USER"] = str(args.r_library.resolve())
    results = []
    report = {"version": args.version, "suite": args.suite, "input": "artificial fixtures only",
              "hrs_data_accessed": False, "results": results,
              "status": "INCOMPLETE"}
    args.report.parent.mkdir(parents=True, exist_ok=True)
    # Exclusive creation prevents races with another run using the same report.
    with args.report.open("x") as output:
        def save():
            output.seek(0)
            output.write(json.dumps(report, indent=2) + "\n")
            output.truncate()
            output.flush()
        save()
        try:
            with tempfile.TemporaryDirectory(prefix="hrs_synthetic_review_") as tmp:
                tree = materialize(args.version, Path(tmp) / "code")
                tests = tree / "02_Analyse" / "tests"
                names = (QUICK if args.suite == "quick" else ["test_ipcw_ties.R", "test_ipcw_impact_audit.R"]
                         if args.suite == "regression" else sorted(p.name for p in tests.glob("*.R")))
                if not names or any(not (tests / name).is_file() for name in names):
                    raise ValueError("The requested test set is empty or incomplete")
                report["requested_tests"] = names
                for name in names:
                    source = tests / name
                    # Some fixtures use repository-root paths, others use analysis-root paths.
                    cwd = tree if "02_Analyse/scripts/" in source.read_text() else tree / "02_Analyse"
                    start = time.monotonic()
                    try:
                        run = run_test(["Rscript", "--vanilla", os.path.relpath(source, cwd)], cwd, env, args.timeout)
                        status = "PASS" if run.returncode == 0 else "FAIL"
                        detail = (run.stdout + run.stderr).replace(tmp, "<synthetic-temp>")
                        # R chooses another per-process temporary directory.
                        import re
                        detail = re.sub(r"/(?:private/)?(?:tmp|var/folders)/[^\s]+", "<R-temp>", detail)
                        result = {"test": name, "status": status, "exit_code": run.returncode,
                                  "elapsed_seconds": round(time.monotonic() - start, 3), "output": detail}
                    except subprocess.TimeoutExpired:
                        result = {"test": name, "status": "TIMEOUT", "elapsed_seconds": args.timeout}
                    results.append(result)
                    save()
                    print(name + ": " + result["status"], flush=True)
                report["status"] = "PASS" if results and all(r["status"] == "PASS" for r in results) else "FAIL"
        except Exception as error:
            report["status"] = "FAIL"
            report["error"] = type(error).__name__ + ": " + str(error)
        finally:
            save()
    return 0 if report["status"] == "PASS" else 1


if __name__ == "__main__":
    raise SystemExit(main())
