"""Security regression checks using disposable, artificial package fixtures."""
from pathlib import Path
import hashlib
import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest

PACKAGE = Path(sys.argv.pop(1)).resolve()


class PackageIntegrityTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="hrs_integrity_regression_")
        self.addCleanup(self.temp.cleanup)
        self.scratch = Path(self.temp.name)
        self.package = self.scratch / "package"
        self.package.mkdir()
        for name in ["verify_local_package.py", "materialize_version.py"]:
            shutil.copyfile(PACKAGE / name, self.package / name)
        self.reported = self.package / "reported_version/02_Analyse/scripts/artificial.R"
        self.corrected = self.package / "corrected_version/overlay/02_Analyse/scripts/artificial.R"
        for path, content in [(self.reported, "# ARTIFICIAL ORIGINAL\n"),
                              (self.corrected, "# ARTIFICIAL CORRECTED\n")]:
            path.parent.mkdir(parents=True)
            path.write_text(content)
        self.records = [{"source": "02_Analyse/scripts/artificial.R",
                         "bundled_as": path.relative_to(self.package).as_posix(),
                         "source_sha256": self.digest(path), "bundle_sha256": self.digest(path),
                         "transformation": "none"}
                        for path in [self.reported, self.corrected]]
        self.write_source_manifest()
        self.seal()

    def digest(self, path):
        return hashlib.sha256(path.read_bytes()).hexdigest()

    def write_source_manifest(self):
        (self.package / "SOURCE_MANIFEST.json").write_text(json.dumps(self.records))

    def seal(self):
        hashes = {p.relative_to(self.package).as_posix(): self.digest(p)
                  for p in self.package.rglob("*") if p.is_file()
                  and p != self.package / "FILE_SHA256.json"}
        (self.package / "FILE_SHA256.json").write_text(json.dumps(hashes))

    def invoke(self, script="verify_local_package.py", args=(), mode="normal"):
        env = os.environ.copy()
        env.pop("PYTHONOPTIMIZE", None)
        if mode == "environment":
            env["PYTHONOPTIMIZE"] = "1"
        cmd = [sys.executable, "-B"] + (["-O"] if mode == "flag" else [])
        return subprocess.run(cmd + [str(self.package / script)] + list(args),
                              env=env, capture_output=True, text=True, timeout=20)

    def require_rejected(self, result):
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertNotIn("PASS:", result.stdout)

    def test_clean_package_passes_every_python_mode(self):
        for mode in ["normal", "flag", "environment"]:
            with self.subTest(mode=mode):
                result = self.invoke(mode=mode)
                self.assertEqual(result.returncode, 0, result.stderr)

    def test_changed_source_rejected_every_python_mode(self):
        self.reported.write_text("# ARTIFICIAL TAMPER\n")
        for mode in ["normal", "flag", "environment"]:
            with self.subTest(mode=mode):
                self.require_rejected(self.invoke(mode=mode))

    def test_extra_nested_manifest_rejected(self):
        (self.package / "reported_version/FILE_SHA256.json").write_text("ARTIFICIAL EXTRA")
        self.require_rejected(self.invoke())

    def test_source_manifest_disagreement_rejected_after_outer_reseal(self):
        self.records[0]["bundle_sha256"] = "0" * 64
        self.write_source_manifest()
        self.seal()
        self.require_rejected(self.invoke())

    def test_source_manifest_duplicate_rejected_after_outer_reseal(self):
        self.records.append(self.records[0].copy())
        self.write_source_manifest()
        self.seal()
        self.require_rejected(self.invoke())

    def test_source_manifest_omission_rejected_after_outer_reseal(self):
        self.records.pop()
        self.write_source_manifest()
        self.seal()
        self.require_rejected(self.invoke())

    def test_source_manifest_unsafe_path_rejected(self):
        self.records[0]["bundled_as"] = "../artificial_outside"
        self.write_source_manifest()
        self.seal()
        self.require_rejected(self.invoke())

    def test_regular_and_corrected_materialization(self):
        for version, expected in [("reported", self.reported), ("corrected", self.corrected)]:
            with self.subTest(version=version):
                destination = self.scratch / version
                result = self.invoke("materialize_version.py", [version, str(destination)])
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual((destination / "02_Analyse/scripts/artificial.R").read_bytes(), expected.read_bytes())

    def test_changed_materialization_rejected_before_output(self):
        self.reported.write_text("# ARTIFICIAL TAMPER\n")
        destination = self.scratch / "changed-copy"
        self.require_rejected(self.invoke("materialize_version.py", ["reported", str(destination)]))
        self.assertFalse(destination.exists())

    def test_symlink_file_rejected_before_output(self):
        external = self.scratch / "artificial_outside.txt"
        external.write_text("SYNTHETIC ONLY\n")
        self.reported.unlink()
        self.reported.symlink_to(external)
        self.require_rejected(self.invoke())
        destination = self.scratch / "linked-copy"
        self.require_rejected(self.invoke("materialize_version.py", ["reported", str(destination)]))
        self.assertFalse(destination.exists())

    def test_symlink_directory_rejected_before_output(self):
        external = self.scratch / "artificial_outside_directory"
        external.mkdir()
        (external / "synthetic.txt").write_text("SYNTHETIC ONLY\n")
        (self.package / "reported_version/external").symlink_to(external, target_is_directory=True)
        self.require_rejected(self.invoke())
        destination = self.scratch / "directory-copy"
        self.require_rejected(self.invoke("materialize_version.py", ["reported", str(destination)]))
        self.assertFalse(destination.exists())

    def test_existing_destination_preserved(self):
        destination = self.scratch / "existing"
        destination.mkdir()
        sentinel = destination / "sentinel.txt"
        sentinel.write_text("PRESERVE\n")
        self.require_rejected(self.invoke("materialize_version.py", ["reported", str(destination)]))
        self.assertEqual(sentinel.read_text(), "PRESERVE\n")

    def test_dangling_destination_symlink_rejected(self):
        destination = self.scratch / "dangling"
        external = self.scratch / "must_not_be_created"
        destination.symlink_to(external, target_is_directory=True)
        self.require_rejected(self.invoke("materialize_version.py", ["reported", str(destination)]))
        self.assertFalse(external.exists())

    def test_destination_inside_package_rejected(self):
        destination = self.package / "new-tree"
        self.require_rejected(self.invoke("materialize_version.py", ["reported", str(destination)]))
        self.assertFalse(destination.exists())


if __name__ == "__main__":
    unittest.main(verbosity=2)
