"""Verify the local review package against its explicit content manifest."""
from pathlib import Path, PurePosixPath
import hashlib
import json
import re


def safe_relative_path(value):
    if (not isinstance(value, str) or not value or '\\' in value
            or PurePosixPath(value).is_absolute() or '..' in PurePosixPath(value).parts
            or PurePosixPath(value).as_posix() != value or value == '.'):
        raise ValueError('Unsafe manifest path')
    return value


def valid_hash(value):
    return isinstance(value, str) and re.fullmatch(r'[0-9a-f]{64}', value) is not None


def verify_package(root=None):
    root = Path(root).resolve() if root is not None else Path(__file__).resolve().parent
    paths = sorted(root.rglob('*'))
    # Reject links before reading any manifest or hashing any candidate content.
    for path in paths:
        if path.is_symlink():
            raise ValueError('Symlink found: ' + path.relative_to(root).as_posix())
        if not path.is_file() and not path.is_dir():
            raise ValueError('Unsupported file type: ' + path.relative_to(root).as_posix())
    manifest = root / 'FILE_SHA256.json'
    expected = json.loads(manifest.read_text())
    if not isinstance(expected, dict) or not expected:
        raise ValueError('Package hash manifest must be a nonempty object')
    for name, digest in expected.items():
        safe_relative_path(name)
        if name == 'FILE_SHA256.json' or not valid_hash(digest):
            raise ValueError('Invalid package hash entry: ' + name)
    actual = {p.relative_to(root).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
              for p in paths if p.is_file() and p != manifest}
    if actual != expected:
        raise ValueError('Missing, extra or modified package file')
    records = json.loads((root / 'SOURCE_MANIFEST.json').read_text())
    if not isinstance(records, list) or not records:
        raise ValueError('Source manifest must be a nonempty list')
    seen = set()
    for record in records:
        if not isinstance(record, dict):
            raise ValueError('Invalid source manifest record')
        name = safe_relative_path(record.get('bundled_as'))
        safe_relative_path(record.get('source'))
        if name in seen:
            raise ValueError('Duplicate source manifest target: ' + name)
        seen.add(name)
        source_hash, bundle_hash = record.get('source_sha256'), record.get('bundle_sha256')
        if not valid_hash(source_hash) or not valid_hash(bundle_hash) or actual.get(name) != bundle_hash:
            raise ValueError('Source manifest disagrees with bundled content: ' + name)
        transformation = record.get('transformation')
        if transformation not in {'none', 'CLI path configuration only'}:
            raise ValueError('Unknown source transformation: ' + name)
        if transformation == 'none' and source_hash != bundle_hash:
            raise ValueError('Untransformed source hash differs: ' + name)
    source_files = {name for name in actual if name.startswith(('reported_version/', 'corrected_version/overlay/'))}
    if seen != source_files:
        raise ValueError('Source manifest does not exactly cover bundled source files')
    return expected


if __name__ == '__main__':
    expected = verify_package()
    print('PASS: all', len(expected), 'allowlisted file hashes and source-manifest hashes match; no symlinks.')
