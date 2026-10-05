"""Verify that added explanatory comments do not hide changed scientific source."""
from pathlib import Path
import hashlib,json
ROOT=Path(__file__).resolve().parents[1]
for row in json.loads((ROOT/'docs/ANNOTATION_MANIFEST.json').read_text()):
    source=(ROOT/row['source']).read_bytes()
    if hashlib.sha256(source).hexdigest()!=row['source_sha256']:
        raise ValueError('Source hash changed: '+row['source'])
    view=(ROOT/row['view']).read_text().split('```r\n',1)[1].rsplit('```',1)[0]
    recovered=''.join(line for line in view.splitlines(keepends=True) if not line.startswith('# REVIEW NOTE: '))
    if recovered!=source.decode():raise ValueError('Annotated view differs: '+row['view'])
print('PASS: four annotated source views preserve exact executed source, apart from added comments.')
