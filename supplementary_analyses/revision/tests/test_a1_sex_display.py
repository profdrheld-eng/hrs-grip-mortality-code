"""Only approved aggregate fixtures; no HRS files or new model execution."""
from pathlib import Path
import csv, subprocess, sys, tempfile

base=Path(__file__).resolve().parents[1]
with tempfile.TemporaryDirectory(prefix='a1_display_')as tmp:
    output=Path(tmp)/'display'
    subprocess.run([sys.executable,str(base/'scripts/build_display_tables.py'),str(output)],check=True,capture_output=True,text=True)
    with (output/'tableS12_selection_characteristics.csv').open()as stream:
        rows=list(csv.reader(stream))
    assert len(rows)==13, 'S12 must include all three released sex rows.'
    assert rows[4:7]==[
        ['Recorded sex: male','SUPPRESSED','SUPPRESSED','—'],
        ['Recorded sex: female','3680','1340','—'],
        ['Recorded sex: missing or invalid','SUPPRESSED','SUPPRESSED','—']]
    for old in (base/'display_tables').glob('*.csv'):
        if old.name!='tableS12_selection_characteristics.csv':
            assert (output/old.name).read_bytes()==old.read_bytes(),old.name
print('PASS: S12 sex rows and suppression match approved sources; six other display CSVs unchanged.')
