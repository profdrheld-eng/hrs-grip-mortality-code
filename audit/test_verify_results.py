"""Check that real aggregate corruption is rejected even after hashes are updated."""
from pathlib import Path
import csv,hashlib,json,shutil,tempfile,unittest
from verify_results import verify
ROOT=Path(__file__).resolve().parents[1]
class ResultTests(unittest.TestCase):
    def setUp(self):
        self.tmp=tempfile.TemporaryDirectory();self.addCleanup(self.tmp.cleanup)
        self.data=Path(self.tmp.name)/'results'
        shutil.copytree(ROOT/'reported_results',self.data)
    def alter(self,name,change):
        p=self.data/name
        with p.open() as f:
            reader=csv.DictReader(f);fields=reader.fieldnames;rows=list(reader)
        change(rows)
        with p.open('w',newline='') as f:
            w=csv.DictWriter(f,fieldnames=fields);w.writeheader();w.writerows(rows)
        m=json.loads((self.data/'SHA256.json').read_text())
        m[name]=hashlib.sha256(p.read_bytes()).hexdigest()
        (self.data/'SHA256.json').write_text(json.dumps(m))
    def test_actual_results_pass(self):
        self.assertEqual(verify(self.data)['paired_contrasts'],40)
    def test_changed_paired_difference_fails(self):
        self.alter('tableS2_paired.csv',lambda r:r[0].update(estimate='0.2'))
        with self.assertRaises(ValueError):verify(self.data)
    def test_wrong_holm_probability_fails(self):
        self.alter('tableS3_equivalence.csv',lambda r:r[0].update(p_holm='0.01'))
        with self.assertRaises(ValueError):verify(self.data)
    def test_duplicate_model_key_fails(self):
        self.alter('table3_source.csv',lambda r:r.__setitem__(1,dict(r[0])))
        with self.assertRaises(ValueError):verify(self.data)
    def test_wrong_primary_margin_flag_fails(self):
        self.alter("tableS8_original_margins.csv",lambda r:r[0].update(interval_contained="TRUE"))
        with self.assertRaises(ValueError):verify(self.data)
    def test_hash_change_fails(self):
        p=self.data/'table2_source.csv';p.write_text(p.read_text()+'\n')
        with self.assertRaises(ValueError):verify(self.data)
if __name__=='__main__':unittest.main(verbosity=2)
