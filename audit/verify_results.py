"""Read-only arithmetic checks of released aggregates, never a private-data replay."""
from pathlib import Path
import csv,hashlib,json,math
ROOT=Path(__file__).resolve().parents[1]
FILES={'table2_source.csv':15,'table3_source.csv':12,'tableS2_paired.csv':40,
       'tableS3_equivalence.csv':360,'tableS4_learning.csv':36,'tableS8_original_margins.csv':18}
def require(ok,message):
    # Runtime checks must also work under python -O.
    if not ok:raise ValueError(message)
def number(value):
    x=float(value);require(math.isfinite(x),'Nonfinite value');return x
def near(a,b,label):
    require(abs(number(a)-number(b))<=1e-12,'Mismatch: '+label)
def keyed(rows,fields):
    out={}
    for r in rows:
        k=tuple(r[f] for f in fields);require(k not in out,'Duplicate key');out[k]=r
    return out
def boolean(s):
    require(s in ('TRUE','FALSE'),'Invalid boolean');return s=='TRUE'
def verify(folder=ROOT/'reported_results'):
    folder=Path(folder);manifest=json.loads((folder/'SHA256.json').read_text())
    require(set(manifest)==set(FILES),'Unexpected manifest entries')
    tables={}
    for name,count in FILES.items():
        p=folder/name
        require(not p.is_symlink(),'Symlink input')
        require(hashlib.sha256(p.read_bytes()).hexdigest()==manifest[name],'Hash mismatch: '+name)
        with p.open() as f:tables[name]=list(csv.DictReader(f))
        require(len(tables[name])==count,'Wrong row count: '+name)
    primary=keyed(tables['table2_source.csv'],['metric','target'])
    for metric in ('auc','brier','calibration_intercept','calibration_slope','interval_log_score'):
        current=primary[metric,'current'];prior=primary[metric,'prior'];delta=primary[metric,'prior_minus_current']
        for field in ('apparent','optimism_corrected'):
            near(delta[field],number(prior[field])-number(current[field]),'primary '+metric+' '+field)
    for r in primary.values():
        require(number(r['ci_lower'])<=number(r['ci_upper']),'Reversed primary interval')
        require(r['status']=='OK' and r['outer_success']=='200' and r['nested_success']=='200','Primary validation incomplete')
    models=keyed(tables['table3_source.csv'],['model','target','arm'])
    expected={(m,t,a) for m in ('weibull','neural','xgboost') for t in ('current','prior') for a in ('original','augmented')}
    require(set(models)==expected,'Incomplete model grid')
    for r in models.values():
        for metric in ('auc','brier'):
            require(0<=number(r[metric])<=1,'Invalid performance range')
            require(number(r[metric+'_lower'])<=number(r[metric+'_upper']),'Reversed model interval')
    fields=['contrast','model','target','arm','reference_model','metric']
    pairs=keyed(tables['tableS2_paired.csv'],fields)
    for k,r in pairs.items():
        c,m,t,a,reference,metric=k
        require(metric in ('auc','brier'),'Unexpected metric')
        left=models[m,t,a]
        if c=='prior_minus_current':
            require(t=='prior' and reference==m,'Wrong prior comparison labels')
            right=models[m,'current',a]
        elif c=='augmented_minus_original':
            require(a=='augmented' and reference==m,'Wrong augmentation labels')
            right=models[m,t,'original']
        else:
            require(c=='model_minus_weibull' and reference=='weibull' and m!='weibull','Wrong baseline comparison')
            right=models['weibull',t,a]
        near(r['estimate'],number(left[metric])-number(right[metric]),'paired '+str(k))
        require(number(r['ci90_lower'])<=number(r['ci90_upper']),'Reversed paired interval')
    tests=tables['tableS3_equivalence.csv'];keyed(tests,fields+['margin'])
    groups={}
    for r in tests:
        k=tuple(r[f] for f in fields);require(k in pairs,'Unknown TOST contrast')
        for f in ('estimate','ci90_lower','ci90_upper'):near(r[f],pairs[k][f],'TOST '+f)
        p=max(number(r['p_lower']),number(r['p_upper']));near(r['p_tost'],p,'TOST maximum')
        require(all(0<=number(r[f])<=1 for f in ('p_lower','p_upper','p_tost','p_holm')),'Invalid probability')
        require(r['bootstrap_B']=='5000','Unexpected draw count');near(r['p_resolution'],1/5001,'Probability resolution')
        require(boolean(r['equivalent'])==(p<.05),'Wrong unadjusted decision')
        margin=number(r['margin'])
        require(boolean(r['interval_contained'])==(number(r['ci90_lower'])>-margin and number(r['ci90_upper'])<margin),'Wrong containment flag')
        groups.setdefault((r['metric'],margin),[]).append(r)
    require(len(groups)==18,'Incomplete margin grid')
    for group in groups.values():
        require(len(group)==20,'Wrong Holm family size')
        last=0
        for i,r in enumerate(sorted(group,key=lambda z:number(z['p_tost']))):
            last=max(last,min(1,(20-i)*number(r['p_tost'])))
            near(r['p_holm'],last,'Holm adjustment')
            require(boolean(r['equivalent_holm'])==(last<.05),'Wrong adjusted decision')
    learning=keyed(tables['tableS4_learning.csv'],['fraction','model','target','arm'])
    for k,r in models.items():
        for metric in ('auc','brier','interval_log_score'):
            near(r[metric],learning[('1',)+k][metric],'Full-training point')
    margins=keyed(tables['tableS8_original_margins.csv'],['metric','delta'])
    for (metric,delta),r in margins.items():
        require(metric in ('auc','brier') and number(delta)>0,'Invalid primary margin')
        ref=primary[metric,'prior_minus_current']
        near(r['estimate'],ref['optimism_corrected'],'Primary margin estimate')
        for f in ('ci_lower','ci_upper'):near(r[f],ref[f],'Primary margin interval')
        lo,hi=number(r['ci_lower']),number(r['ci_upper'])
        near(r['boundary_infimum'],max(abs(lo),abs(hi)),'Primary interval boundary')
        require(boolean(r['interval_contained'])==(lo>-number(delta) and hi<number(delta)),'Wrong primary margin flag')
    return {'status':'PASS','primary_rows':15,'model_variants':12,'paired_contrasts':40,'conditional_tests':360,
            'limitations':'Aggregate agreement, not independent private-data reproduction or coverage validation.'}
if __name__=='__main__':print(json.dumps(verify(),indent=2))
