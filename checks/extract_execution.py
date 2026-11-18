"""Transaction-field projection without strategy execution.
Usage: python extract_execution.py FIRST_TRADE_CSV PARTIAL_LEGS_CSV NEW_OUTPUT_DIR
No strategy/data recalculation. Only selected recorded transaction fields.
"""
import csv,hashlib,json,sys
from pathlib import Path
first,partial,out=map(Path,sys.argv[1:])
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
assert sha(first)=='df83163664f04eab009172f7855ad53ac15bf55e37825739e21877f435133760'
assert sha(partial)=='4d90c9567aff9446751fbd1fa15991e172670f60d22cf540e3ec36fd8b582ac6'
def rows(p):
 with p.open(encoding='utf-8-sig',newline='') as f:return list(csv.DictReader(f))
a=rows(first);b=rows(partial)
assert len(a)==1 and a[0]['Order_ID']=='1'
assert len(b)==2 and all(r['Order_ID']=='35' for r in b)
assert [float(r['ExitAmount']) for r in b]==[-100,-100]
assert [float(r['CurrentPosSize']) for r in b]==[100,0]
# Keep source order and literal recorded strings, including +00:00 timestamps.
spec=[('figure3_1_execution.csv',a,['EntryTime','ExitTime','EntryPrice','ExitPrice']),
 ('table3_3_execution.csv',b,['Order_ID','EntryTime','EntryPrice','OrderSize',
  'ExitTime','ExitPrice','ExitPerc','ExitReason','ExitAmount','CurrentPosSize'])]
assert not out.exists(),'Choose a new output folder; never overwrite evidence.'
out.mkdir(parents=True)
manifest=[]
for name,rr,fields in spec:
 p=out/name
 with p.open('w',encoding='utf-8',newline='') as f:
  w=csv.DictWriter(f,fieldnames=fields);w.writeheader()
  w.writerows({k:r[k] for k in fields} for r in rr)
 manifest.append(dict(file=name,sha256=sha(p),columns=fields,rows=len(rr),renaming='none'))
(out/'extraction-record.json').write_text(json.dumps(dict(
 source_first_sha256=sha(first),source_partial_sha256=sha(partial),outputs=manifest),indent=2)+'\n',encoding='utf-8')
