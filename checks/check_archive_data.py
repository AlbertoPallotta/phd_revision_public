"""Delivery support. Args: SPECIFICATION_ROOT ASSEMBLED_DELIVERY INSTALLED_LIBRARY NEW_LOG.
Inspect archive bytes without extracting/executing. Check six frozen dataset files
in source, archive and installed tree; check all ten preserved PT products in tar.
"""
import csv,hashlib,json,sys,tarfile
from pathlib import Path
spec,delivery,lib,out=map(Path,sys.argv[1:]);assert not out.exists()
def rows(p):
 with p.open(encoding='utf-8-sig',newline='') as f:return list(csv.DictReader(f))
def h(b):return hashlib.sha256(b).hexdigest()
archives={'TradeTesterR':delivery/'archives/TradeTesterR_0.1.2.9001.tar.gz',
          'PortfolioTesteR':delivery/'archives/PortfolioTesteR_0.1.4.9008.tar.gz'}
checks=[]
for package,archive in archives.items():
 with tarfile.open(archive,'r:gz') as tf:
  members={m.name:m for m in tf.getmembers() if m.isfile()}
  allow=[r for r in rows(spec/'PERMITTED_LIBRARY_DATA.csv') if r['package']==package]
  expected={package+'/'+r['proposed_destination'].split(package+'/',1)[1] for r in allow}
  actual={n for n in members if n.startswith((package+'/data/',package+'/inst/extdata/'))}
  assert actual==expected,('Data membership mismatch',package,actual^expected)
  for r in allow:
   rel=r['proposed_destination'].split(package+'/',1)[1];member=package+'/'+rel
   installed=rel[5:] if rel.startswith('inst/') else rel
   for label,b in [('source',(delivery/r['proposed_destination']).read_bytes()),
                   ('archive',tf.extractfile(members[member]).read()),
                   ('installed',(lib/package/installed).read_bytes())]:
    assert h(b)==r['sha256'],(package,rel,label)
    checks.append(dict(package=package,path=rel,location=label,sha256=h(b),status='PASS'))
  if package=='PortfolioTesteR':
   for r in rows(spec/'PREBUILT_DOCUMENTATION_MANIFEST.csv'):
    assert r['member'] in members
    b=tf.extractfile(members[r['member']]).read();assert h(b)==r['sha256']
    checks.append(dict(package=package,path=r['member'],location='archive prebuilt',sha256=h(b),status='PASS'))
out.write_text(json.dumps(dict(archive_sha256={p:h(a.read_bytes()) for p,a in archives.items()},checks=checks),indent=2)+'\n',encoding='utf-8')
