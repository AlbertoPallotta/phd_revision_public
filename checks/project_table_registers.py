"""Delivery support. Usage: python project_table_registers.py TT_REGISTER NEW_FILE.
Keep the complete tables member; omit figure entries; their images and captions are mapped separately.
"""
import hashlib,json,sys
from pathlib import Path
src,out=map(Path,sys.argv[1:])
assert hashlib.sha256(src.read_bytes()).hexdigest()=='2b821075c993f795ff170ea726de713f0e9751f3a1eded00a7965e8f9e1412cf'
assert not out.exists()
j=json.loads(src.read_text(encoding='utf-8-sig'));assert len(j['tables'])==2
out.parent.mkdir(parents=True,exist_ok=True)
out.write_text(json.dumps(dict(tables=j['tables']),ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
print(json.dumps(dict(source_sha256=hashlib.sha256(src.read_bytes()).hexdigest(),output_sha256=hashlib.sha256(out.read_bytes()).hexdigest(),projection='complete tables member only; all rows/headers/captions unchanged')))
