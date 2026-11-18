"""Delivery support. Usage: python export_word_tables.py CURRENT_DOCX NEW_DIR.
Direct OOXML table-cell transcription, no new computation or document edit.
"""
import csv,hashlib,json,sys,zipfile,xml.etree.ElementTree as ET
from pathlib import Path
doc,out=map(Path,sys.argv[1:])
assert hashlib.sha256(doc.read_bytes()).hexdigest()=='275a46c98ef2c6d29244921aeb2937123457d71da99dbf9e3df601dad66d7ecb'
assert not out.exists()
with zipfile.ZipFile(doc) as z: body=ET.fromstring(z.read('word/document.xml'))
ns={'w':'http://schemas.openxmlformats.org/wordprocessingml/2006/main'}
tables=body.findall('.//w:body/w:tbl',ns)
numbers=['2.1','3.1','3.2','3.3','3.4','3.5','4.1','4.2','4.3','4.4','5.1','5.2','5.3','5.4','5.5']
assert len(tables)==len(numbers)==15
out.mkdir(parents=True)
records=[]
for number,table in zip(numbers,tables):
 rows=[]
 for row in table.findall('w:tr',ns):
  cells=[]
  for cell in row.findall('w:tc',ns):
   # Preserve paragraph boundaries and Word tabs/soft line breaks in a cell.
   paragraphs=[]
   for para in cell.findall('w:p',ns):
    parts=[]
    for x in para.iter():
     if x.tag.endswith('}t'): parts.append(x.text or '')
     elif x.tag.endswith('}br') or x.tag.endswith('}cr'): parts.append('\n')
     elif x.tag.endswith('}tab'): parts.append('\t')
    paragraphs.append(''.join(parts))
   cells.append('\n'.join(paragraphs))
  rows.append(cells)
 p=out/f'Table_{number.replace(".","_")}.csv'
 with p.open('w',encoding='utf-8',newline='') as f: csv.writer(f).writerows(rows)
 records.append(dict(table=number,file=p.name,sha256=hashlib.sha256(p.read_bytes()).hexdigest(),rows=len(rows),mode='literal controlling Word cells; no calculation'))
(out/'table-extraction-record.json').write_text(json.dumps(records,indent=2)+'\n',encoding='utf-8')
