from pathlib import Path
import sys,json,importlib.util,platform,argparse
parser=argparse.ArgumentParser(description="Historical synthetic platform check; use the recorded dependency environment.")
parser.add_argument('--source',type=Path,required=True,help="Historical simple_backtrader.py source")
parser.add_argument('--output',type=Path,required=True,help="New JSON output path in an existing directory")
parser.add_argument('--python-library',type=Path,help="Optional recorded dependency directory; otherwise use the configured environment")
args=parser.parse_args()
if not args.source.is_file():parser.error('Missing historical comparison source.')
if args.output.exists():parser.error('Output exists; choose a new file.')
if not args.output.parent.is_dir():parser.error('Missing output directory.')
if args.python_library:
 if not args.python_library.is_dir():parser.error('Missing historical Python library.')
 sys.path.insert(0,str(args.python_library))
import backtrader as bt
import pandas as pd
spec=importlib.util.spec_from_file_location('simple',args.source)
m=importlib.util.module_from_spec(spec);sys.modules['simple']=m;spec.loader.exec_module(m)
checks=[]
def check(name,ok):
    checks.append(dict(check=name,passed=bool(ok)))
    assert ok,name
def run(close,**kw):
    df=pd.DataFrame({'open':close,'close':close,'high':[v+.05 for v in close],'low':[v-.05 for v in close]}, index=pd.date_range('2020-01-01',periods=len(close)))
    cerebro=bt.Cerebro();cerebro.broker.setcash(100000);cerebro.broker.setcommission(commission=0)
    cerebro.adddata(bt.feeds.PandasData(dataname=df));cerebro.addstrategy(m.SimpleCrossover,**kw)
    s=cerebro.run()[0]
    return s
s=run([100,100,99,98,100,102,101,100,99,98,101,103,102,100,99,98],fast=2,slow=3,stop_pct=.5)
check('two completed crossover trades',len(s.executions)==4)
check('one-unit entry and full exit, repeatable',[e['size'] for e in s.executions]==[1,-1,1,-1])
check('flat after reverse crossings',s.position.size==0)
check('entry next observed open',s.executions[0]['date']=='2020-01-07' and s.executions[0]['price']==101)
check('reverse exit next observed open',s.executions[1]['date']=='2020-01-09' and s.executions[1]['price']==99)
z=run([100]*60)
check('default-period flat-price warmup no trade',not z.executions)
t=run([100,100,99,98,100,102,101,98,97,96],fast=2,slow=3)
check('protective gap stop closes full position',[e['size'] for e in t.executions]==[1,-1])
check('stop gap fill uses observed open',t.executions[-1]['price']==98)
args.output.write_text(json.dumps(dict(status='PASS',checks=checks,executions=s.executions,stop_executions=t.executions,python=platform.python_version(),backtrader=bt.__version__,scope='Synthetic illustration only; no historical comparison rerun.'),indent=2))
print(json.dumps({'status':'PASS','checks':len(checks)}))
