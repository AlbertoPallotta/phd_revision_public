# Preparation of historical inputs; all five original input/output locations are explicit.
from pathlib import Path
import argparse,csv,hashlib,json,shutil,sqlite3,zipfile
parser=argparse.ArgumentParser(description="Prepare the recorded historical Chapter 5 inputs; no current-package substitution.")
parser.add_argument('--output',type=Path,required=True)
parser.add_argument('--archive',type=Path,required=True,help="Original illustration ZIP, SHA-256 checked below")
parser.add_argument('--source',type=Path,required=True,help="Recovered historical runner/helper/table directory")
parser.add_argument('--baseline',type=Path,required=True,help="Historical PT9007 bundle containing PortfolioTesteR/")
parser.add_argument('--thesis',type=Path,required=True,help="Original submitted thesis, retained locally for input identity only")
args=parser.parse_args()
O=args.output.resolve();archive=args.archive.resolve();source=args.source.resolve();B=args.baseline.resolve();thesis=args.thesis.resolve()
for path in [archive,source,B/'PortfolioTesteR/DESCRIPTION',thesis]:
 if not path.exists():parser.error('Missing historical input: '+str(path))
if O.exists():parser.error('Output exists; choose a new folder.')
if 'Version: 0.1.4.9007' not in (B/'PortfolioTesteR/DESCRIPTION').read_text(encoding='utf-8'):
 parser.error('Historical PT9007 baseline required.')
P=archive.parent
def sha(p):
 h=hashlib.sha256()
 with p.open('rb') as f:
  for block in iter(lambda:f.read(1024*1024),b''):h.update(block)
 return h.hexdigest()
assert not O.exists()
for d in ['inputs','scripts','evidence','logs','internal','results']:(O/d).mkdir(parents=True,exist_ok=True)
files=[archive,P/'CANNOT_EXIST'][:1]
files += [p for p in (B/'PortfolioTesteR').rglob('*') if p.is_file()]
files += [p for p in source.iterdir() if p.is_file()]
files += [thesis]
rows=[{'path':str(p),'bytes':p.stat().st_size,'sha256':sha(p)} for p in files]
with (O/'evidence/frozen-input-manifest.csv').open('w',encoding='utf-8-sig',newline='') as f:
 w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
assert rows[0]['sha256']=='0b7bf7214603f87cb4b822229430d50df909f4ce905efc5d8722b682c2cb6143'
with zipfile.ZipFile(archive) as z:
 for n in ['sp500_history.db','.RData']:
  member='portfolio tester illustration/'+n
  with z.open(member) as f,(O/'inputs'/n).open('wb') as out:shutil.copyfileobj(f,out)
for n in ['runner.R','ml_helpers.R','recommended_symbols_2014_2024.txt','sp500_sectors.csv',
          'table_A_cost_sensitivity_H13_net.csv','table_B_horizon_comparison_full.csv','table_C_split_stability_H13.csv']:
 shutil.copy2(source/n,O/'inputs'/n)
db=O/'inputs/sp500_history.db'
con=sqlite3.connect(db.as_uri()+'?mode=ro',uri=True)
con.execute('PRAGMA query_only=ON')
columns=[r[1] for r in con.execute("PRAGMA table_info('stocks')")]
assert 'price' in columns
price='price_adjusted' if 'price_adjusted' in columns else 'price'
symbols=[s.strip() for s in (O/'inputs/recommended_symbols_2014_2024.txt').read_text(encoding='utf-8-sig').splitlines() if s.strip() and s.strip() not in ['SPY','^VIX']]
assert len(symbols)==98 and len(set(symbols))==98
query='SELECT date, symbol, '+price+' as price FROM stocks WHERE symbol IN ('+','.join('?' for _ in symbols)+') AND date >= ? AND date <= ? ORDER BY date, symbol'
data=con.execute(query,symbols+['2014-01-01','2024-01-01']).fetchall()
assert data
with (O/'internal/sql-query-data.csv').open('w',encoding='utf-8',newline='') as f:
 w=csv.writer(f);w.writerow(['date','symbol','price']);w.writerows(data)
check=con.execute('PRAGMA quick_check').fetchone()[0];con.close();assert check=='ok'
record={'archive_sha256':rows[0]['sha256'],'database_sha256':sha(db),'database_bytes':db.stat().st_size,
 'database_schema_columns':columns,'selected_price_column':price,'requested_adjusted':True,
 'adapter_fallback_to_price':price=='price','rows':len(data),'universe_count':len(symbols),
 'first_date':data[0][0],'last_date':data[-1][0],'sqlite_quick_check':check,
 'access':'Extracted local copy opened read-only. Python executes the same adapter SQL; R will use package standardisation and weekly grouping.',
 'input_files':[{'path':str(p),'bytes':p.stat().st_size,'sha256':sha(p)} for p in (O/'inputs').iterdir() if p.is_file()]}
(O/'evidence/input-identity.json').write_text(json.dumps(record,indent=2),encoding='utf-8')
(O/'RUN_PLAN.txt').write_text('''HISTORICAL CONTROLLED COMPARISON, 20 OCTOBER 2026
This local experiment compares momentum-feature policies using the specification
below. Both arms are retained regardless of ranking, without tuning to retain
a desired winner.

Use the recovered runner's 98-name universe, supplied database/sector map,
2014-01-01 through 2024-01-01 request, weekly grouping, seed 42, XGBoost with
eta=.05/depth4/200 rounds/subsample1/colsample1/nthread1, horizons 1/4/13,
156/4/4 schedule, pooled versus per_group, top20/softmax12/name cap.10/sector cap3,
initial capital100000 and cost grid5/10/15/20/25/50 bps. Keep lagging, labels,
training schedule, execution and collector definitions unchanged.

Arm legacy: exact ml_prepare_features default momentum12/26/52 behaviour.
Arm propagate: explicitly replace only those three feature panels using
panel_lag(calc_momentum(prices,lookback,na_policy="propagate"),1L).
The existing roll_fit_predict omission policy handles unavailable features.
No package namespace/default edits; do not add a new scoring or timing policy.

Primary ranking criterion: the illustration's reported gross risk-adjusted ratio
(its existing perf_metrics CAGR/volatility convention), for each horizon and
across six variants. Report gross returns, volatility, drawdown, turnover and
net-cost rankings as well; do not declare a winner from a selectively chosen
metric. Report Full/Pre/Post and exact changed-window/signal counts where possible.
Use matching fresh-baseline inputs/backend for attribution; differences from
historical rounded tables remain a separate provenance/comparison issue.

No strategy hyperparameter changes, label purging, cost/calendar redesign,
new data acquisition or TradeTesterR changes form part of this comparison.
Full price/RDS objects remain local and are not distributed.
''',encoding='utf-8')
shutil.copy2(Path(__file__),O/'scripts'/Path(__file__).name)
print(json.dumps({k:v for k,v in record.items() if k!='input_files'},indent=2))
