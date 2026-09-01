03 MCX liquidity and continuity audit

Scope
- BHAV_COM_MCX EOD data.
- GOLD, SILVER, CRUDEOIL, NATURALGAS, COPPER.
- Front-series observations are identified with EXPIRY_SERIES = 0.
- No currency data is used.

Measured audit

| Contract | First | Pre obs | Post obs | Zero-return days | >15% days | Max abs return |
|---|---|---:|---:|---:|---:|---:|
| GOLD | 2003-11-10 | 4,577 | 1,632 | 60 | 0 | 11.7% |
| SILVER | 2003-11-10 | 4,566 | 1,632 | 40 | 4 | 27.0% |
| CRUDEOIL | 2005-02-09 | 4,240 | 1,632 | 59 | 17 | 132,300.0% |
| NATURALGAS | 2006-07-11 | 3,821 | 1,632 | 81 | 18 | 42.7% |
| COPPER | 2004-06-04 | 4,278 | 1,632 | 63 | 3 | 23.2% |

Interpretation

The database contains sufficient history for all five candidates, but the
current front-series audit is not sufficient to admit a commodity return stream.
CRUDEOIL has an implausible 1323x daily move, which is a continuity/schema/contract
problem rather than investable P&L. Natural gas and silver also have large moves
that require contract and multiplier validation. Therefore the explicit plan gate
stops the commodity allocation experiment here rather than fabricating a clean
continuous series. The next step is a contract-safe roll using expiry and lot
metadata, followed by a second audit; only then should equal-weight or volatility-
balanced sleeves be tested.

Files
- run.R: executable five-contract audit.
- screening.csv: generated audit results.
- explore.R, explore2.R, explore3.R: exploratory schema/continuity probes retained
  for the next contract-safe roll implementation.
