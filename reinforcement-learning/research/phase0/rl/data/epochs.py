"""Per-table tick_stamp epoch decoding (research-plan §1.3).

tick_stamp is NOT the same epoch in every table:
- zd_index_bars : Unix epoch (1970-01-01 UTC)   [CandleDownloader: "doesn't offset by basedate"]
- zd_bars_mcx   : Unix epoch (1970-01-01 UTC)   [MCX downloader]
- zd_option_bars: seconds since 1990-01-01      [DerivativeBarsDownloader baseDate]

Decoding with the wrong base silently shifts timestamps by 20 years —
every loader must go through decode_tick()/encode_tick() below.
"""

import datetime as _dt

TICK_EPOCH_1970 = _dt.datetime(1970, 1, 1, tzinfo=_dt.timezone.utc)
TICK_EPOCH_1990 = _dt.datetime(1990, 1, 1, tzinfo=_dt.timezone.utc)

TICK_EPOCHS = {
    "zd_index_bars": 1970,
    "zd_bars_mcx": 1970,
    "zd_option_bars": 1990,
}

# Known-bar regression reference (research-plan §1.3):
# NIFTY 50 2026-08-27 first bar = 2026-08-27 03:45:00 UTC = 1787802300 (Unix).
KNOWN_BAR = {"unix_ts": 1787802300, "utc": _dt.datetime(2026, 8, 27, 3, 45, 0, tzinfo=_dt.timezone.utc)}


def epoch_origin(year):
    if year == 1970:
        return TICK_EPOCH_1970
    if year == 1990:
        return TICK_EPOCH_1990
    raise ValueError(f"unsupported tick epoch year: {year} (use 1970 or 1990)")


def decode_tick(tick_stamp, epoch=1970):
    """tick_stamp -> tz-aware UTC datetime (pandas Timestamp)."""
    import pandas as pd
    return pd.Timestamp(epoch_origin(epoch) + _dt.timedelta(seconds=int(tick_stamp)))


def encode_tick(ts, epoch=1970):
    """datetime -> tick_stamp for the given epoch base (naive = UTC)."""
    if ts.tzinfo is None:
        ts = ts.replace(tzinfo=_dt.timezone.utc)  # naive -> interpret as UTC
    return int((ts - epoch_origin(epoch)).total_seconds())


def decode_series(ticks, epoch=1970):
    """Array of tick_stamps -> DatetimeIndex (UTC, tz-aware)."""
    import pandas as pd
    return pd.DatetimeIndex([decode_tick(t, epoch) for t in ticks])


def assert_known_bar():
    """Unit-test the decoder against the plan's known bar."""
    assert decode_tick(KNOWN_BAR["unix_ts"], 1970) == KNOWN_BAR["utc"], "Unix decode of known bar failed"
    assert encode_tick(KNOWN_BAR["utc"], 1970) == KNOWN_BAR["unix_ts"], "Unix encode of known bar failed"
    # 1990 base sanity: 1990-01-01 00:00:00 UTC == 0
    assert encode_tick(_dt.datetime(1990, 1, 1, tzinfo=_dt.timezone.utc), 1990) == 0
    assert decode_tick(0, 1990) == _dt.datetime(1990, 1, 1, tzinfo=_dt.timezone.utc)
