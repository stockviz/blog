"""Trading-session calendars (research-plan §2.8).

NSE cash session  : 09:15–15:29 IST == 03:45–09:59 UTC (IST = UTC+5:30)
MCX futures        : 09:00–23:30 IST == 03:30–18:00 UTC (most commodities;
                     some contracts 09:00–21:00 IST — treat 23:30 as the default)

Session awareness: reset per-day agent state at session open (KB 05 §5.6);
overnight gaps are discontinuities — never carry LSTM hidden state across days.
"""

import datetime as _dt

IST = _dt.timezone(_dt.timedelta(hours=5, minutes=30))

NSE_OPEN_UTC = _dt.time(3, 45)
NSE_CLOSE_UTC = _dt.time(9, 59)
MCX_OPEN_UTC = _dt.time(3, 30)
MCX_CLOSE_UTC = _dt.time(18, 0)


def ist_now():
    return _dt.datetime.now(IST)


def to_ist(ts):
    """UTC tz-aware timestamp -> IST tz-aware timestamp."""
    if ts.tzinfo is None:
        ts = ts.replace(tzinfo=_dt.timezone.utc)
    return ts.astimezone(IST)


def in_session(ts, exchange="NSE"):
    """True if ts (UTC tz-aware) falls inside the trading session window."""
    if ts.tzinfo is None:
        ts = ts.replace(tzinfo=_dt.timezone.utc)
    t = ts.timetz().replace(tzinfo=None)
    if exchange == "NSE":
        return NSE_OPEN_UTC <= t <= NSE_CLOSE_UTC
    if exchange == "MCX":
        return MCX_OPEN_UTC <= t <= MCX_CLOSE_UTC
    raise ValueError(f"unknown exchange {exchange}")


def session_date(ts, exchange="NSE"):
    """The trading day a bar belongs to (IST calendar date)."""
    return to_ist(ts).date()


def session_open(day, exchange="NSE"):
    """UTC timestamp of the session open for an IST calendar date.
    NSE_OPEN_UTC/MCX_OPEN_UTC are UTC wall-clock — combine directly."""
    if exchange == "NSE":
        return _dt.datetime.combine(day, NSE_OPEN_UTC, tzinfo=_dt.timezone.utc)
    return _dt.datetime.combine(day, MCX_OPEN_UTC, tzinfo=_dt.timezone.utc)


def session_close(day, exchange="NSE"):
    """UTC timestamp of the session close for an IST calendar date."""
    if exchange == "NSE":
        return _dt.datetime.combine(day, NSE_CLOSE_UTC, tzinfo=_dt.timezone.utc)
    return _dt.datetime.combine(day, MCX_CLOSE_UTC, tzinfo=_dt.timezone.utc)


def is_weekend(day):
    return day.weekday() >= 5
