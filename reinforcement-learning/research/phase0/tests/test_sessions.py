"""Session calendar tests."""

import datetime as dt

from rl.data.sessions import (in_session, session_date, session_open,
                              session_close, to_ist, IST)


def _utc(y, mo, d, h, mi):
    return dt.datetime(y, mo, d, h, mi, tzinfo=dt.timezone.utc)


def test_nse_session_bounds():
    # 09:15 IST = 03:45 UTC
    assert in_session(_utc(2026, 8, 27, 3, 45), "NSE")
    assert in_session(_utc(2026, 8, 27, 9, 59), "NSE")
    assert not in_session(_utc(2026, 8, 27, 3, 44), "NSE")
    assert not in_session(_utc(2026, 8, 27, 10, 0), "NSE")


def test_session_date_ist():
    # 03:45 UTC on Aug 27 = 09:15 IST Aug 27 (same day)
    assert session_date(_utc(2026, 8, 27, 3, 45), "NSE") == dt.date(2026, 8, 27)
    # 23:59 UTC Aug 26 = 05:29 IST Aug 27 (next day)
    assert session_date(_utc(2026, 8, 26, 23, 59), "NSE") == dt.date(2026, 8, 27)


def test_session_open_close():
    day = dt.date(2026, 8, 27)
    op = session_open(day, "NSE")
    cl = session_close(day, "NSE")
    assert op == _utc(2026, 8, 27, 3, 45)
    assert cl == _utc(2026, 8, 27, 9, 59)
    assert to_ist(op).hour == 9 and to_ist(op).minute == 15


def test_mcx_session():
    assert in_session(_utc(2026, 8, 27, 3, 30), "MCX")
    assert in_session(_utc(2026, 8, 27, 18, 0), "MCX")
    assert not in_session(_utc(2026, 8, 27, 18, 1), "MCX")
