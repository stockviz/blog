"""Epoch decoder tests (research-plan §1.3 — the base-date trap)."""

import datetime as dt

import pytest

from rl.data.epochs import decode_tick, encode_tick, decode_series, assert_known_bar, KNOWN_BAR, TICK_EPOCHS


def test_known_bar_unix():
    assert_known_bar()  # 1787802300 -> 2026-08-27 03:45:00 UTC


def test_epoch_table():
    assert TICK_EPOCHS["zd_index_bars"] == 1970
    assert TICK_EPOCHS["zd_bars_mcx"] == 1970
    assert TICK_EPOCHS["zd_option_bars"] == 1990  # DerivativeBarsDownloader baseDate


def test_1990_base():
    origin = dt.datetime(1990, 1, 1, tzinfo=dt.timezone.utc)
    assert encode_tick(origin, 1990) == 0
    assert decode_tick(0, 1990) == origin


def test_1970_vs_1990_differ_by_20y():
    ts = 1_000_000_000
    d70 = decode_tick(ts, 1970)
    d90 = decode_tick(ts, 1990)
    assert (d90 - d70).days == 7305  # 20 years


def test_decode_series():
    import pandas as pd
    idx = decode_series([0, 60, 120], 1970)
    assert len(idx) == 3
    assert idx[0] == pd.Timestamp("1970-01-01 00:00:00+00:00")
    assert (idx[1] - idx[0]).seconds == 60
