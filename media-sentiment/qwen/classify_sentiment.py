#!/usr/bin/env python3
"""Classify media and print articles with the local Qwen HTTP server.

Source databases are opened read-only. Results are written to a separate SQLite
file and can be resumed safely because each source item has a unique key.
"""

from __future__ import annotations

import argparse
import json
import re
import sqlite3
import sys
from datetime import datetime
from pathlib import Path
from typing import Iterable
from urllib.parse import urlsplit
from urllib.error import HTTPError
from urllib.request import Request, urlopen

LABELS = ("POSITIVE", "NEGATIVE", "NEUTRAL")


class Article:
    """Normalized input record ready for sentiment classification."""

    def __init__(
        self,
        item_id: str,
        title: str,
        body: str,
        date_stamp: str,
        source: str,
        source_type: str,
    ) -> None:
        self.item_id = item_id
        self.title = title
        self.body = body
        self.date_stamp = date_stamp
        self.source = source
        self.source_type = source_type


def read_only_connection(path: Path) -> sqlite3.Connection:
    """Open a SQLite database in read-only mode and enforce query-only access."""
    connection = sqlite3.connect(f"file:{path}?mode=ro", uri=True)
    connection.execute("PRAGMA query_only = ON")
    return connection


def date_stamp(value: object) -> str:
    """Convert YYYYMMDD database dates to an ISO date string."""
    text = str(value or "")
    try:
        return datetime.strptime(text, "%Y%m%d").date().isoformat()
    except ValueError:
        return text


def print_source(url: str) -> str:
    """Return the requested short print-source label for an article URL."""
    parsed = urlsplit(url.strip())
    if not parsed.hostname:
        return url.strip()
    host = parsed.hostname.lower().removeprefix("www.")
    if host == "livemint.com":
        return "livemint"
    return host.split(".", 1)[0]


def transcript_index(transcript_root: Path) -> dict[str, Path]:
    """Index transcript files by video ID, including source subdirectories."""
    return {path.stem: path for path in transcript_root.glob("*/audio/*.txt")}


def media_articles(database: Path, transcript_root: Path) -> list[Article]:
    """Read video articles into memory, then close MEDIA.db before inference."""
    transcripts = transcript_index(transcript_root)
    articles = []
    with read_only_connection(database) as connection:
        query = "SELECT ID, SRC, UPLOAD_DT, TITLE FROM META ORDER BY UPLOAD_DT, ID"
        for item_id, source, uploaded, title in connection.execute(query):
            transcript = transcripts.get(item_id)
            body = transcript.read_text(errors="replace") if transcript else ""
            articles.append(Article(
                item_id=item_id,
                title=title or "",
                body=body,
                date_stamp=date_stamp(uploaded),
                source=source or "UNKNOWN",
                source_type="VIDEO",
            ))
    return articles


def print_articles(database: Path) -> list[Article]:
    """Read print articles into memory, then close NEWS.db before inference."""
    articles = []
    with read_only_connection(database) as connection:
        query = "SELECT URL, TITLE, CONTENT, PUB_DT FROM ARTICLES ORDER BY PUB_DT, URL"
        for url, title, content, published in connection.execute(query):
            articles.append(Article(
                item_id=url,
                title=title or "",
                body=content or "",
                date_stamp=date_stamp(published),
                source=print_source(url),
                source_type="PRINT",
            ))
    return articles


def input_text(article: Article, max_chars: int) -> str:
    """Build a bounded prompt document so it fits the model context window."""
    text = f"Title: {article.title}\n\nContent:\n{article.body}".strip()
    return text[:max_chars]


def classify(server_url: str, article: Article, max_chars: int, timeout: int) -> str:
    """Ask llama-server for one sentiment label and validate the response."""
    result = None
    attempted_sizes = [max_chars, min(max_chars, 4000), min(max_chars, 2500)]
    for prompt_chars in dict.fromkeys(attempted_sizes):
        prompt = (
            "Classify the sentiment of this media article as exactly one of "
            "POSITIVE, NEGATIVE, or NEUTRAL. Judge the article's overall tone, "
            "not whether the subject is a good or bad investment. Reply with only "
            "the label.\n\n"
            f"{input_text(article, prompt_chars)}"
        )
        payload = {
            "model": "qwen2.5-0.5b-instruct",
            "messages": [
                {"role": "system", "content": "You are a precise sentiment classifier."},
                {"role": "user", "content": prompt},
            ],
            "temperature": 0,
            "max_tokens": 4,
        }
        request = Request(
            f"{server_url.rstrip('/')}/v1/chat/completions",
            data=json.dumps(payload).encode(),
            headers={"Content-Type": "application/json"},
            method="POST",
        )
        try:
            with urlopen(request, timeout=timeout) as response:
                result = json.load(response)
            break
        except HTTPError as exc:
            if exc.code != 400 or prompt_chars == attempted_sizes[-1]:
                raise
    if result is None:
        raise RuntimeError("Qwen server returned no result")
    content = result["choices"][0]["message"]["content"].upper()
    match = re.search(r"\b(POSITIVE|NEGATIVE|NEUTRAL|POS|NEG|NEU)\b", content)
    if not match:
        raise ValueError(f"Unrecognized model response: {content!r}")
    return {
        "POS": "POSITIVE",
        "NEG": "NEGATIVE",
        "NEU": "NEUTRAL",
    }.get(match.group(1), match.group(1))


def create_output_database(connection: sqlite3.Connection) -> None:
    """Create the resumable sentiment result table and useful indexes."""
    connection.executescript(
        """
        CREATE TABLE IF NOT EXISTS SENTIMENT (
            SOURCE_TYPE TEXT NOT NULL CHECK (SOURCE_TYPE IN ('VIDEO', 'PRINT')),
            ITEM_ID TEXT NOT NULL,
            DATE_STAMP TEXT NOT NULL,
            SOURCE TEXT NOT NULL,
            SENTIMENT TEXT NOT NULL CHECK (SENTIMENT IN ('POSITIVE', 'NEGATIVE', 'NEUTRAL')),
            TITLE TEXT NOT NULL,
            CREATED_AT TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
            PRIMARY KEY (SOURCE_TYPE, ITEM_ID)
        );
        CREATE INDEX IF NOT EXISTS IDX_SENTIMENT_DATE
            ON SENTIMENT (DATE_STAMP, SOURCE_TYPE, SOURCE);
        """
    )
    connection.commit()


def already_classified(connection: sqlite3.Connection, article: Article) -> bool:
    """Return whether a source item already has a saved result."""
    row = connection.execute(
        "SELECT 1 FROM SENTIMENT WHERE SOURCE_TYPE = ? AND ITEM_ID = ?",
        (article.source_type, article.item_id),
    ).fetchone()
    return row is not None


def classified_keys(connection: sqlite3.Connection) -> set[tuple[str, str]]:
    """Load saved keys once so resumed runs never re-submit completed items."""
    return set(connection.execute("SELECT SOURCE_TYPE, ITEM_ID FROM SENTIMENT"))


def save_result(connection: sqlite3.Connection, article: Article, sentiment: str) -> None:
    """Insert one validated classification into the output database."""
    connection.execute(
        """
        INSERT INTO SENTIMENT
            (SOURCE_TYPE, ITEM_ID, DATE_STAMP, SOURCE, SENTIMENT, TITLE)
        VALUES (?, ?, ?, ?, ?, ?)
        ON CONFLICT (SOURCE_TYPE, ITEM_ID) DO UPDATE SET
            DATE_STAMP = excluded.DATE_STAMP,
            SOURCE = excluded.SOURCE,
            SENTIMENT = excluded.SENTIMENT,
            TITLE = excluded.TITLE,
            CREATED_AT = CURRENT_TIMESTAMP
        """,
        (
            article.source_type,
            article.item_id,
            article.date_stamp,
            article.source,
            sentiment,
            article.title,
        ),
    )


def parse_args() -> argparse.Namespace:
    """Parse command-line options for paths, filtering, and inference settings."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--media-db", type=Path, default=Path("/mnt/data/recoll/MEDIA.db"))
    parser.add_argument("--news-db", type=Path, default=Path("/mnt/data/recoll/NEWS.db"))
    parser.add_argument("--transcript-root", type=Path, default=Path("/mnt/ssd1/stockviz/lob/youtube"))
    parser.add_argument(
        "--output-db",
        type=Path,
        default=Path("/mnt/data/blog/media-sentiment/qwen/SENTIMENT.db"),
    )
    parser.add_argument("--server-url", default="http://localhost:8080")
    parser.add_argument("--max-chars", type=int, default=6000)
    parser.add_argument("--timeout", type=int, default=120)
    parser.add_argument("--limit", type=int, help="Classify at most this many new records")
    parser.add_argument("--source-type", choices=("VIDEO", "PRINT"), help="Process only one source type")
    parser.add_argument("--overwrite", action="store_true", help="Reclassify existing records")
    return parser.parse_args()


def main() -> int:
    """Read both source databases, classify records, and persist resumable results."""
    args = parse_args()
    args.output_db.parent.mkdir(parents=True, exist_ok=True)
    output = sqlite3.connect(args.output_db)
    create_output_database(output)

    streams: list[Iterable[Article]] = []
    if args.source_type in (None, "VIDEO"):
        streams = [*streams, media_articles(args.media_db, args.transcript_root)]
    if args.source_type in (None, "PRINT"):
        streams = [*streams, print_articles(args.news_db)]

    completed = classified_keys(output)
    pending: list[Article] = []
    skipped = 0
    for stream in streams:
        for article in stream:
            key = (article.source_type, article.item_id)
            if not args.overwrite and key in completed:
                skipped += 1
                continue
            pending.append(article)
    if args.limit is not None:
        pending = pending[:args.limit]

    total = len(pending)
    print(f"already_processed={skipped} remaining={total}", flush=True)
    processed = failed = 0
    try:
        for index, article in enumerate(pending, start=1):
            key = (article.source_type, article.item_id)
            print(f"[{index}/{total}] {article.source_type} {article.item_id}", flush=True)
            try:
                sentiment = classify(args.server_url, article, args.max_chars, args.timeout)
                save_result(output, article, sentiment)
                output.commit()
                completed.add(key)
                processed += 1
                print(f"[{index}/{total}] {article.source_type} {article.item_id} {sentiment}", flush=True)
            except Exception as exc:  # keep a long batch running after one bad item
                failed += 1
                print(f"ERROR {article.source_type} {article.item_id}: {exc}", file=sys.stderr)
    finally:
        output.close()

    print(f"processed={processed} skipped={skipped} failed={failed}")
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
