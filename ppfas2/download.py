#!/usr/bin/env python3
"""Download PPFAS portfolio disclosure files with retry logic.

Reads configuration from config/config.json relative to this script.
"""

import json
import os
import sys
import time
import requests
from concurrent.futures import ThreadPoolExecutor, as_completed
from urllib.parse import urlparse, unquote


def load_config():
    """Load config from config/config.json relative to this script."""
    script_dir = os.path.dirname(os.path.abspath(__file__))
    config_path = os.path.join(script_dir, "config", "config.json")
    if not os.path.exists(config_path):
        print(f"Config not found: {config_path}", file=sys.stderr)
        sys.exit(1)
    with open(config_path) as f:
        cfg = json.load(f)

    # Resolve paths relative to script directory
    cfg["_script_dir"] = script_dir
    cfg["download_dir"] = os.path.join(script_dir, cfg["download_dir"])
    cfg["urls_file"] = os.path.join(script_dir, cfg["urls_file"])
    return cfg


def build_session(cfg):
    """Create a requests Session with configured headers."""
    session = requests.Session()
    headers = {"User-Agent": cfg["user_agent"]}
    headers.update(cfg.get("headers", {}))
    session.headers.update(headers)
    return session


def filename_from_url(url):
    """Extract a clean filename from URL, stripping query params."""
    parsed = urlparse(url)
    path = unquote(parsed.path)
    return os.path.basename(path)


def download_one(url, dest_dir, session, cfg):
    """Download a single file with retries. Returns (url, status, size)."""
    fname = filename_from_url(url)
    dest = os.path.join(dest_dir, fname)
    max_retries = cfg["max_retries"]
    retry_delay = cfg["retry_delay_seconds"]
    timeout = cfg["timeout_seconds"]
    min_size = cfg["min_content_bytes"]

    # Skip if already downloaded and non-zero
    if os.path.exists(dest) and os.path.getsize(dest) > 0:
        return (url, "skipped", os.path.getsize(dest))

    for attempt in range(1, max_retries + 1):
        try:
            resp = session.get(url, timeout=timeout, stream=True)
            if resp.status_code == 200:
                content = resp.content
                if len(content) < min_size:
                    if attempt < max_retries:
                        time.sleep(retry_delay)
                        continue
                    return (url, f"too_small_{len(content)}", 0)

                with open(dest, "wb") as f:
                    f.write(content)
                return (url, "ok", len(content))
            elif resp.status_code == 404:
                return (url, "404", 0)
            else:
                if attempt < max_retries:
                    time.sleep(retry_delay)
                    continue
                return (url, f"http_{resp.status_code}", 0)
        except Exception as e:
            if attempt < max_retries:
                time.sleep(retry_delay)
                continue
            return (url, f"error_{type(e).__name__}", 0)

    return (url, "failed", 0)


def main():
    cfg = load_config()
    session = build_session(cfg)

    if not os.path.exists(cfg["urls_file"]):
        print(f"URLs file not found: {cfg['urls_file']}", file=sys.stderr)
        sys.exit(1)

    with open(cfg["urls_file"]) as f:
        urls = [line.strip() for line in f if line.strip()]

    print(f"Downloading {len(urls)} files to {cfg['download_dir']}")

    os.makedirs(cfg["download_dir"], exist_ok=True)

    results = {"ok": 0, "skipped": 0, "404": 0, "error": 0}
    errors = []

    with ThreadPoolExecutor(max_workers=cfg["max_workers"]) as executor:
        futures = {
            executor.submit(download_one, url, cfg["download_dir"], session, cfg): url
            for url in urls
        }

        for i, future in enumerate(as_completed(futures), 1):
            url = futures[future]
            _, status, size = future.result()

            if status in ("ok", "skipped"):
                results[status] += 1
                if i % 10 == 0 or i == len(urls):
                    print(
                        f"  [{i}/{len(urls)}] {results['ok']} ok, {results['skipped']} skipped...",
                        flush=True,
                    )
            elif status == "404":
                results["404"] += 1
                errors.append(f"404: {url}")
                print(f"  [{i}/{len(urls)}] 404: {filename_from_url(url)}", flush=True)
            else:
                results["error"] += 1
                errors.append(f"{status}: {url}")
                print(
                    f"  [{i}/{len(urls)}] ERROR: {filename_from_url(url)} - {status}",
                    flush=True,
                )

    print(
        f"\nDone! ok={results['ok']} skipped={results['skipped']} "
        f"404={results['404']} error={results['error']}"
    )

    if errors:
        print(f"\nErrors ({len(errors)}):")
        for e in errors[:20]:
            print(f"  {e}")
        if len(errors) > 20:
            print(f"  ... and {len(errors) - 20} more")

    files = sorted(os.listdir(cfg["download_dir"]))
    total_size = sum(
        os.path.getsize(os.path.join(cfg["download_dir"], f)) for f in files
    )
    print(f"\nTotal files on disk: {len(files)}")
    print(f"Total size: {total_size / 1024 / 1024:.1f} MB")


if __name__ == "__main__":
    main()
