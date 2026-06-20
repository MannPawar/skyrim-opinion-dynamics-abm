#!/usr/bin/env python3
# =====================================================================
#  build_lore_kb.py  --  NPC lore knowledge base for the SLM layer
# ---------------------------------------------------------------------
#  Builds a per-NPC knowledge base (NOT a RAG index) from the UESP wiki.
#  Every named NPC is known at inference time, so we do a deterministic
#  entity lookup rather than semantic retrieval: for each roster row we
#  fetch that NPC's exact UESP page (the roster already stores the URL),
#  pull a one-line description and a handful of the NPC's own canonical
#  spoken lines, and write them to a flat CSV the R dialogue layer joins
#  by Name.
#
#  The canonical lines are the key field: feeding 2-3 of an NPC's real
#  in-game lines calibrates the small model's VOICE far better than any
#  abstract persona description.
#
#  Source of truth stays the ABM. This only supplies static lore so the
#  generated dialogue sounds like a specific person, not a stance band.
#
#  Polite by construction: single-threaded, ~0.7s between requests, a
#  descriptive User-Agent, maxlag, on-disk wikitext cache, and resume
#  (already-scraped NPCs are skipped), so re-runs cost no extra requests.
#
#  Usage:  python build_lore_kb.py
#  Output: data/npc_lore_kb.csv   +   data/.uesp_cache/<page>.wikitext
# =====================================================================

import csv
import json
import os
import re
import sys
import time
import urllib.parse
import urllib.request

ROSTER   = os.path.join("People of Skyrim", "Skyrim_Named_Characters.csv")
OUT_CSV  = os.path.join("data", "npc_lore_kb.csv")
CACHE    = os.path.join("data", ".uesp_cache")
API      = "https://en.uesp.net/w/api.php"
UA       = ("SkyrimOpinionDynamics-LoreKB/1.0 "
            "(research project; contact mannspawar1@gmail.com)")
DELAY_S  = 0.7          # be gentle on a community wiki
MAX_LINES = 6           # canonical lines stored per NPC
TIMEOUT  = 30

# Words that mark a line as politically/characterfully revealing -> rank first.
POLITICAL = re.compile(
    r"\b(empire|imperial|stormcloak|ulfric|talos|skyrim|jarl|legion|war|"
    r"thalmor|rebel|king|crown|nord|freedom|loyal|traitor|tullius|elisif)\b",
    re.I)

# Generic merchant/service barks we keep but rank last (still useful voice).
SERVICE = re.compile(r"\b(buy|sell|wares|coin|gold|shop|store|forge|smith|"
                     r"arrows|armor for|weapons for)\b", re.I)


def fetch_wikitext(page):
    """Return raw wikitext for a UESP page, using an on-disk cache."""
    safe = re.sub(r"[^A-Za-z0-9_.-]", "_", page)
    cpath = os.path.join(CACHE, safe + ".wikitext")
    if os.path.exists(cpath):
        with open(cpath, encoding="utf-8") as fh:
            return fh.read()
    params = {
        "action": "parse", "page": page, "prop": "wikitext",
        "format": "json", "formatversion": "2", "maxlag": "5",
    }
    url = API + "?" + urllib.parse.urlencode(params)
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=TIMEOUT) as r:
        data = json.load(r)
    wt = data.get("parse", {}).get("wikitext", "") or ""
    with open(cpath, "w", encoding="utf-8") as fh:
        fh.write(wt)
    time.sleep(DELAY_S)          # only sleep on a real network hit
    return wt


def strip_markup(s):
    """Reduce wiki markup to plain readable text."""
    s = re.sub(r"\{\{[^{}]*\}\}", "", s)                 # templates
    s = re.sub(r"\[\[[^\]|]*\|([^\]]*)\]\]", r"\1", s)   # [[A|B]] -> B
    s = re.sub(r"\[\[([^\]]*)\]\]", r"\1", s)            # [[A]]   -> A
    s = re.sub(r"Skyrim:|Dawnguard:|Dragonborn:", "", s)  # namespace residue
    s = s.replace("'''", "").replace("''", "")           # bold/italic
    s = re.sub(r"<br\s*/?>", " ", s, flags=re.I)         # line breaks
    s = re.sub(r"<[^>]+>", "", s)                        # any other tags
    s = re.sub(r"&nbsp;", " ", s)
    s = re.sub(r"\s+", " ", s)
    return s.strip()


def extract_description(wt):
    """Lead sentence(s): the '''Name''' is a ... paragraph before any section."""
    head = re.split(r"\n==", wt, maxsplit=1)[0]
    # drop the leading {{NPC Summary ...}} infobox
    head = re.sub(r"\{\{NPC Summary.*?\n\}\}", "", head,
                  flags=re.S | re.I)
    # first non-empty content line that starts the bold name intro
    para = ""
    for block in head.split("\n"):
        b = block.strip()
        if b.startswith("'''") or re.match(r"^[A-Z].*\bis a\b", strip_markup(b)):
            para = b
            break
    desc = strip_markup(para)
    # keep it to roughly the first two sentences
    parts = re.split(r"(?<=[.!?])\s+", desc)
    return " ".join(parts[:2]).strip()


def extract_lines(wt):
    """All of the NPC's own spoken lines, formatted ''\"...\"'' in wikitext."""
    raw = re.findall(r"''\"(.+?)\"''", wt)
    seen, cleaned = set(), []
    for ln in raw:
        t = strip_markup(ln)
        if not (12 <= len(t) <= 200):
            continue
        if "<" in t or t.lower() in seen:
            continue
        # skip lines that are just a quest/dialogue stage direction fragment
        if t.endswith(("...", "?")) and len(t) < 18:
            continue
        seen.add(t.lower())
        cleaned.append(t)
    # rank: first-person + political/character lines first, service barks last.
    # First-person bias favours the NPC's OWN speech over lines the wiki's
    # dialogue section quotes other characters saying *to* this NPC.
    def score(t):
        s = 0
        if re.search(r"\b(I|I'm|I'll|I've|my|me)\b", t):
            s += 2
        if POLITICAL.search(t):
            s += 2
        if SERVICE.search(t):
            s -= 1
        return s
    cleaned.sort(key=score, reverse=True)
    return cleaned[:MAX_LINES]


def page_title_from_url(url):
    """Derive the API page title (e.g. 'Skyrim:Beirand') from the roster URL."""
    if not url:
        return None
    path = urllib.parse.urlparse(url).path          # /wiki/Skyrim:Beirand
    title = path.split("/wiki/", 1)[-1]
    return urllib.parse.unquote(title) or None


def load_done(out_csv):
    if not os.path.exists(out_csv):
        return {}, []
    rows, done = [], {}
    with open(out_csv, encoding="utf-8", newline="") as fh:
        for row in csv.DictReader(fh):
            rows.append(row)
            done[row["Name"]] = row
    return done, rows


def main():
    os.makedirs(CACHE, exist_ok=True)
    if not os.path.exists(ROSTER):
        sys.exit("Roster not found: " + ROSTER)

    with open(ROSTER, encoding="utf-8", newline="") as fh:
        roster = list(csv.DictReader(fh))

    done, prior_rows = load_done(OUT_CSV)
    fields = ["Name", "uesp_url", "occupation", "morality",
              "description", "n_lines", "canonical_lines", "kb_found"]

    out_rows = {r["Name"]: r for r in prior_rows}
    total = len(roster)
    n_new = n_found = 0
    print(f"Roster: {total} NPCs | already done: {len(done)}", flush=True)

    for i, npc in enumerate(roster, 1):
        name = (npc.get("Name") or "").strip()
        if not name or name in done:
            continue
        url = (npc.get("Source") or "").strip()
        title = page_title_from_url(url)
        occ = strip_markup(npc.get("Class") or "")
        moral = (npc.get("Morality") or "").strip()

        desc, lines, found = "", [], False
        if title:
            try:
                wt = fetch_wikitext(title)
                if wt:
                    desc = extract_description(wt)
                    lines = extract_lines(wt)
                    found = bool(desc or lines)
            except Exception as e:                       # noqa: BLE001
                print(f"  [{i}/{total}] {name}: ERR {e}", flush=True)

        out_rows[name] = {
            "Name": name,
            "uesp_url": url,
            "occupation": occ,
            "morality": moral,
            "description": desc,
            "n_lines": len(lines),
            "canonical_lines": " | ".join(lines),
            "kb_found": "TRUE" if found else "FALSE",
        }
        n_new += 1
        n_found += int(found)

        if n_new % 25 == 0:
            _flush(OUT_CSV, fields, roster, out_rows)
            print(f"  [{i}/{total}] +{n_new} new ({n_found} with lore) "
                  f"...checkpoint", flush=True)

    _flush(OUT_CSV, fields, roster, out_rows)
    with_lines = sum(1 for r in out_rows.values()
                     if int(r.get("n_lines", 0) or 0) > 0)
    print(f"DONE. {len(out_rows)} NPCs in KB | "
          f"{with_lines} have canonical lines | wrote {OUT_CSV}", flush=True)


def _flush(out_csv, fields, roster, out_rows):
    order = [(npc.get("Name") or "").strip() for npc in roster]
    with open(out_csv, "w", encoding="utf-8", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=fields)
        w.writeheader()
        written = set()
        for nm in order:
            if nm in out_rows and nm not in written:
                w.writerow(out_rows[nm])
                written.add(nm)
        for nm, row in out_rows.items():          # any not in roster order
            if nm not in written:
                w.writerow(row)


if __name__ == "__main__":
    main()
