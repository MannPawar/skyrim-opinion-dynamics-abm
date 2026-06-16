#!/usr/bin/env Rscript
# =====================================================================
#  SLM DIALOGUE REPORT  —  shareable HTML showcase
# ---------------------------------------------------------------------
#  Renders results/slm_dialogue_all.csv (produced by slm_dialogue_layer.R)
#  into a self-contained, browser-openable report curated for a reader:
#  the named key NPCs of each campaign plus a spread of commoners across
#  the opinion spectrum. Only in-character (Status == "ok") lines shown.
#
#  Usage:  Rscript slm_report.R
# =====================================================================

suppressPackageStartupMessages(library(data.table))

CFG <- list(
  input_csv     = "results/slm_dialogue_all.csv",
  output_html   = "results/slm_dialogue_report.html",
  scenarios     = c("Imperial", "Stormcloak"),
  commoners_n   = 8L,    # illustrative commoners per scenario
  imperial_col  = "#C0392B",
  stormcloak_col= "#2980B9"
)

esc <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

# Opinion bar: marker positioned on an Imperial(left)->Stormcloak(right) axis.
opinion_bar <- function(p) {
  pct <- round(100 * p, 1)
  sprintf(
    paste0('<div class="bar"><div class="bar-fill"></div>',
           '<div class="marker" style="left:%.1f%%"></div></div>',
           '<div class="bar-labels"><span>Imperial</span>',
           '<span class="pval">p = %.2f</span><span>Stormcloak</span></div>'),
    pct, p)
}

# Tidy internal data labels for display (underscores, placeholder holds).
pretty_city <- function(city) {
  city <- gsub("_", " ", city)
  city <- gsub("Transient Wilderness", "no fixed hold", city, fixed = TRUE)
  city
}

# Stricter display gate for the curated showcase: reject lines that pass the
# generator's validator but still read as broken (quote-spam, meta narration,
# self-narration like "Name, ... says:").
display_ok <- function(line) {
  if (is.null(line) || is.na(line) || nchar(line) < 12) return(FALSE)
  if (grepl('""', line)) return(FALSE)                       # quote-spam
  if (grepl(", says:|, speaking|in the .*voice of", line)) return(FALSE)
  if (grepl("^[A-Z][a-zA-Z' .-]{2,30}, ", line) &&
      !grepl("\\bI\\b|\\bI'|\\bmy\\b", substr(line, 1, 40))) return(FALSE)
  # require a reasonable share of letters (catches token-glitch lines)
  letters_frac <- nchar(gsub("[^A-Za-z ]", "", line)) / nchar(line)
  letters_frac >= 0.6
}

# Clean internal labels the model sometimes copies verbatim into a spoken line.
clean_dialogue <- function(line) {
  line <- gsub("Transient_Wilderness", "the wilds", line, fixed = TRUE)
  line <- gsub("_", " ", line)   # underscores never belong in speech
  trimws(gsub("\\s+", " ", line))
}

npc_card <- function(row) {
  meta <- paste(esc(row$Race), esc(pretty_city(row$Home_City)),
                paste0("hold: ", esc(row$Faction)), sep = " &middot; ")
  key_badge <- if (isTRUE(row$Is_Key)) '<span class="badge">key NPC</span>' else ''
  sprintf(
    paste0('<div class="card">',
           '<div class="card-head"><span class="name">%s</span>%s</div>',
           '<div class="meta">%s</div>',
           '%s',
           '<div class="stance">%s</div>',
           '<blockquote>%s</blockquote>',
           '</div>'),
    esc(row$Name), key_badge, meta, opinion_bar(row$Prob_t),
    esc(row$Stance), esc(clean_dialogue(row$Dialogue)))
}

# Pick commoners spread evenly across the opinion range (low->high p).
pick_commoners <- function(d, n) {
  comm <- d[Is_Key == FALSE & Status == "ok"]
  comm <- comm[vapply(Dialogue, display_ok, logical(1))]
  if (nrow(comm) == 0) return(comm[0])
  setorder(comm, Prob_t)
  idx <- unique(round(seq(1, nrow(comm), length.out = min(n, nrow(comm)))))
  comm[idx]
}

render_scenario <- function(d, scen) {
  ds <- d[Scenario == scen & Status == "ok"]
  if (nrow(ds) == 0) return(sprintf("<p><em>No in-character lines for %s.</em></p>", esc(scen)))
  keys <- ds[Is_Key == TRUE & vapply(Dialogue, display_ok, logical(1))]
  setorder(keys, Prob_t)
  comm <- pick_commoners(ds, CFG$commoners_n)
  outcome <- if (scen == "Imperial")
    "The Imperial Legion has won the civil war." else
    "Ulfric Stormcloak's rebellion has triumphed."
  paste0(
    sprintf('<section class="scenario %s"><h2>%s campaign</h2>',
            tolower(scen), esc(scen)),
    sprintf('<p class="outcome">%s</p>', esc(outcome)),
    '<h3>Named key NPCs</h3><div class="grid">',
    paste(vapply(seq_len(nrow(keys)), function(i) npc_card(keys[i]), character(1)),
          collapse = ""),
    '</div>',
    if (nrow(comm) > 0) paste0(
      '<h3>Commoners across the opinion spectrum</h3><div class="grid">',
      paste(vapply(seq_len(nrow(comm)), function(i) npc_card(comm[i]), character(1)),
            collapse = ""),
      '</div>') else "",
    '</section>')
}

main <- function() {
  if (!file.exists(CFG$input_csv))
    stop("Missing ", CFG$input_csv, " — run slm_dialogue_layer.R first.")
  d <- fread(CFG$input_csv)
  if (!"Scenario" %in% names(d)) stop("Expected a 'Scenario' column in the corpus CSV.")

  n_total <- nrow(d)
  n_ok    <- sum(d$Status == "ok")
  scen_html <- paste(vapply(CFG$scenarios, function(s) render_scenario(d, s), character(1)),
                     collapse = "")

  css <- sprintf('
    :root{--imp:%s;--sc:%s;}
    *{box-sizing:border-box}
    body{font-family:"Segoe UI",Roboto,Helvetica,Arial,sans-serif;color:#1c1c1c;
         max-width:1100px;margin:0 auto;padding:32px 24px;line-height:1.5;background:#fafafa}
    h1{font-size:26px;margin:0 0 4px}
    .sub{color:#555;margin:0 0 18px;font-size:15px}
    .method{background:#fff;border:1px solid #e2e2e2;border-radius:10px;padding:16px 20px;
            font-size:14px;color:#333;margin-bottom:28px}
    .method code{background:#f0f0f0;padding:1px 5px;border-radius:4px;font-size:13px}
    .stats{font-size:13px;color:#666;margin-top:8px}
    section.scenario{margin:34px 0}
    h2{font-size:21px;border-bottom:3px solid #ccc;padding-bottom:6px}
    .imperial h2{border-color:var(--imp)} .stormcloak h2{border-color:var(--sc)}
    .outcome{font-style:italic;color:#555;margin-top:-4px}
    h3{font-size:15px;text-transform:uppercase;letter-spacing:.04em;color:#777;margin:22px 0 10px}
    .grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(300px,1fr));gap:14px}
    .card{background:#fff;border:1px solid #e4e4e4;border-radius:10px;padding:14px 16px;
          box-shadow:0 1px 2px rgba(0,0,0,.04)}
    .card-head{display:flex;justify-content:space-between;align-items:baseline;gap:8px}
    .name{font-weight:600;font-size:16px}
    .badge{font-size:10px;background:#efe7d3;color:#7a5c11;border-radius:10px;
           padding:2px 7px;text-transform:uppercase;letter-spacing:.04em;white-space:nowrap}
    .meta{font-size:12px;color:#888;margin:2px 0 10px}
    .bar{position:relative;height:8px;border-radius:6px;
         background:linear-gradient(90deg,var(--imp),#ddd 50%%,var(--sc))}
    .marker{position:absolute;top:-3px;width:3px;height:14px;background:#111;border-radius:2px;
            transform:translateX(-50%%)}
    .bar-labels{display:flex;justify-content:space-between;font-size:10px;color:#999;margin:4px 0 10px}
    .pval{color:#444;font-weight:600}
    .stance{font-size:12.5px;font-style:italic;color:#666;margin-bottom:8px}
    blockquote{margin:0;padding:9px 12px;background:#f6f6f4;border-left:3px solid #bbb;
               border-radius:4px;font-size:14px;color:#222}
    .imperial blockquote{border-left-color:var(--imp)}
    .stormcloak blockquote{border-left-color:var(--sc)}
    footer{margin-top:40px;font-size:12px;color:#999;border-top:1px solid #e4e4e4;padding-top:14px}
  ', CFG$imperial_col, CFG$stormcloak_col)

  html <- paste0(
    '<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    '<title>Generative NPC Dialogue: Skyrim Opinion-Dynamics ABM</title>',
    '<style>', css, '</style></head><body>',
    '<h1>Generative NPC Dialogue from an Opinion-Dynamics ABM</h1>',
    '<p class="sub">Each line below is generated by a small language model conditioned ',
    'on an NPC&rsquo;s emergent political opinion from the agent-based simulation.</p>',
    '<div class="method">',
    '<strong>How this works.</strong> A stochastic agent-based model evolves a political ',
    'opinion <code>p</code> for each of 1,010 named <em>Skyrim</em> NPCs over a Civil War ',
    'campaign (<code>p&nbsp;&rarr;&nbsp;0</code> Imperial, <code>p&nbsp;&rarr;&nbsp;1</code> ',
    'Stormcloak). That opinion, not a hand-written script, is mapped to a stance ',
    'and passed to a local small language model (<code>deepseek-r1:1.5b</code> via Ollama), ',
    'which voices the character in-context. The ABM is the source of truth; the SLM is a ',
    'generative front-end.',
    sprintf('<div class="stats">Curated showcase &middot; corpus: %s lines across both ',
            format(n_total, big.mark=",")),
    sprintf('campaigns &middot; %.0f%% passed the in-character validator (%s/%s).</div></div>',
            100*n_ok/n_total, format(n_ok, big.mark=","), format(n_total, big.mark=",")),
    scen_html,
    '<footer>Generated by <code>slm_report.R</code> from <code>results/slm_dialogue_all.csv</code>. ',
    'Dialogue produced by a 1.5B-parameter local model; minor disfluencies are expected at this scale.</footer>',
    '</body></html>')

  writeLines(html, CFG$output_html, useBytes = TRUE)
  cat(sprintf("Wrote %s  (%d/%d in-character lines, curated showcase)\n",
              CFG$output_html, n_ok, n_total))
}

if (sys.nframe() == 0) main()
