#!/usr/bin/env Rscript
# =====================================================================
#  gen_eval_sample.R  --  stratified evaluation corpus for the LLM judge
# ---------------------------------------------------------------------
#  Ollama (llama3.2:3b) is the GENERATOR. This script draws a sample that
#  is stratified across the six stance bands in both campaign outcomes
#  (plus every key NPC), voices each with the KB-augmented pipeline, and
#  writes results/slm_dialogue_sample.csv. An external judge (Claude)
#  then scores each line; judge_report.R aggregates the scores.
# =====================================================================
suppressPackageStartupMessages({ library(data.table); library(httr2); library(jsonlite) })
source("slm_dialogue_layer.R", local = TRUE)

set.seed(CFG$seed)
PER_BAND <- 8L                 # commoners sampled per stance band per scenario
OUT      <- "results/slm_dialogue_sample.csv"

band_of <- function(p) {
  cut(p, breaks = c(-Inf, 0.15, 0.35, 0.50, 0.65, 0.85, Inf),
      labels = 1:6, right = FALSE)
}

full <- fread(CFG$input_csv)
kb   <- load_kb(CFG$kb_csv)

pick_rows <- function(d) {
  d[, band := band_of(Prob_t)]
  keys <- d[Is_Key_NPC == TRUE]
  comm <- d[Is_Key_NPC == FALSE]
  samp <- comm[, .SD[sample(.N, min(PER_BAND, .N))], by = band]
  unique(rbind(keys, samp), by = "Name")
}

out <- list()
t0  <- Sys.time()
for (scen in CFG$scenarios) {
  d   <- full[scenario == scen & run_id == CFG$run_id]
  sel <- pick_rows(d)
  cat(sprintf("=== %s: voicing %d NPCs (%d key) ===\n",
              scen, nrow(sel), sum(sel$Is_Key_NPC)))
  for (i in seq_len(nrow(sel))) {
    row <- sel[i]
    kbe <- kb_lookup(kb, row$Name)
    res <- generate_line(row, scen, kb_entry = kbe)
    out[[length(out) + 1L]] <- data.table(
      Scenario = scen, Name = row$Name, Race = row$Race,
      Home_City = row$Home_City, Faction = row$Faction,
      Is_Key = row$Is_Key_NPC, Band = as.integer(row$band),
      Prob_t = round(row$Prob_t, 3), Stance = stance_descriptor(row$Prob_t),
      Dialogue = res$line, Status = res$status,
      KB_Augmented = !is.null(kbe))
    if (length(out) %% 20 == 0L) {
      fwrite(rbindlist(out), OUT)
      cat(sprintf("  ...%d done (%.1f min)\n", length(out),
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    }
  }
}
fwrite(rbindlist(out), OUT)
res_dt <- rbindlist(out)
cat(sprintf("\nDONE: %d lines | validator-ok %d (%.0f%%) | wrote %s\n",
            nrow(res_dt), sum(res_dt$Status == "ok"),
            100 * mean(res_dt$Status == "ok"), OUT))
