#!/usr/bin/env Rscript
# =====================================================================
#  SLM DIALOGUE LAYER  —  generative front-end for the opinion-dynamics ABM
# ---------------------------------------------------------------------
#  The ABM (FINALCOMPARATIVECAMPAIGN.R) remains the source of truth: it
#  produces a terminal opinion state p_{i,t} = P(support Empire) for each
#  of the 1,010 named NPCs. This module reads that state and conditions a
#  small language model (DeepSeek-R1 1.5B, served locally via Ollama) to
#  generate lore-coherent, in-character dialogue per NPC.
#
#  Pipeline:  results/final_opinions.csv  ->  prompt(p_i, faction, world)
#             ->  Ollama /api/generate  ->  strip <think>  ->  slm_dialogue.csv
#
#  Usage:  Rscript slm_dialogue_layer.R
#  Requires: a running `ollama serve` and the model pulled:
#            ollama pull deepseek-r1:1.5b
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(httr2)
  library(jsonlite)
})

# ----------------------------- CONFIG --------------------------------
CFG <- list(
  ollama_url  = "http://localhost:11434/api/generate",
  model       = "deepseek-r1:1.5b",
  input_csv   = "results/final_opinions.csv",
  output_csv  = "results/slm_dialogue_all.csv",
  scenarios   = c("Imperial", "Stormcloak"),  # voice both campaigns
  run_id      = 1L,           # which Monte Carlo replication to voice
  all_npcs    = TRUE,         # TRUE = every NPC; FALSE = key_only + sample_n
  key_only    = TRUE,
  sample_n    = 4L,
  temperature = 0.6,
  num_predict = 768L,         # generous: R1 thinks a lot; answer must survive
  seed        = 42L,
  max_retries = 2L,           # re-roll empty / out-of-character / non-English
  timeout_s   = 180,
  checkpoint_every = 50L      # flush partial results to disk this often
)

# ------------------------ OPINION -> STANCE --------------------------
# p = P(faction alignment) on the ABM scale where LOW p = Imperial and
# HIGH p = Stormcloak (cf. paper Sec. 4.1: Imperial scenario mean p=0.295
# is "94.4% Imperial-leaning").  Map to a first-person ideological stance
# the SLM can voice.  Phrasing is symmetric around the neutral zone.
stance_descriptor <- function(p) {
  if (p < 0.15) {
    "a staunch Imperial loyalist convinced that only the Empire can hold Skyrim together"
  } else if (p < 0.35) {
    "broadly loyal to the Empire, believing it is Skyrim's best shield against the Thalmor, though not a zealot"
  } else if (p < 0.50) {
    "conflicted about the war, leaning slightly toward the Empire but far from certain"
  } else if (p < 0.65) {
    "conflicted about the war, leaning slightly toward Ulfric's rebellion but far from certain"
  } else if (p < 0.85) {
    "sympathetic to the Stormcloak cause and distrustful of the Empire, though not a zealot"
  } else {
    "a fervent Stormcloak at heart who longs to see the Empire driven from Skyrim and free worship of Talos restored"
  }
}

# World outcome of the campaign the NPC has just lived through.
scenario_world <- function(scenario) {
  if (scenario == "Imperial") {
    "The civil war is over: the Imperial Legion has won, and Imperial banners now fly over the holds of Skyrim."
  } else {
    "The civil war is over: Ulfric Stormcloak's rebellion has triumphed, and Skyrim is now free of Imperial rule."
  }
}

# --------------------------- PROMPTING -------------------------------
build_prompt <- function(row, scenario, strict = FALSE) {
  stance <- stance_descriptor(row$Prob_t)
  world  <- scenario_world(scenario)
  faction_note <- if (row$Faction %in% c("Imperial", "Stormcloak")) {
    sprintf("Your hold sided with the %s.", row$Faction)
  } else {
    "Your hold stayed neutral."
  }

  rules <- paste0(
    "Rules:\n",
    "- You ARE this person, speaking aloud. Use 'I'. Never refer to yourself by name.\n",
    "- Do NOT describe the scene, the weather, or your actions. No narration.\n",
    "- Do NOT play the traveler or narrator. Only your own spoken words.\n",
    "- One or two sentences. Plain English only. No quotation marks.\n"
  )
  if (strict) {
    rules <- paste0(rules,
      "- Output ONLY the sentence you speak, nothing before or after it.\n")
  }

  paste0(
    "You are role-playing one character from The Elder Scrolls V: Skyrim.\n\n",
    "You are ", row$Name, ", a ", row$Race, " of ", row$Home_City, ".\n",
    "Your conviction: you are ", stance, ".\n",
    faction_note, "\n",
    "World state: ", world, "\n\n",
    "A traveler in a tavern asks what you make of how the civil war ended.\n\n",
    rules,
    "\nWrite in the terse, weathered voice of a Nord commoner or soldier. ",
    "Invent your own words drawn from your conviction above; do not copy any phrasing from these instructions.\n\n",
    "Now give your reply, beginning immediately with your own words:"
  )
}

# Remove DeepSeek-R1's chain-of-thought and tidy the spoken line.
strip_think <- function(txt, npc_name = "") {
  if (is.null(txt) || is.na(txt)) return(NA_character_)
  # drop everything up to and including the closing think tag
  out <- sub("(?s).*</think>", "", txt, perl = TRUE)
  # if an unterminated <think> remains (token budget hit), drop from it on
  out <- sub("(?s)<think>.*", "", out, perl = TRUE)
  out <- trimws(out)
  # drop a leading speaker label the model sometimes prepends ("Name: ...")
  if (nzchar(npc_name)) {
    out <- sub(paste0("^\\Q", npc_name, "\\E\\s*:\\s*"), "", out, perl = TRUE)
  }
  out <- sub("^[A-Z][a-zA-Z'’ -]{1,30}:\\s*", "", out)  # any residual label
  out <- gsub('^["“”\']+|["“”\']+$', "", out)            # strip wrapping quotes
  out <- gsub("\\s+", " ", out)
  trimws(out)
}

# Decide whether a cleaned line is acceptable in-character output.
# Returns "" if OK, else a short reason code for logging / retry.
line_problem <- function(line, npc_name) {
  if (is.null(line) || is.na(line) || nchar(line) < 8) return("empty")
  if (grepl("[^\\x01-\\x7F]", line, perl = TRUE)) return("non-english")
  bad <- c("traveler", "you say", "you ask", "as an ai", "i'm your",
           "stage direction", "the scene", "narrat")
  lc <- tolower(line)
  if (any(vapply(bad, function(b) grepl(b, lc, fixed = TRUE), logical(1)))) return("meta")
  # third-person self reference: NPC's own name used as subject
  if (nzchar(npc_name)) {
    first <- strsplit(npc_name, " ")[[1]][1]
    if (grepl(paste0("\\b", first, "\\b"), line) &&
        !grepl("\\bI\\b|\\bI'|\\bmy\\b|\\bme\\b", line)) return("third-person")
  }
  ""
}

# ----------------------------- SLM CALL ------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

# Single generation call to the local SLM.
call_slm_once <- function(prompt, seed) {
  body <- list(
    model  = CFG$model,
    prompt = prompt,
    stream = FALSE,
    options = list(
      temperature = CFG$temperature,
      num_predict = CFG$num_predict,
      seed        = seed
    )
  )
  resp <- tryCatch(
    request(CFG$ollama_url) |>
      req_body_json(body) |>
      req_timeout(CFG$timeout_s) |>
      req_perform(),
    error = function(e) e
  )
  if (inherits(resp, "error")) return(NA_character_)
  resp_body_json(resp)$response %||% NA_character_
}

# Generate, validate, and re-roll up to max_retries on bad output.
# Each retry bumps the seed and switches to the stricter prompt.
generate_line <- function(row, scenario) {
  for (attempt in 0:CFG$max_retries) {
    pr  <- build_prompt(row, scenario, strict = attempt > 0)
    raw <- call_slm_once(pr, seed = CFG$seed + attempt)
    line <- strip_think(raw, row$Name)
    prob <- line_problem(line, row$Name)
    if (!nzchar(prob)) return(list(line = line, attempts = attempt + 1L, status = "ok"))
  }
  list(line = line, attempts = CFG$max_retries + 1L, status = prob)
}

# Verify the model is present and actually generates before committing to a
# full multi-thousand-call run. Aborts loudly on a missing model / dead server.
preflight <- function() {
  probe <- call_slm_once("Reply with the single word: ready.", seed = CFG$seed)
  if (is.na(probe) || !nzchar(trimws(strip_think(probe)))) {
    stop("Preflight failed: model '", CFG$model, "' returned nothing. ",
         "Check that `ollama serve` is running and `ollama pull ", CFG$model,
         "` has completed (the server on ", CFG$ollama_url,
         " must list this exact model).")
  }
  cat("Preflight OK: model responding.\n")
}

# ------------------------------- MAIN --------------------------------
select_npcs <- function(d) {
  if (CFG$all_npcs) {
    sel <- copy(d)
  } else {
    sel <- if (CFG$key_only) d[Is_Key_NPC == TRUE] else d[0]
    if (CFG$sample_n > 0) {
      set.seed(CFG$seed)
      pool <- d[Is_Key_NPC == FALSE]
      sel <- rbind(sel, pool[sample(.N, min(CFG$sample_n, .N))])
    }
  }
  setorder(sel, -Is_Key_NPC, Name)
  sel
}

main <- function() {
  stopifnot(file.exists(CFG$input_csv))
  preflight()
  full <- fread(CFG$input_csv)

  all_out <- list()
  done <- 0L
  t0 <- Sys.time()
  flush <- function() fwrite(rbindlist(all_out), CFG$output_csv)

  # Resume support: reload any prior in-character lines and skip them, so an
  # interruption (e.g. the machine sleeping) costs nothing already generated.
  done_keys <- character(0)
  if (file.exists(CFG$output_csv)) {
    prev <- fread(CFG$output_csv)
    prev <- prev[Status == "ok" & nzchar(Dialogue)]   # re-roll prior failures
    if (nrow(prev) > 0) {
      all_out[[1]] <- prev
      done <- nrow(prev)
      done_keys <- paste(prev$Scenario, prev$Name, sep = "")
      cat(sprintf("Resuming: %d good lines already saved; skipping those.\n", done))
    }
  }

  total <- 0L
  for (scen in CFG$scenarios)
    total <- total + nrow(select_npcs(full[scenario == scen & run_id == CFG$run_id]))
  cat(sprintf("Voicing %d NPC-lines across %d scenario(s) | run=%d | model=%s\n",
              total, length(CFG$scenarios), CFG$run_id, CFG$model))

  for (scen in CFG$scenarios) {
    d <- full[scenario == scen & run_id == CFG$run_id]
    if (nrow(d) == 0) { cat("  (no rows for", scen, ")\n"); next }
    sel <- select_npcs(d)
    cat(sprintf("\n=== Scenario: %s  (%d NPCs) ===\n", scen, nrow(sel)))
    ok_s <- 0L
    for (i in seq_len(nrow(sel))) {
      row <- sel[i]
      if (paste(scen, row$Name, sep = "") %in% done_keys) next  # already done
      res <- generate_line(row, scen)
      if (res$status == "ok") ok_s <- ok_s + 1L
      all_out[[length(all_out) + 1L]] <- data.table(
        Scenario  = scen,
        Name      = row$Name,
        Race      = row$Race,
        Home_City = row$Home_City,
        Faction   = row$Faction,
        Is_Key    = row$Is_Key_NPC,
        Prob_t    = round(row$Prob_t, 3),
        Stance    = stance_descriptor(row$Prob_t),
        Dialogue  = res$line,
        Attempts  = res$attempts,
        Status    = res$status
      )
      done <- done + 1L
      if (done %% CFG$checkpoint_every == 0L) {
        flush()
        el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
        eta <- el / done * (total - done)
        cat(sprintf("  ...%d/%d done | %.1f min elapsed | ~%.1f min left | checkpointed\n",
                    done, total, el, eta))
      }
    }
    cat(sprintf("  %s complete: in-character %d/%d\n", scen, ok_s, nrow(sel)))
    flush()
  }

  flush()
  res_dt <- rbindlist(all_out)
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ok <- sum(res_dt$Status == "ok")
  cat(sprintf("\nDONE in %.1f min (%.1fs/NPC). In-character: %d/%d (%.1f%%). Wrote %s\n",
              dt / 60, dt / nrow(res_dt), ok, nrow(res_dt),
              100 * ok / nrow(res_dt), CFG$output_csv))
}

if (sys.nframe() == 0) main()
