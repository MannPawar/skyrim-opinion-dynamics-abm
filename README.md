# Generative Opinion Dynamics in Skyrim

A stochastic agent-based model (ABM) that evolves the political opinions of 1,010 named *Skyrim* NPCs through the Civil War, validated against an energy-matched null model, and turned into in-character dialogue by a three-layer generative pipeline.

NPCs in most RPGs hold political alignments that change only through hard-coded quest flags, producing a "narrative stasis" that breaks immersion. This project replaces that with a population whose ideological structure *emerges* from local social influence, then voices each character from that emergent state.

The dialogue stack has three layers:

1. **Cognitive substrate (ABM):** the simulation produces each NPC's terminal opinion `p`. This is the source of truth for what the character believes.
2. **Identity (lore knowledge base):** a per-NPC knowledge base scraped from the UESP wiki supplies each character's occupation, relationships, and a sample of their own canonical in-game lines, to ground identity and voice.
3. **Realization (small language model):** a local model (`llama3.2:3b` via Ollama) turns opinion plus lore into a spoken line. An independent LLM judge then scores the result for consistency.

---

## What the model does

Each of the 1,010 agents holds a continuous opinion `p` (the probability they support the Empire; `p -> 0` Imperial, `p -> 1` Stormcloak). Opinions evolve over 150 timesteps under four coupled mechanisms:

- **Bounded-confidence assimilation** (Deffuant 2000; Hegselmann and Krause 2002): agents move toward peers whose opinions are close enough.
- **Out-group repulsion** (Baumann 2020): agents move away from peers who are too far, producing polarization and echo chambers.
- **Zealot anchoring** (Acemoglu 2013): narrative-essential characters (Ulfric, Tullius, Galmar, etc.) hold fixed positions.
- **Geographically-coupled shocks**: four Civil War battles inject opinion shifts amplified for agents living in the contested hold and modulated by their local network neighborhood.

Agents interact over a **hold-clustered small-world network** (Watts and Strogatz 1998), and initial opinions are drawn from lore-derived Beta priors.

## How it is validated

The central claim is that the *social machinery*, not the battle events or mere randomness, produces the believable structure. To show this, the model is compared against an **energy-matched random-drift null**: identical priors, network, and battle shocks, but every social-influence rule replaced by random motion calibrated to the structured model's own per-step change magnitude. The null therefore injects the same "energy", it differs only in *structure*.

Headline result (structured vs null):

| Metric | Structured | Null |
|---|---|---|
| Scenario separation | **0.41** | 0.24 |
| Neutral-zone occupancy | **< 1%** | 21% |
| Terminal population SD | 0.12 to 0.17 | 0.25 to 0.26 |

A two-sample Kolmogorov-Smirnov test separates the two terminal distributions at **D = 0.37**, a larger effect than the zealot mechanism itself induces. The structured dynamics produce sharp bimodal echo chambers; the null collapses toward a diffuse neutral blob. See `figures/fig_null_comparison.png`.

## The dialogue layer

Once the ABM has produced a terminal opinion for each NPC, `slm_dialogue_layer.R` maps that opinion to a stance, looks up the NPC's lore in the knowledge base (`data/npc_lore_kb.csv`, built by `build_lore_kb.py`), and conditions `llama3.2:3b` to voice the character in context. The lore description anchors identity and the canonical lines calibrate dialect, with a guard that rejects verbatim parroting of those lines. The ABM stays the source of truth; the lore KB grounds identity; the SLM realizes the dialogue.

The full corpus (1,010 NPCs across both campaign outcomes, 2,020 lines) is in `results/slm_dialogue_kb.csv`, with a curated, browser-openable showcase in `results/slm_dialogue_report.html`. 96.8% of lines pass an automated in-character validator.

### Why a knowledge base and not RAG

Every NPC is known at inference time, so this is a deterministic entity lookup, not semantic retrieval. RAG would add an embedding dependency and retrieval error for no benefit when the relevant entity is never in doubt. The KB ships as a flat CSV keyed by name.

## Evaluating the dialogue (LLM-as-judge)

`llama3.2:3b` generates; a stronger independent model (Claude) judges. Each line is scored against the ABM-assigned stance and the campaign outcome on two axes (political stance consistency and voice quality); the rubric is in `JUDGE_RUBRIC.md`. On a 120-line random sample of the corpus:

| Metric | Rate | 95% CI (Wilson) |
|---|---|---|
| Stance-consistent (>= 4 of 5) | **85.0%** | [77.5, 90.3] |
| Stance flip (opposite side) | **0.0%** | [0.0, 3.1] |
| Voice pass (>= 3 of 5) | **98.3%** | [94.1, 99.5] |
| Overall shippable | **97.5%** | [92.9, 99.1] |

Honest caveats: a single judge, scored non-blind to the intended stance, on a self-consistent loop (the same stance label drives both generation and judging). The judge inputs and scores are in `results/judge_inputs_full.csv` and `results/judge_scores_full.csv`; `Rscript judge_report.R` regenerates the metrics and `figures/fig_judge_consistency_full.png`.

---

## Repository layout

```
FINALCOMPARATIVECAMPAIGN.R     Main vectorized simulation engine (Monte Carlo, parallelized)
null_baseline_comparison.R     Energy-matched null model and structured-vs-null comparison
build_lore_kb.py               Builds the per-NPC lore knowledge base from the UESP wiki
slm_dialogue_layer.R           ABM opinion + lore -> prompt -> local SLM -> in-character dialogue
slm_report.R                   Renders the dialogue corpus into a shareable HTML showcase
gen_eval_sample.R              Draws a stratified sample for evaluation
judge_report.R                 Aggregates the LLM judge's scores into metrics and a figure
JUDGE_RUBRIC.md                The scoring rubric the judge applies
scripts/                       Figure generators (null comparison, convergence, network)
data/                          Input roster, plus the generated lore KB (npc_lore_kb.csv)
results/                       Summary stats, the dialogue corpus, the showcase, judge artifacts
figures/                       Result figures, including the judge-consistency chart
```

## Running it

Requires R (4.5+) with `data.table`, `parallel`, plus `httr2`/`jsonlite` for the dialogue layer, and Python 3 for the lore KB builder. The dialogue layer needs a running [Ollama](https://ollama.com) server with the model pulled:

```bash
ollama pull llama3.2:3b
```

Then, in order:

```bash
Rscript FINALCOMPARATIVECAMPAIGN.R      # run the Monte Carlo campaigns
Rscript null_baseline_comparison.R      # run the null and compute the comparison
python  build_lore_kb.py                # build the per-NPC lore knowledge base (one-time)
Rscript slm_dialogue_layer.R            # voice the NPCs from their opinions + lore
Rscript slm_report.R                    # build the HTML dialogue showcase
Rscript judge_report.R                  # aggregate LLM-judge scores into metrics + figure
```

The judging step itself is performed by a separate, stronger model applying `JUDGE_RUBRIC.md`; the scored sample is committed so `judge_report.R` reproduces the published numbers without re-judging.

Note: the scripts currently use absolute paths from the original workstation. Adjust the path constants at the top of each script to your local checkout before running. `build_lore_kb.py` is polite to the wiki by default (single-threaded, rate-limited, cached) and the generated `data/npc_lore_kb.csv` is committed so you can skip re-scraping.

## Selected references

Deffuant et al. (2000); Hegselmann and Krause (2002); Baumann et al. (2020); Acemoglu et al. (2013); Yildiz et al. (2013); Watts and Strogatz (1998).
