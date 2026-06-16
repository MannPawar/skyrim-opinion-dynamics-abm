# Generative Opinion Dynamics in Skyrim

A stochastic agent-based model (ABM) that evolves the political opinions of 1,010 named *Skyrim* NPCs through the Civil War, validated against an energy-matched null model, and coupled to a local small language model (SLM) that voices each character from its emergent opinion state.

NPCs in most RPGs hold political alignments that change only through hard-coded quest flags, producing a "narrative stasis" that breaks immersion. This project replaces that with a population whose ideological structure *emerges* from local social influence, and then turns that structure into in-character dialogue.

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

## The SLM dialogue layer

Once the ABM has produced a terminal opinion for each NPC, `slm_dialogue_layer.R` maps that opinion to a stance and conditions a local language model (`deepseek-r1:1.5b` via Ollama) to voice the character in context. The ABM stays the source of truth; the SLM is a generative front-end. 93% of generated lines pass an automated in-character validator. The full generated corpus is in `results/slm_dialogue_all.csv`.

---

## Repository layout

```
FINALCOMPARATIVECAMPAIGN.R     Main vectorized simulation engine (Monte Carlo, parallelized)
null_baseline_comparison.R     Energy-matched null model and structured-vs-null comparison
slm_dialogue_layer.R           ABM opinion -> prompt -> local SLM -> in-character dialogue
slm_report.R                   Renders the dialogue corpus into a shareable HTML report
scripts/                       Figure generators (null comparison, convergence, network)
data/                          Input roster: 1,009 named Skyrim characters
results/                       Summary statistics and the generated dialogue corpus
figures/                       Result figures produced by the pipeline
```

## Running it

Requires R (4.5+) with `data.table`, `parallel`, plus `httr2`/`jsonlite` for the SLM layer. The dialogue layer also needs a running [Ollama](https://ollama.com) server with the model pulled:

```bash
ollama pull deepseek-r1:1.5b
```

Then, in order:

```bash
Rscript FINALCOMPARATIVECAMPAIGN.R      # run the Monte Carlo campaigns
Rscript null_baseline_comparison.R      # run the null and compute the comparison
Rscript slm_dialogue_layer.R            # voice the NPCs from their opinions
Rscript slm_report.R                    # build the HTML dialogue report
```

Note: the scripts currently use absolute paths from the original workstation. Adjust the path constants at the top of each script to your local checkout before running.

## Selected references

Deffuant et al. (2000); Hegselmann and Krause (2002); Baumann et al. (2020); Acemoglu et al. (2013); Yildiz et al. (2013); Watts and Strogatz (1998).
