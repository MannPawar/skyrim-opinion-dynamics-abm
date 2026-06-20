# LLM-as-Judge Rubric (dialogue consistency)

**Generator:** `llama3.2:3b` (local, via Ollama), conditioned on each NPC's
ABM-derived stance and a per-NPC lore knowledge base.
**Judge:** Claude (Opus 4.8), applying the rubric below to each generated line.
The judge sees only the NPC's identity, the *intended* stance (from the ABM),
the campaign outcome, and the generated line. It does not see the lore prompt,
so it scores the product, not the recipe.

The judge is deliberately critical: a 5 is reserved for lines that are both
politically correct for the stance and genuinely voice-authentic. Generic but
correct lines top out at 3 on voice.

## Axis 1 - Stance consistency (1-5)
Does the line express the political conviction the ABM assigned, *given who won
the war*? (Recall: low p = Imperial, high p = Stormcloak. A pro-Stormcloak NPC
in the Stormcloak-victory scenario should sound vindicated; the same NPC in the
Imperial-victory scenario should sound defeated or bitter.)

- **5** - clearly and specifically expresses the assigned stance and reacts
  correctly to the outcome.
- **4** - expresses the assigned stance; outcome reaction muted or generic.
- **3** - leans the right way but hedged or ambiguous.
- **2** - politically neutral / unreadable stance.
- **1** - expresses the *opposite* stance (a flip).

## Axis 2 - Voice quality (1-5)
Is it believable first-person NPC dialogue in a terse, weathered Nord register?

- **5** - vivid, in-character, specific; sounds like a real person.
- **4** - solid in-character line, slightly plain.
- **3** - grammatical and on-topic but generic ("filler NPC").
- **2** - awkward, repetitive, or restates the prompt.
- **1** - narration leak, meta ("the traveler asked"), third-person,
  non-English, or incoherent.

## Pass flag
`pass = TRUE` iff `stance >= 3 AND voice >= 3` (shippable as NPC dialogue).

## Reported metrics
- **Stance-consistency rate** = share with stance >= 4.
- **Flip rate** = share with stance == 1 (the failure that matters most).
- **Voice-pass rate** = share with voice >= 3.
- **Overall pass rate** = share with `pass == TRUE`.
- Mean stance and mean voice, with Wilson 95% CIs on the rates.
- Broken out by KB-augmented vs not, and by stance band.
