#!/usr/bin/env Rscript
# =====================================================================
#  judge_report.R  --  aggregate the LLM judge's scores into metrics
# ---------------------------------------------------------------------
#  Inputs : results/slm_dialogue_sample.csv  (generator output)
#           results/judge_scores.csv         (Claude's per-line scores)
#  Output : results/judge_summary.csv, figures/fig_judge_consistency.png
#
#  judge_scores.csv columns: Scenario, Name, stance (1-5), voice (1-5),
#  pass (TRUE/FALSE), reason. Joined to the sample by (Scenario, Name).
# =====================================================================
suppressPackageStartupMessages({ library(data.table) })
has_ggplot <- requireNamespace("ggplot2", quietly = TRUE)

# Optional args: samp_csv score_csv summary_csv fig_png subtitle
a <- commandArgs(trailingOnly = TRUE)
samp_csv    <- if (length(a) >= 1) a[1] else "results/slm_dialogue_sample.csv"
score_csv   <- if (length(a) >= 2) a[2] else "results/judge_scores.csv"
summary_csv <- if (length(a) >= 3) a[3] else "results/judge_summary.csv"
fig_png     <- if (length(a) >= 4) a[4] else "figures/fig_judge_consistency.png"
sub_txt     <- if (length(a) >= 5) a[5] else "KB-grounded"

samp  <- fread(samp_csv)
score <- fread(score_csv)
# Join on Home_City too when the score file carries it, to disambiguate NPCs
# that share a name within one scenario (e.g. two Imperial "Karita").
join_by <- intersect(c("Scenario", "Name", "Home_City"), names(score))
d <- merge(samp, score, by = join_by, all.x = TRUE)

# Wilson 95% CI for a proportion.
wilson <- function(k, n, z = 1.96) {
  if (n == 0) return(c(NA, NA))
  p <- k / n; d <- 1 + z^2 / n
  c <- (p + z^2 / (2 * n)) / d
  h <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d
  c(max(0, c - h), min(1, c + h))
}

rate_row <- function(lbl, k, n) {
  ci <- wilson(k, n)
  data.table(metric = lbl, k = k, n = n, rate = round(k / n, 3),
             lo = round(ci[1], 3), hi = round(ci[2], 3))
}

n <- nrow(d)
summary <- rbindlist(list(
  rate_row("stance_consistent (>=4)", sum(d$stance >= 4, na.rm = TRUE), n),
  rate_row("flip (stance==1)",        sum(d$stance == 1, na.rm = TRUE), n),
  rate_row("voice_pass (>=3)",        sum(d$voice  >= 3, na.rm = TRUE), n),
  rate_row("overall_pass",            sum(d$pass == TRUE, na.rm = TRUE), n)
))
summary[, mean_stance := round(mean(d$stance, na.rm = TRUE), 2)]
summary[, mean_voice  := round(mean(d$voice,  na.rm = TRUE), 2)]
fwrite(summary, summary_csv)

cat(sprintf("\nJudged %d lines (generator: llama3.2:3b, judge: Claude)\n", n))
print(summary[, .(metric, rate, lo, hi)])
cat(sprintf("\nMean stance %.2f / 5   Mean voice %.2f / 5\n",
            summary$mean_stance[1], summary$mean_voice[1]))

# KB vs non-KB split (only informative if both present)
if (length(unique(d$KB_Augmented)) > 1) {
  cat("\nBy KB augmentation (overall pass rate):\n")
  print(d[, .(pass_rate = round(mean(pass == TRUE, na.rm = TRUE), 3),
              mean_voice = round(mean(voice, na.rm = TRUE), 2), n = .N),
          by = KB_Augmented])
}

if (has_ggplot) {
  library(ggplot2)
  plotd <- summary[metric != "flip (stance==1)"]
  plotd[, metric := factor(metric, levels = rev(metric))]
  p <- ggplot(plotd, aes(rate, metric)) +
    geom_col(fill = "#c9a227", width = 0.6) +
    geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y",
                  width = 0.2, colour = "#e8e8e8") +
    geom_text(aes(x = hi, label = sprintf("%.1f%%", 100 * rate)),
              hjust = -0.25, colour = "#f0f0f0", size = 4.2) +
    scale_x_continuous(limits = c(0, 1.18), labels = scales::percent) +
    labs(title = "Dialogue consistency: Claude judging llama3.2:3b",
         subtitle = sprintf("%d NPC lines, %s | 95%% Wilson CI", n, sub_txt),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(plot.background = element_rect(fill = "#0e0e12", colour = NA),
          panel.background = element_rect(fill = "#0e0e12", colour = NA),
          panel.grid.major.y = element_blank(),
          panel.grid = element_line(colour = "#23232b"),
          text = element_text(colour = "#e8e8e8"),
          axis.text = element_text(colour = "#cfcfcf"))
  dir.create(dirname(fig_png), showWarnings = FALSE, recursive = TRUE)
  ggsave(fig_png, p, width = 8, height = 4.2, dpi = 150, bg = "#0e0e12")
  cat("Wrote", fig_png, "\n")
}
