#!/usr/bin/env Rscript
# KDE overlay: structured vs energy-matched null terminal opinion distributions.
# Supports paper section 4.2 (Null Baseline Comparison).

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

proj <- "C:/Users/mspaw/Documents/Dynamic Sociopolitical Project"
struct_path <- file.path(proj, "results", "final_opinions.csv")
null_path   <- file.path(proj, "results", "null_baseline_finals.csv")
out_dir     <- file.path(proj, "results", "plots")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

struct <- fread(struct_path, select = c("Prob_t", "scenario"))
nullm  <- fread(null_path,   select = c("Prob_t", "scenario"))
struct[, model := "Structured"]
nullm[,  model := "Null (energy-matched)"]

dat <- rbind(struct, nullm)
dat[, model := factor(model, levels = c("Structured", "Null (energy-matched)"))]
dat[, scenario := factor(scenario)]

scenario_colors <- c("Imperial" = "#C0392B", "Stormcloak" = "#2980B9")

p <- ggplot(dat, aes(x = Prob_t, colour = scenario, linetype = model)) +
  geom_density(linewidth = 0.9, adjust = 1.1) +
  scale_colour_manual(values = scenario_colors, name = "Scenario") +
  scale_linetype_manual(values = c("Structured" = "solid",
                                   "Null (energy-matched)" = "dashed"),
                        name = "Model") +
  labs(
    x = "Terminal opinion (P(support Empire))",
    y = "Density",
    title = "Terminal opinion distributions: structured vs. energy-matched null",
    subtitle = "Structured dynamics produce sharp bimodal consensus; the null collapses toward the neutral zone"
  ) +
  coord_cartesian(xlim = c(0, 1)) +
  theme_bw(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey30")
  )

out_path <- file.path(out_dir, "fig_null_comparison.png")
ggsave(out_path, p, width = 8, height = 4.5, dpi = 200, bg = "white")
cat("Wrote:", out_path, "\n")
