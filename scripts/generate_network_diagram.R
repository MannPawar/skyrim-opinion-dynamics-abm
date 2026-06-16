# Generates a paired network diagram (Slide 4) using ACTUAL saved simulation
# output rather than re-running. Loads final_opinions.csv from the Stormcloak
# scenario, picks one MC run, samples ~85 agents across all Holds, and plots
# the hold-clustered small-world network at t=0 (initial priors) and t=150
# (the run's actual final opinions).
#
# Output: results/plots/fig_network_t0_vs_t150.png

suppressPackageStartupMessages({
  library(tidyverse)
  library(igraph)
})

setwd("C:/Users/mspaw/Documents/Dynamic Sociopolitical Project")
set.seed(7)

# ---- Load full population CSV for hold/race metadata ----
pop <- read_csv("People of Skyrim/Skyrim_Named_Characters.csv", show_col_types = FALSE)
names(pop) <- make.names(names(pop))

location_to_hold <- c(
  "Solitude"="Solitude","Dragon Bridge"="Solitude",
  "Markarth"="Markarth","Karthwasten"="Markarth",
  "Falkreath"="Falkreath","Helgen"="Falkreath","Riverwood"="Falkreath",
  "Morthal"="Morthal",
  "Windhelm"="Windhelm","Kynesgrove"="Windhelm",
  "Dawnstar"="Dawnstar",
  "Riften"="Riften","Shor's Stone"="Riften",
  "Winterhold"="Winterhold","College of Winterhold"="Winterhold",
  "Whiterun"="Whiterun","Rorikstead"="Whiterun"
)
pop$Home_City <- coalesce(location_to_hold[replace_na(pop$Home.City, "Unknown")], "Wilderness")
pop <- pop %>% filter(Home_City != "Wilderness") %>% select(Name, Race, Class, Home_City)

# Initial-prior assignment matching the main sim
opinion_prior <- function(name, race, city){
  case_when(
    name == "Ulfric Stormcloak" ~ 0.98,
    name == "General Tullius"   ~ 0.02,
    name == "Galmar Stone-Fist" ~ 0.97,
    name == "Legate Rikke"      ~ 0.05,
    str_detect(name, "Gray.Mane")    ~ 0.90,
    str_detect(name, "Battle.Born")  ~ 0.10,
    name %in% c("Elenwen","Ancano","Ondolemar") ~ 0.50,
    race == "High Elf"          ~ 0.20,
    race == "Nord" & city == "Windhelm" ~ 0.85,
    race == "Nord" & city == "Dawnstar" ~ 0.80,
    race == "Imperial"          ~ 0.15,
    race == "Nord" & city == "Solitude" ~ 0.25,
    race == "Nord" & city == "Whiterun" ~ 0.50,
    TRUE                        ~ 0.50
  )
}
pop$Prior <- opinion_prior(pop$Name, pop$Race, pop$Home_City)

# ---- Load one Stormcloak run from saved output ----
fin <- read_csv("results/final_opinions.csv", show_col_types = FALSE)
sc_run1 <- fin %>%
  filter(scenario == "Stormcloak", run_id == 1) %>%
  select(Name, Prob_t)
cat(sprintf("Loaded %d agent finals from Stormcloak run 1\n", nrow(sc_run1)))

# ---- Sample 85 agents spread across all 9 holds, prioritizing key NPCs ----
key_names <- c("Ulfric Stormcloak","General Tullius","Galmar Stone-Fist","Legate Rikke",
               "Balgruuf the Greater","Vignar Gray-Mane","Elisif the Fair",
               "Maven Black-Briar","Brunwulf Free-Winter","Elenwen","Ancano","Ondolemar")

# Force key NPCs in, then fill quota per hold
forced <- pop %>% filter(Name %in% key_names)
remaining_slots <- 85 - nrow(forced)
per_hold_target <- ceiling(remaining_slots / 9)

filler <- pop %>%
  filter(!(Name %in% key_names)) %>%
  group_by(Home_City) %>%
  slice_sample(n = per_hold_target, replace = FALSE) %>%
  ungroup() %>%
  slice_head(n = remaining_slots)

sampled <- bind_rows(forced, filler) %>%
  distinct(Name, .keep_all = TRUE) %>%
  inner_join(sc_run1, by = "Name") %>%   # only keep agents present in run output
  arrange(Home_City)

n <- nrow(sampled)
cat(sprintf("Final sample: %d agents across %d holds\n", n, length(unique(sampled$Home_City))))
cat("Key NPCs in sample:\n")
print(sampled %>% filter(Name %in% key_names) %>% select(Name, Home_City, Prior, Prob_t))

# ---- Build hold-clustered small-world network ----
local_pool <- split(seq_len(n), sampled$Home_City)
edges <- list()
for (i in seq_len(n)) {
  pool <- setdiff(local_pool[[sampled$Home_City[i]]], i)
  if (length(pool) < 1) pool <- setdiff(seq_len(n), i)
  n_loc  <- rbinom(1, 6, 0.80)
  n_glob <- 6 - n_loc
  loc_samp  <- if (n_loc  > 0 && length(pool) > 0) sample(pool, min(n_loc, length(pool))) else integer(0)
  glob_samp <- if (n_glob > 0) sample(setdiff(seq_len(n), i), n_glob) else integer(0)
  partners <- unique(c(loc_samp, glob_samp))
  for (j in partners) edges[[length(edges)+1]] <- c(i, j)
}
edges_mat <- do.call(rbind, edges)
g <- graph_from_edgelist(edges_mat, directed = FALSE) %>% simplify()

# ---- Colors and sizes ----
faction_color <- function(op) {
  ifelse(op > 0.6, "#2980B9",       # Stormcloak blue
  ifelse(op < 0.4, "#C0392B",       # Imperial red
                   "#888888"))      # Neutral grey
}

influence_class <- c("Jarl"=10,"Legate"=9,"Court Wizard"=8,"Housecarl"=7,
                     "Thalmor"=9,"Warrior"=5,"Mage"=5,"Blacksmith"=4,
                     "Merchant"=3,"Citizen"=2,"Beggar"=1)
v_size_base <- coalesce(influence_class[as.character(sampled$Class)], 3)
v_size <- 4 + (v_size_base / max(v_size_base)) * 6
v_size[sampled$Name %in% key_names] <- v_size[sampled$Name %in% key_names] + 3

# ---- Layout (force-directed, hold-edge weighted to encourage clustering) ----
el <- as_edgelist(g)
same_hold <- sampled$Home_City[as.integer(el[,1])] == sampled$Home_City[as.integer(el[,2])]
E(g)$weight <- ifelse(same_hold, 3, 0.4)
set.seed(11)
lay <- layout_with_fr(g, weights = E(g)$weight, niter = 1500)

# ---- Plot ----
png("results/plots/fig_network_t0_vs_t150.png", width = 1700, height = 850, res = 150, bg = "white")
par(mfrow = c(1, 2), mar = c(2, 1, 3, 1), bg = "white")

plot(g, layout = lay,
     vertex.color = faction_color(sampled$Prior),
     vertex.size = v_size,
     vertex.label = NA,
     vertex.frame.color = "white",
     edge.color = "#cccccc88",
     edge.width = 0.6,
     main = "t = 0   |   Initial cultural priors (mixed across holds)")

plot(g, layout = lay,
     vertex.color = faction_color(sampled$Prob_t),
     vertex.size = v_size,
     vertex.label = NA,
     vertex.frame.color = "white",
     edge.color = "#cccccc88",
     edge.width = 0.6,
     main = "t = 150   |   After full Stormcloak campaign (clusters formed)")

par(xpd = NA)
legend("bottom", inset = c(0, -0.04),
       legend = c("Imperial-leaning (p<0.4)", "Neutral (0.4-0.6)", "Stormcloak-leaning (p>0.6)"),
       fill = c("#C0392B", "#888888", "#2980B9"),
       border = NA, bty = "n", horiz = TRUE, cex = 0.95, text.col = "#222222")

dev.off()
cat("\nSaved: results/plots/fig_network_t0_vs_t150.png\n")
cat(sprintf("  Initial: Imp=%d / Neu=%d / Sc=%d\n",
            sum(sampled$Prior < 0.4),
            sum(sampled$Prior >= 0.4 & sampled$Prior <= 0.6),
            sum(sampled$Prior > 0.6)))
cat(sprintf("  Final:   Imp=%d / Neu=%d / Sc=%d\n",
            sum(sampled$Prob_t < 0.4),
            sum(sampled$Prob_t >= 0.4 & sampled$Prob_t <= 0.6),
            sum(sampled$Prob_t > 0.6)))
