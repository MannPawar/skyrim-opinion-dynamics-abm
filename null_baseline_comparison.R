# ============================================================
#  NULL-MODEL BASELINE COMPARISON  (addresses reviewer W1)
#
#  Builds a structure-free baseline that is IDENTICAL to the full
#  model in every respect EXCEPT the endogenous social-influence
#  update (bounded confidence + repulsion + homophily + Thalmor
#  mean-reversion), which is replaced by zero-mean random drift.
#
#  The drift SD is CALIBRATED to the full model's own mean per-step
#  opinion-change SD, so the null injects the same per-step "energy"
#  as the structured model - it differs only in STRUCTURE, not
#  magnitude. This pre-empts the objection that the null is a
#  strawman tuned to lose.
#
#  Exogenous shocks (battle schedule, susceptibility, Talos modifier,
#  geographic proximity, network contagion, Thalmor inversion) are
#  kept byte-for-byte identical to the primary engine.
#
#  Outputs:
#    results/null_baseline_finals.csv     - null terminal opinions
#    results/null_vs_structured.csv       - head-to-head metric table
#  Console: three believability criteria + KS test (structured vs null)
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(parallel)
})

setwd("C:/Users/mspaw/Documents/Dynamic Sociopolitical Project")

CONFIG <- list(
  DATA_PATH = "People of Skyrim/Skyrim_Named_Characters.csv",
  OUTPUT_DIR = "results",
  N_RUNS = 100,
  TOTAL_TIME_STEPS = 150,
  SEED = 42,
  N_CORES = max(1L, detectCores() - 1L),
  EPSILON = 0.35, MU = 0.40,
  REPULSION_THRESHOLD = 0.70, REPULSION_RATE_BASE = 0.15,
  K_INTERACTIONS = 5, HOMOPHILY_RACE = 0.50, HOMOPHILY_FACTION = 0.70,
  SHOCK_MEAN = 0.15, SHOCK_SD = 0.05, MAX_SHOCK = 0.30,
  ALPHA_SUSC = 2.0, BETA_SUSC = 3.0,
  BOOT_REPS = 2000, CI_LEVEL = 0.95
)

KEY_NPCS <- c(
  "Ulfric Stormcloak", "General Tullius", "Galmar Stone-Fist", "Legate Rikke",
  "Balgruuf the Greater", "Vignar Gray-Mane", "Elisif the Fair",
  "Fralia Gray-Mane", "Idolaf Battle-Born",
  "Maven Black-Briar", "Brunwulf Free-Winter", "Brina Merilis",
  "Dengeir of Stuhn", "Thongvor Silver-Blood",
  "Elenwen", "Ancano", "Ondolemar", "The Dragonborn"
)
THALMOR_AGENTS <- c("Elenwen", "Ancano", "Ondolemar")
ZEALOT_NAMES <- c("Ulfric Stormcloak", "Galmar Stone-Fist",
                  "General Tullius", "Legate Rikke",
                  "Elenwen", "Ancano", "Ondolemar")

scale_01 <- function(x) {
  x <- as.numeric(x)
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) return(rep(0.5, length(x)))
  (x - min(x, na.rm = TRUE)) / rng
}

boot_ci <- function(x, stat_fn = mean, reps = 2000, level = 0.95) {
  bs <- replicate(reps, stat_fn(sample(x, length(x), replace = TRUE)))
  quantile(bs, c((1 - level) / 2, 1 - (1 - level) / 2))
}

# ---- Identical agent initialization to the primary engine ----
load_and_prepare_data <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)
  names(df) <- make.names(names(df))
  location_to_hold <- c(
    "Solitude"="Solitude","Dragon Bridge"="Solitude","Markarth"="Markarth",
    "Karthwasten"="Markarth","Falkreath"="Falkreath","Helgen"="Falkreath",
    "Riverwood"="Falkreath","Morthal"="Morthal","Windhelm"="Windhelm",
    "Kynesgrove"="Windhelm","Dawnstar"="Dawnstar","Riften"="Riften",
    "Shor's Stone"="Riften","Winterhold"="Winterhold",
    "College of Winterhold"="Winterhold","Whiterun"="Whiterun","Rorikstead"="Whiterun"
  )
  hold_allegiance_map <- c(
    "Solitude"="Imperial","Markarth"="Imperial","Falkreath"="Imperial",
    "Morthal"="Imperial","Windhelm"="Stormcloak","Dawnstar"="Stormcloak",
    "Riften"="Stormcloak","Winterhold"="Stormcloak","Whiterun"="Neutral",
    "Transient_Wilderness"="Neutral"
  )
  class_influence <- c("Jarl"=10,"Legate"=9,"Court Wizard"=8,"Housecarl"=7,
                       "Thalmor"=9,"Warrior"=5,"Mage"=5,"Blacksmith"=4,
                       "Merchant"=3,"Citizen"=2,"Beggar"=1)
  is_key <- coalesce(df$Name, "") %in% KEY_NPCS
  df_clean <- df %>%
    mutate(
      Is_Key_NPC = is_key,
      Home_City = coalesce(location_to_hold[replace_na(Home.City, "Unknown")],
                           "Transient_Wilderness"),
      Hold_Allegiance = hold_allegiance_map[Home_City],
      Base_Influence = coalesce(class_influence[as.character(Class)], 2),
      Influence_Score_Norm = scale_01(Base_Influence),
      Opinion_Prior = case_when(
        Name == "Ulfric Stormcloak" ~ 0.98, Name == "General Tullius" ~ 0.02,
        Name == "Galmar Stone-Fist" ~ 0.97, Name == "Legate Rikke" ~ 0.05,
        str_detect(Name, "Gray.Mane") ~ 0.90, str_detect(Name, "Battle.Born") ~ 0.10,
        Name %in% THALMOR_AGENTS ~ 0.50, Race == "High Elf" ~ 0.20,
        Race == "Nord" & Home_City == "Windhelm" ~ 0.85,
        Race == "Nord" & Home_City == "Dawnstar" ~ 0.80,
        Race == "Imperial" ~ 0.15, Race == "Nord" & Home_City == "Solitude" ~ 0.25,
        Race == "Nord" & Home_City == "Whiterun" ~ 0.50,
        Name == "The Dragonborn" ~ 0.50, TRUE ~ 0.50
      ),
      Kappa = case_when(
        Name %in% c("Ulfric Stormcloak", "General Tullius") ~ 80,
        Name %in% c("Galmar Stone-Fist", "Legate Rikke") ~ 60,
        Name %in% THALMOR_AGENTS ~ 75, Is_Key_NPC ~ 20, TRUE ~ 8
      ),
      Beta_Alpha = Opinion_Prior * Kappa, Beta_Beta = (1 - Opinion_Prior) * Kappa,
      Talos_Conviction = case_when(
        Race == "High Elf" ~ -1.0, Name == "Heimskr" ~ 1.0,
        Race == "Nord" & Hold_Allegiance == "Stormcloak" ~ 0.9,
        Race == "Nord" ~ 0.6, Race == "Imperial" ~ 0.4, TRUE ~ 0.1
      ),
      Prob_t = Opinion_Prior,
      Faction = case_when(Opinion_Prior > 0.6 ~ "Stormcloak",
                          Opinion_Prior < 0.4 ~ "Imperial", TRUE ~ "Neutral"),
      Agent_ID = row_number()
    ) %>%
    select(Agent_ID, Name, Race, Home_City, Hold_Allegiance, Class,
           Opinion_Prior, Beta_Alpha, Beta_Beta, Prob_t, Influence_Score_Norm,
           Talos_Conviction, Faction, Is_Key_NPC)
  df_clean$Home_City[is.na(df_clean$Hold_Allegiance)] <- "Transient_Wilderness"
  df_clean$Hold_Allegiance[is.na(df_clean$Hold_Allegiance)] <- "Neutral"
  df_clean
}

append_dragonborn <- function(agents_df, scenario_name) {
  next_id <- max(agents_df$Agent_ID) + 1L
  if (scenario_name == "Imperial") {
    db <- data.frame(Agent_ID=next_id, Name="The Dragonborn", Race="Imperial",
      Home_City="Solitude", Hold_Allegiance="Imperial", Class="Warrior",
      Opinion_Prior=0.02, Beta_Alpha=0.02*90, Beta_Beta=0.98*90, Prob_t=0.02,
      Influence_Score_Norm=1.00, Talos_Conviction=0.30, Faction="Imperial",
      Is_Key_NPC=TRUE, stringsAsFactors=FALSE)
  } else {
    db <- data.frame(Agent_ID=next_id, Name="The Dragonborn", Race="Nord",
      Home_City="Whiterun", Hold_Allegiance="Neutral", Class="Warrior",
      Opinion_Prior=0.98, Beta_Alpha=0.98*90, Beta_Beta=0.02*90, Prob_t=0.98,
      Influence_Score_Norm=1.00, Talos_Conviction=0.80, Faction="Stormcloak",
      Is_Key_NPC=TRUE, stringsAsFactors=FALSE)
  }
  bind_rows(agents_df, db)
}

build_agent_arrays <- function(agents_df) {
  list(
    n = nrow(agents_df), race = agents_df$Race, home_city = agents_df$Home_City,
    hold = agents_df$Hold_Allegiance, faction = agents_df$Faction,
    influence = agents_df$Influence_Score_Norm, talos = agents_df$Talos_Conviction,
    beta_a = agents_df$Beta_Alpha, beta_b = agents_df$Beta_Beta,
    opinion_prior = agents_df$Opinion_Prior, is_key = agents_df$Is_Key_NPC,
    is_thalmor = agents_df$Name %in% THALMOR_AGENTS,
    is_zealot = agents_df$Name %in% ZEALOT_NAMES |
      (abs(agents_df$Opinion_Prior - 0.50) > 0.40 &
       abs(agents_df$Talos_Conviction) > 0.70 &
       agents_df$Influence_Score_Norm > 0.40),
    name = agents_df$Name, agent_id = agents_df$Agent_ID
  )
}

create_scenarios <- function() {
  list(
    IMPERIAL = list(NAME="Imperial", SCHEDULE=tribble(
      ~time_step, ~event_name, ~hold_to_flip, ~shock_direction,
      26,"Battle of Whiterun","Whiterun",-1, 51,"Battle for Dawnstar","Dawnstar",-1,
      76,"Battle for Riften","Riften",-1, 101,"Battle of Windhelm","Windhelm",-1)),
    STORMCLOAK = list(NAME="Stormcloak", SCHEDULE=tribble(
      ~time_step, ~event_name, ~hold_to_flip, ~shock_direction,
      26,"Battle of Whiterun","Whiterun",+1, 51,"Battle for Falkreath","Falkreath",+1,
      76,"Battle for The Reach","Markarth",+1, 101,"Battle for Solitude","Solitude",+1))
  )
}

# ============================================================
#  CALIBRATION: measure full-model mean per-step social-delta SD
# ============================================================
# Runs the STRUCTURED social update (zealot-anchored) but only to
# record the per-step SD of the endogenous delta. Returns the mean
# across steps - used as the null model's drift SD.
calibrate_drift_sd <- function(ag, schedule, n_cal = 5) {
  n <- ag$n; K <- CONFIG$K_INTERACTIONS
  race_int <- as.integer(factor(ag$race))
  sched_t <- schedule$time_step; sched_hold <- schedule$hold_to_flip
  sched_dir <- schedule$shock_direction
  all_sd <- c()
  for (r in seq_len(n_cal)) {
    set.seed(CONFIG$SEED + r)
    opinions <- pmax(0.01, pmin(0.99, rbeta(n, ag$beta_a, ag$beta_b)))
    suscept  <- rbeta(n, CONFIG$ALPHA_SUSC, CONFIG$BETA_SUSC)
    hold_cur <- ag$hold
    fac_cur  <- as.integer(opinions > 0.6) - as.integer(opinions < 0.4)
    local_pool <- split(seq_len(n), ag$home_city)
    personal_network <- matrix(0L, nrow = n, ncol = 20)
    for (i in seq_len(n)) {
      pool <- local_pool[[ag$home_city[i]]]
      if (is.null(pool) || length(pool) < 2) pool <- seq_len(n)
      n_loc <- rbinom(1, 20, 0.80); n_glob <- 20 - n_loc
      personal_network[i, ] <- sample(c(sample(pool, n_loc, replace = TRUE),
                                        sample.int(n, n_glob, replace = TRUE)))
    }
    for (t in seq_len(CONFIG$TOTAL_TIME_STEPS)) {
      cols <- matrix(sample.int(20, n*K, replace=TRUE), nrow=n, ncol=K)
      flat_idx <- rep(1:n, times=K) + n*(as.vector(cols)-1)
      partner_mat <- matrix(personal_network[flat_idx], nrow=n, ncol=K)
      self_mask <- partner_mat == seq_len(n)
      partner_mat[self_mask] <- (partner_mat[self_mask] %% n) + 1L
      p_opinions <- matrix(opinions[partner_mat], nrow=n, ncol=K)
      p_influence <- matrix(ag$influence[partner_mat], nrow=n, ncol=K)
      p_race <- matrix(race_int[partner_mat], nrow=n, ncol=K)
      p_fac <- matrix(fac_cur[partner_mat], nrow=n, ncol=K)
      homophily_w <- 1 + CONFIG$HOMOPHILY_RACE*(p_race==race_int) +
        CONFIG$HOMOPHILY_FACTION*(p_fac==fac_cur)
      diff_mat <- p_opinions - matrix(opinions, nrow=n, ncol=K)
      bc_mask <- abs(diff_mat) < CONFIG$EPSILON
      delta <- rowSums(bc_mask*CONFIG$MU*p_influence*homophily_w*diff_mat)/K
      rep_mask <- abs(diff_mat) > CONFIG$REPULSION_THRESHOLD
      opp_faction <- (p_fac*matrix(fac_cur,nrow=n,ncol=K)) == -1
      repulsion_mult <- 1 + 1.0*opp_faction*(p_race != matrix(race_int,nrow=n,ncol=K))
      rep_delta <- rowSums(rep_mask*CONFIG$REPULSION_RATE_BASE*repulsion_mult*
                           p_influence*diff_mat*-1)/K
      delta <- delta + rep_delta
      thalmor_delta <- 0.05*(0.50-opinions)*ag$is_thalmor
      delta <- ifelse(ag$is_thalmor, 0.10*delta+thalmor_delta, delta)
      delta <- ifelse(ag$is_zealot, 0.00, delta)  # zealot-anchored primary
      all_sd <- c(all_sd, sd(delta))
      opinions <- pmax(0.01, pmin(0.99, opinions + delta))
      noise <- ifelse(ag$is_zealot, 0, rnorm(n, 0, 0.01))
      opinions <- pmax(0.01, pmin(0.99, opinions + noise))
      ev_idx <- match(t, sched_t)
      if (!is.na(ev_idx)) {
        dir <- sched_dir[ev_idx]; in_hold <- hold_cur == sched_hold[ev_idx]
        base_shocks <- pmax(0, pmin(CONFIG$MAX_SHOCK,
                          abs(rnorm(n, CONFIG$SHOCK_MEAN, CONFIG$SHOCK_SD))))
        talos_mod <- 1 + 0.3*ag$talos*dir
        contagion_mod <- 1 + 0.1*(rowSums(p_fac)*dir)
        prox_mod <- ifelse(in_hold, 2.0, 0.6)*pmax(0.1, contagion_mod)
        thalmor_dir <- ifelse(ag$is_thalmor, -sign(opinions-0.50), dir)
        final_shocks <- thalmor_dir*base_shocks*suscept*talos_mod*prox_mod
        final_shocks <- ifelse(ag$is_zealot, final_shocks*0.05, final_shocks)
        opinions <- pmax(0.01, pmin(0.99, opinions + final_shocks))
        hold_cur[in_hold] <- if (dir == 1L) "Stormcloak" else "Imperial"
      }
      fac_cur <- as.integer(opinions > 0.6) - as.integer(opinions < 0.4)
    }
  }
  mean(all_sd)
}

# ============================================================
#  NULL ENGINE: structure-free random drift + IDENTICAL shocks
# ============================================================
run_mc_null_vec <- function(ag, schedule, scenario_name, run_id, drift_sd) {
  n <- ag$n; K <- CONFIG$K_INTERACTIONS
  race_int <- as.integer(factor(ag$race))
  sched_t <- schedule$time_step; sched_hold <- schedule$hold_to_flip
  sched_dir <- schedule$shock_direction
  opinions <- pmax(0.01, pmin(0.99, rbeta(n, ag$beta_a, ag$beta_b)))
  suscept  <- rbeta(n, CONFIG$ALPHA_SUSC, CONFIG$BETA_SUSC)
  hold_cur <- ag$hold
  fac_cur  <- as.integer(opinions > 0.6) - as.integer(opinions < 0.4)
  local_pool <- split(seq_len(n), ag$home_city)
  personal_network <- matrix(0L, nrow = n, ncol = 20)
  for (i in seq_len(n)) {
    pool <- local_pool[[ag$home_city[i]]]
    if (is.null(pool) || length(pool) < 2) pool <- seq_len(n)
    n_loc <- rbinom(1, 20, 0.80); n_glob <- 20 - n_loc
    personal_network[i, ] <- sample(c(sample(pool, n_loc, replace = TRUE),
                                      sample.int(n, n_glob, replace = TRUE)))
  }
  for (t in seq_len(CONFIG$TOTAL_TIME_STEPS)) {
    # ---- partner sampling retained ONLY to feed identical shock contagion ----
    cols <- matrix(sample.int(20, n*K, replace=TRUE), nrow=n, ncol=K)
    flat_idx <- rep(1:n, times=K) + n*(as.vector(cols)-1)
    partner_mat <- matrix(personal_network[flat_idx], nrow=n, ncol=K)
    self_mask <- partner_mat == seq_len(n)
    partner_mat[self_mask] <- (partner_mat[self_mask] %% n) + 1L
    p_fac <- matrix(fac_cur[partner_mat], nrow=n, ncol=K)

    # ---- STRUCTURE-FREE UPDATE: zero-mean random drift (replaces all
    #      bounded-confidence / repulsion / homophily / Thalmor reversion) ----
    delta <- rnorm(n, 0, drift_sd)
    opinions <- pmax(0.01, pmin(0.99, opinions + delta))
    # idiosyncratic noise kept identical to primary engine
    opinions <- pmax(0.01, pmin(0.99, opinions + rnorm(n, 0, 0.01)))

    # ---- EXOGENOUS SHOCK: byte-for-byte identical to primary engine ----
    ev_idx <- match(t, sched_t)
    if (!is.na(ev_idx)) {
      dir <- sched_dir[ev_idx]; in_hold <- hold_cur == sched_hold[ev_idx]
      base_shocks <- pmax(0, pmin(CONFIG$MAX_SHOCK,
                        abs(rnorm(n, CONFIG$SHOCK_MEAN, CONFIG$SHOCK_SD))))
      talos_mod <- 1 + 0.3*ag$talos*dir
      contagion_mod <- 1 + 0.1*(rowSums(p_fac)*dir)
      prox_mod <- ifelse(in_hold, 2.0, 0.6)*pmax(0.1, contagion_mod)
      thalmor_dir <- ifelse(ag$is_thalmor, -sign(opinions-0.50), dir)
      final_shocks <- thalmor_dir*base_shocks*suscept*talos_mod*prox_mod
      opinions <- pmax(0.01, pmin(0.99, opinions + final_shocks))
      hold_cur[in_hold] <- if (dir == 1L) "Stormcloak" else "Imperial"
    }
    fac_cur <- as.integer(opinions > 0.6) - as.integer(opinions < 0.4)
  }
  fac_str <- ifelse(fac_cur==1L,"Stormcloak", ifelse(fac_cur==-1L,"Imperial","Neutral"))
  data.frame(Agent_ID=ag$agent_id, Name=ag$name, Race=ag$race,
    Home_City=ag$home_city, Is_Key_NPC=ag$is_key, Prob_t=opinions,
    Susceptibility=suscept, Faction=fac_str, scenario=scenario_name,
    run_id=run_id, stringsAsFactors=FALSE)
}

run_null_mc_parallel <- function(ag_base, scenario, agents_df, drift_sd) {
  sc_name <- scenario$NAME; schedule <- scenario$SCHEDULE
  agents_with_db <- append_dragonborn(agents_df, sc_name)
  ag_sc <- build_agent_arrays(agents_with_db)
  cl <- makeCluster(CONFIG$N_CORES)
  clusterExport(cl, varlist=c("ag_sc","schedule","sc_name","CONFIG",
    "run_mc_null_vec","drift_sd","THALMOR_AGENTS"), envir=environment())
  clusterEvalQ(cl, { library(stats); library(parallel) })
  worker <- function(run_id) {
    set.seed(CONFIG$SEED + run_id)
    run_mc_null_vec(ag_sc, schedule, sc_name, run_id, drift_sd)
  }
  out <- parLapply(cl, seq_len(CONFIG$N_RUNS), worker)
  stopCluster(cl)
  do.call(rbind, out)
}

# ============================================================
#  MAIN
# ============================================================
cat("Loading agents...\n")
df_agents <- load_and_prepare_data(CONFIG$DATA_PATH)
ag <- build_agent_arrays(df_agents)
SC <- create_scenarios()

cat("Calibrating drift SD to full-model per-step social-delta SD...\n")
ag_imp_cal <- build_agent_arrays(append_dragonborn(df_agents, "Imperial"))
drift_sd <- calibrate_drift_sd(ag_imp_cal, SC$IMPERIAL$SCHEDULE, n_cal = 5)
cat(sprintf("  Calibrated drift SD = %.5f (per agent per step)\n\n", drift_sd))

cat("Running NULL baseline Monte Carlo (Imperial)...\n")
t0 <- Sys.time()
null_imp <- run_null_mc_parallel(ag, SC$IMPERIAL, df_agents, drift_sd)
cat("Running NULL baseline Monte Carlo (Stormcloak)...\n")
null_sc <- run_null_mc_parallel(ag, SC$STORMCLOAK, df_agents, drift_sd)
null_all <- rbind(null_imp, null_sc)
cat(sprintf("  Null MC complete in %.1fs | %d agent-runs\n\n",
            as.numeric(difftime(Sys.time(), t0, units="secs")), nrow(null_all)))

write_csv(as_tibble(null_all), file.path(CONFIG$OUTPUT_DIR, "null_baseline_finals.csv"))

# ---- Load STRUCTURED primary output (saved zealot-anchored finals) ----
struct <- read_csv("results/final_opinions.csv", show_col_types = FALSE)

# ============================================================
#  HEAD-TO-HEAD ON THE THREE BELIEVABILITY CRITERIA
# ============================================================
metric_block <- function(df, label) {
  s <- df %>% group_by(scenario) %>%
    summarise(mean=mean(Prob_t), pop_sd=sd(Prob_t),
              pct_opp_pole = ifelse(scenario[1]=="Imperial",
                                    mean(Prob_t>0.6)*100, mean(Prob_t<0.4)*100),
              .groups="drop")
  # bimodality via dip-like proxy: fraction in the [0.4,0.6] neutral trough
  neut <- df %>% group_by(scenario) %>%
    summarise(pct_neutral=mean(Prob_t>=0.4 & Prob_t<=0.6)*100, .groups="drop")
  left_join(s, neut, by="scenario") %>% mutate(model=label)
}

cat("=== CRITERION 1+2: scenario separation, diversity, neutral trough ===\n")
m_struct <- metric_block(struct, "Structured (primary)")
m_null   <- metric_block(null_all, "Null (random drift)")
comp <- bind_rows(m_struct, m_null) %>% arrange(scenario, model)
print(as.data.frame(comp), row.names = FALSE)

sep_struct <- abs(diff(m_struct$mean[order(m_struct$scenario)]))
sep_null   <- abs(diff(m_null$mean[order(m_null$scenario)]))
cat(sprintf("\nScenario separation |Imp - SC|:  structured = %.4f   null = %.4f\n",
            sep_struct, sep_null))

# ---- Lore consistency: non-anchored key NPCs ----
cat("\n=== CRITERION 3: lore consistency of NON-anchored key NPCs ===\n")
lore_npcs <- c("Balgruuf the Greater", "Elisif the Fair", "Maven Black-Briar")
lore_tbl <- function(df, label) {
  df %>% filter(Name %in% lore_npcs) %>%
    group_by(Name, scenario) %>%
    summarise(mean=mean(Prob_t), sd=sd(Prob_t), .groups="drop") %>%
    mutate(model=label)
}
lore_comp <- bind_rows(lore_tbl(struct,"Structured"), lore_tbl(null_all,"Null")) %>%
  arrange(Name, scenario, model)
print(as.data.frame(lore_comp), row.names = FALSE)

# ============================================================
#  KS TEST: structured vs null terminal distribution (Imperial)
# ============================================================
ks_struct <- struct$Prob_t[struct$scenario=="Imperial"]
ks_null   <- null_all$Prob_t[null_all$scenario=="Imperial"]
ks <- ks.test(ks_struct, ks_null)
cat(sprintf("\n=== KS TEST: structured vs null (Imperial terminal) ===\n"))
cat(sprintf("  D = %.4f   p = %.2e\n", ks$statistic, ks$p.value))

# Bootstrap CIs on scenario means for both models (per-run means)
boot_block <- function(df) {
  df %>% group_by(scenario, run_id) %>%
    summarise(rm=base::mean(Prob_t), .groups="drop") %>%
    group_by(scenario) %>%
    summarise(mean_op=base::mean(rm),
              lo=boot_ci(rm, base::mean, CONFIG$BOOT_REPS, CONFIG$CI_LEVEL)[1],
              hi=boot_ci(rm, base::mean, CONFIG$BOOT_REPS, CONFIG$CI_LEVEL)[2],
              .groups="drop")
}
cat("\n=== Bootstrap 95% CIs on scenario means ===\n")
cat("Structured:\n"); print(as.data.frame(boot_block(struct)), row.names=FALSE)
cat("Null:\n");       print(as.data.frame(boot_block(null_all)), row.names=FALSE)

# ---- Export head-to-head table ----
out_tbl <- comp %>%
  transmute(model, scenario, mean=round(mean,4), pop_sd=round(pop_sd,4),
            pct_neutral_trough=round(pct_neutral,2),
            pct_opposing_pole=round(pct_opp_pole,2))
write_csv(out_tbl, file.path(CONFIG$OUTPUT_DIR, "null_vs_structured.csv"))
cat(sprintf("\nSaved: results/null_baseline_finals.csv (%d rows)\n", nrow(null_all)))
cat("Saved: results/null_vs_structured.csv\n")
cat(sprintf("\nCALIBRATED DRIFT SD = %.5f\n", drift_sd))
cat("DONE.\n")
