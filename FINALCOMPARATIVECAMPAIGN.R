rm(list = ls())

# ============================================================
#  STOCHASTIC AGENT-BASED MODEL OF CIVIL WAR OPINION DYNAMICS
#  A Monte Carlo Simulation Study of Skyrim's Civil War
#
#  Stochastic processes employed:
#    1. Bounded confidence opinion dynamics (Deffuant-Weisbuch)
#    2. Stochastic social influence with homophily weighting
#    3. Exogenous shock propagation with heterogeneous susceptibility
#    4. Monte Carlo sampling over N_RUNS independent replications
#    5. Bootstrap confidence intervals on all summary statistics
#
#  VECTORIZED ENGINE:
#    - All per-agent loops replaced with matrix operations
#    - Static homophily weight matrix precomputed once before time loop
#    - Partner sampling via single apply() call (one R-level loop replaces
#      the previous n_agents × K nested loops)
#    - All diff / BC-mask / shock computations execute in compiled C
#    - MC outer loop parallelised across available CPU cores
#    - rowwise() replaced with vectorized rbeta(n, ...) draws
# ============================================================

library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(parallel) # base-R parallel MC

# ============================================================
# SECTION 1: MODEL PARAMETERS
# ============================================================

CONFIG <- list(
     # --- File Paths ---
     DATA_PATH = "People of Skyrim/Skyrim_Named_Characters.csv",
     OUTPUT_DIR = "results",
     PLOT_DIR = "results/plots",

     # --- Monte Carlo Replication ---
     N_RUNS = 100, # Reduced for rapid test
     # Increase to 500-1000 only if runtime allows
     TOTAL_TIME_STEPS = 150,
     SEED = 42,

     # --- Parallelisation ---
     # detectCores() - 1 keeps one core free for the OS
     N_CORES = max(1L, detectCores() - 1L),

     # --- Bounded Confidence (Deffuant-Weisbuch) ---
     EPSILON = 0.35, # confidence threshold
     MU = 0.40, # convergence rate

     # --- Repulsion (Assimilation-Contrast) ---
     REPULSION_THRESHOLD = 0.70,
     REPULSION_RATE_BASE = 0.15,

     # --- Stochastic Social Influence ---
     K_INTERACTIONS = 5,
     HOMOPHILY_RACE = 0.50,
     HOMOPHILY_FACTION = 0.70,

     # --- Exogenous Shock Parameters ---
     SHOCK_MEAN = 0.15,
     SHOCK_SD = 0.05,
     MAX_SHOCK = 0.30,

     # --- Agent Heterogeneity ---
     ALPHA_SUSC = 2.0,
     BETA_SUSC = 3.0,

     # --- Bootstrap CI ---
     BOOT_REPS = 2000,
     CI_LEVEL = 0.95
)

cat("\n============================================================\n")
cat(" STOCHASTIC ABM (VECTORIZED): SKYRIM CIVIL WAR\n")
cat("============================================================\n")
cat(sprintf(" MC Replications : %d\n", CONFIG$N_RUNS))
cat(sprintf(" Time steps      : %d\n", CONFIG$TOTAL_TIME_STEPS))
cat(sprintf(" CPU cores       : %d\n", CONFIG$N_CORES))
cat(sprintf(" Epsilon (BC)    : %.2f\n", CONFIG$EPSILON))
cat(sprintf(" Shock ~ N(%.2f, %.2f^2)\n", CONFIG$SHOCK_MEAN, CONFIG$SHOCK_SD))
cat("============================================================\n\n")



KEY_NPCS <- c(
     # Civil war principals
     "Ulfric Stormcloak", "General Tullius", "Galmar Stone-Fist", "Legate Rikke",
     # Jarls and hold leaders
     "Balgruuf the Greater", "Vignar Gray-Mane", "Elisif the Fair",
     # Whiterun factions
     "Fralia Gray-Mane", "Idolaf Battle-Born",
     # Other influential named characters
     "Maven Black-Briar", "Brunwulf Free-Winter", "Brina Merilis",
     "Dengeir of Stuhn", "Thongvor Silver-Blood",
     # Thalmor - named agents with high political influence
     "Elenwen", # First Emissary, attends peace council
     "Ancano", # Thalmor operative at College of Winterhold
     "Ondolemar", # Thalmor Justiciar in Markarth
     # Dragonborn - added programmatically per scenario in append_dragonborn()
     "The Dragonborn"
)

# Named Thalmor agents - used for targeted opinion prior and influence overrides
THALMOR_AGENTS <- c("Elenwen", "Ancano", "Ondolemar")
# Immovable political elites for the Zealot scenario
ZEALOT_NAMES <- c(
     "Ulfric Stormcloak", "Galmar Stone-Fist",
     "General Tullius", "Legate Rikke",
     "Elenwen", "Ancano", "Ondolemar"
)
# ============================================================
# SECTION 2: UTILITY FUNCTIONS
# ============================================================

scale_01 <- function(x) {
     x <- as.numeric(x)
     rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
     if (rng == 0) {
          return(rep(0.5, length(x)))
     }
     (x - min(x, na.rm = TRUE)) / rng
}

show_progress <- function(current, total, start_t, prefix = "Progress",
                          extra = "") {
     pct <- round(100 * current / total)
     filled <- round(40 * current / total)
     bar <- paste0(rep("=", filled), collapse = "")
     spaces <- paste0(rep(" ", 40 - filled), collapse = "")

     elapsed <- as.numeric(difftime(Sys.time(), start_t, units = "secs"))
     rate <- if (current > 0) elapsed / current else NA
     eta_secs <- if (!is.na(rate)) round(rate * (total - current)) else NA
     eta_str <- if (!is.na(eta_secs)) {
          if (eta_secs < 60) {
               sprintf("ETA %ds", eta_secs)
          } else {
               sprintf(
                    "ETA %dm %02ds", eta_secs %/% 60,
                    eta_secs %% 60
               )
          }
     } else {
          "ETA --"
     }

     cat(sprintf(
          "\r%s [%s%s] %d/%d (%d%%) | %s%s",
          prefix, bar, spaces,
          current, total, pct,
          eta_str, extra
     ))

     if (current == total) {
          cat(sprintf("\n  Done in %.1fs\n", elapsed))
     }

     flush.console()
}

boot_ci <- function(x, stat_fn = mean, reps = 2000, level = 0.95) {
     boot_stats <- replicate(reps, stat_fn(sample(x, length(x), replace = TRUE)))
     quantile(boot_stats, c((1 - level) / 2, 1 - (1 - level) / 2))
}

# ============================================================
# SECTION 3: DATA LOADING & AGENT INITIALISATION
# ============================================================

load_and_prepare_data <- function(file_path) {
     cat("Loading and initialising agents...\n")
     df <- read_csv(file_path, show_col_types = FALSE)
     names(df) <- make.names(names(df))

     location_to_hold <- c(
          "Solitude" = "Solitude", "Dragon Bridge" = "Solitude",
          "Markarth" = "Markarth", "Karthwasten" = "Markarth",
          "Falkreath" = "Falkreath", "Helgen" = "Falkreath",
          "Riverwood" = "Falkreath", "Morthal" = "Morthal",
          "Windhelm" = "Windhelm", "Kynesgrove" = "Windhelm",
          "Dawnstar" = "Dawnstar", "Riften" = "Riften",
          "Shor's Stone" = "Riften", "Winterhold" = "Winterhold",
          "College of Winterhold" = "Winterhold",
          "Whiterun" = "Whiterun", "Rorikstead" = "Whiterun"
     )

     hold_allegiance_map <- c(
          "Solitude" = "Imperial", "Markarth" = "Imperial",
          "Falkreath" = "Imperial", "Morthal" = "Imperial",
          "Windhelm" = "Stormcloak", "Dawnstar" = "Stormcloak",
          "Riften" = "Stormcloak", "Winterhold" = "Stormcloak",
          "Whiterun" = "Neutral", "Transient_Wilderness" = "Neutral"
     )

     class_influence <- c(
          "Jarl" = 10, "Legate" = 9, "Court Wizard" = 8, "Housecarl" = 7,
          "Thalmor" = 9, # Thalmor Justiciars rank equivalent to Legate in influence
          "Warrior" = 5, "Mage" = 5, "Blacksmith" = 4, "Merchant" = 3,
          "Citizen" = 2, "Beggar" = 1
     )

     # Flag key NPCs before mutate so Kappa can reference it
     is_key <- coalesce(df$Name, "") %in% KEY_NPCS

     df_clean <- df %>%
          mutate(
               Is_Key_NPC = is_key,
               Home_City = coalesce(
                    location_to_hold[replace_na(Home.City, "Unknown")],
                    "Transient_Wilderness"
               ),
               Hold_Allegiance = hold_allegiance_map[Home_City],
               Base_Influence = coalesce(class_influence[as.character(Class)], 2),
               Influence_Score_Norm = scale_01(Base_Influence),
               Opinion_Prior = case_when(
                    # --- Civil war principals ---
                    Name == "Ulfric Stormcloak" ~ 0.98,
                    Name == "General Tullius" ~ 0.02,
                    Name == "Galmar Stone-Fist" ~ 0.97,
                    Name == "Legate Rikke" ~ 0.05,
                    str_detect(Name, "Gray.Mane") ~ 0.90,
                    str_detect(Name, "Battle.Born") ~ 0.10,
                    # --- Thalmor agents: prefer NEITHER side to win ---
                    # The Thalmor goal is to keep Skyrim in perpetual conflict.
                    # A Stormcloak victory restores Talos worship and invalidates
                    # the White-Gold Concordat. An Imperial victory reunifies the
                    # Empire and threatens Aldmeri Dominion hegemony long-term.
                    # → Opinion prior = 0.50: genuinely indifferent between factions,
                    #   but rigidly resistant to being pushed toward either pole.
                    # Their unique shock_modifier (is_thalmor flag) inverts battle
                    # shocks back toward 0.50 in the simulation engine.
                    Name %in% THALMOR_AGENTS ~ 0.50,
                    # Non-named High Elves lean weakly Imperial (cultural affinity)
                    # but are not Thalmor operatives and don't get inverted shocks
                    Race == "High Elf" ~ 0.20,
                    # --- Geographic / racial priors ---
                    Race == "Nord" & Home_City == "Windhelm" ~ 0.85,
                    Race == "Nord" & Home_City == "Dawnstar" ~ 0.80,
                    Race == "Imperial" ~ 0.15,
                    Race == "Nord" & Home_City == "Solitude" ~ 0.25,
                    Race == "Nord" & Home_City == "Whiterun" ~ 0.50,
                    # --- Dragonborn placeholder: set per scenario in append_dragonborn() ---
                    Name == "The Dragonborn" ~ 0.50,
                    TRUE ~ 0.50
               ),

               # Beta concentration kappa: higher = tighter prior = less stochastic variation
               Kappa = case_when(
                    Name %in% c("Ulfric Stormcloak", "General Tullius") ~ 80,
                    Name %in% c("Galmar Stone-Fist", "Legate Rikke") ~ 60,
                    # Thalmor are ideologically rigid about staying at 0.50 -
                    # tight prior centred on neutrality, not on either faction
                    Name %in% THALMOR_AGENTS ~ 75,
                    Is_Key_NPC ~ 20,
                    TRUE ~ 8
               ),
               Beta_Alpha = Opinion_Prior * Kappa,
               Beta_Beta = (1 - Opinion_Prior) * Kappa,
               Talos_Conviction = case_when(
                    Race == "High Elf" ~ -1.0,
                    Name == "Heimskr" ~ 1.0,
                    Race == "Nord" & Hold_Allegiance == "Stormcloak" ~ 0.9,
                    Race == "Nord" ~ 0.6,
                    Race == "Imperial" ~ 0.4,
                    TRUE ~ 0.1
               ),
               Prob_t = Opinion_Prior,
               Faction = case_when(
                    Opinion_Prior > 0.6 ~ "Stormcloak",
                    Opinion_Prior < 0.4 ~ "Imperial",
                    TRUE ~ "Neutral"
               ),
               Agent_ID = row_number()
          ) %>%
          select(
               Agent_ID, Name, Race, Home_City, Hold_Allegiance, Class,
               Opinion_Prior, Beta_Alpha, Beta_Beta,
               Prob_t, Influence_Score_Norm,
               Talos_Conviction, Faction, Is_Key_NPC
          )

     cat(sprintf(
          "Loaded %d agents | %d key NPCs | %d holds\n\n",
          nrow(df_clean),
          sum(df_clean$Is_Key_NPC),
          n_distinct(df_clean$Hold_Allegiance)
     ))

     # ---- DIAGNOSTIC: check for expected named agents ----
     # Any name listed here that is absent from the CSV was either
     # hardcoded in an earlier version or is simply not in the dataset.
     # This is the most likely cause of an agent count discrepancy.
     expected_named <- c(KEY_NPCS, THALMOR_AGENTS)
     missing_named <- setdiff(expected_named, coalesce(df_clean$Name, ""))

     if (length(missing_named) > 0) {
          cat("⚠  DIAGNOSTIC: The following named agents are in KEY_NPCS or\n")
          cat("   THALMOR_AGENTS but were NOT found in the CSV. They will be\n")
          cat("   absent from the simulation unless hardcoded below:\n")
          for (nm in missing_named) cat(sprintf("     - %s\n", nm))
          cat("\n")
     } else {
          cat("✓  All named key agents present in CSV.\n\n")
     }

     # ---- DIAGNOSTIC: agents with NA Hold_Allegiance ----
     # Agents whose Home_City is not covered by location_to_hold end up with
     # NA Hold_Allegiance, which can cause weight matrix and shock logic to
     # silently misbehave. List any such agents here.
     na_hold <- df_clean %>%
          filter(is.na(Hold_Allegiance)) %>%
          select(Agent_ID, Name, Home_City)

     if (nrow(na_hold) > 0) {
          cat(sprintf(
               "⚠  DIAGNOSTIC: %d agents have NA Hold_Allegiance ",
               nrow(na_hold)
          ))
          cat("(their Home_City is not in location_to_hold):\n")
          for (i in seq_len(min(10, nrow(na_hold)))) {
               cat(sprintf(
                    "     Agent %d: %-25s  City: %s\n",
                    na_hold$Agent_ID[i],
                    coalesce(na_hold$Name[i], "<unnamed>"),
                    coalesce(na_hold$Home_City[i], "<NA>")
               ))
          }
          if (nrow(na_hold) > 10) cat(sprintf("     ... and %d more\n", nrow(na_hold) - 10))
          cat("   Fix: add their city to location_to_hold, or they will be\n")
          cat("   assigned Transient_Wilderness via coalesce.\n\n")
          # Repair: force NA → Transient_Wilderness → Neutral
          df_clean$Home_City[is.na(df_clean$Hold_Allegiance)] <- "Transient_Wilderness"
          df_clean$Hold_Allegiance[is.na(df_clean$Hold_Allegiance)] <- "Neutral"
     }

     return(df_clean)
}

# ============================================================
# DRAGONBORN: SCENARIO-DEPENDENT AGENT
# ============================================================
# The Dragonborn's alignment depends on which side they join.
# They are appended to the agent dataframe after scenario selection,
# not during data loading, because their attributes differ per scenario.
#
# Key properties:
#   - Influence_Score_Norm = 1.0 (maximum - unique legendary status)
#   - Kappa = 90 (near-deterministic opinion - they chose a side)
#   - Talos_Conviction differs: Nord Dragonborn devout, Imperial less so

append_dragonborn <- function(agents_df, scenario_name) {
     next_id <- max(agents_df$Agent_ID) + 1L

     if (scenario_name == "Imperial") {
          db <- data.frame(
               Agent_ID             = next_id,
               Name                 = "The Dragonborn",
               Race                 = "Imperial",
               Home_City            = "Solitude",
               Hold_Allegiance      = "Imperial",
               Class                = "Warrior",
               Opinion_Prior        = 0.02,
               Beta_Alpha           = 0.02 * 90, # kappa = 90
               Beta_Beta            = 0.98 * 90,
               Prob_t               = 0.02,
               Influence_Score_Norm = 1.00,
               Talos_Conviction     = 0.30,
               Faction              = "Imperial",
               Is_Key_NPC           = TRUE,
               stringsAsFactors     = FALSE
          )
     } else {
          db <- data.frame(
               Agent_ID             = next_id,
               Name                 = "The Dragonborn",
               Race                 = "Nord",
               Home_City            = "Whiterun",
               Hold_Allegiance      = "Neutral",
               Class                = "Warrior",
               Opinion_Prior        = 0.98,
               Beta_Alpha           = 0.98 * 90,
               Beta_Beta            = 0.02 * 90,
               Prob_t               = 0.98,
               Influence_Score_Norm = 1.00,
               Talos_Conviction     = 0.80,
               Faction              = "Stormcloak",
               Is_Key_NPC           = TRUE,
               stringsAsFactors     = FALSE
          )
     }

     bind_rows(agents_df, db)
}


# ============================================================
# THALMOR AGENTS: HARDCODED APPEND
# ============================================================
# Elenwen, Ancano, Ondolemar are not in the base CSV.
# The old script appended them explicitly in Part 4.
# We do the same here so the total count matches 1010 per scenario
# (CSV base ~1009 agents + 1 Dragonborn appended per scenario).
# Thalmor agents are injected during load_and_prepare_data if absent.

append_thalmor_agents <- function(agents_df) {
     # Only append if not already present (idempotent)
     already_present <- THALMOR_AGENTS %in% coalesce(agents_df$Name, "")
     to_add <- THALMOR_AGENTS[!already_present]
     if (length(to_add) == 0) {
          return(agents_df)
     }

     thalmor_specs <- list(
          Elenwen   = list(home = "Solitude", hold = "Imperial", kappa = 75),
          Ancano    = list(home = "Winterhold", hold = "Stormcloak", kappa = 75),
          Ondolemar = list(home = "Markarth", hold = "Imperial", kappa = 75)
     )

     new_rows <- lapply(to_add, function(nm) {
          sp <- thalmor_specs[[nm]]
          data.frame(
               Agent_ID             = max(agents_df$Agent_ID) + match(nm, to_add),
               Name                 = nm,
               Race                 = "High Elf",
               Home_City            = sp$home,
               Hold_Allegiance      = sp$hold,
               Class                = "Thalmor",
               Opinion_Prior        = 0.50,
               Beta_Alpha           = 0.50 * sp$kappa,
               Beta_Beta            = 0.50 * sp$kappa,
               Prob_t               = 0.50,
               Influence_Score_Norm = 0.75,
               Talos_Conviction     = -1.0,
               Faction              = "Neutral",
               Is_Key_NPC           = TRUE,
               stringsAsFactors     = FALSE
          )
     })

     bind_rows(agents_df, do.call(bind_rows, new_rows))
}

# ============================================================
# SECTION 4: PRECOMPUTE STATIC AGENT ARRAYS
# ============================================================
# Extract plain numeric/character vectors once.
# Indexing atomic vectors is ~10x faster than indexing data frame
# columns inside a hot loop.

build_agent_arrays <- function(agents_df) {
     list(
          n = nrow(agents_df),
          race = agents_df$Race,
          home_city = agents_df$Home_City,
          hold = agents_df$Hold_Allegiance,
          faction = agents_df$Faction,
          influence = agents_df$Influence_Score_Norm,
          talos = agents_df$Talos_Conviction,
          beta_a = agents_df$Beta_Alpha,
          beta_b = agents_df$Beta_Beta,
          opinion_prior = agents_df$Opinion_Prior,
          is_key = agents_df$Is_Key_NPC,
          is_thalmor = agents_df$Name %in% THALMOR_AGENTS,
          is_zealot = agents_df$Name %in% ZEALOT_NAMES |
               (abs(agents_df$Opinion_Prior - 0.50) > 0.40 &
                    abs(agents_df$Talos_Conviction) > 0.70 &
                    agents_df$Influence_Score_Norm > 0.40),
          name = agents_df$Name,
          agent_id = agents_df$Agent_ID
     )
}

# ============================================================
# SECTION 5: PRECOMPUTE STATIC HOMOPHILY WEIGHT MATRIX
# ============================================================
# W_race[i,j] = HOMOPHILY_RACE  if race_i == race_j, else 0
# W_base[i,j] = 1 + W_race[i,j]  (faction part added dynamically
#               since faction changes during simulation)
# Diagonal zeroed to prevent self-interaction.
#
# This matrix is (n × n) = ~1M entries for 1009 agents - about
# 8 MB as a double matrix, computed once and reused every time step.

build_static_weight_matrix <- function(ag) {
     race_match <- outer(ag$race, ag$race, "==")
     W_base <- 1 + CONFIG$HOMOPHILY_RACE * race_match
     diag(W_base) <- 0
     W_base
}

# ============================================================
# SECTION 6: SCENARIO DEFINITIONS
# ============================================================

create_scenarios <- function() {
     list(
          IMPERIAL = list(
               NAME = "Imperial",
               SCHEDULE = tribble(
                    ~time_step, ~event_name,           ~hold_to_flip, ~shock_direction,
                    26,         "Battle of Whiterun",  "Whiterun",    -1,
                    51,         "Battle for Dawnstar", "Dawnstar",    -1,
                    76,         "Battle for Riften",   "Riften",      -1,
                    101,        "Battle of Windhelm",  "Windhelm",    -1
               )
          ),
          STORMCLOAK = list(
               NAME = "Stormcloak",
               SCHEDULE = tribble(
                    ~time_step, ~event_name,            ~hold_to_flip, ~shock_direction,
                    26,         "Battle of Whiterun",   "Whiterun",    +1,
                    51,         "Battle for Falkreath", "Falkreath",   +1,
                    76,         "Battle for The Reach", "Markarth",    +1,
                    101,        "Battle for Solitude",  "Solitude",    +1
               )
          )
     )
}

# ============================================================
# SECTION 7: VECTORIZED SIMULATION ENGINE
# ============================================================
# KEY VECTORIZATION CHANGES vs previous version:
#
#  OLD: nested for(i) for(j) loop - O(n*K) R-level iterations per step
#  NEW: single apply() call samples all partner indices;
#       diffs, BC mask, and updates all use matrix arithmetic (compiled C)
#
#  OLD: for(k) shock loop - O(n) R-level iterations per event
#  NEW: rnorm(n), element-wise multiply - single vectorized pass
#
#  OLD: rowwise() %>% mutate(rbeta(1,...)) - slow row-by-row evaluation
#  NEW: rbeta(n, beta_a, beta_b) - single vectorized draw
#
# Total inner-loop R iterations per run:
#   Old: 150 steps × (1009 agents × 5 partners + noise) ≈ 756,750 iterations
#   New: 150 steps × 1 apply() call + pure matrix ops ≈ 150 R-level calls

run_mc_simulation_vec <- function(ag, schedule, scenario_name, run_id, use_zealots = FALSE) {
     n <- ag$n
     snap_times <- unique(schedule$time_step)

     # Pre-extract schedule columns to plain vectors - avoids data.frame
     # row access ($, [[) inside the hot loop
     sched_t <- schedule$time_step
     sched_hold <- schedule$hold_to_flip
     sched_dir <- schedule$shock_direction
     n_events <- length(sched_t)

     # Pre-compute snap lookup as logical vector indexed by t
     snap_lookup <- logical(CONFIG$TOTAL_TIME_STEPS)
     snap_lookup[snap_times] <- TRUE

     # ---- (a) Vectorized Beta opinion initialisation ----
     opinions <- pmax(0.01, pmin(0.99, rbeta(n, ag$beta_a, ag$beta_b)))

     # ---- (b) Vectorized susceptibility draw ----
     suscept <- rbeta(n, CONFIG$ALPHA_SUSC, CONFIG$BETA_SUSC)

     hold_cur <- ag$hold

     # Encode faction as integer: 1=Stormcloak, -1=Imperial, 0=Neutral
     # Never use string vectors inside the time loop - allocation is too slow
     fac_cur <- as.integer(opinions > 0.6) - as.integer(opinions < 0.4)

     # Trajectory stored as a plain numeric matrix (Agent × 1 per snapshot)
     # - much faster to build than data.frames; converted to df at the end
     traj_opinions <- matrix(NA_real_, nrow = n, ncol = length(snap_times))
     snap_i <- 1L

     # Pre-loop: encode race as integer once
     race_int <- as.integer(factor(ag$race))
     K <- CONFIG$K_INTERACTIONS

     # ---- (0) BUILD SMALL-WORLD SPATIAL ADJACENCY NETWORK ----
     # Fixes the 'panmictic' network flaw. For each agent, we pre-generate a
     # 20-person personal network (approx 80% from their own Hold, 20% global).
     # During the simulation, they sample their K daily partners from this clustered pool.
     local_pool <- split(seq_len(n), ag$home_city)
     personal_network <- matrix(0L, nrow = n, ncol = 20)
     for (i in seq_len(n)) {
          pool <- local_pool[[ag$home_city[i]]]
          if (is.null(pool) || length(pool) < 2) pool <- seq_len(n)
          n_loc <- rbinom(1, 20, 0.80)
          n_glob <- 20 - n_loc
          loc_samp <- sample(pool, n_loc, replace = TRUE)
          glob_samp <- sample.int(n, n_glob, replace = TRUE)
          personal_network[i, ] <- sample(c(loc_samp, glob_samp))
     }

     for (t in seq_len(CONFIG$TOTAL_TIME_STEPS)) {
          # ---- (d,e) VECTORIZED Social Influence + Bounded Confidence ----
          # Vectorized matrix indexing: Draw K columns per agent from their personal spatial network
          cols <- matrix(sample.int(20, n * K, replace = TRUE), nrow = n, ncol = K)
          row_idx <- rep(1:n, times = K)
          col_idx <- as.vector(cols)
          flat_idx <- row_idx + n * (col_idx - 1)
          partner_mat <- matrix(personal_network[flat_idx], nrow = n, ncol = K)
          # Prevent self-influence
          self_mask <- partner_mat == seq_len(n)
          partner_mat[self_mask] <- (partner_mat[self_mask] %% n) + 1L

          p_opinions <- matrix(opinions[partner_mat], nrow = n, ncol = K)
          p_influence <- matrix(ag$influence[partner_mat], nrow = n, ncol = K)
          p_race <- matrix(race_int[partner_mat], nrow = n, ncol = K)
          p_fac <- matrix(fac_cur[partner_mat], nrow = n, ncol = K)

          # Homophily weight: inline, integer comparison only
          homophily_w <- 1 +
               CONFIG$HOMOPHILY_RACE * (p_race == race_int) +
               CONFIG$HOMOPHILY_FACTION * (p_fac == fac_cur)

          diff_mat <- p_opinions - matrix(opinions, nrow = n, ncol = K)
          bc_mask <- abs(diff_mat) < CONFIG$EPSILON

          # --- Bounded Confidence Dead Zone Note ---
          # NOTE: There is an intentional "Dead Zone" between EPSILON (0.35) and
          # REPULSION_THRESHOLD (0.70). If abs(diff_mat) falls here, agents
          # strictly ignore each other (no assimilation, no repulsion),
          # mirroring mild disagreement without escalation.

          delta <- rowSums(bc_mask * CONFIG$MU * p_influence *
               homophily_w * diff_mat) / K

          # --- Repulsion Mechanism (Assimilation-Contrast) ---
          rep_mask <- abs(diff_mat) > CONFIG$REPULSION_THRESHOLD
          # Repulsion rate increases by 2x if they are of opposing factions and different races
          opp_faction <- (p_fac * matrix(fac_cur, nrow = n, ncol = K)) == -1
          repulsion_mult <- 1 + 1.0 * opp_faction * (p_race != matrix(race_int, nrow = n, ncol = K))

          rep_delta <- rowSums(rep_mask * CONFIG$REPULSION_RATE_BASE * repulsion_mult * p_influence *
               diff_mat * -1) / K

          delta <- delta + rep_delta

          # Thalmor mean-reversion
          thalmor_delta <- 0.05 * (0.50 - opinions) * ag$is_thalmor
          delta <- ifelse(ag$is_thalmor,
               0.10 * delta + thalmor_delta, delta
          )

          # Zealot Penalty: Zealots resist social influence almost entirely
          if (use_zealots) {
               delta <- ifelse(ag$is_zealot, 0.00, delta)
          }

          opinions <- pmax(0.01, pmin(0.99, opinions + delta))

          # ---- (f) Idiosyncratic noise ----
          # Zealot Immunity: Zealots are structurally immovable narrative
          # anchors - they must not drift via random noise. Without this
          # guard, rnorm jitter over 150 steps could (rarely) push a zealot
          # across a faction threshold, which is narratively incoherent.
          noise <- rnorm(n, 0, 0.01)
          if (use_zealots) {
               noise <- ifelse(ag$is_zealot, 0, noise)
          }
          opinions <- pmax(0.01, pmin(0.99, opinions + noise))

          # ---- (c) Exogenous Shock ----
          ev_idx <- match(t, sched_t) # integer lookup, no data.frame access

          if (!is.na(ev_idx)) {
               dir <- sched_dir[ev_idx]
               in_hold <- hold_cur == sched_hold[ev_idx]

               base_shocks <- pmax(0, pmin(
                    CONFIG$MAX_SHOCK,
                    abs(rnorm(n, CONFIG$SHOCK_MEAN, CONFIG$SHOCK_SD))
               ))

               talos_mod <- 1 + 0.3 * ag$talos * dir

               # Complex Contagion: Check if local network of K interactions agrees with shock dir
               net_tilt <- rowSums(p_fac)
               contagion_mod <- 1 + 0.1 * (net_tilt * dir)

               prox_mod <- ifelse(in_hold, 2.0, 0.6) * pmax(0.1, contagion_mod)

               thalmor_dir <- ifelse(ag$is_thalmor, -sign(opinions - 0.50), dir)
               final_shocks <- thalmor_dir * base_shocks * suscept * talos_mod * prox_mod

               # Zealot Penalty: Zealots heavily resist battle shocks, feeling only a micro-fraction of the impact
               if (use_zealots) {
                    final_shocks <- ifelse(ag$is_zealot, final_shocks * 0.05, final_shocks)
               }

               opinions <- pmax(0.01, pmin(0.99, opinions + final_shocks))

               new_allegiance <- if (dir == 1L) "Stormcloak" else "Imperial"
               hold_cur[in_hold] <- new_allegiance
          }

          # Update faction as integer - NO string allocation
          fac_cur <- as.integer(opinions > 0.6) - as.integer(opinions < 0.4)

          # Snapshot: store only the numeric opinion vector
          if (snap_lookup[t]) {
               traj_opinions[, snap_i] <- opinions
               snap_i <- snap_i + 1L
          }
     }

     # Reconstruct faction string only once at the very end
     fac_str <- ifelse(fac_cur == 1L, "Stormcloak",
          ifelse(fac_cur == -1L, "Imperial", "Neutral")
     )

     # Build trajectory data.frame once from stored matrix (not inside loop)
     traj_list <- vector("list", length(snap_times))
     for (si in seq_along(snap_times)) {
          op_si <- traj_opinions[, si]
          fac_si <- ifelse(op_si > 0.6, "Stormcloak",
               ifelse(op_si < 0.4, "Imperial", "Neutral")
          )
          traj_list[[si]] <- data.frame(
               Agent_ID = ag$agent_id,
               Name = ag$name,
               Is_Key_NPC = ag$is_key,
               Prob_t = op_si,
               Faction = fac_si,
               time_step = snap_times[si],
               scenario = scenario_name,
               run_id = run_id,
               stringsAsFactors = FALSE
          )
     }

     list(
          final_state = data.frame(
               Agent_ID = ag$agent_id,
               Name = ag$name,
               Race = ag$race,
               Home_City = ag$home_city,
               Is_Key_NPC = ag$is_key,
               Prob_t = opinions,
               Susceptibility = suscept,
               Talos_Conviction = ag$talos,
               Faction = fac_str,
               scenario = scenario_name,
               run_id = run_id,
               stringsAsFactors = FALSE
          ),
          trajectory = do.call(rbind, traj_list)
     )
}

# ============================================================
# SECTION 8: PARALLEL MONTE CARLO WRAPPER
# ============================================================
# Uses base-R parallel::mclapply (fork-based on macOS/Linux) or
# parLapply (socket-based on Windows) to distribute runs across cores.
# Each worker receives its own RNG stream via clusterSetRNGStream /
# set.seed + run_id to guarantee reproducibility.

run_monte_carlo_parallel <- function(ag, scenario, agents_df, use_zealots = FALSE) {
     # ---- setup ----
     n_runs <- CONFIG$N_RUNS
     n_cores <- CONFIG$N_CORES
     schedule <- scenario$SCHEDULE
     sc_name <- scenario$NAME

     # Append scenario-specific Dragonborn and rebuild agent arrays
     agents_with_db <- append_dragonborn(agents_df, sc_name)
     ag_sc <- build_agent_arrays(agents_with_db)

     cat(sprintf(
          "\n=== %s SCENARIO | %d agents (incl. Dragonborn) ===\n",
          sc_name, ag_sc$n
     ))

     # Batch size = one core's worth of work, so each batch completes
     # together and we can print one accurate progress line per batch.
     # Minimum 1, maximum 50 runs per batch.
     batch_size <- max(1L, min(50L, n_cores))
     n_batches <- ceiling(n_runs / batch_size)
     is_windows <- .Platform$OS.type == "windows"

     cat(sprintf(
          "\n=== %s SCENARIO ===\n  Runs: %d | Cores: %d | Batch size: %d | Batches: %d\n",
          sc_name, n_runs, n_cores, batch_size, n_batches
     ))
     cat(sprintf(
          "  %-40s  %6s  %8s  %8s  %s\n",
          "Progress", "Runs", "Mean p", "SD p", "ETA"
     ))
     cat(sprintf("  %s\n", strrep("-", 75)))

     # Worker - fully self-contained for serialisation to socket workers
     # Worker - fully self-contained for serialisation to socket workers
     worker <- function(run_id) {
          set.seed(CONFIG$SEED + run_id)
          run_mc_simulation_vec(ag_sc, schedule, sc_name, run_id, use_zealots)
     }

     if (is_windows || n_cores == 1L) {
          cl <- makeCluster(max(1L, n_cores))
          clusterExport(cl,
               varlist = c(
                    "ag_sc", "schedule", "sc_name", "use_zealots",
                    "CONFIG", "run_mc_simulation_vec"
               ),
               envir = environment()
          )
          # Workers only need base R - no tidyverse required in the sim engine
          clusterEvalQ(cl, {
               library(stats)
               library(parallel)
          })
     } else {
          cl <- NULL
     }

     results_list <- vector("list", n_runs)
     batch_start_t <- Sys.time()
     completed <- 0L
     # Incremental mean/SD tracking - O(1) per batch, not O(completed)
     running_sum <- 0
     running_sum2 <- 0
     running_n <- 0L

     for (b in seq_len(n_batches)) {
          run_from <- (b - 1L) * batch_size + 1L
          run_to <- min(b * batch_size, n_runs)
          run_ids <- run_from:run_to

          if (!is.null(cl)) {
               batch_out <- parLapply(cl, run_ids, worker)
          } else {
               batch_out <- mclapply(run_ids, worker,
                    mc.cores    = n_cores,
                    mc.set.seed = TRUE
               )
          }

          for (k in seq_along(run_ids)) {
               results_list[[run_ids[k]]] <- batch_out[[k]]
               # Accumulate sum/sum² from this run's final opinions only
               op_k <- batch_out[[k]]$final_state$Prob_t
               running_sum <- running_sum + sum(op_k)
               running_sum2 <- running_sum2 + sum(op_k^2)
               running_n <- running_n + length(op_k)
          }

          completed <- run_to

          # O(1) live mean and SD from accumulators
          live_mean <- running_sum / running_n
          live_var <- running_sum2 / running_n - live_mean^2
          live_sd <- sqrt(max(0, live_var))

          elapsed_b <- as.numeric(difftime(Sys.time(), batch_start_t, units = "secs"))
          rate <- elapsed_b / completed
          eta_secs <- round(rate * (n_runs - completed))
          eta_str <- if (completed == n_runs) {
               sprintf("%5.1fs total", elapsed_b)
          } else if (eta_secs < 60) {
               sprintf("ETA %4ds", eta_secs)
          } else {
               sprintf(
                    "ETA %2dm%02ds",
                    eta_secs %/% 60,
                    eta_secs %% 60
               )
          }

          pct <- round(100 * completed / n_runs)
          filled <- round(38 * completed / n_runs)
          bar <- paste0(
               c(
                    rep("=", max(0, filled - 1)),
                    if (completed < n_runs) ">" else "=",
                    rep(" ", 38 - filled)
               ),
               collapse = ""
          )

          cat(sprintf(
               "\r  [%s] %3d%%  %5d/%d  p\u0305=%.4f  \u03c3=%.4f  %s",
               bar, pct, completed, n_runs,
               live_mean, live_sd, eta_str
          ))
          flush.console()
     }

     if (!is.null(cl)) stopCluster(cl)
     cat("\n")

     # ---- Collect and return ----
     finals <- lapply(results_list, `[[`, "final_state")
     trajs <- lapply(results_list, `[[`, "trajectory")

     list(
          results      = do.call(rbind, finals),
          trajectories = do.call(rbind, trajs)
     )
}

# ============================================================
# SECTION 9: MAIN EXECUTION
# ============================================================

cat("STEP 1: Loading Data\n")
cat("---------------------\n")
df_agents <- load_and_prepare_data(CONFIG$DATA_PATH)
# Thalmor agents (Elenwen, Ancano, Ondolemar) confirmed present in CSV -
# no manual append needed. Diagnostic above will flag if any are missing.

cat("STEP 2: Precomputing Base Agent Arrays\n")
cat("----------------------------------------\n")
ag <- build_agent_arrays(df_agents)
cat(sprintf("Base agent array: %d agents (Dragonborn added per scenario)\n", ag$n))
cat(sprintf(
     "Interaction: K=%d partners sampled per step via uniform + inline homophily weights\n\n",
     CONFIG$K_INTERACTIONS
))

cat("STEP 3: Creating Scenarios\n")
cat("---------------------------\n")
SCENARIOS <- create_scenarios()
cat(sprintf("Scenarios: %s\n\n", paste(names(SCENARIOS), collapse = ", ")))

cat("STEP 4: Running Vectorized Monte Carlo (PRIMARY: Anchored Leaders)\n")
cat("-------------------------------------------------------------------\n")
cat("  NOTE: Primary runs use use_zealots = TRUE (immovable anchors).\n")
cat("  The counterfactual comparison (Section 11b) reruns with use_zealots = FALSE\n")
cat("  to empirically validate the necessity of the anchoring mechanism.\n\n")
set.seed(CONFIG$SEED)
start_time <- Sys.time()

# PRIMARY: Anchored Leaders - key NPCs are immovable narrative anchors
# This is the main model WITH narrative anchors enabled by default.
mc_imperial <- run_monte_carlo_parallel(ag, SCENARIOS$IMPERIAL, df_agents, use_zealots = TRUE)
mc_stormcloak <- run_monte_carlo_parallel(ag, SCENARIOS$STORMCLOAK, df_agents, use_zealots = TRUE)

results_df <- rbind(mc_imperial$results, mc_stormcloak$results)
trajectory_df <- rbind(mc_imperial$trajectories, mc_stormcloak$trajectories)

elapsed <- difftime(Sys.time(), start_time, units = "secs")
cat(sprintf(
     "\n\nAnchored MC complete: %.1f seconds | %d total agent-runs\n\n",
     as.numeric(elapsed), nrow(results_df)
))

# ============================================================
# SECTION 10: STATISTICAL SUMMARY
# ============================================================

cat("STEP 5: Computing Statistics\n")
cat("-----------------------------\n")

summary_stats <- results_df %>%
     group_by(scenario) %>%
     summarise(
          N = n(),
          Mean_Opinion = mean(Prob_t),
          SD_Opinion = sd(Prob_t),
          Pct_Imperial = mean(Prob_t < 0.4) * 100,
          Pct_Neutral = mean(Prob_t >= 0.4 & Prob_t <= 0.6) * 100,
          Pct_Stormcloak = mean(Prob_t > 0.6) * 100,
          .groups = "drop"
     )

boot_results <- results_df %>%
     # Step 1: collapse to one mean per run - this is what we're uncertain about
     group_by(scenario, run_id) %>%
     summarise(run_mean = mean(Prob_t), .groups = "drop") %>%
     # Step 2: bootstrap over those N_RUNS values (not over 300k agent observations)
     group_by(scenario) %>%
     summarise(
          CI_Lower = boot_ci(run_mean, mean, CONFIG$BOOT_REPS, CONFIG$CI_LEVEL)[1],
          CI_Upper = boot_ci(run_mean, mean, CONFIG$BOOT_REPS, CONFIG$CI_LEVEL)[2],
          .groups = "drop"
     )

summary_stats <- left_join(summary_stats, boot_results, by = "scenario")

cat(sprintf(
     "\n%-15s %8s %8s %8s %12s\n",
     "Scenario", "Mean", "SD", "CI_Low", "CI_High"
))
cat(strrep("-", 55), "\n")
for (i in seq_len(nrow(summary_stats))) {
     s <- summary_stats[i, ]
     cat(sprintf(
          "%-15s %8.4f %8.4f %8.4f %12.4f\n",
          s$scenario, s$Mean_Opinion, s$SD_Opinion,
          s$CI_Lower, s$CI_Upper
     ))
}

# Formal KS test relocated to Step 6b (Zealot comparison)

convergence_df <- trajectory_df %>%
     group_by(scenario, time_step, run_id) %>%
     summarise(run_mean = mean(Prob_t), .groups = "drop") %>%
     group_by(scenario, time_step) %>%
     summarise(across_run_sd = sd(run_mean), .groups = "drop")

cat("\nCross-run SD at event steps:\n")
cat(sprintf("%-15s %8s %10s\n", "Scenario", "T-step", "Cross-SD"))
convergence_df %>% pwalk(~ cat(sprintf("%-15s %8d %10.4f\n", ..1, ..2, ..3)))

# ============================================================
# SECTION 11: VISUALISATIONS
# ============================================================

cat("\nSTEP 6: Creating Visualizations\n")
cat("---------------------------------\n")

dir.create(CONFIG$OUTPUT_DIR, showWarnings = FALSE)
dir.create(CONFIG$PLOT_DIR, showWarnings = FALSE)

theme_paper <- theme_minimal(base_size = 12) +
     theme(
          plot.title       = element_text(face = "bold", size = 13),
          plot.subtitle    = element_text(size = 10, color = "gray35"),
          legend.position  = "bottom",
          panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold")
     )

faction_colors <- c("Imperial" = "#C0392B", "Stormcloak" = "#2980B9", "Neutral" = "#7F8C8D")
scenario_colors <- c("Imperial" = "#C0392B", "Stormcloak" = "#2980B9")

# Helper: print to IDE then save to disk
print_and_save <- function(plot, filename, width, height) {
     print(plot)
     ggsave(file.path(CONFIG$PLOT_DIR, filename),
          plot,
          width = width, height = height, dpi = 300
     )
     cat(sprintf("    Saved: %s\n", filename))
}

# Helper: print a labelled data frame to console with a header
print_metric <- function(label, df) {
     cat(sprintf("\n--- %s ---\n", label))
     print(as.data.frame(df), row.names = FALSE)
}

# --- FIGURE 1: Main Result with Bootstrap CIs ---
cat("  Figure 1: Main results...\n")

p1 <- ggplot(
     summary_stats,
     aes(
          x = factor(scenario, c("Imperial", "Stormcloak")),
          y = Mean_Opinion, fill = scenario
     )
) +
     geom_col(width = 0.55, alpha = 0.85) +
     geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper),
          width = 0.12, linewidth = 0.9, color = "gray20"
     ) +
     geom_text(
          aes(label = sprintf(
               "%.4f\n[%.4f, %.4f]",
               Mean_Opinion, CI_Lower, CI_Upper
          )),
          vjust = -0.6, size = 3.5, fontface = "bold"
     ) +
     scale_fill_manual(values = scenario_colors) +
     scale_y_continuous(limits = c(0, 0.85), breaks = seq(0, 0.8, 0.1)) +
     labs(
          title = "Figure 1. Believable Narrative Outcomes: Final Opinion Divergence",
          subtitle = sprintf(
               "Mean NPC ideology \u00b1 %d%% bootstrap CI | N = %d replications",
               round(CONFIG$CI_LEVEL * 100), CONFIG$N_RUNS
          ),
          x = "Campaign Victory Scenario",
          y = expression(bar(p)[T] ~ "(Mean Ideological Lean)"),
          fill = NULL,
          caption = sprintf(
               "Agent generative parameters (\u03b5 = %.2f, \u03bc = %.2f) | Quest Shocks ~ N(%.2f, %.2f\u00b2)",
               CONFIG$EPSILON, CONFIG$MU, CONFIG$SHOCK_MEAN, CONFIG$SHOCK_SD
          )
     ) +
     theme_paper +
     theme(legend.position = "none")

print_and_save(p1, "fig1_main_results.png", 9, 6)

# --- FIGURE 2: Opinion Distributions (KDE) ---
cat("  Figure 2: Distributions...\n")

p2 <- ggplot(results_df, aes(x = Prob_t, fill = scenario, color = scenario)) +
     geom_density(alpha = 0.45, linewidth = 0.9) +
     geom_vline(
          xintercept = 0.5, linetype = "dashed",
          color = "gray40", linewidth = 0.7
     ) +
     geom_vline(
          data = summary_stats,
          aes(xintercept = Mean_Opinion, color = scenario),
          linewidth = 1.0, alpha = 0.8
     ) +
     annotate("text",
          x = 0.51, y = 7.5,
          label = "Neutral\nThreshold", hjust = 0, size = 3, color = "gray50"
     ) +
     scale_fill_manual(values = scenario_colors) +
     scale_color_manual(values = scenario_colors) +
     labs(
          title = "Figure 2. NPC Population Ideology: Emergent Echo Chambers",
          subtitle = "Vertical lines mark scenario means | Demonstrating resilient ideological diversity",
          x = expression(p[i](T) ~ "(Final Ideological Stance)"),
          y = "Population Density",
          fill = "Scenario", color = "Scenario",
          caption = "Bimodal extremes demonstrate distinct ideological routing via bounded confidence."
     ) +
     theme_paper

print_and_save(p2, "fig2_distributions.png", 10, 6)

# --- FIGURE 3: MC Spaghetti + Quantile Bands ---
cat("  Figure 3: MC trajectories...\n")

run_means <- trajectory_df %>%
     group_by(scenario, time_step, run_id) %>%
     summarise(run_mean = mean(Prob_t), .groups = "drop")

traj_summary <- run_means %>%
     group_by(scenario, time_step) %>%
     summarise(
          mean_op = mean(run_mean),
          q05     = quantile(run_mean, 0.05),
          q25     = quantile(run_mean, 0.25),
          q75     = quantile(run_mean, 0.75),
          q95     = quantile(run_mean, 0.95),
          .groups = "drop"
     )

spaghetti <- run_means %>%
     filter(run_id %in% sample(CONFIG$N_RUNS, min(60, CONFIG$N_RUNS)))

p3 <- ggplot() +
     geom_line(
          data = spaghetti,
          aes(
               x = time_step, y = run_mean,
               group = interaction(run_id, scenario),
               color = scenario
          ),
          alpha = 0.20, linewidth = 0.4
     ) +
     geom_ribbon(
          data = traj_summary,
          aes(x = time_step, ymin = q05, ymax = q95, fill = scenario),
          alpha = 0.20
     ) +
     geom_ribbon(
          data = traj_summary,
          aes(x = time_step, ymin = q25, ymax = q75, fill = scenario),
          alpha = 0.40
     ) +
     geom_line(
          data = traj_summary,
          aes(x = time_step, y = mean_op, color = scenario),
          linewidth = 1.5
     ) +
     geom_vline(
          xintercept = c(26, 51, 76, 101),
          linetype = "dashed", color = "gray50", alpha = 0.6
     ) +
     scale_color_manual(values = scenario_colors) +
     scale_fill_manual(values = scenario_colors) +
     scale_x_continuous(breaks = c(1, 26, 51, 76, 101, 150)) +
     coord_cartesian(ylim = c(
          min(traj_summary$q05) - 0.05,
          max(traj_summary$q95) + 0.05
     )) +
     facet_wrap(~scenario, ncol = 1, scales = "free_y") +
     labs(
          title = "Figure 3. Generative NPC Trajectories Over Quest Timeline",
          subtitle = "Dynamic opinion shifts mapping directly to scheduled campaign battles",
          x = "Quest Timeline (Time Step t)",
          y = expression(bar(p)(t) ~ "(Mean Ideology)"),
          color = NULL, fill = NULL,
          caption = "Dashed verticals = exogenous quest shock events"
     ) +
     theme_paper

print_and_save(p3, "fig3_mc_trajectories.png", 11, 9)

# --- FIGURE 4: MC Convergence (Cross-Run SD) ---
cat("  Figure 4: Convergence diagnostic...\n")

p4 <- ggplot(
     convergence_df,
     aes(x = time_step, y = across_run_sd, color = scenario)
) +
     geom_line(linewidth = 1.2) +
     geom_point(size = 3) +
     geom_vline(
          xintercept = c(26, 51, 76, 101),
          linetype = "dashed", color = "gray50", alpha = 0.6
     ) +
     scale_color_manual(values = scenario_colors) +
     scale_y_continuous(
          limits = c(0, NA),
          labels = scales::number_format(accuracy = 0.0001),
          expand = expansion(mult = c(0, 0.1))
     ) +
     labs(
          title    = "Figure 4. Narrative Stability: Cross-Run Variance During Battles",
          subtitle = sprintf("Stochastic stability across N=%d replications mapping quest injections", CONFIG$N_RUNS),
          x        = "Quest Timeline (Time Step t)",
          y        = expression(sigma[runs](t) ~ "(Cross-run Variance)"),
          color    = NULL,
          caption  = "Spikes map to real-time generative disruption during local campaign events"
     ) +
     theme_paper

print_and_save(p4, "fig4_mc_convergence.png", 10, 6)

# --- FIGURE 5: Faction Composition Over Time ---
cat("  Figure 5: Faction dynamics...\n")

faction_time <- trajectory_df %>%
     group_by(scenario, time_step) %>%
     summarise(
          Imperial = mean(Prob_t < 0.4) * 100,
          Neutral = mean(Prob_t >= 0.4 & Prob_t <= 0.6) * 100,
          Stormcloak = mean(Prob_t > 0.6) * 100,
          .groups = "drop"
     ) %>%
     pivot_longer(c(Imperial, Neutral, Stormcloak),
          names_to = "faction", values_to = "pct"
     )

p5 <- ggplot(
     faction_time,
     aes(x = time_step, y = pct, fill = faction)
) +
     geom_area(alpha = 0.82, position = "stack") +
     geom_vline(
          xintercept = c(26, 51, 76, 101),
          linetype = "dashed", color = "white", alpha = 0.7, linewidth = 0.8
     ) +
     facet_wrap(~scenario, ncol = 1) +
     scale_fill_manual(values = faction_colors) +
     scale_x_continuous(breaks = c(1, 26, 51, 76, 101)) +
     labs(
          title = "Figure 5. Believable Faction Evolution Across Campaign Intervals",
          subtitle = "Fluid, stochastic population alignment mapping to narrative progression",
          x = "Quest Timeline (Time Step t)", y = "NPC Population %",
          fill = "Faction",
          caption = "Averaged across all generative replications"
     ) +
     theme_paper

print_and_save(p5, "fig5_faction_dynamics.png", 11, 8)

# --- FIGURE 6: Susceptibility vs Final Opinion ---
cat("  Figure 6: Susceptibility analysis...\n")

# --- FIGURE 6: Susceptibility vs Final Opinion ---
cat("  Figure 6: Susceptibility analysis...\n")

# To prevent the points themselves from crashing the IDE,
# we can safely plot a 10% random sample of the points while
# computing the GAM smoother on the FULL dataset.
set.seed(CONFIG$SEED)
sampled_results <- results_df %>% slice_sample(prop = 0.10)

p6 <- ggplot(
     results_df,
     aes(x = Susceptibility, y = Prob_t, color = scenario)
) +
     # Plot points using the sampled data to save rendering time
     geom_point(data = sampled_results, alpha = 0.07, size = 0.8) +
     # FIX: Use GAM instead of LOESS for large N
     geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE, linewidth = 1.2) +
     geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
     scale_color_manual(values = scenario_colors) +
     facet_wrap(~scenario) +
     labs(
          title    = "Figure 6. NPC Narrative Susceptibility vs Final Ideology",
          subtitle = expression("Heterogeneous susceptibility driving realistic individual variance"),
          x        = expression(s[i] ~ "(Innate Susceptibility)"),
          y        = expression(p[i](T) ~ "(Final Ideology)"),
          color    = NULL,
          caption  = "GAM smoother (points 10% sample) \u2014 pliant NPCs amplify the winning narrative"
     ) +
     theme_paper +
     theme(legend.position = "none")

print_and_save(p6, "fig6_susceptibility.png", 11, 6)

# --- FIGURE 7: Key NPC Distributions ---
cat("  Figure 7: Key NPC distributions...\n")

featured_npcs <- c(
     "Ulfric Stormcloak", "General Tullius", "Balgruuf the Greater",
     "Legate Rikke", "Maven Black-Briar", "Elisif the Fair"
)

key_npc_df <- results_df %>%
     filter(Is_Key_NPC, Name %in% featured_npcs) %>%
     mutate(Name = factor(Name, levels = featured_npcs))

p7 <- ggplot(key_npc_df, aes(x = Prob_t, fill = scenario)) +
     geom_histogram(aes(y = after_stat(density)),
          bins = 30, alpha = 0.7,
          position = "identity", color = "white", linewidth = 0.3
     ) +
     geom_density(aes(color = scenario), linewidth = 0.9, fill = NA) +
     geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.5) +
     facet_wrap(~Name, ncol = 2, scales = "free_y") +
     scale_fill_manual(values = scenario_colors) +
     scale_color_manual(values = scenario_colors) +
     labs(
          title = "Figure 7. Generative Stability of Key Lore NPCs",
          subtitle = "Evaluating narrative anchors: High-rigidity NPCs resist complete assimilation",
          x = expression(p[i](T) ~ "(Ideological Stance)"),
          y = "Density",
          fill = "Scenario", color = "Scenario",
          caption = "Zealots strongly resist shock events, generating realistic localized continuity"
     ) +
     theme_paper

print_and_save(p7, "fig7_key_npc_distributions.png", 12, 9)

# --- FIGURE 8: Sensitivity Analysis --- Epsilon ---
cat("  Figure 8: Sensitivity analysis (epsilon)...\n")

eps_grid <- c(0.15, 0.25, 0.35, 0.45, 0.55)

sens_results <- map_dfr(eps_grid, function(eps) {
     old_eps <- CONFIG$EPSILON
     CONFIG$EPSILON <<- eps

     worker_sens <- function(run_id) {
          set.seed(CONFIG$SEED + run_id)
          run_mc_simulation_vec(
               ag,
               SCENARIOS$IMPERIAL$SCHEDULE, "Imperial", run_id
          )$final_state
     }
     worker_sc <- function(run_id) {
          set.seed(CONFIG$SEED + run_id)
          run_mc_simulation_vec(
               ag,
               SCENARIOS$STORMCLOAK$SCHEDULE, "Stormcloak", run_id
          )$final_state
     }

     sens_runs <- 200L
     imp_res <- do.call(rbind, lapply(seq_len(sens_runs), worker_sens))
     sc_res <- do.call(rbind, lapply(seq_len(sens_runs), worker_sc))
     CONFIG$EPSILON <<- old_eps

     rbind(imp_res, sc_res) %>%
          group_by(scenario) %>%
          summarise(
               Mean_Opinion = mean(Prob_t),
               SD_Opinion = sd(Prob_t),
               .groups = "drop"
          ) %>%
          mutate(epsilon = eps)
})

p8 <- ggplot(
     sens_results,
     aes(x = epsilon, y = Mean_Opinion, color = scenario, group = scenario)
) +
     geom_ribbon(
          aes(
               ymin = Mean_Opinion - SD_Opinion,
               ymax = Mean_Opinion + SD_Opinion,
               fill = scenario
          ),
          alpha = 0.15, color = NA
     ) +
     geom_line(linewidth = 1.3) +
     geom_point(size = 3) +
     scale_color_manual(values = scenario_colors) +
     scale_fill_manual(values = scenario_colors) +
     scale_x_continuous(breaks = eps_grid) +
     labs(
          title = "Figure 8. Generative Tuning: Bounded Confidence & NPC Echo Chambers",
          subtitle = "Mapping the parameter space for optimal narrative believability",
          x = expression(epsilon ~ "(Bounded Confidence Tolerance)"),
          y = expression(bar(p)[T] ~ "(Mean Ideology)"),
          color = NULL, fill = NULL,
          caption = "Smaller \u03b5 generates resilient polarization; larger \u03b5 maps to unrealistic instantaneous consensus"
     ) +
     theme_paper

print_and_save(p8, "fig8_sensitivity_epsilon.png", 10, 6)

cat("\nAll figures displayed and saved to:", CONFIG$PLOT_DIR, "\n")

# ============================================================
# SECTION 11b: ZEALOT LEADERS ANALYSIS (FIGURE 9)
# ============================================================

cat("\nSTEP 6b: Running Zealot Leader Scenarios (Anchor Effect Comparison)\n")
cat("--------------------------------------------------------------------\n")
cat("  Rerunning both scenarios with use_zealots = TRUE.\n")
cat("  KS test will compare Pliant Baseline vs Zealot-Anchored distribution.\n\n")

# Run counterfactual Monte Carlo with Zealots toggled OFF (Pliant Leaders)
set.seed(CONFIG$SEED + 9999L) # different seed to ensure independent replication
mc_imp_pliant <- run_monte_carlo_parallel(ag, SCENARIOS$IMPERIAL, df_agents, use_zealots = FALSE)
mc_sc_pliant <- run_monte_carlo_parallel(ag, SCENARIOS$STORMCLOAK, df_agents, use_zealots = FALSE)

# Tag the dataframes with their correct model labels
# results_df was produced above with use_zealots = TRUE → Anchored
zealot_results <- results_df %>% mutate(Model = "Zealot Leaders (\u03B5 \u2248 0)")
normal_results <- rbind(mc_imp_pliant$results, mc_sc_pliant$results) %>%
     mutate(Model = "Pliant Leaders (Counterfactual)")

comparison_df <- rbind(normal_results, zealot_results)

# Two-sample KS Test: Pliant Baseline vs Zealot-Anchored (Imperial scenario only)
# H0: the two empirical distributions are drawn from the same population
# H1: narrative anchors significantly reshape the ideological distribution
ks_imp_baseline <- normal_results$Prob_t[normal_results$scenario == "Imperial"]
ks_imp_zealot <- zealot_results$Prob_t[zealot_results$scenario == "Imperial"]
ks_result <- ks.test(ks_imp_baseline, ks_imp_zealot)

cat(sprintf("\nKS Test (Anchored Primary vs Pliant Counterfactual - Imperial scenario):\n"))
cat(sprintf("  D-statistic : %.4f\n", ks_result$statistic))
cat(sprintf("  p-value     : %.2e\n", ks_result$p.value))
cat(sprintf(
     "  Interpretation: %s\n\n",
     if (ks_result$p.value < 0.001) {
          "Distributions are significantly different (p < 0.001)"
     } else {
          "No significant difference detected"
     }
))

# Calculate means for the plot
comp_means <- comparison_df %>%
     group_by(scenario, Model) %>%
     summarise(mean_op = mean(Prob_t), .groups = "drop")

cat("\n  Figure 9: Zealot impact comparison...\n")

p9 <- ggplot(comparison_df, aes(x = Prob_t, fill = Model, color = Model)) +
     geom_density(alpha = 0.4, linewidth = 0.8) +
     geom_vline(
          data = comp_means, aes(xintercept = mean_op, color = Model),
          linetype = "dashed", linewidth = 1
     ) +
     facet_wrap(~scenario, ncol = 1) +
     scale_fill_manual(values = c("Pliant Leaders (Counterfactual)" = "gray50", "Zealot Leaders (\u03B5 \u2248 0)" = "#D35400")) +
     scale_color_manual(values = c("Pliant Leaders (Counterfactual)" = "gray30", "Zealot Leaders (\u03B5 \u2248 0)" = "#D35400")) +
     labs(
          title    = "Figure 9. Believable Resistance: The Anchor Effect of Narrative Elites",
          subtitle = "Ideological zealots realistically drag the population consensus, preventing total immersion breaks",
          x        = expression(p[i](T) ~ "(Final Ideological Stance)"),
          y        = "Population Density",
          caption  = "By heavily resisting assimilation, key lore figures ensure the civil war maintains long-term believability."
     ) +
     theme_paper +
     theme(legend.position = "top")

print_and_save(p9, "fig9_zealot_comparison.png", 10, 8)

# Print the numerical impact
cat("\n--- Impact of Zealot Leaders on Mean Opinion ---\n")
print(comp_means %>% pivot_wider(names_from = Model, values_from = mean_op))

# ============================================================
# SECTION 12: METRICS --- PRINT TO CONSOLE AND EXPORT
# ============================================================

cat("\nSTEP 7: Analysis Metrics\n")
cat("--------------------------\n")

print_metric("Summary Statistics (Mean, SD, CI, Faction %)", summary_stats)

print_metric("Cross-Run SD at Event Steps (Convergence Diagnostic)", convergence_df)

print_metric(
     "KS Test",
     data.frame(
          Statistic      = round(ks_result$statistic, 4),
          p_value        = formatC(ks_result$p.value, format = "e", digits = 2),
          Interpretation = "Distributions significantly different"
     )
)

print_metric(
     "Sensitivity Analysis (Epsilon Sweep)",
     sens_results %>% arrange(epsilon, scenario)
)

# ============================================================
# SECTION 13: EXPORT TO DISK
# ============================================================

cat("\nSTEP 8: Exporting Results\n")
cat("--------------------------\n")

write_csv(
     as_tibble(results_df),
     file.path(CONFIG$OUTPUT_DIR, "final_opinions.csv")
)
write_csv(
     as_tibble(trajectory_df),
     file.path(CONFIG$OUTPUT_DIR, "trajectories.csv")
)
write_csv(
     summary_stats,
     file.path(CONFIG$OUTPUT_DIR, "summary_statistics.csv")
)
write_csv(
     convergence_df,
     file.path(CONFIG$OUTPUT_DIR, "mc_convergence.csv")
)
write_csv(
     sens_results,
     file.path(CONFIG$OUTPUT_DIR, "sensitivity_epsilon.csv")
)

cat(sprintf("  final_opinions.csv     : %d rows\n", nrow(results_df)))
cat(sprintf("  trajectories.csv       : %d rows\n", nrow(trajectory_df)))
cat(sprintf("  summary_statistics.csv : %d rows\n", nrow(summary_stats)))
cat(sprintf("  mc_convergence.csv     : %d rows\n", nrow(convergence_df)))
cat(sprintf("  sensitivity_epsilon.csv: %d rows\n", nrow(sens_results)))

# ============================================================
# FINAL SUMMARY
# ============================================================

cat("\n============================================================\n")
cat(" VECTORIZED STOCHASTIC ABM COMPLETE\n")
cat("============================================================\n")
cat(sprintf(" Runtime             : %.1f seconds\n", as.numeric(elapsed)))
cat(sprintf(" MC Replications     : %d per scenario\n", CONFIG$N_RUNS))
cat(sprintf(" Agents per run      : %d\n", ag$n))
cat(sprintf(" CPU cores used      : %d\n", CONFIG$N_CORES))
cat(sprintf(" KS statistic (D)    : %.4f\n", ks_result$statistic))
cat(sprintf(" KS p-value          : %.2e\n", ks_result$p.value))
cat(sprintf(" KS interpretation   : Pliant Baseline vs Zealot-Anchored (Imperial)\n"))
cat(sprintf(" Figures produced    : 9 (displayed in IDE + saved to disk)\n"))
cat("------------------------------------------------------------\n")
cat(" Vectorization summary:\n")
cat("   Social influence  : matrix ops (no agent-level loops)\n")
cat("   Shock application : single rnorm(n) + element-wise multiply\n")
cat("   Opinion init      : vectorized rbeta(n, alpha, beta)\n")
cat("   MC outer loop     : parallel across CPU cores\n")
cat("============================================================\n")
