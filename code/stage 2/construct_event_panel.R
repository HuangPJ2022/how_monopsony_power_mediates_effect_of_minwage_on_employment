# ==============================
# Author: Po-Jui Huang (code with Claude together)
# Date: 28th Mar., 20th, Apr. 2026
# Goal: Constructs event panels for each minimum wage increase event
# ==============================
library(arrow)
library(data.table)
library(haven)
library(dplyr)
library(labelled)

ROOT <- "my path"
TEMP <- file.path(ROOT, "temp")
EVENT_DIR <- file.path(TEMP, "event_panel_r") 
dir.create(EVENT_DIR, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 1. Convert .dta to Parquet (my orignial data is massive)
# ============================================================
parquet_path <- file.path(ROOT, "cps_soc2010_merged_onet_all.parquet")

if (!file.exists(parquet_path)) {
  cat("Converting .dta → Parquet (one-time cost) ...\n")
  
  # time clicks
  t0 <- proc.time()
  
  keep_cols <- c(
    "statefips", "quarterly_date", "ind1990",
    "sex", "empstat", "race", "wkstat", "educ",
    "uhrswork1", "age", "wtfinl",
    "skill_score1", "skill_score2", "skill_score3",
    "skill_score4", "skill_score5", "skill_score6"
  )
  
  cps_raw <- read_dta(
    file.path(ROOT, "cps_soc2010_merged_onet_all.dta"),
    col_select = all_of(keep_cols)
  )
  
  # Strip Stata value labels
  cps_raw <- remove_val_labels(cps_raw)
  cps_raw <- zap_labels(cps_raw)
  
  write_parquet(as.data.frame(cps_raw), parquet_path)
  
  elapsed <- (proc.time() - t0)[3]
  cat(sprintf("  Done in %.0fs → %s\n", elapsed, parquet_path))
  rm(cps_raw)
  gc()
  
} else {
  cat("Parquet already exists:", parquet_path, "\n")
}

# ============================================================
# 2. Import data for construction
# ============================================================
events <- as.data.table(read_dta(file.path(TEMP, "policy_state_event.dta"))) # policies
cps_ds <- open_dataset(parquet_path)                                         # original data

# ============================================================
# 3. Loop over events
# ============================================================
panels <- vector("list", n_events)  # pre-allocate list
t_start <- proc.time()

for (i in seq_len(n_events)) {
  
  treated_state <- as.integer(events$statefips[i])
  ev_tq         <- as.integer(events$quarterly_date[i])
  win_start     <- ev_tq - 24L
  win_end       <- ev_tq + 23L
  event_id      <- i
  
  cat(sprintf("Processing event %d/%d: state %d, quarter %d\n",
              i, n_events, treated_state, ev_tq))
  
  # --- Load contaminated states ---
  cont_path <- file.path(
    TEMP, "contaminated_state_by_event",
    sprintf("contaminated_statefips_%d_%d.dta", treated_state, ev_tq)
  )
  cont_states <- as.integer(read_dta(cont_path)$statefips)
  cont_states <- setdiff(cont_states, treated_state)  # keep treated in sample
  
  # --- Filter CPS: window + drop contaminated ---
  panel <- cps_ds |>
    filter(quarterly_date >= win_start, quarterly_date <= win_end) |>
    filter(!(statefips %in% cont_states & statefips != treated_state)) |>
    collect()                     
  
  setDT(panel)                    
  
  # --- Keep variables ---
  panel[, `:=`(
    D              = as.integer(statefips == treated_state),
    j              = quarterly_date - ev_tq,
    event_id       = event_id,
    female         = as.integer(sex == 2L),
    employed       = as.integer(empstat < 20L),
    black          = as.integer(race == 200L),
    parttime       = as.integer(wkstat >= 20L & wkstat <= 40L),
    college        = as.integer(educ >= 80L),
    uhrswork1_clean = fifelse(uhrswork1 == 999L, NA_real_, as.double(uhrswork1))
  )]
  
  # ============================================================
  # Industry-level collapse (ind1990 × state × quarter)
  # ============================================================
  ind_agg <- panel[, .(
    n_employed   = sum(employed * wtfinl, na.rm = TRUE),   # weighted count of employed
    avg_hours    = weighted.mean(uhrswork1_clean, wtfinl, na.rm = TRUE),
    parttime     = weighted.mean(parttime,        wtfinl, na.rm = TRUE),
    skill_score1 = weighted.mean(skill_score1,    wtfinl, na.rm = TRUE),
    skill_score2 = weighted.mean(skill_score2,    wtfinl, na.rm = TRUE),
    skill_score3 = weighted.mean(skill_score3,    wtfinl, na.rm = TRUE),
    skill_score4 = weighted.mean(skill_score4,    wtfinl, na.rm = TRUE),
    skill_score5 = weighted.mean(skill_score5,    wtfinl, na.rm = TRUE),
    skill_score6 = weighted.mean(skill_score6,    wtfinl, na.rm = TRUE),
    tot_weight   = sum(wtfinl, na.rm = TRUE),
    n_obs_ind    = .N
  ), by = .(ind1990, statefips, quarterly_date, D, j, event_id)]
  
  ind_agg[, `:=`(
    ln_n_employed = log(n_employed),
    ln_avg_hours  = log(avg_hours)
  )]
  
  # ============================================================
  # State-level collapse (state × quarter)
  # ============================================================
  state_agg <- panel[, .(
    college     = weighted.mean(college, wtfinl, na.rm = TRUE),
    educ        = weighted.mean(educ,    wtfinl, na.rm = TRUE),
    female      = weighted.mean(female,  wtfinl, na.rm = TRUE),
    age         = weighted.mean(age,     wtfinl, na.rm = TRUE),
    black       = weighted.mean(black,   wtfinl, na.rm = TRUE),
    state_pop   = sum(wtfinl, na.rm = TRUE),    # state population (sum of weights)
    n_obs_state = .N
  ), by = .(statefips, quarterly_date, D, j, event_id)]
  
  # ============================================================
  # Merge state demographics onto industry panel
  # ============================================================
  agg <- merge(
    ind_agg, state_agg,
    by = c("statefips", "quarterly_date", "D", "j", "event_id"),
    all.x = TRUE
  )
  
  panels[[i]] <- agg
  
  # Save individual event panel
  event_file <- file.path(EVENT_DIR,
                          sprintf("event_panel_%d_%d.dta", treated_state, ev_tq))
  write_dta(agg, event_file, version = 14)
}

# ============================================================
# 4. Stack up all panels 
# ============================================================
full_panel <- rbindlist(panels, use.names = TRUE)

write_parquet(full_panel, file.path(EVENT_DIR, "event_panel_all.parquet"))

write_dta(full_panel, file.path(EVENT_DIR, "event_panel_all.dta"), version = 14)

# time clicks
elapsed_total <- (proc.time() - t_start)[3]
cat(sprintf("Total time: %.0fs\n", elapsed_total))