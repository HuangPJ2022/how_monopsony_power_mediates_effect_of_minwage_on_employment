# ==============================
# Author: Po-Jui Huang (code with Claude together)
# Date: 20th, Apr., 21th, Apr. 2026
# Goal: Constructs the event panel with wage variables appended(essentially identical to previous files)
# ==============================

library(arrow)
library(data.table)
library(haven)
library(dplyr)
library(labelled)

ROOT <- "my path"
TEMP <- file.path(ROOT, "temp")
EVENT_DIR <- file.path(TEMP, "event_panel_wage_r_noPaidHour")
dir.create(EVENT_DIR, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 1. Convert .dta to Parquet (my orignial data is massive)
# ============================================================
parquet_path <- file.path(ROOT, "cps_wage.parquet")

if (!file.exists(parquet_path)) {
  cat("Converting .dta → Parquet (wage version) ...\n")
  t0 <- proc.time()
  
  keep_cols <- c(
    "statefips", "quarterly_date", "ind1990",
    "sex", "empstat", "race", "wkstat", "educ",
    "uhrswork1", "age", "wtfinl",
    "hourwage", "paidhour", "qhourwag",
    "skill_score1", "skill_score2", "skill_score3",
    "skill_score4", "skill_score5", "skill_score6"
  )
  
  cps_raw <- read_dta(
    file.path(ROOT, "cps_soc2010_merged_onet_all.dta"),
    col_select = all_of(keep_cols)
  )
  
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
panels <- vector("list", n_events)

# time clicks
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
  cont_states <- setdiff(cont_states, treated_state)
  
  # --- Filter CPS: window + drop contaminated ---
  panel <- cps_ds |>
    filter(quarterly_date >= win_start, quarterly_date <= win_end) |>
    filter(!(statefips %in% cont_states & statefips != treated_state)) |>
    collect()
  
  setDT(panel)
  
  # --- Sample selection: hourly workers with valid wage ---
  panel <- panel[qhourwag == 0]
  
  # --- Clean hourwage ---
  panel[, hourwage_clean := as.double(hourwage)]
  panel[hourwage_clean <= 0, hourwage_clean := NA_real_]
  
  # --- Keep variables ---
  panel[, `:=`(
    D               = as.integer(statefips == treated_state),
    j               = quarterly_date - ev_tq,
    event_id        = event_id,
    female          = as.integer(sex == 2L),
    employed        = as.integer(empstat < 20L),
    black           = as.integer(race == 200L),
    parttime        = as.integer(wkstat >= 20L & wkstat <= 40L),
    college         = as.integer(educ >= 80L),
    uhrswork1_clean = fifelse(uhrswork1 == 999L, NA_real_, as.double(uhrswork1)),
    paidhour        = paidhour
  )]
  
  # ============================================================
  # Industry-level collapse (ind1990 × state × quarter)
  # ============================================================
  ind_agg <- panel[, .(
    avg_hourwage = weighted.mean(hourwage_clean, wtfinl, na.rm = TRUE),
    n_employed   = sum(employed * wtfinl, na.rm = TRUE),
    avg_hours    = weighted.mean(uhrswork1_clean, wtfinl, na.rm = TRUE),
    parttime     = weighted.mean(parttime,        wtfinl, na.rm = TRUE),
    paidhour     = weighted.mean(paidhour,        wtfinl, na.rm = TRUE),
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
    ln_avg_hourwage = log(avg_hourwage),
    ln_n_employed   = log(n_employed),
    ln_avg_hours    = log(avg_hours)
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
    state_pop   = sum(wtfinl, na.rm = TRUE),
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

write_parquet(full_panel, file.path(EVENT_DIR, "event_panel_wage_all.parquet"))

write_dta(full_panel, file.path(EVENT_DIR, "event_panel_wage_all.dta"), version = 14)

rm(panels); gc()

# ============================================================
# 5. Collapse to annual level data
# ============================================================
dt <- full_panel

dt <- dt[quarterly_date > 119 & quarterly_date < 224]
dt[, `:=`(
  j        = as.integer(j),
  emp_ratio = n_employed / state_pop,
  parttime_ratio   = parttime   / state_pop,
  paidhour_ratio   = paidhour   / state_pop
)]
dt <- dt[!is.na(emp_ratio)]

dt[, `:=`(
  j_annual = floor(j / 4L),
  year       = 1960L + quarterly_date %/% 4L,
  event_year = 1960L + (quarterly_date - j) %/% 4L
)]

# --- Collapse to annual ---
mean_vars  <- c("emp_ratio", "avg_hourwage", "ln_avg_hourwage",
                paste0("skill_score", 1:6), "parttime_ratio", "paidhour_ratio",
                "college", "educ", "female", "age", "black")

first_vars <- c("D", "j_annual", "event_year")

dt_annual <- dt[, c(
  lapply(.SD[, ..mean_vars],  mean, na.rm = TRUE),
  lapply(.SD[, ..first_vars], first)
), by = .(event_id, ind1990, statefips, year)]

setnames(dt_annual, "j_annual", "j")

rm(dt); gc()

# ============================================================
# 6. Save annual panel
# ============================================================
write_parquet(dt_annual, file.path(EVENT_DIR, "event_panel_wage_all_yr.parquet"))
write_dta(dt_annual,     file.path(EVENT_DIR, "event_panel_wage_all_yr.dta"), version = 14)

# time clicks
elapsed_total <- (proc.time() - t_start)[3]
cat(sprintf("Total time: %.0fs\n", elapsed_total))