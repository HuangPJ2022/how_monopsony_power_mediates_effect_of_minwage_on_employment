# ==============================
# Author: Po-Jui Huang (code with Claude together)
# Date: 6th May, 8th May, 18th May 2026
# Goal: Estimates Model 2 — the effect of minimum wage increases on log average real wage
# Sample: events which increased state mw over 10%. + the bottom 20% low wage industries within each state
# ==============================

library(arrow)
library(dplyr)
library(haven)
library(data.table)
library(fixest)
library(ggplot2)
library(modelsummary)
library(car)
library(readr)

setwd("my path")
outdir <- "results/260518_eq2_reg"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================
# 1. Import data
# ==============================================================
ds <- open_dataset("temp/event_panel_wage_r_noPaidHour/event_panel_wage_all.parquet")
policy_id <- as.data.table(read_stata("temp/policy_id.dta"))
wage <- as.data.table(read_dta("temp/combined_hourwage/average_combined_hwage_ist.dta"))
cpi_state <- as.data.table(
  read_csv("rawdata/price/cpi_state_annual.csv",
           show_col_types = FALSE)
)
mono <- as.data.table(read_dta("temp/newRecruit_rate_yqt.dta"))

# ==============================================================
# 2. Select event time and merge data
# ==============================================================
dt <- ds |>
  filter(quarterly_date > 119, quarterly_date < 248) |>
  filter(ind1990 > 0, ind1990 < 940) |>
  select(event_id, ind1990, statefips, quarterly_date, j, D,
         avg_hourwage, tot_weight, n_obs_ind,
         college, female, age, black) |>
  collect()

setDT(dt)

dt[, `:=`(
  j          = as.integer(j),
  year       = 1960L + quarterly_date %/% 4L,
  event_year = 1960L + (quarterly_date - j) %/% 4L
)]

dt <- merge(dt, policy_id[, .(event_id, gr_mw)],
            by = "event_id", all.x = TRUE)

dt <- merge(dt, wage[, .(ind1990, statefips, year,
                         nq4_combined_hwage_ist, nq_combined_hwage_ist)],
            by.x = c("ind1990", "statefips", "event_year"),
            by.y = c("ind1990", "statefips", "year"),
            all.x = TRUE)

rm(wage, policy_id); gc()

# ==============================================================
# 3. Construct leave-one-out monopsony measure
# ==============================================================
mono_ind_total <- mono[, .(
  wsum_total = sum(newRecruit_rate * wtfinl, na.rm = TRUE),
  w_total    = sum(wtfinl, na.rm = TRUE),
  cell_total = sum(cell_size, na.rm = TRUE)
), by = .(ind1990)]

mono_own <- mono[, .(
  wsum_own = sum(newRecruit_rate * wtfinl, na.rm = TRUE),
  w_own    = sum(wtfinl, na.rm = TRUE),
  cell_own = sum(cell_size, na.rm = TRUE)
), by = .(ind1990, statefips)]

mono_ind_st <- merge(mono_own, mono_ind_total, by = "ind1990")

mono_ind_st[, `:=`(
  newRecruit_rate_loo = (wsum_total - wsum_own) / (w_total - w_own),
  cell_size_loo       = cell_total - cell_own
)]

mono_ind_st[, c("wsum_total", "w_total", "cell_total",
                "wsum_own", "w_own", "cell_own") := NULL]

mono_ind_st[, nq4_monopsony_loo := cut(newRecruit_rate_loo,
                                       breaks = quantile(newRecruit_rate_loo, probs = 0:4/4, na.rm = TRUE),
                                       labels = 1:4, include.lowest = TRUE)]
mono_ind_st[, nq4_monopsony_loo := as.integer(nq4_monopsony_loo)]

mono_ind_st[, nq_monopsony_loo := cut(newRecruit_rate_loo,
                                      breaks = quantile(newRecruit_rate_loo, probs = 0:10/10, na.rm = TRUE),
                                      labels = 1:10, include.lowest = TRUE)]
mono_ind_st[, nq_monopsony_loo := as.integer(nq_monopsony_loo)]

# ==============================================================
# 4. Merge monopsony and deflate nominal wage
# ==============================================================
dt <- merge(dt, unique(mono_ind_st[, .(ind1990, statefips,
                                       newRecruit_rate_loo, nq4_monopsony_loo,
                                       nq_monopsony_loo, cell_size_loo)]),
            by = c("ind1990", "statefips"), all.x = TRUE)

rm(mono, mono_own, mono_ind_total, mono_ind_st); gc()

setnames(cpi_state, "cps_fips", "statefips")
base_cpi <- cpi_state[year == 1990, mean(cpi, na.rm = TRUE)]
cpi_state[, cpi_index := cpi / base_cpi * 100]

dt <- merge(dt,
            cpi_state[, .(statefips, year, cpi_index)],
            by = c("statefips", "year"),
            all.x = TRUE)

dt[, real_hourwage := avg_hourwage / cpi_index * 100]

rm(cpi_state); gc()

# ==============================================================
# 5. Generate variables
# ==============================================================
dt[, `:=`(
  high_mono10 = as.integer(nq_monopsony_loo > 9),
  low_wage20  = as.integer(nq_combined_hwage_ist < 3),
  gr10        = as.integer(gr_mw >= 0.1)
)]

pre_j  <- c(-3, -2)        # pre treatment (baseline is -1)
post_j <- c(0, 1, 2, 3, 4) # post treatment


# Baseline employment share: industry's fraction of state employment
# averaged over the pre-treatment period
state_emp <- dt[j < 0L,
                .(state_total = sum(tot_weight, na.rm = TRUE)),
                by = .(statefips, event_id)]

ind_emp <- dt[j < 0L,
              .(ind_total = sum(tot_weight, na.rm = TRUE)),
              by = .(ind1990, statefips, event_id)]

ind_emp <- merge(ind_emp, state_emp, by = c("statefips", "event_id"))
ind_emp[, emp_share := ind_total / state_total]

dt <- merge(dt, ind_emp[, .(ind1990, statefips, event_id, emp_share)],
            by = c("ind1990", "statefips", "event_id"), all.x = TRUE)

rm(state_emp, ind_emp); gc()

# ==============================================================
# 6. Two-step aggregation to state x year level
#    (replicates original eq2 aggregation exactly)
# ==============================================================
aggregate_to_state_year <- function(dt_sub) {
  # Step 1: industry x state x quarter -> state x quarter
  dt_state <- dt_sub[, {
    w <- agg_wt
    list(
      avg_hourwage  = sum(avg_hourwage * w, na.rm = TRUE) / sum(w[!is.na(avg_hourwage)]),
      real_hourwage = sum(real_hourwage * w, na.rm = TRUE) / sum(w[!is.na(real_hourwage)]),
      college       = sum(college * w, na.rm = TRUE) / sum(w[!is.na(college)]),
      female        = sum(female * w, na.rm = TRUE) / sum(w[!is.na(female)]),
      age           = sum(age * w, na.rm = TRUE) / sum(w[!is.na(age)]),
      black         = sum(black * w, na.rm = TRUE) / sum(w[!is.na(black)]),
      tot_weight_st = sum(w, na.rm = TRUE),
      n_obs         = sum(n_obs_ind, na.rm = TRUE)
    )
  }, by = .(statefips, quarterly_date, year, event_id, D, j, gr_mw)]
  
  # Step 2: state x quarter -> state x year
  dt_state[, j_annual := floor(j / 4L)]
  
  dt_yr <- dt_state[, {
    w <- tot_weight_st
    list(
      avg_hourwage  = sum(avg_hourwage * w, na.rm = TRUE) / sum(w[!is.na(avg_hourwage)]),
      real_hourwage = sum(real_hourwage * w, na.rm = TRUE) / sum(w[!is.na(real_hourwage)]),
      college       = sum(college * w, na.rm = TRUE) / sum(w[!is.na(college)]),
      female        = sum(female * w, na.rm = TRUE) / sum(w[!is.na(female)]),
      age           = sum(age * w, na.rm = TRUE) / sum(w[!is.na(age)]),
      black         = sum(black * w, na.rm = TRUE) / sum(w[!is.na(black)]),
      tot_weight_yr = sum(w, na.rm = TRUE),
      n_obs_yr      = sum(n_obs, na.rm = TRUE)
    )
  }, by = .(statefips, year, event_id, D, j_annual, gr_mw)]
  
  dt_yr[, `:=`(
    ln_real_hourwage = log(real_hourwage),
    gr10 = as.integer(gr_mw >= 0.1)
  )]
  dt_yr
}

dt[, agg_wt := tot_weight]
dt_gen <- aggregate_to_state_year(dt[low_wage20 == 1])

dt[, agg_wt := emp_share]
dt_high    <- aggregate_to_state_year(dt[low_wage20 == 1 & high_mono10 == 1])
dt_nonhigh <- aggregate_to_state_year(dt[low_wage20 == 1 & high_mono10 == 0])

rm(dt); gc()

# ==============================================================
# 7. Run eq2 regression on each sample
# ==============================================================
run_eq2 <- function(dt_st) {
  feols(
    ln_real_hourwage ~
      i(j_annual, D, ref = -1) +
      college + female + age + black |
      statefips + year,
    data = dt_st[gr10 == 1 & j_annual %between% c(-3, 4)],
    cluster = ~statefips
  )
}

est_gen     <- run_eq2(dt_gen)
est_high    <- run_eq2(dt_high)
est_nonhigh <- run_eq2(dt_nonhigh)

# ==============================================================
# 8. Export regression tables
# ==============================================================
export_table <- function(est, filename, title) {
  cn <- names(coef(est))
  nice <- cn |>
    gsub("j_annual::", "j=", x = _) |>
    gsub(":D", " × D", x = _)
  cmap <- setNames(nice, cn)
  
  modelsummary(est,
               output   = file.path(outdir, filename),
               fmt      = 4,
               coef_map = cmap,
               stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
               title    = title)
}

export_table(est_gen,     "eq2_table_general.tex",     "Equation (2) — General Effect")
export_table(est_high,    "eq2_table_highmono.tex",    "Equation (2) — High Monopsony")
export_table(est_nonhigh, "eq2_table_nonhighmono.tex", "Equation (2) — Non-high Monopsony")

# ==============================================================
# 9. Extract coefficients
# ==============================================================
extract_coefs <- function(est) {
  cc <- coeftable(est)
  get_coef <- function(name) {
    if (name %in% rownames(cc)) {
      b  <- cc[name, "Estimate"]
      se <- cc[name, "Std. Error"]
      return(c(b = b, lo = b - 1.96 * se, hi = b + 1.96 * se))
    }
    return(c(b = NA_real_, lo = NA_real_, hi = NA_real_))
  }
  
  rows <- list()
  for (jj in pre_j) {
    rows[[length(rows) + 1]] <- c(j = jj, get_coef(paste0("j_annual::", jj, ":D")))
  }
  rows[[length(rows) + 1]] <- c(j = -1, b = 0, lo = 0, hi = 0)
  for (jj in post_j) {
    rows[[length(rows) + 1]] <- c(j = jj, get_coef(paste0("j_annual::", jj, ":D")))
  }
  
  as.data.table(do.call(rbind, rows))
}

df_gen     <- extract_coefs(est_gen)
df_high    <- extract_coefs(est_high)
df_nonhigh <- extract_coefs(est_nonhigh)

setnames(df_gen,     c("j", "b_gen",     "lo_gen",     "hi_gen"))
setnames(df_high,    c("j", "b_high",    "lo_high",    "hi_high"))
setnames(df_nonhigh, c("j", "b_nonhigh", "lo_nonhigh", "hi_nonhigh"))

knitr::kable(df_gen, format = "latex", booktabs = TRUE, digits = 8,
             caption = "General effect") |>
  writeLines(file.path(outdir, "coef_general.tex"))

knitr::kable(df_high, format = "latex", booktabs = TRUE, digits = 8,
             caption = "High monopsony") |>
  writeLines(file.path(outdir, "coef_highmono.tex"))

knitr::kable(df_nonhigh, format = "latex", booktabs = TRUE, digits = 8,
             caption = "Non-high monopsony") |>
  writeLines(file.path(outdir, "coef_nonhighmono.tex"))

# ==============================================================
# 10. Pre-trend Tests
# ==============================================================
sink(file.path(outdir, "pretrend_hypo_tests.txt"))

cat("==========================================\n")
cat("GENERAL SAMPLE\n")
cat("==========================================\n")
pre_coefs <- paste0("j_annual::", pre_j, ":D")
pre_coefs_g <- pre_coefs[pre_coefs %in% names(coef(est_gen))]
if (length(pre_coefs_g) > 0) print(wald(est_gen, pre_coefs_g))

cat("\n==========================================\n")
cat("HIGH MONOPSONY SAMPLE\n")
cat("==========================================\n")
pre_coefs_h <- pre_coefs[pre_coefs %in% names(coef(est_high))]
if (length(pre_coefs_h) > 0) print(wald(est_high, pre_coefs_h))

cat("\n==========================================\n")
cat("NON-HIGH MONOPSONY SAMPLE\n")
cat("==========================================\n")
pre_coefs_n <- pre_coefs[pre_coefs %in% names(coef(est_nonhigh))]
if (length(pre_coefs_n) > 0) print(wald(est_nonhigh, pre_coefs_n))

sink()

# ==============================================================
# 11. Plot
# ==============================================================
# --- Plot: general regression only ---
p_gen <- ggplot(df_gen, aes(x = j)) +
  geom_errorbar(aes(ymin = lo_gen, ymax = hi_gen),
                width = 0.2, color = alpha("navy", 0.5), linewidth = 0.4) +
  geom_line(aes(y = b_gen), color = "navy") +
  geom_point(aes(y = b_gen), color = "navy", shape = 16, size = 2.5) +
  geom_vline(xintercept = -0.5, color = "red", linewidth = 0.3) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  scale_x_continuous(breaks = -3:4) +
  labs(x = "Years relative to MW hike",
       y = "Effect on log average real wage",
       title = "The Wage Effect of the Minimum Wage Increases") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(file.path(outdir, "event_study_real_wage_general.png"), p_gen,
       width = 8, height = 5, dpi = 300)

# --- Plot: three trends ---
df_three <- merge(df_gen, merge(df_nonhigh, df_high, by = "j"), by = "j")

p3 <- ggplot(df_three, aes(x = j)) +
  # General (dark red)
  geom_errorbar(aes(ymin = lo_gen, ymax = hi_gen),
                width = 0.2, color = alpha("darkred", 0.4), linewidth = 0.4) +
  geom_line(aes(y = b_gen, color = "General"), linetype = "dotted") +
  geom_point(aes(y = b_gen, color = "General"), shape = 17, size = 2.5) +
  # Non-high mono (gray)
  geom_errorbar(aes(ymin = lo_nonhigh, ymax = hi_nonhigh),
                width = 0.2, color = "gray70", linewidth = 0.4) +
  geom_line(aes(y = b_nonhigh, color = "Non-high monopsony"), linetype = "dashed") +
  geom_point(aes(y = b_nonhigh, color = "Non-high monopsony"), shape = 1, size = 2.5) +
  # High mono (navy)
  geom_errorbar(aes(ymin = lo_high, ymax = hi_high),
                width = 0.2, color = alpha("navy", 0.4), linewidth = 0.4) +
  geom_line(aes(y = b_high, color = "High monopsony")) +
  geom_point(aes(y = b_high, color = "High monopsony"), shape = 16, size = 2.5) +
  scale_color_manual(values = c("General" = "darkred",
                                "Non-high monopsony" = "gray60",
                                "High monopsony" = "navy")) +
  geom_vline(xintercept = -0.5, color = "red", linewidth = 0.3) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  scale_x_continuous(breaks = -3:4) +
  labs(x = "Years relative to MW hike",
       y = "Effect on log average real wage",
       color = NULL,
       title = "The Wage Effect of the Minimum Wage Increases by Monopsony Power") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

ggsave(file.path(outdir, "event_study_real_wage.png"), p3,
       width = 8, height = 5, dpi = 300)

# --- Plot: two trends (high vs non-high) ---
df_plot <- merge(df_nonhigh, df_high, by = "j")

p <- ggplot(df_plot, aes(x = j)) +
  geom_errorbar(aes(ymin = lo_nonhigh, ymax = hi_nonhigh),
                width = 0.2, color = "gray70", linewidth = 0.4) +
  geom_errorbar(aes(ymin = lo_high, ymax = hi_high),
                width = 0.2, color = alpha("navy", 0.5), linewidth = 0.4) +
  geom_line(aes(y = b_nonhigh, color = "Non-high monopsony"), linetype = "dashed") +
  geom_point(aes(y = b_nonhigh, color = "Non-high monopsony"), shape = 1, size = 2.5) +
  geom_line(aes(y = b_high, color = "High monopsony")) +
  geom_point(aes(y = b_high, color = "High monopsony"), shape = 16, size = 2.5) +
  scale_color_manual(values = c("High monopsony" = "navy",
                                "Non-high monopsony" = "gray60")) +
  geom_vline(xintercept = -0.5, color = "red", linewidth = 0.3) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  scale_x_continuous(breaks = -3:4) +
  labs(x = "Years relative to MW hike",
       y = "Effect on log average real wage",
       color = NULL,
       title = "The Wage Effect of the Minimum Wage Increases by Monopsony Power") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

ggsave(file.path(outdir, "event_study_by_monopsony.png"), p,
       width = 8, height = 5, dpi = 300)