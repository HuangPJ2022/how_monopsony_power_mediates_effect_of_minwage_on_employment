# ==============================
# Author: Po-Jui Huang (code with Claude together)
# Date: 7th May 2026
# Goal: compute summary stats
# ==============================

library(arrow)
library(haven)
library(data.table)
library(fixest)
library(ggplot2)
library(modelsummary)
library(car)

setwd("my path")
outdir <- "results/260507_summary_stats"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================
# 1. Define a function to combine regression coef and vcov
# ==============================================================
lincom <- function(est, coefs, signs = rep(1, length(coefs))) {
  b <- coef(est)
  V <- vcov(est)
  missing <- coefs[!coefs %in% names(b)]
  if (length(missing) > 0) stop("Not found: ", paste(missing, collapse = ", ")) # pause if missing
  val <- sum(signs * b[coefs])
  se  <- sqrt(as.numeric(t(signs) %*% V[coefs, coefs] %*% signs))
  list(estimate = val, se = se)
}


# ==============================================================
# 2. Import data
# ==============================================================
dt <- as.data.table(read_parquet("temp/event_panel_r/event_panel_all_yr.parquet"))       # event panel
mono <- as.data.table(read_dta("temp/newRecruit_rate_yqt.dta"))                          # monopsony power
ind_detail <- as.data.table(read_dta("rawdata/ind1990_details.dta"))                     # industry
wage <- as.data.table(read_dta("temp/combined_hourwage/average_combined_hwage_ist.dta")) # wage
policy_id <- as.data.table(read_stata("temp/policy_id.dta"))                             # min wage policy

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

mono_ind_st <- merge(mono_ind_st, ind_detail[, .(ind1990, sec1990, cat1990)],
                     by = "ind1990", all.x = TRUE)

# Industry-level (non-LOO) monopsony
mono_ind <- mono[, .(
  newRecruit_rate_ind = weighted.mean(newRecruit_rate, wtfinl, na.rm = TRUE)
), by = .(ind1990)]

# ==============================================================
# 4. Merge and select data
# ==============================================================
dt <- merge(dt, mono_ind, by = "ind1990", all.x = TRUE)

dt <- merge(dt, policy_id[, .(event_id, gr_mw)],
            by = "event_id", all.x = TRUE)

dt <- merge(dt, mono_ind_st[, .(ind1990, statefips, sec1990, cat1990,
                                newRecruit_rate_loo, nq4_monopsony_loo,
                                nq_monopsony_loo, cell_size_loo)],
            by = c("ind1990", "statefips"), all.x = TRUE)

dt <- merge(dt, wage[, .(ind1990, statefips, year,
                         combined_hourwage_ist, hwage_cell_size_ist,
                         nq4_combined_hwage_ist, nq_combined_hwage_ist)],
            by.x = c("ind1990", "statefips", "event_year"),
            by.y = c("ind1990", "statefips", "year"),
            all.x = TRUE)

dt_annual <- dt[ind1990 != 0 & ind1990 < 940]

rm(dt, mono, mono_own, mono_ind_total, mono_ind_st, wage, policy_id, ind_detail); gc()

# ==============================================================
# 5. Generate variables and pre-allocate space
# ==============================================================
dt_annual[, `:=`(
  high_mono10 = as.integer(nq_monopsony_loo > 9),
  low_wage20  = as.integer(nq_combined_hwage_ist < 3),
  gr10        = as.integer(gr_mw >= 0.1)
)]


# ==============================================================
# 6. Summary Statistics
# ==============================================================
sum_vars <- c("emp_ratio", "college", "educ", "female", "age", "black",
              "skill_score1", "skill_score2", "skill_score3",
              "skill_score4", "skill_score5", "skill_score6",
              "combined_hourwage_ist",
              "newRecruit_rate_ind",
              "newRecruit_rate_loo",
              "cell_size_loo")

var_labels <- c("emp_ratio", "college", "educ", "female", "age", "black",
                "Non-routine cognitive: Analytical",
                "Non-routine cognitive: Interpersonal",
                "Routine cognitive",
                "Routine manual",
                "Non-routine manual physical",
                "Offshorability",
                "Hourly wage (combined)",
                "Monopsony power",
                "Monopsony power (Leave-one-out)",
                "cell_size_loo")

# define function
compute_stats <- function(dt_sub, vars) {
  rows <- lapply(seq_along(vars), function(k) {
    x <- dt_sub[[vars[k]]]
    data.table(
      Variable = var_labels[k],
      N        = sum(!is.na(x)),
      Mean     = mean(x, na.rm = TRUE),
      SD       = sd(x, na.rm = TRUE),
      Min      = min(x, na.rm = TRUE),
      P25      = quantile(x, 0.25, na.rm = TRUE),
      P50      = median(x, na.rm = TRUE),
      P75      = quantile(x, 0.75, na.rm = TRUE),
      Max      = max(x, na.rm = TRUE)
    )
  })
  rbindlist(rows)
}

# Regression sample
dt_reg <- dt_annual[low_wage20 == 1 & gr10 == 1 & j %between% c(-3, 4)]

# Panel A: Full sample ---
stats_full <- compute_stats(dt_reg, sum_vars)

# Panel B: High monopsony ---
stats_high <- compute_stats(dt_reg[high_mono10 == 1], sum_vars)

# Panel C: Non-high monopsony ---
stats_low <- compute_stats(dt_reg[high_mono10 == 0], sum_vars)

# --- Print ---
cat("\n===== Panel A: Full sample =====\n")
print(stats_full)
cat("\n===== Panel B: High monopsony =====\n")
print(stats_high)
cat("\n===== Panel C: Non-high monopsony =====\n")
print(stats_low)

# --- Export to LaTeX ---
knitr::kable(stats_full, format = "latex", booktabs = TRUE, digits = 4,
             caption = "Panel A: Full sample") |>
  writeLines(file.path(outdir, "summary_stats_full.tex"))

knitr::kable(stats_high, format = "latex", booktabs = TRUE, digits = 4,
             caption = "Panel B: High monopsony") |>
  writeLines(file.path(outdir, "summary_stats_high_mono.tex"))

knitr::kable(stats_low, format = "latex", booktabs = TRUE, digits = 4,
             caption = "Panel C: Non-high monopsony") |>
  writeLines(file.path(outdir, "summary_stats_low_mono.tex"))

# --- Combined: side by side ---
stats_high2 <- compute_stats(dt_reg[high_mono10 == 1], sum_vars)
stats_low2  <- compute_stats(dt_reg[high_mono10 == 0], sum_vars)

setnames(stats_high2, c("N", "Mean", "SD", "P50"),
         c("N_high", "Mean_high", "SD_high", "P50_high"))
setnames(stats_low2,  c("N", "Mean", "SD", "P50"),
         c("N_low", "Mean_low", "SD_low", "P50_low"))

stats_comp <- merge(
  stats_high2[, .(Variable, N_high, Mean_high, SD_high, P50_high)],
  stats_low2[, .(Variable, N_low, Mean_low, SD_low, P50_low)],
  by = "Variable"
)

knitr::kable(stats_comp, format = "latex", booktabs = TRUE, digits = 4,
             caption = "Summary Statistics: High vs Non-high Monopsony") |>
  writeLines(file.path(outdir, "summary_stats_comparison.tex"))

cat("\n===== Comparison =====\n")
print(stats_comp)