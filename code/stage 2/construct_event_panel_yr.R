# ==============================
# Author: Po-Jui Huang
# Date: 31st Mar., 20th, Apr. 2026
# Goal: Collapses the quarter-level event panel to an annual level.
# ==============================
library(arrow)
library(data.table)
library(haven)

setwd("my path")

# ==============================================================
# 1. Load, filter, create variables
# ==============================================================
dt <- as.data.table(read_parquet("temp/event_panel_r/event_panel_all.parquet"))

dt <- dt[quarterly_date > 119 & quarterly_date < 224]
dt[, `:=`(
  j          = as.integer(j),
  emp_ratio   = n_employed / state_pop,
  parttime_ratio   = parttime   / state_pop
)]

dt <- dt[!is.na(emp_ratio)]

dt[, `:=`(
  j_annual = floor(j / 4L),
  year       = 1960L + quarterly_date %/% 4L,
  event_year = 1960L + (quarterly_date - j) %/% 4L
)]

# ==============================================================
# 2. Collapse to annual cell
# ==============================================================
mean_vars  <- c("emp_ratio", paste0("skill_score", 1:6), "parttime_ratio",
                "college", "educ", "female", "age", "black")

first_vars <- c("D", "j_annual", "event_year")

dt_annual <- dt[, c(
  lapply(.SD[, ..mean_vars],  mean, na.rm = TRUE),
  lapply(.SD[, ..first_vars], first)
), by = .(event_id, ind1990, statefips, year)]

setnames(dt_annual, "j_annual", "j")

rm(dt); gc()

# ==============================================================
# 3. Save
# ==============================================================
write_parquet(dt_annual, "temp/event_panel_r/event_panel_all_yr.parquet")
write_dta(dt_annual,     "temp/event_panel_r/event_panel_all_yr.dta", version = 14)