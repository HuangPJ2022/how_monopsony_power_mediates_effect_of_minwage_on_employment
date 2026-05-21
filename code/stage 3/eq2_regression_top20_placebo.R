# ==============================
# Author: Po-Jui Huang (code with Claude together)
# Date: 6th May, 18th May 2026
# Goal: Estimates Model 2 — placebo test(top 20% high wage industries withn each state)
# Sample: events which increased state mw over 10%. + the top 20% high wage industries within each state
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
outdir <- "results/260518_eq2_placebo_top20"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================
# 1. Import data
# ==============================================================
ds <- open_dataset("temp/event_panel_wage_r_noPaidHour/event_panel_wage_all.parquet")
policy_id <- as.data.table(read_stata("temp/policy_id.dta"))
wage <- as.data.table(read_dta("temp/combined_hourwage/average_combined_hwage_ist.dta"))
cpi_state <- as.data.table(
  read_csv("rawdata/price/cpi_state_annual.csv", show_col_types = FALSE)
)

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

dt[, `:=`(
  high_wage20 = as.integer(nq_combined_hwage_ist > 8),
  gr10        = as.integer(gr_mw >= 0.1)
)]

rm(wage, policy_id); gc()

# ==============================================================
# 3. Deflate nominal wage
# ==============================================================
setnames(cpi_state, "cps_fips", "statefips")
base_cpi <- cpi_state[year == 1990, mean(cpi, na.rm = TRUE)]
cpi_state[, cpi_index := cpi / base_cpi * 100]

dt <- merge(dt, cpi_state[, .(statefips, year, cpi_index)],
            by = c("statefips", "year"), all.x = TRUE)

dt[, real_hourwage := avg_hourwage / cpi_index * 100]

rm(cpi_state); gc()

# ==============================================================
# 4. Focus on the top 20% low-wage industries
# ==============================================================
# weighted average of variables of the top 20% low-wage industries(state x quarter)
dt_state <- dt[high_wage20 == 1, {
  w <- tot_weight
  list(
    real_hourwage = sum(real_hourwage * w, na.rm = TRUE) / sum(w[!is.na(real_hourwage)]),
    college       = sum(college * w, na.rm = TRUE) / sum(w[!is.na(college)]),
    female        = sum(female * w, na.rm = TRUE) / sum(w[!is.na(female)]),
    age           = sum(age * w, na.rm = TRUE) / sum(w[!is.na(age)]),
    black         = sum(black * w, na.rm = TRUE) / sum(w[!is.na(black)]),
    tot_weight_st = sum(w, na.rm = TRUE),
    n_obs         = sum(n_obs_ind, na.rm = TRUE)
  )
}, by = .(statefips, quarterly_date, year, event_id, D, j, gr_mw)]

# aggregate into state x year level
dt_state[, j_annual := floor(j / 4L)]

dt_annual <- dt_state[, {
  w <- tot_weight_st
  list(
    real_hourwage = sum(real_hourwage * w, na.rm = TRUE) / sum(w[!is.na(real_hourwage)]),
    college       = sum(college * w, na.rm = TRUE) / sum(w[!is.na(college)]),
    female        = sum(female * w, na.rm = TRUE) / sum(w[!is.na(female)]),
    age           = sum(age * w, na.rm = TRUE) / sum(w[!is.na(age)]),
    black         = sum(black * w, na.rm = TRUE) / sum(w[!is.na(black)]),
    tot_weight_yr = sum(w, na.rm = TRUE),
    n_obs_yr      = sum(n_obs, na.rm = TRUE)
  )
}, by = .(statefips, year, event_id, D, j_annual, gr_mw)]

dt_annual[, `:=`(
  ln_real_hourwage = log(real_hourwage),
  gr10 = as.integer(gr_mw >= 0.1)
)]

# ==============================================================
# 5. Regression
# ==============================================================
est <- feols(
  ln_real_hourwage ~
    i(j_annual, D, ref = -1) +
    college + female + age + black |
    statefips + year,
  data = dt_annual[gr10 == 1 & j_annual %between% c(-3, 4)],
  cluster = ~statefips
)

# ==============================================================
# 6. Export results
# ==============================================================
cn <- names(coef(est))
nice <- cn |>
  gsub("j_annual::", "j=", x = _) |>
  gsub(":D", " × D", x = _)
cmap <- setNames(nice, cn)

modelsummary(est,
             output   = file.path(outdir, "eq_table.tex"),
             fmt      = 4,
             coef_map = cmap,
             stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             title    = "Placebo: Top 20% Wage Industries")

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
for (jj in -3:4) {
  cname <- paste0("j_annual::", jj, ":D")
  gc <- get_coef(cname)
  if (is.na(gc["b"])) gc <- c(b = 0, lo = 0, hi = 0)
  rows[[length(rows) + 1]] <- c(j = jj, gc)
}

df_coef <- as.data.table(do.call(rbind, rows))
setnames(df_coef, c("j", "b", "lo", "hi"))

knitr::kable(df_coef, format = "latex", booktabs = TRUE, digits = 4) |>
  writeLines(file.path(outdir, "coef.tex"))

# ==============================================================
# 7. Plot
# ==============================================================
p <- ggplot(df_coef, aes(x = j)) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                width = 0.2, color = alpha("navy", 0.5), linewidth = 0.4) +
  geom_line(aes(y = b), color = "navy") +
  geom_point(aes(y = b), color = "navy", shape = 16, size = 2.5) +
  geom_vline(xintercept = -0.5, color = "red", linewidth = 0.3) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  scale_x_continuous(breaks = -3:4) +
  labs(x = "Years relative to MW hike",
       y = "Effect on log average real wage",
       title = "Placebo: the Wage Effect of the Minimum Wage Increases \n — Top 20% High-Wage Industries") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(file.path(outdir, "event_study.png"), p,
       width = 8, height = 5, dpi = 300)
