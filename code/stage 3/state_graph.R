# ==============================
# Author: Po-Jui Huang (code with Claude together)
# Date: 21st Apr., 6th May, 7th May 2026
# Goal: Generates plots about monopsony power or minimum wage policy changes over time by state.
# ==============================

library(statebins)
library(ggplot2)
library(data.table)

setwd("my path")
outdir <- "results/260506_state_graph"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================
# 1. state monopsony power map
# ==============================================================
mono <- as.data.table(read_dta("temp/newRecruit_rate_yqt.dta"))

# Collapse to state level
state_mono <- mono[, .(
  newRecruit_rate = weighted.mean(newRecruit_rate, wtfinl, na.rm = TRUE)
), by = .(statefips)]

# Strip haven labels
state_mono[, statefips := as.integer(statefips)]

# FIPS → state name lookup
fips_to_name <- data.table(
  statefips = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
                30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56),
  state = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut",
            "Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois",
            "Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts",
            "Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada",
            "New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota",
            "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
            "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington",
            "West Virginia","Wisconsin","Wyoming")
)

state_mono <- merge(state_mono, fips_to_name, by = "statefips")

p <- statebins(state_mono,
               state_col = "state",
               value_col = "newRecruit_rate",
               palette = "PiYG",
               direction = -1) +
  theme_statebins() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_distiller(palette = "PiYG", direction = -1,
                       breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "Monopsony Power by State",
       fill  = "Fraction of New Recruitment\n from the Non-employed")

ggsave(file.path(outdir, "statebins_monopsony.png"), p, width = 8, height = 5, dpi = 300)

# ==============================================================
# 2. state policy change 
# ==============================================================
policy_id <- as.data.table(read_stata("temp/policy_id.dta"))

# Convert quarterly_date to year
policy_id[, year := 1960L + quarterly_date %/% 4L]
policy_id <- policy_id[year >= 1990 & year <= 2016]

# Classify by growth rate
policy_id[, change_cat := fcase(
  gr_mw > 0.10, ">10%",
  gr_mw > 0.05, "5–10%",
  gr_mw > 0,    "<5%",
  default = NA_character_
)]

# All states × all years grid
all_states <- data.table(
  stateabb = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL",
               "GA","HI","ID","IL","IN","IA","KS","KY","LA","ME",
               "MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
               "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
               "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
)

all_years <- data.table(year = 1990:2016)
grid <- CJ(stateabb = all_states$stateabb, year = all_years$year)

# Merge events onto grid
policy_plot <- merge(grid, policy_id[, .(stateabb, year, change_cat)],
                     by = c("stateabb", "year"), all.x = TRUE)

# Order states: most events at top, no-change states at bottom
state_order <- policy_plot[!is.na(change_cat), .N, by = stateabb][order(N)]$stateabb
no_change <- setdiff(all_states$stateabb, state_order)

policy_plot[, stateabb := factor(stateabb, levels = c(no_change, state_order))]

p <- ggplot(policy_plot, aes(x = year, y = stateabb)) +
  geom_tile(aes(fill = change_cat), color = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = c("<5%" = "#fcc5c0", "5–10%" = "#f768a1", ">10%" = "#7a0177"),
    na.value = "grey95"
  ) +
  scale_x_continuous(breaks = seq(1990, 2016, by = 2)) +
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 10, family = "mono", color = "black"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  labs(x = NULL, y = NULL, fill = "MW increase",
       title = "State Minimum Wage Changes, 1990–2016")

ggsave(file.path(outdir, "mw_heatmap.png"), p, width = 10, height = 8, dpi = 300)

# ==============================================================
# 3. overlaps between high mono and low wage
# ==============================================================
wage <- as.data.table(read_dta("temp/combined_hourwage/average_combined_hwage_ist.dta"))

# --- Leave one out(LOO) monopsony ---
mono_total <- mono[, .(
  wsum_total = sum(newRecruit_rate * wtfinl, na.rm = TRUE),
  w_total    = sum(wtfinl, na.rm = TRUE)
), by = .(ind1990)]

mono_own <- mono[, .(
  wsum_own = sum(newRecruit_rate * wtfinl, na.rm = TRUE),
  w_own    = sum(wtfinl, na.rm = TRUE)
), by = .(ind1990, statefips)]

mono_loo <- merge(mono_own, mono_total, by = "ind1990")
mono_loo[, newRecruit_rate_loo := (wsum_total - wsum_own) / (w_total - w_own)]
mono_loo[, c("wsum_total", "w_total", "wsum_own", "w_own") := NULL]

mono_loo[, nq_monopsony_loo := cut(newRecruit_rate_loo,
                                   breaks = quantile(newRecruit_rate_loo, probs = 0:10/10, na.rm = TRUE),
                                   labels = 1:10, include.lowest = TRUE)]
mono_loo[, nq_monopsony_loo := as.integer(nq_monopsony_loo)]
mono_loo[, high_mono := as.integer(nq_monopsony_loo > 9)]

# --- Merge wage + monopsony ---
overlap <- merge(wage, mono_loo[, .(ind1990, statefips, high_mono, nq_monopsony_loo)],
                 by = c("ind1990", "statefips"), all.x = TRUE)

# --- Low wage indicator (using combined quantiles) ---
overlap[, low_wage20 := as.integer(nq_combined_hwage_ist < 3)]

# --- Compute overlap stats by year ---
stats_yr <- overlap[!is.na(high_mono) & !is.na(low_wage20), .(
  n_total      = .N,
  n_high_mono  = sum(high_mono == 1),
  n_low_wage   = sum(low_wage20 == 1),
  n_both       = sum(high_mono == 1 & low_wage20 == 1)
), by = year]

stats_yr[, `:=`(
  pct_mono_is_lowwage = n_both / n_high_mono * 100,
  pct_lowwage_is_mono = n_both / n_low_wage * 100,
  pct_both            = n_both / n_total * 100
)]

# --- Plot ---
stats_long <- melt(stats_yr,
                   id.vars = "year",
                   measure.vars = c("pct_mono_is_lowwage", "pct_lowwage_is_mono"),
                   variable.name = "direction", value.name = "pct")

stats_long[, direction := fifelse(
  direction == "pct_mono_is_lowwage",
  "Low wage condition on High mono",
  "High mono condition on Low wage"
)]

p <- ggplot(stats_long, aes(x = year, y = pct, color = direction)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Low wage condition on High mono" = "navy",
                                "High mono condition on Low wage" = "darkred")) +
  scale_x_continuous(breaks = seq(1982, 2021, by = 4)) +
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position  = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(x = NULL, y = "Percent (%)", color = NULL,
       title = "Overlap: High-monopsony(top 10%) × Low-Wage (bottom 20%)")

ggsave(file.path(outdir, "overlap_mono10_wage20_combined.png"), p,
       width = 8, height = 5, dpi = 300)

# ==============================================================
# 4. Monopsony power is slightly time-variant.
# ==============================================================
mono[, year := 1960L + quarterly_date %/% 4L]

est_mono <- feols(
  newRecruit_rate ~ year | ind1990 + statefips,
  data = mono,
  weights = ~wtfinl,
  cluster = ~statefips
)

summary(est_mono)

# focus on low wage industries
mono_w <- merge(mono, wage[, .(ind1990, statefips, year, nq_combined_hwage_ist)],
                by = c("ind1990", "statefips", "year"), all.x = TRUE)

mono_w[, low_wage20 := as.integer(nq_combined_hwage_ist < 3)]

est_lw <- feols(
  newRecruit_rate ~ year | ind1990 + statefips,
  data = mono_w[low_wage20 == 1],
  weights = ~wtfinl,
  cluster = ~statefips
)

summary(est_lw)