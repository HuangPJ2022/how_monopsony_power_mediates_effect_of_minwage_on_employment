# ==============================
# Author: Po-Jui Huang (code with Claude together)
# Date: 8th May, 18th May 2026
# Goal: Estimates Model 3 — placebo test(1yr lead before treatment)
# Sample: events which increased state mw over 10%. + the bottom 20% low wage industries within each state
# ==============================


library(arrow)
library(haven)
library(data.table)
library(fixest)
library(ggplot2)
library(modelsummary)
library(car)

setwd("my path")
outdir <- "results/260518_eq3_reg_1yr_lead_three_trends"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================
# 1. Define a function to combine regression coef and vcov
# ==============================================================
lincom <- function(est, coefs, signs = rep(1, length(coefs))) {
  b <- coef(est)
  V <- vcov(est)
  missing <- coefs[!coefs %in% names(b)]
  if (length(missing) > 0) stop("Not found: ", paste(missing, collapse = ", "))
  val <- sum(signs * b[coefs])
  se  <- sqrt(as.numeric(t(signs) %*% V[coefs, coefs] %*% signs))
  list(estimate = val, se = se)
}

# ==============================================================
# 2. Import data
# ==============================================================
dt <- as.data.table(read_parquet("temp/event_panel_r/event_panel_all_yr.parquet"))
mono <- as.data.table(read_dta("temp/newRecruit_rate_yqt.dta"))
ind_detail <- as.data.table(read_dta("rawdata/ind1990_details.dta"))
wage <- as.data.table(read_dta("temp/combined_hourwage/average_combined_hwage_ist.dta"))
policy_id <- as.data.table(read_stata("temp/policy_id.dta"))

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

# ==============================================================
# 4. Merge and select data
# ==============================================================
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
# 5. Generate variables
# ==============================================================
dt_annual[, `:=`(
  high_mono10 = as.integer(nq_monopsony_loo > 9),
  low_wage20  = as.integer(nq_combined_hwage_ist < 3),
  gr10        = as.integer(gr_mw >= 0.1),
  j_lead      = j + 1L
)]

dt_annual[, D_mono := D * high_mono10]

pre_j  <- c(-3, -2)
post_j <- c(0, 1, 2, 3, 4)

rows_base <- list()
rows_mono <- list()

# ==============================================================
# 6. General regression (1-year lead)
# ==============================================================
est_general <- feols(
  emp_ratio ~
    i(j_lead, D, ref = -1) +
    skill_score1 + skill_score2 +
    skill_score3 + skill_score4 +
    skill_score5 + skill_score6 |
    ind1990 +
    statefips +
    ind1990^statefips +
    statefips^year +
    ind1990^year,
  data = dt_annual[low_wage20 == 1 & gr10 == 1 & j_lead %between% c(-3, 4)],
  cluster = ~statefips
)

cn_g <- names(coef(est_general))
nice_g <- cn_g |>
  gsub("j_lead::", "j=", x = _) |>
  gsub(":D", " × D", x = _)
cmap_g <- setNames(nice_g, cn_g)

modelsummary(est_general,
             output   = file.path(outdir, "eq3_table_general_lead.tex"),
             fmt      = 4,
             coef_map = cmap_g,
             stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             title    = "Equation (3) 1-Year Lead — General Effect")

cc_g <- coeftable(est_general)
get_coef_g <- function(name) {
  if (name %in% rownames(cc_g)) {
    b  <- cc_g[name, "Estimate"]
    se <- cc_g[name, "Std. Error"]
    return(c(b = b, lo = b - 1.96 * se, hi = b + 1.96 * se))
  }
  return(c(b = NA_real_, lo = NA_real_, hi = NA_real_))
}

rows_general <- list()
for (jj in pre_j) {
  rows_general[[length(rows_general) + 1]] <- c(j = jj, get_coef_g(paste0("j_lead::", jj, ":D")))
}
rows_general[[length(rows_general) + 1]] <- c(j = -1, b = 0, lo = 0, hi = 0)
for (jj in post_j) {
  rows_general[[length(rows_general) + 1]] <- c(j = jj, get_coef_g(paste0("j_lead::", jj, ":D")))
}

df_general <- as.data.table(do.call(rbind, rows_general))
setnames(df_general, c("j", "b_gen", "lo_gen", "hi_gen"))

knitr::kable(df_general, format = "latex", booktabs = TRUE, digits = 8,
             caption = "General effect (1-year lead)") |>
  writeLines(file.path(outdir, "coef_general_1yr_lead.tex"))

# ==============================================================
# 7. Regression interacting with monopsony power (1-year lead)
# ==============================================================
est <- feols(
  emp_ratio ~
    i(j_lead, D, ref = -1) +
    i(j_lead, D_mono, ref = -1) +
    skill_score1 + skill_score2 +
    skill_score3 + skill_score4 +
    skill_score5 + skill_score6 |
    ind1990 +
    statefips +
    ind1990^statefips +
    statefips^year +
    ind1990^year,
  data = dt_annual[low_wage20 == 1 & gr10 == 1 & j_lead %between% c(-3, 4)],
  cluster = ~statefips
)

cn <- names(coef(est))
nice <- cn |>
  gsub("j_lead::", "j=", x = _) |>
  gsub(":D_mono", " × D × M", x = _) |>
  gsub(":D", " × D", x = _)
cmap_l <- setNames(nice, cn)

modelsummary(est,
             output   = file.path(outdir, "eq3_table_lead.tex"),
             fmt      = 4,
             coef_map = cmap_l,
             stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             title    = "Equation (3) — 1-year lead placebo")

cc <- coeftable(est)
get_coef <- function(name) {
  if (name %in% rownames(cc)) {
    b  <- cc[name, "Estimate"]
    se <- cc[name, "Std. Error"]
    return(c(b = b, lo = b - 1.96 * se, hi = b + 1.96 * se))
  }
  return(c(b = NA_real_, lo = NA_real_, hi = NA_real_))
}

for (jj in pre_j) {
  rows_base[[length(rows_base) + 1]] <- c(j = jj, get_coef(paste0("j_lead::", jj, ":D")))
  lc_m <- lincom(est, c(paste0("j_lead::", jj, ":D"), paste0("j_lead::", jj, ":D_mono")))
  rows_mono[[length(rows_mono) + 1]] <- c(j = jj, b = lc_m$estimate,
                                          lo = lc_m$estimate - 1.96 * lc_m$se,
                                          hi = lc_m$estimate + 1.96 * lc_m$se)
}

rows_base[[length(rows_base) + 1]] <- c(j = -1, b = 0, lo = 0, hi = 0)
rows_mono[[length(rows_mono) + 1]] <- c(j = -1, b = 0, lo = 0, hi = 0)

for (jj in post_j) {
  rows_base[[length(rows_base) + 1]] <- c(j = jj, get_coef(paste0("j_lead::", jj, ":D")))
  lc_m <- lincom(est, c(paste0("j_lead::", jj, ":D"), paste0("j_lead::", jj, ":D_mono")))
  rows_mono[[length(rows_mono) + 1]] <- c(j = jj, b = lc_m$estimate,
                                          lo = lc_m$estimate - 1.96 * lc_m$se,
                                          hi = lc_m$estimate + 1.96 * lc_m$se)
}

df_base <- as.data.table(do.call(rbind, rows_base))
df_mono <- as.data.table(do.call(rbind, rows_mono))

setnames(df_base, c("j", "b_base", "lo_base", "hi_base"))
setnames(df_mono, c("j", "b_mono", "lo_mono", "hi_mono"))

knitr::kable(df_base, format = "latex", booktabs = TRUE, digits = 8,
             caption = "Non high monopsony (1-year lead)") |>
  writeLines(file.path(outdir, "coef_base_1yr_lead.tex"))

knitr::kable(df_mono, format = "latex", booktabs = TRUE, digits = 8,
             caption = "High monopsony (1-year lead)") |>
  writeLines(file.path(outdir, "coef_mono_1yr_lead.tex"))

# ==============================================================
# 8. Pre-trend Tests
# ==============================================================
sink(file.path(outdir, "pretrend_1yr_lead.txt"))

cat("==========================================\n\n")

cat("PRE-TREND TEST: Base D (H0: v_j jointly zero)\n")
cat("------------------------------------------\n")
print(wald(est, paste0("j_lead::", pre_j, ":D")))

cat("\nPRE-TREND TEST: Mono (H0: lambda_j jointly zero)\n")
cat("------------------------------------------\n")
print(wald(est, paste0("j_lead::", pre_j, ":D_mono")))

cat("\nPRE-TREND TEST: All (H0: v_j and lambda_j jointly zero)\n")
cat("------------------------------------------\n")
print(wald(est, c(paste0("j_lead::", pre_j, ":D"), paste0("j_lead::", pre_j, ":D_mono"))))

cat("\nPRE-TREND TEST: Mono parallel linear\n")
cat("------------------------------------------\n")
print(linearHypothesis(est, "j_lead::-3:D_mono - j_lead::-2:D_mono = 0", vcov = vcov(est)))

sink()

# ==============================================================
# 9. Plot — three trends
# ==============================================================
df_three <- merge(df_general, merge(df_base, df_mono, by = "j"), by = "j")

p3 <- ggplot(df_three, aes(x = j)) +
  geom_errorbar(aes(ymin = lo_gen, ymax = hi_gen),
                width = 0.2, color = alpha("darkred", 0.4), linewidth = 0.4) +
  geom_line(aes(y = b_gen, color = "General"), linetype = "dotted") +
  geom_point(aes(y = b_gen, color = "General"), shape = 17, size = 2.5) +
  geom_errorbar(aes(ymin = lo_base, ymax = hi_base),
                width = 0.2, color = "gray70", linewidth = 0.4) +
  geom_line(aes(y = b_base, color = "Non-high monopsony"), linetype = "dashed") +
  geom_point(aes(y = b_base, color = "Non-high monopsony"), shape = 1, size = 2.5) +
  geom_errorbar(aes(ymin = lo_mono, ymax = hi_mono),
                width = 0.2, color = alpha("navy", 0.4), linewidth = 0.4) +
  geom_line(aes(y = b_mono, color = "High monopsony")) +
  geom_point(aes(y = b_mono, color = "High monopsony"), shape = 16, size = 2.5) +
  scale_color_manual(values = c("General" = "darkred",
                                "Non-high monopsony" = "gray60",
                                "High monopsony" = "navy")) +
  geom_vline(xintercept = -0.5, color = "red", linewidth = 0.3) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  scale_x_continuous(breaks = -3:4) +
  labs(x = "Years relative to placebo MW hike",
       y = "Effect on employment ratio",
       color = NULL,
       title = "Placebo: the Employment Effect of the Minimum Wage Increases — 1-Year Lead") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

ggsave(file.path(outdir, "event_study_employment_1yr_lead.png"), p3,
       width = 8, height = 5, dpi = 300)