# ==============================
# Author: Po-Jui Huang (code with Claude together)
# Date: 6th May, 7th May, 18th May 2026
# Goal: Estimates Model 3 — placebo test(top 20% high wage industries withn each state)
# Sample: events which increased state mw over 10%. + the top 20% high wage industries within each state
# ==============================

library(arrow)
library(haven)
library(data.table)
library(fixest)
library(ggplot2)
library(modelsummary)
library(car)

setwd("my path")
outdir <- "results/260518_eq3_reg_placebo_top20"
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
# 5. Generate variables and pre-allocate space
# ==============================================================
dt_annual[, `:=`(
  high_mono10 = as.integer(nq_monopsony_loo > 9),
  high_wage20  = as.integer(nq_combined_hwage_ist > 8),
  gr10        = as.integer(gr_mw >= 0.1)
)]

dt_annual[, D_mono := D * high_mono10]

pre_j  <- c(-3, -2)        # pre treatment(baseline is -1)
post_j <- c(0, 1, 2, 3, 4) # post treatmet

rows_base <- list() ## contain coef.
rows_mono <- list() ## contain coef.

# ==============================================================
# 6.Regression
# ==============================================================
est <- feols(
  emp_ratio ~
    i(j, D, ref = -1) +
    i(j, D_mono, ref = -1) +
    skill_score1 + skill_score2 +
    skill_score3 + skill_score4 +
    skill_score5 + skill_score6 |
    ind1990 +
    statefips +
    ind1990^statefips +
    statefips^year +
    ind1990^year,
  data = dt_annual[high_wage20 == 1 & gr10 == 1 & j %between% c(-3, 4)],
  cluster = ~statefips
)

# ==============================================================
# 7. Export results
# ==============================================================
# basic model summary
cn <- names(coef(est))
nice <- cn |>
  gsub("j::", "j=", x = _) |>
  gsub(":D_mono", " × D × M", x = _) |>
  gsub(":D", " × D", x = _)
cmap <- setNames(nice, cn) # rename coef

modelsummary(est,
             output   = file.path(outdir, paste0("eq3_table_placebo_top20.tex")),
             fmt      = 4,
             coef_map = cmap,
             stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             title    =  "Placebo: Top 20% Wage Industries — Equation (3)")

# coef. matrix (easier to read) 
cc <- coeftable(est)
get_coef <- function(name) {
  if (name %in% rownames(cc)) {
    b  <- cc[name, "Estimate"]
    se <- cc[name, "Std. Error"]
    return(c(b = b, lo = b - 1.96 * se, hi = b + 1.96 * se))
  }
  return(c(b = NA_real_, lo = NA_real_, hi = NA_real_))
}

## pre
for (jj in pre_j) {
  rows_base[[length(rows_base) + 1]] <- c(j = jj, get_coef(paste0("j::", jj, ":D")))
  
  lc_m <- lincom(est, c(paste0("j::", jj, ":D"), paste0("j::", jj, ":D_mono")))
  rows_mono[[length(rows_mono) + 1]] <- c(j = jj, b = lc_m$estimate,
                                          lo = lc_m$estimate - 1.96 * lc_m$se,
                                          hi = lc_m$estimate + 1.96 * lc_m$se)
}

## baseline j = -1
rows_base[[length(rows_base) + 1]] <- c(j = -1, b = 0, lo = 0, hi = 0)
rows_mono[[length(rows_mono) + 1]] <- c(j = -1, b = 0, lo = 0, hi = 0)

## post
for (jj in post_j) {
  rows_base[[length(rows_base) + 1]] <- c(j = jj, get_coef(paste0("j::", jj, ":D")))
  
  lc_m <- lincom(est, c(paste0("j::", jj, ":D"), paste0("j::", jj, ":D_mono")))
  rows_mono[[length(rows_mono) + 1]] <- c(j = jj, b = lc_m$estimate,
                                          lo = lc_m$estimate - 1.96 * lc_m$se,
                                          hi = lc_m$estimate + 1.96 * lc_m$se)
}

df_base <- as.data.table(do.call(rbind, rows_base))
df_mono <- as.data.table(do.call(rbind, rows_mono))

setnames(df_base, c("j", "b_base", "lo_base", "hi_base"))
setnames(df_mono, c("j", "b_mono", "lo_mono", "hi_mono"))

knitr::kable(df_base, format = "latex", booktabs = TRUE, digits = 4,
             caption = paste0("Non high monopsony")) |>
  writeLines(file.path(outdir, paste0("coef_base_placebo_top20.tex")))

knitr::kable(df_mono, format = "latex", booktabs = TRUE, digits = 4,
             caption = paste0("High monopsony")) |>
  writeLines(file.path(outdir, paste0("coef_mono_placebo_top20.tex")))


# ==============================================================
# 8. Plot
# ==============================================================
df_plot <- merge(df_base, df_mono, by = "j")

p <- ggplot(df_plot, aes(x = j)) +
  geom_errorbar(aes(ymin = lo_base, ymax = hi_base),
                width = 0.2, color = "gray70", linewidth = 0.4) +
  geom_errorbar(aes(ymin = lo_mono, ymax = hi_mono),
                width = 0.2, color = alpha("navy", 0.5), linewidth = 0.4) +
  geom_line(aes(y = b_base, color = "Non-high monopsony"), linetype = "dashed") +
  geom_point(aes(y = b_base, color = "Non-high monopsony"), shape = 1, size = 2.5) +
  geom_line(aes(y = b_mono, color = "High monopsony")) +
  geom_point(aes(y = b_mono, color = "High monopsony"), shape = 16, size = 2.5) +
  scale_color_manual(values = c("High monopsony" = "navy",
                                "Non-high monopsony" = "gray60")) +
  geom_vline(xintercept = -0.5, color = "red", linewidth = 0.3) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  scale_x_continuous(breaks = -3:4) +
  labs(x = "Years relative to MW hike",
       y = "Effect on employment ratio",
       color = NULL,
       title = "Placebo: the Employment Effect of the Minimum Wage Increases \n — Top 20% High-Wage Industries") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

ggsave(file.path(outdir, paste0("event_study_placebo_top20.png")), p,
       width = 8, height = 5, dpi = 300)

# ==============================================================
# 9. Pre-trend Test(hypothesis testing)
# ==============================================================
sink(file.path(outdir, paste0("pretrend_placebo_top20.txt")))

cat("==========================================\n\n")

cat("PRE-TREND TEST: Base D (H0: v_j jointly zero)\n")
cat("------------------------------------------\n")
print(wald(est, paste0("j::", pre_j, ":D")))

cat("\nPRE-TREND TEST: Mono (H0: lambda_j jointly zero)\n")
cat("------------------------------------------\n")
print(wald(est, paste0("j::", pre_j, ":D_mono")))

cat("\nPRE-TREND TEST: All (H0: v_j and lambda_j jointly zero)\n")
cat("------------------------------------------\n")
print(wald(est, c(paste0("j::", pre_j, ":D"), paste0("j::", pre_j, ":D_mono"))))

cat("\nPRE-TREND TEST: Mono parallel linear\n")
cat("------------------------------------------\n")
print(linearHypothesis(est, "j::-3:D_mono - j::-2:D_mono = 0", vcov = vcov(est)))

sink()

