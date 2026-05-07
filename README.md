# Replication Package: Monopsony power and the employment effect of minimum wage increases: Evidence from the U.S.

> **Data Availability:** Data files are not uploaded to this repository due to size constraints. Please refer to the data sources listed below to obtain the raw data or contact Po-Jui Huang (hpj707024@gmail.com) directly.

**Author:** Po-Jui Huang ([hpj707024@gmail.com](mailto:hpj707024@gmail.com))  
**Latest Update:** May 2026


---

## Overview

This replication package studies how monopsony power mediates the employment effects of minimum wage increases on the low-wage industries in the United States. Monopsony power is measured using the fraction of new recruits from non-employment, following Manning (2003), which stems from the search-friction model of the labour market.

The project incorporates U.S. Current Population Survey basic monthly data, O*NET skill measures, CPI (Regional, Annual) and historical state-level minimum wage records from Vaghul and Zipperer (2022).

The identification strategy follows an event-study design, proposed by Cengiz et al. (2019) and Sun and Abraham (2021). Each state-level minimum wage change of at least 10% constitutes a treated event. Other states raising minimum wage within the event window are treated as contaminated samples and excluded from a panel. The staggered full panel covers 60 qualifying events out of 217 total state-level changes from 1990 to 2016. In addition, this study concentrates on the low-wage industry, i.e. the bottom 20% of industries by average wage within each state at a given time.

The empirical strategy estimates three econometric models:
- **Model 1 (Eq. 1)** — Correlation between monopsony power and wage compression (Kaitz index and MW coverage rate), with industry, state, and time fixed effects
- **Model 2 (Eq. 2)** — Causal effect of minimum wage increases on average real wages in low-wage industries
- **Model 3 (Eq. 3)** — Triple-difference design estimating the employment effect of minimum wage increases within low-wage industries: after treatment, the difference in employment between treated and non-treated states is estimated separately for high monopsony power industries (top 10% by state) and non-high monopsony power industries, with both differences normalised to zero at the reference period.

---

## Software Requirements

- **Stata** (for data cleaning, crosswalk construction, and Eq. 1 regression)
- **R** (for event panel construction, merging, and Eq. 2 & 3 regressions)

---

## Data Sources

| Dataset | Description |
|---|---|
| CPS Basic Monthly Data | Main individual-level data on employment status, occupation (Census 2010 OCC codes), industry (1990 Census Industry Classification), wages, and socioeconomic characteristics |
| O*NET | Six occupation-level skill measures following Acemoglu and Autor (2011): Non-routine cognitive (Analytical & Interpersonal), Routine cognitive, Routine manual, Non-routine manual physical, and Offshorability |
| Historical Minimum Wage Data | Vaghul & Zipperer (2022), available at [github.com/benzipperer/historicalminwage](https://github.com/benzipperer/historicalminwage/releases/tag/v1.4.0) |
| CPI (Regional, Annual) | Annual average CPI for four U.S. regions from the Bureau of Labor Statistics, used to deflate nominal wages into real wages |

---

## Code Pipeline

All programmes are saved to `.\code\`.

 

### Stage 1 — Data Preparation (Stata)

| File | Description |
|---|---|
| `harmonise_occ2010_soc2010.do` | Constructs a crosswalk mapping CPS occupation codes (OCC2010) to SOC 2010 codes and splits CPS data into birth-year cohorts. |
| `prepare_onet_merge.do` | Loads raw O*NET ability, work activity, and context files and saves them as `.dta` files for downstream merging. |
| `onet_merge.do` | Merges O*NET skill measures to SOC 2010 occupation codes at multiple digit levels (3–7 digits) to maximise CPS match coverage. *(coded collaboratively with Claude)* |
| `clean_earnings.do` | Cleans and imputes hourly earnings from CPS; drops observations where imputation is not possible. |
| `cps_soc_match_skill_crosswalk.do` | Builds a one-to-one crosswalk between CPS occupation codes and O*NET skill measure codes, then merges skill measures into the CPS dataset. *(coded collaboratively with Claude)* |
| `compute_monopsony_power.do` | Constructs a panel and computes the fraction of new recruits from non-employment by state × year-quarter × industry. |
| `mw_policy_state_yqt.do` | Processes historical state minimum wage data and identifies all state-level minimum wage change events by quarter. |
| `organise_price.R` | Loads and organises regional annual CPI data from the Bureau of Labor Statistics for real wage deflation. *(coded collaboratively with Claude)* |
| `mean_wage.do` | Computes mean wage datasets in three versions: imputed wages, actual (non-imputed) wages, and actual wages with gaps filled by imputed values. |

### Stage 2 — Event Panel Construction (R)

| File | Description |
|---|---|
| `mw_policy_state_yqt_id.R` | Assigns unique event IDs to each state-level minimum wage change in the policy dataset. |
| `construct_event_panel.R` | Constructs a separate event panel for each minimum wage event (events 1–240), excluding contaminated control states within the event window; saves as `.parquet` for efficiency. *(coded collaboratively with Claude)* |
| `construct_event_panel_yr.R` | Collapses the quarter-level event panel to an annual level to reduce computational load. |
| `construct_wage_event_panel.R` | Constructs the event panel with wage variables appended, at both quarterly and annual levels. *(coded collaboratively with Claude)* |

### Stage 3 — Analysis

| File | Description |
|---|---|
| `compute_avg_coverage_by_gr_mw.do` | Computes average minimum wage coverage (share of affected workers) separately for events with ≥5% and ≥10% wage growth rates. |
| `state_graph.R` | Generates state-level maps of monopsony power and plots of minimum wage policy changes over time by state. *(coded collaboratively with Claude)* |
| `summary_stats.R` | Generates summary statistics. *(coded collaboratively with Claude)* |
| `eq1_regression.do` | **(Stata)** Estimates Model 1 — OLS regression of wage compression (Kaitz index and MW coverage rate) on monopsony power, with and without a squared term and industry/state/time fixed effects; produces Table 2 and Figure 3. |
| `eq2_regression.R` | **(R)** Estimates Model 2 — event-study effect of minimum wage increases on log average real wage in low-wage industries. *(coded collaboratively with Claude)* |
| `eq2_regression_1yr_lead_placebo.R` | **(R)** Placebo test for Model 2: uses one year prior to the actual event as a false treatment date to test for pre-existing trends. *(coded collaboratively with Claude)* |
| `eq2_regression_top20_placebo.R` | **(R)** Placebo test for Model 2: uses the top 20% highest-wage industries as a falsification sample. *(coded collaboratively with Claude)* |
| `eq3_regression.R` | **(R)** Estimates Model 3 — event-study employment effects of minimum wage increases, interacting treatment with an indicator for high monopsony power (top 10% by state), using the leave-one-out average monopsony measure to address endogeneity. *(coded collaboratively with Claude)* |
| `eq3_regression_1yr_lead_placebo.R` | **(R)** Placebo test for Model 3: uses one year prior to the actual event as a false treatment date. *(coded collaboratively with Claude)* |
| `eq3_regression_top20_placebo.R` | **(R)** Placebo test for Model 3: uses the top 20% highest-wage industries as a falsification sample. *(coded collaboratively with Claude)* |

---

## Notes

- **Monopsony measure:** Fraction of new recruits from non-employment, following Manning (2003); averaged across states other than the own state across years (leave-one-out) to address endogeneity in Model 3
- **High monopsony threshold:** Top 10% of industries by monopsony power within each state
- **Low-wage industry threshold:** Bottom 20% of industries by average wage within each state at a given time
- **Minimum wage growth rate threshold:** Events with ≥ 10% state-level minimum wage increase (60 out of 217 events, 1990–2016; average coverage rate 9.8%)
- **Reference period:** Year −1 (one year before implementation) is omitted as the reference group in Models 2 and 3
- **Wage measure:** Allocated, non-imputed hourly wage from IPUMS CPS as primary; imputed wage used supplementarily where missing
- **Real wage deflation:** Annual average CPI for four U.S. regions (Bureau of Labor Statistics)
- **Fixed effects (Models 2 & 3):** Industry, state, industry × state, state × year, industry × year
- **Standard errors:** Clustered at state level
- **Event window contamination:** States with any minimum wage change within the event window are excluded as controls

---

## Results Directory

All output tables and figures are saved to `.\results\`. Key outputs include:

| Output | Description |
|---|---|
| `260506_wageCompression` | Eq. 1 results — wage compression |
| `260506_eq2_reg` | Eq. 2 results — wage effects |
| `260506_eq3_reg` | Eq. 3 results — employment effects by monopsony |
| `260506_eq2_reg_1yr_lead` / `_top20` | Placebo tests for Eq. 2 |
| `260506_eq3_reg_1yr_lead` / `_top20` | Placebo tests for Eq. 3 |
| `260506_state_graph` | State maps and policy change plots |
| `260507_summary_stats` | Summary statistics |

---

## References

- Acemoglu, D., & Autor, D. (2011). Skills, Tasks and Technologies: Implications for Employment and Earnings. In D. Card, & O. Ashenfelter (Eds.), *Handbook of Labor Economics* Vol. 4, Part B (pp. 1043-1171). Amsterdam: Elsevier. https://doi.org/10.1016/S0169-7218(11)02410-5
- Anthropic (2026). Claude (claude-opus-4-6) [Large language model]. https://www.anthropic.com.
- Cengiz, D., Dube, A., Lindner, A., & Zipperer, B. (2019). The Effect of Minimum Wages on Low-Wage Jobs. *The Quarterly Journal of Economics*, 134(3), 1405–1454. https://doi.org/10.1093/qje/qjz014
- Manning, A. (2003). *Monopsony in Motion: Imperfect Competition in Labor Markets*. Princeton University Press. https://doi.org/10.2307/j.ctt5hhpvk
- Sun, L., & Abraham, S. (2021). Estimating dynamic treatment effects in event studies with heterogeneous treatment effects. *Journal of Econometrics*, 225(2), 175–199. https://doi.org/10.1016/j.jeconom.2020.09.006
- Vaghul, K., & Zipperer, B. (2022). Historical State and Sub-state Minimum Wages. Version 1.4.0. https://github.com/benzipperer/historicalminwage/releases/tag/v1.4.0.
