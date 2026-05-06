# ==============================
# Author: Po-Jui Huang 
# Date: 5th, Apr. 2026
# Goal: ensure event IDs are consistent between event panels and the policy dataset.
# ==============================
library(arrow)
library(haven)
library(data.table)

setwd("my path")

dt <- as.data.table(read_parquet("temp/event_panel_r/event_panel_all.parquet"))

event_list <- dt[D == 1 & j == 0, .(statefips, quarterly_date, event_id)]
event_list <- unique(event_list)

policy <- as.data.table(read_stata("temp/policy_state_event.dta"))

policy_id <- merge(policy, event_list,
                   by.x = c("statefips", "quarterly_date"),
                   by.y = c("statefips", "quarterly_date"),
                   all.x = TRUE)

write_dta(policy_id, "temp/policy_id.dta")