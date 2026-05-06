# ==============================
# Author: Po-Jui Huang (code with Claude together)
# Date: 22nd, Apr. 2026
# Goal: Loads and organises state-level annual CPI data for real wage deflation.
# ==============================

library(tidyverse)
library(lubridate)
library(readxl)

setwd("my path")
# ============================================================
# 1. Map State-to-Region and CPS FIPS Code
# ============================================================
state_map <- tribble(
  ~state,           ~region,    ~cps_fips,
  # Northeast
  "Connecticut",    "Northeast",  9,
  "Maine",          "Northeast", 23,
  "Massachusetts",  "Northeast", 25,
  "New Hampshire",  "Northeast", 33,
  "New Jersey",     "Northeast", 34,
  "New York",       "Northeast", 36,
  "Pennsylvania",   "Northeast", 42,
  "Rhode Island",   "Northeast", 44,
  "Vermont",        "Northeast", 50,
  # Midwest
  "Illinois",       "Midwest",   17,
  "Indiana",        "Midwest",   18,
  "Iowa",           "Midwest",   19,
  "Kansas",         "Midwest",   20,
  "Michigan",       "Midwest",   26,
  "Minnesota",      "Midwest",   27,
  "Missouri",       "Midwest",   29,
  "Nebraska",       "Midwest",   31,
  "North Dakota",   "Midwest",   38,
  "Ohio",           "Midwest",   39,
  "South Dakota",   "Midwest",   46,
  "Wisconsin",      "Midwest",   55,
  # South
  "Alabama",        "South",      1,
  "Arkansas",       "South",      5,
  "Delaware",       "South",     10,
  "DC",             "South",     11,
  "Florida",        "South",     12,
  "Georgia",        "South",     13,
  "Kentucky",       "South",     21,
  "Louisiana",      "South",     22,
  "Maryland",       "South",     24,
  "Mississippi",    "South",     28,
  "North Carolina", "South",     37,
  "Oklahoma",       "South",     40,
  "South Carolina", "South",     45,
  "Tennessee",      "South",     47,
  "Texas",          "South",     48,
  "Virginia",       "South",     51,
  "West Virginia",  "South",     54,
  # West
  "Alaska",         "West",       2,
  "Arizona",        "West",       4,
  "California",     "West",       6,
  "Colorado",       "West",       8,
  "Hawaii",         "West",      15,
  "Idaho",          "West",      16,
  "Montana",        "West",      30,
  "Nevada",         "West",      32,
  "New Mexico",     "West",      35,
  "Oregon",         "West",      41,
  "Utah",           "West",      49,
  "Washington",     "West",      53,
  "Wyoming",        "West",      56
)

# ============================================================
# 2. Import data
# ============================================================
read_cpi <- function(path, region_name) {
  read_excel(path, skip = 11) %>%       # skip 11 metadata rows, row 12 becomes header(how they look when I download from Fred.)
    rename_with(tolower) %>%
    rename(year = 1, cpi = 2) %>%       
    mutate(
      year   = as.integer(year),         
      cpi    = as.numeric(cpi),
      region = region_name
    ) %>%
    filter(!is.na(cpi), !is.na(year), year <= 2023) %>%
    select(year, region, cpi)
}

cpi_raw <- bind_rows(
  read_cpi("northeast.xlsx", "Northeast"),
  read_cpi("midwest.xlsx",   "Midwest"),
  read_cpi("south.xlsx",     "South"),
  read_cpi("west.xlsx",      "West")
)

# ============================================================
# 3. Merge with state 
# ============================================================
cpi_state <- cpi_raw %>%
  left_join(state_map, by = "region") %>%
  select(state, cps_fips, region, year, cpi) %>%
  arrange(cps_fips, year)

# ============================================================
# 4. save
# ============================================================
write_csv(cpi_state, "cpi_state_annual.csv")