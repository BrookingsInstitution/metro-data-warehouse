library(tabulizer)
library(tidyverse)
library(tidylog)

# download data table from source page
# "https://www.dropbox.com/s/909kjdhad4z0w0f/AppendixB_SupplyChain_Categorization.pdf?dl=0"
tmp <- tabulizer::extract_tables("../../../The Brookings Institution/Metro Research - JParilla/COVID recession/library/small biz/AppendixB_SupplyChain_Categorization.pdf")

naics_sc <- purrr::map(tmp[3:20], as.data.frame) %>%
  dplyr::bind_rows() %>%
  mutate_all(list(~ na_if(., ""))) %>%
  fill(V4) %>%
  filter(V1 != "") %>%
  select(
    naics6_code = V2, naics6_name = V3,
    sector = V4, sc = V5, traded = V6, mfg = V7,
    emp15 = V8, pct_pce = V9
  ) %>%
  mutate_at(vars(sc:pct_pce), ~ as.numeric(gsub("%", "", .)))

skimr::skim(naics_sc)
save(naics_sc, file = "data-raw/naics_sc.rda")

# modifty to 3 digit ---
load("data-raw/naics_sc.rda")

naics3_sc <- naics_sc %>%
  mutate(naics3_code = str_sub(naics6_code, 1, 3)) %>%
  group_by(naics3_code) %>%
  summarise(across(sc:mfg, ~ weighted.mean(., emp15))) %>%

  left_join(naics_sc %>%
    mutate(naics3_code = str_sub(naics6_code, 1, 3)) %>%
    group_by(naics3_code, sector) %>%
    summarise(emp = sum(emp15)) %>%
    slice_max(emp))

naics3_sc <- naics3_sc %>%
  add_row(naics_code = "482",
          sector = "B2C Traded Services",
          traded = 1,
          supply_chain = 0) %>%
  select(naics_code = naics3_code, sector, supply_chain = sc, traded)

# save modified data
save(naics3_sc, file = "data-raw/naics3_sc.rda")

naics6_sc <- naics_sc %>%
  select(naics_code = naics6_code, sector, supply_chain = sc, traded)
save(naics6_sc, file = "data-raw/naics6_sc.rda")


# alternative definitions
naics3_sc %>%
  mutate(sector_alt = case_when(
    supply_chain > 0.6 ~ "Supply Chain",
    traded  > 0.6 ~ "B2C Traded Manufacturing and Services",
    T ~ "Local Mainstreet"
  ))

# BEST ===============

best <- c(
  "Forestry and Logging",
  "Fishing, Hunting and Trapping",
  "Support Activities for Agriculture and Forestry",
  "Oil and Gas Extraction",
  "Mining (except Oil and Gas)",
  "Support Activities for Mining",
  "Food Manufacturing",
  "Beverage and Tobacco Product Manufacturing",
  "Paper Manufacturing",
  "Petroleum and Coal Products Manufacturing",
  "Chemical Manufacturing",
  "Nonmetallic Mineral Product Manufacturing",
  "Primary Metal Manufacturing",
  "Machinery Manufacturing",
  "Computer and Electronic Product Manufacturing",
  "Electrical Equipment, Appliance, and Component Manufacturing",
  "Transportation Equipment Manufacturing",
  "Miscellaneous Manufacturing",
  "Air Transportation",
  "Water Transportation",
  "Pipeline Transportation",
  "Publishing Industries (except Internet)",
  "Motion Picture and Sound Recording Industries",
  "Broadcasting (except Internet)",
  "Telecommunications",
  "Data Processing, Hosting, and Related Services",
  "Other Information Services",
  "Credit Intermediation and Related Activities",
  "Securities, Commodity Contracts, and Other Financial Investments and Related Activities",
  "Insurance Carriers and Related Activities",
  "Funds, Trusts, and Other Financial Vehicles",
  "Rental and Leasing Services",
  "Lessors of Nonfinancial Intangible Assets (except Copyrighted Works)",
  "Professional, Scientific, and Technical Services"
)

naics3_BEST <- naics %>%
  filter(naics_name %in% best) %>%
  filter(naics_level == 3) %>%
  pull(naics_code)

save(naics3_BEST, file = "data-raw/naics3_BEST.rda")
