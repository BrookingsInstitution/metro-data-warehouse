# acs variables list ==================
library(purrr)

# Basics ---------------
pop_race_codes <- c("S2301_C01_001",map_chr(seq(12,20),function(x)paste0("S2301_C01_0",x))) # total population
pov_race_codes <- map_chr(seq(13,21),function(x)paste0("S1701_C03_0",x))  # poverty status
drive_codes <- c("S0802_C01_001", "S0802_C02_001") #drove to work alone

# Labor market --------------
lfp_race_codes <- c("S2301_C02_001",map_chr(seq(12,20),function(x)paste0("S2301_C01_0",x))) # labor force participation rate
epop_race_codes <- c("S2301_C03_001",map_chr(seq(12,20),function(x)paste0("S2301_C01_0",x))) # employment/population ratio
unemp_race_codes <- c("S2301_C04_001",map_chr(seq(12,20),function(x)paste0("S2301_C01_0",x))) # unemployment rate

# Education ------------
ed_race_codes <- map_chr(seq(28,54),function(x)paste0("S1501_C01_0",x)) # education attainment
earnings_edu_codes <- map_chr(seq(1,6),function(x){paste0("B20004_00",x)}) # median earnings by education attainment

# Transport ------------
zero_car_codes <- c("B08201_001","B08201_002")

# Earnings-----------
earnings_race_codes <- c("B20017A_001",
                         "B20017B_001",
                         "B20017C_001",
                         "B20017D_001",
                         "B20017E_001",
                         "B20017F_001",
                         "B20017G_001",
                         "B20017H_001",
                         "B20017I_001") # median earnings by race

# Migration---------
migration_edu_codes <- c(map_chr(seq(1,9),function(x){paste0("B07009_00",x)}),
                         map_chr(seq(10,36),function(x){paste0("B07009_0",x)}))# migration by educational attainment


acs_var = objects()
acs_var = mget(acs_var)

usethis::use_data(acs_var,overwrite = T)
