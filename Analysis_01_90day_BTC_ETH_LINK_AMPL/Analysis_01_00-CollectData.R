# Analysis 1: Comparing BTC, ETC, ChainLink, and AMPL over the past 90 days. 

# Step 00 Data Collection ---- 

# Data Collection Period: June 1, 2020 - August 29, 2020 
 # Note: Re-running this script on a different day gets different results due
 # to API limitations. Thus, data was collected and saved on August 30, 2020 
 # for future reproducibility. Some August 30th data will leak in,
 # but it is removed for analysis.

# Source Functions 
source("coingecko_functions.R")

# Get Coin IDs 
coin_reference_tbl <- read.csv("Data/coin_reference.csv")
desired_coins <- c("Bitcoin", "Ethereum","Ampleforth","ChainLink")
desired_ids <- coin_reference_tbl[
  coin_reference_tbl$coin_names %in% desired_coins, "coin_ids"]

for(i in desired_ids){ 
assign(x = i, value = convert_coin_data(get_coin_N_day_hour_lvl(i, N = 90)))
write.csv(x = get(i),
          file = paste0("Data/",i,"_",Sys.Date()-90,"_",Sys.Date(),".csv"))
}


