# Analysis 1: Comparing BTC, ETC, ChainLink, and AMPL over the past 90 days. 

library(lubridate)

# Step 01 Data Load ---- 

ampleforth <- read.csv(file = "Data/ampleforth_2020-06-01_2020-08-30.csv",
                       stringsAsFactors = FALSE)
bitcoin <- read.csv(file = "Data/bitcoin_2020-06-01_2020-08-30.csv",
                    stringsAsFactors = FALSE)
chainlink <- read.csv(file = "Data/chainlink_2020-06-01_2020-08-30.csv",
                      stringsAsFactors = FALSE)
ethereum <- read.csv(file = "Data/ethereum_2020-06-01_2020-08-30.csv",
                     stringsAsFactors = FALSE)

# Step 02 Cleanup and Feature Selection ---- 
 # combine into a list for easier manipulation en masse. 
my_coins <- list("ampleforth" = ampleforth,
                 "bitcoin" = bitcoin,
                 "chainlink" = chainlink,
                 "ethereum" = ethereum) 

# Not the same number of rows. 
lapply(my_coins, function(x){
nrow(x)
}) 

# fix their classes (I use CSV for easier portability, I could have used .rds to
# preserve the Time as a class POSIXct). 

my_coins <- lapply(X = my_coins,
       FUN = function(x){
         temp. <- x
         temp.[["Time"]] <- as.POSIXct(x[["Time"]])
         temp.
         })

# Reduce to days before Aug 30th. 
my_coins <- lapply(X = my_coins,
                   FUN = function(x){
                  temp. <- x 
                  temp. <- temp.[temp.$Time <= 
                                   as.POSIXct("2020-08-29 23:59:59 EDT"), ]
                  temp.
                   })

# Still not the same number of rows LOL. Oh well.  
lapply(my_coins, function(x){
  nrow(x)
}) 

min_row <- min(unlist(lapply(my_coins, function(x){
  nrow(x)
})))

# Chainlink has the fewest so I'll just reduce all to 2,149 rows.  

my_coins <- lapply(X = my_coins, FUN = function(x){ 
  temp. <- x 
  temp. <- temp.[1:min_row, ]
  temp. 
  })

# All 2149 now. 
lapply(my_coins, function(x){
  nrow(x)
}) 

# 
                  