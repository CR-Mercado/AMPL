# Analysis 1: Comparing BTC, ETC, ChainLink, and AMPL over the past 90 days. 

library(lubridate)
library(ggplot2)

# Step 01 Data Load ---- 

ampleforth <- read.csv(file = "Data/ampleforth_2020-06-01_2020-08-30.csv",
                       stringsAsFactors = FALSE)
bitcoin <- read.csv(file = "Data/bitcoin_2020-06-01_2020-08-30.csv",
                    stringsAsFactors = FALSE)
chainlink <- read.csv(file = "Data/chainlink_2020-06-01_2020-08-30.csv",
                      stringsAsFactors = FALSE)
ethereum <- read.csv(file = "Data/ethereum_2020-06-01_2020-08-30.csv",
                     stringsAsFactors = FALSE)

# Step 02 Cleanup ---- 
#' Mostly just looking around for weird patterns. 

 # combine into a list for easier manipulation en masse. 
my_coins <- list("ampleforth" = ampleforth,
                 "bitcoin" = bitcoin,
                 "chainlink" = chainlink,
                 "ethereum" = ethereum) 

# Not the same number of rows. 
lapply(my_coins, function(x){
nrow(x)
}) 

# fix their classes (I used CSV for easier portability, 
# you can use .rds to preserve the Time as a class POSIXct). 

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

# Step 03 Correlation ---- 

#' Curious about both general correlation in the last 2,100+ hours 
#' and rolling 24 hour correlations. 

get_correlation_tbl <- function(my_coins, index_range = NULL){ 
# gets the correlation table for a particular index range

correlation_tbl <- data.frame(coin1 = character(),
                              coin2 = character(),
                              correlation = numeric())

if( is.null(index_range) | 
    !(class(index_range) %in% c("numeric","integer")) ){
  index_range <- 1:nrow(my_coins[[1]])
}

for(i in names(my_coins)){ 
  for(j in names(my_coins)){
    x = my_coins[[i]]
    y = my_coins[[j]]
    correlation_tbl <- rbind(correlation_tbl, 
                             data.frame(coin1 = i,
                                        coin2 = j,
                                        correlation = 
                                          cor(
                          x$Market_Cap[index_range],
                          y$Market_Cap[index_range]))
                             ) 
    }
}

return(correlation_tbl)
}

get_correlation_plot <- function(correlation_tbl){ 

correlation_plot <- ggplot(data = correlation_tbl,
       aes(x = coin1, y = coin2)) + 
  geom_label(aes(label = round(correlation, 2),
                 col = round(correlation, 2))) +
  labs(title = "Total Market Cap Correlation",
       sub = "June 1, 2020 - Aug 29, 2020",
       x = "", y = "") + 
  theme_classic() + theme(axis.text = element_text(size = 14),
                          title = element_text(size = 14,
                                               hjust = 0.5))

correlation_plot$labels$colour <- NULL

return(correlation_plot)
} 

general_correlation_tbl <- get_correlation_tbl(my_coins)

get_correlation_plot(general_correlation_tbl)


# Go along a series of indexes to get a rolling correlation.

select_sequence <- list()
for(i in 1:(min_row-24)){ 
  select_sequence[[i]] <- i:(i+24)  
}

rolling_correlations <- lapply(select_sequence, 
                               FUN = function(x){ 
                                 get_correlation_tbl(my_coins, 
                                                         index_range = x)
                                 })


get_rolling_coin_corrs <- function(rolling_correlations, coin1, coin2){ 
  # outputs the numeric rolling correlations 
  
  unlist ( lapply(rolling_correlations, FUN = function(x){ 
    x[x$coin1 == coin1 & x$coin2 == coin2, "correlation"]
    })
  ) 
  
  }

ampl_btc_rolling_corrs <- get_rolling_coin_corrs(rolling_correlations,
                                                 "ampleforth",
                                                 "bitcoin")









