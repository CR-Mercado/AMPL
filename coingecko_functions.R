library(rjson)
get_coin_list <- function(names_only = FALSE){ 
coinlist_url = "https://api.coingecko.com/api/v3/coins/list"
coinlist <- rjson::fromJSON(file = coinlist_url)  

if(names_only == TRUE){ 
  
coinlist <- lapply(coinlist, function(x){ 
    x[["name"]]
    })

coinlist <- unlist(coinlist)
} else {
  return(coinlist) 
  }

return(coinlist)

}

get_coin_1_day_minute_lvl <- function(coin_id){ 
  coin_url = paste0(
    "https://api.coingecko.com/api/v3/coins/",
    coin_id,
    "/market_chart?vs_currency=usd&days=1")
  
  coin_data <- rjson::fromJSON(file = coin_url)  
  
  return(coin_data)

}

get_coin_N_day_hour_lvl <- function(coin_id, N){ 
  if(N <= 1 | N > 90) { 
    stop("For N < 1, use get_coin_1_day_minute_lvl; for N > 90, 
         only daily level data is available. Use get_coin_N_day_daily_lvl")
  }
  if(N %% 1 != 0){ 
    stop("N must be a whole number.")
    }
  
  coin_url = paste0(
    "https://api.coingecko.com/api/v3/coins/",
    coin_id,
    "/market_chart?vs_currency=usd&days=",N)
  coin_data <- rjson::fromJSON(file = coin_url)  
  return(coin_data)
  
}

get_coin_N_days_day_level <-

convert_coin_date <- function(coin_time){ 
  as.POSIXct(coin_time/1000, origin = "1970-01-01", tz = "America/New_York")
  }



