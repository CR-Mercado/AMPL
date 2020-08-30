
#rjson makes curl much easier for collecting the CoinGecko JSON. 
library(rjson)

# For Creating a coin ID/Name reference table ---- 
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

# Collecting Data ----
#' custom errors provided for prohibited or potentially confusing requests; 
#' i.e. I prevent auto-granularity from the API and require functions 
#' be called at the desired time level. Otherwise it may be confusing 
#' to accidentally get daily data when you expect hourly, or similarly. 

get_coin_1_day_minute_lvl <- function(coin_id){ 
  # Automatically returns only most recent 24 hours 

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

get_coin_N_days_daily_lvl <- function(coin_id, N){ 
  if(N < 90) { 
    stop("For N < 1, use get_coin_1_day_minute_lvl; for N < 90, 
         hourly level data is available. Use get_coin_N_day_hour_lvl")
  }
  if(N %% 1 != 0){ 
    stop("N must be a whole number.")
  }
  
  coin_url = paste0(
    "https://api.coingecko.com/api/v3/coins/",
    coin_id,
    "/market_chart?vs_currency=usd&days=", N)
  coin_data <- rjson::fromJSON(file = coin_url)  
  return(coin_data)
  }

# Examples

#' All items are lists that contain: 
#' "prices"        "market_caps"   "total_volumes"
#' At their specified levels; 
#' All time is in milliseconds since Jan 1 1970 (I think ... LOL)
btc_min <- get_coin_1_day_minute_lvl("bitcoin")
btc_hour <- get_coin_N_day_hour_lvl("bitcoin",N = 90)
btc_daily <- get_coin_N_days_daily_lvl("bitcoin", N = 365)


# Converting JSON to Data Frame ---- 
#' I want time to be the observation with 3 columns indicating the Price
#' Market_Cap and Total_Volume.
#' NOTE: in testing, I found the time stamps to be 100% matched across elements.
#' So only the price Time Stamp is used. 

convert_coin_data <- function(coin_data){
  # Takes ANY level of coin data and turns it into table of the form: 
  # Time | Price | Market_Cap | Volume
  
  data.frame( 
  Time = { 
    as.POSIXct(
      # the first value in each price element is the time stamp in milliseconds 
      # since Jan 1 1970. Thus, it's converted to a POSIXct in EST. 
    unlist( lapply(coin_data[["prices"]],`[`, 1) )/1000, 
    origin = "1970-01-01", 
    tz = "America/New_York")
  }, 
  Prices =  unlist( lapply(coin_data[["prices"]],`[`, 2) ),
  Market_Cap = unlist( lapply(coin_data[["market_caps"]],`[`, 2) ),
  Volume = unlist( lapply(coin_data[["total_volumes"]],`[`, 2) )
  )
  
}

btc_min_tbl <- convert_coin_data(btc_min)
btc_hour_tbl <- convert_coin_data(btc_hour)
btc_daily_tbl <- convert_coin_data(btc_daily)

# For cleaner sourcing 
rm(btc_min,btc_hour,btc_daily,btc_daily_tbl,btc_hour_tbl,btc_min_tbl)

