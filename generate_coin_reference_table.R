source("coingecko_functions.R")
coinlist <- get_coin_list()
coin_ids <- unlist ( lapply(coinlist, `[[`, "id") )
coin_names <- unlist ( lapply(coinlist, `[[`, "name") )

coin_reference_tbl <- data.frame(coin_ids, coin_names, stringsAsFactors = FALSE)
write.csv(coin_reference_tbl, "coin_reference.csv")
