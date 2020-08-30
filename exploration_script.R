library(ggplot2)
library(forecast)

ampl <- read.csv("Data/ampleforth_2019-6-27_2020-7-30.csv",
                 check.names = FALSE,stringsAsFactors = FALSE)

colnames(ampl)[1] <- c("Date")
colnames(ampl)[7] <- "market_cap"
ampl$Date <- as.Date(ampl$Date,format = "%B-%d-%Y")
ampl$Day_Price_Change <- ampl$Close - ampl$Open
ampl$Day_Market_Change <- c(0, diff(ampl$market_cap))


ampl_plot <- function(dataset = ampl, xvar = "Date", yvar){ 
  ggplot(dataset, aes_string(x = xvar, y = yvar)) + geom_point(color = "blue") + 
    geom_line()
  }

price_change_plot <- ampl_plot(y = "Day_Price_Change")

mkt_change_plot <- ampl_plot(y = "Day_Market_Change")


july <- ampl[ampl$Date > as.Date("2020-06-30"),]

july_price_change <- ampl_plot(dataset = july, y = "Day_Price_Change")
july_mkt_change <- ampl_plot(dataset = july, y = "Day_Market_Change") 


rally_lengths <- diff(which(ampl$Day_Market_Change < 0))
train_length = floor(0.8 * length(rally_lengths))

train_rally_lengths <- auto.arima(
  rally_lengths[1:train_length]
  )

dip_length_prediction = predict(train_rally_lengths,
                     n.ahead = length(rally_lengths)-train_length)

real_dip = rally_lengths[(train_length+1):length(rally_lengths)]


ampl_200 <- ampl[ (nrow(ampl)-200):nrow(ampl), ]

plot( ampl_200$Day_Price_Change, ampl_200$Day_Market_Change)

ampl_center <- ampl[-c(1:200, 380:399), ]
ampl_mod <- auto.arima(ampl_center$Day_Market_Change)

predict_july10_july29 <- predict(ampl_mod, n.ahead = 20)
predict_july10_Aug8 <- predict(ampl_mod, n.ahead = 30)

plot(predict_july10_july29$pred, ampl$Day_Market_Change[380:399])
