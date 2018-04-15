library(forecast)
library(ggplot2)
library(reshape2)

setwd("/home/johannes/Documents/Ubiqum/Course4/task1/data")
months_mean <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/hhpower_months_mean.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)

#### HW function ####
HWplot <- function(month_series_global,  n.ahead = 12,  CI = .95,  error.ribbon = 'green', line.size = 1){
  
  hw <- HoltWinters(month_series_global)
  
  forecast <- predict(hw, n.ahead = n.ahead,  prediction.interval = T,  level = CI)
  
  
  for_values <- data.frame(time = round(time(forecast),  3),  value_forecast = as.data.frame(forecast)$fit,  dev = as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
  
  fitted_values <- data.frame(time = round(time(hw$fitted),  3),  value_fitted = as.data.frame(hw$fitted)$xhat)
  
  actual_values <- data.frame(time = round(time(hw$x),  3),  Actual = c(hw$x))
  
  
  graphset <- merge(actual_values,  fitted_values,  by = 'time',  all = TRUE)
  graphset <- merge(graphset,  for_values,  all = TRUE,  by = 'time')
  graphset[is.na(graphset$dev),  ]$dev<-0
  
  graphset$Fitted <- c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  for_values$value_forecast)
  
  
  graphset.melt <- melt(graphset[, c('time', 'Actual', 'Fitted')], id = 'time')
  
  p <- ggplot(graphset.melt,  aes(x = time,  y = value)) + 
    geom_ribbon(data = graphset, aes(x = time, y = Fitted, ymin = Fitted-dev,  ymax = Fitted + dev),  alpha = .2,  fill = error.ribbon) + 
    geom_line(aes(colour = variable), size = line.size) + geom_vline(x = max(actual_values$time),  xintercept = 2) + 
    xlab('Time') + ylab('Global Consumption Months') + ggtitle("Holt-Winters forecast") + theme(plot.title = element_text(hjust = 0.5))
  return(p)
  
}

# call function for plot
HWplot(month_series_global) + xlim(2007,2012)

#### TS monthly ####
month_series_global <- ts(months_mean$household_minute_averaged_active_power, frequency=12, start=c(2007,1))
month_series_kitchen <- ts(months_mean$kitchen, frequency=12, start=c(2007,1))
month_series_laundry <- ts(months_mean$laundry_room, frequency=12, start=c(2007,1))
month_series_wa <- ts(months_mean$water_air, frequency=12, start=c(2007,1))

# plot monthly
plot.ts(month_series_global)
plot.ts(month_series_kitchen)
plot.ts(month_series_laundry)
plot.ts(month_series_wa)

#### TSLM ####
# global
my_df_ts <- data.frame(temperature = month_series_global, as.numeric(time(month_series_global)))
names(my_df_ts) <- c("Global_Consumption", "time")

mymodel <- tslm(Global_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=12)
autoplot(my_fc) + xlab("Time") + ylab("Global Consumption Months")

# kitchen
my_df_ts <- data.frame(temperature = month_series_kitchen, as.numeric(time(month_series_kitchen)))
names(my_df_ts) <- c("Kitchen_Consumption", "time")

mymodel <- tslm(Kitchen_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=12)
autoplot(my_fc)

# laundry
my_df_ts <- data.frame(temperature = month_series_laundry, as.numeric(time(month_series_laundry)))
names(my_df_ts) <- c("Laundry_Consumption", "time")

mymodel <- tslm(Laundry_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=12)
autoplot(my_fc)

# water & air
my_df_ts <- data.frame(temperature = month_series_wa, as.numeric(time(month_series_wa)))
names(my_df_ts) <- c("Water&Air_Consumption", "time")

mymodel <- tslm(Water&Air_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=12)
autoplot(my_fc)

#### Holt Winters ####
# global
hw <- HoltWinters(month_series_global)
plot(hw)

forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)

# kitchen
hw <- HoltWinters(month_series_kitchen)
plot(hw)

forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)

# laundry
hw <- HoltWinters(month_series_laundry)
plot(hw)

forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)

# water & air
hw <- HoltWinters(month_series_wa)
plot(hw)

forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)


#### decomposed monthly over years ####
month_series_global_dc <- decompose(month_series_global)
month_series_kitchen_dc <- decompose(month_series_kitchen)
month_series_laundry_dc <- decompose(month_series_laundry)
month_series_wa_dc <- decompose(month_series_wa)

# plot decomposed
plot(month_series_global_dc)
plot(month_series_kitchen_dc)
plot(month_series_laundry_dc)
plot(month_series_wa_dc)



