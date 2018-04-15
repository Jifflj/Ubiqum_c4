library(forecast)
library(ggplot2)
library(reshape)

winter_days_mean <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/seasons_days_mean/winter_days_mean.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)
spring_days_mean <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/seasons_days_mean/spring_days_mean.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)
summer_days_mean <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/seasons_days_mean/summer_days_mean.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)
automn_days_mean <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/seasons_days_mean/automn_days_mean.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)

#### HW function ####
HWplot <- function(series_winter_days_global,  n.ahead = 24,  CI = .95,  error.ribbon = 'green', line.size = 1){
  
  hw <- HoltWinters(series_winter_days_global)
  
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
    xlab('Time') + ylab('Global Consumption Winter days') + ggtitle("Holt-Winters forecast") + theme(plot.title = element_text(hjust = 0.5))
  return(p)
  
}


#### winter series ####
series_winter_days_global <- ts(winter_days_mean$household_minute_averaged_active_power, frequency=24, start=c(2007,1))
series_winter_days_kitchen <- ts(winter_days_mean$kitchen, frequency=24, start=c(2007,1))
series_winter_days_laundry <- ts(winter_days_mean$laundry_room, frequency=24, start=c(2007,1))
series_winter_days_wa <- ts(winter_days_mean$water_air, frequency=24, start=c(2007,1))

# plot winter series
plot(series_winter_days_global)
plot(series_winter_days_kitchen)
plot(series_winter_days_laundry)
plot(series_winter_days_wa)

#### TSLM ####
# global
my_df_ts <- data.frame(temperature = series_winter_days_global, as.numeric(time(series_winter_days_global)))
names(my_df_ts) <- c("Global_Consumption", "time")

mymodel <- tslm(Global_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)  + xlab("Time") + ylab("Global Consumption Winter Days") + ggtitle("TSLM forecast") + theme(plot.title = element_text(hjust = 0.5))

# kitchen
my_df_ts <- data.frame(temperature = series_winter_days_kitchen, as.numeric(time(series_winter_days_kitchen)))
names(my_df_ts) <- c("kitchen_Consumption", "time")

mymodel <- tslm(kitchen_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# laundry
my_df_ts <- data.frame(temperature = series_winter_days_laundry, as.numeric(time(series_winter_days_laundry)))
names(my_df_ts) <- c("laundry_Consumption", "time")

mymodel <- tslm(laundry_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# water & air
my_df_ts <- data.frame(temperature = series_winter_days_wa, as.numeric(time(series_winter_days_wa)))
names(my_df_ts) <- c("Water&Air_Consumption", "time")

mymodel <- tslm(Water&Air_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# Holt Winters
#hw <- HoltWinters(series_winter_days_global)
#plot(hw)

HWplot(series_winter_days_global) + xlim(2007,2012)

# forecast
#forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
#plot(hw, forecast)


#### spring series ####
series_spring_days_global <- ts(spring_days_mean$household_minute_averaged_active_power, frequency=24, start=c(2007,1))
series_spring_days_kitchen <- ts(spring_days_mean$kitchen, frequency=24, start=c(2007,1))
series_spring_days_laundry <- ts(spring_days_mean$laundry_room, frequency=24, start=c(2007,1))
series_spring_days_wa <- ts(spring_days_mean$water_air, frequency=24, start=c(2007,1))

# plot spring series
plot(series_spring_days_global)
plot(series_spring_days_kitchen)
plot(series_spring_days_laundry)
plot(series_spring_days_wa)

#### TSLM ####
# global
my_df_ts <- data.frame(temperature = series_spring_days_global, as.numeric(time(series_spring_days_global)))
names(my_df_ts) <- c("Global_Consumption", "time")

mymodel <- tslm(Global_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# kitchen
my_df_ts <- data.frame(temperature = series_spring_days_kitchen, as.numeric(time(series_spring_days_kitchen)))
names(my_df_ts) <- c("kitchen_Consumption", "time")

mymodel <- tslm(kitchen_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# laundry
my_df_ts <- data.frame(temperature = series_spring_days_laundry, as.numeric(time(series_spring_days_laundry)))
names(my_df_ts) <- c("laundry_Consumption", "time")

mymodel <- tslm(laundry_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# water & air
my_df_ts <- data.frame(temperature = series_spring_days_wa, as.numeric(time(series_spring_days_wa)))
names(my_df_ts) <- c("Water&Air_Consumption", "time")

mymodel <- tslm(Water&Air_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# Holt Winters

HWplot(seasons_series_global) + xlim(2007,2012)

#hw <- HoltWinters(series_winter_days_global, alpha = 0.3, beta = 0.3, gamma = 0.3)
#plot(hw)

#forecast <- predict(hw, n.ahead = 24, prediction.interval = T, level = 0.95)
#plot(hw, forecast)


# Holt-Winters
hw <- HoltWinters(series_spring_days_global, alpha = 0.5, beta = 0.5, gamma = 0.5, seasonal="multiplicative")
plot(hw)

forecast <- predict(hw, n.ahead = 24, prediction.interval = T, level = 0.95)
plot(hw, forecast)

#### summer series ####
series_summer_days_global <- ts(summer_days_mean$household_minute_averaged_active_power, frequency=24, start=c(2007,1))
series_summer_days_kitchen <- ts(summer_days_mean$kitchen, frequency=24, start=c(2007,1))
series_summer_days_laundry <- ts(summer_days_mean$laundry_room, frequency=24, start=c(2007,1))
series_summer_days_wa <- ts(summer_days_mean$water_air, frequency=24, start=c(2007,1))

# plot summer series
plot(series_summer_days_global)
plot(series_summer_days_kitchen)
plot(series_summer_days_laundry)
plot(series_summer_days_wa)

# Linear model
fit.summer_days_lm <- lm(household_minute_averaged_active_power ~ Month, data = summer_days_mean)
plot(fit.summer_days_lm)

#### TSLM ####
# global
my_df_ts <- data.frame(temperature = series_summer_days_global, as.numeric(time(series_summer_days_global)))
names(my_df_ts) <- c("Global_Consumption", "time")

mymodel <- tslm(Global_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# kitchen
my_df_ts <- data.frame(temperature = series_summer_days_kitchen, as.numeric(time(series_summer_days_kitchen)))
names(my_df_ts) <- c("kitchen_Consumption", "time")

mymodel <- tslm(kitchen_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# laundry
my_df_ts <- data.frame(temperature = series_summer_days_laundry, as.numeric(time(series_summer_days_laundry)))
names(my_df_ts) <- c("laundry_Consumption", "time")

mymodel <- tslm(laundry_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# water & air
my_df_ts <- data.frame(temperature = series_summer_days_wa, as.numeric(time(series_summer_days_wa)))
names(my_df_ts) <- c("Water&Air_Consumption", "time")

mymodel <- tslm(Water&Air_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# Holt Winters
hw <- HoltWinters(series_winter_days_global)
plot(hw)

# forecast
forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)



# Holt Winters
hw <- Holtsummers(series_summer_days_global)
plot(hw)

# forecast
forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)

#### automn series ####
series_automn_days_global <- ts(automn_days_mean$household_minute_averaged_active_power, frequency=24, start=c(2007,1))
series_automn_days_kitchen <- ts(automn_days_mean$kitchen, frequency=24, start=c(2007,1))
series_automn_days_laundry <- ts(automn_days_mean$laundry_room, frequency=24, start=c(2007,1))
series_automn_days_wa <- ts(automn_days_mean$water_air, frequency=24, start=c(2007,1))

# plot automn series
plot(series_automn_days_global)
plot(series_automn_days_kitchen)
plot(series_automn_days_laundry)
plot(series_automn_days_wa)

# Linear model
fit.automn_days_lm <- lm(household_minute_averaged_active_power ~ Month, data = automn_days_mean)
plot(fit.automn_days_lm)

#### TSLM ####
# global
my_df_ts <- data.frame(temperature = series_automn_days_global, as.numeric(time(series_automn_days_global)))
names(my_df_ts) <- c("Global_Consumption", "time")

mymodel <- tslm(Global_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# kitchen
my_df_ts <- data.frame(temperature = series_automn_days_kitchen, as.numeric(time(series_automn_days_kitchen)))
names(my_df_ts) <- c("kitchen_Consumption", "time")

mymodel <- tslm(kitchen_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# laundry
my_df_ts <- data.frame(temperature = series_automn_days_laundry, as.numeric(time(series_automn_days_laundry)))
names(my_df_ts) <- c("laundry_Consumption", "time")

mymodel <- tslm(laundry_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# water & air
my_df_ts <- data.frame(temperature = series_automn_days_wa, as.numeric(time(series_automn_days_wa)))
names(my_df_ts) <- c("Water&Air_Consumption", "time")

mymodel <- tslm(Water&Air_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# Holt Winters
hw <- HoltWinters(series_winter_days_global)
plot(hw)

# forecast
forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)



# Holt Winters
hw <- Holtautomns(series_automn_days_global)
plot(hw)

# forecast
forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)


#### decomposed winter_days over years ####
series_winter_days_global_dc <- decompose(series_winter_days_global)
series_winter_days_kitchen_dc <- decompose(series_seasons_day_kitchen)
series_winter_days_laundry_dc <- decompose(series_seasons_day_laundry)
series_winter_days_wa_dc <- decompose(series_seasons_day_wa)

# plot decomposed
plot(series_winter_days_global_dc)
plot(series_winter_days_kitchen_dc)
plot(series_winter_days_laundry_dc)
plot(series_winter_days_wa_dc)

#### decomposed spring_days over years ####
series_spring_days_global_dc <- decompose(series_seasons_day_global)
series_spring_days_kitchen_dc <- decompose(series_seasons_day_kitchen)
series_spring_days_laundry_dc <- decompose(series_seasons_day_laundry)
series_spring_days_wa_dc <- decompose(series_seasons_day_wa)

# plot decomposed
plot(series_spring_days_global_dc)
plot(series_spring_days_kitchen_dc)
plot(series_spring_days_laundry_dc)
plot(series_spring_days_wa_dc)

#### decomposed summer_days over years ####
series_summer_days_global_dc <- decompose(series_seasons_day_global)
series_summer_days_kitchen_dc <- decompose(series_seasons_day_kitchen)
series_summer_days_laundry_dc <- decompose(series_seasons_day_laundry)
series_summer_days_wa_dc <- decompose(series_seasons_day_wa)

# plot decomposed
plot(series_summer_days_global_dc)
plot(series_summer_days_kitchen_dc)
plot(series_summer_days_laundry_dc)
plot(series_summer_days_wa_dc)

#### decomposed automn_days over years ####
series_automn_days_global_dc <- decompose(series_seasons_day_global)
series_automn_days_kitchen_dc <- decompose(series_seasons_day_kitchen)
series_automn_days_laundry_dc <- decompose(series_seasons_day_laundry)
series_automn_days_wa_dc <- decompose(series_seasons_day_wa)

# plot decomposed
plot(series_automn_days_global_dc)
plot(series_automn_days_kitchen_dc)
plot(series_automn_days_laundry_dc)
plot(series_automn_days_wa_dc)
