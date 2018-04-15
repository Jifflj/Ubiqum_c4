library(forecast)
library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(gtable)
library(ggpubr)

months_mean <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/hhpower_months_mean.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)

# Preprocessing #
months_mean$year <- factor(months_mean$year)


# --------------------------------------------------------Plotting----------------------------------------------####
# Global ####
pl_season_global <- ggplot() + 
  
  geom_line(data = months_mean, aes(x = Month, y = household_minute_averaged_active_power , 
                                  color = year), size=1.5) + 
  
  #scale_color_manual(values = c())
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(breaks=1:12) + 
  
  ggtitle("Global Power Consumption") + theme(plot.title = element_text(hjust = 0.5))+
  
  ylab("Power in Kilowatt")

#print(pl_season)

# Kichen ####
pl_season_kitchen <- ggplot() + 
  
  geom_line(data = months_mean, aes(x = Month, y = kitchen, 
                                    color = year), size=1.5) + 
  
  #scale_color_manual(values = c())
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(breaks=1:12) + 
  
  ggtitle("Kitchen Power Consumption") + theme(plot.title = element_text(hjust = 0.5))+
  
  ylab("Power in Kilowatt")

# Laundry ####
pl_season_laundry <- ggplot() + 
  
  geom_line(data = months_mean, aes(x = Month, y = laundry_room, 
                                    color = year), size=1.5) + 
  
  #scale_color_manual(values = c())
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(breaks=1:12) + 
  
  ggtitle("Laundry Power Consumption") + theme(plot.title = element_text(hjust = 0.5))+
  
  ylab("Power in Kilowatt")

# Water/Air ####
pl_season_wa <- ggplot() + 
  
  geom_line(data = months_mean, aes(x = Month, y = water_air, 
                                    color = year), size=1.5) + 
  
  #scale_color_manual(values = c())
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(breaks=1:12) + 
  
  ggtitle("Water & Air Consumption") + theme(plot.title = element_text(hjust = 0.5))+
  
  ylab("Power in Kilowatt")

# Not measured ####
pl_season_nm <- ggplot() + 
  
  geom_area(data = months_mean, aes(x = Month, y = not_measured_in_sub, 
                                    color = year), size=1.5) + 
  
  #scale_color_manual(values = c())
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(breaks=1:12) + 
  
  ggtitle("Not measured Consumption") + theme(plot.title = element_text(hjust = 0.5))+
  
  ylab("Power in Kilowatt")

print(pl_season_nm)

# create a grid for first 4 plots
grid.arrange(pl_season_global,pl_season_kitchen,pl_season_laundry, pl_season_wa, nrow = 2,ncol = 2,top = text_grob ("Power consumption per month over years", size=18))
