library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(gtable)
library(ggpubr)

# read file
power_wd <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/days/power_wd.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)


# ------------------------------------Preprocessing--------------------------------------####

# set DateTime as GMT
power_wd$DateTime <- as.POSIXct(power_wd$DateTime, tz ="GMT")

# factorize Year
power_wd$Year <- factor(power_wd$Year)


#----------------------------------Grouping/Aggregating----------------------------------####

# group by season
seasons <- group_by(power_wd, Season = (quarter(DateTime)))

# create timetag
power_wd <- mutate(power_wd, timetag = paste(Year,quarter(DateTime),days,formatC(hour(DateTime), width = 2, flag = "0"),sep = "/"))

# group by timetag
power_wd_timegrouped <- group_by(power_wd, timetag)

# summarise by mean
power_wd_agg <- summarise_all(power_wd_timegrouped, funs(mean), na.rm = T)

# separate timetag
power_wd_sep <- separate(power_wd_agg, timetag, c("Year", "Season", "Day","Hour"))


# -----------------------------------Filter days by season-------------------------------####

#### 2010 - season 1 ####
season1_2010 <- power_wd_sep %>% filter(Season == 1)
s1y2010 <- season1_2010 %>% filter(Year == 2010)
d1y2010s1 <- s1y2010 %>% filter(Day == "Montag")
d2y2010s1 <- s1y2010 %>% filter(Day == "Dienstag")
d3y2010s1 <- s1y2010 %>% filter(Day == "Mittwoch")
d4y2010s1 <- s1y2010 %>% filter(Day == "Donnerstag")
d5y2010s1 <- s1y2010 %>% filter(Day == "Freitag")


#### 2010 - season 2 ####
season2_2010 <- power_wd_sep %>% filter(Season == 2)
s2y2010 <- season2 %>% filter(Year == 2010)
d1y2010s2 <- s2y2010 %>% filter(Day == "Montag")
d2y2010s2 <- s2y2010 %>% filter(Day == "Dienstag")
d3y2010s2 <- s2y2010 %>% filter(Day == "Mittwoch")
d4y2010s2 <- s2y2010 %>% filter(Day == "Donnerstag")
d5y2010s2 <- s2y2010 %>% filter(Day == "Freitag")


#### 2010 - season 3 ####
season3_2010 <- power_wd_sep %>% filter(Season == 3)
s3y2010 <- season3 %>% filter(Year == 2010)
d1y2010s3 <- s3y2010 %>% filter(Day == "Montag")
d2y2010s3 <- s3y2010 %>% filter(Day == "Dienstag")
d3y2010s3 <- s3y2010 %>% filter(Day == "Mittwoch")
d4y2010s3 <- s3y2010 %>% filter(Day == "Donnerstag")
d5y2010s3 <- s3y2010 %>% filter(Day == "Freitag")


#### 2010 - season 4 ####
season4_2010 <- power_wd_sep %>% filter(Season == 4)
s4y2010 <- season4 %>% filter(Year == 2010)
d1y2010s4 <- s4y2010 %>% filter(Day == "Montag")
d2y2010s4 <- s4y2010 %>% filter(Day == "Dienstag")
d3y2010s4 <- s4y2010 %>% filter(Day == "Mittwoch")
d4y2010s4 <- s4y2010 %>% filter(Day == "Donnerstag")
d5y2010s4 <- s4y2010 %>% filter(Day == "Freitag")



#---------------------------------------Plotting----------------------------------------####

#-----------------------Season 1--------------------------####

# plot average power
pl_week_2010s1 <- ggplot() + 
  
  geom_line(data = d1y2010s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) + 
  geom_line(data = d2y2010s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  geom_line(data = d3y2010s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day), size=1.5) +
  geom_line(data = d4y2010s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  geom_line(data = d5y2010s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day), size=1.5) +
  #scale_color_manual(values = c())
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(name= "Hours of the day",breaks = c(0,4,8,12,16,20,23),labels = c("0 a.m","4 a.m", "8 a.m", "12 a.m", "16 a.m", "20 a.m", "23 a.m" )) + 
  
  ggtitle("Global Power Consumption") + theme(plot.title = element_text(hjust = 0.5))+
  
  ylab("Power in Kilowatt")

#print(pl_week_2010s1)


# plot kitchen power
pl_week_2010s1_kit <- ggplot() + 
  
  geom_line(data = d1y2010s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) + 
  geom_line(data = d2y2010s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d3y2010s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day), size=1.5) +
  geom_line(data = d4y2010s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d5y2010s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day), size=1.5) +
  
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(name= "Hours of the day",breaks = c(0,4,8,12,16,20,23),labels = c("0 a.m","4 a.m", "8 a.m", "12 a.m", "16 a.m", "20 a.m", "23 a.m" )) + 
  
  ggtitle("Kitchen Power Consumption") + theme(plot.title = element_text(hjust = 0.5))+
  
  ylab("Power in Kilowatt")

#print(pl_week_2010s1_kit)

# plot laundry power
pl_week_2010s1_laund <- ggplot() + 
  
  geom_line(data = d1y2010s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) + 
  geom_line(data = d2y2010s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d3y2010s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day), size=1.5) +
  geom_line(data = d4y2010s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d5y2010s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day), size=1.5) +
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(name= "Hours of the day",breaks = c(0,4,8,12,16,20,23),labels = c("0 a.m","4 a.m", "8 a.m", "12 a.m", "16 a.m", "20 a.m", "23 a.m" )) + 
  
  ggtitle("Laundry Power Consumption") + theme(plot.title = element_text(hjust = 0.5))+
  
  ylab("Power in Kilowatt")

#print(pl_week_2010s1_laund)

# plot water and air power
pl_week_2010s1_wa <- ggplot() + 
  
  geom_line(data = d1y2010s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) + 
  geom_line(data = d2y2010s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d3y2010s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d4y2010s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d5y2010s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(name= "Hours of the day",breaks = c(0,4,8,12,16,20,23),labels = c("0 a.m","4 a.m", "8 a.m", "12 a.m", "16 a.m", "20 a.m", "23 a.m" )) + 
  
  ggtitle("Water and Air Power Consumption") + theme(plot.title = element_text(hjust = 0.5))+
  ylab("Power in Kilowatt")

#print(pl_week_2010s1_wa)

# arrange grid for all 4 plots
grid.arrange(pl_week_2010s1,pl_week_2010s1_kit,pl_week_2010s1_laund,pl_week_2010s1_wa, nrow = 2,ncol = 2,top = text_grob ("Power consumption weekdays season 1, 2010", size=18))

