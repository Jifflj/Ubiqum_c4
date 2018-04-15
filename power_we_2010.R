library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(gtable)
library(ggpubr)

# read file
power_we <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/days/power_we.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)

# set DateTime as GMT
power_we$DateTime <- as.POSIXct(power_we$DateTime, tz ="GMT")

# factorize Year
power_we$Year <- factor(power_we$Year)

####---------------------------------Grouping/Aggregating----------------------------------####

# group by season
seasons <- group_by(power_we, Season = (quarter(DateTime)))

# create timetag
power_we <- mutate(power_we, timetag = paste(Year,quarter(DateTime),days,formatC(hour(DateTime), width = 2, flag = "0"),sep = "/"))

# group by timetag
power_we_timegrouped <- group_by(power_we, timetag)

# summarise by mean
power_we_agg <- summarise_all(power_we_timegrouped, funs(mean), na.rm =T)

# separate timetag
power_we_sep <- separate(power_we_agg, timetag, c("Year", "Season", "Day","Hour"))


# -----------------Filter days by season------------------####

#### 2010 - season 1 ####
season1_2010 <- power_we_sep %>% filter(Season == 1)
s1y2010 <- season1_2010 %>% filter(Year == 2010)

d6y2010s1 <- s1y2010 %>% filter(Day == "Samstag")
d7y2010s1 <- s1y2010 %>% filter(Day == "Sonntag")

#### 2010 - season 2 ####
season2_2010 <- power_we_sep %>% filter(Season == 2)
s2y2010 <- season2_2010 %>% filter(Year == 2010)

d6y2010s2 <- s2y2010 %>% filter(Day == "Samstag")
d7y2010s2 <- s2y2010 %>% filter(Day == "Sonntag")

#### 2010 - season 3 ####
season3_2010 <- power_we_sep %>% filter(Season == 3)
s3y2010 <- season3_2010 %>% filter(Year == 2010)

d6y2010s3 <- s3y2010 %>% filter(Day == "Samstag")
d7y2010s3 <- s3y2010 %>% filter(Day == "Sonntag")

#### 2010 - season 4 ####
season4_2010 <- power_we_sep %>% filter(Season == 4)
s4y2010 <- season4_2010 %>% filter(Year == 2010)

d6y2010s4 <- s4y2010 %>% filter(Day == "Samstag")
d7y2010s4 <- s4y2010 %>% filter(Day == "Sonntag")


####---------------------------------------Plotting------------------------------------------####

####------------------Season 1------------------####

# plot average power
pl_weekend_2010s1 <- ggplot() + 
  
  geom_line(data = d6y2010s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day), size=1.5) + 
  geom_line(data = d7y2010s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day), size=1.5) +
  #scale_color_manual(values = c())
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(name= "Hours of the day",breaks = c(0,4,8,12,16,20,23),labels = c("0 a.m","4 a.m", "8 a.m", "12 a.m", "16 a.m", "20 a.m", "23 a.m" )) + 
  
  ggtitle("Global Power Consumption") + theme(plot.title = element_text(hjust = 0.5))+
  
  ylab("Power in Kilowatt")

#print(pl_weekend_2010s1)


# plot kitchen power
pl_weekend_2010s1_kit <- ggplot() + 
  
  geom_line(data = d6y2010s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day), size=1.5) + 
  geom_line(data = d7y2010s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day), size=1.5) +
  
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(name= "Hours of the day",breaks = c(0,4,8,12,16,20,23),labels = c("0 a.m","4 a.m", "8 a.m", "12 a.m", "16 a.m", "20 a.m", "23 a.m" )) + 
  
  ggtitle("Kitchen Power Consumption") + theme(plot.title = element_text(hjust = 0.5))+
  
  ylab("Power in Kilowatt")

#print(pl_weekend_2010s1_kit)

# plot laundry power
pl_weekend_2010s1_laund <- ggplot() + 
  
  geom_line(data = d6y2010s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day), size=1.5) + 
  geom_line(data = d7y2010s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day), size=1.5) +

  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(name= "Hours of the day",breaks = c(0,4,8,12,16,20,23),labels = c("0 a.m","4 a.m", "8 a.m", "12 a.m", "16 a.m", "20 a.m", "23 a.m" )) + 
  
  ggtitle("Laundry Power Consumption") + theme(plot.title = element_text(hjust = 0.5))+
  
  ylab("Power in Kilowatt")

#print(pl_weekend_2010s1_laund)

# plot water and air power
pl_weekend_2010s1_wa <- ggplot() + 
  
  geom_line(data = d6y2010s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day), size=1.5) + 
  geom_line(data = d7y2010s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day), size=1.5) +
  
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(name= "Hours of the day",breaks = c(0,4,8,12,16,20,23),labels = c("0 a.m","4 a.m", "8 a.m", "12 a.m", "16 a.m", "20 a.m", "23 a.m" )) + 
  
  ggtitle("Water and Air Power Consumption") + theme(plot.title = element_text(hjust = 0.5))+
  ylab("Power in Kilowatt")

#print(pl_weekend_2010s1_wa)


grid.arrange(pl_weekend_2010s1,pl_weekend_2010s1_kit,pl_weekend_2010s1_laund,pl_weekend_2010s1_wa, nrow = 2,ncol = 2,top = text_grob ("Power consumption week-end-days 2010, season 1", size=18))


####------------------------Season 2----------------------####

# plot average power
pl_weekend_2010s2_av <- ggplot() + 
  
  geom_line(data = d6y2010s2, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) + 
  geom_line(data = d7y2010s2, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Average Power weekends 2010, season 2") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2010s2_av)

# plot kitchen power
pl_weekend_2010s2_kit <- ggplot() + 
  
  geom_line(data = d6y2010s2, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) + 
  geom_line(data = d7y2010s2, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Kitchen Power weekends 2010 season 2") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2010s2_kit)

# plot laundry power
pl_weekend_2010s2_laund <- ggplot() + 
  
  geom_line(data = d6y2010s2, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) + 
  geom_line(data = d7y2010s2, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Laundry Power consumption weekends 2010 season 2") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2010s2_laund)

# plot water and air power
pl_weekend_2010s2_wa <- ggplot() + 
  
  geom_line(data = d1y2010s2, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) + 
  geom_line(data = d2y2010s2, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Water and Air Power consumption on weekends 2010, season 2") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2010s2_wa)


####-----------------------Season 3---------------------------####

# plot average power
pl_weekend_2010s3_av <- ggplot() + 
  
  geom_line(data = d6y2010s3, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) + 
  geom_line(data = d7y2010s3, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Average Power weekends 2010, season 3") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2010s3_av)

# plot kitchen power
pl_weekend_2010s3_kit <- ggplot() + 
  
  geom_line(data = d6y2010s3, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) + 
  geom_line(data = d7y2010s3, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Kitchen Power weekends 2010 season 3") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2010s3_kit)

# plot laundry power
pl_weekend_2010s3_laund <- ggplot() + 
  
  geom_line(data = d6y2010s3, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) + 
  geom_line(data = d7y2010s3, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Laundry Power consumption weekends 2010 season 3") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2010s3_laund)

# plot water and air power
pl_weekend_2010s3_wa <- ggplot() + 
  
  geom_line(data = d1y2010s3, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) + 
  geom_line(data = d2y2010s3, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Water and Air Power consumption on weekends 2010, season 3") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2010s3_wa)


####--------------------------Season 4-----------------------------####

# plot average power
pl_weekend_2010s4_av <- ggplot() + 
  
  geom_line(data = d6y2010s4, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) + 
  geom_line(data = d7y2010s4, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Average Power weekends 2010, season 4") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2010s4_av)

# plot kitchen power
pl_weekend_2010s4_kit <- ggplot() + 
  
  geom_line(data = d6y2010s4, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) + 
  geom_line(data = d7y2010s4, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Kitchen Power weekends 2010 season 4") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2010s4_kit)

# plot laundry power
pl_weekend_2010s4_laund <- ggplot() + 
  
  geom_line(data = d6y2010s4, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) + 
  geom_line(data = d7y2010s4, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Laundry Power consumption weekends 2010 season 4") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2010s4_laund)

# plot water and air power
pl_weekend_2010s4_wa <- ggplot() + 
  
  geom_line(data = d1y2010s4, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) + 
  geom_line(data = d2y2010s4, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Water and Air Power consumption on weekends 2010, season 4") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2010s4_wa)
