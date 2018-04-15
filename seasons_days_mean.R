library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(gtable)
library(ggpubr)

seasons_days <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/power_days.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)

seasons_days$DateTime <- as.POSIXct(seasons_days$DateTime, tz ="GMT")

####----------------------------------Grouping/Aggregating----------------------------------####

# group by season
seasons <- group_by(seasons_days, Season = (quarter(DateTime)))




seasons_days <- mutate(seasons_days, timetag = paste(Year,quarter(DateTime),days,formatC(hour(DateTime), width = 2, flag = "0"),sep = "/"))
power_seasons_timegrouped <- group_by(seasons_days, timetag)

power_seasons_agg <- summarise_all(power_seasons_timegrouped, funs(mean), na.rm = T)

power_seasons_sep <- separate(power_seasons_agg, timetag, c("Year", "Season", "Day","Hour"))
power_seasons_sep$Day <- NULL
power_seasons_sep$Hour <- NULL

power_seasons_sep$Year <- as.numeric(power_seasons_sep$Year)
power_seasons_sep$Season <- as.numeric(power_seasons_sep$Season)

####2007####
#### 2007 - season 1 ####
season1 <- power_seasons_sep %>% filter(Season == 1)
s1y2007 <- season1 %>% filter(Year == 2007)
s1y2007 <- mutate(s1y2007, daytag = hour(DateTime))

my_group <- group_by(s1y2007, daytag)
winter_day_2007 <- summarise_all(my_group, funs(mean))

#### 2007 - season 2 ####
season2 <- power_seasons_sep %>% filter(Season == 2)
s2y2007 <- season2 %>% filter(Year == 2007)
s2y2007 <- mutate(s1y2007, daytag = hour(DateTime))

my_group <- group_by(s1y2007, daytag)
spring_day_2007 <- summarise_all(my_group, funs(mean))

#### 2007 - season 3 ####
season3 <- power_seasons_sep %>% filter(Season == 3)
s3y2007 <- season3 %>% filter(Year == 2007)
s3y2007 <- mutate(s1y2007, daytag = hour(DateTime))

my_group <- group_by(s1y2007, daytag)
summer_day_2007 <- summarise_all(my_group, funs(mean))

#### 2007 - season 4 ####
season4 <- power_seasons_sep %>% filter(Season == 4)
s4y2007 <- season4 %>% filter(Year == 2007)
s4y2007 <- mutate(s1y2007, daytag = hour(DateTime))

my_group <- group_by(s1y2007, daytag)
automn_day_2007 <- summarise_all(my_group, funs(mean))

####2008####
#### 2008 - season 1 ####
#season1 <- power_seasons_sep %>% filter(Season == 1)
s1y2008 <- season1 %>% filter(Year == 2008)
s1y2008 <- mutate(s1y2008, daytag = hour(DateTime))

my_group <- group_by(s1y2008, daytag)
winter_day_2008 <- summarise_all(my_group, funs(mean))

#### 2008 - season 2 ####
#season1 <- power_seasons_sep %>% filter(Season == 2)
s2y2008 <- season2 %>% filter(Year == 2008)
s2y2008 <- mutate(s1y2008, daytag = hour(DateTime))

my_group <- group_by(s1y2008, daytag)
spring_day_2008 <- summarise_all(my_group, funs(mean))

#### 2008 - season 3 ####
#season3 <- power_seasons_sep %>% filter(Season == 3)
s3y2008 <- season3 %>% filter(Year == 2008)
s3y2008 <- mutate(s1y2008, daytag = hour(DateTime))

my_group <- group_by(s1y2008, daytag)
summer_day_2008 <- summarise_all(my_group, funs(mean))

#### 2008 - season 4 ####
#season4 <- power_seasons_sep %>% filter(Season == 4)
s4y2008 <- season4 %>% filter(Year == 2008)
s4y2008 <- mutate(s1y2008, daytag = hour(DateTime))

my_group <- group_by(s1y2008, daytag)
automn_day_2008 <- summarise_all(my_group, funs(mean))

###2009####
#### 2009 - season 1 ####
#season1 <- power_seasons_sep %>% filter(Season == 1)
s1y2009 <- season1 %>% filter(Year == 2009)
s1y2009 <- mutate(s1y2009, daytag = hour(DateTime))

my_group <- group_by(s1y2009, daytag)
winter_day_2009 <- summarise_all(my_group, funs(mean))

#### 2009 - season 2 ####
#season2 <- power_seasons_sep %>% filter(Season == 2)
s2y2009 <- season2 %>% filter(Year == 2009)
s2y2009 <- mutate(s1y2009, daytag = hour(DateTime))

my_group <- group_by(s1y2009, daytag)
spring_day_2009 <- summarise_all(my_group, funs(mean))

#### 2009 - season 3 ####
#season3 <- power_seasons_sep %>% filter(Season == 3)
s3y2009 <- season3 %>% filter(Year == 2009)
s3y2009 <- mutate(s1y2009, daytag = hour(DateTime))

my_group <- group_by(s1y2009, daytag)
summer_day_2009 <- summarise_all(my_group, funs(mean))

#### 2009 - season 4 ####
#season4 <- power_seasons_sep %>% filter(Season == 4)
s4y2009 <- season4 %>% filter(Year == 2009)
s4y2009 <- mutate(s1y2009, daytag = hour(DateTime))

my_group <- group_by(s1y2009, daytag)
automn_day_2009 <- summarise_all(my_group, funs(mean))

####2010####
#### 2007 - season 1 ####
#season1 <- power_seasons_sep %>% filter(Season == 1)
s1y2010 <- season1 %>% filter(Year == 2010)
s1y2010 <- mutate(s1y2010, daytag = hour(DateTime))

my_group <- group_by(s1y2010, daytag)
winter_day_2010 <- summarise_all(my_group, funs(mean))

#### 2010 - season 2 ####
#season2 <- power_seasons_sep %>% filter(Season == 2)
s2y2010 <- season2 %>% filter(Year == 2010)
s2y2010 <- mutate(s1y2010, daytag = hour(DateTime))

my_group <- group_by(s1y2010, daytag)
spring_day_2010 <- summarise_all(my_group, funs(mean))

#### 2010 - season 3 ####
#season3 <- power_seasons_sep %>% filter(Season == 3)
s3y2010 <- season3 %>% filter(Year == 2010)
s3y2010 <- mutate(s1y2010, daytag = hour(DateTime))

my_group <- group_by(s1y2010, daytag)
summer_day_2010 <- summarise_all(my_group, funs(mean))

#### 2010 - season 4 ####
#season4 <- power_seasons_sep %>% filter(Season == 4)
s4y2010 <- season4 %>% filter(Year == 2010)
s4y2010 <- mutate(s1y2010, daytag = hour(DateTime))

my_group <- group_by(s1y2010, daytag)
automn_day_2010 <- summarise_all(my_group, funs(mean))
