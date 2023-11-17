library(readxl)
library(readr)
tripdata08 <- read_csv("2022-2023-cyclistic-data/202208-divvy-tripdata.csv")
summary(tripdata08)
library(skimr)
skim_without_charts(tripdata08)
tripdata07 <- read_csv("2022-2023-cyclistic-data/202307-divvy-tripdata.csv")
tripdata06 <- read_csv("2022-2023-cyclistic-data/202306-divvy-tripdata.csv")
tripdata05 <- read_csv("2022-2023-cyclistic-data/202305-divvy-tripdata.csv")
tripdata04 <- read_csv("2022-2023-cyclistic-data/202304-divvy-tripdata.csv")
tripdata03 <- read_csv("2022-2023-cyclistic-data/202303-divvy-tripdata.csv")
tripdata02 <- read_csv("2022-2023-cyclistic-data/202302-divvy-tripdata.csv")
tripdata01 <- read_csv("2022-2023-cyclistic-data/202301-divvy-tripdata.csv")
tripdata12 <- read_csv("2022-2023-cyclistic-data/202212-divvy-tripdata.csv")
tripdata11 <- read_csv("2022-2023-cyclistic-data/202211-divvy-tripdata.csv")
tripdata10 <- read_csv("2022-2023-cyclistic-data/202210-divvy-tripdata.csv")
tripdata09 <- read_csv("2022-2023-cyclistic-data/202209-divvy-publictripdata.csv")
library(dplyr)
tripall <- bind_rows(tripdata08,tripdata09,tripdata10,tripdata11,tripdata12,tripdata01,tripdata02,tripdata03,tripdata04,tripdata05,tripdata06,tripdata07)
View(tripall)
summary(tripall)
skim_without_charts(tripall)
library(tidyverse)
tripall$ride_length = as.numeric(tripall$ride_length)
tripall1 <- tripall %>% mutate(year = format(as.Date(started_at,'%m/%d/%Y %H:%M'),'%Y')) %>% mutate(month = format(as.Date(started_at,'%m/%d/%Y %H:%M'),'%m')) %>% mutate(day = format(as.Date(started_at,'%m/%d/%Y %H:%M'),'%d')) %>% mutate(hours = hour(strptime(started_at,'%m/%d/%Y %H:%M'))) %>% mutate(minutes = minute(strptime(started_at,'%m/%d/%Y %H:%M')))
tripall2 <- na.omit(tripall1)
View(tripall2)
options(scipen = 999)


tripall2 %>% group_by(member_casual,rideable_type) %>% summarise(ride_count = n())
ggplot(data = tripall2)+geom_bar(mapping = aes(x=member_casual,fill=rideable_type)) + labs(title   = "Ride type for members and casuals",x = "Type of riders",y = "Ride count")


print(tripall2 %>% group_by(member_casual,rideable_type,day_of_week) %>% summarise(ride_count = n()),n=40)
ggplot(data = tripall2)+geom_bar(mapping = aes(x = member_casual,fill = rideable_type))+facet_wrap(~day_of_week) + labs(title   = "Ride type for members and casuals across days of week",x = "Type of riders",y = "Ride count")

tripall2 %>% group_by(member_casual,day_of_week) %>% summarise(ride_count = n())
ggplot(data = tripall2)+geom_bar(mapping = aes(x = member_casual))+facet_wrap(~day_of_week) + labs(title   = "Rides for members and casuals across days of week",x = "Type of riders",y = "Ride count")

tripall2 %>% group_by(member_casual) %>% summarise(ride_count = n())
ggplot(data = tripall2)+geom_bar(mapping = aes(x = member_casual)) + labs(title = "Total rides for members and casuals",x = "Type of riders",y = "Ride count")


print(tripall2 %>% group_by(member_casual,hours) %>% summarise(ride_count = n()),n = 50)
ggplot(data = tripall2)+geom_bar(mapping = aes(x = hours,fill = member_casual))+facet_wrap(~day_of_week) + labs(title = "Rides for members and casuals across 24 hours",subtitle = "across days of week",x = "Type of riders",y = "Ride count")

print(tripall2 %>% group_by(member_casual,month) %>% summarise(ride_count = n()),n = 30)
ggplot(data = tripall2)+geom_bar(mapping = aes(x = month,fill = member_casual)) + labs(title = "Rides for members and casuals across 12 months",x = "Months",y = "Ride count")



print(tripall2 %>% group_by(member_casual,rideable_type,month) %>% summarise(ride_count = n()),n = 80)
ggplot(data = tripall2)+geom_bar(mapping = aes(x = rideable_type,fill = member_casual))+facet_wrap(~month) + labs(title = "Rides for members and casuals for different bike types",subtitle = "Across 12 months",x = "Type of bikes",y = "Ride count")


print(tripall2 %>% group_by(member_casual,month) %>% summarise(ride_time = sum(ride_length)),n = 30)
ggplot(data = tripall2)+geom_col(mapping = aes(x = member_casual,y = ride_length))+facet_wrap(~month) + labs(title = "Ride time for members and casuals",subtitle = "Across 12 months",x = "Type of riders",y = "Ride time")

tripall2 %>% group_by(member_casual,day_of_week) %>% summarise(ride_time = sum(ride_length))
ggplot(data = tripall2)+geom_col(mapping = aes(x = member_casual,y = ride_length))+facet_wrap(~day_of_week) + labs(title = "Ride time for members and casuals",subtitle = "Across days of week",x = "Type of riders",y = "Ride time")


rle(sort(tripall2$start_station_name))
as.data.frame(table(tripall2$start_station_name))
trip_start_stn <- head(sort(table(tripall2$start_station_name),decreasing = TRUE),10) 
View(trip_start_stn)
trip_end_stn <-head(sort(table(tripall2$end_station_name),decreasing = TRUE),10)
View(trip_end_stn)
skim_without_charts(tripall2)
