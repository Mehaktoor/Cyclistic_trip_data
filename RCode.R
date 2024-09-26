install.packages("tidyverse") #core package for data manipulation and visualization
install.packages("skimr") #package for data summary
install.packages("janitor") #package for cleaning data

#loading the libraries of the installed packages
library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)#for working with date-time

setwd("/cloud/project/GDC_Capstone_dataset")

#loading the ride data of the months Jan, Feb and March
trip_01 <- read_csv("202301-divvy-tripdata.csv")
trip_02 <- read_csv("202302-divvy-tripdata.csv")
trip_03 <- read_csv("202303-divvy-tripdata.csv")
#trip_04 <- read_csv("202304-divvy-tripdata.csv")
#trip_05 <- read_csv("202305-divvy-tripdata.csv")
#trip_06 <- read_csv("202306-divvy-tripdata.csv")
#trip_07 <- read_csv("202307-divvy-tripdata.csv")
#trip_08 <- read_csv("202308-divvy-tripdata.csv")
#trip_09 <- read_csv("202309-divvy-tripdata.csv")
#trip_10 <- read_csv("202310-divvy-tripdata.csv")
#trip_11 <- read_csv("202311-divvy-tripdata.csv")
#trip_12 <- read_csv("202312-divvy-tripdata.csv")

#get the colnames of each table to check whether the column names are same in all tables
colnames(trip_01)
colnames(trip_02)
colnames(trip_03)
#colnames(trip_04)
#colnames(trip_05)
#colnames(trip_06)
#colnames(trip_07)
#colnames(trip_08)
#colnames(trip_09)
#colnames(trip_10)
#colnames(trip_11)
#colnames(trip_12)
##Since the Column names are similar in all tables
##combines the rows in tables 1-12 to form a single table.

all_trips <- bind_rows(trip_01,trip_02,trip_03)#,trip_04,trip_05,trip_06)
#,trip_07,trip_08,trip_09,trip_10,trip_11,trip_12)

skim_without_charts(all_trips)# provides the summary of the data in the table

#remove the unnecessary columns that are not required for the analysis.
all_trips <- all_trips %>% 
              select(-c(start_lat, start_lng, end_lat, end_lng,start_station_name,
                         start_station_id,end_station_id,end_station_name))

#check the type of customers
table(all_trips$member_casual)

#calculate the ride duration of each ride
all_trips <- all_trips %>%
    mutate(ride_length=difftime(ended_at,started_at))
head(all_trips)

#calculate the WEEKDAY of the ride started
all_trips <- all_trips %>% 
                mutate(day_of_week=format(as.Date(started_at), "%A"))
skim_without_charts(all_trips)

#convert char type ride_length to numeric
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

skim_without_charts(all_trips)

#remove the negative values of the ridelength
all_trips <- all_trips[!(all_trips$ride_length<0)]

skim_without_charts(all_trips)

#eliminate the columns of date-time and ride_id to proceed for further analysis
all_trips <- all_trips %>% 
  select(-c(started_at,ended_at,ride_id))

#further proceed for the descriptive analysis
mean(all_trips$ride_length)
min(all_trips$ride_length)
max(all_trips$ride_length)
median(all_trips$ride_length)
summary(all_trips$ride_length)
table(all_trips$member_casual)

#create an csv file for the clean data 
write_csv(all_trips,file = 'cleaned_data.csv')
summary(all_trips$ride_length)
 Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 
  0.0     263.0     441.0     791.5     756.0 2016224.0
