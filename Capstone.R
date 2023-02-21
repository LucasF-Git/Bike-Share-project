#setting up environment
install.packages('tidyverse')
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)

#import data files
Data <- "C:\\Users\\lucas\\Desktop\\Data Analysis\\BikeData\\CSV files"

bike_data_merge <- list.files(path = Data) %>% 
  lapply(read_csv) %>% 
  bind_rows

#Gotta take a look at that sweet sweet data
head(bike_data_merge)
tail(bike_data_merge)
colnames(bike_data_merge)
str(bike_data_merge)

#Creating a column for ride length, and month/day/year/day_of_week for future use
bike_data_add <- mutate(bike_data_merge, 
                        Ride_Time = difftime(ended_at, 
                                             started_at, 
                                             units = "secs"))

#Removing columns I don't need
data_trimmed <- subset(bike_data_add, select = -c(start_station_id, 
                                                    end_station_name, 
                                                    end_station_id, 
                                                    start_lat, 
                                                    start_lng, 
                                                    end_lat, end_lng))

#Renaming columns to be a bit easier to work with.
data_renamed <- rename(data_trimmed, 
                       ID = ride_id, 
                       Type = rideable_type, 
                       Start_Date = started_at, 
                       End_Date = ended_at, 
                       Member_Type = member_casual)

#getting rid of rows with NA, rides with 0 or negative durations and any 
#stations that are actually the QA recall designation.
data_cleaned <- data_renamed[!(data_renamed$start_station_name == "HQ QR" |
                                 data_renamed$Ride_Time <= 0 | 
                                 data_renamed$Ride_Time == "NA"),] %>% 
                                 arrange(Member_Type)


data_cleaned$date <- as.Date(data_cleaned$Start_Date)
data_cleaned$month <- format(as.Date(data_cleaned$date), "%m")
data_cleaned$day <- format(as.Date(data_cleaned$date), "%d")
data_cleaned$year <- format(as.Date(data_cleaned$date), "%Y")
data_cleaned$day_of_week <- format(as.Date(data_cleaned$date), "%A")

#Just looking at the sweet sweet data again, I can do what I want
str(data_cleaned)
head(data_cleaned)

#clarifying a data type to make sure it can be used for calculations
data_cleaned$Ride_Time <- as.numeric(as.character(data_cleaned$Ride_Time))

#clearing up rows with NA values and confirming they are gone
data_final <- na.omit(data_cleaned)

sum(is.na(data_final$Ride_Time))


aggregate(data_final$Ride_Time ~ data_final$Member_Type, FUN = mean)
aggregate(data_final$Ride_Time ~ data_final$Member_Type, FUN = median)
aggregate(data_final$Ride_Time ~ data_final$Member_Type, FUN = max)
aggregate(data_final$Ride_Time ~ data_final$Member_Type, FUN = min)

data_final %>% 
  mutate(weekday = wday(Start_Date, label = TRUE)) %>% 
  group_by(Member_Type, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(Member_Type, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = Member_Type)) +
  geom_col(position = "dodge")+
  labs(title = 'Total rides by day of week')

data_final %>% 
  mutate(weekday = wday(Start_Date, label = TRUE)) %>% 
  group_by(Member_Type, weekday) %>% 
  summarise(average_duration = mean(Ride_Time)) %>% 
  arrange(Member_Type, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = Member_Type)) +
  geom_col(position = "dodge")+
  labs(title = 'Average ride time by day of week', subtitle = 'Graph #2 Electric Boogaloo')

