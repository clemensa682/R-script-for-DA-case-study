install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(ggplot2)
bike_data1 <- read.csv("~/Downloads/bike_trip_data_20200901_20210831/202009-divvy-tripdata.csv")
bike_data2 <- read.csv("~/Downloads/bike_trip_data_20200901_20210831/202010-divvy-tripdata.csv")
bike_data3 <- read.csv("~/Downloads/bike_trip_data_20200901_20210831/202011-divvy-tripdata.csv")
bike_data4 <- read.csv("~/Downloads/bike_trip_data_20200901_20210831/202012-divvy-tripdata.csv")
bike_data5 <- read.csv("~/Downloads/bike_trip_data_20200901_20210831/202101-divvy-tripdata.csv")
bike_data6 <- read.csv("~/Downloads/bike_trip_data_20200901_20210831/202102-divvy-tripdata.csv")
bike_data7 <- read.csv("~/Downloads/bike_trip_data_20200901_20210831/202103-divvy-tripdata.csv")
bike_data8 <- read.csv("~/Downloads/bike_trip_data_20200901_20210831/202104-divvy-tripdata.csv")
bike_data9 <- read.csv("~/Downloads/bike_trip_data_20200901_20210831/202105-divvy-tripdata.csv")
bike_data10 <- read.csv("~/Downloads/bike_trip_data_20200901_20210831/202106-divvy-tripdata.csv")
bike_data11 <- read.csv("~/Downloads/bike_trip_data_20200901_20210831/202107-divvy-tripdata.csv")
bike_data12 <- read.csv("~/Downloads/bike_trip_data_20200901_20210831/202108-divvy-tripdata.csv")
# Importing the monthly data from the bike-share company
bike_data1 <- mutate(bike_data1, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
bike_data2 <- mutate(bike_data2, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
bike_data3 <- mutate(bike_data3, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
# Standardizing the data types across all monthly data sets
bike_data_all <- bind_rows(bike_data1, bike_data2, bike_data3, bike_data4, bike_data5, bike_data6, bike_data7, bike_data8, bike_data9, bike_data10, bike_data11, bike_data12)
# Combines the September 2020-August 2021 bike trip data into one data set
table(bike_data_all$member_casual) # creates a table showing the number of bike users who fall into the "member" and "casual" categories in the bike_data_all set
bike_data_all$date <- as.Date(bike_data_all$started_at) # lines 25-29 create columns that allow for aggregation of data for months, years, days, and days of the week as opposed to simply aggregating by started_at
bike_data_all$month <- format(as.Date(bike_data_all$date), "%m")
bike_data_all$day <- format(as.Date(bike_data_all$date), "%d")
bike_data_all$year <- format(as.Date(bike_data_all$date), "%Y")
bike_data_all$day_of_week <- format(as.Date(bike_data_all$date), "%A")
bike_data_all$ride_length <- difftime(bike_data_all$ended_at, bike_data_all$started_at) # creates calculation of ride length
bike_data_all$ride_length <- as.numeric(as.character(bike_data_all$ride_length)) # converts the difftime object created in line 30 to a numeric format
bike_data_v2 <- bike_data_all[!(bike_data_all$start_station_name=="HQ QR" | bike_data_all$ride_length<0),] # creates a version of the bike_data_all set without QA rides and rides of negative length
bike_data_v2$day_of_week <- ordered(bike_data_v2$day_of_week, levels = c("Sunday","Monday", "Tuesday", "Wednesday","Thursday", "Friday","Saturday")) # properly orders the days of the week in the bike_data_v2 set
summary(bike_data_v2$ride_length) # creates a statistical summary, including min, max, quartiles, and average, of the variable ride_length (measured in seconds) for bike_data_v2
aggregate(bike_data_v2$ride_length ~ bike_data_v2$member_casual, FUN = mean) # creates a table that splits out the mean ride lengths for members and "casual" riders
aggregate(bike_data_v2$ride_length ~ bike_data_v2$member_casual, FUN = min) # same table but with minimum values instead of means
aggregate(bike_data_v2$ride_length ~ bike_data_v2$member_casual, FUN = max) # same table but with maximum values
aggregate(bike_data_v2$ride_length ~ bike_data_v2$member_casual, FUN = median) # finds the median values for members and casuals
aggregate(bike_data_v2$ride_length ~ bike_data_v2$member_casual + bike_data_v2$day_of_week, FUN = mean) # finds averages for each day of the week by rider group
# lines 41 et seq. create a visualization of ridership data by rider type and day of week
bike_data_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge")
# lines 48 et seq. create a visualization of average ride duration for members and casuals on each day of the week
bike_data_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "Ride Duration by Membership Status", subtitle = "Separated by days of the week", x = "Day", y = "Average Duration (seconds)", fill = "Membership Status", caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")

bike_data_v3 <- bike_data_v2[!(bike_data_v2$ride_length>=86400),] # removes "rides" that lasted longer than a day
# lines 57-62 create the same visualization as 40-46, but without "rides" lasting longer than a day
bike_data_v3 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "Number of Rides by Membership Status", 
                                                                                                          subtitle = "Excluding rides of less than 60 seconds\nthat start and end at the same station", x = "Day", 
                                                                                                          y = "Number of Rides", fill = "Membership Status", 
                                                                                                          caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")
# lines 64 et seq. create the same visualization as 48-53, but without "rides" lasting longer than a day
bike_data_v3 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "Ride Duration by Membership Status", subtitle = "Separated by days of the week", x = "Day", y = "Average Duration (seconds)", fill = "Membership Status", caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")
# remove data for rides lasting less than a minute that start and end at the same station
bike_data_v4 <- bike_data_v3[!((bike_data_v3$start_station_name == bike_data_v3$end_station_name) & (bike_data_v3$ride_length <= 60)),]
# use this data set to create the same visualizations, first number of rides...
bike_data_v4 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "Number of Rides by Membership Status", 
    subtitle = "Excluding rides of less than 60 seconds\nthat start and end at the same station", x = "Day", 
    y = "Number of Rides", fill = "Membership Status", 
    caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")
# ...then average duration
bike_data_v4 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "Ride Duration for Members and Nonmembers", 
    subtitle = "Excluding rides of less than 60 seconds\nthat start and end at the same station", x = "Day", 
    y = "Average Duration (seconds)", fill = "Membership Status", 
    caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")
# to get a better idea of typical usage, this subset excludes rides longer than 12 hours
bike_data_v5 <- bike_data_v4[!(bike_data_v4$ride_length >= 43200),]
# building the same plots again for this data subset
bike_data_v5 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "Number of Rides by Membership Status", 
    subtitle = "Excluding rides of more than 12 hours", x = "Day", 
    y = "Number of Rides", fill = "Membership Status", 
    caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")

bike_data_v5 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "Ride Duration for Members and Nonmembers", 
    subtitle = "Excluding rides of more than 12 hours", x = "Day", 
    y = "Average Duration (seconds)", fill = "Membership Status", 
    caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")
# it may also be useful to isolate these atypically long rides (>12h) to discern patterns
bike_data_v6 <- bike_data_v2[(bike_data_v2$ride_length >= 43200),]

bike_data_v6 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "Number of Rides by Membership Status", 
    subtitle = "Only rides of more than 12 hours", x = "Day", 
    y = "Number of Rides", fill = "Membership Status", 
    caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")

bike_data_v6 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "Ride Duration for Members and Nonmembers", 
  subtitle = "Only rides of more than 12 hours", x = "Day", 
  y = "Average Duration (seconds)", fill = "Membership Status", 
  caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")

bike_data_v6 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), ride_length) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = ride_length, color = member_casual)) + geom_point() + labs(title = "Ride Duration with Fluid Timeline", 
                                                                                                             subtitle = "Only rides of more than 12 hours", x = "Day", 
                                                                                                             y = "Duration (seconds)", key = "Membership Status", 
                                                                                                             caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")

bike_data_v6 %>%
  group_by(member_casual, started_at) %>%
  summarize(number_of_rides = n(), ride_length) %>%
  arrange(member_casual, started_at) %>%
  ggplot(aes(x = started_at, y = ride_length, color = member_casual)) + geom_point() + labs(title = "Ride Duration for Members and Nonmembers", 
                                                                                         subtitle = "Only rides of more than 12 hours", x = "Time (read Sep '20 -> Aug '21)", 
                                                                                         y = "Duration (seconds)", legend = "Membership Status", 
                                                                                         caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")

# now, isolate the ride lengths between 1 and 2 days
bike_data_v7 <- bike_data_v6[!(bike_data_v6$ride_length > 169200),]
bike_data_v7 %>%
  group_by(member_casual, started_at) %>%
  summarize(number_of_rides = n(), ride_length) %>%
  arrange(member_casual, started_at) %>%
  ggplot(aes(x = started_at, y = ride_length, color = member_casual)) + geom_point() + labs(title = "Ride Duration for Members and Nonmembers", 
                                                                                            subtitle = "Only rides of more than 12 hours", x = "Time (read Sep '20 -> Aug '21)", 
                                                                                            y = "Duration (seconds)", legend = "Membership Status", 
                                                                                            caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")

# here's an analysis of rides under 2 hours in length
bike_data_v8 <- bike_data_v2[(bike_data_v2$ride_length < 7200) & !((bike_data_v2$ride_length <= 60) & (bike_data_v2$start_station_name == bike_data_v2$end_station_name)),]

bike_data_v8 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "Number of Rides by Membership Status", 
                                                                                                            subtitle = "Only rides of more than 12 hours", x = "Day", 
                                                                                                            y = "Number of Rides", fill = "Membership Status", 
                                                                                                            caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")

bike_data_v8 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "Ride Duration for Members and Nonmembers", 
                                                                                                             subtitle = "Only rides of more than 12 hours", x = "Day", 
                                                                                                             y = "Average Duration (seconds)", fill = "Membership Status", 
                                                                                                             caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")

# looking specifically at the middle 50% of our initial observations
bike_data_v9 <- bike_data_v2[((bike_data_v2$ride_length <= 1397) & (bike_data_v2$ride_length >= 432)),]

bike_data_v9 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "Number of Rides by Membership Status", 
                                                                                                            subtitle = "Only rides of more than 12 hours", x = "Day", 
                                                                                                            y = "Number of Rides", fill = "Membership Status", 
                                                                                                            caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")

bike_data_v9 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "Ride Duration for Members and Nonmembers", 
                                                                                                             subtitle = "Only rides of more than 12 hours", x = "Day", 
                                                                                                             y = "Average Duration (seconds)", fill = "Membership Status", 
                                                                                                             caption = "Based on rider data collected 1 Sep 2020 - 31 Aug 2021")

