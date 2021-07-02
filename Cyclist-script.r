librarian::shelf(dplyr, purrr, data.table, lubridate, ggplot2, geosphere, rlist)

mydata <- do.call(rbind, lapply(list.files(pattern="*.csv"), fread))
summary(mydata)


mydata <- mydata %>% 
      select(-c(ride_id, start_station_name, end_station_name)) %>%
      mutate(distance = distHaversine(as.matrix(cbind(mydata$start_lng, mydata$start_lat)), as.matrix(cbind(mydata$end_lng, mydata$end_lat))),
             ride_length = as.numeric(ended_at-started_at),
             ended_at = ymd_hms(ended_at),
             started_at = ymd_hms(started_at),
             day = factor(weekdays(started_at), c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday")),
             member_casual = as.factor(member_casual))

mydata$ride_length <- replace(mydata$ride_length, which(mydata$ride_length < 0), NA)

ggplot(data = mydata) +
      geom_histogram(aes(x=ride_length, fill = member_casual))+
      xlim(0,5000) +
      labs(fill = "Casual User or Member?") +
      xlab("Ride Length (s)") +
      ggtitle(("Histogram of Ride Length by Member Status")) +
      theme_bw()

ggplot(data = mydata) +
      geom_histogram(aes(x=distance, fill = member_casual)) + 
      xlim(0,20000) +
      labs(fill = "Casual User or Member?") +
      xlab("Ride Distane (m)") +
      ggtitle("Histogram of Ride Distance by Member Status") +
      theme_bw()

ggplot(mydata) +
      geom_bar(aes(x=day, fill = member_casual), position = 'dodge')+
      labs(fill = "Casual User or Member?") +            
      xlab("Day of the Week") 
      ggtitle(("Frequencies of Weekday Usage by Member Status")) +
      theme_bw()
      
ggplot(mydata) +
      geom_bar(aes(x=rideable_type, fill = member_casual), position = 'dodge')+
      labs(fill = "Casual User or Member?") +            
      xlab("Type of Bike") +
      ggtitle(("Usage of Bike Type by Member Status")) +
      theme_bw()      

mydata %>% group_by(member_casual) %>%
      summarise(Mean_length = mean(ride_length, na.rm = T),
                Mode_weekday = DescTools::Mode(day),
                Mode_month = DescTools::Mode(month(started_at, label = T)),
                Mean_distance = mean(distance, na.rm = T)) %>%
      write.csv(file = "mydata_summary.csv")
