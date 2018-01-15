# question 3
## @knitr trip_distance_hourly_distribution
mean_trip_dist <- mean(raw_data$Trip_distance, na.rm = T)
med_trip_dist <- median(raw_data$Trip_distance, na.rm = T)
raw_data$lpep_pickup_datetime_form <- strptime(raw_data$lpep_pickup_datetime, format = "%Y-%m-%d %H:%M:%S")
conversion_flag <- sum(as.character(raw_data$lpep_pickup_datetime_form) == as.character(raw_data$lpep_pickup_datetime)) == nrow(raw_data)
conversion_flag
## @knitr trip_distance_hourly_data
raw_data$lpep_pickup_datetime_hour <- hour(raw_data$lpep_pickup_datetime_form)
hod_mean_median_trip_distance <- raw_data[, c("lpep_pickup_datetime_hour", "Trip_distance")] %>%
  group_by(lpep_pickup_datetime_hour) %>% 
  summarize(
    mean_trip_dist = mean(Trip_distance, na.rm = T), 
    med_trip_dist = median(Trip_distance, na.rm = T)
    )

## @knitr trip_distance_hourly_data_print
kable(hod_mean_median_trip_distance, "html", caption = "Hourly Distribution of Mean and Median Trip Distance") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center")

## @knitr trip_distance_hourly_data_viz
mean_med_viz <- ggplot(hod_mean_median_trip_distance, aes(lpep_pickup_datetime_hour)) + 
  geom_line(aes(y = mean_trip_dist, colour = "mean"), linetype="dashed", size=1.5) + 
  geom_line(aes(y = med_trip_dist, colour = "median"), linetype="dashed", size=1.5) +
  geom_point(aes(y = mean_trip_dist, colour = "mean"), colour="black", size=2, shape=21, fill="white") +
  geom_point(aes(y = med_trip_dist, colour = "mean"), colour="black", size=2, shape=21, fill="white") +
  scale_x_continuous(name = "hour of day") +
  scale_y_continuous(name = "trip_distance") +
  geom_hline(yintercept = mean_trip_dist) + 
  annotate("text", x = 12, y = mean_trip_dist, vjust = -1, label = "Baseline Mean") +
  geom_hline(yintercept = med_trip_dist) + 
  annotate("text", x = 12, y = med_trip_dist, vjust = -1, label = "Baseline Median") +
  ggtitle("mean and median trip distance by hour of day")
mean_med_viz

## @knitr trip_distance_hourly_data_boxplot
trip_dist$lpep_pickup_datetime_form <- strptime(trip_dist$lpep_pickup_datetime, format = "%Y-%m-%d %H:%M:%S")
conversion_flag <- sum(as.character(trip_dist$lpep_pickup_datetime_form) == as.character(trip_dist$lpep_pickup_datetime)) == nrow(trip_dist)
trip_dist$lpep_pickup_datetime_hour <- as.factor(hour(trip_dist$lpep_pickup_datetime_form))
box_hour <- ggplot(trip_dist, aes(lpep_pickup_datetime_hour, Trip_distance)) +
  geom_boxplot(fill='#8E13BD', color="#290637") +
  ggtitle("box-whisker plot for trip distance by hour")
box_hour

## @knitr airport_rides
jfk_coordinates <- c(-73.7781, 40.6413)
ewr_coordinates <- c(-74.1745, 40.6895)
lgr_coordinates <- c(-73.8740, 40.7769)
jfk_dist <- distm (as.matrix(cbind(raw_data$Pickup_longitude, raw_data$Pickup_latitude)), jfk_coordinates, fun = distHaversine)
ewr_dist <- distm (as.matrix(cbind(raw_data$Pickup_longitude, raw_data$Pickup_latitude)), ewr_coordinates, fun = distHaversine)
lgr_dist <- distm (as.matrix(cbind(raw_data$Pickup_longitude, raw_data$Pickup_latitude)), lgr_coordinates, fun = distHaversine)
jfk_dist_drop <- distm (as.matrix(cbind(raw_data$Dropoff_longitude, raw_data$Dropoff_latitude)), jfk_coordinates, fun = distHaversine)
ewr_dist_drop <- distm (as.matrix(cbind(raw_data$Dropoff_longitude, raw_data$Dropoff_latitude)), ewr_coordinates, fun = distHaversine)
lgr_dist_drop <- distm (as.matrix(cbind(raw_data$Dropoff_longitude, raw_data$Dropoff_latitude)), lgr_coordinates, fun = distHaversine)

jfk_cutoff <- NULL
ewr_cutoff <- NULL
lgr_cutoff <- NULL
jfk_drop_cutoff <- NULL
ewr_drop_cutoff <- NULL
lgr_drop_cutoff <- NULL
for(i in 1:20){
  jfk_cutoff <- append(jfk_cutoff, sum(jfk_dist < i*1000))
  ewr_cutoff <- append(ewr_cutoff, sum(ewr_dist < i*1000))
  lgr_cutoff <- append(lgr_cutoff, sum(lgr_dist < i*1000))
  jfk_drop_cutoff <- append(jfk_drop_cutoff, sum(jfk_dist_drop < i*1000))
  ewr_drop_cutoff <- append(ewr_drop_cutoff, sum(ewr_dist_drop < i*1000))
  lgr_drop_cutoff <- append(lgr_drop_cutoff, sum(lgr_dist_drop < i*1000))
}
buffer_dist <- as.data.frame(cbind(1:20, jfk_cutoff, ewr_cutoff, lgr_cutoff, jfk_drop_cutoff, ewr_drop_cutoff, lgr_drop_cutoff))
names(buffer_dist) <- c("buffer", "jfk", "ewr", "lgr", "jfk_drop", "ewr_drop", "lgr_drop")

jfk_buffer_viz <- ggplot(buffer_dist, aes(buffer)) + 
  geom_line(aes(y = jfk, colour = "jfk"), linetype="dashed", size=1.5) + 
  geom_line(aes(y = jfk_drop, colour = "jfk_drop"), linetype="dashed", size=1.5) +
  geom_point(aes(y = jfk, colour = "jfk"), colour="black", size=2, shape=21, fill="white") +
  geom_point(aes(y = jfk_drop, colour = "jfk_drop"), colour="black", size=2, shape=21, fill="white") +
  scale_x_continuous(name = "buffer [kms]") +
  scale_y_continuous(name = "count of trips [in thousands]", labels = axis_format_1000) +
  geom_vline(xintercept = 6) +
  annotate("text", x = 11, y = 700000, vjust = -1, label = "<- JFK Pick Up Buffer Cutoff") +
  geom_vline(xintercept = 1) +
  annotate("text", x = 6, y = 250000, vjust = -1, label = "<- JFK Drop Off Buffer Cutoff") +
  ggtitle("jfk airport buffer distance calculation")

ewr_buffer_viz <- ggplot(buffer_dist, aes(buffer)) + 
  geom_line(aes(y = ewr, colour = "ewr"), linetype="dashed", size=1.5) + 
  geom_line(aes(y = ewr_drop, colour = "ewr_drop"), linetype="dashed", size=1.5) +
  geom_point(aes(y = ewr, colour = "ewr"), colour="black", size=2, shape=21, fill="white") +
  geom_point(aes(y = ewr_drop, colour = "ewr_drop"), colour="black", size=2, shape=21, fill="white") +
  scale_x_continuous(name = "buffer [kms]") +
  scale_y_continuous(name = "count of trips [in thousands]", labels = axis_format_1000) +
  geom_vline(xintercept = 13) +
  annotate("text", x = 10, y = 400000, vjust = -1, label = "EWR Buffer Cutoff [Pick Up and Drop Off]") +
  ggtitle("ewr airport buffer distance calculation")

lgr_buffer_viz <- ggplot(buffer_dist, aes(buffer)) + 
  geom_line(aes(y = lgr, colour = "lgr"), linetype="dashed", size=1.5) + 
  geom_line(aes(y = lgr_drop, colour = "lgr_drop"), linetype="dashed", size=1.5) +
  geom_point(aes(y = lgr, colour = "lgr"), colour="black", size=2, shape=21, fill="white") +
  geom_point(aes(y = lgr_drop, colour = "lgr_drop"), colour="black", size=2, shape=21, fill="white") +
  scale_x_continuous(name = "buffer [kms]") +
  scale_y_continuous(name = "count of trips [in thousands]", labels = axis_format_1000) +
  geom_vline(xintercept = 1) +
  annotate("text", x = 6, y = 1000000, vjust = 1, label = "<- LGR Drop Off Buffer Cutoff") +
  geom_vline(xintercept = 2) +
  annotate("text", x = 7, y = 1500000, vjust = 1, label = "<- LGR Pick Up Buffer Cutoff") +
  ggtitle("lgr airport buffer distance calculation")

grid.arrange(jfk_buffer_viz, ewr_buffer_viz, lgr_buffer_viz, nrow = 3)

total_airport_rides <- sum(jfk_dist < 6*1000) + sum(jfk_dist_drop < 1*1000) + sum(ewr_dist < 13*1000) + sum(ewr_dist_drop < 13*1000) + sum(lgr_dist < 2*1000) + sum(lgr_dist_drop < 1*1000)