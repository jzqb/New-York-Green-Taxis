#question5
## @knitr viz_q5
anim_data <- subset(model_data, model_data$pick_up_day < 8)
anim_data$pick_up_hour <- str_pad(string = anim_data$pick_up_hour, width = 2, pad = "0")
anim_data$pick_up_day <- str_pad(string = anim_data$pick_up_day, width = 2, pad = "0")
anim_data$frame <- paste(anim_data$pick_up_day, anim_data$pick_up_hour, sep = "_")

map <- get_map(location = c(lon = -73.93891, lat = 40.74), zoom = 11, maptype = "roadmap", source='google',color='color')

pick_up_plot <- ggmap(map, extent = "device", ylab = "lat", xlab = "lon") + 
  geom_point(aes(x = Pickup_longitude, y = Pickup_latitude, size = Trip_distance, frame = frame), data = anim_data, alpha = .2, color = "#306738") +
  ggtitle("pick up locations at day_hour:")
gganimate(pick_up_plot, interval = 0.2, filename = "pick_ups.gif", saver = "gif")

drop_off_plot <- ggmap(map, extent = "device", ylab = "lat", xlab = "lon") + 
  geom_point(aes(x = Dropoff_longitude, y = Dropoff_latitude, size = Trip_distance, frame = frame), data = anim_data, alpha = .2, color = "#E1552C") +
  ggtitle("drop off locations at hour_day:")
gganimate(drop_off_plot, interval = 0.2, filename = "drop_offs.gif", saver = "gif")