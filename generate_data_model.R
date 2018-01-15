# generate required data for predictions
link_to_data <- "https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv"
raw_data <- read.csv(url(link_to_data))
raw_data$tip_perc <- raw_data$Tip_amount/raw_data$Total_amount*100
clean_data <- raw_data[, -which(names(raw_data) == "Ehail_fee")]
clean_data$tip_perc[is.na(clean_data$tip_perc)] <- 0
clean_data <- na.omit(clean_data)
clean_data <- subset(clean_data, clean_data$RateCodeID != 99)
incorrect_coord <- sum(clean_data$Pickup_longitude == 0 | clean_data$Pickup_latitude == 0 | clean_data$Dropoff_latitude == 0 | clean_data$Dropoff_longitude == 0)
clean_data <- subset(clean_data, clean_data$Pickup_longitude != 0)
clean_data <- subset(clean_data, clean_data$Pickup_latitude != 0)
clean_data <- subset(clean_data, clean_data$Dropoff_latitude != 0)
clean_data <- subset(clean_data, clean_data$Dropoff_longitude != 0)
clean_data <- subset(clean_data, clean_data$Passenger_count != 0)
clean_data <- subset(clean_data, clean_data$Fare_amount >= 0)
clean_data <- subset(clean_data, clean_data$Extra >= 0)
clean_data <- subset(clean_data, clean_data$MTA_tax >= 0)
clean_data <- subset(clean_data, clean_data$Tip_amount >= 0)
clean_data <- subset(clean_data, clean_data$Tolls_amount >= 0)
clean_data <- subset(clean_data, clean_data$improvement_surcharge >= 0)
clean_data <- subset(clean_data, clean_data$Total_amount > 0)
clean_data <- subset(clean_data, clean_data$Trip_distance <= quantile(clean_data$Trip_distance, probs = 0.9999))
clean_data <- subset(clean_data, clean_data$Total_amount <= quantile(clean_data$Total_amount, probs = 0.9999))
clean_data$lpep_pickup_datetime_form <- strptime(clean_data$lpep_pickup_datetime, format = "%Y-%m-%d %H:%M:%S")
clean_data$Lpep_dropoff_datetime <- strptime(clean_data$Lpep_dropoff_datetime, format = "%Y-%m-%d %H:%M:%S")
clean_data$Trip_time <- as.numeric(clean_data$Lpep_dropoff_datetime - clean_data$lpep_pickup_datetime_form)
clean_data <- subset(clean_data, clean_data$Trip_time != 0)
clean_data$Trip_speed <- clean_data$Trip_distance/(clean_data$Trip_time/3600)
clean_data <- subset(clean_data, clean_data$Trip_speed < 200)
clean_data$pick_up_day <- day(clean_data$lpep_pickup_datetime_form)
clean_data$pick_up_weekday <- weekdays(clean_data$lpep_pickup_datetime_form)
clean_data$pick_up_hour <- as.numeric(hour(clean_data$lpep_pickup_datetime_form))

factor_variables <- c("VendorID", "Store_and_fwd_flag", "RateCodeID", "Payment_type", "Trip_type", "pick_up_weekday")
numeric_variables <- c("Pickup_longitude", "Pickup_latitude", "Dropoff_longitude", "Dropoff_latitude", "Passenger_count", "Trip_distance", "Fare_amount", "Extra", "MTA_tax", "Tolls_amount", "improvement_surcharge", "Trip_time", "Trip_speed", "pick_up_day", "pick_up_hour", "tip_perc")

model_data <- clean_data[, names(clean_data) %in% c(factor_variables, numeric_variables)]
model_data[, factor_variables] <- lapply(model_data[, factor_variables] , factor)
model_data[, numeric_variables] <- lapply(model_data[, numeric_variables] , as.numeric)
write.csv(model_data, "generated_data_model.csv", row.names = FALSE)