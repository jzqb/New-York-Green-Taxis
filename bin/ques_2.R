# question 2
## @knitr trip_distance_histogram
barfill <- "#25B293"
barlines <- "#267564"
axis_format_1000 <- function(x){ 
  x/1000
}
plot_hist <- ggplot(raw_data, aes(x = Trip_distance)) +
  geom_histogram(aes(y = ..count..), binwidth = 10,
                 colour = barlines, fill = barfill) +
  scale_x_continuous(name = "Trip_Distance") +
  scale_y_continuous(name = "Count [in thousands]", labels = axis_format_1000) +
  ggtitle("frequency distribution of \n trip distance")

## @knitr log_trip_distance_histogram
barfill <- "#2A6A93"
barlines <- "#22465D"
plot_hist_log <- ggplot(raw_data, aes(x = log(Trip_distance+1))) +
  geom_histogram(aes(y = ..count..), binwidth = 0.5,
                 colour = barlines, fill = barfill) +
  scale_x_continuous(name = "Log_Trip_Distance") +
  scale_y_continuous(name = "Count [in thousands]", labels = axis_format_1000) +
  ggtitle("frequency distribution of \n log trip distance")

## @knitr trip_distance_histogram_print
grid.arrange(plot_hist, plot_hist_log, ncol=2)

## @knitr summary_trip_distance
summ_trip_dist <- summary(raw_data$Trip_distance)
## @knitr summary_trip_distance_print
summ_trip_dist

## @knitr cap_trip_distance_histogram
barfill <- "#25B293"
barlines <- "#267564"
trip_dist <- subset(raw_data, raw_data$Trip_distance <= quantile(raw_data$Trip_distance, probs = 0.9999))
plot_hist_cap <- ggplot(trip_dist, aes(x = Trip_distance)) +
  geom_histogram(aes(y = ..count..), binwidth = 4,
                 colour = barlines, fill = barfill) +
  scale_x_continuous(name = "Trip_Distance") +
  scale_y_continuous(name = "Count [in thousands]", labels = axis_format_1000) +
  ggtitle("frequency distribution of \n trip distance [truncated 99.99%-tile]")

## @knitr cap_log_trip_distance_histogram
barfill <- "#2A6A93"
barlines <- "#22465D"
plot_hist_log_cap <- ggplot(trip_dist, aes(x = log(Trip_distance+1))) +
  geom_histogram(aes(y = ..count..), binwidth = 0.5,
                 colour = barlines, fill = barfill) +
  scale_x_continuous(name = "Log_Trip_Distance") +
  scale_y_continuous(name = "Count [in thousands]", labels = axis_format_1000) +
  ggtitle("frequency distribution of \n log trip distance [truncated 99.99%-tile]")

## @knitr cap_trip_distance_histogram_print
grid.arrange(plot_hist_cap, plot_hist_log_cap, ncol=2)

## @knitr cap_summary_trip_distance
summ_trip_dist_cap <- summary(trip_dist$Trip_distance)
## @knitr cap_summary_trip_distance_print
summ_trip_dist_cap

## @knitr fitting_gamma
set.seed(42)
fit_gamma <- fitdist(sample(trip_dist$Trip_distance, 10000, replace=FALSE), distr = "gamma", method = "mle", lower = c(0, 0), start = list(scale = 1, shape = 1))
plot(fit_gamma)
## @knitr fitting_gamma_results
fit_gamma$estimate