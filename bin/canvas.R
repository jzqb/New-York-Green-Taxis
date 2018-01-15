#plots_to_save <- c("plot_hist", "plot_hist_log", "plot_hist_cap", "plot_hist_log_cap")
objects_to_save <- c("dimensions", "summ_trip_dist", "summ_trip_dist_cap", "fit_gamma", "hod_mean_median_trip_distance", "conversion_flag", "buffer_dist", "total_airport_rides", "raw_data_summ")
save(list = objects_to_save, file = "worker.RData", envir = .GlobalEnv)
#save(list = plots_to_save, file = "plots.RData", envir = .GlobalEnv)


set.seed(42)
train_test_split <- function(ratings, train_proportion = 0.8){
  sample_size <- floor(train_proportion*nrow(ratings))
  train_ind <- sample(seq_len(nrow(ratings)), size = sample_size)
  train_data <- ratings[train_ind,]
  test_data <- ratings[-train_ind,]
  split_data <- list('train_data' = train_data, 'test_data' = test_data)
  return(split_data)
}
split_data <- train_test_split(model_data, train_proportion = 0.1)
train_data <- split_data$train_data
test_data <- split_data$test_data
lm_fit <- lm(data = train_data, formula = tip_perc ~. )
predictedvals <- predict(lm_fit, )
sqrt(sum((predictedvals - test_data$tip_perc)^2)/nrow(test_data))
library(parallelSVM)
svm_fit <- parallelSVM(train_data[,-which(names(train_data) == "tip_perc")], train_data$tip_perc, numberCores = 8, samplingSize = 0.2, scale = FALSE, kernel = "polynomial", degree = 3, 
                       gamma = 0.1, 
                       coef0 = 0, cost = 1, cachesize = 40, tolerance = 0.001, epsilon = 0.1, 
                       shrinking = TRUE, cross = 0, probability = FALSE, 
                       fitted = TRUE, seed = 1, na.action = na.omit)

library(e1071)
library(rpart)
svm.model <- svm(tip_perc~., data=train_data, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[,-10])



library(caret)
library(randomForest)
library(doParallel)

cores <- 8
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

dataset <- read.csv("/home/anonimo/Modelli/total_merge.csv", header=TRUE)
dati <- data.frame(dataset)
attach(dati)


trainSet <- dati[2:107570,]
testSet <- dati[107570:480343,]

# 3 times cross validation.
control <- trainControl(method="cv", number=3, repeats=2)
metric <- "RMSE"
my_forest <- train(tip_perc~. , data=train_data,
                   method = "parRF",
                   ntree = 25,
                   trControl = control)


library(randomForest)
library(mlbench)
library(caret)

control <- trainControl(method="repeatedcv", number=3, repeats=2)
metric <- "RMSE"
mtry <- sqrt(ncol(train_data))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(tip_perc~., data=train_data, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

grid.arrange(pick_up_plot, drop_off_map, ncol = 2, padding = 1)




p <- ggplot(daily_hour, aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
  geom_point() +
  gganimate(p)

# libraries
library(ggplot2)
library(gganimate)
library(ggforce)
library(tweenr)

d <- data.frame(x = rnorm(20), y = rnorm(20), time = sample(100, 20), alpha = 0, 
                size = 1, ease = 'elastic-out', id = 1:20, 
                stringsAsFactors = FALSE)
d2 <- d
d2$time <- d$time + 10
d2$alpha <- 1
d2$size <- 3
d2$ease <- 'linear'
d3 <- d2
d3$time <- d2$time + sample(50:100, 20)
d3$size = 10
d3$ease <- 'bounce-out'
d4 <- d3
d4$y <- min(d$y) - 0.5
d4$size <- 2
d4$time <- d3$time + 10
d5 <- d4
d5$time <- max(d5$time)
df <- rbind(d, d2, d3, d4, d5)

# Using tweenr
dt <- tween_elements(df, 'time', 'id', 'ease', nframes = 500)

# Animate with gganimate
p <- ggplot(data = dt) + 
  geom_point(aes(x=x, y=y, size=size, alpha=alpha, frame = .frame)) + 
  scale_size(range = c(0.1, 20), guide = 'none') + 
  scale_alpha(range = c(0, 1), guide = 'none') + 
  ggforce::theme_no_axes()
animation::ani.options(interval = 1/24)
gganimate(p, 'dropping balls.gif', title_frame = F)



library(tweenr)

gapminder_edit <- gapminder %>%
  arrange(country, year) %>%
  select(gdpPercap,lifeExp,year,country, continent, pop) %>%
  rename(x=gdpPercap,y=lifeExp,time=year,id=country) %>%
  mutate(ease="linear")

gapminder_tween <- tween_elements(gapminder_edit,
                                  "time", "id", "ease", nframes = 300) %>%
  mutate(year = round(time), country = .group) %>%
  left_join(gapminder, by=c("country","year","continent")) %>%
  rename(population = pop.x)

p2 <- ggplot(gapminder_tween,
             aes(x=x, y=y, frame = .frame)) +
  geom_point(aes(size=population, color=continent),alpha=0.8) +
  xlab("GDP per capita") +
  ylab("Life expectancy at birth") +
  scale_x_log10(labels=comma)

gganimate(p2, filename="gapminder-tween.gif", title_frame = FALSE, interval = 0.05)






#ggmap(map)
ggmap(map, extent = "device", ylab = "lat", xlab = "lon") + 
  geom_segment(data = daily_hour, aes(y = Pickup_latitude, x = Pickup_longitude, yend = Dropoff_latitude, xend = Dropoff_longitude),
               arrow = arrow(length = unit(0.15, "cm"))) +
  annotation_custom(rasterGrob(rmat, width=unit(1,"npc"), height=unit(1, "npc")),
                    x = 6, xmax=6.2, y=2.5, ymax=4)

library(grid)

png("mask.png")
grid.polygon(c(-0.06, 0.06, 0.06, 0.15, 0, -0.15, -0.06),
             c(-5, -5, 2.5, 2, 5, 2, 2.5), gp=gpar(fill="black"),
             def="native",
             vp=viewport(xs=c(-0.15, 0.15), ys=c(-5, 5)))
dev.off()

library(png)
m <- readPNG("mask.png", native=FALSE)
mask <- matrix(rgb(m[,,1],m[,,2],m[,,3]),
               nrow=nrow(m))

rmat <- matrix(rgb(colorRamp(c("blue","white","red"))(seq(0,1,length=nrow(m))), maxColorValue=255),
               nrow=nrow(m), ncol=ncol(m))
rmat[mask == "#FFFFFF"] <- NA
grid.newpage()
grid.raster(rmat)


library(ggplot2)
ggplot(iris) + geom_path(aes(Sepal.Length, Petal.Length, colour = Petal.Width)) +
  guides(colour = guide_colourbar()) +
  annotation_custom(rasterGrob(rmat, width=unit(1,"npc"), height=unit(1, "npc")),
                    x = 6, xmax=6.2, y=2.5, ymax=4)


library(htmlwidgets)
library(leaflet)
library(ggmap)


#=============================== Data ===============================#  


#Retrieving coordinates for two airports
Adress = c("Lufthavnsboulevarden 6, 2770 Kastrup, Denmark", "Edvard Munchs veg, 2061 Gardermoen, Norway")

# This function geocodes a location (find latitude and longitude) using the Google Maps API
geo <- geocode(location = Adress, output="latlon", source="google")

#moving the data around, so that second observation will be end-destination
geo$lonend[1] <- geo$lon[2]
geo$latend[1] <- geo$lat[2]

#removing second row
row_to_keep = c(TRUE, FALSE)
geo = geo[row_to_keep,]

#putting a number in there to make some dots for the airports
geo$Number = 999

#=============================== Ploting =============================# 

# get a Google map
require(ggmap)
map<-get_map(location='Europe', zoom=5, maptype = "terrain-background",
             source='google',color='color')

get_map(location = c(lon = -95.3632715, lat = 29.7632836), zoom = "auto",
        scale = "auto", maptype = c("terrain", "terrain-background", "satellite",
                                    "roadmap", "hybrid", "toner", "watercolor", "terrain-labels", "terrain-lines",
                                    "toner-2010", "toner-2011", "toner-background", "toner-hybrid",
                                    "toner-labels", "toner-lines", "toner-lite"), source = c("google", "osm",
                                                                                             "stamen", "cloudmade"), force = ifelse(source == "google", TRUE, TRUE),
        messaging = FALSE, urlonly = FALSE, filename = "ggmapTemp",
        crop = TRUE, color = c("color", "bw"), language = "en-EN", api_key)


# plot it with ggplot2
require("ggplot2")
require("RColorBrewer")
ggmap(map, extent = "device", ylab = "lat", xlab = "lon") + 
  geom_segment(data = geo, aes(y = lat, x = lon, yend = latend, xend = lonend),
               arrow = arrow())

ggmap(map) +
  geom_point(
    aes(x=lon, 
        y=lat, 
        show_guide = TRUE, 
        colour=Number), 
    data=geo, 
    alpha=.8, 
    na.rm = T)  + 
  scale_color_gradient(low="beige", high="blue")

#Set the pointer from (y,x) => (yend,xend) using geom-segment
ggmap(map, extent = "device", ylab = "lat", xlab = "lon") + 
  geom_segment(aes(y = geo$lon, x = geo$lat, yend = geo$lonend, xend = geo$latend))