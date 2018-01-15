# question 1
## @knitr question1
link_to_data <- "https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv"
raw_data <- read.csv(url(link_to_data))
dimensions <- as.data.frame(dim(raw_data))
names(dimensions) <- "count"
dimensions$axis <- c("rows", "columns")
dimensions <- dimensions[c("axis", "count")]

## @knitr question1_print
kable(dimensions, "html", caption = "Raw Data Dimensions") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center")
