# checks if libraries are installed on the machine to be loaded, and loads the packages
## @knitr libraries_and_packages
list_of_packages <- c("ggplot2", "lubridate", "dplyr", "geosphere", "kableExtra", "gridExtra", "GGally", "stringr", "ggmap")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(ggplot2, quietly = TRUE, verbose = FALSE)
library(lubridate, quietly = TRUE, verbose = FALSE)
library(dplyr, quietly = TRUE, verbose = FALSE)
library(geosphere, quietly = TRUE, verbose = FALSE)
library(kableExtra, quietly = TRUE, verbose = FALSE)
library(knitr, quietly = TRUE, verbose = FALSE)
library(gridExtra, quietly = TRUE, verbose = FALSE)
library(GGally, quietly = TRUE, verbose = FALSE)
library(stringr, quietly = TRUE, verbose = FALSE)
library(ggmap, quietly = TRUE, verbose = FALSE)
library(gganimate, quietly = TRUE, verbose = FALSE)