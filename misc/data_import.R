library(tidyverse)

setwd("C:\\OneDrive\\R Projects\\REAC\\Data")
example <- read.csv("example.csv")
usethis::use_data(example, overwrite = TRUE)

setwd("C:\\OneDrive\\R Projects\\REAC\\Data")
example2 <- read.csv("example2.csv")
usethis::use_data(example2, overwrite = TRUE)
