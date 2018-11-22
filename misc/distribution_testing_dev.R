library(REAC)
library(xlsx)
library(tidyverse)
setwd("C:\\OneDrive\\R Projects\\Kutatás")
data <- read.xlsx("FaceDot2015_16_Tamasnak.xlsx", sheetIndex = 1)
data <- data[,2:116]
data_ref <- data %>% gather(2:115, key = trial, value = RT)
hist(data_ref$RT)
vector <- as.numeric(data_ref$RT)
vector <- as_tibble(vector) %>% filter(!is.na(value))
vector <- as.numeric(vector$value)

distribution <- function(data, ...) {
  # calculating empirical parameters
  mu <- retimes::mexgauss(vector)[1]
  lambda <- retimes::mexgauss(vector)[3]
  sigma <- retimes::mexgauss(vector)[2]
  n <- length(vector)
 hipo_wald <- extraDistr::
 hist(hipo_wald)
 cor(vector, hipo_wald)
}

m <- mean(vector)
var <- var(vector)
skewness <- e1071::skewness(vector)

mu <- m - var^0.5 * (skewness/2)^(1/3)
var_exgauss <- var * (1 - (skewness/2)^(2/3))
tau <- var^0.5 * (skewness/2)^(1/3)
