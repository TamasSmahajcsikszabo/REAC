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
vector <- as.numeric(RT = vector$value)

distribution <- function(data, ...) {
  # calculating empirical parameters
  mu <- retimes::mexgauss(vector)[1]
  tau <- retimes::mexgauss(vector)[3]
  sigma <- retimes::mexgauss(vector)[2]
  n <- length(vector)
 hipo_exgauss<- retimes::rexgauss(n/10000, mu = mu, sigma = sigma, tau = tau, positive = TRUE)
 hist(hipo_exgauss)
 cor(vector, hipo_exgauss)
}

m <- mean(vector)
var <- var(vector)
skewness <- e1071::skewness(vector)

mu <- m - var^0.5 * (skewness/2)^(1/3)
var_exgauss <- var * (1 - (skewness/2)^(2/3))
tau <- var^0.5 * (skewness/2)^(1/3)
retimes::
