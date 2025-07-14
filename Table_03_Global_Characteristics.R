library(ggplot2)
library(readr)
library(bsts)
library(Metrics)
library(dplyr)
library(forecast)
library(fpp3)
library(tsibble)
library(fable)
library(readxl)
library(stats)
library(patchwork)
library(rugarch)
library(pracma)
library(e1071)
library(tseries)
library(nonlinearTseries)
library(seastests)
library(car)

setwd('Forecasting_Climate_Policy_Uncertainty_Evidence_from_the_United_States/Dataset/Data')
cpu_data <- read_xlsx('CPU_Data.xlsx')
cpu_data$Date <- as.Date(cpu_data$Date)
cpu_data$Date <- ydm(cpu_data$Date)
covariates_reduced <- read_xlsx('CPU_Data_Reduced.xlsx') %>% select(-cpu_index)  

sum(is.na(cpu_data))
colSums(is.na(cpu_data))
cpu_data[is.na(cpu_data)] <- 0
sum(is.na(cpu_data))

cpu <- cpu_data[,139]
cpu <- unlist(cpu)

n = 24
set.seed(100)

train_24 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_24 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_24 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_24 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

##################################################### Functions #####################################################

SUMMARY <- function(data) {
  if (!is.numeric(data)){
    stop("Data must be numeric")
  } 
  
  CoV <- function(data) {
    return(sd(data, na.rm = TRUE) / mean(data, na.rm = TRUE) * 100)
  }
  
  Entropy <- function(data) {
    probs <- table(data) / length(data)
    return(-sum(probs * log(probs), na.rm = TRUE))
  }
  
  Min <- min(data, na.rm = TRUE)
  Q1 <- quantile(data, 0.25, na.rm = TRUE)
  Median <- median(data, na.rm = TRUE)
  Mean <- mean(data, na.rm = TRUE)
  Q3 <- quantile(data, 0.75, na.rm = TRUE)
  Max <- max(data, na.rm = TRUE)
  CoV_value <- CoV(data)
  Entropy_value <- Entropy(data)
  
  summ <- tibble(
    Min = Min,
    Q1 = Q1,
    Median = Median,
    Mean = Mean,
    Q3 = Q3,
    Max = Max,
    CoV = CoV_value,
    Entropy = Entropy_value
  )
  
  return(summ)
}


statistical_tests_summary <- function(data) {
  
  if (!is.numeric(data)){
    stop("Data must be numeric")
  } 
  
  if(any(is.na(data))){
    data <- na.approx(data)
  }
  
  skewness_value <- skewness(data)
  kurtosis_value <- kurtosis(data)
  
  if(is.nan(nonlinearityTest(data, verbose = TRUE)$Tsay$p.value)){
    if(nonlinearityTest(data, verbose = TRUE)$Keenan$p.value <= 0.05){
      nonlinearity_value <- 'Non-linear'
    }else{
      nonlinearity_value <- 'Linear'
    }
  }else{
    if(nonlinearityTest(data, verbose = TRUE)$Tsay$p.value <= 0.05){
      nonlinearity_value <- 'Non-linear'
    }else{
      nonlinearity_value <- 'Linear'
    }
  }
  
  check_seasonality <- function(data) {
    seasonality_value <- NULL
    
    tryCatch({
      if (isSeasonal(data, test = "seasdum", freq = 12)) {
        seasonality_value <- 'Seasonal'
      } else {
        seasonality_value <- 'Non-seasonal'
      }
    }, error = function(e) {
      if (isSeasonal(data, test = "combined", freq = 12)) {
        seasonality_value <<- 'Seasonal'
      } else {
        seasonality_value <<- 'Non-seasonal'
      }
    })
    
    return(seasonality_value)
  }
  seasonality_value <- check_seasonality(data)
  
  if(kpss.test(data)$p.value > 0.05){
    stationarity_value <- 'Startionary'
  } else{
    stationarity_value <- 'Non-stationary'
  } 
  
  hurst_Hs_value <- as.numeric(hurstexp(data)[1])
 
  result <- tibble(
    Skewness = skewness_value,
    Kurtosis = kurtosis_value,
    Linearity = nonlinearity_value,
    Seasonality = seasonality_value,
    Stationarity = stationarity_value,
    LongRangeDependence = hurst_Hs_value
  )
  return(result)
}

statistical_tests_summary(train_24$cpu_index)
