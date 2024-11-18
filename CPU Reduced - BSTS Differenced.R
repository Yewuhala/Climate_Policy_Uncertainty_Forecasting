library(ggplot2)
library(readr)
library(bsts)
library(Metrics)
library(dplyr)
library(forecast)
library(fracdiff)
library(readxl)
library(fpp3)
library(stats)

setwd('C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network')
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


##################################################### Evaluation Function #####################################################


evaluate <- function(test,pred,model){
  MAPE <- mape(test,pred)*100
  SMAPE <- smape(test,pred)
  MAE <- mae(test,pred)
  MASE <- mase(test,pred)
  RMSE <- rmse(test,pred)
  
  return(tibble('MODEL' = model,
                'MAPE' = MAPE,
                'SMAPE' = SMAPE,
                'MAE' = MAE,
                'MASE' = MASE,
                'RMSE' = RMSE))
}


##################################################### Fractional Undifferencing #####################################################


# Remove missing values from end points
na.ends <- function(x) {
  tspx <- tsp(x)
  # Strip initial and final missing values
  nonmiss <- (1:length(x))[!is.na(x)]
  if (length(nonmiss) == 0) {
    stop("No non-missing data")
  }
  j <- nonmiss[1]
  k <- nonmiss[length(nonmiss)]
  x <- x[j:k]
  if (!is.null(tspx)) {
    x <- ts(x, start = tspx[1] + (j - 1) / tspx[3], frequency = tspx[3])
  }
  return(x)
}

unfracdiff <- function(x, y, n, h, d) {
  bin.c <- (-1) ^ (0:(n + h)) * choose(d, (0:(n + h)))
  b <- numeric(n)
  xnew <- LHS <- numeric(h)
  RHS <- cumsum(y)
  bs <- cumsum(bin.c[1:h])
  b <- bin.c[(1:n) + 1]
  xnew[1] <- RHS[1] <- y[1] - sum(b * rev(x))
  if (h > 1) {
    for (k in 2:h)
    {
      b <- b + bin.c[(1:n) + k]
      RHS[k] <- RHS[k] - sum(b * rev(x))
      LHS[k] <- sum(rev(xnew[1:(k - 1)]) * bs[2:k])
      xnew[k] <- RHS[k] - LHS[k]
    }
  }
  tspx <- tsp(x)
  if (is.null(tspx)) {
    tspx <- c(1, length(x), 1)
  }
  return(ts(xnew, frequency = tspx[3], start = tspx[2] + 1 / tspx[3]))
}

frac_bsts <- function(x, xreg = NULL, niter = 1000, seed = 100, ping = 100, expected_model_size = 10) {
  
  d <- arfima(x)$d
  differenced_series <- diffseries(x, d)
  
  ss <- AddLocalLevel(list(), differenced_series)
  ss <- AddSeasonal(ss, differenced_series, nseasons = 12)
  
  if(is.null(xreg)){
    bsts_model <- bsts(differenced_series,
                       state.specification = ss,
                       niter = niter,
                       ping = ping,
                       seed = seed,
                       expected.model.size = expected_model_size)
  }
  
  else{
    bsts_model <- bsts(differenced_series ~ .,
                       state.specification = ss,
                       niter = niter,
                       ping = ping,
                       seed = seed,
                       expected.model.size = expected_model_size,
                       data = as.data.frame(cbind(differenced_series, xreg)))
  }
  
  
  bsts_model_list <- list(
    coefficients = bsts_model$coefficients,
    state.contributions = bsts_model$state.contributions,
    final.state = bsts_model$final.state,
    one.step.prediction.errors = bsts_model$one.step.prediction.errors,
    log.likelihood = bsts_model$log.likelihood,
    has.regression = bsts_model$has.regression,
    state.specification = bsts_model$state.specification,
    timestamp.info = bsts_model$timestamp.info,
    model.options = bsts_model$model.options,
    niter = bsts_model$niter,
    original.series = x,
    xlevels = bsts_model$xlevels,
    terms = bsts_model$terms,
    predictors = bsts_model$predictors,
    d = d, 
    differenced.series = differenced_series,
    model = bsts_model)
  
  return(bsts_model_list)
}



frac_bsts_forecast <- function(object, newdata = NULL, h = 10, quantiles = c(0.1, 0.9)) {
  
  if (object$has.regression == TRUE && is.null(newdata)) {
    stop("Error: The model includes regressors, but no newdata has been provided. Forecasting cannot proceed without the regressors.")
  }
  
  if (object$has.regression == TRUE) {
    bsts_pred <- predict(object$model, h = h, burn = SuggestBurn(0.1, object$model), newdata = newdata, quantiles = quantiles)
  } 
  
  if (object$has.regression == FALSE){
    bsts_pred <- predict(object$model, h = h, burn = SuggestBurn(0.1, object$model), quantiles = quantiles)
  }
  
  undifferenced_forecast <- unfracdiff(na.ends(object$original.series) - mean(object$original.series), bsts_pred$mean, n = length(object$original.series), h = h, object$d) + mean(object$original.series)
  undifferenced_lower <- unfracdiff(na.ends(object$original.series) - mean(object$original.series), bsts_pred$interval[1,], n = length(object$original.series), h = h, object$d) + mean(object$original.series)
  undifferenced_upper <- unfracdiff(na.ends(object$original.series) - mean(object$original.series), bsts_pred$interval[2,], n = length(object$original.series), h = h, object$d) + mean(object$original.series)
  
  lo_colname <- paste("Lo", (1 - 2*quantiles[1])*100, sep = " ")  
  hi_colname <- paste("Hi", (1 - 2*quantiles[1])*100, sep = " ")  
  
  forecast_df <- data.frame(lo_colname = undifferenced_lower, mean = undifferenced_forecast, hi_colname = undifferenced_upper)
  
  colnames(forecast_df)[1] <- lo_colname
  colnames(forecast_df)[3] <- hi_colname
  
  return(forecast_df)
}


# Testing

arfima_fit <- arfima(cpu)
arfima_fc <- forecast(arfima_fit, h = 10)

differenced_series <- diffseries(cpu, arfima_fit$d)
ar_fit <- ar(differenced_series, order.max = 1)
ar_fc <- forecast(ar_fit, h = 10)
ar_fc_undifferenced <- unfracdiff(na.ends(cpu) - mean(cpu), ar_fc$mean, n = length(cpu), h = 10, arfima_fit$d) + mean(cpu)

evaluate(arfima_fc$mean, ar_fc_undifferenced, model = 'AR - Differenced')


########################################################### Integer Differencing ########################################################### 3

##################################################### Horizon 1 #####################################################

n = 1
set.seed(100)

train_1 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_1 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_1 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_1 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_1 <- tibble()  
predict_1 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_1) + 1:length(test_1)]))  



# BSTS Differenced

arima_1 <- auto.arima(train_1$cpu_index)
d <- ndiffs(train_1$cpu_index)

if(d == arima_1$arma[6]){
  print('YES')
}

differenced_series_1 <- c(0,diff(train_1$cpu_index, d))

ss <- AddLocalLevel(list(), differenced_series_1)
ss <- AddSeasonal(ss, differenced_series_1, nseasons = 12)
bsts_1 <- bsts(cpu_index_diff ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10,
                data = as.data.frame(cbind(differenced_series_1, train_reg_reduced_1)) %>% rename('cpu_index_diff' = differenced_series_1))
bsts_1_pred <- predict(bsts_1, horizon = n, burn = SuggestBurn(.1, bsts_1), newdata = test_reg_reduced_1, quantiles = c(0.1,0.9))

bsts_1_pred_undiff <- diffinv(bsts_1_pred$mean, differences = d)[-1] + tail(train_1$cpu_index, 1)
bsts_1_lower_undiff <- diffinv(bsts_1_pred$interval[1,], differences = d)[-1] + tail(train_1$cpu_index, 1)
bsts_1_upper_undiff <- diffinv(bsts_1_pred$interval[2,], differences = d)[-1] + tail(train_1$cpu_index, 1)
bsts_1_forecast <- data.frame(mean = bsts_1_pred_undiff, LL = bsts_1_lower_undiff, UL = bsts_1_upper_undiff)

model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, bsts_1_forecast$mean, model = 'BSTS - Differenced'))
predict_1 <- predict_1 %>% mutate('BSTS - Differenced' = bsts_1_forecast$mean, 'BSTS - Differenced - LL' = bsts_1_lower_undiff, 'BSTS - Differenced - UL' = bsts_1_upper_undiff)


write.csv(model_evaluate_1, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Integer - Horizon 1.csv', row.names = FALSE)
write.csv(predict_1, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Integer - Forecast 1.csv', row.names = FALSE)



##################################################### Horizon 3 #####################################################

n = 3
set.seed(100)

train_3 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_3 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_3 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_3 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_3 <- tibble()  
predict_3 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_3) + 1:length(test_3)]))  



# BSTS Differenced

arima_3 <- auto.arima(train_3$cpu_index)
d <- ndiffs(train_3$cpu_index)

if(d == arima_3$arma[6]){
  print('YES')
}

differenced_series_3 <- c(0,diff(train_3$cpu_index, d))

ss <- AddLocalLevel(list(), differenced_series_3)
ss <- AddSeasonal(ss, differenced_series_3, nseasons = 12)
bsts_3 <- bsts(cpu_index_diff ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10,
                data = as.data.frame(cbind(differenced_series_3, train_reg_reduced_3)) %>% rename('cpu_index_diff' = differenced_series_3))
bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), newdata = test_reg_reduced_3, quantiles = c(0.1,0.9))

bsts_3_pred_undiff <- diffinv(bsts_3_pred$mean, differences = d)[-1] + tail(train_3$cpu_index, 1)
bsts_3_lower_undiff <- diffinv(bsts_3_pred$interval[1,], differences = d)[-1] + tail(train_3$cpu_index, 1)
bsts_3_upper_undiff <- diffinv(bsts_3_pred$interval[2,], differences = d)[-1] + tail(train_3$cpu_index, 1)
bsts_3_forecast <- data.frame(mean = bsts_3_pred_undiff, LL = bsts_3_lower_undiff, UL = bsts_3_upper_undiff)

model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_forecast$mean, model = 'BSTS - Differenced'))
predict_3 <- predict_3 %>% mutate('BSTS - Differenced' = bsts_3_forecast$mean, 'BSTS - Differenced - LL' = bsts_3_lower_undiff, 'BSTS - Differenced - UL' = bsts_3_upper_undiff)


write.csv(model_evaluate_3, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Integer - Horizon 3.csv', row.names = FALSE)
write.csv(predict_3, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Integer - Forecast 3.csv', row.names = FALSE)



##################################################### Horizon 6 #####################################################

n = 6
set.seed(100)

train_6 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_6 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_6 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_6 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_6 <- tibble()  
predict_6 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_6) + 1:length(test_6)]))  



# BSTS Differenced

arima_6 <- auto.arima(train_6$cpu_index)
d <- ndiffs(train_6$cpu_index)

if(d == arima_6$arma[6]){
  print('YES')
}

differenced_series_6 <- c(0,diff(train_6$cpu_index, d))

ss <- AddLocalLevel(list(), differenced_series_6)
ss <- AddSeasonal(ss, differenced_series_6, nseasons = 12)
bsts_6 <- bsts(cpu_index_diff ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10,
                data = as.data.frame(cbind(differenced_series_6, train_reg_reduced_6)) %>% rename('cpu_index_diff' = differenced_series_6))
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6, quantiles = c(0.1,0.9))

bsts_6_pred_undiff <- diffinv(bsts_6_pred$mean, differences = d)[-1] + tail(train_6$cpu_index, 1)
bsts_6_lower_undiff <- diffinv(bsts_6_pred$interval[1,], differences = d)[-1] + tail(train_6$cpu_index, 1)
bsts_6_upper_undiff <- diffinv(bsts_6_pred$interval[2,], differences = d)[-1] + tail(train_6$cpu_index, 1)
bsts_6_forecast <- data.frame(mean = bsts_6_pred_undiff, LL = bsts_6_lower_undiff, UL = bsts_6_upper_undiff)

model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_forecast$mean, model = 'BSTS - Differenced'))
predict_6 <- predict_6 %>% mutate('BSTS - Differenced' = bsts_6_forecast$mean, 'BSTS - Differenced - LL' = bsts_6_lower_undiff, 'BSTS - Differenced - UL' = bsts_6_upper_undiff)


write.csv(model_evaluate_6, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Integer - Horizon 6.csv', row.names = FALSE)
write.csv(predict_6, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Integer - Forecast 6.csv', row.names = FALSE)



##################################################### Horizon 12 #####################################################

n = 12
set.seed(100)

train_12 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_12 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_12 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_12 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_12 <- tibble()  
predict_12 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_12) + 1:length(test_12)]))  



# BSTS Differenced

arima_12 <- auto.arima(train_12$cpu_index)
d <- ndiffs(train_12$cpu_index)

if(d == arima_12$arma[6]){
  print('YES')
}

differenced_series_12 <- c(0,diff(train_12$cpu_index, d))

ss <- AddLocalLevel(list(), differenced_series_12)
ss <- AddSeasonal(ss, differenced_series_12, nseasons = 12)
bsts_12 <- bsts(cpu_index_diff ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10,
                data = as.data.frame(cbind(differenced_series_12, train_reg_reduced_12)) %>% rename('cpu_index_diff' = differenced_series_12))
bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), newdata = test_reg_reduced_12, quantiles = c(0.1,0.9))

bsts_12_pred_undiff <- diffinv(bsts_12_pred$mean, differences = d)[-1] + tail(train_12$cpu_index, 1)
bsts_12_lower_undiff <- diffinv(bsts_12_pred$interval[1,], differences = d)[-1] + tail(train_12$cpu_index, 1)
bsts_12_upper_undiff <- diffinv(bsts_12_pred$interval[2,], differences = d)[-1] + tail(train_12$cpu_index, 1)
bsts_12_forecast <- data.frame(mean = bsts_12_pred_undiff, LL = bsts_12_lower_undiff, UL = bsts_12_upper_undiff)

model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_forecast$mean, model = 'BSTS - Differenced'))
predict_12 <- predict_12 %>% mutate('BSTS - Differenced' = bsts_12_forecast$mean, 'BSTS - Differenced - LL' = bsts_12_lower_undiff, 'BSTS - Differenced - UL' = bsts_12_upper_undiff)


write.csv(model_evaluate_12, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Integer - Horizon 12.csv', row.names = FALSE)
write.csv(predict_12, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Integer - Forecast 12.csv', row.names = FALSE)



##################################################### Horizon 24 #####################################################

n = 24
set.seed(100)

train_24 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_24 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_24 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_24 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_24 <- tibble()  
predict_24 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_24) + 1:length(test_24)]))  



# BSTS Differenced

arima_24 <- auto.arima(train_24$cpu_index)
d <- ndiffs(train_24$cpu_index)

if(d == arima_24$arma[6]){
  print('YES')
}

differenced_series_24 <- c(0,diff(train_24$cpu_index, d))

ss <- AddLocalLevel(list(), differenced_series_24)
ss <- AddSeasonal(ss, differenced_series_24, nseasons = 12)
bsts_24 <- bsts(cpu_index_diff ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10,
                data = as.data.frame(cbind(differenced_series_24, train_reg_reduced_24)) %>% rename('cpu_index_diff' = differenced_series_24))
bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), newdata = test_reg_reduced_24, quantiles = c(0.1,0.9))

bsts_24_pred_undiff <- diffinv(bsts_24_pred$mean, differences = d)[-1] + tail(train_24$cpu_index, 1)
bsts_24_lower_undiff <- diffinv(bsts_24_pred$interval[1,], differences = d)[-1] + tail(train_24$cpu_index, 1)
bsts_24_upper_undiff <- diffinv(bsts_24_pred$interval[2,], differences = d)[-1] + tail(train_24$cpu_index, 1)
bsts_24_forecast <- data.frame(mean = bsts_24_pred_undiff, LL = bsts_24_lower_undiff, UL = bsts_24_upper_undiff)

model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_forecast$mean, model = 'BSTS - Differenced'))
predict_24 <- predict_24 %>% mutate('BSTS - Differenced' = bsts_24_forecast$mean, 'BSTS - Differenced - LL' = bsts_24_lower_undiff, 'BSTS - Differenced - UL' = bsts_24_upper_undiff)


write.csv(model_evaluate_24, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Integer - Horizon 24.csv', row.names = FALSE)
write.csv(predict_24, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Integer - Forecast 24.csv', row.names = FALSE)



##################################################### Horizon 36 #####################################################

n = 36
set.seed(100)

train_36 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_36 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_36 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_36 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_36 <- tibble()  
predict_36 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_36) + 1:length(test_36)]))  



# BSTS Differenced

arima_36 <- auto.arima(train_36$cpu_index)
d <- ndiffs(train_36$cpu_index)

if(d == arima_36$arma[6]){
  print('YES')
}

differenced_series_36 <- c(0,diff(train_36$cpu_index, d))

ss <- AddLocalLevel(list(), differenced_series_36)
ss <- AddSeasonal(ss, differenced_series_36, nseasons = 12)
bsts_36 <- bsts(cpu_index_diff ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10,
                data = as.data.frame(cbind(differenced_series_36, train_reg_reduced_36)) %>% rename('cpu_index_diff' = differenced_series_36))
bsts_36_pred <- predict(bsts_36, horizon = n, burn = SuggestBurn(.1, bsts_36), newdata = test_reg_reduced_36, quantiles = c(0.1,0.9))

bsts_36_pred_undiff <- diffinv(bsts_36_pred$mean, differences = d)[-1] + tail(train_36$cpu_index, 1)
bsts_36_lower_undiff <- diffinv(bsts_36_pred$interval[1,], differences = d)[-1] + tail(train_36$cpu_index, 1)
bsts_36_upper_undiff <- diffinv(bsts_36_pred$interval[2,], differences = d)[-1] + tail(train_36$cpu_index, 1)
bsts_36_forecast <- data.frame(mean = bsts_36_pred_undiff, LL = bsts_36_lower_undiff, UL = bsts_36_upper_undiff)

model_evaluate_36 <- rbind(model_evaluate_36, evaluate(test_36, bsts_36_forecast$mean, model = 'BSTS - Differenced'))
predict_36 <- predict_36 %>% mutate('BSTS - Differenced' = bsts_36_forecast$mean, 'BSTS - Differenced - LL' = bsts_36_lower_undiff, 'BSTS - Differenced - UL' = bsts_36_upper_undiff)


write.csv(model_evaluate_36, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Integer - Horizon 36.csv', row.names = FALSE)
write.csv(predict_36, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Integer - Forecast 36.csv', row.names = FALSE)




##################################################### Horizon 48 #####################################################

n = 48
set.seed(100)

train_48 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_48 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_48 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_48 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_48 <- tibble()  
predict_48 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_48) + 1:length(test_48)]))  



# BSTS Differenced

arima_48 <- auto.arima(train_48$cpu_index)
d <- ndiffs(train_48$cpu_index)

if(d == arima_48$arma[6]){
  print('YES')
}

differenced_series_48 <- c(0,diff(train_48$cpu_index, d))

ss <- AddLocalLevel(list(), differenced_series_48)
ss <- AddSeasonal(ss, differenced_series_48, nseasons = 12)
bsts_48 <- bsts(cpu_index_diff ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10,
                data = as.data.frame(cbind(differenced_series_48, train_reg_reduced_48)) %>% rename('cpu_index_diff' = differenced_series_48))
bsts_48_pred <- predict(bsts_48, horizon = n, burn = SuggestBurn(.1, bsts_48), newdata = test_reg_reduced_48, quantiles = c(0.1,0.9))

bsts_48_pred_undiff <- diffinv(bsts_48_pred$mean, differences = d)[-1] + tail(train_48$cpu_index, 1)
bsts_48_lower_undiff <- diffinv(bsts_48_pred$interval[1,], differences = d)[-1] + tail(train_48$cpu_index, 1)
bsts_48_upper_undiff <- diffinv(bsts_48_pred$interval[2,], differences = d)[-1] + tail(train_48$cpu_index, 1)
bsts_48_forecast <- data.frame(mean = bsts_48_pred_undiff, LL = bsts_48_lower_undiff, UL = bsts_48_upper_undiff)

model_evaluate_48 <- rbind(model_evaluate_48, evaluate(test_48, bsts_48_forecast$mean, model = 'BSTS - Differenced'))
predict_48 <- predict_48 %>% mutate('BSTS - Differenced' = bsts_48_forecast$mean, 'BSTS - Differenced - LL' = bsts_48_lower_undiff, 'BSTS - Differenced - UL' = bsts_48_upper_undiff)


write.csv(model_evaluate_48, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Integer - Horizon 48.csv', row.names = FALSE)
write.csv(predict_48, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Integer - Forecast 48.csv', row.names = FALSE)









########################################################### Fractional Differencing ########################################################### 3

##################################################### Horizon 1 #####################################################

n = 1
set.seed(100)

train_1 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_1 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_1 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_1 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_1 <- tibble()  
predict_1 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_1) + 1:length(test_1)]))  

bsts_1 <- frac_bsts(train_1$cpu_index, train_reg_reduced_1)
bsts_1_pred <- frac_bsts_forecast(bsts_1, newdata = test_reg_reduced_1, h = n, quantiles = c(0.005,0.995))

model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, bsts_1_pred$mean, model = 'BSTS - Differenced'))
predict_1 <- predict_1 %>% mutate('BSTS - FracDiff' = bsts_1_pred$mean)

write.csv(model_evaluate_1, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Fraction - Horizon 1.csv', row.names = FALSE)
write.csv(predict_1, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Fraction - Forecast 1.csv', row.names = FALSE)



##################################################### Horizon 3 #####################################################

n = 3
set.seed(100)

train_3 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_3 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_3 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_3 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_3 <- tibble()  
predict_3 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_3) + 1:length(test_3)]))  

bsts_3 <- frac_bsts(train_3$cpu_index, train_reg_reduced_3)
bsts_3_pred <- frac_bsts_forecast(bsts_3, newdata = test_reg_reduced_3, h = n, quantiles = c(0.005,0.995))

model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_pred$mean, model = 'BSTS - Differenced'))
predict_3 <- predict_3 %>% mutate('BSTS - FracDiff' = bsts_3_pred$mean)

write.csv(model_evaluate_3, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Fraction - Horizon 3.csv', row.names = FALSE)
write.csv(predict_3, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Fraction - Forecast 3.csv', row.names = FALSE)



##################################################### Horizon 6 #####################################################

n = 6
set.seed(100)

train_6 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_6 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_6 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_6 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_6 <- tibble()  
predict_6 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_6) + 1:length(test_6)]))  

bsts_6 <- frac_bsts(train_6$cpu_index, train_reg_reduced_6)
bsts_6_pred <- frac_bsts_forecast(bsts_6, newdata = test_reg_reduced_6, h = n, quantiles = c(0.005,0.995))

model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_pred$mean, model = 'BSTS - Differenced'))
predict_6 <- predict_6 %>% mutate('BSTS - FracDiff' = bsts_6_pred$mean)

write.csv(model_evaluate_6, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Fraction - Horizon 6.csv', row.names = FALSE)
write.csv(predict_6, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Fraction - Forecast 6.csv', row.names = FALSE)



##################################################### Horizon 12 #####################################################

n = 12
set.seed(100)

train_12 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_12 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_12 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_12 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_12 <- tibble()  
predict_12 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_12) + 1:length(test_12)]))  

bsts_12 <- frac_bsts(train_12$cpu_index, train_reg_reduced_12)
bsts_12_pred <- frac_bsts_forecast(bsts_12, newdata = test_reg_reduced_12, h = n, quantiles = c(0.005,0.995))

model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_pred$mean, model = 'BSTS - Differenced'))
predict_12 <- predict_12 %>% mutate('BSTS - FracDiff' = bsts_12_pred$mean)

write.csv(model_evaluate_12, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Fraction - Horizon 12.csv', row.names = FALSE)
write.csv(predict_12, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Fraction - Forecast 12.csv', row.names = FALSE)



##################################################### Horizon 24 #####################################################

n = 24
set.seed(100)

train_24 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_24 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_24 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_24 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_24 <- tibble()  
predict_24 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_24) + 1:length(test_24)]))  

bsts_24 <- frac_bsts(train_24$cpu_index, train_reg_reduced_24)
bsts_24_pred <- frac_bsts_forecast(bsts_24, newdata = test_reg_reduced_24, h = n, quantiles = c(0.005,0.995))

model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_pred$mean, model = 'BSTS - Differenced'))
predict_24 <- predict_24 %>% mutate('BSTS - FracDiff' = bsts_24_pred$mean)

write.csv(model_evaluate_24, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Fraction - Horizon 24.csv', row.names = FALSE)
write.csv(predict_24, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Fraction - Forecast 24.csv', row.names = FALSE)



##################################################### Horizon 48 #####################################################

n = 48
set.seed(100)

train_48 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_48 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_48 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_48 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_48 <- tibble()  
predict_48 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_48) + 1:length(test_48)]))  

bsts_48 <- frac_bsts(train_48$cpu_index, train_reg_reduced_48)
bsts_48_pred <- frac_bsts_forecast(bsts_48, newdata = test_reg_reduced_48, h = n, quantiles = c(0.005,0.995))

model_evaluate_48 <- rbind(model_evaluate_48, evaluate(test_48, bsts_48_pred$mean, model = 'BSTS - Differenced'))
predict_48 <- predict_48 %>% mutate('BSTS - FracDiff' = bsts_48_pred$mean)

write.csv(model_evaluate_48, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Fraction - Horizon 48.csv', row.names = FALSE)
write.csv(predict_48, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Differenced/Fraction - Forecast 48.csv', row.names = FALSE)
