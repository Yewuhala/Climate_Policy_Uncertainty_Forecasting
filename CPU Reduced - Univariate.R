library(ggplot2)
library(readr)
library(bsts)
library(Metrics)
library(dplyr)
library(forecast)
library(fpp3)
library(RTransferEntropy)
library(readxl)

setwd('C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network')
cpu_data <- read_xlsx('CPU_Data.xlsx')[,c(1,139)]
cpu_data$Date <- as.Date(cpu_data$Date)
cpu_data$Date <- ydm(cpu_data$Date)

cpu <- cpu_data[,2]
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


##################################################### Horizon 1 #####################################################

n = 1
set.seed(100)

train_1 <- cpu[1:(length(cpu)-n)]
test_1 <- cpu[(length(cpu) - n + 1):length(cpu)]

model_evaluate_1 <- tibble()  
predict_1 <- tibble(Date = as.Date(cpu_data$Date[length(train_1) + 1:length(test_1)]))  


# ARFIMA
arfima_1 <- arfima(train_1)
arfima_1_pred <- forecast(arfima_1, h = n)
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, arfima_1_pred$mean, model = 'ARFIMA'))
predict_1 <- predict_1 %>% mutate('ARFIMA' = arfima_1_pred$mean)


# ARIMA
arima_1 <- auto.arima(train_1)
arima_1_pred <- forecast(arima_1, h = n)
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, arima_1_pred$mean,  model = 'ARIMA'))
predict_1 <- predict_1 %>% mutate('ARIMA' = arima_1_pred$mean)


# ARNN
arnn_1 <- nnetar(train_1)
arnn_1_pred <- forecast(arnn_1, h = n)
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, arnn_1_pred$mean,  model = 'ARNN'))
predict_1 <- predict_1 %>% mutate('ARNN' = arnn_1_pred$mean)


# BSTS LS
ss <- AddLocalLevel(list(), train_1)
ss <- AddSeasonal(ss, train_1, nseasons = 12)
bsts_1_LS <- bsts(train_1, 
                  state.specification = ss,
                  niter = 1000,
                  ping = 100,
                  seed = 100,
                  expected.model.size = 10)
bsts_1_LS_pred <- predict(bsts_1_LS, horizon = n, burn = SuggestBurn(.1, bsts_1_LS))
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, bsts_1_LS_pred$mean, model = 'BSTS - LS'))
predict_1 <- predict_1 %>% mutate('BSTS - LS' = bsts_1_LS_pred$mean)


# BSTS TS
ss <- AddLocalLinearTrend(list(), train_1)
ss <- AddSeasonal(ss, train_1, nseasons = 12)
bsts_1_TS <- bsts(train_1, 
                  state.specification = ss,
                  niter = 1000,
                  ping = 100,
                  seed = 100,
                  expected.model.size = 10)
bsts_1_TS_pred <- predict(bsts_1_TS, horizon = n, burn = SuggestBurn(.1, bsts_1_TS))
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, bsts_1_TS_pred$mean, model = 'BSTS - TS'))
predict_1 <- predict_1 %>% mutate('BSTS - TS' = bsts_1_TS_pred$mean)


# BSTS Differenced
d <- ndiffs(train_1)
differenced_series_1 <- c(0,diff(train_1, d))

ss <- AddLocalLevel(list(), differenced_series_1)
ss <- AddSeasonal(ss, differenced_series_1, nseasons = 12)
bsts_1 <- bsts(differenced_series_1, 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10)
bsts_1_pred <- predict(bsts_1, horizon = n, burn = SuggestBurn(.1, bsts_1), quantiles = c(0.1,0.9))

bsts_1_pred_undiff <- diffinv(bsts_1_pred$mean, differences = d)[-1] + tail(train_1, 1)
bsts_1_lower_undiff <- diffinv(bsts_1_pred$interval[1,], differences = d)[-1] + tail(train_1, 1)
bsts_1_upper_undiff <- diffinv(bsts_1_pred$interval[2,], differences = d)[-1] + tail(train_1, 1)
bsts_1_forecast <- data.frame(mean = bsts_1_pred_undiff, LL = bsts_1_lower_undiff, UL = bsts_1_upper_undiff)

model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, bsts_1_forecast$mean, model = 'BSTS - Differenced'))
predict_1 <- predict_1 %>% mutate('BSTS - Differenced' = bsts_1_forecast$mean, 'BSTS - Differenced - LL' = bsts_1_lower_undiff, 'BSTS - Differenced - UL' = bsts_1_upper_undiff)


# Fractional BSTS 
bsts_1 <- frac_bsts(train_1)
bsts_1_pred <- frac_bsts_forecast(bsts_1, h = n, quantiles = c(0.005,0.995))

model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, bsts_1_pred$mean, model = 'BSTS - FracDiff'))
predict_1 <- predict_1 %>% mutate('BSTS - FracDiff' = bsts_1_pred$mean)


write.csv(model_evaluate_1, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Univariate Models/Results/Horizon 1.csv', row.names = FALSE)
write.csv(predict_1, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Univariate Models/Results/Forecast 1.csv', row.names = FALSE)



##################################################### Horizon 3 #####################################################

n = 3
set.seed(100)

train_3 <- cpu[1:(length(cpu)-n)]
test_3 <- cpu[(length(cpu) - n + 1):length(cpu)]

model_evaluate_3 <- tibble()  
predict_3 <- tibble(Date = as.Date(cpu_data$Date[length(train_3) + 1:length(test_3)]))  


# ARFIMA
arfima_3 <- arfima(train_3)
arfima_3_pred <- forecast(arfima_3, h = n)
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, arfima_3_pred$mean, model = 'ARFIMA'))
predict_3 <- predict_3 %>% mutate('ARFIMA' = arfima_3_pred$mean)


# ARIMA
arima_3 <- auto.arima(train_3)
arima_3_pred <- forecast(arima_3, h = n)
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, arima_3_pred$mean,  model = 'ARIMA'))
predict_3 <- predict_3 %>% mutate('ARIMA' = arima_3_pred$mean)


# ARNN
arnn_3 <- nnetar(train_3)
arnn_3_pred <- forecast(arnn_3, h = n)
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, arnn_3_pred$mean,  model = 'ARNN'))
predict_3 <- predict_3 %>% mutate('ARNN' = arnn_3_pred$mean)


# BSTS LS
ss <- AddLocalLevel(list(), train_3)
ss <- AddSeasonal(ss, train_3, nseasons = 12)
bsts_3_LS <- bsts(train_3, 
                  state.specification = ss,
                  niter = 1000,
                  ping = 100,
                  seed = 100,
                  expected.model.size = 10)
bsts_3_LS_pred <- predict(bsts_3_LS, horizon = n, burn = SuggestBurn(.1, bsts_3_LS))
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_LS_pred$mean, model = 'BSTS - LS'))
predict_3 <- predict_3 %>% mutate('BSTS - LS' = bsts_3_LS_pred$mean)


# BSTS TS
ss <- AddLocalLinearTrend(list(), train_3)
ss <- AddSeasonal(ss, train_3, nseasons = 12)
bsts_3_TS <- bsts(train_3, 
                  state.specification = ss,
                  niter = 1000,
                  ping = 100,
                  seed = 100,
                  expected.model.size = 10)
bsts_3_TS_pred <- predict(bsts_3_TS, horizon = n, burn = SuggestBurn(.1, bsts_3_TS))
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_TS_pred$mean, model = 'BSTS - TS'))
predict_3 <- predict_3 %>% mutate('BSTS - TS' = bsts_3_TS_pred$mean)


# BSTS Differenced
d <- ndiffs(train_3)
differenced_series_3 <- c(0,diff(train_3, d))

ss <- AddLocalLevel(list(), differenced_series_3)
ss <- AddSeasonal(ss, differenced_series_3, nseasons = 12)
bsts_3 <- bsts(differenced_series_3, 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10)
bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), quantiles = c(0.1,0.9))

bsts_3_pred_undiff <- diffinv(bsts_3_pred$mean, differences = d)[-1] + tail(train_3, 1)
bsts_3_lower_undiff <- diffinv(bsts_3_pred$interval[1,], differences = d)[-1] + tail(train_3, 1)
bsts_3_upper_undiff <- diffinv(bsts_3_pred$interval[2,], differences = d)[-1] + tail(train_3, 1)
bsts_3_forecast <- data.frame(mean = bsts_3_pred_undiff, LL = bsts_3_lower_undiff, UL = bsts_3_upper_undiff)

model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_forecast$mean, model = 'BSTS - Differenced'))
predict_3 <- predict_3 %>% mutate('BSTS - Differenced' = bsts_3_forecast$mean, 'BSTS - Differenced - LL' = bsts_3_lower_undiff, 'BSTS - Differenced - UL' = bsts_3_upper_undiff)


# Fractional BSTS 
bsts_3 <- frac_bsts(train_3)
bsts_3_pred <- frac_bsts_forecast(bsts_3, h = n, quantiles = c(0.005,0.995))

model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_pred$mean, model = 'BSTS - FracDiff'))
predict_3 <- predict_3 %>% mutate('BSTS - FracDiff' = bsts_3_pred$mean)


write.csv(model_evaluate_3, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Univariate Models/Results/Horizon 3.csv', row.names = FALSE)
write.csv(predict_3, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Univariate Models/Results/Forecast 3.csv', row.names = FALSE)



##################################################### Horizon 6 #####################################################

n = 6
set.seed(100)

train_6 <- cpu[1:(length(cpu)-n)]
test_6 <- cpu[(length(cpu) - n + 1):length(cpu)]

model_evaluate_6 <- tibble()  
predict_6 <- tibble(Date = as.Date(cpu_data$Date[length(train_6) + 1:length(test_6)]))  


# ARFIMA
arfima_6 <- arfima(train_6)
arfima_6_pred <- forecast(arfima_6, h = n)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, arfima_6_pred$mean, model = 'ARFIMA'))
predict_6 <- predict_6 %>% mutate('ARFIMA' = arfima_6_pred$mean)


# ARIMA
arima_6 <- auto.arima(train_6)
arima_6_pred <- forecast(arima_6, h = n)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, arima_6_pred$mean,  model = 'ARIMA'))
predict_6 <- predict_6 %>% mutate('ARIMA' = arima_6_pred$mean)


# ARNN
arnn_6 <- nnetar(train_6)
arnn_6_pred <- forecast(arnn_6, h = n)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, arnn_6_pred$mean,  model = 'ARNN'))
predict_6 <- predict_6 %>% mutate('ARNN' = arnn_6_pred$mean)


# BSTS LS
ss <- AddLocalLevel(list(), train_6)
ss <- AddSeasonal(ss, train_6, nseasons = 12)
bsts_6_LS <- bsts(train_6, 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10)
bsts_6_LS_pred <- predict(bsts_6_LS, horizon = n, burn = SuggestBurn(.1, bsts_6_LS))
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_LS_pred$mean, model = 'BSTS - LS'))
predict_6 <- predict_6 %>% mutate('BSTS - LS' = bsts_6_LS_pred$mean)


# BSTS TS
ss <- AddLocalLinearTrend(list(), train_6)
ss <- AddSeasonal(ss, train_6, nseasons = 12)
bsts_6_TS <- bsts(train_6, 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10)
bsts_6_TS_pred <- predict(bsts_6_TS, horizon = n, burn = SuggestBurn(.1, bsts_6_TS))
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_TS_pred$mean, model = 'BSTS - TS'))
predict_6 <- predict_6 %>% mutate('BSTS - TS' = bsts_6_TS_pred$mean)


# BSTS Differenced
d <- ndiffs(train_6)
differenced_series_6 <- c(0,diff(train_6, d))

ss <- AddLocalLevel(list(), differenced_series_6)
ss <- AddSeasonal(ss, differenced_series_6, nseasons = 12)
bsts_6 <- bsts(differenced_series_6, 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10)
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), quantiles = c(0.1,0.9))

bsts_6_pred_undiff <- diffinv(bsts_6_pred$mean, differences = d)[-1] + tail(train_6, 1)
bsts_6_lower_undiff <- diffinv(bsts_6_pred$interval[1,], differences = d)[-1] + tail(train_6, 1)
bsts_6_upper_undiff <- diffinv(bsts_6_pred$interval[2,], differences = d)[-1] + tail(train_6, 1)
bsts_6_forecast <- data.frame(mean = bsts_6_pred_undiff, LL = bsts_6_lower_undiff, UL = bsts_6_upper_undiff)

model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_forecast$mean, model = 'BSTS - Differenced'))
predict_6 <- predict_6 %>% mutate('BSTS - Differenced' = bsts_6_forecast$mean, 'BSTS - Differenced - LL' = bsts_6_lower_undiff, 'BSTS - Differenced - UL' = bsts_6_upper_undiff)


# Fractional BSTS 
bsts_6 <- frac_bsts(train_6)
bsts_6_pred <- frac_bsts_forecast(bsts_6, h = n, quantiles = c(0.005,0.995))

model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_pred$mean, model = 'BSTS - FracDiff'))
predict_6 <- predict_6 %>% mutate('BSTS - FracDiff' = bsts_6_pred$mean)


write.csv(model_evaluate_6, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Univariate Models/Results/Horizon 6.csv', row.names = FALSE)
write.csv(predict_6, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Univariate Models/Results/Forecast 6.csv', row.names = FALSE)



##################################################### Horizon 12 #####################################################

n = 12
set.seed(100)

train_12 <- cpu[1:(length(cpu)-n)]
test_12 <- cpu[(length(cpu) - n + 1):length(cpu)]

model_evaluate_12 <- tibble()  
predict_12 <- tibble(Date = as.Date(cpu_data$Date[length(train_12) + 1:length(test_12)]))  


# ARFIMA
arfima_12 <- arfima(train_12)
arfima_12_pred <- forecast(arfima_12, h = n)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, arfima_12_pred$mean, model = 'ARFIMA'))
predict_12 <- predict_12 %>% mutate('ARFIMA' = arfima_12_pred$mean)


# ARIMA
arima_12 <- auto.arima(train_12)
arima_12_pred <- forecast(arima_12, h = n)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, arima_12_pred$mean,  model = 'ARIMA'))
predict_12 <- predict_12 %>% mutate('ARIMA' = arima_12_pred$mean)


# ARNN
arnn_12 <- nnetar(train_12)
arnn_12_pred <- forecast(arnn_12, h = n)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, arnn_12_pred$mean,  model = 'ARNN'))
predict_12 <- predict_12 %>% mutate('ARNN' = arnn_12_pred$mean)



# BSTS LS
ss <- AddLocalLevel(list(), train_12)
ss <- AddSeasonal(ss, train_12, nseasons = 12)
bsts_12_LS <- bsts(train_12, 
                  state.specification = ss,
                  niter = 1000,
                  ping = 100,
                  seed = 100,
                  expected.model.size = 10)
bsts_12_LS_pred <- predict(bsts_12_LS, horizon = n, burn = SuggestBurn(.1, bsts_12_LS))
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_LS_pred$mean, model = 'BSTS - LS'))
predict_12 <- predict_12 %>% mutate('BSTS - LS' = bsts_12_LS_pred$mean)


# BSTS TS
ss <- AddLocalLinearTrend(list(), train_12)
ss <- AddSeasonal(ss, train_12, nseasons = 12)
bsts_12_TS <- bsts(train_12, 
                  state.specification = ss,
                  niter = 1000,
                  ping = 100,
                  seed = 100,
                  expected.model.size = 10)
bsts_12_TS_pred <- predict(bsts_12_TS, horizon = n, burn = SuggestBurn(.1, bsts_12_TS))
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_TS_pred$mean, model = 'BSTS - TS'))
predict_12 <- predict_12 %>% mutate('BSTS - TS' = bsts_12_TS_pred$mean)


# BSTS Differenced
d <- ndiffs(train_12)
differenced_series_12 <- c(0,diff(train_12, d))

ss <- AddLocalLevel(list(), differenced_series_12)
ss <- AddSeasonal(ss, differenced_series_12, nseasons = 12)
bsts_12 <- bsts(differenced_series_12, 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10)
bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), quantiles = c(0.1,0.9))

bsts_12_pred_undiff <- diffinv(bsts_12_pred$mean, differences = d)[-1] + tail(train_12, 1)
bsts_12_lower_undiff <- diffinv(bsts_12_pred$interval[1,], differences = d)[-1] + tail(train_12, 1)
bsts_12_upper_undiff <- diffinv(bsts_12_pred$interval[2,], differences = d)[-1] + tail(train_12, 1)
bsts_12_forecast <- data.frame(mean = bsts_12_pred_undiff, LL = bsts_12_lower_undiff, UL = bsts_12_upper_undiff)

model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_forecast$mean, model = 'BSTS - Differenced'))
predict_12 <- predict_12 %>% mutate('BSTS - Differenced' = bsts_12_forecast$mean, 'BSTS - Differenced - LL' = bsts_12_lower_undiff, 'BSTS - Differenced - UL' = bsts_12_upper_undiff)


# Fractional BSTS 
bsts_12 <- frac_bsts(train_12)
bsts_12_pred <- frac_bsts_forecast(bsts_12, h = n, quantiles = c(0.005,0.995))

model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_pred$mean, model = 'BSTS - FracDiff'))
predict_12 <- predict_12 %>% mutate('BSTS - FracDiff' = bsts_12_pred$mean)


write.csv(model_evaluate_12, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Univariate Models/Results/Horizon 12.csv', row.names = FALSE)
write.csv(predict_12, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Univariate Models/Results/Forecast 12.csv', row.names = FALSE)



##################################################### Horizon 24 #####################################################

n = 24
set.seed(100)

train_24 <- cpu[1:(length(cpu)-n)]
test_24 <- cpu[(length(cpu) - n + 1):length(cpu)]

model_evaluate_24 <- tibble()  
predict_24 <- tibble(Date = as.Date(cpu_data$Date[length(train_24) + 1:length(test_24)]))  


# ARFIMA
arfima_24 <- arfima(train_24)
arfima_24_pred <- forecast(arfima_24, h = n)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, arfima_24_pred$mean, model = 'ARFIMA'))
predict_24 <- predict_24 %>% mutate('ARFIMA' = arfima_24_pred$mean)


# ARIMA
arima_24 <- auto.arima(train_24)
arima_24_pred <- forecast(arima_24, h = n)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, arima_24_pred$mean,  model = 'ARIMA'))
predict_24 <- predict_24 %>% mutate('ARIMA' = arima_24_pred$mean)


# ARNN
arnn_24 <- nnetar(train_24)
arnn_24_pred <- forecast(arnn_24, h = n)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, arnn_24_pred$mean,  model = 'ARNN'))
predict_24 <- predict_24 %>% mutate('ARNN' = arnn_24_pred$mean)


# BSTS LS
ss <- AddLocalLevel(list(), train_24)
ss <- AddSeasonal(ss, train_24, nseasons = 12)
bsts_24_LS <- bsts(train_24, 
                  state.specification = ss,
                  niter = 1000,
                  ping = 100,
                  seed = 100,
                  expected.model.size = 10)
bsts_24_LS_pred <- predict(bsts_24_LS, horizon = n, burn = SuggestBurn(.1, bsts_24_LS))
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_LS_pred$mean, model = 'BSTS - LS'))
predict_24 <- predict_24 %>% mutate('BSTS - LS' = bsts_24_LS_pred$mean)


# BSTS TS
ss <- AddLocalLinearTrend(list(), train_24)
ss <- AddSeasonal(ss, train_24, nseasons = 12)
bsts_24_TS <- bsts(train_24, 
                  state.specification = ss,
                  niter = 1000,
                  ping = 100,
                  seed = 100,
                  expected.model.size = 10)
bsts_24_TS_pred <- predict(bsts_24_TS, horizon = n, burn = SuggestBurn(.1, bsts_24_TS))
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_TS_pred$mean, model = 'BSTS - TS'))
predict_24 <- predict_24 %>% mutate('BSTS - TS' = bsts_24_TS_pred$mean)


# BSTS Differenced
d <- ndiffs(train_24)
differenced_series_24 <- c(0,diff(train_24, d))

ss <- AddLocalLevel(list(), differenced_series_24)
ss <- AddSeasonal(ss, differenced_series_24, nseasons = 12)
bsts_24 <- bsts(differenced_series_24, 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10)
bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), quantiles = c(0.1,0.9))

bsts_24_pred_undiff <- diffinv(bsts_24_pred$mean, differences = d)[-1] + tail(train_24, 1)
bsts_24_lower_undiff <- diffinv(bsts_24_pred$interval[1,], differences = d)[-1] + tail(train_24, 1)
bsts_24_upper_undiff <- diffinv(bsts_24_pred$interval[2,], differences = d)[-1] + tail(train_24, 1)
bsts_24_forecast <- data.frame(mean = bsts_24_pred_undiff, LL = bsts_24_lower_undiff, UL = bsts_24_upper_undiff)

model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_forecast$mean, model = 'BSTS - Differenced'))
predict_24 <- predict_24 %>% mutate('BSTS - Differenced' = bsts_24_forecast$mean, 'BSTS - Differenced - LL' = bsts_24_lower_undiff, 'BSTS - Differenced - UL' = bsts_24_upper_undiff)


# Fractional BSTS 
bsts_24 <- frac_bsts(train_24)
bsts_24_pred <- frac_bsts_forecast(bsts_24, h = n, quantiles = c(0.005,0.995))

model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_pred$mean, model = 'BSTS - FracDiff'))
predict_24 <- predict_24 %>% mutate('BSTS - FracDiff' = bsts_24_pred$mean)


write.csv(model_evaluate_24, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Univariate Models/Results/Horizon 24.csv', row.names = FALSE)
write.csv(predict_24, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Univariate Models/Results/Forecast 24.csv', row.names = FALSE)



##################################################### Horizon 36 #####################################################

n = 36
set.seed(100)

train_36 <- cpu[1:(length(cpu)-n)]
test_36 <- cpu[(length(cpu) - n + 1):length(cpu)]

model_evaluate_36 <- tibble()  
predict_36 <- tibble(Date = as.Date(cpu_data$Date[length(train_36) + 1:length(test_36)]))  


# ARFIMA
arfima_36 <- arfima(train_36)
arfima_36_pred <- forecast(arfima_36, h = n)
model_evaluate_36 <- rbind(model_evaluate_36, evaluate(test_36, arfima_36_pred$mean, model = 'ARFIMA'))
predict_36 <- predict_36 %>% mutate('ARFIMA' = arfima_36_pred$mean)


# ARIMA
arima_36 <- auto.arima(train_36)
arima_36_pred <- forecast(arima_36, h = n)
model_evaluate_36 <- rbind(model_evaluate_36, evaluate(test_36, arima_36_pred$mean,  model = 'ARIMA'))
predict_36 <- predict_36 %>% mutate('ARIMA' = arima_36_pred$mean)


# ARNN
arnn_36 <- nnetar(train_36)
arnn_36_pred <- forecast(arnn_36, h = n)
model_evaluate_36 <- rbind(model_evaluate_36, evaluate(test_36, arnn_36_pred$mean,  model = 'ARNN'))
predict_36 <- predict_36 %>% mutate('ARNN' = arnn_36_pred$mean)


# BSTS LS
ss <- AddLocalLevel(list(), train_36)
ss <- AddSeasonal(ss, train_36, nseasons = 12)
bsts_36_LS <- bsts(train_36, 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   expected.model.size = 10)
bsts_36_LS_pred <- predict(bsts_36_LS, horizon = n, burn = SuggestBurn(.1, bsts_36_LS))
model_evaluate_36 <- rbind(model_evaluate_36, evaluate(test_36, bsts_36_LS_pred$mean, model = 'BSTS - LS'))
predict_36 <- predict_36 %>% mutate('BSTS - LS' = bsts_36_LS_pred$mean)


# BSTS TS
ss <- AddLocalLinearTrend(list(), train_36)
ss <- AddSeasonal(ss, train_36, nseasons = 12)
bsts_36_TS <- bsts(train_36, 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   expected.model.size = 10)
bsts_36_TS_pred <- predict(bsts_36_TS, horizon = n, burn = SuggestBurn(.1, bsts_36_TS))
model_evaluate_36 <- rbind(model_evaluate_36, evaluate(test_36, bsts_36_TS_pred$mean, model = 'BSTS - TS'))
predict_36 <- predict_36 %>% mutate('BSTS - TS' = bsts_36_TS_pred$mean)


# BSTS Differenced
d <- ndiffs(train_36)
differenced_series_36 <- c(0,diff(train_36, d))

ss <- AddLocalLevel(list(), differenced_series_36)
ss <- AddSeasonal(ss, differenced_series_36, nseasons = 12)
bsts_36 <- bsts(differenced_series_36, 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10)
bsts_36_pred <- predict(bsts_36, horizon = n, burn = SuggestBurn(.1, bsts_36), quantiles = c(0.1,0.9))

bsts_36_pred_undiff <- diffinv(bsts_36_pred$mean, differences = d)[-1] + tail(train_36, 1)
bsts_36_lower_undiff <- diffinv(bsts_36_pred$interval[1,], differences = d)[-1] + tail(train_36, 1)
bsts_36_upper_undiff <- diffinv(bsts_36_pred$interval[2,], differences = d)[-1] + tail(train_36, 1)
bsts_36_forecast <- data.frame(mean = bsts_36_pred_undiff, LL = bsts_36_lower_undiff, UL = bsts_36_upper_undiff)

model_evaluate_36 <- rbind(model_evaluate_36, evaluate(test_36, bsts_36_forecast$mean, model = 'BSTS - Differenced'))
predict_36 <- predict_36 %>% mutate('BSTS - Differenced' = bsts_36_forecast$mean, 'BSTS - Differenced - LL' = bsts_36_lower_undiff, 'BSTS - Differenced - UL' = bsts_36_upper_undiff)


# Fractional BSTS 
bsts_36 <- frac_bsts(train_36)
bsts_36_pred <- frac_bsts_forecast(bsts_36, h = n, quantiles = c(0.005,0.995))

model_evaluate_36 <- rbind(model_evaluate_36, evaluate(test_36, bsts_36_pred$mean, model = 'BSTS - FracDiff'))
predict_36 <- predict_36 %>% mutate('BSTS - FracDiff' = bsts_36_pred$mean)


write.csv(model_evaluate_36, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Univariate Models/Results/Horizon 36.csv', row.names = FALSE)
write.csv(predict_36, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Univariate Models/Results/Forecast 36.csv', row.names = FALSE)



##################################################### Horizon 48 #####################################################

n = 48
set.seed(100)

train_48 <- cpu[1:(length(cpu)-n)]
test_48 <- cpu[(length(cpu) - n + 1):length(cpu)]

model_evaluate_48 <- tibble()  
predict_48 <- tibble(Date = as.Date(cpu_data$Date[length(train_48) + 1:length(test_48)]))  


# ARFIMA
arfima_48 <- arfima(train_48)
arfima_48_pred <- forecast(arfima_48, h = n)
model_evaluate_48 <- rbind(model_evaluate_48, evaluate(test_48, arfima_48_pred$mean, model = 'ARFIMA'))
predict_48 <- predict_48 %>% mutate('ARFIMA' = arfima_48_pred$mean)


# ARIMA
arima_48 <- auto.arima(train_48)
arima_48_pred <- forecast(arima_48, h = n)
model_evaluate_48 <- rbind(model_evaluate_48, evaluate(test_48, arima_48_pred$mean,  model = 'ARIMA'))
predict_48 <- predict_48 %>% mutate('ARIMA' = arima_48_pred$mean)


# ARNN
arnn_48 <- nnetar(train_48)
arnn_48_pred <- forecast(arnn_48, h = n)
model_evaluate_48 <- rbind(model_evaluate_48, evaluate(test_48, arnn_48_pred$mean,  model = 'ARNN'))
predict_48 <- predict_48 %>% mutate('ARNN' = arnn_48_pred$mean)


# BSTS LS
ss <- AddLocalLevel(list(), train_48)
ss <- AddSeasonal(ss, train_48, nseasons = 12)
bsts_48_LS <- bsts(train_48, 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   expected.model.size = 10)
bsts_48_LS_pred <- predict(bsts_48_LS, horizon = n, burn = SuggestBurn(.1, bsts_48_LS))
model_evaluate_48 <- rbind(model_evaluate_48, evaluate(test_48, bsts_48_LS_pred$mean, model = 'BSTS - LS'))
predict_48 <- predict_48 %>% mutate('BSTS - LS' = bsts_48_LS_pred$mean)


# BSTS TS
ss <- AddLocalLinearTrend(list(), train_48)
ss <- AddSeasonal(ss, train_48, nseasons = 12)
bsts_48_TS <- bsts(train_48, 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   expected.model.size = 10)
bsts_48_TS_pred <- predict(bsts_48_TS, horizon = n, burn = SuggestBurn(.1, bsts_48_TS))
model_evaluate_48 <- rbind(model_evaluate_48, evaluate(test_48, bsts_48_TS_pred$mean, model = 'BSTS - TS'))
predict_48 <- predict_48 %>% mutate('BSTS - TS' = bsts_48_TS_pred$mean)


# BSTS Differenced
d <- ndiffs(train_48)
differenced_series_48 <- c(0,diff(train_48, d))

ss <- AddLocalLevel(list(), differenced_series_48)
ss <- AddSeasonal(ss, differenced_series_48, nseasons = 12)
bsts_48 <- bsts(differenced_series_48, 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10)
bsts_48_pred <- predict(bsts_48, horizon = n, burn = SuggestBurn(.1, bsts_48), quantiles = c(0.1,0.9))

bsts_48_pred_undiff <- diffinv(bsts_48_pred$mean, differences = d)[-1] + tail(train_48, 1)
bsts_48_lower_undiff <- diffinv(bsts_48_pred$interval[1,], differences = d)[-1] + tail(train_48, 1)
bsts_48_upper_undiff <- diffinv(bsts_48_pred$interval[2,], differences = d)[-1] + tail(train_48, 1)
bsts_48_forecast <- data.frame(mean = bsts_48_pred_undiff, LL = bsts_48_lower_undiff, UL = bsts_48_upper_undiff)

model_evaluate_48 <- rbind(model_evaluate_48, evaluate(test_48, bsts_48_forecast$mean, model = 'BSTS - Differenced'))
predict_48 <- predict_48 %>% mutate('BSTS - Differenced' = bsts_48_forecast$mean, 'BSTS - Differenced - LL' = bsts_48_lower_undiff, 'BSTS - Differenced - UL' = bsts_48_upper_undiff)


# Fractional BSTS 
bsts_48 <- frac_bsts(train_48)
bsts_48_pred <- frac_bsts_forecast(bsts_48, h = n, quantiles = c(0.005,0.995))

model_evaluate_48 <- rbind(model_evaluate_48, evaluate(test_48, bsts_48_pred$mean, model = 'BSTS - FracDiff'))
predict_48 <- predict_48 %>% mutate('BSTS - FracDiff' = bsts_48_pred$mean)


write.csv(model_evaluate_48, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Univariate Models/Results/Horizon 48.csv', row.names = FALSE)
write.csv(predict_48, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Univariate Models/Results/Forecast 48.csv', row.names = FALSE)



##################################################### Plots #####################################################

n = 3
bsts_3_LS_pred <- predict(bsts_3_LS, horizon = n, burn = SuggestBurn(.1, bsts_3_LS), quantiles = c(0.1, 0.9))
bsts_3_TS_pred <- predict(bsts_3_TS, horizon = n, burn = SuggestBurn(.1, bsts_3_TS), quantiles = c(0.1, 0.9))
data_3 <- as_tibble(cbind.data.frame('LL_TS' = as.numeric(bsts_3_TS_pred$interval[1,]),
                                     'UL_TS' = as.numeric(bsts_3_TS_pred$interval[2,]),
                                     'LL_LS' = as.numeric(bsts_3_LS_pred$interval[1,]),
                                     'UL_LS' = as.numeric(bsts_3_LS_pred$interval[2,]),
                                     'Date' = cpu_data$Date[(length(cpu) - n + 1):length(cpu)],
                                     'Test' = test_3,
                                     'BSTS_TS' = bsts_3_TS_pred$mean,
                                     'BSTS_LS' = bsts_3_LS_pred$mean,
                                     'ARNN' = arnn_3_pred$mean,
                                     'ARIMA' = arima_3_pred$mean,
                                     'ARFIMA' = arfima_3_pred$mean))

ggplot(data = data_3, aes(x = Date)) +
  geom_ribbon(aes(ymin = LL_LS, ymax = UL_LS), fill = 'red', alpha = .15) +
  geom_ribbon(aes(ymin = LL_TS, ymax = UL_TS), fill = 'forestgreen', alpha = .25) +
  geom_line(aes(y = Test, color = 'Test'), linewidth = 2) +
  geom_line(aes(y = BSTS_TS, color = 'BSTS TS'), linewidth = 2) +
  geom_line(aes(y = BSTS_LS, color = 'BSTS LS'), linewidth = 2) +
  geom_line(aes(y = ARIMA, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = ARFIMA, color = 'ARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARNN, color = 'ARNN'), linewidth = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m-%d", limits = c(ymd("2023-04-01"), ymd("2023-06-01"))) +
  ylab('CPU Index') +
  xlab('Date') +
  labs(color = 'Models') +
  ggtitle('CPU Index: 3 Month Holdout') +
  scale_color_manual(values = c(
    'BSTS TS' = 'forestgreen',
    'BSTS LS' = 'red',
    'ARIMA' = 'orange',
    'ARFIMA' = 'orchid',
    'ARNN' = 'dodgerblue3', 
    'Test' = 'black'
  )) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'))



n = 6
bsts_6_LS_pred <- predict(bsts_6_LS, horizon = n, burn = SuggestBurn(.1, bsts_6_LS), quantiles = c(0.1, 0.9))
bsts_6_TS_pred <- predict(bsts_6_TS, horizon = n, burn = SuggestBurn(.1, bsts_6_TS), quantiles = c(0.1, 0.9))
data_6 <- as_tibble(cbind.data.frame('LL_TS' = as.numeric(bsts_6_TS_pred$interval[1,]),
                                     'UL_TS' = as.numeric(bsts_6_TS_pred$interval[2,]),
                                     'LL_LS' = as.numeric(bsts_6_LS_pred$interval[1,]),
                                     'UL_LS' = as.numeric(bsts_6_LS_pred$interval[2,]),
                                     'Date' = cpu_data$Date[(length(cpu) - n + 1):length(cpu)],
                                     'Test' = test_6,
                                     'BSTS_TS' = bsts_6_TS_pred$mean,
                                     'BSTS_LS' = bsts_6_LS_pred$mean,
                                     'ARNN' = arnn_6_pred$mean,
                                     'ARIMA' = arima_6_pred$mean,
                                     'ARFIMA' = arfima_6_pred$mean))

ggplot(data = data_6, aes(x = Date)) +
  geom_ribbon(aes(ymin = LL_LS, ymax = UL_LS), fill = 'red', alpha = .15) +
  geom_ribbon(aes(ymin = LL_TS, ymax = UL_TS), fill = 'forestgreen', alpha = .25) +
  geom_line(aes(y = Test, color = 'Test'), linewidth = 2) +
  geom_line(aes(y = BSTS_TS, color = 'BSTS TS'), linewidth = 2) +
  geom_line(aes(y = BSTS_LS, color = 'BSTS LS'), linewidth = 2) +
  geom_line(aes(y = ARIMA, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = ARFIMA, color = 'ARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARNN, color = 'ARNN'), linewidth = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m-%d", limits = c(ymd("2023-01-01"), ymd("2023-06-01"))) +
  ylab('CPU Index') +
  xlab('Date') +
  labs(color = 'Models') +
  ggtitle('CPU Index: 6 Month Holdout') +
  scale_color_manual(values = c(
    'BSTS TS' = 'forestgreen',
    'BSTS LS' = 'red',
    'ARIMA' = 'orange',
    'ARFIMA' = 'orchid',
    'ARNN' = 'dodgerblue3', 
    'Test' = 'black'
  )) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'))



n = 12
bsts_12_LS_pred <- predict(bsts_12_LS, horizon = n, burn = SuggestBurn(.1, bsts_12_LS), quantiles = c(0.1, 0.9))
bsts_12_TS_pred <- predict(bsts_12_TS, horizon = n, burn = SuggestBurn(.1, bsts_12_TS), quantiles = c(0.1, 0.9))
data_12 <- as_tibble(cbind.data.frame('LL_TS' = as.numeric(bsts_12_TS_pred$interval[1,]),
                                     'UL_TS' = as.numeric(bsts_12_TS_pred$interval[2,]),
                                     'LL_LS' = as.numeric(bsts_12_LS_pred$interval[1,]),
                                     'UL_LS' = as.numeric(bsts_12_LS_pred$interval[2,]),
                                     'Date' = cpu_data$Date[(length(cpu) - n + 1):length(cpu)],
                                     'Test' = test_12,
                                     'BSTS_TS' = bsts_12_TS_pred$mean,
                                     'BSTS_LS' = bsts_12_LS_pred$mean,
                                     'ARNN' = arnn_12_pred$mean,
                                     'ARIMA' = arima_12_pred$mean,
                                     'ARFIMA' = arfima_12_pred$mean))

ggplot(data = data_12, aes(x = Date)) +
  geom_ribbon(aes(ymin = LL_LS, ymax = UL_LS), fill = 'red', alpha = .15) +
  geom_ribbon(aes(ymin = LL_TS, ymax = UL_TS), fill = 'forestgreen', alpha = .25) +
  geom_line(aes(y = Test, color = 'Test'), linewidth = 2) +
  geom_line(aes(y = BSTS_TS, color = 'BSTS TS'), linewidth = 2) +
  geom_line(aes(y = BSTS_LS, color = 'BSTS LS'), linewidth = 2) +
  geom_line(aes(y = ARIMA, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = ARFIMA, color = 'ARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARNN, color = 'ARNN'), linewidth = 2) +
  scale_x_date(date_breaks = "2 month", date_labels = "%Y-%m-%d", limits = c(ymd("2022-07-01"), ymd("2023-06-01"))) +
  ylab('CPU Index') +
  xlab('Date') +
  labs(color = 'Models') +
  ggtitle('CPU Index: 12 Month Holdout') +
  scale_color_manual(values = c(
    'BSTS TS' = 'forestgreen',
    'BSTS LS' = 'red',
    'ARIMA' = 'orange',
    'ARFIMA' = 'orchid',
    'ARNN' = 'dodgerblue3', 
    'Test' = 'black'
  )) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'))



n = 24
bsts_24_LS_pred <- predict(bsts_24_LS, horizon = n, burn = SuggestBurn(.1, bsts_24_LS), quantiles = c(0.1, 0.9))
bsts_24_TS_pred <- predict(bsts_24_TS, horizon = n, burn = SuggestBurn(.1, bsts_24_TS), quantiles = c(0.1, 0.9))
data_24 <- as_tibble(cbind.data.frame('LL_TS' = as.numeric(bsts_24_TS_pred$interval[1,]),
                                      'UL_TS' = as.numeric(bsts_24_TS_pred$interval[2,]),
                                      'LL_LS' = as.numeric(bsts_24_LS_pred$interval[1,]),
                                      'UL_LS' = as.numeric(bsts_24_LS_pred$interval[2,]),
                                      'Date' = cpu_data$Date[(length(cpu) - n + 1):length(cpu)],
                                      'Test' = test_24,
                                      'BSTS_TS' = bsts_24_TS_pred$mean,
                                      'BSTS_LS' = bsts_24_LS_pred$mean,
                                      'ARNN' = arnn_24_pred$mean,
                                      'ARIMA' = arima_24_pred$mean,
                                      'ARFIMA' = arfima_24_pred$mean))

ggplot(data = data_24, aes(x = Date)) +
  geom_ribbon(aes(ymin = LL_LS, ymax = UL_LS), fill = 'red', alpha = .15) +
  geom_ribbon(aes(ymin = LL_TS, ymax = UL_TS), fill = 'forestgreen', alpha = .25) +
  geom_line(aes(y = Test, color = 'Test'), linewidth = 2) +
  geom_line(aes(y = BSTS_TS, color = 'BSTS TS'), linewidth = 2) +
  geom_line(aes(y = BSTS_LS, color = 'BSTS LS'), linewidth = 2) +
  geom_line(aes(y = ARIMA, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = ARFIMA, color = 'ARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARNN, color = 'ARNN'), linewidth = 2) +
  scale_x_date(date_breaks = "4 month", date_labels = "%Y-%m-%d", limits = c(ymd("2021-07-01"), ymd("2023-06-01"))) +
  ylab('CPU Index') +
  xlab('Date') +
  labs(color = 'Models') +
  ggtitle('CPU Index: 24 Month Holdout') +
  scale_color_manual(values = c(
    'BSTS TS' = 'forestgreen',
    'BSTS LS' = 'red',
    'ARIMA' = 'orange',
    'ARFIMA' = 'orchid',
    'ARNN' = 'dodgerblue3', 
    'Test' = 'black'
  )) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'))



##################################################### MCB RMSE #####################################################

library(tsutils)
library(PMCMRplus)
library(vioplot)
library(greybox)
library(MTS)
library(readxl)
library(dplyr)

par(mfrow = c(1,1), mfcol = c(1,1))
par(mgp = c(3, 1, 0))
par(mar = c(5.1, 4.1, 4.1, 2.1))

setwd('C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced')
err <- read_excel('Results Combined - RMSE.xlsx')
tsutils::nemenyi(err, plottype = "mcb", main = 'MCB Plot for RMSE Metric')
