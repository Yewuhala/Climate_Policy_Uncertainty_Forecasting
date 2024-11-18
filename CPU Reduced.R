library(ggplot2)
library(readr)
library(bsts)
library(Metrics)
library(dplyr)
library(forecast)
library(fpp3)
library(RTransferEntropy)
library(readxl)
library(patchwork)


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


##################################################### Horizon 1 #####################################################

n = 1
set.seed(100)

train_1 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_1 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_1 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_1 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_1 <- tibble()  
predict_1 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_1) + 1:length(test_1)]))  


# ARFIMA
arfima_1 <- arfima(train_1$cpu_index, xreg = train_reg_reduced_1)
arfima_1_pred <- forecast(arfima_1, h = n, xreg = test_reg_reduced_1)
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, arfima_1_pred$mean, model = 'ARFIMA'))
predict_1 <- predict_1 %>% mutate('ARFIMA' = arfima_1_pred$mean)


# ARIMA
arima_1 <- auto.arima(train_1$cpu_index, xreg = train_reg_reduced_1)
arima_1_pred <- forecast(arima_1, h = n, xreg = test_reg_reduced_1)
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, arima_1_pred$mean,  model = 'ARIMA'))
predict_1 <- predict_1 %>% mutate('ARIMA' = arima_1_pred$mean)


# ARNN
arnn_1 <- nnetar(train_1$cpu_index, xreg = train_reg_reduced_1, MaxNWts = 3000)
arnn_1_pred <- forecast(arnn_1, h = n, xreg = test_reg_reduced_1)
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, arnn_1_pred$mean,  model = 'ARNN'))
predict_1 <- predict_1 %>% mutate('ARNN' = arnn_1_pred$mean)


# BSTS
ss <- AddLocalLevel(list(), train_1$cpu_index)
ss <- AddSeasonal(ss, train_1$cpu_index, nseasons = 12)
bsts_1 <- bsts(cpu_index ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10,
                data = as.data.frame(cbind(train_1$cpu_index, train_reg_reduced_1)) %>% rename('cpu_index' = V1))
bsts_1_pred <- predict(bsts_1, horizon = n, burn = SuggestBurn(.1, bsts_1), newdata = test_reg_reduced_1)
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, bsts_1_pred$mean, model = 'BSTS'))
predict_1 <- predict_1 %>% mutate('BSTS' = bsts_1_pred$mean)


# BSTS
# ss <- AddLocalLinearTrend(list(), train_1$cpu_index)
# ss <- AddSeasonal(ss, train_1$cpu_index, nseasons = 12)
# bsts_1 <- bsts(cpu_index ~ ., 
#                 state.specification = ss,
#                 niter = 1000,
#                 ping = 100,
#                 seed = 100,
#                 expected.model.size = 10,
#                 data = as.data.frame(cbind(train_1$cpu_index, train_reg_reduced_1)) %>% rename('cpu_index' = V1))
# bsts_1_pred <- predict(bsts_1, horizon = n, burn = SuggestBurn(.1, bsts_1), newdata = test_reg_reduced_1)
# model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, bsts_1_pred$mean, model = 'BSTS'))
# predict_1 <- predict_1 %>% mutate('BSTS' = bsts_1_pred$mean)


write.csv(model_evaluate_1, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced/Horizon 1.csv', row.names = FALSE)
write.csv(predict_1, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced/Forecast 1.csv', row.names = FALSE)



##################################################### Horizon 3 #####################################################

n = 3
set.seed(100)

train_3 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_3 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_3 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_3 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_3 <- tibble()  
predict_3 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_3) + 1:length(test_3)]))  


# ARFIMA
arfima_3 <- arfima(train_3$cpu_index, xreg = train_reg_reduced_3)
arfima_3_pred <- forecast(arfima_3, h = n, xreg = test_reg_reduced_3)
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, arfima_3_pred$mean, model = 'ARFIMA'))
predict_3 <- predict_3 %>% mutate('ARFIMA' = arfima_3_pred$mean)


# ARIMA
arima_3 <- auto.arima(train_3$cpu_index, xreg = train_reg_reduced_3)
arima_3_pred <- forecast(arima_3, h = n, xreg = test_reg_reduced_3)
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, arima_3_pred$mean,  model = 'ARIMA'))
predict_3 <- predict_3 %>% mutate('ARIMA' = arima_3_pred$mean)


# ARNN
arnn_3 <- nnetar(train_3$cpu_index, xreg = train_reg_reduced_3, MaxNWts = 3000)
arnn_3_pred <- forecast(arnn_3, h = n, xreg = test_reg_reduced_3)
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, arnn_3_pred$mean,  model = 'ARNN'))
predict_3 <- predict_3 %>% mutate('ARNN' = arnn_3_pred$mean)

# BSTS
ss <- AddLocalLevel(list(), train_3$cpu_index)
ss <- AddSeasonal(ss, train_3$cpu_index, nseasons = 12)
bsts_3 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10,
               data = as.data.frame(cbind(train_3$cpu_index, train_reg_reduced_3)) %>% rename('cpu_index' = V1))
bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), newdata = test_reg_reduced_3)


# BSTS
# ss <- AddLocalLinearTrend(list(), train_3$cpu_index)
# ss <- AddSeasonal(ss, train_3$cpu_index, nseasons = 12)
# bsts_3 <- bsts(cpu_index ~ ., 
#                 state.specification = ss,
#                 niter = 1000,
#                 ping = 100,
#                 seed = 100,
#                 expected.model.size = 10,
#                 data = as.data.frame(cbind(train_3$cpu_index, train_reg_reduced_3)) %>% rename('cpu_index' = V1))
# bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), newdata = test_reg_reduced_3)
# model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_pred$mean, model = 'BSTS'))
# predict_3 <- predict_3 %>% mutate('BSTS' = bsts_3_pred$mean)


write.csv(model_evaluate_3, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced/Horizon 3.csv', row.names = FALSE)
write.csv(predict_3, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced/Forecast 3.csv', row.names = FALSE)



##################################################### Horizon 6 #####################################################

n = 6
set.seed(100)

train_6 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_6 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_6 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_6 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_6 <- tibble()  
predict_6 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_6) + 1:length(test_6)]))  


# ARFIMA
arfima_6 <- arfima(train_6$cpu_index, xreg = train_reg_reduced_6)
arfima_6_pred <- forecast(arfima_6, h = n, xreg = test_reg_reduced_6)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, arfima_6_pred$mean, model = 'ARFIMA'))
predict_6 <- predict_6 %>% mutate('ARFIMA' = arfima_6_pred$mean)


# ARIMA
arima_6 <- auto.arima(train_6$cpu_index, xreg = train_reg_reduced_6)
arima_6_pred <- forecast(arima_6, h = n, xreg = test_reg_reduced_6)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, arima_6_pred$mean,  model = 'ARIMA'))
predict_6 <- predict_6 %>% mutate('ARIMA' = arima_6_pred$mean)


# ARNN
arnn_6 <- nnetar(train_6$cpu_index, xreg = train_reg_reduced_6, MaxNWts = 3000)
arnn_6_pred <- forecast(arnn_6, h = n, xreg = test_reg_reduced_6)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, arnn_6_pred$mean,  model = 'ARNN'))
predict_6 <- predict_6 %>% mutate('ARNN' = arnn_6_pred$mean)

# BSTS LS
# ss <- AddLocalLevel(list(), train_6$cpu_index)
# ss <- AddSeasonal(ss, train_6$cpu_index, nseasons = 12)
# bsts_6 <- bsts(cpu_index ~ ., 
#                state.specification = ss,
#                niter = 1000,
#                ping = 100,
#                seed = 100,
#                expected.model.size = 10,
#                data = as.data.frame(cbind(train_6$cpu_index, train_reg_reduced_6)) %>% rename('cpu_index' = V1))
# bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)
# model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_pred$mean, model = 'BSTS LS'))
# predict_6 <- predict_6 %>% mutate('BSTS LS' = bsts_6_pred$mean)


# BSTS
ss <- AddLocalLinearTrend(list(), train_6$cpu_index)
ss <- AddSeasonal(ss, train_6$cpu_index, nseasons = 12)
bsts_6 <- bsts(cpu_index ~ .,
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10,
               data = as.data.frame(cbind(train_6$cpu_index, train_reg_reduced_6)) %>% rename('cpu_index' = V1))
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_pred$mean, model = 'BSTS'))
predict_6 <- predict_6 %>% mutate('BSTS' = bsts_6_pred$mean)


write.csv(model_evaluate_6, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced/Horizon 6.csv', row.names = FALSE)
write.csv(predict_6, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced/Forecast 6.csv', row.names = FALSE)



##################################################### Horizon 12 #####################################################

n = 12
set.seed(100)

train_12 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_12 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_12 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_12 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_12 <- tibble()  
predict_12 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_12) + 1:length(test_12)]))  


# ARFIMA
arfima_12 <- arfima(train_12$cpu_index, xreg = train_reg_reduced_12)
arfima_12_pred <- forecast(arfima_12, h = n, xreg = test_reg_reduced_12)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, arfima_12_pred$mean, model = 'ARFIMA'))
predict_12 <- predict_12 %>% mutate('ARFIMA' = arfima_12_pred$mean)


# ARIMA
arima_12 <- auto.arima(train_12$cpu_index, xreg = train_reg_reduced_12)
arima_12_pred <- forecast(arima_12, h = n, xreg = test_reg_reduced_12)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, arima_12_pred$mean,  model = 'ARIMA'))
predict_12 <- predict_12 %>% mutate('ARIMA' = arima_12_pred$mean)


# ARNN
arnn_12 <- nnetar(train_12$cpu_index, xreg = train_reg_reduced_12, MaxNWts = 3000)
arnn_12_pred <- forecast(arnn_12, h = n, xreg = test_reg_reduced_12)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, arnn_12_pred$mean,  model = 'ARNN'))
predict_12 <- predict_12 %>% mutate('ARNN' = arnn_12_pred$mean)

# BSTS LS
ss <- AddLocalLevel(list(), train_12$cpu_index)
ss <- AddSeasonal(ss, train_12$cpu_index, nseasons = 12)
bsts_12 <- bsts(cpu_index ~ .,
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10,
                data = as.data.frame(cbind(train_12$cpu_index, train_reg_reduced_12)) %>% rename('cpu_index' = V1))
bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), newdata = test_reg_reduced_12)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_pred$mean, model = 'BSTS LS'))
predict_12 <- predict_12 %>% mutate('BSTS LS' = bsts_12_pred$mean)


# BSTS
# ss <- AddLocalLinearTrend(list(), train_12$cpu_index)
# ss <- AddSeasonal(ss, train_12$cpu_index, nseasons = 12)
# bsts_12 <- bsts(cpu_index ~ ., 
#                state.specification = ss,
#                niter = 1000,
#                ping = 100,
#                seed = 100,
#                expected.model.size = 10,
#                data = as.data.frame(cbind(train_12$cpu_index, train_reg_reduced_12)) %>% rename('cpu_index' = V1))
# bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), newdata = test_reg_reduced_12)
# model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_pred$mean, model = 'BSTS'))
# predict_12 <- predict_12 %>% mutate('BSTS' = bsts_12_pred$mean)


write.csv(model_evaluate_12, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced/Horizon 12.csv', row.names = FALSE)
write.csv(predict_12, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced/Forecast 12.csv', row.names = FALSE)



##################################################### Horizon 24 #####################################################

n = 24
set.seed(100)

train_24 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_24 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_24 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_24 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_24 <- tibble()  
predict_24 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_24) + 1:length(test_24)]))  


# ARFIMA
arfima_24 <- arfima(train_24$cpu_index, xreg = train_reg_reduced_24)
arfima_24_pred <- forecast(arfima_24, h = n, xreg = test_reg_reduced_24)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, arfima_24_pred$mean, model = 'ARFIMA'))
predict_24 <- predict_24 %>% mutate('ARFIMA' = arfima_24_pred$mean)


# ARIMA
arima_24 <- auto.arima(train_24$cpu_index, xreg = train_reg_reduced_24)
arima_24_pred <- forecast(arima_24, h = n, xreg = test_reg_reduced_24)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, arima_24_pred$mean,  model = 'ARIMA'))
predict_24 <- predict_24 %>% mutate('ARIMA' = arima_24_pred$mean)


# ARNN
arnn_24 <- nnetar(train_24$cpu_index, xreg = train_reg_reduced_24, MaxNWts = 3000)
arnn_24_pred <- forecast(arnn_24, h = n, xreg = test_reg_reduced_24)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, arnn_24_pred$mean,  model = 'ARNN'))
predict_24 <- predict_24 %>% mutate('ARNN' = arnn_24_pred$mean)

# BSTS LS
# ss <- AddLocalLevel(list(), train_24$cpu_index)
# ss <- AddSeasonal(ss, train_24$cpu_index, nseasons = 12)
# bsts_24 <- bsts(cpu_index ~ ., 
#                 state.specification = ss,
#                 niter = 1000,
#                 ping = 100,
#                 seed = 100,
#                 expected.model.size = 10,
#                 data = as.data.frame(cbind(train_24$cpu_index, train_reg_reduced_24)) %>% rename('cpu_index' = V1))
# bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), newdata = test_reg_reduced_24)
# model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_pred$mean, model = 'BSTS LS'))
# predict_24 <- predict_24 %>% mutate('BSTS LS' = bsts_24_pred$mean)


# BSTS
ss <- AddLocalLinearTrend(list(), train_24$cpu_index)
ss <- AddSeasonal(ss, train_24$cpu_index, nseasons = 12)
bsts_24 <- bsts(cpu_index ~ .,
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10,
               data = as.data.frame(cbind(train_24$cpu_index, train_reg_reduced_24)) %>% rename('cpu_index' = V1))
bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), newdata = test_reg_reduced_24)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_pred$mean, model = 'BSTS'))
predict_24 <- predict_24 %>% mutate('BSTS' = bsts_24_pred$mean)


write.csv(model_evaluate_24, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced/Horizon 24.csv', row.names = FALSE)
write.csv(predict_24, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced/Forecast 24.csv', row.names = FALSE)



##################################################### Horizon 36 #####################################################

n = 36
set.seed(100)

train_36 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_36 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_36 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_36 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_36 <- tibble()  
predict_36 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_36) + 1:length(test_36)]))  


# ARFIMA
arfima_36 <- arfima(train_36$cpu_index, xreg = train_reg_reduced_36)
arfima_36_pred <- forecast(arfima_36, h = n, xreg = test_reg_reduced_36)
model_evaluate_36 <- rbind(model_evaluate_36, evaluate(test_36, arfima_36_pred$mean, model = 'ARFIMA'))
predict_36 <- predict_36 %>% mutate('ARFIMA' = arfima_36_pred$mean)


# ARIMA
arima_36 <- auto.arima(train_36$cpu_index, xreg = train_reg_reduced_36)
arima_36_pred <- forecast(arima_36, h = n, xreg = test_reg_reduced_36)
model_evaluate_36 <- rbind(model_evaluate_36, evaluate(test_36, arima_36_pred$mean,  model = 'ARIMA'))
predict_36 <- predict_36 %>% mutate('ARIMA' = arima_36_pred$mean)


# ARNN
arnn_36 <- nnetar(train_36$cpu_index, xreg = train_reg_reduced_36, MaxNWts = 3000)
arnn_36_pred <- forecast(arnn_36, h = n, xreg = test_reg_reduced_36)
model_evaluate_36 <- rbind(model_evaluate_36, evaluate(test_36, arnn_36_pred$mean,  model = 'ARNN'))
predict_36 <- predict_36 %>% mutate('ARNN' = arnn_36_pred$mean)

# BSTS LS
ss <- AddLocalLevel(list(), train_36$cpu_index)
ss <- AddSeasonal(ss, train_36$cpu_index, nseasons = 12)
bsts_36 <- bsts(cpu_index ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10,
                data = as.data.frame(cbind(train_36$cpu_index, train_reg_reduced_36)) %>% rename('cpu_index' = V1))
bsts_36_pred <- predict(bsts_36, horizon = n, burn = SuggestBurn(.1, bsts_36), newdata = test_reg_reduced_36)
model_evaluate_36 <- rbind(model_evaluate_36, evaluate(test_36, bsts_36_pred$mean, model = 'BSTS LS'))
predict_36 <- predict_36 %>% mutate('BSTS LS' = bsts_36_pred$mean)


# BSTS - TS
ss <- AddLocalLinearTrend(list(), train_36$cpu_index)
ss <- AddSeasonal(ss, train_36$cpu_index, nseasons = 12)
bsts_36 <- bsts(cpu_index ~ .,
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10,
                data = as.data.frame(cbind(train_36$cpu_index, train_reg_reduced_36)) %>% rename('cpu_index' = V1))
bsts_36_pred <- predict(bsts_36, horizon = n, burn = SuggestBurn(.1, bsts_36), newdata = test_reg_reduced_36)
model_evaluate_36 <- rbind(model_evaluate_36, evaluate(test_36, bsts_36_pred$mean, model = 'BSTS - TS'))
predict_36 <- predict_36 %>% mutate('BSTS - TS' = bsts_36_pred$mean)


write.csv(model_evaluate_36, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced/Horizon 36.csv', row.names = FALSE)
write.csv(predict_36, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced/Forecast 36.csv', row.names = FALSE)



##################################################### Horizon 48 #####################################################

n = 48
set.seed(100)

train_48 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_48 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_48 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_48 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_48 <- tibble()  
predict_48 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_48) + 1:length(test_48)]))  


# ARFIMA
arfima_48 <- arfima(train_48$cpu_index, xreg = train_reg_reduced_48)
arfima_48_pred <- forecast(arfima_48, h = n, xreg = test_reg_reduced_48)
model_evaluate_48 <- rbind(model_evaluate_48, evaluate(test_48, arfima_48_pred$mean, model = 'ARFIMA'))
predict_48 <- predict_48 %>% mutate('ARFIMA' = arfima_48_pred$mean)


# ARIMA
arima_48 <- auto.arima(train_48$cpu_index, xreg = train_reg_reduced_48)
arima_48_pred <- forecast(arima_48, h = n, xreg = test_reg_reduced_48)
model_evaluate_48 <- rbind(model_evaluate_48, evaluate(test_48, arima_48_pred$mean,  model = 'ARIMA'))
predict_48 <- predict_48 %>% mutate('ARIMA' = arima_48_pred$mean)


# ARNN
arnn_48 <- nnetar(train_48$cpu_index, xreg = train_reg_reduced_48, MaxNWts = 3000)
arnn_48_pred <- forecast(arnn_48, h = n, xreg = test_reg_reduced_48)
model_evaluate_48 <- rbind(model_evaluate_48, evaluate(test_48, arnn_48_pred$mean,  model = 'ARNN'))
predict_48 <- predict_48 %>% mutate('ARNN' = arnn_48_pred$mean)

# BSTS LS
ss <- AddLocalLevel(list(), train_48$cpu_index)
ss <- AddSeasonal(ss, train_48$cpu_index, nseasons = 12)
bsts_48 <- bsts(cpu_index ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10,
                data = as.data.frame(cbind(train_48$cpu_index, train_reg_reduced_48)) %>% rename('cpu_index' = V1))
bsts_48_pred <- predict(bsts_48, horizon = n, burn = SuggestBurn(.1, bsts_48), newdata = test_reg_reduced_48)
model_evaluate_48 <- rbind(model_evaluate_48, evaluate(test_48, bsts_48_pred$mean, model = 'BSTS LS'))
predict_48 <- predict_48 %>% mutate('BSTS LS' = bsts_48_pred$mean)


# BSTS - TS
ss <- AddLocalLinearTrend(list(), train_48$cpu_index)
ss <- AddSeasonal(ss, train_48$cpu_index, nseasons = 12)
bsts_48 <- bsts(cpu_index ~ .,
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10,
               data = as.data.frame(cbind(train_48$cpu_index, train_reg_reduced_48)) %>% rename('cpu_index' = V1))
bsts_48_pred <- predict(bsts_48, horizon = n, burn = SuggestBurn(.1, bsts_48), newdata = test_reg_reduced_48)
model_evaluate_48 <- rbind(model_evaluate_48, evaluate(test_48, bsts_48_pred$mean, model = 'BSTS - TS'))
predict_48 <- predict_48 %>% mutate('BSTS - TS' = bsts_48_pred$mean)


write.csv(model_evaluate_48, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced/Horizon 48.csv', row.names = FALSE)
write.csv(predict_48, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced/Forecast 48.csv', row.names = FALSE)




##################################################### Plots #####################################################

SUMMARY(train_24$cpu_index)
statistical_tests_summary(train_24$cpu_index)
ts_diagnostic_plots(train_24$cpu_index, start_date = '1987-04-01')


n = 24
df <- data.frame(y = train_24$cpu_index, date = seq(from = as.Date("1987-04-01"), to = as.Date("2021-06-01"), by = "month"))
g1 <- ggplot(data = df, aes(x = as.Date(date))) +
  geom_line(aes(y = y)) +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab('') +
  ylab('') +  
  ggtitle("Time Series Plot") +
  theme(plot.title = element_text(hjust = 0.5)) 


diff_train <-  diff(train_24$cpu_index, differences = ndiffs(train_24$cpu_index))
g2 <- ggAcf(diff_train) + geom_point(color = 'navy blue') + theme_bw() + ggtitle("ACF Plot") + theme(plot.title = element_text(hjust = 0.5)) 
g3 <- ggPacf(diff_train) + geom_point(color = 'navy blue') + theme_bw() + ggtitle("PACF Plot") + theme(plot.title = element_text(hjust = 0.5))

g1 + g2 + g3






n = 3
bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), newdata = test_reg_reduced_3, quantiles = c(0.1,0.9))
data_3 <- as_tibble(cbind.data.frame('LL' = as.numeric(bsts_3_pred$interval[1,]),
                                      'UL' = as.numeric(bsts_3_pred$interval[2,]),
                                      'Date' = cpu_data$Date[(length(cpu) - n + 1):length(cpu)],
                                      'Test' = test_3,
                                      'BSTS' = bsts_3_pred$mean,
                                      'ARNN' = arnn_3_pred$mean,
                                      'ARIMA' = arima_3_pred$mean,
                                      'ARFIMA' = arfima_3_pred$mean))

ggplot(data = data_3, aes(x = Date)) +
  geom_line(aes(y = Test, color = 'Test'), linewidth = 2) +
  geom_line(aes(y = BSTS, color = 'BSTS'), linewidth = 2) +
  geom_line(aes(y = ARIMA, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = ARFIMA, color = 'ARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARNN, color = 'ARNN'), linewidth = 2) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = 'grey', alpha = .5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m-%d", limits = c(ymd("2023-04-01"), ymd("2023-06-01"))) +
  ylab('CPU Index') +
  xlab('Date') +
  labs(color = 'Models') +
  ggtitle('CPU Index: 3 Month Holdout') +
  scale_color_manual(values = c(
    'BSTS' = 'forestgreen',
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
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6, quantiles = c(0.1,0.9))
data_6 <- as_tibble(cbind.data.frame('LL' = as.numeric(bsts_6_pred$interval[1,]),
                                      'UL' = as.numeric(bsts_6_pred$interval[2,]),
                                      'Date' = cpu_data$Date[(length(cpu) - n + 1):length(cpu)],
                                      'Test' = test_6,
                                      'BSTS' = bsts_6_pred$mean,
                                      'ARNN' = arnn_6_pred$mean,
                                      'ARIMA' = arima_6_pred$mean,
                                      'ARFIMA' = arfima_6_pred$mean))

ggplot(data = data_6, aes(x = Date)) +
  geom_line(aes(y = Test, color = 'Test'), linewidth = 2) +
  geom_line(aes(y = BSTS, color = 'BSTS'), linewidth = 2) +
  geom_line(aes(y = ARIMA, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = ARFIMA, color = 'ARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARNN, color = 'ARNN'), linewidth = 2) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = 'grey', alpha = .5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m-%d", limits = c(ymd("2023-01-01"), ymd("2023-06-01"))) +
  ylab('CPU Index') +
  xlab('Date') +
  labs(color = 'Models') +
  ggtitle('CPU Index: 6 Month Holdout') +
  scale_color_manual(values = c(
    'BSTS' = 'forestgreen',
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

arfima_12 <- arfima(train_12$cpu_index, xreg = train_reg_reduced_12)
arfima_12_pred <- forecast(arfima_12, h = n, xreg = test_reg_reduced_12)

arima_12 <- auto.arima(train_12$cpu_index)
arima_12_pred <- forecast(arima_12, h = n)

bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), newdata = test_reg_reduced_12, quantiles = c(0.1,0.9))
data_12 <- as_tibble(cbind.data.frame('LL' = as.numeric(bsts_12_pred$interval[1,]),
                                      'UL' = as.numeric(bsts_12_pred$interval[2,]),
                                      'Date' = cpu_data$Date[(length(cpu) - n + 1):length(cpu)],
                                      'Test' = test_12,
                                      'BSTS' = bsts_12_pred$mean,
                                      #'ARNN' = arnn_12_pred$mean,
                                      'ARIMA' = arima_12_pred$mean,
                                      'ARFIMA' = arfima_12_pred$mean))
max(data_12$UL - data_12$LL)


ggplot(data = data_12, aes(x = Date)) +
  geom_line(aes(y = Test, color = 'Test'), linewidth = 2) +
  geom_line(aes(y = BSTS, color = 'BSTS'), linewidth = 2) +
  geom_line(aes(y = ARIMA, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = ARFIMA, color = 'ARFIMA'), linewidth = 2) +
  #geom_line(aes(y = ARNN, color = 'ARNN'), linewidth = 2) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = 'grey', alpha = .5) +
  scale_x_date(date_breaks = "2 month", date_labels = "%Y-%m-%d", limits = c(ymd("2022-07-01"), ymd("2023-06-01"))) +
  ylab('CPU Index') +
  xlab('Date') +
  labs(color = 'Models') +
  ggtitle('CPU Index: 12 Month Holdout') +
  scale_color_manual(values = c(
    'BSTS' = 'forestgreen',
    'ARIMA' = 'orange',
    'ARFIMA' = 'dodgerblue3',
   # 'ARNN' = 'orchid', 
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

arfima_24 <- arfima(train_24$cpu_index, xreg = train_reg_reduced_24)
arfima_24_pred <- forecast(arfima_24, h = n, xreg = test_reg_reduced_24)

arima_24 <- auto.arima(train_24$cpu_index)
arima_24_pred <- forecast(arima_24, h = n)

bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), newdata = test_reg_reduced_24, quantiles = c(0.1,0.9))
data_24 <- as_tibble(cbind.data.frame('LL' = as.numeric(bsts_24_pred$interval[1,]),
                                     'UL' = as.numeric(bsts_24_pred$interval[2,]),
                                     'Date' = cpu_data$Date[(length(cpu) - n + 1):length(cpu)],
                                     'Test' = test_24,
                                     'BSTS' = bsts_24_pred$mean,
                                     #'ARNN' = arnn_24_pred$mean,
                                     'ARIMA' = arima_24_pred$mean,
                                     'ARFIMA' = arfima_24_pred$mean))
max(data_24$UL - data_24$LL)

ggplot(data = data_24, aes(x = Date)) +
  geom_line(aes(y = Test, color = 'Test'), linewidth = 2) +
  geom_line(aes(y = BSTS, color = 'BSTS'), linewidth = 2) +
  geom_line(aes(y = ARIMA, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = ARFIMA, color = 'ARFIMA'), linewidth = 2) +
 # geom_line(aes(y = ARNN, color = 'ARNN'), linewidth = 2) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = 'grey', alpha = .5) +
  scale_x_date(date_breaks = "4 month", date_labels = "%Y-%m-%d", limits = c(ymd("2021-07-01"), ymd("2023-06-01"))) +
  ylab('CPU Index') +
  xlab('Date') +
  labs(color = 'Models') +
  ggtitle('CPU Index: 24 Month Holdout') +
  scale_color_manual(values = c(
    'BSTS' = 'forestgreen',
    'ARIMA' = 'orange',
    'ARFIMA' = 'dodgerblue3',
   # 'ARNN' = 'orchid', 
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

setwd('C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Variants')
err <- read_excel('Results Combined - RMSE.xlsx')
tsutils::nemenyi(err, plottype = "mcb", main = 'MCB Plot for RMSE Metric')

setwd('C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - Reduced')
err <- read_excel('Results Combined - RMSE.xlsx') 
err <- err[1:5, c(-5,-6,-9,-10)]
tsutils::nemenyi(err, plottype = "mcb", main = 'MCB Plot for RMSE Metric')

err_all <- read_excel('Results Combined - RMSE - All.xlsx')
err_all <- err_all[1:5, c(-5,-6,-9,-10,-17,-18,-21,-22)]
tsutils::nemenyi(err_all, plottype = "mcb", main = 'MCB Plot for RMSE Metric')

