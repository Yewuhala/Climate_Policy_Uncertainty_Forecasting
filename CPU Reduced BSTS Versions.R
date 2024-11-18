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


# BSTS
ss <- AddLocalLinearTrend(list(), train_1$cpu_index)
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

# BSTS - Default Expected Model Size
ss <- AddLocalLinearTrend(list(), train_1$cpu_index)
ss <- AddSeasonal(ss, train_1$cpu_index, nseasons = 12)
bsts_1 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_1$cpu_index, train_reg_reduced_1)) %>% rename('cpu_index' = V1))
bsts_1_pred <- predict(bsts_1, horizon = n, burn = SuggestBurn(.1, bsts_1), newdata = test_reg_reduced_1)
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, bsts_1_pred$mean, model = 'BSTS - Default Expected Model Size'))
predict_1 <- predict_1 %>% mutate('BSTS - Default Expected Model Size' = bsts_1_pred$mean)


# BSTS LTS
ss <- AddLocalLevel(list(), train_1$cpu_index)
ss <- AddLocalLinearTrend(ss, train_1$cpu_index)
ss <- AddSeasonal(ss, train_1$cpu_index, nseasons = 12)
bsts_1 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10,
               data = as.data.frame(cbind(train_1$cpu_index, train_reg_reduced_1)) %>% rename('cpu_index' = V1))
bsts_1_pred <- predict(bsts_1, horizon = n, burn = SuggestBurn(.1, bsts_1), newdata = test_reg_reduced_1)
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, bsts_1_pred$mean, model = 'BSTS LTS'))
predict_1 <- predict_1 %>% mutate('BSTS LTS' = bsts_1_pred$mean)

# BSTS LTS - Default Expected Model Size
ss <- AddLocalLevel(list(), train_1$cpu_index)
ss <- AddLocalLinearTrend(ss, train_1$cpu_index)
ss <- AddSeasonal(ss, train_1$cpu_index, nseasons = 12)
bsts_1 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_1$cpu_index, train_reg_reduced_1)) %>% rename('cpu_index' = V1))
bsts_1_pred <- predict(bsts_1, horizon = n, burn = SuggestBurn(.1, bsts_1), newdata = test_reg_reduced_1)
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, bsts_1_pred$mean, model = 'BSTS LTS - Default Expected Model Size'))
predict_1 <- predict_1 %>% mutate('BSTS LTS - Default Expected Model Size' = bsts_1_pred$mean)


# BSTS LT
ss <- AddLocalLevel(list(), train_1$cpu_index)
ss <- AddLocalLinearTrend(ss, train_1$cpu_index)
bsts_1 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10,
               data = as.data.frame(cbind(train_1$cpu_index, train_reg_reduced_1)) %>% rename('cpu_index' = V1))
bsts_1_pred <- predict(bsts_1, horizon = n, burn = SuggestBurn(.1, bsts_1), newdata = test_reg_reduced_1)
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, bsts_1_pred$mean, model = 'BSTS LT'))
predict_1 <- predict_1 %>% mutate('BSTS LT' = bsts_1_pred$mean)

# BSTS LT - Default Expected Model Size
ss <- AddLocalLevel(list(), train_1$cpu_index)
ss <- AddLocalLinearTrend(ss, train_1$cpu_index)
bsts_1 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_1$cpu_index, train_reg_reduced_1)) %>% rename('cpu_index' = V1))
bsts_1_pred <- predict(bsts_1, horizon = n, burn = SuggestBurn(.1, bsts_1), newdata = test_reg_reduced_1)
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, bsts_1_pred$mean, model = 'BSTS LT - Default Expected Model Size'))
predict_1 <- predict_1 %>% mutate('BSTS LT - Default Expected Model Size' = bsts_1_pred$mean)


# BSTS LS
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
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, bsts_1_pred$mean, model = 'BSTS LS'))
predict_1 <- predict_1 %>% mutate('BSTS LS' = bsts_1_pred$mean)

# BSTS LS - Default Expected Model Size
ss <- AddLocalLevel(list(), train_1$cpu_index)
ss <- AddSeasonal(ss, train_1$cpu_index, nseasons = 12)
bsts_1 <- bsts(cpu_index ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                data = as.data.frame(cbind(train_1$cpu_index, train_reg_reduced_1)) %>% rename('cpu_index' = V1))
bsts_1_pred <- predict(bsts_1, horizon = n, burn = SuggestBurn(.1, bsts_1), newdata = test_reg_reduced_1)
model_evaluate_1 <- rbind(model_evaluate_1, evaluate(test_1, bsts_1_pred$mean, model = 'BSTS LS - Default Expected Model Size'))
predict_1 <- predict_1 %>% mutate('BSTS LS - Default Expected Model Size' = bsts_1_pred$mean)


write.csv(model_evaluate_1, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Variants/Horizon 1.csv', row.names = FALSE)
write.csv(predict_1, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Variants/Forecast 1.csv', row.names = FALSE)



##################################################### Horizon 3 #####################################################

n = 3
set.seed(100)

train_3 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_3 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_3 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_3 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_3 <- tibble()  
predict_3 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_3) + 1:length(test_3)]))  


# BSTS
ss <- AddLocalLinearTrend(list(), train_3$cpu_index)
ss <- AddSeasonal(ss, train_3$cpu_index, nseasons = 12)
bsts_3 <- bsts(cpu_index ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10,
                data = as.data.frame(cbind(train_3$cpu_index, train_reg_reduced_3)) %>% rename('cpu_index' = V1))
bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), newdata = test_reg_reduced_3)
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_pred$mean, model = 'BSTS'))
predict_3 <- predict_3 %>% mutate('BSTS' = bsts_3_pred$mean)

# BSTS - Default Expected Model Size
ss <- AddLocalLinearTrend(list(), train_3$cpu_index)
ss <- AddSeasonal(ss, train_3$cpu_index, nseasons = 12)
bsts_3 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_3$cpu_index, train_reg_reduced_3)) %>% rename('cpu_index' = V1))
bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), newdata = test_reg_reduced_3)
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_pred$mean, model = 'BSTS - Default Expected Model Size'))
predict_3 <- predict_3 %>% mutate('BSTS - Default Expected Model Size' = bsts_3_pred$mean)


# BSTS LTS
ss <- AddLocalLevel(list(), train_3$cpu_index)
ss <- AddLocalLinearTrend(ss, train_3$cpu_index)
ss <- AddSeasonal(ss, train_3$cpu_index, nseasons = 12)
bsts_3 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10,
               data = as.data.frame(cbind(train_3$cpu_index, train_reg_reduced_3)) %>% rename('cpu_index' = V1))
bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), newdata = test_reg_reduced_3)
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_pred$mean, model = 'BSTS LTS'))
predict_3 <- predict_3 %>% mutate('BSTS LTS' = bsts_3_pred$mean)

# BSTS LTS - Default Expected Model Size
ss <- AddLocalLevel(list(), train_3$cpu_index)
ss <- AddLocalLinearTrend(ss, train_3$cpu_index)
ss <- AddSeasonal(ss, train_3$cpu_index, nseasons = 12)
bsts_3 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_3$cpu_index, train_reg_reduced_3)) %>% rename('cpu_index' = V1))
bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), newdata = test_reg_reduced_3)
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_pred$mean, model = 'BSTS LTS - Default Expected Model Size'))
predict_3 <- predict_3 %>% mutate('BSTS LTS - Default Expected Model Size' = bsts_3_pred$mean)


# BSTS LT
ss <- AddLocalLevel(list(), train_3$cpu_index)
ss <- AddLocalLinearTrend(ss, train_3$cpu_index)
bsts_3 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10,
               data = as.data.frame(cbind(train_3$cpu_index, train_reg_reduced_3)) %>% rename('cpu_index' = V1))
bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), newdata = test_reg_reduced_3)
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_pred$mean, model = 'BSTS LT'))
predict_3 <- predict_3 %>% mutate('BSTS LT' = bsts_3_pred$mean)

# BSTS LT - Default Expected Model Size
ss <- AddLocalLevel(list(), train_3$cpu_index)
ss <- AddLocalLinearTrend(ss, train_3$cpu_index)
bsts_3 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_3$cpu_index, train_reg_reduced_3)) %>% rename('cpu_index' = V1))
bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), newdata = test_reg_reduced_3)
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_pred$mean, model = 'BSTS LT - Default Expected Model Size'))
predict_3 <- predict_3 %>% mutate('BSTS LT - Default Expected Model Size' = bsts_3_pred$mean)


# BSTS LS
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
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_pred$mean, model = 'BSTS LS'))
predict_3 <- predict_3 %>% mutate('BSTS LS' = bsts_3_pred$mean)

# BSTS LS - Default Expected Model Size
ss <- AddLocalLevel(list(), train_3$cpu_index)
ss <- AddSeasonal(ss, train_3$cpu_index, nseasons = 12)
bsts_3 <- bsts(cpu_index ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                data = as.data.frame(cbind(train_3$cpu_index, train_reg_reduced_3)) %>% rename('cpu_index' = V1))
bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), newdata = test_reg_reduced_3)
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_pred$mean, model = 'BSTS LS - Default Expected Model Size'))
predict_3 <- predict_3 %>% mutate('BSTS LS - Default Expected Model Size' = bsts_3_pred$mean)


write.csv(model_evaluate_3, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Variants/Horizon 3.csv', row.names = FALSE)
write.csv(predict_3, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Variants/Forecast 3.csv', row.names = FALSE)


##################################################### Horizon 6 #####################################################

n = 6
set.seed(100)

train_6 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_6 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_6 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_6 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_6 <- tibble()  
predict_6 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_6) + 1:length(test_6)]))  


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

# BSTS - Default Expected Model Size
ss <- AddLocalLinearTrend(list(), train_6$cpu_index)
ss <- AddSeasonal(ss, train_6$cpu_index, nseasons = 12)
bsts_6 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_6$cpu_index, train_reg_reduced_6)) %>% rename('cpu_index' = V1))
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_pred$mean, model = 'BSTS - Default Expected Model Size'))
predict_6 <- predict_6 %>% mutate('BSTS - Default Expected Model Size' = bsts_6_pred$mean)


# BSTS LTS
ss <- AddLocalLevel(list(), train_6$cpu_index)
ss <- AddLocalLinearTrend(ss, train_6$cpu_index)
ss <- AddSeasonal(ss, train_6$cpu_index, nseasons = 12)
bsts_6 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10,
               data = as.data.frame(cbind(train_6$cpu_index, train_reg_reduced_6)) %>% rename('cpu_index' = V1))
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_pred$mean, model = 'BSTS LTS'))
predict_6 <- predict_6 %>% mutate('BSTS LTS' = bsts_6_pred$mean)

# BSTS LTS - Default Expected Model Size
ss <- AddLocalLevel(list(), train_6$cpu_index)
ss <- AddLocalLinearTrend(ss, train_6$cpu_index)
ss <- AddSeasonal(ss, train_6$cpu_index, nseasons = 12)
bsts_6 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_6$cpu_index, train_reg_reduced_6)) %>% rename('cpu_index' = V1))
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_pred$mean, model = 'BSTS LTS - Default Expected Model Size'))
predict_6 <- predict_6 %>% mutate('BSTS LTS - Default Expected Model Size' = bsts_6_pred$mean)


# BSTS LT
ss <- AddLocalLevel(list(), train_6$cpu_index)
ss <- AddLocalLinearTrend(ss, train_6$cpu_index)
bsts_6 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10,
               data = as.data.frame(cbind(train_6$cpu_index, train_reg_reduced_6)) %>% rename('cpu_index' = V1))
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_pred$mean, model = 'BSTS LT'))
predict_6 <- predict_6 %>% mutate('BSTS LT' = bsts_6_pred$mean)

# BSTS LT - Default Expected Model Size
ss <- AddLocalLevel(list(), train_6$cpu_index)
ss <- AddLocalLinearTrend(ss, train_6$cpu_index)
bsts_6 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_6$cpu_index, train_reg_reduced_6)) %>% rename('cpu_index' = V1))
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_pred$mean, model = 'BSTS LT - Default Expected Model Size'))
predict_6 <- predict_6 %>% mutate('BSTS LT - Default Expected Model Size' = bsts_6_pred$mean)


# BSTS LS
ss <- AddLocalLevel(list(), train_6$cpu_index)
ss <- AddSeasonal(ss, train_6$cpu_index, nseasons = 12)
bsts_6 <- bsts(cpu_index ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10,
                data = as.data.frame(cbind(train_6$cpu_index, train_reg_reduced_6)) %>% rename('cpu_index' = V1))
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_pred$mean, model = 'BSTS LS'))
predict_6 <- predict_6 %>% mutate('BSTS LS' = bsts_6_pred$mean)

# BSTS LS - Default Expected Model Size
ss <- AddLocalLevel(list(), train_6$cpu_index)
ss <- AddSeasonal(ss, train_6$cpu_index, nseasons = 12)
bsts_6 <- bsts(cpu_index ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                data = as.data.frame(cbind(train_6$cpu_index, train_reg_reduced_6)) %>% rename('cpu_index' = V1))
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_pred$mean, model = 'BSTS LS - Default Expected Model Size'))
predict_6 <- predict_6 %>% mutate('BSTS LS - Default Expected Model Size' = bsts_6_pred$mean)


write.csv(model_evaluate_6, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Variants/Horizon 6.csv', row.names = FALSE)
write.csv(predict_6, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Variants/Forecast 6.csv', row.names = FALSE)



##################################################### Horizon 12 #####################################################

n = 12
set.seed(100)

train_12 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_12 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_12 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_12 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_12 <- tibble()  
predict_12 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_12) + 1:length(test_12)]))  


# BSTS
ss <- AddLocalLinearTrend(list(), train_12$cpu_index)
ss <- AddSeasonal(ss, train_12$cpu_index, nseasons = 12)
bsts_12 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10,
               data = as.data.frame(cbind(train_12$cpu_index, train_reg_reduced_12)) %>% rename('cpu_index' = V1))
bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), newdata = test_reg_reduced_12)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_pred$mean, model = 'BSTS'))
predict_12 <- predict_12 %>% mutate('BSTS' = bsts_12_pred$mean)

# BSTS - Default Expected Model Size
ss <- AddLocalLinearTrend(list(), train_12$cpu_index)
ss <- AddSeasonal(ss, train_12$cpu_index, nseasons = 12)
bsts_12 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_12$cpu_index, train_reg_reduced_12)) %>% rename('cpu_index' = V1))
bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), newdata = test_reg_reduced_12)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_pred$mean, model = 'BSTS - Default Expected Model Size'))
predict_12 <- predict_12 %>% mutate('BSTS - Default Expected Model Size' = bsts_12_pred$mean)


# BSTS LTS
ss <- AddLocalLevel(list(), train_12$cpu_index)
ss <- AddLocalLinearTrend(ss, train_12$cpu_index)
ss <- AddSeasonal(ss, train_12$cpu_index, nseasons = 12)
bsts_12 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10,
               data = as.data.frame(cbind(train_12$cpu_index, train_reg_reduced_12)) %>% rename('cpu_index' = V1))
bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), newdata = test_reg_reduced_12)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_pred$mean, model = 'BSTS LTS'))
predict_12 <- predict_12 %>% mutate('BSTS LTS' = bsts_12_pred$mean)

# BSTS LTS - Default Expected Model Size
ss <- AddLocalLevel(list(), train_12$cpu_index)
ss <- AddLocalLinearTrend(ss, train_12$cpu_index)
ss <- AddSeasonal(ss, train_12$cpu_index, nseasons = 12)
bsts_12 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_12$cpu_index, train_reg_reduced_12)) %>% rename('cpu_index' = V1))
bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), newdata = test_reg_reduced_12)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_pred$mean, model = 'BSTS LTS - Default Expected Model Size'))
predict_12 <- predict_12 %>% mutate('BSTS LTS - Default Expected Model Size' = bsts_12_pred$mean)


# BSTS LT
ss <- AddLocalLevel(list(), train_12$cpu_index)
ss <- AddLocalLinearTrend(ss, train_12$cpu_index)
bsts_12 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10,
               data = as.data.frame(cbind(train_12$cpu_index, train_reg_reduced_12)) %>% rename('cpu_index' = V1))
bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), newdata = test_reg_reduced_12)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_pred$mean, model = 'BSTS LT'))
predict_12 <- predict_12 %>% mutate('BSTS LT' = bsts_12_pred$mean)

# BSTS LT - Default Expected Model Size
ss <- AddLocalLevel(list(), train_12$cpu_index)
ss <- AddLocalLinearTrend(ss, train_12$cpu_index)
bsts_12 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_12$cpu_index, train_reg_reduced_12)) %>% rename('cpu_index' = V1))
bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), newdata = test_reg_reduced_12)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_pred$mean, model = 'BSTS LT - Default Expected Model Size'))
predict_12 <- predict_12 %>% mutate('BSTS LT - Default Expected Model Size' = bsts_12_pred$mean)


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

# BSTS LS - Default Expected Model Size
ss <- AddLocalLevel(list(), train_12$cpu_index)
ss <- AddSeasonal(ss, train_12$cpu_index, nseasons = 12)
bsts_12 <- bsts(cpu_index ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                data = as.data.frame(cbind(train_12$cpu_index, train_reg_reduced_12)) %>% rename('cpu_index' = V1))
bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), newdata = test_reg_reduced_12)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_pred$mean, model = 'BSTS LS - Default Expected Model Size'))
predict_12 <- predict_12 %>% mutate('BSTS LS - Default Expected Model Size' = bsts_12_pred$mean)


write.csv(model_evaluate_12, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Variants/Horizon 12.csv', row.names = FALSE)
write.csv(predict_12, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Variants/Forecast 12.csv', row.names = FALSE)



##################################################### Horizon 24 #####################################################

n = 24
set.seed(100)

train_24 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_24 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_24 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_24 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_24 <- tibble()  
predict_24 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_24) + 1:length(test_24)]))  


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

# BSTS - Default Expected Model Size
ss <- AddLocalLinearTrend(list(), train_24$cpu_index)
ss <- AddSeasonal(ss, train_24$cpu_index, nseasons = 12)
bsts_24 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_24$cpu_index, train_reg_reduced_24)) %>% rename('cpu_index' = V1))
bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), newdata = test_reg_reduced_24)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_pred$mean, model = 'BSTS - Default Expected Model Size'))
predict_24 <- predict_24 %>% mutate('BSTS - Default Expected Model Size' = bsts_24_pred$mean)


# BSTS LTS
ss <- AddLocalLevel(list(), train_24$cpu_index)
ss <- AddLocalLinearTrend(ss, train_24$cpu_index)
ss <- AddSeasonal(ss, train_24$cpu_index, nseasons = 12)
bsts_24 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10,
               data = as.data.frame(cbind(train_24$cpu_index, train_reg_reduced_24)) %>% rename('cpu_index' = V1))
bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), newdata = test_reg_reduced_24)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_pred$mean, model = 'BSTS LTS'))
predict_24 <- predict_24 %>% mutate('BSTS LTS' = bsts_24_pred$mean)

# BSTS LTS - Default Expected Model Size
ss <- AddLocalLevel(list(), train_24$cpu_index)
ss <- AddLocalLinearTrend(ss, train_24$cpu_index)
ss <- AddSeasonal(ss, train_24$cpu_index, nseasons = 12)
bsts_24 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_24$cpu_index, train_reg_reduced_24)) %>% rename('cpu_index' = V1))
bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), newdata = test_reg_reduced_24)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_pred$mean, model = 'BSTS LTS - Default Expected Model Size'))
predict_24 <- predict_24 %>% mutate('BSTS LTS - Default Expected Model Size' = bsts_24_pred$mean)


# BSTS LT
ss <- AddLocalLevel(list(), train_24$cpu_index)
ss <- AddSeasonal(ss, train_24$cpu_index, nseasons = 12)
bsts_24 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               expected.model.size = 10,
               data = as.data.frame(cbind(train_24$cpu_index, train_reg_reduced_24)) %>% rename('cpu_index' = V1))
bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), newdata = test_reg_reduced_24)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_pred$mean, model = 'BSTS LT'))
predict_24 <- predict_24 %>% mutate('BSTS LT' = bsts_24_pred$mean)

# BSTS LT - Default Expected Model Size
ss <- AddLocalLevel(list(), train_24$cpu_index)
ss <- AddLocalLinearTrend(ss, train_24$cpu_index)
bsts_24 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_24$cpu_index, train_reg_reduced_24)) %>% rename('cpu_index' = V1))
bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), newdata = test_reg_reduced_24)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_pred$mean, model = 'BSTS LT - Default Expected Model Size'))
predict_24 <- predict_24 %>% mutate('BSTS LT - Default Expected Model Size' = bsts_24_pred$mean)


# BSTS LS
ss <- AddLocalLevel(list(), train_24$cpu_index)
ss <- AddSeasonal(ss, train_24$cpu_index, nseasons = 12)
bsts_24 <- bsts(cpu_index ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 10,
                data = as.data.frame(cbind(train_24$cpu_index, train_reg_reduced_24)) %>% rename('cpu_index' = V1))
bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), newdata = test_reg_reduced_24)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_pred$mean, model = 'BSTS LS'))
predict_24 <- predict_24 %>% mutate('BSTS LS' = bsts_24_pred$mean)

# BSTS LS - Default Expected Model Size
ss <- AddLocalLevel(list(), train_24$cpu_index)
ss <- AddSeasonal(ss, train_24$cpu_index, nseasons = 12)
bsts_24 <- bsts(cpu_index ~ ., 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                data = as.data.frame(cbind(train_24$cpu_index, train_reg_reduced_24)) %>% rename('cpu_index' = V1))
bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), newdata = test_reg_reduced_24)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_pred$mean, model = 'BSTS LS - Default Expected Model Size'))
predict_24 <- predict_24 %>% mutate('BSTS LS - Default Expected Model Size' = bsts_24_pred$mean)


write.csv(model_evaluate_24, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Variants/Horizon 24.csv', row.names = FALSE)
write.csv(predict_24, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Results - BSTS Variants/Forecast 24.csv', row.names = FALSE)




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
