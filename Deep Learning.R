library(forecast)
library(tidyverse)
library(Metrics)
library(ggplot2)
library(tsDyn)
library(forecastHybrid)
library(WaveletArima)
library(FinTS)
library(tseries)
library(rugarch)
library(bsts)
library(readxl)

setwd('C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Deep Learning')
data <- read_excel('CPU_Data_Reduced.xlsx')                
cpu_series <- ts(data$cpu_index)
    
cpu_1_NBEATS <- unlist(read_csv('NBEATS_1.csv')[2])      
cpu_1_NHiTS <- unlist(read_csv('NHiTS_1.csv')[2])      
cpu_1_TCN <- unlist(read_csv('TCN_1.csv')[2])      
cpu_1_Transformer <- unlist(read_csv('Transformer_1.csv')[2]) 
cpu_1_Dlinear <- unlist(read_csv('Dlinear_1.csv')[2]) 
cpu_1_Nlinear <- unlist(read_csv('Nlinear_1.csv')[2]) 

cpu_3_NBEATS <- unlist(read_csv('NBEATS_3.csv')[2])      
cpu_3_NHiTS <- unlist(read_csv('NHiTS_3.csv')[2])      
cpu_3_TCN <- unlist(read_csv('TCN_3.csv')[2])      
cpu_3_Transformer <- unlist(read_csv('Transformer_3.csv')[2]) 
cpu_3_Dlinear <- unlist(read_csv('Dlinear_3.csv')[2]) 
cpu_3_Nlinear <- unlist(read_csv('Nlinear_3.csv')[2])    

cpu_6_NBEATS <- unlist(read_csv('NBEATS_6.csv')[2])      
cpu_6_NHiTS <- unlist(read_csv('NHiTS_6.csv')[2])      
cpu_6_TCN <- unlist(read_csv('TCN_6.csv')[2])      
cpu_6_Transformer <- unlist(read_csv('Transformer_6.csv')[2]) 
cpu_6_Dlinear <- unlist(read_csv('Dlinear_6.csv')[2]) 
cpu_6_Nlinear <- unlist(read_csv('Nlinear_6.csv')[2])  

cpu_12_NBEATS <- unlist(read_csv('NBEATS_12.csv')[2])      
cpu_12_NHiTS <- unlist(read_csv('NHiTS_12.csv')[2])      
cpu_12_TCN <- unlist(read_csv('TCN_12.csv')[2])      
cpu_12_Transformer <- unlist(read_csv('Transformer_12.csv')[2]) 
cpu_12_Dlinear <- unlist(read_csv('Dlinear_12.csv')[2]) 
cpu_12_Nlinear <- unlist(read_csv('Nlinear_12.csv')[2])    

cpu_24_NBEATS <- unlist(read_csv('NBEATS_24.csv')[2])      
cpu_24_NHiTS <- unlist(read_csv('NHiTS_24.csv')[2])      
cpu_24_TCN <- unlist(read_csv('TCN_24.csv')[2])      
cpu_24_Transformer <- unlist(read_csv('Transformer_24.csv')[2]) 
cpu_24_Dlinear <- unlist(read_csv('Dlinear_24.csv')[2]) 
cpu_24_Nlinear <- unlist(read_csv('Nlinear_24.csv')[2])     



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


##################################################### cpuil 1 #####################################################

n = 1
test_cpu_1 <- subset(cpu_series, start = length(cpu_series) - n + 1)

model_evaluate_cpu_1 <- tibble()  
model_evaluate_cpu_1 <- rbind(model_evaluate_cpu_1, evaluate(test_cpu_1, cpu_1_NBEATS, model = 'NBEATS'))
model_evaluate_cpu_1 <- rbind(model_evaluate_cpu_1, evaluate(test_cpu_1, cpu_1_NHiTS, model = 'NHiTS'))
model_evaluate_cpu_1 <- rbind(model_evaluate_cpu_1, evaluate(test_cpu_1, cpu_1_Transformer, model = 'Transformer'))
model_evaluate_cpu_1 <- rbind(model_evaluate_cpu_1, evaluate(test_cpu_1, cpu_1_TCN, model = 'TCN'))
model_evaluate_cpu_1 <- rbind(model_evaluate_cpu_1, evaluate(test_cpu_1, cpu_1_Dlinear, model = 'DLinear'))
model_evaluate_cpu_1 <- rbind(model_evaluate_cpu_1, evaluate(test_cpu_1, cpu_1_Nlinear, model = 'NLinear'))
write.csv(model_evaluate_cpu_1, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Deep Learning/CPU 1.csv', row.names = FALSE)



##################################################### cpuil 3 #####################################################

n = 3
test_cpu_3 <- subset(cpu_series, start = length(cpu_series) - n + 1)


model_evaluate_cpu_3 <- tibble()  
model_evaluate_cpu_3 <- rbind(model_evaluate_cpu_3, evaluate(test_cpu_3, cpu_3_NBEATS, model = 'NBEATS'))
model_evaluate_cpu_3 <- rbind(model_evaluate_cpu_3, evaluate(test_cpu_3, cpu_3_NHiTS, model = 'NHiTS'))
model_evaluate_cpu_3 <- rbind(model_evaluate_cpu_3, evaluate(test_cpu_3, cpu_3_Transformer, model = 'Transformer'))
model_evaluate_cpu_3 <- rbind(model_evaluate_cpu_3, evaluate(test_cpu_3, cpu_3_TCN, model = 'TCN'))
model_evaluate_cpu_3 <- rbind(model_evaluate_cpu_3, evaluate(test_cpu_3, cpu_3_Dlinear, model = 'DLinear'))
model_evaluate_cpu_3 <- rbind(model_evaluate_cpu_3, evaluate(test_cpu_3, cpu_3_Nlinear, model = 'NLinear'))
write.csv(model_evaluate_cpu_3, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Deep Learning/CPU 3.csv', row.names = FALSE)


##################################################### cpuil 6 #####################################################

n = 6
test_cpu_6 <- subset(cpu_series, start = length(cpu_series) - n + 1)


model_evaluate_cpu_6 <- tibble()  
model_evaluate_cpu_6 <- rbind(model_evaluate_cpu_6, evaluate(test_cpu_6, cpu_6_NBEATS, model = 'NBEATS'))
model_evaluate_cpu_6 <- rbind(model_evaluate_cpu_6, evaluate(test_cpu_6, cpu_6_NHiTS, model = 'NHiTS'))
model_evaluate_cpu_6 <- rbind(model_evaluate_cpu_6, evaluate(test_cpu_6, cpu_6_Transformer, model = 'Transformer'))
model_evaluate_cpu_6 <- rbind(model_evaluate_cpu_6, evaluate(test_cpu_6, cpu_6_TCN, model = 'TCN'))
model_evaluate_cpu_6 <- rbind(model_evaluate_cpu_6, evaluate(test_cpu_6, cpu_6_Dlinear, model = 'DLinear'))
model_evaluate_cpu_6 <- rbind(model_evaluate_cpu_6, evaluate(test_cpu_6, cpu_6_Nlinear, model = 'NLinear'))
write.csv(model_evaluate_cpu_6, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Deep Learning/CPU 6.csv', row.names = FALSE)


##################################################### cpuil 12 #####################################################

n = 12
test_cpu_12 <- subset(cpu_series, start = length(cpu_series) - n + 1)

model_evaluate_cpu_12 <- tibble()  
model_evaluate_cpu_12 <- rbind(model_evaluate_cpu_12, evaluate(test_cpu_12, cpu_12_NBEATS, model = 'NBEATS'))
model_evaluate_cpu_12 <- rbind(model_evaluate_cpu_12, evaluate(test_cpu_12, cpu_12_NHiTS, model = 'NHiTS'))
model_evaluate_cpu_12 <- rbind(model_evaluate_cpu_12, evaluate(test_cpu_12, cpu_12_Transformer, model = 'Transformer'))
model_evaluate_cpu_12 <- rbind(model_evaluate_cpu_12, evaluate(test_cpu_12, cpu_12_TCN, model = 'TCN'))
model_evaluate_cpu_12 <- rbind(model_evaluate_cpu_12, evaluate(test_cpu_12, cpu_12_Dlinear, model = 'DLinear'))
model_evaluate_cpu_12 <- rbind(model_evaluate_cpu_12, evaluate(test_cpu_12, cpu_12_Nlinear, model = 'NLinear'))
write.csv(model_evaluate_cpu_12, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Deep Learning/CPU 12.csv', row.names = FALSE)


##################################################### cpuil 24 #####################################################

n = 24
test_cpu_24 <- subset(cpu_series, start = length(cpu_series) - n + 1)


model_evaluate_cpu_24 <- tibble()  
model_evaluate_cpu_24 <- rbind(model_evaluate_cpu_24, evaluate(test_cpu_24, cpu_24_NBEATS, model = 'NBEATS'))
model_evaluate_cpu_24 <- rbind(model_evaluate_cpu_24, evaluate(test_cpu_24, cpu_24_NHiTS, model = 'NHiTS'))
model_evaluate_cpu_24 <- rbind(model_evaluate_cpu_24, evaluate(test_cpu_24, cpu_24_Transformer, model = 'Transformer'))
model_evaluate_cpu_24 <- rbind(model_evaluate_cpu_24, evaluate(test_cpu_24, cpu_24_TCN, model = 'TCN'))
model_evaluate_cpu_24 <- rbind(model_evaluate_cpu_24, evaluate(test_cpu_24, cpu_24_Dlinear, model = 'DLinear'))
model_evaluate_cpu_24 <- rbind(model_evaluate_cpu_24, evaluate(test_cpu_24, cpu_24_Nlinear, model = 'NLinear'))
write.csv(model_evaluate_cpu_24, 'C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network/Deep Learning/CPU 24.csv', row.names = FALSE)
