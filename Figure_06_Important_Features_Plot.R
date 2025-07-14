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

setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Data')
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

# BSTS - Inclusion Probability
ss <- AddLocalLevel(list(), train_24$cpu_index)
ss <- AddLocalLinearTrend(ss, train_24$cpu_index)
ss <- AddAutoAr(ss, train_24$cpu_index)
ss <- AddSeasonal(ss, train_24$cpu_index, nseasons = 12)
bsts_24 <- bsts(cpu_index ~ .,
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                expected.model.size = 20,
                data = as.data.frame(cbind(train_24$cpu_index, train_reg_reduced_24[,1:60])) %>% rename('cpu_index' = V1))
plot(bsts_24,'coefficients')
