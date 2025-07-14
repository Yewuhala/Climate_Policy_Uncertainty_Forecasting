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

##################################################### Plots #####################################################

# Google Trends
trends <- read.csv("GooleTrendsData.csv")
trends$Date <- as.Date(paste0(trends$Date, "-01"), format = "%Y-%m-%d")

ggplot(data = trends, aes(x = as.Date(Date))) +
  geom_line(aes(y = Climate.policy...Worldwide., color = 'Climate Policy'), linewidth = 2) +
  geom_line(aes(y = Climate.Risk...Worldwide., color = 'Climate Risk'), linewidth = 2) +
  geom_line(aes(y = cpu_data$cpu_index[202:411], color = 'CPU'), linewidth = 2) +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y-%m-%d", limits = c(ymd("2004-01-01"), ymd("2021-06-01"))) +
  ylab('Interest') +
  xlab('Date') +
  labs(color = 'Search Terms') +
  ggtitle('Interest Over Time') +
  scale_color_manual(values = c(
    'Climate Risk' = '#960018',
    'Climate Policy' = 'dodgerblue3',
    'CPU' = 'forestgreen'
  )) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 20, face = 'bold'), 
        axis.title = element_text(size = 25, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 20, face = 'bold'), 
        legend.title = element_text(size = 25, face = 'bold'))
