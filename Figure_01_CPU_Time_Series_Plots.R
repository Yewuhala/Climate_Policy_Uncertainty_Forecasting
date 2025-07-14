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

##################################################### Plots #####################################################

# Combined TS Plots
n = 24
df <- data.frame(y = train_24$cpu_index, date = seq(from = as.Date("1987-04-01"), to = as.Date("2021-06-01"), by = "month"))
g1 <- ggplot(data = df, aes(x = as.Date(date))) +
  geom_line(aes(y = y)) +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  xlab('Years') +
  ylab('CPU Index') +  
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'))

diff_train <-  diff(train_24$cpu_index, differences = ndiffs(train_24$cpu_index))

g2 <- ggAcf(diff_train) +
  geom_point(color = 'navy blue') +
  theme_classic() +
  ggtitle("") +
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'))

g3 <- ggPacf(diff_train) +
  geom_point(color = 'navy blue') +
  theme_classic() +
  ggtitle("") +
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'))

train <- cpu_data[1:(nrow(cpu_data) - n), c(1,ncol(cpu_data))]
str(train)
train$Date <- yearmonth(train$Date)
train <- as_tsibble(train, index = Date)

g4 <- train %>%
  model(STL(cpu_index)) %>%
  components() %>%
  autoplot()  +
  theme_classic() +
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'))



(g1 / g2 / g3) | g4 +
  plot_layout(ncol = 2, widths = c(5, 0))
