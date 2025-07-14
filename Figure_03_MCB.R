library(tsutils)
library(PMCMRplus)
library(vioplot)
library(greybox)
library(MTS)
library(readxl)
library(dplyr)
library(murphydiagram)

par(mfrow = c(1,1), mfcol = c(1,1))
par(mgp = c(3, 1, 0))
par(mar = c(5.1, 4.1, 4.1, 2.1))

##################################################### MCB RMSE ##################################################### 
setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/MCB')
err <- read_excel('Results Combined X - RMSE.xlsx')
tsutils::nemenyi(err, plottype = "mcb", main = 'MCB Plot for RMSE Metric', conf.level = 0.85)
