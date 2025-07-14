library(lpirfs)
library(ggpubr)
library(gridExtra)
library(readxl)

######################################### Linear impulse responses with local projections ######################################### 

# Setting the working directory
setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Data')
getwd()

all_data <- read_xlsx("CPU_Data_Reduced.xlsx")
str(all_data)

# Convert Date into Datetime Value
all_data$Date <- as.Date(all_data$Date)
str(all_data)

endog_data_all <- all_data[,c("cpu_index", 
                              "BCI", 
                              "cli",
                              "S&P 500",
                              "CAPE",
                              "cred",
                              "credgdp",
                              "HPI",
                              "capr",
                              "mortg",
                              "mortg_inc",
                              "prfi_gdp",
                              "pip_inc",
                              "UNRATE",
                              "HOUST",
                              "DPCERA3M086SBEA",
                              "PERMITNE"
)]

str(endog_data_all)

results_lin <- lp_lin(endog_data_all, 
                      lags_endog_lin = NaN,    # Number of lags for endogenous data
                      lags_criterion = 'BIC', 
                      max_lags       = 12,
                      trend          = 1,      # 0 = no trend, 1 = trend, 2 = trend & trend^2    
                      shock_type     = 0,      # 0 = standard deviation shock, 1 = unit shock
                      confint        = 1.96,   # Width of confidence bands: # 1 = 68%, 1.67 = 90%, 1.96 = 95%
                      use_nw         = TRUE,
                      hor            = 24)     # Number of cores to use. When NULL, the number of cores 


# is chosen automatically 
linear_plots <- plot_lin(results_lin)

linear_plots[[2]]
linear_plots[[3]]
linear_plots[[4]]
linear_plots[[5]]
linear_plots[[6]]
linear_plots[[7]]
linear_plots[[8]]
linear_plots[[9]]
linear_plots[[10]]
linear_plots[[11]]
linear_plots[[12]]
linear_plots[[13]]
linear_plots[[14]]
linear_plots[[15]]
linear_plots[[16]]
linear_plots[[17]]



# Show all plots 
lin_plots_all <- sapply(linear_plots, ggplotGrob)

marrangeGrob(lin_plots_all,
             nrow = ncol(endog_data_all), 
             ncol = ncol(endog_data_all),
             top = NULL)
