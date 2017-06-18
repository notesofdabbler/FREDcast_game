#' ---
#' title: FREDcast game attempt
#' author: Notesofdabbler
#' date: June 18, 2017
#' always_allow_html: yes
#' ---
#' 
#+ message = FALSE, warning = FALSE

# load libraries
library(dygraphs)
library(xts)
library(dplyr)
library(ggplot2)

# load data
# CPIAUCSL - Consumer price index (https://fred.stlouisfed.org/series/CPIAUCSL)
# GDPC1 - Gross Domestic Product (https://fred.stlouisfed.org/series/GDPC1)
# PAYEMS - Total Nonfarm Payroll (https://fred.stlouisfed.org/series/PAYEMS)
# UNRATE - Civilian Unemployment Rate (https://fred.stlouisfed.org/series/UNRATE)
#
# Everything except GDPC1 is monthly data (GDPC1 is quarterly)

cpi_df = read.csv("data/CPIAUCSL.csv", stringsAsFactors = FALSE)
gdp_df = read.csv("data/GDPC1.csv", stringsAsFactors = FALSE)
payems_df = read.csv("data/PAYEMS.csv", stringsAsFactors = FALSE)
unrate_df = read.csv("data/UNRATE.csv", stringsAsFactors = FALSE)

# convert data to xts format
cpi = xts(cpi_df[,2], order.by = as.Date(cpi_df[,1]))
gdp = xts(gdp_df[,2], order.by = as.Date(gdp_df[,1]))
payems = xts(payems_df[,2], order.by = as.Date(payems_df[,1]))
unrate = xts(unrate_df[,2], order.by = as.Date(unrate_df[,1]))

# plot the graphs for each of the series
dygraph(cpi)
dygraph(gdp)
dygraph(payems)
dygraph(unrate)

# Explore pairwise plots of the series
# merge different series into one dataframe (note: GDP is at quarterly level)
datelist = unique(c(cpi_df$DATE, gdp_df$DATE, payems_df$DATE, unrate_df$DATE))
alldf = data.frame(DATE = datelist, stringsAsFactors = FALSE)
alldf = left_join(alldf, cpi_df, by = "DATE")
alldf = left_join(alldf, payems_df, by = "DATE")
alldf = left_join(alldf, unrate_df, by = "DATE")
alldf = left_join(alldf, gdp_df, by = "DATE")

# Check pairs plot
pairs(alldf[,c(2,3,4,5)])

#' Here a very simplistic model is applied to the timeseries and used to forecast
#' the next time point. The model just fits a linear model with dependent variable
#' as the current time point with independent variable as previous time point
#' 
#+
#---------------Fcst CPIAUCSL-----------------------

# Simplistic model based on previous time point

# create a lag variable
cpi_df$CPIAUCSL_lag = lag(cpi_df$CPIAUCSL)

# time series of CPI
dygraph(cpi)
# linear model of current time point with previous time point
ggplot(data = cpi_df, aes(x = CPIAUCSL_lag, y = CPIAUCSL)) + geom_point() + theme_bw()

#
# Check prediction error by running a model that covers a sliding 12 month period
# covering 24 instances starting with "2017-04-01" (second last time point) and going back
# 
#

thor = 12 # pick time horizon - 12 months
tperiods = 24 # number of time periods 
# index of end dates of models
idxstseq = seq(nrow(cpi_df) - tperiods, nrow(cpi_df) - 1)

# initialize the predicted residual of forecasted time point
predres = rep(NA, tperiods)

# loop over different end dates and fit models with 12 month 
# period ending with each end date
#
for(i in seq_along(idxstseq)){
  
  print(i)
  idxend = idxstseq[i]

  # filter the timeseries for the 12 month period ending for a given end date
  dffilt = cpi_df[(idxend - thor + 1):idxend,]
  
  # fit linear model
  cpi_lm = lm(CPIAUCSL ~ CPIAUCSL_lag, dffilt)
  
  # print model summary
  #summary(cpi_lm)
  
  # store predicted residual
  predres[i] = cpi_df[idxend+1,"CPIAUCSL"] - predict(cpi_lm, newdata = cpi_df[idxend+1,])
  
}

# summary of predicted residuals
summary(predres)
# plot of predicted residuals
predres_xts = xts(predres, order.by = as.Date(cpi_df$DATE[idxstseq]))
dygraph(predres_xts)

# forecast for next time point
fcst_cpi = predict(cpi_lm, data.frame(CPIAUCSL_lag = cpi_df$CPIAUCSL[nrow(cpi_df)]))
fcst_cpi

# % change in CPI from one year ago
cpi_1yrago = cpi_df$CPIAUCSL[cpi_df$DATE == "2016-06-01"]
(fcst_cpi - cpi_1yrago)*100/cpi_1yrago

#'
#' The methodology for other series follows the same as for the CPI series
#' 
#+
#---------------Fcst PAYEMS -----------------------

# Simplistic model based on previous time point

payems_df$PAYEMS_lag = lag(payems_df$PAYEMS)

dygraph(payems)
ggplot(data = payems_df, aes(x = PAYEMS_lag, y = PAYEMS)) + geom_point() + theme_bw()


# Check prediction error by running a 12 month horizon for 24 ending dates 
# starting with "2017-04-01" (second last time point) and going back
#

thor = 12 # pick time horizon - 12 months
tperiods = 24 # number of time periods 
# idxst sequence
idxstseq = seq(nrow(payems_df) - tperiods, nrow(payems_df) - 1)

predres = rep(NA, tperiods)

for(i in seq_along(idxstseq)){
  
  #print(i)
  idxend = idxstseq[i]
  
  dffilt = payems_df[(idxend - thor + 1):idxend,]
  
  payems_lm = lm(PAYEMS ~ PAYEMS_lag, dffilt)
  summary(payems_lm)
  
  predres[i] = payems_df[idxend+1,"PAYEMS"] - predict(payems_lm, newdata = payems_df[idxend+1,])
  
}

summary(predres)
predres_xts = xts(predres, order.by = as.Date(payems_df$DATE[idxstseq]))
dygraph(predres_xts)

fcst_payems = predict(payems_lm, data.frame(PAYEMS_lag = payems_df$PAYEMS[nrow(payems_df)]))
fcst_payems

# change in employment
fcst_payems - payems_df$PAYEMS[nrow(payems_df)]

#---------------Fcst GDPC1 -----------------------

# Simplistic model based on previous time point


gdp_df$GDPC1_lag = lag(gdp_df$GDPC1)

dygraph(gdp)
ggplot(data = gdp_df, aes(x = GDPC1_lag, y = GDPC1)) + geom_point() + theme_bw()


# Check prediction error by running a 12 qtr horizon for 24 ending dates 
# starting with "2016-10-01" (second last time point) and going back
#

thor = 12 # pick time horizon - 12 qtrs
tperiods = 24 # number of time periods 
# idxst sequence
idxstseq = seq(nrow(gdp_df) - tperiods, nrow(gdp_df) - 1)

predres = rep(NA, tperiods)

for(i in seq_along(idxstseq)){
  
  #print(i)
  idxend = idxstseq[i]
  
  dffilt = gdp_df[(idxend - thor + 1):idxend,]
  
  gdp_lm = lm(GDPC1 ~ GDPC1_lag, dffilt)
  summary(gdp_lm)
  
  predres[i] = gdp_df[idxend+1,"GDPC1"] - predict(gdp_lm, newdata = gdp_df[idxend+1,])
  
}

summary(predres)
predres_xts = xts(predres, order.by = as.Date(gdp_df$DATE[idxstseq]))
dygraph(predres_xts)

fcst_gdp = predict(gdp_lm, data.frame(GDPC1_lag = gdp_df$GDPC1[nrow(gdp_df)]))
fcst_gdp

# real GDP growth rate
(fcst_gdp - gdp_df$GDPC1[nrow(gdp_df)])*100/gdp_df$GDPC1[nrow(gdp_df)]

#---------------Fcst UNRATE -----------------------

# Simplistic model based on previous time point


unrate_df$UNRATE_lag = lag(unrate_df$UNRATE)

dygraph(unrate)
ggplot(data = unrate_df, aes(x = UNRATE_lag, y = UNRATE)) + geom_point() + theme_bw()


# Check prediction error by running a 12 month horizon for 24 ending dates 
# starting with "2017-04-01" (second last time point) and going back
#

thor = 12 # pick time horizon - 12 qtrs
tperiods = 24 # number of time periods 
# idxst sequence
idxstseq = seq(nrow(unrate_df) - tperiods, nrow(unrate_df) - 1)

predres = rep(NA, tperiods)

for(i in seq_along(idxstseq)){
  
  #print(i)
  idxend = idxstseq[i]
  
  dffilt = unrate_df[(idxend - thor + 1):idxend,]
  
  unrate_lm = lm(UNRATE ~ UNRATE_lag, dffilt)
  summary(unrate_lm)
  
  predres[i] = unrate_df[idxend+1,"UNRATE"] - predict(unrate_lm, newdata = unrate_df[idxend+1,])
  
}

summary(predres)
predres_xts = xts(predres, order.by = as.Date(unrate_df$DATE[idxstseq]))
dygraph(predres_xts)

fcst_unrate = predict(unrate_lm, data.frame(UNRATE_lag = unrate_df$UNRATE[nrow(unrate_df)]))
fcst_unrate

#' ### Session Info
#+
sessionInfo()