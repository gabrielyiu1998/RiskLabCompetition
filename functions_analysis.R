  # ==== Metadata ====
#' Authors: Jason Ho, Edward Wang, Gabriel Yiu
#' Risk Lab Case Competition March 2019
#' 
#' Data References:#' 
#' Stock Data is given from Risk Lab
#' S&P500/TSX composite is from Yahoo Finance
#'    https://ca.finance.yahoo.com/quote/%5EGSPTSE?p=%5EGSPTSE
#' Fama French Returns is from Dartmouth
#'    http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
#' FRED for oil and exchangerates
#'    https://fred.stlouisfed.org/


# ==== Data Import and Cleaning ====
  
library(MASS)
library(tidyverse)
library(ggplot2)
library(kableExtra)
library(lubridate)
library(tseries)
library(rugarch)
library(RiskPortfolios)
library(corrplot)
library(caret)
options(scipen = 10)

RL_STCKS = read_csv("data/historical_prices.csv")
SP500 = read_csv("data/GSPTSE.csv")
fama_french = read_csv("data/ff5_monthly.csv", na = c("99.99", "-999"))
exc_rates = read_csv("data/EXCAUS.csv")
oil = read_csv("data/oil.csv", na = c(".")) %>% na.omit()

RL_STCKS$Date = base::as.Date(RL_STCKS$Date, "%m/%d/%Y")

exc_rates[, "YearEX"] = year(exc_rates$Date)
exc_rates[, "MonthEX"] = month(exc_rates$Date)
exc_rates = select(exc_rates, -c("Date"))

oil$Date = base::as.Date(oil$Date, "%m/%d/%Y")
# ==== CAPM Functions ====

get_stock = function(stck){
  #' @param stck A string whose company is one of the companies in RISKLAB_STOCKS
  #' 
  #' @return A tibble where RISKLAB_STOCKS is filtered by stck.
  
  return(filter(RL_STCKS, Company == !! stck))
}

prep_CAPM = function(stck, rf){
  #' @param stck A string whose company is one of the companies in RISKLAB_STOCKS
  #' 
  #' @return A data.frame
  #' 
  #' @description Returns a data.frame prepared for running it through lm.
  
  stock_data = get_stock(stck)
  SP500_copy = SP500
  colnames(SP500_copy) = paste(colnames(SP500), "SP", sep ="")
  
  df = left_join(select(stock_data, c("Company", "Date", "Open", "Close", "AdjClose")),
                 select(SP500_copy, c("DateSP", "OpenSP", "CloseSP", "AdjCloseSP")),
                 by = c("Date" = "DateSP"))
  
  df = df[order(df$Date),] %>% filter(year(Date) > 2015)
  
  df = df %>%
    group_by(Year = year(Date), Month = month(Date)) %>%
    summarize(Company = first(Company),
              Open = first(AdjClose),
              OpenSP = first(AdjCloseSP),
              AdjClose = last(AdjClose),
              AdjCloseSP = last(AdjCloseSP))
  
  target = rep(NA, nrow(df) - 1)
  predictor = rep(NA, nrow(df) - 1)
  stck_close = pull(df, "AdjClose")
  mt_close = pull(df, "AdjCloseSP")

  for (i in 1:(nrow(df) - 1)){
    target[i] = log(stck_close[i + 1] / stck_close[i]) - rf
    predictor[i] = log(mt_close[i + 1] / mt_close[i]) - rf
  }

  return(data.frame(Company = pull(df, "Company")[1:length(target)],
                    Mt_Rtn = predictor,
                    St_Rtn = target))
}

linear_model_output = function(df){
  #' @param df data.frame output of prep_CAPM
  #' 
  #' @return kable object
  #' 
  #' @description Returns a table of the linear model output of beta
  #' and alpha.
  
  lm_stock = lm(St_Rtn ~ Mt_Rtn, data = df)
  obj = summary(lm_stock)
  return(kable(obj[['coefficients']]) %>% kable_styling())
}
 
accuracy_output = function(df){
  #' @param data.frame output of prep_CAPM
  #' 
  #' @return kable object
  #' 
  #' @description Returns a table of the rsq and rsq - adj values.
  
  lm_stock = lm(St_Rtn ~ Mt_Rtn, data = df)
  obj = summary(lm_stock)
  return(kable(data.frame(R.sq = obj$r.squared, Adj.R.sq = obj$adj.r.squared)) %>% 
           kable_styling())
}

# ==== Generalized Factor Model ====

generalized_factor_prep = function(stck, rf, years, beta_profiles,
                                   profile_names, monthly = TRUE){
  #' @param stck A string variable with the stock name
  #' @param rf The risk free interest rate
  #' @param years The number of years in the data
  #' @param beta_profiles A list of profiles with columns AdjClose and Date
  #' @param profile_names A string vector with the profile indeces
  #'
  #' @return a data.frame object prepared for fitting a model
  #' 
  #' @description It is important that beta_profiles is a list of daily
  #' or monthly stock return data where Date and AdjClose are columns.
  #' The ith entry of the profile name should match the ith dataset in
  #' beta_profiles.

  if (length(profile_names) != length(beta_profiles)){
    warning("Length of profiles must match length of names.")
  }

  stock_data = get_stock(stck)

  for (i in 1:length(profile_names)){
    colnames(beta_profiles[[i]]) = paste(colnames(beta_profiles[[i]]),
                                         profile_names[i], sep = "")
  }

  df = select(stock_data, c("Company", "Date", "AdjClose"))
  
  for (i in 1:length(beta_profiles)){
    df = left_join(df,
                   select(beta_profiles[[i]],
                          c(paste("Date", profile_names[i], sep = ""),
                            paste("AdjClose", profile_names[i], sep = ""))),
                   by = c("Date" = paste("Date", profile_names[i], sep = "")))
  }
  
  df = df %>% filter(year(Date) > 2018 - years) %>% na.omit()
  df = df[order(pull(df, "Date")),]
  
  if (monthly){
    df = df %>%
      group_by(Year = year(Date), Month = month(Date)) %>%
      summarize_each(first, colnames(df))
  }

  target = rep(NA, nrow(df) - 1)
  predictor = rep(NA, nrow(df) - 1)
  stck_close = pull(df, "AdjClose")
  list_stocks = list()

  for (j in 1:length(beta_profiles)){
    list_stocks[[j]] = pull(df, paste("AdjClose", profile_names[j], sep = ""))
  }
  
  predictors = matrix(NA, nrow(df) - 1, length(beta_profiles))

  for (i in 1:(nrow(df) - 1)){
    target[i] = log(stck_close[i + 1] / stck_close[i]) - rf
    for (j in 1:length(beta_profiles)){
      predictors[i, j] = log(list_stocks[[j]][i + 1] / list_stocks[[j]][i]) - rf
    }
  }
  
  if (monthly){
    ret_frame = data.frame(Company = pull(df, "Company")[1:length(target)],
                           Year = pull(df, "Year")[1:length(target)],
                           Month = pull(df, "Month")[1:length(target)],
                           St_Rtn = target,
                           predictors)
    colnames(ret_frame)[5:ncol(ret_frame)] = profile_names
  } else {
    ret_frame = data.frame(Company = pull(df, "Company")[1:length(target)],
                           Date = pull(df, "Date")[1:length(target)],
                           St_Rtn = target,
                           predictors)
    colnames(ret_frame)[4:ncol(ret_frame)] = profile_names   
  }

  return(ret_frame)
}

daily_returns = function(stocks){
  data = matrix(NA, nrow = 250, ncol = length(stocks))
  for (j in 1:length(stocks)){
    st_dt = generalized_factor_prep(stocks[j], 0.0235/365, 1, list(SP500), c("SP"), monthly = FALSE)
    data[, j] = pull(st_dt, "St_Rtn")
  }
  data = data.frame(data)
  colnames(data) = stocks
  return(data)
}

lm_factor_output = function(df, stp = FALSE){
  #' @param df data.frame output of generalized_factor_prep
  #' 
  #' @return kable object
  #' 
  #' @description Returns a table of the linear model output of beta
  #' and alpha.
  
  lm_stock = lm(St_Rtn ~ ., data = select(df, -c("Company", "Year", "Month")))
  
  df = na.omit(df)
  
  if (stp){
    lm_stock = step(lm_stock)
  }
  
  obj = summary(lm_stock)
  
  return(kable(obj[['coefficients']]) %>% kable_styling())
}

accuracy_factor_output = function(df, stp = FALSE){
  #' @param data.frame output of prep_CAPM
  #' 
  #' @return kable object
  #' 
  #' @description Returns a table of the rsq and rsq - adj values.
   
  df = na.omit(df)
  
  lm_stock = lm(St_Rtn ~ ., data = select(df, -c("Company", "Year", "Month")))
  
  if (stp){
    lm_stock = step(lm_stock, trace = 0)
  }
  
  obj = summary(lm_stock)
  return(kable(data.frame(R.sq = obj$r.squared, Adj.R.sq = obj$adj.r.squared)) %>% 
           kable_styling())
}

remove_outliers = function(df){
  #' Removes outliers from daily returns in df
  #' 
  #' @param df daily return output of generalized_factor_model
  #' 
  #' @return Returns the dataframe with outliers removed
  
  outliers = boxplot(pull(df, "St_Rtn"), plot=FALSE)$out
  return(df[-which(pull(df, "St_Rtn") %in% outliers),])
}

# ==== EWMA ====
d_rts = daily_returns(levels(as.factor(RL_STCKS$Company)))
market_returns = read_csv("data/3yrmt_rts.csv")

simulation_market_variables = function(n, stck_data){
  stk_d = select(stck_data, -c("Company", "Month", "Year"))
  
  cov_est = covEstimation(
    as.matrix(stk_d),
    control = list(type = "ewma", lambda = 0.94))
  return(mvrnorm(n, 12 * colMeans(stk_d), 12 * cov_est))
}

# ==== Cross Validation ====
get_CV = function(df, stp = FALSE){
  #' Runs cross validation procedures on the data. With training set as 2014-2017 data
  #' and test set as 2018 data
  #'
  #' @param df daily return output of generalized_factor_model
  #'
  #' @return Returns the out of sample R squared, RMSE, and MAE
  train = df[df$Year != 2018,]
  test = df[df$Year == 2018,]
  # realtest = test[rowSums(is.na(test)) != ncol(test),]
  model = lm(St_Rtn ~., data = select(train, -c("Year", "Month", "Company")))
  if (stp){
    model = step(model, trace = FALSE)
    if ("Parkland Fuel Corporation" %in% df$Company){
      model = lm(St_Rtn ~OIL + HML, data = select(train, -c("Year", "Month", "Company")))
    }  
  }
  predictions <- model %>% predict(test)
  result = data.frame(
    R2 = R2(predictions, test$St_Rtn),
    RMSE = RMSE(predictions, test$St_Rtn),
    MAE = MAE(predictions, test$St_Rtn))
  return(result)
}

# ==== GARCH Model ====

get_garch_model = function(stck){
  stock_data = generalized_factor_prep(stck, 0.0235/365, 3,
                                       list(SP500), c("SP"), monthly = FALSE)
  
  gar_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                        distribution.model = "std")
  stock_data = data.frame(select(stock_data, "St_Rtn"), row.names = pull(stock_data, "Date"))
  stock_gar = ugarchfit(spec = gar_spec, data = stock_data,
                        solver.control = list(tol = 1e-6))
  
  return(stock_gar)
}

forecast_future_volatility = function(model, days){
  return(ugarchforecast(model, n.ahead = days))
}

plot_VaR = function(mdl, a){
  vmodel  = mdl@model$modeldesc$vmodel
  T = mdl@model$modeldata$T
  insample = 1:T
  xseries = mdl@model$modeldata$data[insample]
  xdates  = mdl@model$modeldata$index[insample]
  xsigma 	= mdl@fit$sigma
  distribution = mdl@model$modeldesc$distribution
  xcmu = fitted(mdl)
  idx = mdl@model$pidx
  pars  = mdl@fit$ipars[,1]
  skew  = pars[idx["skew",1]]
  shape = pars[idx["shape",1]]
  if(distribution == "ghst") ghlambda = -shape/2 else ghlambda = pars[idx["ghlambda",1]]
  z1 	= a
  z2 	= (1 - a)
  
  q01 = fitted(mdl) + sigma(mdl)* qdist(distribution, z1, 0, 1, lambda = ghlambda, skew, shape)
  q99 = fitted(mdl) + sigma(mdl)* qdist(distribution, z2, 0, 1, lambda = ghlambda, skew, shape)
  plot(xdates, xseries, type = "l", col = "steelblue", ylab = "Log Returns", xlab="Time",
       main = paste("Series with with", 100 * a, "percent VaR Limits"), cex.main = 0.8)
  
  lines(xdates, q01, col = "tomato1")
  lines(xdates, q99, col = "green")
  mtext(paste("GARCH model :", vmodel), side = 4, adj = 0, padj=0, col = "gray", cex = 0.5)
  if(vmodel == "fGARCH"){
    mtext(paste("fGARCH submodel: ", mdl@model$modeldesc$vsubmodel, sep = ""),
          side = 4, adj = 0, padj=1.5, col = "gray", cex = 0.5)
  }
  abline(h = 0, col = "grey", lty = 3)
  grid()
}

