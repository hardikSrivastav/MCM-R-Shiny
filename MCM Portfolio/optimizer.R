require(pacman)
require(yfR)
require(rio)
require(dplyr)
require(tidyr)
require(pacman)
require(purrr)
require(plyr)
require(lubridate)
require(ggplot2)

#source('~/Projects/MCM\ Portfolio/app.R')
#########
getdata <- function(ticker, from, to, frequency = 'daily') {
  return(
    yf_get(
      tickers = ticker,
      first_date = from,
      last_date = to,
      freq = frequency
    )
  )  
}
test.data <- getdata(c('AAPL', 'MSFT', 'GOOG'), '2020-01-01', '2024-01-01')
##########

ret_risk <- function(data){
  make_data <- function(data){
    stock_data <- data %>%
      group_by(ticker) %>%
      mutate(pct_change = (price_close - lag(price_close)) / lag(price_close) * 100) %>% 
      .[ , c('ticker', 'ref_date', 'price_close', 'pct_change')]
    stock_data[is.na(stock_data)] <- 0
    stock_list <- stock_data %>%
      split(.$ticker) %>%  
      map(~ list(pct_change = .$pct_change))
    return(
      list(
        stock_data,
        stock_list
      )
    )
  }
  stock_data <- make_data(data)[[1]]
  stock_list <- make_data(data)[[2]]
  cov_df <- data.frame(stock_list)
  names(cov_df) <- names(stock_list)
  covMatrix <- cov(cov_df)
  get_ret <- function(data){
    ret <- c()
    for (name in unique(stock_data$ticker)) {
      ret[name] <- mean(filter(stock_data, ticker == name)$pct_change)
    }
    return(ret)
  }
  ret <- get_ret(stock_data)
  return(
    list(
      ret,
      covMatrix,
      stock_list,
      stock_data
    )
  )
}

sr <- function(weight_iters, data){
  ret = ret_risk(data)[[1]]
  cov = ret_risk(data)[[2]]
  weight_collection <- list()
  return_collection <- c()
  risk_collection <- c()
  sharpe_collection <- c()
  generate_weights <- function(n) {
    #random_numbers <- sort(runif(n-1))
    #random_set <- c(random_numbers[1], diff(random_numbers), 1 - random_numbers[n - 1])
    #return(random_set)
    random_numbers <- runif(n)
    random_numbers <- random_numbers/sum(random_numbers)
    return(random_numbers)
  } 
  for (i in 1:weight_iters){
    instant_wt <- generate_weights(length(ret))
    inst_return <- as.double(instant_wt%*%ret)*252
    inst_risk <- sqrt(as.double(instant_wt%*%(cov%*%instant_wt)))*sqrt(252)
    inst_sharpe <- inst_return/inst_risk
    weight_collection[[paste('iter', i, sep = "-")]] <- instant_wt
    return_collection[paste('iter', i, sep = "-")] <- inst_return
    risk_collection[paste('iter', i, sep = "-")] <- inst_risk
    sharpe_collection[paste('iter', i, sep = "-")] <- inst_sharpe
  }
  index <- function(val, list){
    for(i in 1:length(list)){
      if(list[paste('iter', i, sep = '-')] == val){
        iteration <- i
      }
    }
    return(paste('iter', iteration, sep = '-'))
  }
  max_sr <- data.frame(
    'iteration' = index(max(sharpe_collection), sharpe_collection),
    'sharpe ratio' = max(sharpe_collection), 
    'return' = return_collection[index(max(sharpe_collection), sharpe_collection)],
    'risk' = risk_collection[index(max(sharpe_collection), sharpe_collection)]
  )
  min_vol <- data.frame(
    'iteration' = index(min(risk_collection), risk_collection),
    'sharpe ratio' = sharpe_collection[index(min(risk_collection), risk_collection)], 
    'return' = return_collection[index(min(risk_collection), risk_collection)],
    'risk' = min(risk_collection)
  )
  max_ret <- data.frame(
    'iteration' = index(max(return_collection), return_collection),
    'sharpe ratio' = sharpe_collection[index(max(return_collection), return_collection)], 
    'return' = max(return_collection),
    'risk' = risk_collection[index(max(return_collection), return_collection)]
  )
  weights <- data.frame(
    'weights' = weight_collection[[index(max(sharpe_collection), sharpe_collection)]],
    'mean_returns (annual %)' = ret
  )
  return(
    list(
      'results' = do.call("rbind", list(
        'max_sharpe' = max_sr,
        'min_risk' = min_vol,
        'max_return' = max_ret
      )), 
      'weight' = weights,
      'weight_collection' = weight_collection,
      'ret_collection' = return_collection,
      'risk_collection' = risk_collection
    )
  )
}

wt_collection <- sr(10000, test.data)$weight_collection
ret_collection <- sr(10000, test.data)$ret_collection
risk_collection <- sr(10000, test.data)$risk_collection

joined_ret_risk <- data.frame(
  'return' = ret_collection,
  'risk' = risk_collection
)
ggplot(data = joined_ret_risk, aes(x = risk, y = return)) +
  geom_point()


beta <- function(ticker) {
  tickers <- c('^GSPC', '^TNX', ticker)
  date1 = '2020-01-01'
  date2 = '2024-01-01'
  len = length(seq(from = as.Date(date1), to = as.Date(date2), by = 'year'))-1
  sp_data_og <- getdata(tickers, date1, date2)
  sp_data <- sp_data_og %>%
    group_by(ticker) %>%
    mutate(pct_change = (price_close - lag(price_close)) / lag(price_close)) %>% 
    .[ , c('ticker', 'ref_date', 'price_close', 'pct_change')]
  sp_data[is.na(sp_data)] <- 0
  sp_list <- sp_data %>%
    split(.$ticker) %>%  
    map(~ list(pct_change = .$pct_change)) %>%
    data.frame(.)
  colnames(sp_list) <- tickers
  covMatrix <- cov(sp_list)
  treasury <- filter(sp_data_og, ticker == '^TNX')
  sp <- filter(sp_data_og, ticker == '^GSPC')
  r_f <- 0.037
  r_m <- ((sp$price_close[length(rownames(sp))] - sp$price_close[1])/sp$price_close[1])/len
  beta <- c()
  for (i in ticker){
    beta[i] <- covMatrix['^GSPC', i]/var(sp_list$'^GSPC') 
  }
  capm <- function(beta, r_m, r_f){
    multiplier = r_m - r_f  
    capm <- beta * multiplier + r_m
    capm_ <- data.frame(
      'beta' = beta,
      'capm' = capm
    )
    return(capm_)
  }
  capm <- capm(beta, r_m, r_f)
  covPresent <- covMatrix[-c(1,2), -c(1,2)]
  return(list(
    'covariance' = covPresent,
    'capm' = t(capm)
  )
  )
}

data.frame(date = unique(ret_risk(test.data)[[4]]$ref_date), ret_risk(test.data)[[3]])
test.data.list <- data.frame(date = unique(ret_risk(test.data)[[4]]$ref_date), ret_risk(test.data)[[3]])
names(test.data.list) <- c('Date', 'AAPL', 'GOOG', 'MSFT')

########
library(PortfolioAnalytics)
# initialise with asset names uses time series data
data_p2 = zoo(test.data.list[, -1], order.by = as.Date(test.data.list$Date))
# create specification
port = portfolio.spec(assets = c(colnames(data_p2)))
# add long only constraint
port = add.constraint(portfolio = port, type = "long_only")
# add full investment contraint
port = add.constraint(portfolio = port, type = "full_investment")

# objective: manimise risk
port_rnd = add.objective(portfolio = port, type = "risk", name = "StdDev")

# objective: maximise return
port_rnd = add.objective(portfolio = port_rnd, type = "return", name = "mean")

# 1. optimise random portfolios

rand_p = optimize.portfolio(R = data_p2, portfolio = port_rnd, optimize_method = "random",
                            trace = TRUE, search_size = 1000)
# plot

chart.RiskReward(rand_p, risk.col = "StdDev", return.col = "mean", chart.assets = TRUE)  #also plots the equally weighted portfolio

port_msd = add.objective(portfolio = port, type = "risk", name = "StdDev")

minvar1 = optimize.portfolio(R = data_p2, portfolio = port_msd, optimize_method = "ROI",
                             trace = TRUE)

chart.RiskReward(minvar1, risk.col = "StdDev", return.col = "mean", chart.assets = TRUE)
#########
