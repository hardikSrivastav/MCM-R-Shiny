library(quantmod)
library(ggplot2)
library(pacman)
library(yfinance)
library(yfR)
library(pracma)
library(TTR)
library(patchwork)

get_cf(ticker = c('AAPL'), report_type = 'quarterly')

aapl <- yf_get('MSFT', '2020-01-01', '2024-01-01', freq = 'daily')

getSymbols('AAPL', from = '2020-01-01', to = '2024-01-01')

aapl$SMA <- movavg(aapl$price_adjusted, n=200, type='e') 
aapl$Volatility <- runSD(aapl$ret_adjusted_prices, n = 20) * sqrt(252)
aapl$EMA <- movavg(aapl$price_adjusted, n=20, type='e') 

color <- c('Adjusted Price' = 'black', 'EMA_50' = 'blue', 'EMA_200' = 'orange')
SMA <- ggplot(data = aapl, aes(x = ref_date)) +
  geom_line(aes(y = price_adjusted, color = 'Adjusted Price'), size = 1) +
  geom_line(aes(y = EMA, color = "EMA_50"), size = 1 ) + 
  geom_line(aes(y = SMA, color = 'EMA_200'), size = 1) +
  scale_color_manual(values = color)

stock.osc <- as.data.frame(SMI(data.frame(aapl$price_high, aapl$price_low, aapl$price_close), n = 13, nFast = 2, nSlow = 25, nSig = 9))
stock.osc$date <- aapl$ref_date


for (i in c('price_high', 'price_close', 'price_low')) {
  aapl[[i]] <- as.numeric(as.character(unlist(aapl[[i]])))
}

aapl[, c('price_high', 'price_close', 'price_low')] %>% as.numeric(as.character(unlist(.)))


stock.osc[is.na(stock.osc)] <- 0

aapl$rsi[is.na(aapl$rsi)] <- 0

aapl$rsi <- RSI(aapl$price_close) 
aapl$rsi[is.na(aapl$rsi)] <- 0

RSI <- ggplot(data = aapl, aes(x = ref_date)) +
  geom_line(aes(y = rsi)) +
  ylim(0, 100) + 
  geom_hline(yintercept = 70) +
  geom_hline(yintercept = 30)

stock.SMA <- stoch(aapl$SMA)
stock.EMA <- stoch(aapl$EMA)
stock.RSI <- stoch(aapl$rsi)

plot(stock.RSI[, 'fastK'], type = 'l')

SMI <- ggplot(data = stock.osc, aes(x = date)) +
  geom_line(aes(y = SMI), color = "blue", size = 1) +
  geom_line(aes(y = signal), color = "orange", size = 1) + 
  ylim(-100, 100) + 
  geom_hline(yintercept = -40) +
  geom_hline(yintercept = 40)

SMI + SMA + RSI +plot_layout(2)

signal <- function(indicator) {
  sig <- vector()
  if (indicator == 'RSI') {
    for (i in 1:length(aapl$rsi)) {
      if (aapl$rsi[i] > 70) {
        sig[i] <- 'Sell'
      } else if (aapl$rsi[i] < 30) {
        sig[i] <- 'Buy'
      } else {
        sig[i] <- 'Hold'
      }
    }
  } else if (indicator == 'SMI') {
    for (i in 1:length(stock.osc$date)) {
      if (stock.osc$SMI[i] > 40 & stock.osc$SMI[i] < stock.osc$signal[i]) {
        sig[i] <- 'Sell'
      } else if (stock.osc$SMI[i] < -40 & stock.osc$SMI[i] > stock.osc$signal[i]) {
        sig[i] <- 'Buy' 
      } else {
        sig[i] <- 'Hold'
      }
    }
  } else if (indicator == 'EMA') {
    for (i in 2:length(aapl$SMA)) {
      if (aapl$SMA[i-1] < aapl$EMA[i-1] & aapl$SMA[i] > aapl$EMA[i]) {
        sig[ i-1 ] <- 'Sell'
      } else if (aapl$SMA[i-1] > aapl$EMA[i-1] & aapl$SMA[i] < aapl$EMA[i]) {
        sig[ i-1 ] <- 'Buy'
      } else {
        sig [i-1 ] <- 'Hold'
      }
    }
    sig[length(sig)+1] <- 'Hold'
  }
  sig <- as.data.frame(sig, row.names = as.character(aapl$ref_date))
  return(sig)
}

signals[, RSI] <- signal('RSI')
signals[, 'SMI'] <- signal('SMI')

colnames(signals) <- c('RSI', 'SMI')

aapl[is.na(aapl)] <- 0
sig <- vector()
for (i in 2:length(aapl$SMA)) {
  if (aapl$SMA[i-1] < aapl$EMA[i-1] & aapl$SMA[i] > aapl$EMA[i]) {
    sig[ i-1 ] <- 'Sell'
  } else if (aapl$SMA[i-1] > aapl$EMA[i-1] & aapl$SMA[i] < aapl$EMA[i]) {
    sig[ i-1 ] <- 'Buy'
  } else {
    sig [i-1 ] <- 'Hold'
  }
}

sig <- as.data.frame(sig, row.names = as.character(aapl$ref_date[1:1005]))

datevec <- vector()
label <- vector()
for (i in 1:length(sig$sig)) {
  if (sig$sig[i] == 'Buy') {
    datevec <- c(datevec, rownames(sig)[i])
    label <- c(label, 'Buy')
  } else if (sig$sig[i] == 'Sell') {
    datevec <- c(datevec, rownames(sig)[i])
    label <- c(label, 'Sell')
  }
}

y <- vector()
for (i in 1:length(aapl$ref_date)){
  if (as.character(aapl$ref_date)[i] %in% datevec) {
    y <- c(y, aapl$SMA[i])
  }
}

annotation <- data.frame(
  x = datevec,
  y = y,
  label = label
)

SMA + geom_label(
  data=annotation, aes( x=as.Date(x), y=y, label=label),                 , 
  color="red", size = 2.75
)


'''
ticker_by_sector <-  list()
sector.data <- list()
sectors <- unique(nasdaq$Sector)
for (i in sectors) {
  tickers <- nasdaq[nasdaq$Sector == i,]$Symbol
  ticker_by_sector[[ i ]] <- sample(tickers, size = 100, replace = F)
  sector.data[[ i ]] <- yf_get(ticker_by_sector[[ i ]], first_date= '2000-01-01', last_date = '2024-01-01', freq = 'monthly')
}

sector.data[['Technology']] <- yf_get(sample(nasdaq[nasdaq$Sector == 'Technology',]$Symbol, size = 100, replace = F), first_date= '2000-01-01', last_date = '2024-01-01', freq = 'monthly')

mean.list <- list()
for (i in names(sector.data)) {
  df <- as.data.frame(sector.data[[ i ]])
  data.mean <- list()
  for (j in as.character(unique(df$ref_date))) {
    mean_ <-  mean(df[df$ref_date == j,]$price_open)
    data.mean[[ j ]] <- mean_
  }
  mean.list[[ i ]] <- transpose(as.data.frame(data.mean))
  rownames(mean.list[[i]]) <- names(data.mean)
}
'''
