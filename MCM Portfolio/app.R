library(quantmod)
library(shiny)
library(shinydashboard)
library(plotly)
library(pacman)
library(rio)
library(yfR)
library(gghighlight)
library(reshape2)
library(rsconnect)
library(prophet)
library(data.table)
library(pracma)
library(TTR)
library(DT)

#stock.predictor(10, '2024-01-01', 'AAPL', '2020-01-01', '2022-01-01', 'weekly', 'AAPL')$'median'

nasdaq <- as.data.frame(import("/Users/hardiksrivastav/Projects/MCM\ Portfolio/nasdaq.csv", index = T, header = T))
ticker.list <- as.list(nasdaq[, 'Symbol'])

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



source('~/Projects/MCM\ Portfolio/optimizer.R')


describe.portfolio <- function(tickers) {
  return(
    as.data.frame(subset(nasdaq, Symbol %in% tickers, c('Symbol', 'Name', 'Market Cap', 'Country', 'IPO Year', 'Volume', 'Sector', 'Industry')))
  )
}

get.mean.sdev <- function(ticker, from, to, freq = 'daily', alt) {
  if (ticker == 'Portfolio') {
    temp <- portfolio.maker(alt, from, to, freq)$'ret_adjusted_prices'
  } else {
    temp <- getdata(ticker, from, to, freq)$'ret_adjusted_prices'
  }
  temp[is.na(temp)] <- 0
  median <- round(median(temp), 6)
  sdev <- round(sqrt(var(temp)), 6)
  return (list(
      median = median, sdev = sdev
  ))
}

portfolio.maker <- function(ticker, from, to, freq = 'daily') {
  data <- getdata(ticker, from, to, freq)
  temp.data <- data[, 2:length(colnames(data))]
  temp.data <- aggregate(. ~ ref_date, data=temp.data, FUN=sum)
  return(temp.data)
}

stock.predictor <- function(iterations, end.date, ticker, from, to, freq = 'daily', alt) {
  if (ticker == 'Portfolio') {
    data <- portfolio.maker(alt, from, to, freq)
  } else {
    data <- getdata(ticker, from, to, freq)
  }
  mean <- get.mean.sdev(ticker, from, to, freq, alt)[[1]]
  sdev <- get.mean.sdev(ticker, from, to, freq, alt)[[2]]
  if (freq == 'daily') {
    by <- 'day'
  } else if (freq == 'weekly') {
    by <- 'week'
  } else if (freq == 'monthly') {
    by <- 'month'
  } else if (freq == 'yearly') {
    by <- 'year'
  }
  days <- seq(from = as.Date(to), to = as.Date(end.date), by = by) %>% .[2:length(.)]
  day.length <- length(days)
  iter <-  data.frame(matrix(nrow = day.length))
  for (iteration in 1:iterations) {
    randints <- rnorm(day.length, mean, sdev)
    values = c()
    for (i in 1:length(randints)) {
      if (length(values) == 0) {
        values[i] <- (data$'price_adjusted'[length(data$'price_adjusted')])*(1+randints[i])
      } else {
        values [i] <- values[i-1]*(1+randints[i])
      }
    }
    iter[, paste('iter', as.character(iteration), sep = '.')] <- values
    iter[, 1] <- days 
    colnames(iter)[1] <- 'Date'
  }
  norm <- iter[length(rownames(iter)), 2:length(colnames(iter))]
  norm2 <- as.numeric(norm)
  med <- sort(norm2)[length(norm2)/2]
  median.iter <- 0
  for (name in names(norm)) {
    if (norm[[name]] == med) {
      median.iter <- iter[[name]]
    }
  }
  return(list(
    'full' = iter,
    'normal' = norm2,
    'median' = as.numeric(median.iter)
    )
  )
}

prophet.predict <- function(ticker, from, to, frequency = 'daily', end.date, alt) {
  if (ticker == 'Portfolio') {
    data <- portfolio.maker(alt, from, to, frequency)[, c('ref_date', 'price_adjusted')]
  } else {
    data <- getdata(ticker, from, to, frequency)[, c('ref_date', 'price_adjusted')]
  }
  colnames(data) <- c('ds', 'y')
  period.predict <- length(seq(as.Date(to), as.Date(end.date), "day")) - 1
  model <- prophet(data)
  time.series <- make_future_dataframe(model, periods = period.predict)
  forecast <- predict(model, time.series)
  forecast.length <- length(rownames(forecast))
  period.train <- forecast.length - period.predict
  forecast <- forecast[period.train:forecast.length, c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]
  fig <- plot_ly(forecast, x = ~ds, y = ~yhat_upper, type = 'scatter', mode = 'lines',
                 line = list(color = 'transparent'),
                 showlegend = FALSE, name = 'High 2014') 
  fig <- fig %>% add_trace(y = ~yhat_lower, type = 'scatter', mode = 'lines',
                           fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                           showlegend = FALSE, name = 'Low 2014') 
  fig <- fig %>% add_trace(x = ~ds, y = ~yhat, type = 'scatter', mode = 'lines',
                           line = list(color='rgb(0,100,80)'),
                           name = 'Average') 
  return(fig)
}


signal <- function(ticker, from, to, freq = 'daily', alt, small.ema, big.ema) {
  if (ticker == 'Portfolio') {
    data <- portfolio.maker(alt, from, to, freq)
  } else {
    data <- getdata(ticker, from, to, freq)
  }
  data$EMA_big <- movavg(data$price_adjusted, n=big.ema, type='e') 
  data$Volatility <- runSD(data$ret_adjusted_prices, n = 20) * sqrt(252)
  data$EMA_small <- movavg(data$price_adjusted, n=small.ema, type='e')
  data$rsi <- RSI(data$price_close) 
  data$rsi[is.na(data$rsi)] <- 0
  stock.osc <- as.data.frame(SMI(data.frame(data$price_high, data$price_low, data$price_close), n = 13, nFast = 2, nSlow = 25, nSig = 9))
  stock.osc$date <- data$ref_date
  stock.osc[is.na(stock.osc)] <- 0
  sig1 <- vector()
  sig2 <- vector()
  sig3 <- vector()
  state <- 'Sell'
  for (i in 1:length(data$rsi)) {
    if (data$rsi[i] > 70 & state == 'Sell') {
      sig1[i] <- 'Buy'
      state <- 'Buy'
    } else if (data$rsi[i] < 30 & state == 'Buy') {
      sig1[i] <- 'Sell'
      state <- 'Sell'
    } else {
      sig1[i] <- 'Hold'
    }
  }
  state <- 'Sell'
  for (i in 1:length(stock.osc$date)) {
    if (stock.osc$SMI[i] > 40 & stock.osc$SMI[i] < stock.osc$signal[i] & state == 'Buy') {
      sig2[i] <- 'Buy'
      state <- 'Sell'
    } else if (stock.osc$SMI[i] < -40 & stock.osc$SMI[i] > stock.osc$signal[i] & state == 'Sell') {
      sig2[i] <- 'Sell' 
      state <- 'Buy'
    } else {
      sig2[i] <- 'Hold'
    }
  }
  for (i in 2:length(data$EMA_big)) {
    if (data$EMA_big[i-1] < data$EMA_small[i-1] & data$EMA_big[i] > data$EMA_small[i]) {
      sig3[ i-1 ] <- 'Sell'
    } else if (data$EMA_big[i-1] > data$EMA_small[i-1] & data$EMA_big[i] < data$EMA_small[i]) {
      sig3[ i-1 ] <- 'Buy'
    } else {
      sig3[i-1 ] <- 'Hold'
    }
  }
  sig3[length(sig3)+1] <- 'Hold'
  sig.col <- data.frame(sig1, sig2, sig3, data$EMA_big, data$EMA_small, data$price_adjusted, data$rsi, stock.osc$SMI, stock.osc$signal)
  rownames(sig.col) <- data$ref_date
  colnames(sig.col) <- c('RSI.suggest', 'SMI.suggest', 'EMA.suggest', 'EMA.big', 'EMA.small', 'price_adjusted', 'rsi', 'SMI.big', 'SMI.small')
  return(sig.col)
}

annotations <- function(sig.col) {
  annotation.vec <- list()
  for (col in colnames(sig.col)[1:3]) {
    datevec <- vector()
    label <- vector()
    for (i in 1:length(sig.col[, col])) {
      if (sig.col[, col][i] == 'Buy') {
        datevec <- c(datevec, rownames(sig.col)[i])
        label <- c(label, 'Buy')
      } else if (sig.col[, col][i] == 'Sell') {
        datevec <- c(datevec, rownames(sig.col)[i])
        label <- c(label, 'Sell')
      }
    }
    y <- vector()
    for (i in 1:length(rownames(sig.col))){
      if (as.character(rownames(sig.col))[i] %in% datevec) {
        y <- c(y, sig.col$EMA.small[i])
        #print(sig.col$EMA[i])
      }
    }
    val <- data.frame(
      x = datevec,
      y = y,
      label = label
    )
    annotation.vec <- append(annotation.vec, val) 
  }
  names(annotation.vec) <- c('x1', 'y1', 'l1', 'x2', 'y2', 'l2', 'x3', 'y3', 'l3')
  return(annotation.vec)
}

#APP

header <- dashboardHeader(
  title = 'MCM Portfolio'
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'dashboard',
    fluidRow(
      box(
        title = 'Date',
        sliderInput("date", "Date Range: ", 
                    min = as.Date('2000-01-02'),
                    max = Sys.Date(),
                    value = c(as.Date('2015-12-01'), as.Date('2023-12-01'))
                    )
        
      ),
      box(
        #width = '20%',
        selectInput(
          'freq', "Frequency :",
          choices = c('daily', 'weekly', 'monthly', 'yearly'),
          multiple = FALSE,
          selected = 'Daily'
        ),
        title = textOutput('showfreq'),
        textOutput('mean'),
        textOutput('stdev')
      )
    ),
    fluidRow(
      box(
        checkboxInput('check.general', "General Stocks", value = FALSE),
        selectInput(
          "single.tick", "Company :",
          choices = ticker.list,
          selectize = TRUE,
          multiple = FALSE,
          selected = "AAPL"
        ),
        width = "100%",
        textOutput('result'),
        plotlyOutput("plot1")
      )
    ),
  ),
  tabItem(tabName = 'compare',
  fluidRow(
    box(
      title = 'Box Plot Compare',
      plotOutput('compare')
    ),
    box(
      title = 'Line Plot Compare',
      plotOutput('linecompare')
    ),
  ),
  fluidRow(
    box(
      width = '100%',
      title = "Portfolio Description",
      tableOutput('nasdaq.pf')
    )
  )
  ),
  tabItem(tabName = 'mcm',
  fluidRow(
    box(
      title = 'MCM Analysis',
      selectInput(
        'iter', "Iterations: ",
        choices = c(10, 100, 1000), 
        multiple = FALSE, selected = 100
      ),
      selectInput(
        'unit', "Unit: ",
        choices = c('weekly', 'monthly', 'yearly'),
        multiple = FALSE,
        selected = 'weekly'
      )
    ),
    box(  
      sliderInput("till.date", "Date Till: ", 
                  min = as.Date('2023-01-01'),
                  max = as.Date('2050-01-01'),
                  value = Sys.Date()
      )
    )
  ),
  fluidRow(
    box(
      #width = '100%',
      plotOutput('mcmgraph'),
      plotOutput('mcmpredict')
    ), 
    box(
      textOutput('median.array'),
      plotOutput('mcmnorm'),
      plotOutput('mcmbox')
    )
  )
  ),
  tabItem(tabName = 'prophet',
  fluidRow(
    box(
      title = 'Meta Prophet Prediction',
      plotlyOutput('prophet.predict')
    )
  )
  ),
  tabItem(tabName = 'tech',
  fluidRow(
    box(
      #width = '100%',
      title = 'Relative Strength Index',
      plotOutput('rsi.suggest')
    ),
    box(
      plotOutput('rsi')
    )
  ),
  fluidRow(
    box(
      #width = '100%',
      title = 'Stochastic Momentum Index',
      plotOutput('smi.suggest')
    ),
    box(
      plotOutput('smi')
    )
  ),
  fluidRow(
    box(
      #width = '100%',
      title = 'Exponential Moving Average',
      plotOutput('ema.suggest')
    ),
    box(
      sliderInput("big.ema", "Long EMA Period: ", 
                  min = 50,
                  max = 300,
                  value = 200
      ), 
      sliderInput("small.ema", "Short EMA Period: ",
                  min = 1, 
                  max = 50, 
                  value = 20
      ),
      plotOutput('ema')
    )
  )
  ),
  tabItem(tabName = 'optimize',
          fluidRow(
          box(
            sliderInput("iters", "Iterations: ",
                        min = 1, 
                        max = 10000, 
                        value = 1000
            ) 
          )
          ),
          fluidRow(
          box(
            #textOutput('textCheck'),
            tableOutput('optimise'),
            tableOutput('weights')
          )
          )
  )
)
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Stock Viewer", tabName = 'dashboard'),
    menuItem("Portfolio", tabName = 'compare'),
    menuItem("MCM Analysis", tabName = 'mcm'),
    menuItem("Prophet Prediction", tabName = 'prophet'),
    menuItem("Technicals", tabName = 'tech'),
    menuItem("Optimiser", tabName = 'optimize'),
    selectInput(
      "multiple.tick", "Portfolio :",
      choices = ticker.list,
      selectize = TRUE,
      multiple = TRUE,
      selected = "AAPL"
    ),
    actionButton(
      inputId = "reset_button",
      label = "Reset"
    )
  )
)

ui <- dashboardPage(
  header, sidebar, body
)

server <- function(input, output, session) {
  output$plot1 <- renderPlotly({
  if (input$single.tick == 'Portfolio') {
    data_ <- portfolio.maker(input$multiple.tick, input$date[1], input$date[2], input$freq)
  } else {
    data_ <- getdata(input$single.tick, input$date[1], input$date[2], input$freq)
  }
  fig <- data_ %>% plot_ly(x = ~ref_date, type = 'candlestick', 
                    open = ~price_open, close = ~price_close, 
                    high = ~price_high, low = ~price_low) %>% 
    layout(xaxis = list(rangeslider = list(visible = F), dtick = "M1"),
           plot_bgcolor='#e5ecf6')
  fig
  })
  output$compare <- renderPlot({
    data2 <- getdata(input$multiple.tick, input$date[1], input$date[2])
    ggplot(data2, aes(x = ticker, y = price_adjusted)) +
      geom_boxplot() +
      stat_summary(fun.y=mean, geom="point", shape=23, size=4)
  })
  output$result <- renderText({
    input$ticker
  })
  output$showfreq <- renderText({
    paste("Frequency: ", input$freq)
  })
  output$mean <- renderText({
    paste("Mean Return:", as.character(get.mean.sdev(input$single.tick, input$date[1], input$date[2], input$freq, input$multiple.tick)[[1]]*100), "%")
  })
  output$stdev <- renderText({
    paste("Standard Deviation:", as.character(get.mean.sdev(input$single.tick, input$date[1], input$date[2], input$freq, input$multiple.tick)[[2]]*100), "%")
  })
  output$median.array <- renderText({
    stock.median <- stock.predictor(input$iter, input$till.date, input$single.tick, input$date[1], input$date[2], input$unit, input$multiple.tick)$'median' %>% .[length(.)]
    paste('Median Stock Price', stock.median)
  })
  output$mcmgraph <- renderPlot({
    temp.test <- stock.predictor(input$iter, input$till.date, input$single.tick, input$date[1], input$date[2], input$unit, input$multiple.tick)$'full' %>% reshape2::melt(., id.vars = 1, measure.vars = 2:length(colnames(.)))
    if (input$single.tick == 'Portfolio') {
      last <- portfolio.maker(input$multiple.tick, input$date[1], input$date[2], input$freq)$'price_adjusted' %>% .[length(.)]
    } else {
      last <- getdata(input$single.tick, input$date[1], input$date[2], input$freq)$'price_adjusted' %>% .[length(.)]
    }
    ggplot(temp.test, aes(Date, value, col = variable)) +  geom_line() + theme(legend.position="none") + gghighlight(median(value) > last)
  })
  output$mcmnorm <- renderPlot({
    normal <- stock.predictor(input$iter, input$till.date, input$single.tick, input$date[1], input$date[2], input$unit, input$multiple.tick)$'normal'
    mean <- mean(normal)
    sd <- sd(normal)
    x <- (seq(-2, 2, length = 1000) * sd + mean) 
    y <- dnorm(x, mean, sd)
    plot(x, y, type = 'l')
  })
  output$mcmbox <- renderPlot({
    normal <- stock.predictor(input$iter, input$till.date, input$single.tick, input$date[1], input$date[2], input$unit, input$multiple.tick)$'normal'
    boxplot(normal, horizontal = TRUE)
  })
  output$mcmpredict <- renderPlot({
    normal <- stock.predictor(input$iter, input$till.date, input$single.tick, input$date[1], input$date[2], input$unit, input$multiple.tick)$'normal'
    plot(normal, type = 'l', xlab="Days Fwd", ylab ="Predicted Movement")
  })
  output$linecompare <- renderPlot({
    data3 <- getdata(input$multiple.tick, input$date[1], input$date[2])
    ggplot(data3, aes(x = ref_date, y = price_adjusted, col = ticker, group = ticker)) +
      geom_line()
  })
  output$nasdaq.pf <- renderTable({
    describe.portfolio(input$multiple.tick)
  })
  output$prophet.predict <- renderPlotly({
    prophet.predict(input$single.tick, input$date[1], input$date[2], input$freq, input$till.date, input$multiple.tick)
  })
  output$rsi.suggest <- renderPlot({
    data__ <- signal(input$single.tick, input$date[1], input$date[2], 'daily', input$multiple.tick, input$small.ema, input$big.ema)
    ann <- annotations(data__)
    ann1 <- data.frame(
      x = ann[['x1']],
      y = ann[['y1']],
      label = ann[['l1']]
    )
    ggplot(data = data__, aes(x = as.Date(rownames(data__)))) +
      geom_line(aes(y = price_adjusted), size = 1) +
      #geom_line(aes(y = EMA, color = "EMA_50"), size = 1 ) + 
      #geom_line(aes(y = SMA, color = 'EMA_200'), size = 1) +
      #scale_color_manual(values = 'blac') +
      geom_label(
        data=ann1, aes(x=as.Date(x), y=y, label=label),                  
        color="red", size = 2.75
      )
  })
  output$rsi <- renderPlot({
    data__ <- signal(input$single.tick, input$date[1], input$date[2], 'daily', input$multiple.tick, input$small.ema, input$big.ema)
    #color <- c("EMA Short" = 'azure3', "EMA Long" = 'azure4')
    ggplot(data = data__, aes(x = as.Date(rownames(data__)))) +
    #geom_line(aes(y = price_adjusted, color = 'Adjusted Price'), size = 1) +
      geom_line(aes(y = rsi), color = 'black', size = 1, alpha = 0.5) + 
      ylim(0, 100) +
      geom_hline(yintercept = 70, color = 'azure4') +
      geom_hline(yintercept = 30, color = 'azure4') +
      annotate("rect", xmin=as.Date(rownames(data__)[1]), xmax=as.Date(tail(rownames(data__), n=1)), ymin=30, ymax=70, alpha=0.5, fill="azure3") 
      #geom_rect(data=data__, mapping=aes(xmin=as.Date(rownames(data__)[1]), xmax=as.Date(tail(rownames(data__), n=1)), ymin=30, ymax=70), fill="blue", alpha=0.2)
    #scale_color_manual(values = color)
  })
  output$smi.suggest <- renderPlot({
    data__ <- signal(input$single.tick, input$date[1], input$date[2], 'daily', input$multiple.tick, input$small.ema, input$big.ema)
    ann <- annotations(data__)
    ann1 <- data.frame(
      x = ann[['x2']],
      y = ann[['y2']],
      label = ann[['l2']]
    )
    ggplot(data = data__, aes(x = as.Date(rownames(data__)))) +
      geom_line(aes(y = price_adjusted), size = 1) +
      #geom_line(aes(y = EMA, color = "EMA_50"), size = 1 ) + 
      #geom_line(aes(y = SMA, color = 'EMA_200'), size = 1) +
      #scale_color_manual(values = color) +
      geom_label(
        data=ann1, aes( x=as.Date(x), y=y, label=label),                  
        color="red", size = 2.75
      )
  })
  output$smi <- renderPlot({
    data__ <- signal(input$single.tick, input$date[1], input$date[2], 'daily', input$multiple.tick, input$small.ema, input$big.ema)
    color <- c("%K" = 'black', "%D" = 'grey')
    ggplot(data = data__, aes(x = as.Date(rownames(data__)))) +
      geom_line(aes(y = SMI.small, color = "%K"), size = 1) +
      geom_line(aes(y = SMI.big, color = "%D"), size = 0.5) + 
      ylim(-100, 100) + 
      geom_hline(yintercept = -40) +
      geom_hline(yintercept = 40) +
      scale_color_manual(values = color) + 
      annotate("rect", xmin=as.Date(rownames(data__)[1]), xmax=as.Date(tail(rownames(data__), n=1)), ymin=-40, ymax=40, alpha=0.5, fill="azure3") 
  })
  output$ema.suggest <- renderPlot({
    data__ <- signal(input$single.tick, input$date[1], input$date[2], 'daily', input$multiple.tick, input$small.ema, input$big.ema)
    ann <- annotations(data__)
    ann1 <- data.frame(
      x = ann[['x3']],
      y = ann[['y3']],
      label = ann[['l3']]
    )
    #color <- c('Adjusted Price' = 'black', 'EMA_50' = 'blue', 'EMA_200' = 'orange')
    ggplot(data = data__, aes(x = as.Date(rownames(data__)))) +
      geom_line(aes(y = price_adjusted), size = 1) +
      #geom_line(aes(y = EMA, color = "EMA_50"), size = 1 ) + 
      #geom_line(aes(y = SMA, color = 'EMA_200'), size = 1) +
      #scale_color_manual(values = color) +
      geom_label(
        data=ann1, aes( x=as.Date(x), y=y, label=label),                  
        color="red", size = 2.75
      )
  })
  output$ema <- renderPlot({
    data__ <- signal(input$single.tick, input$date[1], input$date[2], 'daily', input$multiple.tick, input$small.ema, input$big.ema)
    color <- c("EMA Short" = 'azure3', "EMA Long" = 'azure4')
    ggplot(data = data__, aes(x = as.Date(rownames(data__)))) +
      #geom_line(aes(y = price_adjusted, color = 'Adjusted Price'), size = 1) +
      geom_line(aes(y = EMA.small, color = "EMA Short"), size = 1 ) + 
      geom_line(aes(y = EMA.big, color = 'EMA Long'), size = 1) +
      scale_color_manual(values = color)
  })
  output$optimise <- DT::renderDataTable({
    data.frame(sr(1000, getdata(input$multiple.tick, input$date[1], input$date[2]))$results)
    #getdata(input$multiple.tick, input$date[1], input$date[2])
  })
  output$weights <- DT::renderDataTable({
    data.frame(sr(1000, getdata(input$multiple.tick, input$date[1], input$date[2]))$weight)
  })
  observe({
    if (input$check.general == FALSE) {
      choice <- c('Portfolio', input$multiple.tick)
    } else {
      choice <- ticker.list
    }
    updateSliderInput(session, "till.date", min = input$date[2])
    updateSliderInput(session, 'big.ema', min = input$small.ema)
    updateSelectInput(
      session,
      "single.tick",
      choices = choice,
      selected = 'AAPL'
    )
  })
  observeEvent(input$reset_button, {
    session$reload()
  })
}

shinyApp(ui, server)


