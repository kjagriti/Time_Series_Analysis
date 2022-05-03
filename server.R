server <- function(input, output) {

  df_ma = read.csv("mastercard.csv")
  df_tsla = read.csv("tesla.csv")
  df_aapl = read.csv("apple.csv")
  df_amzn = read.csv("amazon.csv")
  
  imputed_data <- reactive({
    
# Removing $ sign
    
    df_ma$Close = gsub("\\$","", df_ma$Close)
    df_ma$Open = gsub("\\$","", df_ma$Open)
    df_ma$High = gsub("\\$","", df_ma$High)
    df_ma$Low = gsub("\\$","", df_ma$Low)
    
    df_aapl$Close = gsub("\\$","", df_aapl$Close)
    df_aapl$Open = gsub("\\$","", df_aapl$Open)
    df_aapl$High = gsub("\\$","", df_aapl$High)
    df_aapl$Low = gsub("\\$","", df_aapl$Low)
    
    df_amzn$Close = gsub("\\$","", df_amzn$Close)
    df_amzn$Open = gsub("\\$","", df_amzn$Open)
    df_amzn$High = gsub("\\$","", df_amzn$High)
    df_amzn$Low = gsub("\\$","", df_amzn$Low)
    
    df_tsla$Close = gsub("\\$","", df_tsla$Close)
    df_tsla$Open = gsub("\\$","", df_tsla$Open)
    df_tsla$High = gsub("\\$","", df_tsla$High)
    df_tsla$Low = gsub("\\$","", df_tsla$Low)
    
# Removing comma from digit
    
    df_ma$Close = as.double(gsub(",","",df_ma$Close))
    df_ma$Open = as.double(gsub(",","",df_ma$Open))
    df_ma$High = as.double(gsub(",","",df_ma$High))
    df_ma$Low = as.double(gsub(",","",df_ma$Low))
    
    df_aapl$Close = as.double(gsub(",","",df_aapl$Close))
    df_aapl$Open = as.double(gsub(",","",df_aapl$Open))
    df_aapl$High = as.double(gsub(",","",df_aapl$High))
    df_aapl$Low = as.double(gsub(",","",df_aapl$Low))
    
    df_amzn$Close = as.double(gsub(",","",df_amzn$Close))
    df_amzn$Open = as.double(gsub(",","",df_amzn$Open))
    df_amzn$High = as.double(gsub(",","",df_amzn$High))
    df_amzn$Low = as.double(gsub(",","",df_amzn$Low))
    
    df_tsla$Close = as.double(gsub(",","",df_tsla$Close))
    df_tsla$Open = as.double(gsub(",","",df_tsla$Open))
    df_tsla$High = as.double(gsub(",","",df_tsla$High))
    df_tsla$Low = as.double(gsub(",","",df_tsla$Low))
    
# Formatting Date
    
    df_ma <- df_ma %>%
      mutate(Date = ymd(anytime(Date)))
    
    df_aapl <- df_aapl %>%
      mutate(Date = ymd(anytime(Date)))
    
    df_amzn <- df_amzn %>%
      mutate(Date = ymd(anytime(Date)))
    
    df_tsla <- df_tsla %>%
      mutate(Date = ymd(anytime(Date)))
    
# Filling gaps in Date
    
    df_ma <- df_ma %>%
      as_tsibble(index = Date)
    df_ma <- df_ma %>%
      fill_gaps()
    
    df_aapl <- df_aapl %>%
      as_tsibble(index = Date)
    df_aapl <- df_aapl %>%
      fill_gaps()
    
    df_amzn <- df_amzn %>%
      as_tsibble(index = Date)
    df_amzn <- df_amzn %>%
      fill_gaps()
    
    df_tsla <- df_tsla %>%
      as_tsibble(index = Date)
    df_tsla <- df_tsla %>%
      fill_gaps()
    
# Imputing values using Last Observation carried forward
    
    df_ma$Close <- imputeTS::na_locf(imputeTS::na_locf(df_ma$Close,option = "locf"),option="locf")
    df_ma$Open <- imputeTS::na_locf(imputeTS::na_locf(df_ma$Open,option = "locf"),option="locf")
    df_ma$High <- imputeTS::na_locf(imputeTS::na_locf(df_ma$High,option = "locf"),option="locf")
    df_ma$Low <- imputeTS::na_locf(imputeTS::na_locf(df_ma$Low,option = "locf"),option="locf")
    
    df_aapl$Close <- imputeTS::na_locf(imputeTS::na_locf(df_aapl$Close,option = "locf"),option="locf")
    df_aapl$Open <- imputeTS::na_locf(imputeTS::na_locf(df_aapl$Open,option = "locf"),option="locf")
    df_aapl$High <- imputeTS::na_locf(imputeTS::na_locf(df_aapl$High,option = "locf"),option="locf")
    df_aapl$Low <- imputeTS::na_locf(imputeTS::na_locf(df_aapl$Low,option = "locf"),option="locf")
    
    df_amzn$Close <- imputeTS::na_locf(imputeTS::na_locf(df_amzn$Close,option = "locf"),option="locf")
    df_amzn$Open <- imputeTS::na_locf(imputeTS::na_locf(df_amzn$Open,option = "locf"),option="locf")
    df_amzn$High <- imputeTS::na_locf(imputeTS::na_locf(df_amzn$High,option = "locf"),option="locf")
    df_amzn$Low <- imputeTS::na_locf(imputeTS::na_locf(df_amzn$Low,option = "locf"),option="locf")
    
    df_tsla$Close <- imputeTS::na_locf(imputeTS::na_locf(df_tsla$Close,option = "locf"),option="locf")
    df_tsla$Open <- imputeTS::na_locf(imputeTS::na_locf(df_tsla$Open,option = "locf"),option="locf")
    df_tsla$High <- imputeTS::na_locf(imputeTS::na_locf(df_tsla$High,option = "locf"),option="locf")
    df_tsla$Low <- imputeTS::na_locf(imputeTS::na_locf(df_tsla$Low,option = "locf"),option="locf")
    
# Filling NA's
    
    df_ma$Volume[is.na(df_ma$Volume)]<-0
    df_aapl$Volume[is.na(df_aapl$Volume)]<-0
    df_amzn$Volume[is.na(df_amzn$Volume)]<-0
    df_tsla$Volume[is.na(df_tsla$Volume)]<-0
    
    df_ma$stock[is.na(df_ma$stock)]<-"MA"
    df_aapl$stock[is.na(df_aapl$stock)]<-"AAPL"
    df_amzn$stock[is.na(df_amzn$stock)]<-"AMZN"
    df_tsla$stock[is.na(df_tsla$stock)]<-"TSLA"
    
# Binding tsibble data into one dataframe
    
    data <- rbind(as.data.frame(df_ma), as.data.frame(df_aapl), as.data.frame(df_amzn), as.data.frame(df_tsla))
    
  })
  
# Trend Charts
  
  output$Daily_trend_chart <- renderPlot({ imputed_data() %>%
      filter(stock == input$stock) %>%
      filter(as.Date(Date) >= input$dates[1], as.Date(Date) <= input$dates[2]) %>%
      ggplot(aes(x = Date, y = Close)) + geom_line() + labs(title = "Trend chart for selected time period")
  })
  
  
  # Decompsoition
  
  output$daily_decomposition <- renderPlot({
    imputed_data() %>%
      filter(stock == input$stock) %>%
      filter(as.Date(Date) >= input$dates[1], as.Date(Date) <= input$dates[2]) %>%
      as_tsibble() %>%
      model(
        STL(Close ~ trend(window = 7) +
              season(window = "periodic"),
            robust = TRUE)) %>%
      components() %>%
      autoplot() + labs(title = "Daily STL Decomposition")
  })
  

  
  # Moving Average    
  
  output$daily_moving_average <- renderPlot({
    ts_ma <- imputed_data() %>%
      filter(stock == input$stock) %>%
      filter(as.Date(Date) >= input$dates[1], as.Date(Date) <= input$dates[2]) %>%
      as_tsibble() %>%
      mutate(
        `5-MA` = slider::slide_dbl(Close, mean,
                                   .before = 5, .after = 5, .complete = TRUE)
      )
    ts_ma %>%
      autoplot(Close) +
      geom_line(aes(y = `5-MA`), colour = "red") +
      labs(y = "Closing price",
           title = "5 Day Moving Average for Stock Price in selected time period") +
      guides(colour = guide_legend(title = "series"))
  })
  
  
  # Forecasting    
  
  output$forecast <- renderPlot({
  train_ts_date <- imputed_data() %>%
    filter(stock == input$stock)%>%
    filter(as.Date(Date) >= input$dates[1], as.Date(Date) <= input$dates[2]) %>%
    as_tsibble()
  
  train_ts_date_fit <- train_ts_date %>%
    model(
      Mean = MEAN(Close),
      `Naïve` = NAIVE(Close),
      `Seasonal naïve` = SNAIVE(Close),
      Drift = NAIVE(Close ~ drift())
    )
    
  train_ts_data_fc <- train_ts_date_fit %>%
    forecast(h = input$forecast_input)
  
  train_ts_data_fc %>%
    autoplot(train_ts_date,level = NULL) +
    labs(
      y = "$US",
      title = "Forecast for the Stock Selected"
    ) +
    guides(colour = guide_legend(title = "Forecast"))
  
  })
  

  
  
  
  # Summary Table
  
  summary_table <- function(){
    temp <- imputed_data() %>%
      filter(stock == input$stock) %>%
      filter(as.Date(Date) >= input$dates[1], as.Date(Date) <= input$dates[2])
  }
  
  output$summary <- renderTable({
    imputed_data() %>%
      filter(stock == input$stock) %>%
      filter(as.Date(Date) >= input$dates[1], as.Date(Date) <= input$dates[2]) %>%
      summary()
  })

  
  
  
}