#setwd(".\shiny\Stock_prediction")
path = "env.R"
source(path)

# renderLeaflet: map -----
#output$map <- renderLeaflet({
#  cual = which(input$SelLugar01==GLugares)
#  LAT = GLatLong$Lat[cual]
#  LONG = GLatLong$Long[cual]
#  ZOOM=18
  # Dibuja el mapa!
#  leaflet() %> %
#    setView(lng=LONG, lat=LAT, zoom=ZOOM ) %> %
#    addProviderTiles("OpenStreetMap.Mapnik")
#})



server <- function(input, output, session)  {
  
  # File upload
  observeEvent(input$file1,{
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = isolate(input$HaveHeader),
                   sep = isolate(input$SepChar))
    print(df)
    dataColNames <- colnames(df)
    print(input$file1)
    print(dataColNames)
    print(class(dataColNames))
    print(colNameFormat)
    print(!all(dataColNames != colNameFormat))
    
    # Show modal if data format is not correct
    if (!(all(dataColNames == colNameFormat0) | all(dataColNames == colNameFormat1) | all(dataColNames == colNameFormat2))){
      showModal(modalDialog(
              title = "Important message",
              var_err_data_format,
              footer = modalButton("Close"),
            ))
    } else {
      name <- paste("www/data/updated_data", currTime, ".csv", sep = "")
      write.csv(df,file = name, quote = FALSE, col.names = dataColNames, row.names = FALSE)
      shiny::updateSelectInput(session,"Iddataselect","Selecciona un dataset",
                               choice = list.files('www/data/'),
                               selected = paste("updated_data", currTime, ".csv", sep = ""))
    }
  })
  
  
  datasetInputStock <- reactive({
    name <- input$Iddataselect
    var_selec <- c(1,2,5)
    inputData <- paste('www/data/', name, sep = "")
    dataset <- read.csv(inputData)
    dataset[,2:7] <- sapply(dataset[,2:7], as.numeric)
    dataset <- dataset[!(is.null(dataset[,2]) | is.na(dataset[,2]) | dataset[,2] =="null"), ]
    dataset <- dataset[var_selec]
    colnames(dataset) <- c('date', 'open', 'close')
    dataset
  })
  
  datasetFullInputStock <- reactive({
    var_selec <- c(1,2,3,4,5,7)
    inputData <- paste('www/data/', input$Iddataselect, sep = "")
    datasetFull <- read.csv(inputData)
    datasetFull[,2:6] <- sapply(datasetFull[,2:7], as.numeric)
    datasetFull <- datasetFull[!(is.null(datasetFull[,2]) | is.na(datasetFull[,2]) | datasetFull[,2] =="null"), ]
    datasetFull <- datasetFull[var_selec]
    colnames(datasetFull) <- c('date', 'open', 'high', 'low', 'close', 'volume')
    as.data.frame(datasetFull)
  })
  
  datasetSumStock <-  reactive({
    var_selec <- c(1,2,5)
    inputData <- paste('www/data/', input$Iddataselect, sep = "")
    dataset <- read.csv(inputData)
    dataset[,2:6] <- sapply(dataset[,2:6], as.numeric)
    dataset <- dataset[!(is.null(dataset[,2]) | is.na(dataset[,2]) | dataset[,2] =="null"), ]
    dataset <- dataset[var_selec]
    datasetSumStock <- head(datasetInputStock(), n = input$obs)
    datasetSumStock
  })
  
  datasetWinnersStock <- reactive({
    new_stock <- function(data){
      result = data.frame()
      
      for(item in data$quotes){
        #print(item.keys())
        dispname = item$displayName
        name = item$shortName
        price = item$regularMarketPrice
        open = item$regularMarketOpen
        percent = item$regularMarketChangePercent
        
        result <- rbind(result, c(dispname, name, price, open, percent))}
      
      colnames(result) <- c('disname', 'name', 'price', 'open', 'percent')
      return(result)
    }
    # --- main ---
    
    url <- "https://yahoo-finance15.p.rapidapi.com/api/yahoo/ga/topgainers"
    
    querystring = list(start = "0")
    
    headers = list(
      x_rapidapi_host = "yahoo-finance15.p.rapidapi.com",
      x_rapidapi_key = "9efd0f3e52mshd859f5daf34a429p11cb2ajsn2b0e421d681e"
    )
    
    # response = requests.request("GET", url, headers=headers, params=querystring)
    response <- VERB("GET", url, add_headers("x-rapidapi-host" = "yahoo-finance15.p.rapidapi.com",
                                             "x-rapidapi-key" = "9efd0f3e52mshd859f5daf34a429p11cb2ajsn2b0e421d681e"),
                     query = querystring, content_type("application/octet-stream"))
    
    data <- content(response, "parsed")
    res <- new_stock(data)
  })
  
  histOpen <- reactive({
    pOpen <- plot_ly(
      x = datasetInputStock()[,2],
      type = 'histogram',
      marker = list(color = "orange",
                    line = list(color = "black",
                                width = 2))
    )
  })
  
  histClose <- reactive({
    pClose <- plot_ly(
      x = datasetInputStock()[,3],
      type = 'histogram',
      marker = list(color = "orange",
                    line = list(color = "black",
                                width = 2))
    )
  })
  
  plotOpen <- reactive({
    data <- datasetInputStock()
    dateG <- as.Date(as.yearmon(as.Date(data$date)))
    ggplot(data,aes(x=dateG, y=open, group=1)) +
      geom_point(shape=21, color="black", fill="#69b3a2", size=2) +
      geom_line() +
      ggtitle("Index Open") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_breaks = "2 years")
  })
  
  plotClose <- reactive({
    data <- datasetInputStock()
    dateG <- as.Date(as.yearmon(as.Date(data$date)))
    ggplot(data,aes(x=dateG, y=close, group=1)) +
      geom_point(shape=21, color="black", fill="#69b3a2", size=2) +
      geom_line() +
      ggtitle("Index Close") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_breaks = "2 years")
  })
  
  openTS <- reactive({
    data <- datasetInputStock()[,c(1,2)]
    data_ts <- ts(data[,2])
  })
  
  closeTS <- reactive({
    data <- datasetInputStock()[,c(1,3)]
    data_ts <- ts(data[,2])
  })
  
  plotClose <- reactive({
    data <- datasetInputStock()
    dateG <- as.Date(as.yearmon(as.Date(data$date)))
    ggplot(data,aes(x=dateG, y=close, group=1)) +
      geom_point(shape=21, color="black", fill="#69b3a2", size=2) +
      geom_line() +
      ggtitle("Index Close") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_breaks = "2 years")
  })
  
  ADFtest <- reactive({
    data <- datasetInputStock()
    testClose <- adf.test(data$close) 
  })
  
  
  resAutoArimaClose <- eventReactive(input$autoArima,{
    data <- closeTS()
  })
  
  shapiro <- eventReactive(input$autoArima,{
    data <- closeTS()
  })
  
  # Input Data
  datasetInput <- reactive({  
    
    # outlook,temperature,humidity,windy,play
    df <- data.frame(
      Name = c("outlook",
               "temperature",
               "humidity",
               "windy"),
      Value = as.character(c(input$outlook,
                             input$temperature,
                             input$humidity,
                             input$windy)),
      stringsAsFactors = FALSE)
    
    play <- "play"
    df <- rbind(df, play)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    test$outlook <- factor(test$outlook, levels = c("overcast", "rainy", "sunny"))
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  wGDPdataMap <- reactive({
    world <- ne_countries(type = "countries",  returnclass = 'sf')
    
    # Take a peek at the name, gdp_md_est column and economy columns. 
    # The same way we would peek at any data frame.
    head(world[c('name', 'gdp_md_est', 'economy')], n = 6)
    
    # Create shading by GDP. Let's go with purples.
    gdpPal <- colorQuantile("YlGnBu", world$gdp_md_est, n = 20)
    
    # Create popup country name and income group so that something happens
    # when a user clicks the map.
    popup <- paste0("<strong>Country: </strong>", 
                    world$name,  
                    "<br><strong>GDP: </strong>", 
                    world$gdp_md_est,
                    "<br><strong>Market Stage: </strong>", 
                    world$income_grp)
    
    leaf_world <- leaflet(world) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      setView(lng =  20, lat =  15, zoom = 2) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = .7, 
                  
                  # Note the layer ID. Not a country name! It's a country code! 
                  
                  color = ~gdpPal(gdp_md_est), layerId = ~iso_a3, popup = popup)%>%
      leaflet::addLegend("bottomright", pal = gdpPal, values = ~gdp_md_est,
                         title = "Est. GDP",
                         labFormat = labelFormat(prefix = "$"),
                         opacity = 1
      )
    
  })
  
  points <- eventReactive(input$recalc, {
    point = cbind(input$lat, input$long)
    print(point)
  }, ignoreNULL = FALSE)
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint(
    summary(datasetInputStock())
  )
  
  # Show the first "n" observations ----
  output$view <- DT::renderDataTable(
    DT::datatable(
      datasetSumStock(),
      rownames = FALSE,
      options = list(dom='t',
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#2B3E50', 'color': 'white'});",
                       "}")
                    ), 
      container = tags$table(class="stripe row-border hover",
                             tags$thead(tags$tr(lapply(colnames(datasetSumStock()), tags$th)))
                            )
    ) %>% formatStyle(columns=colnames(datasetSumStock()),color='white',background = '#2B3E50',target = 'row')
  )
  
  output$histoOpen <- renderPlotly(histOpen()%>%
     layout(title = "Histogram of Open Prices",
       xaxis = list(range = c(datasetInputStock()[,2] - 0.1*min(datasetInputStock()[,2]),datasetInputStock()[,2] + 0.1*max(datasetInputStock()[,2])))
     )
  )
  
  output$histoClose <- renderPlotly(histClose()%>%
     layout(title = "Histogram of Close Prices",
       xaxis = list(range = c(datasetInputStock()[,3] - 0.1*min(datasetInputStock()[,3]),datasetInputStock()[,3] + 0.1*max(datasetInputStock()[,3])))
     )
  )
  
  output$plotOpen <- renderPlot(plotOpen())
  
  output$plotClose <- renderPlot(plotClose())

  
  output$plot1 <- renderPlotly({
    p1 <- datasetFullInputStock() %>%
      plot_ly(x = ~date,
              type = "candlestick", 
              open = ~open, 
              close = ~close,  
              high = ~high,
              low = ~low,
              name = "price") %>%
        layout(
          xaxis = list(rangeslider = list(visible = FALSE)),
          yaxis = list(title = "Price ($)",
                       showgrid = TRUE,
                       showticklabels = TRUE))
    
      p2 <- datasetFullInputStock() %>%
        plot_ly(x=~date, y=~volume, type='bar', name = "Volume") %>%
        layout(yaxis = list(title = "Volume"))
      
      plot1 <- subplot(p1, p2, heights = c(0.7,0.3), nrows=2,
                       shareX = TRUE, titleY = TRUE) %>%
        layout(title = 'Histórico')
      plot1
  })
  
  output$ADFtest <- renderPrint( 
    ADFtest()$p.value
  )
  
  output$ADFplot <- renderPlot({
      par(mfrow=c(1,2))
      acf(closeTS())
      pacf(closeTS())
    }
  )
  
  output$resAutoAirima <- renderPrint({
    modelfit <- auto.arima(resAutoArimaClose(), lambda = "auto")
    summary(modelfit)
  }
  )
  
  output$plotResAutoAirima <- renderPlot({
    model <- auto.arima(resAutoArimaClose(), lambda = "auto")
    
    plot1 <- ggplot(model$residuals, aes(x = x, y = model$residuals)) +
                geom_line(size = 0.01, col = "#2AA27F")
    plot2 <- ggplot(model$residuals, aes(x = y, y = ..density..)) +
                geom_histogram(color="darkblue", fill="lightblue") +
                geom_density(alpha=.4, fill="#A22A83", size = 1, col = "#8B2AA2")
    grid.arrange(plot1 + labs(title= "Residuals", y="Residuals", x = "Time") + 
                          theme(plot.title = element_text(hjust = 0.5, color="#2B3E50", size=14, face="bold.italic"),
                                axis.title.x = element_text(color="#8D0951", size=12, face="bold"),
                                axis.title.y = element_text(color="#8D0951", size=12, face="bold")), 
                 plot2 + labs(title= "Density", y="Density", x = "Residuals") + 
                          theme(plot.title = element_text(hjust = 0.5, color="#2B3E50", size=14, face="bold.italic"),
                                axis.title.x = element_text(color="#8D0951", size=12, face="bold"),
                                axis.title.y = element_text(color="#8D0951", size=12, face="bold")), ncol=2)
  }
  )
  
  output$Shapiro_test <- renderPrint({
    model <- auto.arima(resAutoArimaClose(), lambda = "auto")
    shapiro.test(model$residuals)$p.value
  })
  
  output$Ljung_test <- renderPrint({
    model <- auto.arima(resAutoArimaClose(), lambda = "auto")
    Box.test(model$residuals, type = "Ljung")$p.value
  })
  
  output$plotTsdiag <- renderPlot({
    model <- auto.arima(resAutoArimaClose(), lambda = "auto")
    
    tsdiag(model)
  }
  )
  
  output$plotResults <- renderPlot({
    model <- auto.arima(resAutoArimaClose(), lambda = "auto")
    
    price_forecast <- forecast(model, h=100)
    plot(price_forecast)
  }
  )
  
  # Show winners of the day ----
  output$WinWin <- DT::renderDataTable(
    DT::datatable(datasetWinnersStock(),
                  rownames = FALSE,
                  options = list(dom='t',
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#2B3E50', 'color': 'white'});",
                                   "}")
                  ), 
                  container = tags$table(class="stripe row-border hover",
                                         tags$thead(tags$tr(lapply(colnames(datasetWinnersStock()), tags$th)))
                  )
    ) %>% formatStyle(columns=colnames(datasetWinnersStock()),color='white',background = '#2B3E50',target = 'row')
  )
  
  output$mymap <- renderLeaflet({
    wGDPdataMap()
  })
  
  output$mymap2 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
    
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
  observeEvent(input$SelDataset, {
    if (input$SelDataset==GDatasets[1]) {
      Gdatos$datos = read.csv("advertising.csv")
      Gdatos$Nombres = c("Id","TVPubl","RadioPubl","PeriodicosPubl","Ventas")
      Gdatos$var_selec = c(2:5)
    }
    
    # este sería código necesario
    shiny::updateSelectInput(session,"Selvariable01Uni","Selecciona variable",
                             choices = Gdatos$Nombres[Gdatos$var_selec],
                             selected = Gdatos$Nombres[Gdatos$var_selec[1]])
    shiny::updateSelectInput(session,"Selvariable01Bid","Selecciona variable dependiente (Y)",
                             choices = Gdatos$Nombres[Gdatos$var_selec],
                             selected = Gdatos$Nombres[Gdatos$var_selec[1]])
  })
}
