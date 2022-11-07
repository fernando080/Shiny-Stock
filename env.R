library(DT)
library(ggplot2)
library(plotly)
library(leaflet)
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)
library(quantmod)
library(forecast)
library(xlsx)
library(tseries)
library(timeSeries)
library(dplyr)
library(fGarch)
library(prophet)
library(gridExtra)
library(httr)
library(rnaturalearth)
library(sp)
library(leaflet)

var_texto = "
This shiny app will help you with your technical analysis of the stock
marquet. You can load you data in the 'Try with your company data', or you
can see how these app work in 'Own stock data'
"
var_texto_mitfm = "
In this application we will find an aid for the prediction of the opening and 
closing data in the stock market using temporary data models. We can upload our 
own in the appropriate format or see some examples of companies chosen by the 
author
"

var_err_data_format <- "
Data must have the next columns Date, Open, High, Low, Close, Adj Close, Volume. 
In adition, is not necessary but the data should be numeric and do not exists 
null values.
"

currTime <- format(Sys.time(), "_%H_%M_%S")

GDatasets = c("TSLA.csv","bitcoinUSD.csv","CrudeOil.csv","gold.csv","Nasdaq.csv")


# Colname format
colNameFormat0 <- c("Date","Open","High","Low","Close","Adj Close","Volume")
colNameFormat1 <- c("Date","Open","High","Low","Close","Adj Close","Volume")
colNameFormat2 <- c("Date","Open","High","Low","Close","Adj.Close","Volume")


# Read data
weather <- read.csv("www/data/wheater.csv")
weather$play <- as.factor(weather$play)
weather$outlook <- factor(weather$outlook, levels = c("overcast", "rainy", "sunny"))

# Build model
model <- randomForest(play ~ ., data = weather, ntree = 500, mtry = 4, importance = TRUE)


# Load documentation 
fileName <- 'www/doc.txt'
doc <- readChar(fileName, file.info(fileName)$size)

       
# Save model to RDS file
# saveRDS(model, "model.rds")

# Read in the RF model
#model <- readRDS("model.rds")
# for (i in GDatasets){
#   inputData <- paste('www/data/', i, sep = "")
#   dataset <- read.csv(inputData, header = TRUE)
#   dataset[,2:6] <- sapply(dataset[,2:6], as.numeric)
#   write.csv(dataset, file = inputData)
# }

