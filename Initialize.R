
# -- Rquired Packages in order to use the R API
#library("downloader")
if (Sys.getenv("COMPUTERNAME")=="D2X8CJR2") {
  setwd("C:/Dev/MyFiles/Nextcloud/Documents/Trading/TradeStation/")
} else if (Sys.getenv("COMPUTERNAME")=="DESKTOP-3S9QUS6") {
  setwd("D:/Documents/Trading/TradeStation")
} else {
  setwd("C:/Users/Eugene/Nextcloud/Documents/Trading/TradeStation/")
}
options(stringsAsFactors = FALSE,scipen=999)
library(ggplot2)
library(xts)
#library(dygraphs)
library("RCurl")
library("jsonlite")
library("httr")
library("stringr")
library("plotly")
library("TTR")
library("quantmod")
library("psych")
source("ROandaAPIFunctions.R")
source("PullPriceData.R")
source("DataCleanup.R")
source("Analyses.R")
source("CountFunctions.R")
source("ChartPrices.R")
source("AddIndicators.R")
source("CombiningData.R")
source("ModelPreProcess.R")

