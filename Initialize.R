
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

PACK = c("ggplot2","xts","RCurl","jsonlite","httr","stringr","plotly","TTR","quantmod","psych")

### Install packages not already installed ###
for (i in 1:length(PACK)){
  if(PACK[i] %in% rownames(installed.packages()) == FALSE) {
    print(PACK[i])
    if (isOpen(url(description = "https://www.google.com"))) install.packages(PACK[i],dependencies = TRUE)}
}

lapply(PACK, require, character.only = TRUE)

source("ROandaAPIFunctions.R")
source("PullPriceData.R")
source("DataCleanup.R")
source("PriceETL.R")
source("Analyses.R")
source("CountFunctions.R")
source("ChartPrices.R")
source("AddIndicators.R")
source("CombiningData.R")
source("ModelPreProcess.R")
source("WeeklyDataArchive.R")


