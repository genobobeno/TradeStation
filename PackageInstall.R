
####Get Environment ready for OandaR

Pkg <- c("base","downloader","forecast","httr","jsonlite","lubridate","moments",
         "PerformanceAnalytics","quantmod","reshape2","RCurl","stats","scales","tseries",
         "TTR","TSA","xts","zoo")

inst <- Pkg %in% installed.packages()
if(length(Pkg[!inst]) > 0) install.packages(Pkg[!inst])
instpackages <- lapply(Pkg, library, character.only=TRUE)


#Source GitHub Oanda API Functions
downloader::source_url("http://bit.ly/GitHubROandaAPI",prompt=FALSE,quiet=TRUE)
