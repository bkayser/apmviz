
library(ggplot2)
library(dplyr)
library(stringi)
library(data.table)
library(lubridate)
library(scales)
source('utils/utils.R')
source('utils/load_data.R')
source('utils/themes.R')
source('utils/plots.R')
source('utils/interactive.R')
source('utils/horizon.R')


knitr::opts_chunk$set(fig.width=9, fig.height=4.5,
                      echo=F, 
                      cache=T,
                      warning=F, 
                      message=F)


if (!exists('cvb')) {
    cvb <- data.frame(Name=vector(), Efficiency=vector(), Value=vector(), Notice=vector(), stringsAsFactors = F)
}

if (!exists('ex1')) {
#  ex1 <- read_transactions('./data/raw/short-tall-spike.rds', 'backend')  
#    ex1 <- read_transactions('./data/raw/staging_transactions_56.rds', 'db')
    microservice <- read_transactions('./data/timeseries/microservice.rds', 'duration')
} 

if (!exists('app_group')) {
    app_group <- list()
    app_group[[1]] <- read_transactions('./data/timeseries/by-client.rds', 'frontend', apdex_t=9000)
    app_group[[2]] <- read_transactions('./data/timeseries/news-site.rds', 'frontend', apdex_t=9000)
    app_group[[3]] <- read_transactions('./data/timeseries/storefront-deploy.rds', 'frontend')
    app_group[[4]] <- read_transactions('./data/timeseries/supplychain.rds', 'frontend')
    app_group[[5]] <- read_transactions('./data/timeseries/rpm.rds', 'frontend')
}
