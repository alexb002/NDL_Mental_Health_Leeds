## NDL Mental Health Leeds
# Exploratory Analysis

# setup -------------------------------------------------------------------
library(ggplot2)
library(purrr)
library(cowplot)
library(tidyr)
library(dplyr)
library(lubridate)
library(plotly)
library(skimr)
library(DBI)
library(odbc)
library(stringr)
library(forcats)
library(plotly)
library(inspectdf)
library(NHSDataDictionaRy)
getwd()

save(list = ls(), file = "NDL_MHL_ExploratoryAnalysis.RData")
#load(file = "NDL_MHL_ExploratoryAnalysis.RData")
