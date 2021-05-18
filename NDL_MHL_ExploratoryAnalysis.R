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


# load --------------------------------------------------------------------
## MHSDS dataset - see schema

## load IMD
IMD <- read.csv(file = "/Documents and Settings/BrownriggA01/Documents/R/HF_NHSI_R/NDL_IMD_LSOA_2019.csv", 
                header = T, stringsAsFactors = F) %>% 
  dplyr::select(lsoa11cd, lsoa11nm, IMDDecil)


# reformat ----------------------------------------------------------------
IMD2 <- 
  IMD %>% dplyr::mutate(
    IMDDecil = as.character(IMDDecil), 
    IMDQuintil = case_when(
      IMDDecil %in% 1:2 ~ "1",
      IMDDecil %in% 3:4 ~ "2",
      IMDDecil %in% 5:6 ~ "3",
      IMDDecil %in% 7:8 ~ "4",
      IMDDecil %in% 9:10 ~ "5",
      IMDDecil == 0 ~ NA_character_,
      TRUE ~ as.character(IMDDecil)
    )
  )

# rename
IMD2 <- dplyr::rename(IMD2, LSOA = lsoa11cd, LSOA_nm = lsoa11nm, 
                      IMD_Decile = IMDDecil, IMD_Quintile = IMDQuintil)

# inspect -----------------------------------------------------------------
glimpse(IMD2)

IMD2 %>% 
  mutate(IMD_Decile = as.numeric(IMD_Decile)) %>% 
  ggplot() +
  geom_histogram(aes(x = IMD_Decile), binwidth = 1, color = "white") +
  theme_bw()
