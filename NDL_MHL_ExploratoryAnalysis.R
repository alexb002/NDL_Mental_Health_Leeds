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

# inspect MHSDS data
MH1 %>% inspect_na() %>% show_plot()

MHS101 <- rename(MH1, LSOA = Postcode)
MH_COM2 <- 
  MH_COM1 %>% 
  left_join(., IMD2, by = "LSOA") %>% 
  mutate(Attendance_Date = as_date(Attendance_Date),
         Attendance_Year = year(as_date(Attendance_Date)),
         Arrival_Time = str_remove(Arrival_Time, "1900-01-01 ") %>% hms(.),
         #         Referral_Type = str_remove(Referral_Type, "")
         Age_Group = dplyr::case_when(
           Age_At_Start_Of_Episode < 30 ~ "<30",
           Age_At_Start_Of_Episode >= 30 & Age_At_Start_Of_Episode <= 49 ~ "30-49",
           Age_At_Start_Of_Episode >= 50 & Age_At_Start_Of_Episode <= 69 ~ "50-69",
           Age_At_Start_Of_Episode >= 70 ~ "70+",
           is.na(Age_At_Start_Of_Episode) ~ NA_character_,
           TRUE ~ as.character(Age_At_Start_Of_Episode)
         ),
         sex = fct_explicit_na(factor(Sex, levels = c("Female", "Male")), na_level = "Unknown/other"),
         Age_Group = fct_explicit_na(factor(Age_Group, levels = c("<30", "30-49", "50-69", "70+"))
                                     , na_level = "Unknown"),
         IMD_Quintile = fct_explicit_na(factor(IMD_Quintile, levels = c("1", "2", "3", "4", "5")),
                                        na_level = "Unknown"),
         IMD_Decile = fct_explicit_na(factor(IMD_Decile, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")),
                                      na_level = "Unknown")
  )
         
MH_COM2_insp <- 
 MH_COM2 %>% 
 select(where(is.character), -Spell_ID, -NHS_Number) %>% 
 inspect_cat()

MH_COM2 %>% 
 select(where(is.double)) %>% 
 inspect_num()

MH_COM2 %>% 
 select(-Spell_ID, -NHS_Number) %>% 
 inspect_na()

range(MH_COM2$Attendance_Date)

names(MH_COM2)
glimpse(MH_COM2)

MH_COM2 %>% 
 ggplot(aes(x = Age_At_Start_Of_Episode, fill = Sex)) +
 geom_histogram(binwidth = 1) +
 scale_fill_viridis_d() +
 facet_wrap(~year(Attendance_Date)) +
 theme_bw()

vars1 <- MH_COM2_insp %>% dplyr::filter(cnt < 30) %>% pull(col_name)


