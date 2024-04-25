# set up
library(tidyverse)

# read in time series file
orthodontic_data <- read.csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/BSA_published_data/time_series_orthodontic_activity.csv")

# read in contract to ICB mapping table
source("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/BSA_published_data/icb_to_region_mapping.R")

# reformat month and join tables to add region column and map all months to ICBs
orthodontic_data_formatted <- orthodontic_data %>% 
  mutate(YEAR_MONTH = as.Date(paste(substr(YEAR_MONTH, 1, 4), substr(YEAR_MONTH, 5, 6), "01", sep = "-"))) %>% 
  left_join(extract, by = c("COMMISSIONER_CODE" = "STP_Code")) %>% 
  mutate(COMMISSIONER_NAME = ifelse(is.na(Region_Name), COMMISSIONER_NAME, STP_Name)) %>% 
  select(-STP_Name) %>% 
  rename("REGION_NAME" = Region_Name, 
         "REGION_CODE" = Region_Code) %>% 
  select(YEAR_MONTH, COMMISSIONER_NAME, COMMISSIONER_CODE, REGION_NAME, REGION_CODE, everything())

# save out as csv
write.csv(orthodontic_data_formatted, 
          "N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/BSA_published_data/time_series_orthodontic_formatted.csv", 
          row.names = FALSE)
