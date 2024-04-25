# set up
library(tidyverse)

# read in time series file
dental_data <- read.csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/BSA_published_data/time_series_dental_activity.csv")

# read in contract to ICB mapping table
source("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/BSA_published_data/icb_to_region_mapping.R")

# reformat month then join tables to add region column and map all months to ICBs
dental_data_formatted <- dental_data %>% 
  mutate(YEAR_MONTH = as.Date(paste(substr(YEAR_MONTH, 1, 4), substr(YEAR_MONTH, 5, 6), "01", sep = "-"))) %>% 
  left_join(extract, by = c("COMMISSIONER_CODE" = "STP_Code")) %>% 
  mutate(COMMISSIONER_NAME = ifelse(is.na(Region_Name), COMMISSIONER_NAME, STP_Name)) %>% 
  select(-STP_Name) %>% 
  rename("REGION_NAME" = Region_Name, 
         "REGION_CODE" = Region_Code) %>% 
  select(YEAR_MONTH, COMMISSIONER_NAME, COMMISSIONER_CODE, REGION_NAME, REGION_CODE, everything())

# add derived fields
# calculates UDAs delivered from FP17s
dental_data_formatted <- dental_data_formatted %>% 
  rowwise() %>% 
  mutate(UDA_BAND_1_DELIVERED = BAND_1_DELIVERED*1, 
         UDA_BAND_2_DELIVERED = BAND_2_DELIVERED*3, 
         UDA_BAND_2A_DELIVERED = BAND_2A_DELIVERED*3, 
         UDA_BAND_2B_DELIVERED = BAND_2B_DELIVERED*5, 
         UDA_BAND_2C_DELIVERED = BAND_2C_DELIVERED*7, 
         UDA_BAND_3_DELIVERED = BAND_3_DELIVERED*12, 
         UDA_BAND_URGENT_DELIVERED = BAND_URGENT_DELIVERED*1.2, 
         UDA_BAND_OTHER_DELIVERED = as.integer(UDA_DELIVERED - sum(UDA_BAND_1_DELIVERED, 
                                                                  UDA_BAND_2_DELIVERED, 
                                                                  UDA_BAND_2A_DELIVERED, 
                                                                  UDA_BAND_2B_DELIVERED, 
                                                                  UDA_BAND_2C_DELIVERED, 
                                                                  UDA_BAND_3_DELIVERED, 
                                                                  UDA_BAND_URGENT_DELIVERED, 
                                                                  na.rm = TRUE)))

# save out as csv
write.csv(dental_data_formatted, 
          "N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/BSA_published_data/time_series_dental_formatted.csv", 
          row.names = FALSE)
