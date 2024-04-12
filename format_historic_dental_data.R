# set up
library(tidyverse)

# read in time series file
dental_data <- read.csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/BSA_published_data/time_series_dental_activity.csv")

# read in contract to ICB mapping table
source("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/contract_to_icb_mapping.R")

# join tables to add region column and map all months to ICBs
dental_data_formatted <- dental_data %>% 
  left_join(extract, by = c("CONTRACT_NUMBER" = "contract_number")) %>% 
  select(-c(COMMISSIONER_CODE, COMMISSIONER_NAME)) %>% 
  rename("COMMISSIONER_NAME" = commissioner_name, 
         "COMMISSIONER_CODE" = commissioner_ods_code_icb, 
         "REGION_NAME" = region_name, 
         "REGION_CODE" = region_code) %>% 
  select(YEAR_MONTH, COMMISSIONER_NAME, COMMISSIONER_CODE, REGION_NAME, REGION_CODE, everything())

# add derived fields
# reformat month and calculates UDAs delivered from FP17s
dental_data_formatted <- dental_data %>% 
  rowwise() %>% 
  mutate(YEAR_MONTH = as.Date(paste(substr(YEAR_MONTH, 1, 4), substr(YEAR_MONTH, 5, 6), "01", sep = "-")), 
         UDA_BAND_1_DELIVERED = BAND_1_DELIVERED*1, 
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
