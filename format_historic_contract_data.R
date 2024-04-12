# set up
library(tidyverse)

# read in time series file
contract_data <- read.csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/BSA_published_data/time_series_contractual.csv")

# read in contract to ICB mapping table
source("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/contract_to_icb_mapping.R")

# join tables to add region column and map all months to ICBs
contract_data_formatted <- contract_data %>% 
  left_join(extract, by = c("CONTRACT_NUMBER" = "contract_number")) %>% 
  select(-c(COMMISSIONER_CODE, COMMISSIONER_NAME)) %>% 
  rename("COMMISSIONER_NAME" = commissioner_name, 
         "COMMISSIONER_CODE" = commissioner_ods_code_icb, 
         "REGION_NAME" = region_name, 
         "REGION_CODE" = region_code) %>% 
  select(YEAR_MONTH, COMMISSIONER_NAME, COMMISSIONER_CODE, REGION_NAME, REGION_CODE, everything())

# reformat month, contract start date, and contract end date
contract_data_formatted <- contract_data_formatted %>% 
  mutate(YEAR_MONTH = as.Date(paste(substr(YEAR_MONTH, 1, 4), substr(YEAR_MONTH, 5, 6), "01", sep = "-")), 
         START_DATE = as.Date(START_DATE, format = "%d/%m/%Y"), 
         END_DATE = as.Date(END_DATE, format = "%d/%m/%Y"))
