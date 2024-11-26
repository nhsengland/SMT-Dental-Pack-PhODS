library(tidyverse)
library(ggpubr)
library(scales)
library(readxl)
library(textclean)
library(lubridate)
library(openxlsx)

source("ExportDataFile_metadata.R")

######UDA#####
#### No UDA delivered, contracted & percentage
data_Nat_UDA=plot_UDA_UOA_delivery_wd(data = UDA_calendar_data, 
                         UDAorUOA = "UDA",
                         plotChart = FALSE, 
                         all_regions_and_STPs = FALSE,
                         level = "National",
                         region_STP_name = NULL)

data_Nat_UDA<- data_Nat_UDA %>% 
  select(calendar_month,UDAs_annual_contracted,UDAs_delivered_month,UDAs_delivered_month_percent_contracted_standardised)%>%
  mutate(geography_level='National',geography_name='England') %>% 
  arrange(desc(calendar_month))

data_reg_UDA=plot_UDA_UOA_delivery_all_regions(data = UDA_calendar_data, 
                                  UDAorUOA = "UDA",
                                  level = "National",
                                  region_STP_name = NULL,
                                  plotChart = FALSE, 
                                  all_regions_and_STPs = TRUE)
data_reg_UDA <- data_reg_UDA %>%
  select(calendar_month,geography_name=region_name,
         UDAs_annual_contracted,UDAs_delivered_month,UDAs_delivered_month_percent_contracted_standardised)%>%
  mutate(geography_level='Regional') %>% 
  arrange(desc(calendar_month))

data_ICB_UDA <- Table_UDA_UOA_delivery_all_ICBs(data = UDA_calendar_data, 
                                UDAorUOA = "UDA")

data_ICB_UDA <- data_ICB_UDA %>%
  select(calendar_month,geography_name=commissioner_name,
         UDAs_annual_contracted,UDAs_delivered_month,UDAs_delivered_month_percent_contracted_standardised)%>%
  mutate(geography_level='ICB') %>% 
  arrange(desc(calendar_month))

data_UDA_de_co<- rbind(data_Nat_UDA, data_reg_UDA, data_ICB_UDA)

####YTD delivery
data_Nat_YTD_UDA<- Table_YTD_UDA_UOA_delivery (data = UDA_calendar_data, 
                                       UDAorUOA = "UDA",
                                       level = "National",
                                       all_regions_and_STPs = FALSE)

data_Nat_YTD_UDA<- data_Nat_YTD_UDA %>% 
  mutate(geography_level='National',geography_name='England') %>% 
  arrange(desc(calendar_month))

data_reg_YTD_UDA<- Table_YTD_UDA_UOA_delivery (data = UDA_calendar_data, 
                                               UDAorUOA = "UDA",
                                               level = "Regional",
                                               all_regions_and_STPs =TRUE)
data_reg_YTD_UDA <- data_reg_YTD_UDA %>%
  rename(geography_name=region_name)%>%
  mutate(geography_level='Regional') %>% 
  arrange(desc(calendar_month))

data_icb_YTD_UDA<- Table_YTD_UDA_UOA_delivery (data = UDA_calendar_data, 
                                               UDAorUOA = "UDA",
                                               level = "STP",
                                               all_regions_and_STPs =TRUE)
data_icb_YTD_UDA <- data_icb_YTD_UDA %>%
  rename(geography_name=commissioner_name)%>%
  mutate(geography_level='ICB') %>% 
  arrange(desc(calendar_month))

data_UDA_YTD<- rbind(data_Nat_YTD_UDA, data_reg_YTD_UDA, data_icb_YTD_UDA) %>% 
  rename(UDAs_delivered_year_to_date = YTD_delivery)

#### Banded CoTs
data_Nat_CoT <- table_banded_CoT(data = UDA_calendar_data_FD, 
                                 level = "National", 
                                 all_regions_and_STPs = FALSE, 
                                 standardised = TRUE)

data_Nat_CoT <- data_Nat_CoT %>% 
  rowwise() %>% 
  mutate(CoT_total_delivered_incl_FD_standardised = sum(band1, band2, band3, other, urgent, na.rm = TRUE)) %>%
  mutate(geography_level='National',geography_name='England') %>% 
  rename(calendar_month = month, 
         CoT_band1_delivered_incl_FD_standardised = band1, 
         CoT_band2_delivered_incl_FD_standardised = band2,
         CoT_band2a_delivered_incl_FD_standardised = band2a,
         CoT_band2b_delivered_incl_FD_standardised = band2b,
         CoT_band2c_delivered_incl_FD_standardised = band2c,
         CoT_band3_delivered_incl_FD_standardised = band3, 
         CoT_other_delivered_incl_FD_standardised = other, 
         CoT_urgent_delivered_incl_FD_standardised = urgent) %>% 
  arrange(desc(calendar_month))

data_reg_CoT <- table_banded_CoT(data = UDA_calendar_data_FD, 
                                 level = "Regional", 
                                 all_regions_and_STPs = TRUE, 
                                 standardised = TRUE)

# Convert relevant columns to numeric and handle coercion to NA for non-numeric values
data_reg_CoT <- data_reg_CoT %>%
  mutate(across(c(band1, band2, band2a, band2b, band2c, band3, other, urgent), ~ as.numeric(as.character(.))))

# Create the Total FP17s column by summing the specified numeric columns
data_reg_CoT <- data_reg_CoT %>%
  mutate(CoT_total_delivered_incl_FD_standardised = rowSums(across(c(band1, band2, band3, other, urgent)), na.rm = TRUE))%>%
  mutate(geography_level = "Regional") %>% 
  rename(calendar_month = month,
         geography_name = region_name,
         CoT_band1_delivered_incl_FD_standardised = band1, 
         CoT_band2_delivered_incl_FD_standardised = band2, 
         CoT_band2a_delivered_incl_FD_standardised = band2a,
         CoT_band2b_delivered_incl_FD_standardised = band2b,
         CoT_band2c_delivered_incl_FD_standardised = band2c,
         CoT_band3_delivered_incl_FD_standardised = band3, 
         CoT_other_delivered_incl_FD_standardised = other, 
         CoT_urgent_delivered_incl_FD_standardised = urgent) %>% 
  arrange(desc(calendar_month))

data_icb_CoT <- table_banded_CoT(data = UDA_calendar_data_FD,
                                 level = "STP", 
                                 all_regions_and_STPs = TRUE, 
                                 standardised = TRUE)

# Convert relevant columns to numeric, handling non-numeric values by coercing them to NA
data_icb_CoT <- data_icb_CoT %>%
  mutate(across(c(band1, band2, band2a, band2b, band2c, band3, other, urgent), ~ as.numeric(as.character(.))))

# If the data is grouped, ungroup it
data_icb_CoT <- data_icb_CoT %>% ungroup()

# Create the Total FP17s column by summing the specified numeric columns
data_icb_CoT <- data_icb_CoT %>%
  mutate(CoT_total_delivered_incl_FD_standardised = rowSums(select(., band1, band2, band3, other, urgent), na.rm = TRUE))%>%
  mutate(geography_level = "ICB") %>% 
  rename(calendar_month = month,
         geography_name = commissioner_name,
         CoT_band1_delivered_incl_FD_standardised = band1, 
         CoT_band2_delivered_incl_FD_standardised = band2,  
         CoT_band2a_delivered_incl_FD_standardised = band2a,
         CoT_band2b_delivered_incl_FD_standardised = band2b,
         CoT_band2c_delivered_incl_FD_standardised = band2c,
         CoT_band3_delivered_incl_FD_standardised = band3, 
         CoT_other_delivered_incl_FD_standardised = other, 
         CoT_urgent_delivered_incl_FD_standardised = urgent) %>% 
  arrange(desc(calendar_month))

# repeat for non-standardised CoTs
data_Nat_CoT_unstandardised <- table_banded_CoT(data = UDA_calendar_data_FD, 
                                 level = "National", 
                                 all_regions_and_STPs = FALSE, 
                                 standardised = FALSE)

data_Nat_CoT_unstandardised <- data_Nat_CoT_unstandardised %>% 
  rowwise() %>% 
  mutate(CoT_total_delivered_incl_FD = sum(band1, band2, band3, other, urgent, na.rm = TRUE)) %>%
  mutate(geography_level='National',geography_name='England') %>% 
  rename(calendar_month = month, 
         CoT_band1_delivered_incl_FD = band1, 
         CoT_band2_delivered_incl_FD = band2,  
         CoT_band2a_delivered_incl_FD = band2a,
         CoT_band2b_delivered_incl_FD = band2b,
         CoT_band2c_delivered_incl_FD = band2c,
         CoT_band3_delivered_incl_FD = band3, 
         CoT_other_delivered_incl_FD = other, 
         CoT_urgent_delivered_incl_FD = urgent) %>% 
  arrange(desc(calendar_month))

data_reg_CoT_unstandardised <- table_banded_CoT(data = UDA_calendar_data_FD, 
                                 level = "Regional", 
                                 all_regions_and_STPs = TRUE, 
                                 standardised = FALSE)

# Convert relevant columns to numeric and handle coercion to NA for non-numeric values
data_reg_CoT_unstandardised <- data_reg_CoT_unstandardised %>%
  mutate(across(c(band1, band2, band2a, band2b, band2c, band3, other, urgent), ~ as.numeric(as.character(.))))

# Create the Total FP17s column by summing the specified numeric columns
data_reg_CoT_unstandardised <- data_reg_CoT_unstandardised %>%
  mutate(CoT_total_delivered_incl_FD = rowSums(across(c(band1, band2, band3, other, urgent)), na.rm = TRUE))%>%
  mutate(geography_level = "Regional") %>% 
  rename(calendar_month = month,
         geography_name = region_name,
         CoT_band1_delivered_incl_FD = band1, 
         CoT_band2_delivered_incl_FD = band2,  
         CoT_band2a_delivered_incl_FD = band2a,
         CoT_band2b_delivered_incl_FD = band2b,
         CoT_band2c_delivered_incl_FD = band2c, 
         CoT_band3_delivered_incl_FD = band3, 
         CoT_other_delivered_incl_FD = other, 
         CoT_urgent_delivered_incl_FD = urgent) %>% 
  arrange(desc(calendar_month))

data_icb_CoT_unstandardised <- table_banded_CoT(data = UDA_calendar_data_FD,
                                 level = "STP", 
                                 all_regions_and_STPs = TRUE, 
                                 standardised = FALSE)

# Convert relevant columns to numeric, handling non-numeric values by coercing them to NA
data_icb_CoT_unstandardised <- data_icb_CoT_unstandardised %>%
  mutate(across(c(band1, band2, band2a, band2b, band2c, band3, other, urgent), ~ as.numeric(as.character(.))))

# If the data is grouped, ungroup it
data_icb_CoT_unstandardised <- data_icb_CoT_unstandardised %>% ungroup()

# Create the Total FP17s column by summing the specified numeric columns
data_icb_CoT_unstandardised <- data_icb_CoT_unstandardised %>%
  mutate(CoT_total_delivered_incl_FD = rowSums(select(., band1, band2, band3, other, urgent), na.rm = TRUE))%>%
  mutate(geography_level = "ICB") %>% 
  rename(calendar_month = month,
         geography_name = commissioner_name,
         CoT_band1_delivered_incl_FD = band1, 
         CoT_band2_delivered_incl_FD = band2,  
         CoT_band2a_delivered_incl_FD = band2a,
         CoT_band2b_delivered_incl_FD = band2b,
         CoT_band2c_delivered_incl_FD = band2c, 
         CoT_band3_delivered_incl_FD = band3, 
         CoT_other_delivered_incl_FD = other, 
         CoT_urgent_delivered_incl_FD = urgent) %>% 
  arrange(desc(calendar_month))

data_CoT_standardised <- rbind(data_Nat_CoT, data_reg_CoT, data_icb_CoT)

data_CoT_unstandardised <- rbind(data_Nat_CoT_unstandardised, data_reg_CoT_unstandardised, data_icb_CoT_unstandardised)

data_CoT <- data_CoT_unstandardised %>% 
  full_join(data_CoT_standardised, by = c("calendar_month", "geography_name", "geography_level"))

###### UOA #####
#### No UOA delivered, contracted & percentage & start/complete
data_start_nat <- UOA_calendar_data %>%
  group_by(calendar_month=as.character(format(as.Date(month), "%Y-%m"))) %>%
  summarise(UOA_started = sum(ortho_trt_started, na.rm = TRUE),
            UOA_completed = sum(ortho_trt_completed, na.rm = TRUE))

data_Nat_UOA=plot_UDA_UOA_delivery_wd(data = UOA_calendar_data,
                                     UDAorUOA = "UOA",
                                     plotChart = FALSE,
                                     all_regions_and_STPs = FALSE,
                                     level = "National",
                                     region_STP_name = NULL)

data_Nat_UOA<- data_Nat_UOA %>%
  select(calendar_month, financial_year,`UOAs_annual_contracted`,`UOAs_delivered_month`,`no workdays`,
         UOAs_delivered_month_percent_contracted_standardised)%>%
   left_join(data_start_nat,by='calendar_month')%>%
  mutate(geography_level='National',geography_name='England') %>%
  arrange(desc(calendar_month))

data_start_reg <- UOA_calendar_data %>%
  group_by(calendar_month=as.character(format(as.Date(month), "%Y-%m")), geography_name=region_name) %>%
  summarise(UOA_started = sum(ortho_trt_started, na.rm = TRUE),
            UOA_completed = sum(ortho_trt_completed, na.rm = TRUE))

data_reg_UOA=plot_UDA_UOA_delivery_all_regions(data = UOA_calendar_data,
                                              UDAorUOA = "UOA",
                                              level = "Regional",
                                              region_STP_name = NULL,
                                              plotChart = FALSE,
                                              all_regions_and_STPs = TRUE)
data_reg_UOA <- data_reg_UOA %>%
  select(calendar_month, financial_year, geography_name=region_name,`UOAs_annual_contracted`,`UOAs_delivered_month`,`no workdays`,
         UOAs_delivered_month_percent_contracted_standardised)%>%
  left_join(data_start_reg,by=c('calendar_month','geography_name'))%>%
  mutate(geography_level='Regional') %>% 
  arrange(desc(calendar_month))


data_start_ICB <- UOA_calendar_data %>%
  group_by(calendar_month=as.character(format(as.Date(month), "%Y-%m")), geography_name=commissioner_name) %>%
  summarise(UOA_started = sum(ortho_trt_started, na.rm = TRUE),
            UOA_completed = sum(ortho_trt_completed, na.rm = TRUE))


data_ICB_UOA <- Table_UDA_UOA_delivery_all_ICBs(data = UOA_calendar_data,
                                                UDAorUOA = "UOA")

data_ICB_UOA <- data_ICB_UOA %>%
  select(calendar_month, financial_year, geography_name=commissioner_name,`UOAs_annual_contracted`=annual_contracted_UDA_UOA,
         `UOAs_delivered_month`=monthly_UDA_UOAs_delivered,`no workdays`,
         UOAs_delivered_month_percent_contracted_standardised=perc_standardised_wd)%>%
  left_join(data_start_ICB,by=c('calendar_month','geography_name'))%>%
  mutate(geography_level='ICB') %>% 
  arrange(desc(calendar_month))

data_UOA_de_co<- rbind(data_Nat_UOA, data_reg_UOA, data_ICB_UOA) %>% 
 select(calendar_month, financial_year,geography_level,geography_name,UOAs_annual_contracted,UOAs_delivered_month,
        UOAs_delivered_month_percent_contracted_standardised,UOA_started,UOA_completed)

####YTD delivery
data_Nat_YTD_UOA<- Table_YTD_UDA_UOA_delivery (data = UOA_calendar_data,
                                              UDAorUOA = "UOA",
                                              level = "National",
                                              all_regions_and_STPs = FALSE)

data_Nat_YTD_UOA<- data_Nat_YTD_UOA %>%
 mutate(geography_level='National',geography_name='England') %>% 
  arrange(desc(calendar_month))

data_reg_YTD_UOA<- Table_YTD_UDA_UOA_delivery (data = UOA_calendar_data,
                                              UDAorUOA = "UOA",
                                              level = "Regional",
                                              all_regions_and_STPs =TRUE)

data_reg_YTD_UOA <- data_reg_YTD_UOA %>%
 rename(geography_name=region_name)%>%
 mutate(geography_level='Regional') %>% 
  arrange(desc(calendar_month))

data_icb_YTD_UOA<- Table_YTD_UDA_UOA_delivery (data = UOA_calendar_data,
                                              UDAorUOA = "UOA",
                                              level = "STP",
                                              all_regions_and_STPs =TRUE)

data_icb_YTD_UOA <- data_icb_YTD_UOA %>%
 rename(geography_name=`commissioner_name`)%>%
 mutate(geography_level='ICB') %>% 
  arrange(desc(calendar_month))

data_UOA_YTD<- rbind(data_Nat_YTD_UOA, data_reg_YTD_UOA, data_icb_YTD_UOA) %>% 
  rename(UOAs_delivered_year_to_date = YTD_delivery)

data_orthodontic_activity <- data_UOA_de_co %>% 
  full_join(data_UOA_YTD, by = c("calendar_month", "financial_year", "geography_name", "geography_level"))

### Unique patients seen ####
# Unique patients rolling
# get the right data -- sum unique patient seen for general population/adult/child 
data_Nat_unique <- pull_unique_patients() %>% 
  select(month, all_12m_count_ctry, child_12m_count_ctry, adult_24m_count_ctry) %>% 
  distinct() %>% 
  mutate(month = format(as.Date(month), "%Y-%m"), 
         geography_level='National',geography_name='England') %>% 
  rename(unique_patients_seen_12_month = all_12m_count_ctry, 
         unique_children_seen_12_month = child_12m_count_ctry, 
         unique_adults_seen_24_month = adult_24m_count_ctry, 
         calendar_month = month) %>% 
  arrange(desc(calendar_month))

data_ICB_unique <- pull_unique_patients() %>% 
  select(month, commissioner_name, all_12m_count, child_12m_count, adult_24m_count) %>% 
  mutate(commissioner_name = str_to_title(commissioner_name), 
         commissioner_name = gsub("Icb", "ICB", commissioner_name), 
         month = format(as.Date(month), "%Y-%m"), 
         geography_level = "ICB") %>% 
  rename(geography_name = commissioner_name, 
         unique_patients_seen_12_month = all_12m_count, 
         unique_children_seen_12_month = child_12m_count, 
         unique_adults_seen_24_month = adult_24m_count, 
         calendar_month = month) %>% 
  arrange(desc(calendar_month))

data_unique <- rbind(data_Nat_unique, data_ICB_unique)

### New Patient Premium ####
npp_nat <- npp_data %>% 
  filter(month >= "2024-03-01" & new_patient_tariff_amount > 0) %>% 
  group_by(month) %>% 
  summarise(total_NPP_patients_seen = sum(total),
            adult_NPP_patients_seen = sum(adult_count), 
            child_NPP_patients_seen = sum(child_count), 
            adult_NPP_band_1 = sum(band1_adult_count), 
            adult_NPP_band_23 = sum(band23_adult_count), 
            child_NPP_band_1 = sum(band1_child_count), 
            child_NPP_band_23 = sum(band23_child_count)) %>% 
  mutate(geography_level = "National", 
         geography_name = "England", 
         month = format(as.Date(month), "%Y-%m")) %>% 
  arrange(desc(month)) %>% 
  rename(calendar_month = month)

npp_reg <- npp_data %>% 
  filter(month >= "2024-03-01" & new_patient_tariff_amount > 0) %>% 
  group_by(month, region_name) %>% 
  summarise(total_NPP_patients_seen = sum(total),
            adult_NPP_patients_seen = sum(adult_count), 
            child_NPP_patients_seen = sum(child_count), 
            adult_NPP_band_1 = sum(band1_adult_count), 
            adult_NPP_band_23 = sum(band23_adult_count), 
            child_NPP_band_1 = sum(band1_child_count), 
            child_NPP_band_23 = sum(band23_child_count)) %>% 
  mutate(geography_level = "Regional", 
         month = format(as.Date(month), "%Y-%m")) %>% 
  arrange(desc(month)) %>% 
  rename(geography_name = region_name, 
         calendar_month = month)

npp_icb <- npp_data %>% 
  filter(month >= "2024-03-01" & new_patient_tariff_amount > 0) %>% 
  group_by(month, commissioner_name) %>% 
  summarise(total_NPP_patients_seen = sum(total),
            adult_NPP_patients_seen = sum(adult_count), 
            child_NPP_patients_seen = sum(child_count), 
            adult_NPP_band_1 = sum(band1_adult_count), 
            adult_NPP_band_23 = sum(band23_adult_count), 
            child_NPP_band_1 = sum(band1_child_count), 
            child_NPP_band_23 = sum(band23_child_count)) %>% 
  mutate(geography_level = "ICB", 
         month = format(as.Date(month), "%Y-%m")) %>% 
  arrange(desc(month)) %>% 
  rename(geography_name = commissioner_name, 
         calendar_month = month)

npp_total <- rbind(npp_nat, npp_reg, npp_icb)

npp_contracts_nat <- npp_data %>% 
  filter(month >= "2024-03-01") %>% 
  mutate(saw_new_patient = ifelse(new_patient_tariff_amount == 0, "no", "yes")) %>% 
  group_by(month, saw_new_patient) %>% 
  summarise(contracts = n_distinct(contract_number)) %>% 
  pivot_wider(names_from = saw_new_patient, values_from = contracts) %>% 
  mutate(yes = ifelse(is.na(yes), 0, yes),
         no = ifelse(is.na(no), 0, no),
         eligible = yes + no, 
         month = format(as.Date(month), "%Y-%m"), 
         geography_name = "England", 
         geography_level = "National") %>% 
  select(-no) %>% 
  arrange(desc(month)) %>% 
  rename(calendar_month = month, 
         contracts_seeing_NPP_patients = yes,
         eligible_contracts_for_NPP = eligible)

npp_contracts_reg <- npp_data %>% 
  filter(month >= "2024-03-01") %>% 
  mutate(saw_new_patient = ifelse(new_patient_tariff_amount == 0, "no", "yes")) %>% 
  group_by(month, region_name, saw_new_patient) %>% 
  summarise(contracts = n_distinct(contract_number)) %>% 
  pivot_wider(names_from = saw_new_patient, values_from = contracts) %>% 
  mutate(yes = ifelse(is.na(yes), 0, yes), 
         no = ifelse(is.na(no), 0, no),
         eligible = yes + no, 
         month = format(as.Date(month), "%Y-%m"),
         geography_level = "Regional") %>% 
  select(-no) %>% 
  arrange(desc(month)) %>% 
  rename(calendar_month = month, 
         contracts_seeing_NPP_patients = yes,
         eligible_contracts_for_NPP = eligible, 
         geography_name = region_name)

npp_contracts_icb <- npp_data %>% 
  filter(month >= "2024-03-01") %>% 
  mutate(saw_new_patient = ifelse(new_patient_tariff_amount == 0, "no", "yes")) %>% 
  group_by(month, commissioner_name, saw_new_patient) %>% 
  summarise(contracts = n_distinct(contract_number)) %>% 
  pivot_wider(names_from = saw_new_patient, values_from = contracts) %>% 
  mutate(yes = ifelse(is.na(yes), 0, yes),
         no = ifelse(is.na(no), 0, no),
         eligible = yes + no, 
         month = format(as.Date(month), "%Y-%m"),
         geography_level = "ICB") %>% 
  select(-no) %>% 
  arrange(desc(month)) %>% 
  rename(calendar_month = month, 
         contracts_seeing_NPP_patients = yes,
         eligible_contracts_for_NPP = eligible, 
         geography_name = commissioner_name)

npp_contracts <- rbind(npp_contracts_nat, npp_contracts_reg, npp_contracts_icb) %>% 
  select(calendar_month, eligible_contracts_for_NPP, contracts_seeing_NPP_patients, everything())

data_dental_activity<-data_UDA_de_co%>%
  full_join(data_UDA_YTD,by=c('calendar_month','geography_name','geography_level'))%>%
  left_join(data_CoT, by = c('calendar_month', 'geography_name', 'geography_level')) %>% 
  left_join(data_unique, by = c('calendar_month', 'geography_name', 'geography_level'))%>%
  left_join(npp_total, by = c("calendar_month", "geography_name", "geography_level")) %>% 
  left_join(npp_contracts, by = c("calendar_month", "geography_name", "geography_level")) %>% 
  select(calendar_month, financial_year, geography_level, geography_name, everything())


##########DCP#######################################

  data = UDA_calendar_data
  dcp_data = DCP_data
  dcp_data <- dcp_data %>%
    rename(month = Month)
  
  #calculate total FP19, UDA_B1, B2, B3 and urgent by month for "Total_dentist_only_and_DCP_assisted' by using UDA Calendar data
  delivery_total_national <-  UDA_calendar_data %>% 
    rename(Region = region_name) %>%
    group_by(month) %>%
    dplyr::summarise( completed_courses_of_treatment = sum(general_FP17s, na.rm = TRUE),
                      UDA_B1 = sum(UDA_band_1, na.rm = TRUE),
                      UDA_B2 = sum(UDA_band_2, na.rm = TRUE),
                      UDA_B3 = sum(UDA_band_3, na.rm = TRUE),
                      UDA_urgent = sum(UDA_urgent, na.rm = TRUE)) %>%
    mutate (DCP_description = "Total_dentist_only_and_DCP_assisted") %>%
    select (month, DCP_description, completed_courses_of_treatment,UDA_B1, UDA_B2, UDA_B3, UDA_urgent)
  
  delivery_total_regional <-  UDA_calendar_data %>% 
    rename(Region = region_name) %>%
    group_by(month, Region) %>%
    dplyr::summarise( completed_courses_of_treatment = sum(general_FP17s, na.rm = TRUE),
                      UDA_B1 = sum(UDA_band_1, na.rm = TRUE),
                      UDA_B2 = sum(UDA_band_2, na.rm = TRUE),
                      UDA_B3 = sum(UDA_band_3, na.rm = TRUE),
                      UDA_urgent = sum(UDA_urgent, na.rm = TRUE)) %>%
    mutate (DCP_description = "Total_dentist_only_and_DCP_assisted") %>%
    select (month, Region, DCP_description, completed_courses_of_treatment,UDA_B1, UDA_B2, UDA_B3, UDA_urgent)
  
  
  delivery_total_ICB <-  UDA_calendar_data %>% 
    rename(Region = region_name) %>%
    group_by(month, commissioner_name) %>%
    dplyr::summarise( completed_courses_of_treatment = sum(general_FP17s, na.rm = TRUE),
                      UDA_B1 = sum(UDA_band_1, na.rm = TRUE),
                      UDA_B2 = sum(UDA_band_2, na.rm = TRUE),
                      UDA_B3 = sum(UDA_band_3, na.rm = TRUE),
                      UDA_urgent = sum(UDA_urgent, na.rm = TRUE)) %>%
    mutate (DCP_description = "Total_dentist_only_and_DCP_assisted") %>%
    select (month, commissioner_name, DCP_description, completed_courses_of_treatment,UDA_B1, UDA_B2, UDA_B3, UDA_urgent)
 
    #calculate total FP19, UDA_B1, B2, B3 and urgent by month for separate DCP description by using DCP data 
  dcp_main_new <- dcp_data %>% 
    filter(DCP_description != 'Clinical Technician') %>%
    filter(DCP_description != 'Technician') %>%
    filter(DCP_DIR_DESC != "Technician") %>% 
    mutate(DCP_description = replace(DCP_description, DCP_description== "Nurse", "Dental_Nurse_assisted"), 
           DCP_description = replace(DCP_description, DCP_description=="Dental Hygienist", "Hygienist_assisted"), 
           DCP_description = replace(DCP_description, DCP_description=="Dental Therapist", "Therapist_assisted"),
           DCP_description = replace(DCP_description, DCP_description=="Hygienist", "Hygienist_assisted"), 
           DCP_description = replace(DCP_description, DCP_description=="Therapist", "Therapist_assisted"), 
           DCP_DIR_DESC = replace(DCP_DIR_DESC, DCP_DIR_DESC=="Therapist", "Therapist_led"), 
           DCP_DIR_DESC = replace(DCP_DIR_DESC, DCP_DIR_DESC=="Hygienist", "Hygienist_led"), 
           DCP_DIR_DESC = replace(DCP_DIR_DESC, DCP_DIR_DESC=="Nurse", "Dental_Nurse_led"))
  
  dcp_summary_national <- dcp_main_new%>% 
    pivot_longer(cols = DCP_description:DCP_DIR_DESC, names_to = "variable", values_to = "DCP_description") %>% 
    select(-variable) %>% 
    filter(!DCP_description == "") %>% 
    group_by(month, DCP_description) %>%
    dplyr::summarise (completed_courses_of_treatment = sum(FP17_Current_Year_total, na.rm = TRUE),
                      UDA_B1 = sum(Band_1._UDA, na.rm = TRUE),
                      UDA_B2 = sum(Band_2._UDA, na.rm = TRUE),
                      UDA_B3 = sum(Band_3._UDA, na.rm = TRUE),
                      UDA_urgent = sum(Urgent_UDA, na.rm = TRUE)) %>% 
    filter(!(month <= "2024-03-01" & DCP_description == "Dental_Nurse_led")) %>% 
    filter(!(month <= "2024-03-01" & DCP_description == "Hygienist_led")) %>% 
    filter(!(month <= "2024-03-01" & DCP_description == "Therapist_led"))
  
  dcp_summary_regional <- dcp_main_new%>% 
    pivot_longer(cols = DCP_description:DCP_DIR_DESC, names_to = "variable", values_to = "DCP_description") %>% 
    select(-variable) %>% 
    filter(!DCP_description == "") %>% 
    group_by(month, Region, DCP_description) %>%
    dplyr::summarise (completed_courses_of_treatment = sum(FP17_Current_Year_total, na.rm = TRUE),
                      UDA_B1 = sum(Band_1._UDA, na.rm = TRUE),
                      UDA_B2 = sum(Band_2._UDA, na.rm = TRUE),
                      UDA_B3 = sum(Band_3._UDA, na.rm = TRUE),
                      UDA_urgent = sum(Urgent_UDA, na.rm = TRUE)) %>% 
    filter(!(month <= "2024-03-01" & DCP_description == "Dental_Nurse_led")) %>% 
    filter(!(month <= "2024-03-01" & DCP_description == "Hygienist_led")) %>% 
    filter(!(month <= "2024-03-01" & DCP_description == "Therapist_led"))
  
  
  dcp_summary_icb <- dcp_main_new%>% 
    pivot_longer(cols = DCP_description:DCP_DIR_DESC, names_to = "variable", values_to = "DCP_description") %>% 
    select(-variable) %>% 
    filter(!DCP_description == "") %>% 
    group_by(month, commissioner_name, DCP_description) %>%
      dplyr::summarise (completed_courses_of_treatment = sum(FP17_Current_Year_total, na.rm = TRUE),
                        UDA_B1 = sum(Band_1._UDA, na.rm = TRUE),
                        UDA_B2 = sum(Band_2._UDA, na.rm = TRUE),
                        UDA_B3 = sum(Band_3._UDA, na.rm = TRUE),
                        UDA_urgent = sum(Urgent_UDA, na.rm = TRUE)) %>% 
    filter(!(month <= "2024-03-01" & DCP_description == "Dental_Nurse_led")) %>% 
    filter(!(month <= "2024-03-01" & DCP_description == "Hygienist_led")) %>% 
    filter(!(month <= "2024-03-01" & DCP_description == "Therapist_led"))
    
    #change the format of total delivery and separate dcp and then get full data ready for plotting 
    # then calculate the percentage of each dcp description/total delivery 
    dcp_summary_national_longer <- dcp_summary_national %>% pivot_longer ( ##where does dcp summary come from?
      cols = c(completed_courses_of_treatment, UDA_B1, UDA_B2, UDA_B3, UDA_urgent),
      names_to = "DCP_metric",
      names_prefix = "dcp",
      values_to = "numbers",
      values_drop_na = TRUE
    )
    
    delivery_total_national_longer <- delivery_total_national %>% pivot_longer(
      cols =c(completed_courses_of_treatment, UDA_B1, UDA_B2, UDA_B3, UDA_urgent),
      names_to = "DCP_metric",
      names_prefix = "dcp",
      values_to = "all_numbers",
      values_drop_na = TRUE
    )
    
    all_lookup_national <- left_join(dcp_summary_national_longer, delivery_total_national_longer, by = 
                                  c("month", "DCP_metric")) %>% 
      select (month, DCP_description.x, DCP_metric, numbers, DCP_description.y, all_numbers)
    
    total_national <- all_lookup_national %>% 
      mutate (dcp_percent = formattable::percent (numbers / all_numbers, digits=2))%>%
      mutate(geography_name='England',geography_level='National') %>% 
      arrange(desc(month))
    
    
    dcp_summary_regional_longer <- dcp_summary_regional %>% pivot_longer ( ##where does dcp summary come from?
      cols = c(completed_courses_of_treatment, UDA_B1, UDA_B2, UDA_B3, UDA_urgent),
      names_to = "DCP_metric",
      names_prefix = "dcp",
      values_to = "numbers",
      values_drop_na = TRUE
    ) 
    
    delivery_total_regional_longer <- delivery_total_regional %>% pivot_longer(
      cols = c(completed_courses_of_treatment, UDA_B1, UDA_B2, UDA_B3, UDA_urgent),
      names_to = "DCP_metric",
      names_prefix = "dcp",
      values_to = "all_numbers",
      values_drop_na = TRUE
    )
    
    all_lookup_regional <- left_join(dcp_summary_regional_longer, delivery_total_regional_longer, by = 
                                       c("month", "Region", "DCP_metric")) %>% 
      select (month, Region,DCP_description.x, DCP_metric, numbers, DCP_description.y, all_numbers)
    
    total_regional <- all_lookup_regional %>% 
      mutate (dcp_percent = formattable::percent (numbers / all_numbers, digits=2))%>%
      rename(geography_name=`Region`)%>%
      mutate(geography_level='Region') %>% 
      arrange(desc(month))
  
  dcp_summary_icb_longer <- dcp_summary_icb %>% pivot_longer ( ##where does dcp summary come from?
    cols = c(completed_courses_of_treatment, UDA_B1, UDA_B2, UDA_B3, UDA_urgent),
    names_to = "DCP_metric",
    names_prefix = "dcp",
    values_to = "numbers",
    values_drop_na = TRUE
  ) 
  
  delivery_total_ICB_longer <- delivery_total_ICB %>% pivot_longer(
    cols = c(completed_courses_of_treatment, UDA_B1, UDA_B2, UDA_B3, UDA_urgent),
    names_to = "DCP_metric",
    names_prefix = "dcp",
    values_to = "all_numbers",
    values_drop_na = TRUE
  )
  
  all_lookup_icb <- left_join(dcp_summary_icb_longer, delivery_total_ICB_longer, by = 
                            c("month", "DCP_metric", "commissioner_name")) %>% 
    select (month, commissioner_name,DCP_description.x, DCP_metric, numbers, DCP_description.y, all_numbers)
  
  total_icb <- all_lookup_icb %>% 
    mutate (dcp_percent = formattable::percent (numbers / all_numbers, digits=2))%>%
    rename(geography_name=`commissioner_name`)%>%
    mutate(geography_level='ICB') %>% 
    arrange(desc(month))
  
  total_dcp<- rbind(total_national, total_regional, total_icb) %>% 
    filter(DCP_metric != "completed_courses_of_treatment") %>% 
    mutate(financial_year = get_financial_year(month), 
           `month` = format(as.Date(month), "%Y-%m"))%>%
    select(calendar_month=month, financial_year,geography_level,geography_name,DCP_metric,DCP_description=DCP_description.x,
           metric_count_by_DCP = numbers,metric_count_total = all_numbers,DCP_involved_percent = dcp_percent)
  


#####################################BPE##########################################
  data_national <-BPE_data %>% 
    filter(Total.Form.Count>0,
           Year_Month>= "2023-04-01") %>%
    group_by(Year_Month, Contract.Number) %>%
    summarise (nlow_risk = 
        sum (as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_0_UDT), na.rm = TRUE),
      low_risk_less1year = 
        sum(as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year), na.rm = TRUE)) %>%  
    mutate (percent_low_risk_whic_are1_year = 
              formattable::percent (low_risk_less1year/nlow_risk, digits =1)) %>% 
    filter(!is.na(percent_low_risk_whic_are1_year)) %>% 
    filter (percent_low_risk_whic_are1_year<2)%>%
    mutate(threshold_percent_low_risk_whic_are1_year=ifelse(percent_low_risk_whic_are1_year >= 0.5, 'YES','NO'))
  
  data_total_national <- data_national %>% 
    group_by(Year_Month) %>%
    summarise (NContractors = n_distinct(Contract.Number))
  
  data_high_national <- data_national %>% 
    filter(threshold_percent_low_risk_whic_are1_year=='YES') %>%
    group_by(Year_Month) %>%
    summarise (low_risk_NContractors = n_distinct(Contract.Number)) 
  
  BPE_all_national<-data_total_national%>%
    left_join(data_high_national, by='Year_Month')%>%
    mutate(geography_name='England',geography_level='National',
           pct_low_risk_recalled=formattable::percent (low_risk_NContractors/ NContractors, digits =0) ) %>% 
    arrange(desc(Year_Month))

  
  data_region <-BPE_data %>% 
    filter(Total.Form.Count>0,
           Year_Month>= "2023-04-01") %>%
    group_by(Year_Month, Latest.Region.Description,Contract.Number) %>%
    summarise (nlow_risk = 
                 sum (as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_0_UDT), na.rm = TRUE),
               low_risk_less1year = 
                 sum(as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year), na.rm = TRUE)) %>%  
    mutate (percent_low_risk_whic_are1_year = 
              formattable::percent (low_risk_less1year/nlow_risk, digits =1)) %>% 
    filter(!is.na(percent_low_risk_whic_are1_year)) %>% 
    filter (percent_low_risk_whic_are1_year<2)%>%
    mutate(threshold_percent_low_risk_whic_are1_year=ifelse(percent_low_risk_whic_are1_year >= 0.5, 'YES','NO'))
  
  data_total_region <- data_region %>% 
    group_by(Year_Month, geography_name=Latest.Region.Description) %>%
    summarise (NContractors = n_distinct(Contract.Number)) 
  
  data_high_region<- data_region %>% 
    filter(threshold_percent_low_risk_whic_are1_year=='YES') %>%
    group_by(Year_Month, geography_name=Latest.Region.Description) %>%
    summarise (low_risk_NContractors = n_distinct(Contract.Number)) 
  
  BPE_all_region<-data_total_region%>%
    left_join(data_high_region, by=c('Year_Month',"geography_name"))%>%
    mutate(geography_level='Region',
           pct_low_risk_recalled=formattable::percent (low_risk_NContractors/ NContractors, digits =0) ) %>% 
    arrange(desc(Year_Month))
  

  data_ICB <-BPE_data %>% 
    filter(Total.Form.Count>0,
           Year_Month>= "2023-04-01") %>%
    group_by(Year_Month, commissioner_name,Contract.Number) %>%
    summarise (nlow_risk = 
                 sum (as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_0_UDT), na.rm = TRUE),
               low_risk_less1year = 
                 sum(as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year), na.rm = TRUE)) %>%  
    mutate (percent_low_risk_whic_are1_year = 
              formattable::percent (low_risk_less1year/nlow_risk, digits =1)) %>% 
    filter(!is.na(percent_low_risk_whic_are1_year)) %>% 
    filter (percent_low_risk_whic_are1_year<2)%>%
    mutate(threshold_percent_low_risk_whic_are1_year=ifelse(percent_low_risk_whic_are1_year >= 0.5, 'YES','NO'))
  
  data_total_ICB<- data_ICB %>% 
    group_by(Year_Month, geography_name=commissioner_name) %>%
    summarise (NContractors = n_distinct(Contract.Number)) 
  
  data_high_ICB<- data_ICB %>% 
    filter(threshold_percent_low_risk_whic_are1_year=='YES') %>%
    group_by(Year_Month, geography_name=commissioner_name) %>%
    summarise (low_risk_NContractors = n_distinct(Contract.Number)) 
  
  BPE_all_ICB<-data_total_ICB%>%
    left_join(data_high_ICB, by=c('Year_Month',"geography_name"))%>%
    mutate(geography_level='ICB',
           pct_low_risk_recalled=formattable::percent (low_risk_NContractors/ NContractors, digits =0) ) %>% 
    arrange(desc(Year_Month))
  
  total_bpe<- rbind(BPE_all_national, BPE_all_region, BPE_all_ICB) %>% 
    mutate(financial_year = get_financial_year(Year_Month), 
      Year_Month = format(as.Date(Year_Month), "%Y-%m"))%>%
    select(calendar_month=Year_Month, financial_year, geography_level,geography_name,no_contracts=NContractors,
           no_contracts_recall_50pct_low_risk=low_risk_NContractors,
           pct_contracts_recall_50pct_low_risk=pct_low_risk_recalled)

#### ICB/Region Code ####
# create table to get ICB & region codes
con <- dbConnect(odbc::odbc(), "NCDR")
  
icb_region_mapping <- dbGetQuery(con, "SELECT DISTINCT Region_Code AS geography_code, Region_Name AS geography_name
                                 FROM [NHSE_Reference].[dbo].[tbl_Ref_ODS_Commissioner_Hierarchies]
                                 WHERE STP_Code NOT IN ('DUM001', 'DUM002', 'UNK','X24')
                                 UNION
                                 SELECT DISTINCT STP_Code AS geography_code, STP_Name AS geography_name
                                 FROM [NHSE_Reference].[dbo].[tbl_Ref_ODS_Commissioner_Hierarchies]
                                 WHERE STP_Code NOT IN ('DUM001', 'DUM002', 'UNK','X24')")

dbDisconnect(con)
  
# tidy up region & ICB names so mapping on name works
icb_region_mapping$geography_name <- str_to_title(icb_region_mapping$geography_name)
icb_region_mapping$geography_name <- ifelse(substr(icb_region_mapping$geography_code,1,1)=="Q", 
                                      substr(icb_region_mapping$geography_name,1,
                                             nchar(icb_region_mapping$geography_name)-21),
                                      icb_region_mapping$geography_name)
icb_region_mapping$geography_name <- ifelse(substr(icb_region_mapping$geography_code,1,1)=="Q", 
                                      substr(icb_region_mapping$geography_name,5,
                                             nchar(icb_region_mapping$geography_name)),
                                      icb_region_mapping$geography_name)
icb_region_mapping$geography_name <- ifelse(substr(icb_region_mapping$geography_code,1,1)=="Q", 
                                      paste0(icb_region_mapping$geography_name,
                                             rep("ICB", nrow(icb_region_mapping))),
                                      icb_region_mapping$geography_name)

# add row for national figures
icb_region_mapping[nrow(icb_region_mapping)+1,] = c('National', 'England')

# add to output tables
data_dental_activity <- data_dental_activity %>% 
  left_join(icb_region_mapping, by = "geography_name") %>% 
  select(calendar_month, financial_year, geography_level, geography_name,
         geography_code, everything())

data_orthodontic_activity <- data_orthodontic_activity %>% 
  left_join(icb_region_mapping, by = "geography_name") %>% 
  select(calendar_month, financial_year, geography_level, geography_name,
         geography_code, everything())

total_dcp <- total_dcp %>% 
  left_join(icb_region_mapping, by = "geography_name") %>% 
  select(calendar_month, financial_year, geography_level, geography_name,
         geography_code, everything())

total_bpe <- total_bpe %>% 
  left_join(icb_region_mapping, by = "geography_name") %>% 
  select(calendar_month, financial_year, geography_level, geography_name,
         geography_code, everything())

###### Output #####
# create Excel file
# create function to run the output
  
create_export_file <- function(){
  output_file <- createWorkbook()
  
  addWorksheet(output_file, "Dental contract & activity")
  writeData(output_file, "Dental contract & activity", data_dental_activity)
  
  addWorksheet(output_file, "Orthodontic contract & activity")
  writeData(output_file, "Orthodontic contract & activity", data_orthodontic_activity)
  
  addWorksheet(output_file, "DCP")
  writeData(output_file, "DCP", total_dcp)
  
  addWorksheet(output_file, "Oral health risk assessment")
  writeData(output_file, "Oral health risk assessment", total_bpe)
  
  addWorksheet(output_file, "Metadata")
  writeData(output_file, "Metadata", metadata)
  setColWidths(output_file, "Metadata", cols = 1:3, widths = "auto")
  
  # we will first create a folder to save our output
  # Print the current working directory
  current_wd <- getwd()
  print(paste("Current working directory:", current_wd))
  
  # Check if the reports directory exists
  reports_dir <- file.path(current_wd, "reports")
  if (!dir.exists(reports_dir)) {
    # Try to create the directory
    dir.create(reports_dir)
    # Verify if the directory was successfully created
    if (!dir.exists(reports_dir)) {
      stop("Failed to create 'reports' directory")
    }
  }
  
  # overwrite file if it already exists in the directory
  openxlsx::saveWorkbook(output_file, file = paste0(reports_dir, '/SMT_pack_data_', format(Sys.Date(), '%B%Y'), '.xlsx'), overwrite = TRUE)
}
  
create_pcdid_extract <- function(){
  output <- data_dental_activity %>% 
    select(calendar_month, financial_year, geography_level, geography_name, geography_code,
           unique_children_seen_12_month, unique_adults_seen_24_month) %>% 
    filter(!is.na(unique_children_seen_12_month))
  
  write.csv(output, 
            paste0(reports_dir, '/dental_patients_extract_for_PCDID_data_up_to_', format(Sys.Date()-30, '%B%Y'), '.csv'), 
            row.names = FALSE)
}

create_uda_projections_extract <- function(){
  output <- data_dental_activity %>% 
    select(calendar_month, geography_level, geography_code, geography_name,
           UDAs_annual_contracted, UDAs_delivered_month) %>% 
    filter(calendar_month >= format(current_fy_start, "%Y-%m") &
             geography_level %in% c("National", "ICB")) %>% 
    arrange(calendar_month) %>% 
    mutate(calendar_month = month.name[as.numeric(substr(calendar_month, 6, 7))])
  
  # select contracted UDAs and add columns for remaining months of financial year
  # assume contracted is equal to last available month for the rest of the financial year
  contracted <- output %>% 
    select(calendar_month, geography_name, UDAs_annual_contracted) %>% 
    pivot_wider(names_from = calendar_month, values_from = UDAs_annual_contracted)
  
  # if the month is not already in the columns, add a new column with the same values as the previous column
  if(ncol(contracted) < 13){
    if(!"May" %in% names(contracted)){
      contracted <- contracted %>% 
        mutate(May = April)
    }
    if(!"June" %in% names(contracted)){
      contracted <- contracted %>% 
        mutate(June = May)
    }
    if(!"July" %in% names(contracted)){
      contracted <- contracted %>% 
        mutate(July = June)
    }
    if(!"August" %in% names(contracted)){
      contracted <- contracted %>% 
        mutate(August = July)
    }
    if(!"September" %in% names(contracted)){
      contracted <- contracted %>% 
        mutate(September = August)
    }
    if(!"October" %in% names(contracted)){
      contracted <- contracted %>% 
        mutate(October = September)
    }
    if(!"November" %in% names(contracted)){
      contracted <- contracted %>% 
        mutate(November = October)
    }
    if(!"December" %in% names(contracted)){
      contracted <- contracted %>% 
        mutate(December = November)
    }
    if(!"January" %in% names(contracted)){
      contracted <- contracted %>% 
        mutate(January = December)
    }
    if(!"February" %in% names(contracted)){
      contracted <- contracted %>% 
        mutate(February = January)
    }
    if(!"March" %in% names(contracted)){
      contracted <- contracted %>% 
        mutate(March = February)
    }
  }
  
  names(contracted) <- paste0(names(contracted), "_contracted")
  names(contracted)[names(contracted) == "geography_name_contracted"] <- "geography_name"
  
  # select delivered UDAs and add columns for remaining months of financial year
  # set delivered to blank for rest of financial year
  delivered <- output %>% 
    select(calendar_month, geography_name, UDAs_delivered_month) %>% 
    pivot_wider(names_from = calendar_month, values_from = UDAs_delivered_month)
  
  # if the month is not already in the columns, add a new column with 0 as value
  if(ncol(delivered) < 13){
    if(!"May" %in% names(delivered)){
      delivered <- delivered %>% 
        mutate(May = 0)
    }
    if(!"June" %in% names(delivered)){
      delivered <- delivered %>% 
        mutate(June = 0)
    }
    if(!"July" %in% names(delivered)){
      delivered <- delivered %>% 
        mutate(July = 0)
    }
    if(!"August" %in% names(delivered)){
      delivered <- delivered %>% 
        mutate(August = 0)
    }
    if(!"September" %in% names(delivered)){
      delivered <- delivered %>% 
        mutate(September = 0)
    }
    if(!"October" %in% names(delivered)){
      delivered <- delivered %>% 
        mutate(October = 0)
    }
    if(!"November" %in% names(delivered)){
      delivered <- delivered %>% 
        mutate(November = 0)
    }
    if(!"December" %in% names(delivered)){
      delivered <- delivered %>% 
        mutate(December = 0)
    }
    if(!"January" %in% names(delivered)){
      delivered <- delivered %>% 
        mutate(January = 0)
    }
    if(!"February" %in% names(delivered)){
      delivered <- delivered %>% 
        mutate(February = 0)
    }
    if(!"March" %in% names(delivered)){
      delivered <- delivered %>% 
        mutate(March = 0)
    }
  }
  
  names(delivered) <- paste0(names(delivered), "_delivered")
  names(delivered)[names(delivered) == "geography_name_delivered"] <- "geography_name"
  
  output <- contracted %>% 
    left_join(delivered, by = "geography_name")
  
  write.csv(output, 
            paste0(reports_dir, '/SMT_pack_extract_for_UDA_projections_data_up_to_', format(Sys.Date()-30, '%B%Y'), '.csv'), 
            row.names = FALSE)
}