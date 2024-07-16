library(tidyverse)
library(ggplot2)
library(ggpubr)
library(stringr)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(scales)
library(readxl)
library(textclean)
library(lubridate)
library(openxlsx)

#### get raw data -- do not need if using auto-render
source(knitr::purl("SQLpulls.Rmd", output = tempfile()), local = TRUE)  
source(knitr::purl("Data_Processing.Rmd", output = tempfile()))  
source(knitr::purl("plotting.Rmd", output = tempfile()))  
source("ExportDataFile_metadata.R")


######UDA
#### No UDA delivered, contracted & percentage
data_Nat_UDA=plot_UDA_UOA_delivery_wd(data = UDA_calendar_data, 
                         UDAorUOA = "UDA",
                         plotChart = FALSE, 
                         all_regions_and_STPs = FALSE,
                         level = "National",
                         region_STP_name = NULL)

data_Nat_UDA<- data_Nat_UDA %>% 
  select(`Calendar month`,`Annual contracted UDAs`,
         `Monthly UDAs Band1 delivered`,`Monthly UDAs Band2 delivered`,`Monthly UDAs Band3 delivered`,`Monthly UDAs Band Urgent delivered`,
         `Monthly UDAs Band Other delivered`,`Monthly UDAs total delivered`,`no workdays`,`Standardised monthly percentage of contracted UDAs delivered`)%>%
  mutate(`Geography Level`='National',`Geography Name`='England')

data_reg_UDA=plot_UDA_UOA_delivery_all_regions(data = UDA_calendar_data, 
                                  UDAorUOA = "UDA",
                                  level = "National",
                                  region_STP_name = NULL,
                                  plotChart = FALSE, 
                                  all_regions_and_STPs = TRUE)
data_reg_UDA <- data_reg_UDA %>%
  select(`Calendar month`,`Geography Name`=`Region Name`,`Annual contracted UDAs`,`Monthly UDAs Band1 delivered`,`Monthly UDAs Band2 delivered`,
         `Monthly UDAs Band3 delivered`,`Monthly UDAs Band Urgent delivered`,
         `Monthly UDAs Band Other delivered`,`Monthly UDAs total delivered`,`no workdays`,
         `Standardised monthly percentage of contracted UDAs delivered`)%>%
  mutate(`Geography Level`='Regional')

data_ICB_UDA <- Table_UDA_UOA_delivery_all_ICBs(data = UDA_calendar_data, 
                                UDAorUOA = "UDA")

data_ICB_UDA <- data_ICB_UDA %>%
  select(`Calendar month`,`Geography Name`=`Commissioner Name`,`Annual contracted UDAs`,`Monthly UDAs Band1 delivered`,
         `Monthly UDAs Band2 delivered`,`Monthly UDAs Band3 delivered`,`Monthly UDAs Band Urgent delivered`,
         `Monthly UDAs Band Other delivered`,`Monthly UDAs total delivered`,`no workdays`,
         `Standardised monthly percentage of contracted UDAs delivered`)%>%
  mutate(`Geography Level`='ICB')

data_UDA_de_co<- rbind(data_Nat_UDA, data_reg_UDA, data_ICB_UDA) %>% 
  rename("Workdays" = `no workdays`)%>%
  select("Calendar month","Geography Level","Geography Name","Annual contracted UDAs",
         #"Monthly UDAs Band1 delivered" ,"Monthly UDAs Band2 delivered","Monthly UDAs Band3 delivered",
         #"Monthly UDAs Band Urgent delivered",
         "Monthly UDAs total delivered" ,"Monthly UDAs total delivered" ,
          "Workdays","Standardised monthly percentage of contracted UDAs delivered")

####YTD delivery
data_Nat_YTD_UDA<- Table_YTD_UDA_UOA_delivery (data = UDA_calendar_data, 
                                       UDAorUOA = "UDA",
                                       level = "National",
                                       all_regions_and_STPs = FALSE)

data_Nat_YTD_UDA<- data_Nat_YTD_UDA %>% 
  mutate(`Geography Level`='National',`Geography Name`='England')

data_reg_YTD_UDA<- Table_YTD_UDA_UOA_delivery (data = UDA_calendar_data, 
                                               UDAorUOA = "UDA",
                                               level = "Regional",
                                               all_regions_and_STPs =TRUE)
data_reg_YTD_UDA <- data_reg_YTD_UDA %>%
  rename(`Geography Name`=`region_name`)%>%
  mutate(`Geography Level`='Regional')

data_icb_YTD_UDA<- Table_YTD_UDA_UOA_delivery (data = UDA_calendar_data, 
                                               UDAorUOA = "UDA",
                                               level = "STP",
                                               all_regions_and_STPs =TRUE)
data_icb_YTD_UDA <- data_icb_YTD_UDA %>%
  rename(`Geography Name`=`commissioner_name`)%>%
  mutate(`Geography Level`='ICB')

data_UDA_YTD<- rbind(data_Nat_YTD_UDA, data_reg_YTD_UDA, data_icb_YTD_UDA)

#### Banded CoTs
data_Nat_CoT <- table_banded_CoT(data = UDA_calendar_data_FD, 
                                 level = "National", 
                                 all_regions_and_STPs = FALSE)

data_Nat_CoT <- data_Nat_CoT %>% 
  rowwise() %>% 
  mutate(`Total CoT incl FD` = sum(band1, band2, band3, other, urgent, na.rm = TRUE)) %>%
  mutate(`Geography Level`='National',`Geography Name`='England') %>% 
  rename(`Calendar month` = month, 
         `Band 1 CoT incl FD` = band1, 
         `Band 2 CoT incl FD` = band2, 
         `Band 3 CoT incl FD` = band3, 
         `Other CoT incl FD` = other, 
         `Urgent CoT incl FD` = urgent)

data_reg_CoT <- table_banded_CoT(data = UDA_calendar_data_FD, 
                                 level = "Regional", 
                                 all_regions_and_STPs = TRUE)

# Convert relevant columns to numeric and handle coercion to NA for non-numeric values
data_reg_CoT <- data_reg_CoT %>%
  mutate(across(c(band1, band2, band3, other, urgent), ~ as.numeric(as.character(.))))

# Create the Total FP17s column by summing the specified numeric columns
data_reg_CoT <- data_reg_CoT %>%
  mutate(`Total CoT incl FD` = rowSums(across(c(band1, band2, band3, other, urgent)), na.rm = TRUE))%>%
  mutate(`Geography Level` = "Regional") %>%
  rename(`Geography Name` = region_name, 
         `Calendar month` = month, 
         `Band 1 CoT incl FD` = band1, 
         `Band 2 CoT incl FD` = band2, 
         `Band 3 CoT incl FD` = band3, 
         `Other CoT incl FD` = other, 
         `Urgent CoT incl FD` = urgent)

data_icb_CoT <- table_banded_CoT(data = UDA_calendar_data_FD,
                                 level = "STP", 
                                 all_regions_and_STPs = TRUE)

# Convert relevant columns to numeric, handling non-numeric values by coercing them to NA
data_icb_CoT <- data_icb_CoT %>%
  mutate(across(c(band1, band2, band3, other, urgent), ~ as.numeric(as.character(.))))

# If the data is grouped, ungroup it
data_icb_CoT <- data_icb_CoT %>% ungroup()

# Create the Total FP17s column by summing the specified numeric columns
data_icb_CoT <- data_icb_CoT %>%
  mutate(`Total CoT incl FD` = rowSums(select(., band1, band2, band3, other, urgent), na.rm = TRUE))%>%
  mutate(`Geography Level` = "ICB") %>% 
  rename(`Geography Name` = commissioner_name, 
         `Calendar month` = month, 
         `Band 1 CoT incl FD` = band1, 
         `Band 2 CoT incl FD` = band2, 
         `Band 3 CoT incl FD` = band3, 
         `Other CoT incl FD` = other, 
         `Urgent CoT incl FD` = urgent)

data_CoT <- rbind(data_Nat_CoT, data_reg_CoT, data_icb_CoT)

######UOA
#### No UDA delivered, contracted & percentage
#data_Nat_UOA=plot_UDA_UOA_delivery_wd(data = UOA_calendar_data, 
#                                      UDAorUOA = "UOA",
#                                      plotChart = FALSE, 
#                                      all_regions_and_STPs = FALSE,
#                                      level = "National",
#                                      region_STP_name = NULL)

#data_Nat_UOA<- data_Nat_UOA %>% 
#  select(`Calendar month`,`Annual contracted UOAs`,`Monthly UOAs delivered`,`no workdays`,`Standardised monthly percentage of contracted UOAs delivered`)%>%
#  mutate(`Geography Level`='National',`Geography Name`='England')

#data_reg_UOA=plot_UDA_UOA_delivery_all_regions(data = UOA_calendar_data, 
#                                               UDAorUOA = "UOA",
#                                               level = "National",
#                                               region_STP_name = NULL,
#                                               plotChart = FALSE, 
#                                               all_regions_and_STPs = TRUE)
#data_reg_UOA <- data_reg_UOA %>%
# select(`Calendar month`,`Geography Name`=`Region Name`,`Annual contracted UOAs`,`Monthly UOAs delivered`,
#         `no workdays`,`Standardised monthly percentage of contracted UOAs delivered`)%>%
#  mutate(`Geography Level`='Regional')

#data_ICB_UOA <- Table_UDA_UOA_delivery_all_ICBs(data = UOA_calendar_data, 
#                                                UDAorUOA = "UOA")

#data_ICB_UOA <- data_ICB_UOA %>%
#  select(`Calendar month`,`Geography Name`=`Commissioner Name`,`Annual contracted UOAs`,`Monthly UOAs delivered`,
#         `no workdays`,`Standardised monthly percentage of contracted UOAs delivered`)%>%
#  mutate(`Geography Level`='ICB')

#data_UOA_de_co<- rbind(data_Nat_UOA, data_reg_UOA, data_ICB_UOA) %>% 
#  rename("Workdays" = `no workdays`)%>%
#  select("Calendar month","Geography Level","Geography Name","Annual contracted UOAs","Monthly UOAs delivered",
#         "Workdays","Standardised monthly percentage of contracted UOAs delivered")

####YTD delivery
#data_Nat_YTD_UOA<- Table_YTD_UDA_UOA_delivery (data = UOA_calendar_data, 
#                                               UDAorUOA = "UOA",
#                                               level = "National",
#                                               all_regions_and_STPs = FALSE)
#data_Nat_YTD_UOA<- data_Nat_YTD_UOA %>% 
#  mutate(`Geography Level`='National',`Geography Name`='England')

#data_reg_YTD_UOA<- Table_YTD_UDA_UOA_delivery (data = UOA_calendar_data, 
#                                               UDAorUOA = "UOA",
#                                               level = "Regional",
#                                               all_regions_and_STPs =TRUE)
#data_reg_YTD_UOA <- data_reg_YTD_UOA %>%
#  rename(`Geography Name`=`region_name`)%>%
#  mutate(`Geography Level`='Regional')

#data_icb_YTD_UOA<- Table_YTD_UDA_UOA_delivery (data = UOA_calendar_data, 
#                                               UDAorUOA = "UOA",
#                                               level = "STP",
#                                               all_regions_and_STPs =TRUE)
#data_icb_YTD_UOA <- data_icb_YTD_UOA %>%
#  rename(`Geography Name`=`commissioner_name`)%>%
#  mutate(`Geography Level`='ICB')

#data_UOA_YTD<- rbind(data_Nat_YTD_UOA, data_reg_YTD_UOA, data_icb_YTD_UOA)

#data_UOA<-data_UOA_de_co%>%
#  full_join(data_UOA_YTD,by=c('Calendar month','Geography Name','Geography Level'))


# data<- list('UDA' = data_UDA, 'UOA' = data_UOA)
# 
# openxlsx::write.xlsx(data, file = paste0('Data_', format(Sys.Date(), '%B%Y'), '.xlsx')) 


### Unique patients seen ###
# Unique patients rolling
# get the right data -- sum unique patient seen for general population/adult/child 
data_Nat_unique <- get_unique_patients() %>% 
  mutate(month = format(as.Date(month), "%Y-%m"), 
         `Geography Level`='National',`Geography Name`='England') %>% 
  rename(`Unique patients seen in 12 month rolling period` = all_12m_count, 
         `Children seen in 12 month rolling period` = child_12m_count, 
         `Adults seen in 24 month rolling period` = adult_24m_count, 
         `Calendar month` = month)

# data_icb_unique <- get_unique_patients(all_regions_and_STPs = TRUE) %>% 
#   group_by(month, commissioner_name) %>% 
#   dplyr::summarise(all_12m_count = sum(all_12m_count, na.rm = TRUE),
#                    child_12m_count = sum(child_12m_count, na.rm = TRUE), 
#                    adult_24m_count = sum(adult_24m_count, na.rm = TRUE)) %>% 
#   mutate(month = format(as.Date(month), "%Y-%m"), 
#          commissioner_name = str_to_title(commissioner_name), 
#          commissioner_name = replace(commissioner_name, commissioner_name == "Icb", "ICB"),
#          `Geography Level` = "ICB") %>% 
#   rename(`Unique patients seen in 12 month rolling period` = all_12m_count, 
#          `Children seen in 12 month rolling period` = child_12m_count, 
#          `Adults seen in 24 month rolling period` = adult_24m_count, 
#          `Geography Name` = commissioner_name, 
#          `Calendar month` = month)

# change Icb to ICB so joins work
# data_icb_unique$`Geography Name` <- substr(data_icb_unique$`Geography Name`, 1, 
#                                            nchar(data_icb_unique$`Geography Name`)-3)
# data_icb_unique$`Geography Name` <- paste0(data_icb_unique$`Geography Name`, "ICB", sep = "")

# data_unique <- rbind(data_Nat_unique, data_reg_unique, data_icb_unique)

data_unique <- data_Nat_unique

data_dental_activity<-data_UDA_de_co%>%
  full_join(data_UDA_YTD,by=c('Calendar month','Geography Name','Geography Level'))%>%
  left_join(data_CoT, by = c('Calendar month', 'Geography Name', 'Geography Level')) %>% 
  left_join(data_unique, by = c('Calendar month', 'Geography Name', 'Geography Level'))%>%
  select("Calendar month","financial_year", "Geography Level","Geography Name", "Annual contracted UDAs",
         "UDAs total delivered exc FD"="Monthly UDAs total delivered",
         "Standardised monthly percentage of contracted UDAs delivered",                                              
         "YTD_delivery_excl_FD"="YTD_delivery" ,"Band 1 CoT incl FD","Band 2 CoT incl FD","Band 3 CoT incl FD","Urgent CoT incl FD" ,                                            
         "Other CoT incl FD","Total CoT incl FD","Unique patients seen in 12 month rolling period", "Children seen in 12 month rolling period",
         "Adults seen in 24 month rolling period")


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
    mutate(DCP_description = replace(DCP_description, DCP_description== "Nurse", "Dental_Nurse_assisted"), 
           DCP_description = replace(DCP_description, DCP_description=="Dental Hygienist", "Hygienist_assisted"), 
           DCP_description = replace(DCP_description, DCP_description=="Dental Therapist", "Therapist_assisted"),
           DCP_description = replace(DCP_description, DCP_description=="Hygienist", "Hygienist_assisted"), 
           DCP_description = replace(DCP_description, DCP_description=="Therapist", "Therapist_assisted"))
  
  dcp_summary_national <- dcp_main_new%>% 
    group_by(month, DCP_description) %>%
    dplyr::summarise (completed_courses_of_treatment = sum(FP17_Current_Year_total, na.rm = TRUE),
                      UDA_B1 = sum(Band_1._UDA, na.rm = TRUE),
                      UDA_B2 = sum(Band_2._UDA, na.rm = TRUE),
                      UDA_B3 = sum(Band_3._UDA, na.rm = TRUE),
                      UDA_urgent = sum(Urgent_UDA, na.rm = TRUE))
  
  dcp_summary_regional <- dcp_main_new%>% 
    group_by(month, Region, DCP_description) %>%
    dplyr::summarise (completed_courses_of_treatment = sum(FP17_Current_Year_total, na.rm = TRUE),
                      UDA_B1 = sum(Band_1._UDA, na.rm = TRUE),
                      UDA_B2 = sum(Band_2._UDA, na.rm = TRUE),
                      UDA_B3 = sum(Band_3._UDA, na.rm = TRUE),
                      UDA_urgent = sum(Urgent_UDA, na.rm = TRUE))
  
  
    dcp_summary_icb <- dcp_main_new%>% 
    group_by(month, commissioner_name, DCP_description) %>%
      dplyr::summarise (completed_courses_of_treatment = sum(FP17_Current_Year_total, na.rm = TRUE),
                        UDA_B1 = sum(Band_1._UDA, na.rm = TRUE),
                        UDA_B2 = sum(Band_2._UDA, na.rm = TRUE),
                        UDA_B3 = sum(Band_3._UDA, na.rm = TRUE),
                        UDA_urgent = sum(Urgent_UDA, na.rm = TRUE))
    
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
      mutate (asissted_percent = formattable::percent (numbers / all_numbers, digits=2))%>%
      mutate(`Geography Name`='England',`Geography Level`='National')
    
    
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
      mutate (asissted_percent = formattable::percent (numbers / all_numbers, digits=2))%>%
      rename(`Geography Name`=`Region`)%>%
      mutate(`Geography Level`='Region')
  
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
    mutate (asissted_percent = formattable::percent (numbers / all_numbers, digits=2))%>%
    rename(`Geography Name`=`commissioner_name`)%>%
    mutate(`Geography Level`='ICB')
  
  total_dcp<- rbind(total_national, total_regional, total_icb) %>% 
    rename(assisted_percent = asissted_percent)%>%
    mutate(`month` = format(as.Date(month), "%Y-%m"))%>%
    select('Calendar month'='month',"Geography Level","Geography Name","DCP metric (excl FD)"="DCP_metric","DCP description"="DCP_description.x","numbers",
           "DCP_description.y", "all_numbers","assisted_percent")

# create Excel file
output_file <- createWorkbook()

addWorksheet(output_file, "Dental contract and activity")
writeData(output_file, "Dental contract and activity", data_dental_activity)

addWorksheet(output_file, "DCP")
writeData(output_file, "DCP", total_dcp)

addWorksheet(output_file, "Metadata")
writeData(output_file, "Metadata", metadata)
setColWidths(output_file, "Metadata", cols = 1:3, widths = "auto")
  
# specify tabs
# data<- list('Dental contract and activity' = data_dental_activity, 
#             'DCP' = total_dcp, 
#             'Metadata' = metadata)


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
  