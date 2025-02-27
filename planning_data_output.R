library(tidyverse)
library(ggpubr)
library(scales)
library(readxl)
library(textclean)
library(lubridate)
library(openxlsx)


######UDA#####
#### No UDA delivered, contracted & percentage

    data = UDA_calendar_data
                                            
    #include data only after 2020-04-01 and select fields needed
    data <- data %>%
      mutate(month = as.Date(month)) %>%
      filter(month >= as.Date ("2020-04-01")) %>%
      select(month, contract_number, commissioner_name, region_name, annual_contracted_UDA, UDA_band_1,UDA_band_2,UDA_band_3,UDA_other,UDA_urgent,UDA_delivered)
    
    #get data into the right format and calculate the annual contracted UDA, delivered UDA and percentage of UDA delivered
    data <- get_delivery_data(data = data, UDAorUOA = "UDA", all_regions_and_STPs = TRUE)
    data <- data %>%
      filter(month>"2020-03-01")
    
    # Create the 'Quarter' column with custom quarter assignment
    data <- data %>%
      mutate(month_num = month(month),                                     # Extract month number
             year = year(month),                                           # Extract year
             fiscal_year_start = if_else(month_num >= 4, year, year - 1),  # Fiscal year start
             fiscal_year_end = fiscal_year_start + 1,                      # Fiscal year end
             fiscal_year_label = paste0(substr(fiscal_year_start, 3, 4), "/", substr(fiscal_year_end, 3, 4)), # e.g., "20/21"
             Quarter = case_when(                                          # Assign fiscal quarters
               month_num %in% 4:6 ~ "Q1",
               month_num %in% 7:9 ~ "Q2",
               month_num %in% 10:12 ~ "Q3",
               month_num %in% 1:3 ~ "Q4",
               TRUE ~ NA_character_
             ),
             Year_Quarter = paste0(fiscal_year_label, " ", Quarter)  )
    
    data <- data %>%
      group_by(Year_Quarter, commissioner_name) %>%
      summarise(monthly_UDA_UOAs_BAND1_delivered = sum(monthly_UDA_UOAs_BAND1_delivered, na.rm = TRUE),
                monthly_UDA_UOAs_BAND2_delivered = sum(monthly_UDA_UOAs_BAND2_delivered, na.rm = TRUE),
                monthly_UDA_UOAs_BAND3_delivered = sum(monthly_UDA_UOAs_BAND3_delivered, na.rm = TRUE),
                monthly_UDA_UOAs_BAND_OTHER_delivered = sum(monthly_UDA_UOAs_BAND_OTHER_delivered, na.rm = TRUE),
                monthly_UDA_UOAs_BAND_URGENT_delivered = sum(monthly_UDA_UOAs_BAND_URGENT_delivered, na.rm = TRUE),
                monthly_UDA_UOAs_delivered = sum(monthly_UDA_UOAs_delivered, na.rm = TRUE),
                annual_contracted_UDA_UOA = last(annual_contracted_UDA_UOA[order(month_num)], na.rm = TRUE))
    
    #create Quarter for working days
    working_days <- working_days %>%
      mutate(
        month_num = month(month),                                     # Extract month number
        year = year(month),                                           # Extract year
        fiscal_year_start = if_else(month_num >= 4, year, year - 1),  # Fiscal year start
        fiscal_year_end = fiscal_year_start + 1,                      # Fiscal year end
        fiscal_year_label = paste0(substr(fiscal_year_start, 3, 4), "/", substr(fiscal_year_end, 3, 4)), # e.g., "20/21"
        Quarter = case_when(                                          # Assign fiscal quarters
          month_num %in% 4:6 ~ "Q1",
          month_num %in% 7:9 ~ "Q2",
          month_num %in% 10:12 ~ "Q3",
          month_num %in% 1:3 ~ "Q4",
          TRUE ~ NA_character_
        ),
        Year_Quarter = paste0(fiscal_year_label, " ", Quarter)
      ) %>%
      group_by(Year_Quarter) %>%
      summarise(
        `no workdays` = sum(`no workdays`, na.rm = TRUE), # Sum of no workdays
        `total workdays` = last( `total workdays`[order(month_num)], na.rm = TRUE) # latest of total workdays
      )
    
    data <- data %>% 
      left_join(working_days,by=c('Year_Quarter')) %>%
      mutate(perc_standardised_wd_int = 
               100*(monthly_UDA_UOAs_delivered /(annual_contracted_UDA_UOA*(`no workdays`/`total workdays`))))%>% 
      mutate(perc_standardised_wd = round(perc_standardised_wd_int, digits = 0))
    
    #rename columns
    new_col_names <- c("Year_Quarter" = "Year_Quarter",
                       "UDAs_delivered_quarter" = "monthly_UDA_UOAs_delivered",
                       "UDAs_annual_contracted" = "annual_contracted_UDA_UOA",
                       "UDAs_delivered_quarter_percent_contracted_standardised" = "perc_standardised_wd")
    
     data <- data %>%
      rename(any_of(new_col_names))
    
    
data_ICB_UDA <- data %>%
  select(Year_Quarter,geography_name=commissioner_name,
         UDAs_annual_contracted,UDAs_delivered_quarter,UDAs_delivered_quarter_percent_contracted_standardised)%>%
  arrange(desc(Year_Quarter))



#Unique Patient Seen 
data_ICB_unique_quarterly <- pull_unique_patients() %>%
  filter(month>"2020-03-01") %>%
  mutate(
  month_num = month(month),                                     # Extract month number
  year = year(month),                                           # Extract year
  fiscal_year_start = if_else(month_num >= 4, year, year - 1),  # Fiscal year start
  fiscal_year_end = fiscal_year_start + 1,                      # Fiscal year end
  fiscal_year_label = paste0(substr(fiscal_year_start, 3, 4), "/", substr(fiscal_year_end, 3, 4)), # e.g., "20/21"
  Quarter = case_when(                                          # Assign fiscal quarters
    month_num %in% 4:6 ~ "Q1",
    month_num %in% 7:9 ~ "Q2",
    month_num %in% 10:12 ~ "Q3",
    month_num %in% 1:3 ~ "Q4",
    TRUE ~ NA_character_
  ),
  Year_Quarter = paste0(fiscal_year_label, " ", Quarter)
) %>%
  group_by(fiscal_year_label, Quarter,Year_Quarter,commissioner_name,commissioner_ods_code_icb) %>%
  filter(month == max(month)) %>%   
  summarise(
    `child_12m_count` = sum(`child_12m_count`, na.rm = TRUE), 
    `adult_24m_count` = sum(`adult_24m_count`, na.rm = TRUE) ,
    `all_12m_count` = sum(`all_12m_count`, na.rm = TRUE)
  ) %>%
  select(Year_Quarter, commissioner_name,commissioner_ods_code_icb, all_12m_count, child_12m_count, adult_24m_count) %>% 
  mutate(commissioner_name = str_to_title(commissioner_name), 
         commissioner_name = gsub("Icb", "ICB", commissioner_name)) %>% 
  rename(geography_name = commissioner_name,
         ODS_CODE=commissioner_ods_code_icb,
         unique_patients_seen_12_month = all_12m_count, 
         unique_children_seen_12_month = child_12m_count, 
         unique_adults_seen_24_month = adult_24m_count, 
         Year_Quarter = Year_Quarter) %>% 
  arrange(desc(Year_Quarter))



population_ICB <- population_ICB %>%
  filter(month>"2020-04-01") %>%
  mutate(
    month_num = month(month),                                     # Extract month number
    year = year(month),                                           # Extract year
    fiscal_year_start = if_else(month_num >= 4, year, year - 1),  # Fiscal year start
    fiscal_year_end = fiscal_year_start + 1,                      # Fiscal year end
    fiscal_year_label = paste0(substr(fiscal_year_start, 3, 4), "/", substr(fiscal_year_end, 3, 4)), # e.g., "20/21"
    Quarter = case_when(                                          # Assign fiscal quarters
      month_num %in% 5:7 ~ "Q1",
      month_num %in% 8:10 ~ "Q2",
      month_num %in% 11:1 ~ "Q3",
      month_num %in% 2:4 ~ "Q4",
      TRUE ~ NA_character_
    ),
    Year_Quarter = paste0(fiscal_year_label, " ", Quarter)
  ) %>%
  group_by(Year_Quarter,ODS_CODE=`ODS ICB Code`,geography_name =`ICB Name`) %>%
  summarise(
    `children` = sum(`Age 0-17`, na.rm = TRUE), 
    `adults` = sum(`Age 18+`, na.rm = TRUE) ,
    `all` = sum(`Total`, na.rm = TRUE)
  )


data_ICB_unique_quarterly <- data_ICB_unique_quarterly %>% 
  inner_join(population_ICB, by = c('Year_Quarter', 'ODS_CODE')) %>% 
  #filter(!is.na(children) & !is.na(adults) & !is.na(all)) %>% 
  group_by(Year_Quarter, geography_name.x) %>% 
  summarise(
    unique_children_seen_12_month= sum(`unique_children_seen_12_month`, na.rm = TRUE),
    children_population= sum(`children`, na.rm = TRUE),
    unique_adults_seen_24_month= sum(`unique_adults_seen_24_month`, na.rm = TRUE),
    adult_populations= sum(`adults`, na.rm = TRUE),
    unique_patients_seen_12_month= sum(`unique_patients_seen_12_month`, na.rm = TRUE),
    all_population= sum(`all`, na.rm = TRUE),
    `% unique children seen 12M` = round(100 * (unique_children_seen_12_month / children_population), 0),
    `% unique adult seen 24M` = round(100 * (unique_adults_seen_24_month / adult_populations), 0),
    `% unique patients seen 12M` = round(100 * (unique_patients_seen_12_month / all_population), 0),
    .groups = "drop"  # Prevents unexpected grouping downstream
  )



###### Output #####
# create Excel file
# create function to run the output

create_export_file <- function(){
  output_file <- createWorkbook()
  
  addWorksheet(output_file, "% UDA delivered")
  writeData(output_file, "% UDA delivered", data_ICB_UDA)
  
  addWorksheet(output_file, "% Unique Patients Seen")
  writeData(output_file, "% Unique Patients Seen", data_ICB_unique_quarterly)

  
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
  latest_month <- paste(month.name[as.numeric(substr(max(data_dental_activity$calendar_month), 6,7))], 
                        substr(max(data_dental_activity$calendar_month), 1,4), 
                        sep = " ")
  
  filename <- paste0(reports_dir, 
                     '/',
                     month(Sys.Date()), 
                     " ", 
                     year(Sys.Date()),
                     " Quaterly Data-ICB reporting up to end of ",
                     latest_month, 
                     '.xlsx')
  
  openxlsx::saveWorkbook(output_file, file = filename, overwrite = TRUE)
}