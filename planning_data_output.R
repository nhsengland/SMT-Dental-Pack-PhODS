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
      filter(final_yn=='Y') %>%
      mutate(month = as.Date(month)) %>%
      filter(month >= as.Date ("2022-04-01")) %>%
      select(month, commissioner_ods_code_icb, annual_contracted_UDA, UDA_delivered)%>%
      group_by(month, commissioner_ods_code_icb) %>%
      summarise(UDA_delivered = sum(UDA_delivered, na.rm = TRUE),
                annual_contracted_UDA = sum(annual_contracted_UDA, na.rm = TRUE))
    
    # Create the 'Quarter' column with custom quarter assignment
    data <- data %>%
      mutate(month = as.Date(month),   # Convert 'month' to Date format if it isn't already
        month_num = month(month),   # Extract month number
        year = year(month),         # Extract year
        
        Quarter_End = case_when(    # Assign last month of the quarter
          month_num %in% 1:3  ~ "03",  # Jan–Mar → "YY-03"
          month_num %in% 4:6  ~ "06",  # Apr–Jun → "YY-06"
          month_num %in% 7:9  ~ "09",  # Jul–Sep → "YY-09"
          month_num %in% 10:12 ~ "12"  # Oct–Dec → "YY-12"
        ),
        
        Year_Quarter = paste0(substr(year, 3, 4), "-", Quarter_End)  # Format "YY-MM"
      )
    
    
    data <- data %>%
      group_by(Year_Quarter, commissioner_ods_code_icb) %>%
      filter(n_distinct(month_num) == 3) %>%  # Include only full quarters
      arrange(Year_Quarter, commissioner_ods_code_icb, month_num) %>%  # Sort properly
      summarise(monthly_UDA_UOAs_delivered = sum(UDA_delivered, na.rm = TRUE),
                annual_contracted_UDA_UOA = last(annual_contracted_UDA, na.rm = TRUE)  # Get the last month’s value
      ) %>%
      ungroup()
    
    #create Quarter for working days
    working_days <- working_days %>%
      mutate(
        month = as.Date(month),   # Convert 'month' to Date format if it isn't already
        month_num = month(month),   # Extract month number
        year = year(month),         # Extract year
        
        Quarter_End = case_when(    # Assign last month of the quarter
          month_num %in% 1:3  ~ "03",  # Jan–Mar → "YY-03"
          month_num %in% 4:6  ~ "06",  # Apr–Jun → "YY-06"
          month_num %in% 7:9  ~ "09",  # Jul–Sep → "YY-09"
          month_num %in% 10:12 ~ "12"  # Oct–Dec → "YY-12"
        ),
        
        Year_Quarter = paste0(substr(year, 3, 4), "-", Quarter_End)  # Format "YY-MM"
      ) %>%
      group_by(Year_Quarter) %>%
      summarise(
        `no workdays` = sum(`no workdays`, na.rm = TRUE),  # Sum of no workdays
        `total workdays` = last(`total workdays`, order_by = month_num, na.rm = TRUE)  # Get the latest total workdays
      )
    
    
    data <- data %>% 
      left_join(working_days,by=c('Year_Quarter')) %>%
      mutate(planning_ref='E.D.24',
        quarterly_contracted_UDA =annual_contracted_UDA_UOA*(`no workdays`/`total workdays`),
             perc_standardised_wd_int = 
               100*(monthly_UDA_UOAs_delivered /quarterly_contracted_UDA))%>% 
      arrange(desc(Year_Quarter))
    
    
    #rename columns
    new_col_names <- c("reporting_date" = "Year_Quarter",
                       "UDAs_delivered_quarter" = "monthly_UDA_UOAs_delivered",
                       "UDAs_contracted_quarter" = "quarterly_contracted_UDA",
                       "UDAs_delivered_quarter_percent_contracted_standardised" = "perc_standardised_wd_int")
    
     data <- data %>%
      rename(any_of(new_col_names))
    
    
data_ICB_UDA <- data %>%
  select(planning_ref,org_code=commissioner_ods_code_icb,
         reporting_date,value=UDAs_delivered_quarter_percent_contracted_standardised,
         numerator_value=UDAs_delivered_quarter,
         denominator_value =UDAs_contracted_quarter)%>%
  arrange(desc(reporting_date))



#Unique Patient Seen 
data_ICB_unique_quarterly <- pull_unique_patients() %>%
  filter(final_yn=='Y') %>%
  filter(month>"2020-03-01") %>%
  mutate(month = as.Date(month),   # Convert 'month' to Date format if it isn't already
    month_num = month(month),   # Extract month number
    year = year(month),         # Extract year
    
    Quarter_End = case_when(    # Assign last month of the quarter
      month_num %in% 1:3  ~ "03",  # Jan–Mar → "YY-03"
      month_num %in% 4:6  ~ "06",  # Apr–Jun → "YY-06"
      month_num %in% 7:9  ~ "09",  # Jul–Sep → "YY-09"
      month_num %in% 10:12 ~ "12"  # Oct–Dec → "YY-12"
    ),
    
    Year_Quarter = paste0(substr(year, 3, 4), "-", Quarter_End)  # Format "YY-MM"
  )%>%
  group_by(Year_Quarter,commissioner_ods_code_icb) %>%
  filter(n_distinct(month_num) == 3) %>%  # Keep only full quarters
  slice_max(month, n = 1, with_ties = FALSE) %>%  # Get latest month per group
  group_by(Year_Quarter, commissioner_ods_code_icb) %>%  # Re-group after slice_max()
  summarise(
    `child_12m_count` = sum(`child_12m_count`, na.rm = TRUE), 
    `adult_24m_count` = sum(`adult_24m_count`, na.rm = TRUE) ,
    `all_12m_count` = sum(`all_12m_count`, na.rm = TRUE)
  ) %>%
  select(Year_Quarter, commissioner_ods_code_icb, all_12m_count, child_12m_count, adult_24m_count) %>% 
  rename(ODS_CODE=commissioner_ods_code_icb,
         unique_patients_seen_12_month = all_12m_count, 
         unique_children_seen_12_month = child_12m_count, 
         unique_adults_seen_24_month = adult_24m_count, 
         Year_Quarter = Year_Quarter) %>% 
  arrange(desc(Year_Quarter))



population_ICB <- population_ICB %>%
  filter(month>"2020-04-01") %>%
  mutate(month = as.Date(month),   # Convert 'month' to Date format if it isn't already
    month_num = month(month),                   # Extract month number
    year = year(month),                         # Extract year
    
    Quarter_End = case_when(                    # Assign last month of the quarter
      month_num %in% 2:4  ~ "03",  # Feb–Apr  → "XX-03"
      month_num %in% 5:7  ~ "06",  # May–Jul  → "XX-06"
      month_num %in% 8:10 ~ "09",  # Aug–Oct  → "XX-09"
      month_num %in% 11:1 ~ "12"   # Nov–Jan  → "XX-12"
    ),
    
    Quarter_Year = case_when(                    # Assign year label
      month_num == 1 ~ as.character(year - 1),   # January belongs to the previous year's quarter
      TRUE ~ as.character(year)                  # Other months stay in their respective years
    ),
    
    Year_Quarter = paste0(substr(Quarter_Year, 3, 4), "-", Quarter_End)  # Format "YY-MM"
  ) %>%
  group_by(Year_Quarter,ODS_CODE=`ODS ICB Code`) %>%
  summarise(
    `children` = sum(`Age 0-17`, na.rm = TRUE), 
    `adults` = sum(`Age 18+`, na.rm = TRUE) ,
    `all` = sum(`Total`, na.rm = TRUE)
  )


data_ICB_unique_quarterly_adult <- data_ICB_unique_quarterly %>% 
  inner_join(population_ICB, by = c('Year_Quarter', 'ODS_CODE')) %>% 
  #filter(!is.na(children) & !is.na(adults) & !is.na(all)) %>% 
  group_by(reporting_date=Year_Quarter,org_code= ODS_CODE) %>% 
  summarise(planning_ref='E.D.22',
    unique_adults_seen_24_month= sum(`unique_adults_seen_24_month`, na.rm = TRUE),
    adult_populations= sum(`adults`, na.rm = TRUE),
    `% unique adult seen 24M` =100*(unique_adults_seen_24_month / adult_populations) ,
    .groups = "drop"  # Prevents unexpected grouping downstream
  )%>% select(planning_ref,org_code,reporting_date,value=`% unique adult seen 24M` ,
              numerator_value=unique_adults_seen_24_month,
              denominator_value=adult_populations)


data_ICB_unique_quarterly_children <- data_ICB_unique_quarterly %>% 
  inner_join(population_ICB, by = c('Year_Quarter', 'ODS_CODE')) %>% 
  #filter(!is.na(children) & !is.na(adults) & !is.na(all)) %>% 
  group_by(reporting_date=Year_Quarter,org_code= ODS_CODE) %>% 
  summarise(planning_ref='E.D.23',
    unique_children_seen_12_month= sum(`unique_children_seen_12_month`, na.rm = TRUE),
    children_population= sum(`children`, na.rm = TRUE),
    `% unique children seen 12M` =100*(unique_children_seen_12_month / children_population) ,
    .groups = "drop"  # Prevents unexpected grouping downstream
  )%>% select(planning_ref,org_code,reporting_date,value=`% unique children seen 12M` ,
              numerator_value=unique_children_seen_12_month,
              denominator_value=children_population)



###### Output #####
# create Excel file
# create function to run the output

create_export_file_quartly <- function(){
  output_file <- createWorkbook()

  addWorksheet(output_file, "ED22_unique adults")
  writeData(output_file, "ED22_unique adults", data_ICB_unique_quarterly_adult)

  addWorksheet(output_file, "ED23_unique children")
  writeData(output_file, "ED23_unique children", data_ICB_unique_quarterly_children)
  
  addWorksheet(output_file, "ED24 % UDA delivered")
  writeData(output_file, "ED24 % UDA delivered", data_ICB_UDA)
  
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