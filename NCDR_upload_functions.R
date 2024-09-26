# load packages
library(tidyverse)
library(odbc)
library(DBI)
library(readxl)

# connect to NCDR
con <- dbConnect(odbc::odbc(), "NCDR")

#### CALENDAR DATA ####
# Upload downloaded BSA data to NCDR
upload_data_to_backup <- function(folder_name = "BPE", 
                        table_name = "Calendar_BPE"){
  
  # set full SQL table name
  table_name_full <- paste(
    "[NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[", 
    table_name,
    "]", 
    sep = "")
  
  # set table name for backup
  table_name_backup <- paste(
    table_name, 
    "backup", 
    sep = "_"
  )
  
  # SQL query
  sql <- paste(
    "SELECT * FROM ", 
    table_name_full, 
    sep = "")
  
  # set table name for archive of provisional data
  provisional_table_name <- paste(
    table_name,
    "_provisional_archived", 
    sep = "")
  
  # set file path for new data
  folder <- paste(
    "N:/_Everyone/Primary Care Group/SMT_Dental Calendar data format/BSA Calendar data/",
    folder_name,
    sep = "")
  
  # read in existing table
  print(paste("Reading table from NCDR:", table_name, sep = " "))
  existing_table <- dbGetQuery(con, sql)
  
  # if the latest expected month is already in the existing table, skip updating this table
  if(latest_expected_month %in% unique(existing_table$YEAR_MONTH)){
    print(paste("Table already updated, skipping:", table_name, sep = " "))
  }
  else {
    # remove provisional rows
    print("Removing provisional rows")
    final <- existing_table %>% 
      filter(FINAL_YN == "Y")
  
    # select provisional rows and add today's date
    provisional <- existing_table %>% 
      filter(FINAL_YN == "N") %>% 
      mutate(date_removed = Sys.Date())
  
    # return an error message if the number of rows in the existing table isn't equal to the final plus provisional tables
    # else continue with upload process
    if(nrow(existing_table) != nrow(final) + nrow(provisional)){
    
      warning("ERROR: number of rows in existing table isn't equal to final plus provisional. Check the split by FINAL_YN has worked correctly.")
    
    } else {
    
      # find filepath of new file
      print(paste("Reading latest file from:", folder, sep = " "))
      filepath <- list.files(path = folder, 
                             pattern = "csv$", 
                             full.names = TRUE) %>%
        magrittr::extract(which.max(file.mtime(.)))
    
      # read in new file and reformat month
      new <- read.csv(filepath) %>% 
        mutate(YEAR_MONTH = as.Date(paste(substr(YEAR_MONTH, 1, 4), substr(YEAR_MONTH, 5, 6), "01", sep = "-")))
    
      # append new data to existing final data
      combined <- rbind(final, new)
      
      print(paste("Writing table to NCDR:", table_name_backup, sep = " "))
    
      # write combined table to NCDR backup
      dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",
                           schema="Dental",
                           table=table_name_backup),
                   value = combined,
                   row.names = FALSE,
                   append = FALSE,
                   overwrite = TRUE)
    
      # write provisional table to NCDR
      dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",
                           schema="Dental",
                           table=provisional_table_name),
                   value = provisional,
                   row.names = FALSE,
                   append = TRUE,
                   overwrite = FALSE)
    }
  }
}

upload_tested_data <- function(folder_name = "BPE", 
                               table_name = "Calendar_BPE"){
  
  # set full table name for backup
  table_name_backup_full <- paste(
    "[NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[",
    table_name,
    "_backup]", 
    sep = ""
  )
  
  # SQL query
  sql <- paste(
    "SELECT * FROM ", 
    table_name_backup_full, 
    sep = "")
  
  # read in backup table
  print(paste("Reading table from NCDR: ", table_name, "_backup", sep = ""))
  backup_table <- dbGetQuery(con, sql)
  
  # write table to NCDR 
  print(paste("Writing table to NCDR:", table_name, sep = " "))
  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",
                       schema="Dental",
                       table=table_name),
               value = backup_table,
               row.names = FALSE,
               append = FALSE,
               overwrite = TRUE)
}

#### SCHEDULED DATA ####
# upload scheduled data previously used for packs
# unique patients seen
import_and_clean_unique_patients_data <- function(data_path,
                                                  commissioner_lookup = STP_ICB_lookup_codes){
  
  unique_patients <- readxl::read_excel(data_path,
                                        skip = 19)
  
  #fixes column names
  unique_patients <- unique_patients %>%
    mutate(month_ending = data_month) %>%
    rename(contract_number = "Contract Number",
           commissioner_ods_code_icb = "Latest Commissioner Code",
           unique_patients_rolling_12M = "Unique Patient Count Rolling 12M",
           band1_unique_patients_rolling_12M = "Band 1 Unique Patient Count Rolling 12M",
           band2_or_3_unique_patients_rolling_12M = "Band 2 or Band 3 Unique Patient Count Rolling 12M",
           band1_urgent_unique_patients_rolling_12M = "Band 1 Urgent Unique Patient Count Rolling 12M",
           band_other_unique_patients_rolling_12M = "Other Unique Patient Count Rolling 12M") %>%
    filter(!is.na(contract_number))
  
  #join in commissioner name
  commissioner_lookup <- commissioner_lookup %>%
    select(commissioner_ods_code_icb = commissioner_ONS_boundary_code_ICB, 
           commissioner_name = commissioner_name_ICB, 
           region_name) %>%
    distinct()
  
  unique_patients <- unique_patients %>%
    left_join(commissioner_lookup, by = "commissioner_ods_code_icb")
  
}

# scheduled UDA
#N.B. this assumes that the columns are in the same order each time!
import_and_clean_scheduled_UDA_data <- function(data_path,
                                                data_date,
                                                commissioner_lookup = STP_ICB_lookup_codes){
  
  #read in data with correct types and removing top 3 rows and renaming columns 
  data <- read_excel(data_path,
                     col_types = c("numeric", "text", "text",
                                   "text", "text", "date", "date", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric"), 
                     skip = 3,
                     col_names = TRUE,
                     .name_repair = ~ paste0("X__", seq_along(.x)))
  
  #check format of data - manual check should be done to see if columns are in the same order as expected
  #this will stop the process if the number of columns is not as expected
  if(ncol(data) != 46){
    warning("WARNING: The data you have loaded is not in the usual format. Please check.")
  } else{
    #add column for date and rename columns
    data <- data %>%
      mutate(data_month = data_date) %>%
      select(data_month, everything()) %>%
      rename(contract_number = X__1,
             name_or_company_name = X__2,
             commissioner_name = X__3,
             contract_type = X__4,
             paid_by_BSA = X__5,
             contract_start_date = X__6,
             contract_end_date = X__7,
             annual_contracted_UDA = X__8,
             annual_contracted_UOA = X__9,
             UDA_delivered = X__11,
             general_FP17s = X__12,
             UDA_band_1 = X__17,
             UDA_band_2 = X__18,         
             UDA_band_3 = X__19,
             UDA_urgent = X__20,
             UDA_other = X__21,
             FP17s_band_1 = X__22,
             FP17s_band_2 = X__23,
             FP17s_band_3 = X__24,
             FP17s_band_urgent = X__25,
             FP17s_band_other = X__26) %>%
      select(-starts_with("X__")) %>%
      filter(!is.na(contract_number))
    
    #join in commissioner name
    commissioner_lookup <- commissioner_lookup %>%
      select(commissioner_ods_code_icb = commissioner_ONS_boundary_code_ICB, 
             commissioner_name = commissioner_name_ICB, 
             region_name) %>%
      distinct()
    
    data <- data %>%
      left_join(commissioner_lookup, by = "commissioner_name")
  }
}

# scheduled UOA
#N.B. this assumes that the columns are in the same order each time!
import_and_clean_scheduled_UOA_data <- function(data_path,
                                                data_date,
                                                commissioner_lookup = STP_ICB_lookup_codes){
  
  #read in data with correct types and removing top 3 rows and renaming columns
  data <- read_excel(data_path,
                     col_types = c("numeric", "text", "text",
                                   "text", "text", "date", "date", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric"),
                     skip = 3,
                     col_names = TRUE,
                     .name_repair = ~ paste0("X__", seq_along(.x)))
  
  #check format of data - manual check should be done to see if columns are in the same order as expected
  #this will stop the process if the number of columns is not as expected
  if(ncol(data) != 18){
    warning("WARNING: The data you have loaded is not in the usual format. Please check.")
  } else{
    #add column for date and rename columns
    data <- data %>%
      mutate(data_month = data_date) %>%
      select(data_month, everything()) %>%
      rename(contract_number = X__1,
             contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             paid_by_BSA = X__5,
             contract_start_date = X__6,
             contract_end_date = X__7,
             annual_contracted_UOA = X__8,
             annual_contracted_UDA = X__9,
             UOA_delivered = X__11,
             orthodontic_FP17s = X__14,
             orthodontic_starts = X__17,
             orthodontic_completions = X__18) %>%
      select(-starts_with("X__")) %>%
      filter(!is.na(contract_number))
    
    #join in commissioner code
    commissioner_lookup <- commissioner_lookup %>%
      select(commissioner_ods_code_icb = commissioner_ONS_boundary_code_ICB,
             commissioner_name = commissioner_name_ICB,
             region_name) %>%
      distinct()
    
    data <- data %>%
      left_join(commissioner_lookup, by = "commissioner_name")
  }
}

# upload UDA, UOA and unique patients
upload_UDA_scheduled_data <- function(UDA_latest){
  
  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="UDA_scheduled"),
               value = UDA_latest, row.names = FALSE, append=TRUE)
}

upload_UOA_scheduled_data <- function(UOA_latest){
  
  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="UOA_scheduled"),
               value = UOA_latest, row.names = FALSE, append=TRUE)
}

upload_unique_patients_data <- function(unique_patients){
  
  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="unique_patients_rolling_12_month"),
               value = unique_patients, row.names = FALSE, append = TRUE)
}
