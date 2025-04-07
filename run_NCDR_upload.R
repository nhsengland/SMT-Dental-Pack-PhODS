source("NCDR_upload_functions.R")

## MANUAL INPUTS ##
# each month, update to the next month
latest_expected_month <- "2025-03-01"

#### CALENDAR - initial ####
folder_name <- c("BPE", "Contract", "NPP", "UDA Activity", "UDA Activity DCP", "UDA Activity FD only", "Unique rolling", "UOA Activity")

table_name <- c("Calendar_BPE", "Calendar_Contracts", "Calendar_NPP_Eligible_Activity", "Calendar_UDA_Activity", 
                 "Calendar_DCP", "Calendar_UDA_Activity_FD_only", "Calendar_Unique_rolling", "Calendar_UOA_Activity")

mapply(upload_data_to_backup, folder_name, table_name)

#### RUN UPLOAD TESTS ####
# run tests in file upload_tests.R

#### CALENDAR - final ####
# Only run once you are happy with the data
folder_name <- c("BPE", "Contract", "NPP", "UDA Activity", "UDA Activity DCP", "UDA Activity FD only", "Unique rolling", "UOA Activity")

table_name <- c("Calendar_BPE", "Calendar_Contracts", "Calendar_NPP_Eligible_Activity", "Calendar_UDA_Activity", 
                "Calendar_DCP", "Calendar_UDA_Activity_FD_only", "Calendar_Unique_rolling", "Calendar_UOA_Activity")

mapply(upload_tested_data, folder_name, table_name)

#### SCHEDULED ####
#load in lookup
STP_ICB_lookup_codes <- read_excel("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/data_for_monthly_report/STP_ICB_lookup_codes.xlsx")

#gets latest month 
data_month <- lubridate::floor_date(Sys.Date()  - lubridate::weeks(4), unit = "month")
data_month_name <- format(Sys.Date()  - lubridate::weeks(4), format = "%b%y")

#gets paths of data to be loaded in
raw_UDA_scheduled_data_folder_path <- "N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/raw_eDEN_dental_data/UDA_scheduled_raw_data/"
raw_UOA_scheduled_data_folder_path <- "N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/raw_eDEN_dental_data/UOA_scheduled_raw_data/"
raw_unique_scheduled_data_folder_path <- "N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/unique_patients/"

#gets latest cleaned data ready to be uploaded
UDA_scheduled_data_latest <- import_and_clean_scheduled_UDA_data(data_path = paste0(raw_UDA_scheduled_data_folder_path, "UDA_scheduled_", data_month_name,".xlsx"),
                                                                 data_date = data_month)
UOA_scheduled_data_latest <- import_and_clean_scheduled_UOA_data(data_path = paste0(raw_UOA_scheduled_data_folder_path, "UOA_scheduled_", data_month_name,".xlsx"),
                                                                 data_date = data_month)
unique_patients_latest <- import_and_clean_unique_patients_data(data_path = paste0(raw_unique_scheduled_data_folder_path, "unique_patients_rolling_", data_month_name,".xlsx"))


# upload data
upload_UDA_scheduled_data(UDA_latest = UDA_scheduled_data_latest)
upload_UOA_scheduled_data(UOA_latest = UOA_scheduled_data_latest)
upload_unique_patients_data(unique_patients = unique_patients_latest)