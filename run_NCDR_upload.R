source("NCDR_upload_function.R")

folder_name <- c("BPE", "Contract", "NPP", "UDA Activity", "UDA Activity DCP", "UDA Activity FD only", "Unique rolling")

table_name <- c("Calendar_BPE", "Calendar_Contracts", "Calendar_NPP_Eligible_Activity", "Calendar_UDA_Activity", 
                 "Calendar_DCP", "Calendar_UDA_Activity_FD_only", "Calendar_Unique_rolling")

mapply(upload_data, folder_name, table_name)
