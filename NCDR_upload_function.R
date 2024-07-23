library(tidyverse)
library(odbc)
library(DBI)

con <- dbConnect(odbc::odbc(), "NCDR")

upload_data <- function(folder_name = "BPE", 
                        table_name = "Calendar_BPE"){
  
  sql <- paste(
    "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[", 
    table_name,
    "]", 
    sep = "")
  
  provisional_table_name <- paste(
    table_name,
    "_provisional_archived", 
    sep = "")
  
  folder <- paste(
    "N:/_Everyone/Primary Care Group/SMT_Dental Calendar data format/BSA Calendar data/",
    folder_name,
    sep = "")
  
  # read in existing table
  existing_table <- dbGetQuery(con, sql)
  
  # remove provisional rows
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
    filepath <- list.files(path = folder, 
                           pattern = "csv$", 
                           full.names = TRUE) %>%
      magrittr::extract(which.max(file.mtime(.)))
    
    # read in new file and reformat month
    new <- read.csv(filepath) %>% 
      mutate(YEAR_MONTH = as.Date(paste(substr(YEAR_MONTH, 1, 4), substr(YEAR_MONTH, 5, 6), "01", sep = "-")))
    
    # append new data to existing final data
    combined <- rbind(final, new)
    
    # write combined table to NCDR
    # dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts", 
    #                      schema="Dental",
    #                      table=table_name), 
    #              value = combined,
    #              row.names = FALSE, 
    #              append = TRUE, 
    #              overwrite = FALSE)
    
    # write provisional table to NCDR
    # dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts", 
    #                      schema="Dental",
    #                      table=provisional_table_name), 
    #              value = provisional,
    #              row.names = FALSE, 
    #              append = TRUE, 
    #              overwrite = FALSE)
  }
}