library(openxlsx)
library(DBI)
library(magrittr) 
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(tidyr)

con <- dbConnect(odbc::odbc(), "NCDR")

############ Extract data from NCDR ---- excluding 5 contracts that are not urgent care for north west

sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts_urgent_700000] 
WHERE CONTRACT_NUMBER NOT IN ('1004930000','1004950000','1005030000','1005050000','6962930001')" 
result <- dbSendQuery(con, sql)
u7_contract <- dbFetch(result)
dbClearResult(result)

sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000]
WHERE CONTRACT_NUMBER NOT IN ('1004930000','1004950000','1005030000','1005050000','6962930001')"
result <- dbSendQuery(con, sql)
u7_UDA <- dbFetch(result)
dbClearResult(result)

sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000]
WHERE CONTRACT_NUMBER NOT IN ('1004930000','1004950000','1005030000','1005050000','6962930001')"
result <- dbSendQuery(con, sql)
u7_FDonly <- dbFetch(result)
dbClearResult(result)

sql <- "SELECT DISTINCT [Region_Name],[STP_Code]
  FROM [NHSE_Reference].[dbo].[tbl_Ref_ODS_Commissioner_Hierarchies]
  WHERE STP_Code NOT IN ('DUM001', 'DUM002', 'UNK','X24')"
result <- dbSendQuery(con, sql)
ICB_region <- dbFetch(result)
dbClearResult(result)

############ Process data ----
#prep master data as ready to be filtered by ICB and region

master<-u7_UDA%>%
  select(YEAR_MONTH, 
         COMMISSIONER_CODE, 
         COMMISSIONER_NAME, 
         CONTRACT_NUMBER, 
         URGENT_SAME_DAY_DELIVERED, 
         URGENT_DIFF_DAY_DELIVERED, 
         UDA_URGENT_SAME_DAY_DELIVERED,
         UDA_URGENT_DIFF_DAY_DELIVERED,
         URGENT_SAME_DAY_DELIVERED_LATE,
         URGENT_DIFF_DAY_DELIVERED_LATE)%>%
  full_join(subset(u7_FDonly, select=c(YEAR_MONTH, ### <-- left_join to bring in FD only activity data 
                                       CONTRACT_NUMBER, 
                                       URGENT_SAME_DAY_DELIVERED, 
                                       URGENT_DIFF_DAY_DELIVERED, 
                                       UDA_URGENT_SAME_DAY_DELIVERED,
                                       UDA_URGENT_DIFF_DAY_DELIVERED,
                                       URGENT_SAME_DAY_DELIVERED_LATE,
                                       URGENT_DIFF_DAY_DELIVERED_LATE)), c("YEAR_MONTH","CONTRACT_NUMBER"))%>%
  left_join(ICB_region,by=c("COMMISSIONER_CODE"="STP_Code"))%>%  ###<--- bring in region names
  collect()

master[is.na(master)] = 0

master<-master%>%
  mutate(URGENT_SAME_DAY_DELIVERED= `URGENT_SAME_DAY_DELIVERED.x`+`URGENT_SAME_DAY_DELIVERED.y`,  ###<-- This step to add urgent care delivered by non-FD and FD together
         URGENT_DIFF_DAY_DELIVERED = `URGENT_DIFF_DAY_DELIVERED.x`+`URGENT_DIFF_DAY_DELIVERED.y`, 
         URGENT_DELIVERED = URGENT_SAME_DAY_DELIVERED + URGENT_DIFF_DAY_DELIVERED,
         UDA_URGENT_SAME_DAY_DELIVERED = `UDA_URGENT_SAME_DAY_DELIVERED.x`+`UDA_URGENT_SAME_DAY_DELIVERED.y`,
         UDA_URGENT_DIFF_DAY_DELIVERED = `UDA_URGENT_DIFF_DAY_DELIVERED.x`+ `UDA_URGENT_DIFF_DAY_DELIVERED.y`,
         UDA_URGENT_DELIVERED =  UDA_URGENT_SAME_DAY_DELIVERED + UDA_URGENT_DIFF_DAY_DELIVERED,
         URGENT_SAME_DAY_DELIVERED_LATE = `URGENT_SAME_DAY_DELIVERED_LATE.x`+`URGENT_SAME_DAY_DELIVERED_LATE.y`,
         URGENT_DIFF_DAY_DELIVERED_LATE = `URGENT_DIFF_DAY_DELIVERED_LATE.x`+`URGENT_DIFF_DAY_DELIVERED_LATE.y`,
         URGENT_DELIVERED_LATE = URGENT_SAME_DAY_DELIVERED_LATE + URGENT_DIFF_DAY_DELIVERED_LATE,
         Region_Name=ifelse(Region_Name==0, "Region Unknown", Region_Name))%>%
  select(YEAR_MONTH,      ###<--- this step is to keep needed fields only
         ICB_CODE = COMMISSIONER_CODE, 
         ICB_NAME = COMMISSIONER_NAME, 
         CONTRACT_NUMBER, 
         URGENT_SAME_DAY_DELIVERED, 
         URGENT_DIFF_DAY_DELIVERED, 
         URGENT_DELIVERED,
         UDA_URGENT_SAME_DAY_DELIVERED,
         UDA_URGENT_DIFF_DAY_DELIVERED,
         UDA_URGENT_DELIVERED,
         URGENT_SAME_DAY_DELIVERED_LATE,
         URGENT_DIFF_DAY_DELIVERED_LATE,
         URGENT_DELIVERED_LATE, 
         Region_Name)%>%
  #filter(!YEAR_MONTH %in% c(max(YEAR_MONTH)%m-% months(1), max(YEAR_MONTH)))%>% no longer removing the last two provisional months
  collect()



#### get 42 ICBs (to be used in loop for automating 42 outputs later)
ICB_list<-distinct(subset(master, select = c( ICB_CODE))) 

#### get 7 regions (to be used in loop for automating 7 outputs later)
region_list<-distinct(subset(master, select = c( Region_Name))) 


### get month of update (this will be used as part of ICB output file name)

# need to change the file name as not helpful using system date. needs to be final month #todo
update<- format(Sys.Date(), '%b%y') 
update2<- max(master$YEAR_MONTH, '%b%y') #not showing as year_month




### prep a complete list of contracts available in all three data files during this time period
contract_a<-distinct(subset(u7_contract, select = c(COMMISSIONER_CODE, 
                                                    COMMISSIONER_NAME, 
                                                    CONTRACT_NUMBER, 
                                                    PROVIDER_ID, 
                                                    PROVIDER_NAME)))

contract_b<- distinct(subset(u7_UDA, select = c(COMMISSIONER_CODE, 
                                                COMMISSIONER_NAME, 
                                                CONTRACT_NUMBER, 
                                                PROVIDER_ID, 
                                                PROVIDER_NAME)))
contract_c<- distinct(subset(u7_FDonly, select = c(COMMISSIONER_CODE, 
                                                   COMMISSIONER_NAME, 
                                                   CONTRACT_NUMBER, 
                                                   PROVIDER_ID, 
                                                   PROVIDER_NAME)))

contract<- distinct(rbind(contract_a, contract_b, contract_c))%>%
  rename(ICB_CODE = COMMISSIONER_CODE, ICB_NAME = COMMISSIONER_NAME)%>%
  left_join(ICB_region,by=c("ICB_CODE"="STP_Code"))%>%
  collect()

### Wide tables for ICB ----
extract_icb_data<-function(ICB="QRV"){
  
  contract<-contract%>%
    filter( ICB_CODE == ICB )%>%
    collect()
  
  
  # filter master activity table to include deliveries by contracts within selected ICB only
  data1<-filter(master, ICB_CODE == ICB )
  
  # filter out rows with no urgent care delivery and only keep columns for urgent care delivery
  data2<- subset(data1, select = c(YEAR_MONTH, CONTRACT_NUMBER, URGENT_DELIVERED))%>%
    filter(URGENT_DELIVERED>0)%>%
    collect()
  
  # convert long table to wide (using months as columns)
  urgent_delivered<- spread(data2, key = "YEAR_MONTH",value = "URGENT_DELIVERED")
  
  # add contracts details into wide table
  urgent_delivered<-contract%>%
    right_join(urgent_delivered,  "CONTRACT_NUMBER" )
  
  # assign tables into right tabs for excel output
  dataset_names <- list(#'Contracts'= contract,    ### decided to hide this tab for now
    'Urgent Delivered' = urgent_delivered)
  
  
  
  # we will first create a folder to save our output
  # Print the current working directory
  current_wd <- getwd()
  #print(paste("Current working directory:", current_wd))
  
  # Check if the reports directory exists
  reports_dir <- file.path(current_wd, "ICB_outputs")
  if (!dir.exists(reports_dir)) {
    # Try to create the directory
    dir.create(reports_dir)
    # Verify if the directory was successfully created
    if (!dir.exists(reports_dir)) {
      stop("Failed to create 'reports' directory")
    }
  }
  
  openxlsx::write.xlsx(dataset_names, file = paste0(reports_dir,'\\Urgent700k_',ICB,'_',update, '_output.xlsx')) 
  
}

### Write all 42 files ----

for (i in (1: nrow(ICB_list))) {
  extract_icb_data(ICB_list$ICB_CODE[i])
}

### Wide tables for Region ----
extract_region_data<-function(reg="London"){
  
  contract<-contract%>%
    filter( Region_Name==reg)%>%
    collect()
  
  
  # filter master activity table to include deliveries by contracts within selected ICB only
  data1<-filter(master, Region_Name==reg )
  
  # filter out rows with no urgent care delivery and only keep columns for urgent care delivery
  data2<- subset(data1, select = c(YEAR_MONTH, CONTRACT_NUMBER, URGENT_DELIVERED))%>%
    filter(URGENT_DELIVERED>0)%>%
    collect()
  
  # convert long table to wide (using months as columns)
  urgent_delivered<- spread(data2, key = "YEAR_MONTH",value = "URGENT_DELIVERED")
  
  # add contracts details into wide table
  urgent_delivered<-contract%>%
    right_join(urgent_delivered,  "CONTRACT_NUMBER" )
  
  # assign tables into right tabs for excel output
  dataset_names <- list(#'Contracts'= contract,    ### decided to hide this tab for now
    'Urgent Delivered' = urgent_delivered)
  
  # we will first create a folder to save our output
  # Print the current working directory
  current_wd <- getwd()
  #print(paste("Current working directory:", current_wd))
  
  # Check if the reports directory exists
  reports_dir <- file.path(current_wd, "ICB_outputs")
  if (!dir.exists(reports_dir)) {
    # Try to create the directory
    dir.create(reports_dir)
    # Verify if the directory was successfully created
    if (!dir.exists(reports_dir)) {
      stop("Failed to create 'reports' directory")
    }
  }
  openxlsx::write.xlsx(dataset_names, file = paste0(reports_dir,'\\Urgent700k_',reg,'_',update, '_output.xlsx')) 
}

### Write all 7 regional files ----

for (i in (1: nrow(region_list))) {
  extract_region_data(region_list$Region_Name[i])
}


