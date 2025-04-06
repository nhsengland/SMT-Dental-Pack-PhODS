library(openxlsx)
library(DBI)
library(magrittr) 
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)

con <- dbConnect(odbc::odbc(), "NCDR")

##################################################

##save a copy of current tables as backup (in case anything goes wrong, we can revert back to backup tables)
sql=" 
DROP TABLE IF EXISTS [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts_urgent_700000_bk]

DROP TABLE IF EXISTS [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000_bk]

DROP TABLE IF EXISTS [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000_bk]

SELECT * INTO  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts_urgent_700000_bk] FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts_urgent_700000]

SELECT * INTO  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000_bk] FROM  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000]

SELECT * INTO [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000_bk] FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000]

### Need to add lines below to remove provisional data and save them to different tables ###########################################

INSERT INTO  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts_urgent_700000_provisional_archived] 
SELECT *, [date_removed]= getdate()
FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts_urgent_700000] 
where [FINAL_YN]= 'N'

INSERT INTO  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000_provisional_archived] 
SELECT *, [date_removed]= getdate()
FROM  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000]
where [FINAL_YN]= 'N'


INSERT INTO [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000_provisional_archived] 
SELECT *, [date_removed]= getdate()
FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000]
where [FINAL_YN]= 'N'

DELETE FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts_urgent_700000] 
where [FINAL_YN]= 'N'

DELETE FROM  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000]
where [FINAL_YN]= 'N'

DELETE FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000]
where [FINAL_YN]= 'N'

"
result<-dbSendQuery(con,sql)
dbClearResult(result)

#### Read source files ----
## Access all files available within the same folder
path="N:/_Everyone/Primary Care Group/SMT_Dental Calendar data format/BSA Calendar data/Urgent 700000"
filenames <- as_tibble(list.files(path))

filenames<-filenames%>%
  mutate(lastMon=as.Date(paste0(str_sub(value,-10,-7), "-",str_sub(value,-6,-5), "-01"),"%Y-%m-%d" ))%>%  ### check last available month for each file in the folder
  filter(lastMon==max(lastMon))%>%  ### only keep the latest/newest data files
  collect()
### check duplicated uploads ----
### Check the last month available in NCDR to see if latest data files have already been uploaded
sql=" 
SELECT max([YEAR_MONTH]) as last, name='Contract' FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts_urgent_700000]
union
SELECT max([YEAR_MONTH]) as last , name='UDA' FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000]
union
SELECT max([YEAR_MONTH]) as last , name='FDonly' FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000]
"
result<-dbSendQuery(con,sql)
lastMon<-dbFetch(result)
dbClearResult(result)

# if latest contracts data has not been uploaded, then upload otherwise only print out a message
if(max(filenames$lastMon)==subset(lastMon, name == "Contract")$last){
  print("Data have already been uploaded to [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts_urgent_700000].")
  }else{  

contract_file <- subset(filenames, select=("value"))%>% filter(grepl("contract_urgent", value, fixed = TRUE))
contract<- read.csv(paste0(path, "/",contract_file))

u7_contract<-contract%>%mutate(`YEAR_MONTH`= as.Date(paste0(substr(`YEAR_MONTH`,1,4), "-", substr(`YEAR_MONTH`, 5,6), "-01"), "%Y-%m-%d"))
  }

# if latest UDA activity data has not been uploaded, then upload otherwise only print out a message
if(max(filenames$lastMon)==subset(lastMon, name == "UDA")$last){
   print("Data have already been uploaded to [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000].") 
   }else{  
   UDA_file <- subset(filenames, select=("value"))%>% filter(grepl("UDA_activity_urgent", value, fixed = TRUE))
   UDA<- read.csv(paste0(path, "/", UDA_file))
 
   u7_UDA<-UDA%>%mutate(`YEAR_MONTH`= as.Date(paste0(substr(`YEAR_MONTH`,1,4), "-", substr(`YEAR_MONTH`, 5,6), "-01"), "%Y-%m-%d"))
   }

# if latest UDA FD only activity data has not been uploaded, then upload otherwise only print out a message
if(max(filenames$lastMon)==subset(lastMon, name == "FDonly")$last){
   print("Data have already been uploaded to [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000].")
   }else{  
   
   FDonly_file <- subset(filenames, select=("value"))%>% filter(grepl("UDA_activity_FD_only_urgent", value, fixed = TRUE))
   FDonly<-read.csv(paste0(path, "/", FDonly_file))
   
   u7_FDonly<-FDonly%>%mutate(`YEAR_MONTH`= as.Date(paste0(substr(`YEAR_MONTH`,1,4), "-", substr(`YEAR_MONTH`, 5,6), "-01"), "%Y-%m-%d"))
 }

#### NCDR upload ----
## create new temp tables in NCDR and upload data ##
dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="Calendar_Contracts_urgent_700000_temp"),
             value = u7_contract, 
             row.names = FALSE, append = FALSE, overwrite = TRUE)

dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="Calendar_UDA_Activity_urgent_700000_temp"),
             value = u7_UDA, 
             row.names = FALSE, append = FALSE, overwrite = TRUE)

dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="Calendar_UDA_Activity_FD_only_urgent_700000_temp"),
             value = u7_FDonly, 
             row.names = FALSE, append = FALSE, overwrite = TRUE)

## Append new temp tables into existing tables
sql=" 
INSERT INTO
  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts_urgent_700000]
SELECT * FROM  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts_urgent_700000_temp]

INSERT INTO
  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000]
SELECT * FROM  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000_temp]

INSERT INTO
  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000]
SELECT * FROM  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000_temp]


"
result<-dbSendQuery(con,sql)
dbClearResult(result)


########################################################
## delete temp tables after QA -----
sql=" 
Drop table [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts_urgent_700000_temp]

Drop table [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000_temp]

Drop table [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000_temp]

Drop table [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts_urgent_700000_bk]

Drop table [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000_bk]

Drop table [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000_bk]
"
result<-dbSendQuery(con,sql)
dbClearResult(result)