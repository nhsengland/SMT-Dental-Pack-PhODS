library(openxlsx)
library(DBI)
library(magrittr) 
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)

con <- dbConnect(odbc::odbc(), "NCDR")

##################################################
#### Read source file ----
# Read all new data files and consolidate tables ##
folder <- "N:/_Everyone/Primary Care Group/SMT_Dental Calendar data format/BSA Calendar data/Urgent 700000"

contract1<- read.csv(paste0(folder,"/Production_dental_contract_urgent_202307_to_202406.csv"))
contract2<- read.csv(paste0(folder,"/Production_dental_contract_urgent_202407_to_202411.csv"))

UDA1<- read.csv(paste0(folder,"/Production_dental_UDA_activity_urgent_202307_to_202406.csv"))
UDA2<-read.csv(paste0(folder,"/Production_dental_UDA_activity_urgent_202407_to_202411.csv"))

FDonly1<-read.csv(paste0(folder,"/Production_dental_UDA_activity_FD_only_urgent_202307_to_202406.csv"))
FDonly2<- read.csv(paste0(folder,"/Production_dental_UDA_activity_FD_only_urgent_202407_to_202411.csv"))

u7_contract<-rbind(contract1, contract2)%>%mutate(`YEAR_MONTH`= as.Date(paste0(substr(`YEAR_MONTH`,1,4), "-", substr(`YEAR_MONTH`, 5,6), "-01"), "%Y-%m-%d"))
u7_UDA<-rbind(UDA1, UDA2)%>%mutate(`YEAR_MONTH`= as.Date(paste0(substr(`YEAR_MONTH`,1,4), "-", substr(`YEAR_MONTH`, 5,6), "-01"), "%Y-%m-%d"))
u7_FDonly<-rbind(FDonly1, FDonly2)%>%mutate(`YEAR_MONTH`= as.Date(paste0(substr(`YEAR_MONTH`,1,4), "-", substr(`YEAR_MONTH`, 5,6), "-01"), "%Y-%m-%d"))

#### NCDR upload ----
## create new tables in NCDR and upload data ##
dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="Calendar_Contracts_urgent_700000"),
             value = u7_contract, 
             row.names = FALSE, append = FALSE, overwrite = TRUE)

dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="Calendar_UDA_Activity_urgent_700000"),
             value = u7_UDA, 
             row.names = FALSE, append = FALSE, overwrite = TRUE)

dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="Calendar_UDA_Activity_FD_only_urgent_700000"),
             value = u7_FDonly, 
             row.names = FALSE, append = FALSE, overwrite = TRUE)

