library(openxlsx)
library(DBI)
library(magrittr) 
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(tidyr)

con <- dbConnect(odbc::odbc(), "NCDR")

############ Extract data from NCDR ----

sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts_urgent_700000]"
result <- dbSendQuery(con, sql)
u7_contract <- dbFetch(result)
dbClearResult(result)

sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000]"
result <- dbSendQuery(con, sql)
u7_UDA <- dbFetch(result)
dbClearResult(result)

sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000]"
result <- dbSendQuery(con, sql)
u7_FDonly <- dbFetch(result)
dbClearResult(result)

############ Process data ----
#prep master data as ready to be filtered by ICB

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
         URGENT_DELIVERED_LATE = URGENT_SAME_DAY_DELIVERED_LATE + URGENT_DIFF_DAY_DELIVERED_LATE)%>%
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
         URGENT_DELIVERED_LATE)%>%
  collect()



#### get 42 ICBs (to be used in loop for automating 42 outputs later)
ICB_list<-distinct(subset(master, select = c( ICB_CODE))) 

### get month of update (this will be used as part of ICB output file name)
update<- format(Sys.Date(), '%b%y') 


### Prep wide tables for each ICB ----
extract_icb_data<-function(ICB="QRV"){

# prep a reference table for all contracts within selected ICB during this time period
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

openxlsx::write.xlsx(dataset_names, file = paste0('~/Rprojects/SMT-Dental-Pack-PhODS/ICB_outputs\\Urgent700k_',ICB,'_',update, '_update.xlsx')) 

}

### Write all 42 files ----


for (i in (1: nrow(ICB_list))) {
  extract_icb_data(ICB_list$ICB_CODE[i])
}

#########################################################################################
### QA ----
#spot check outputs against SQL queries (PLEASE CHANGE ICB CODE AND CONTRACT NUMBER BELOW TO CHECK)

sql = "select a.*, b.[ACT], (a.FDONLY+b.ACT) as total
from
(SELECT  [YEAR_MONTH]
 ,([URGENT_SAME_DAY_DELIVERED]+[URGENT_DIFF_DAY_DELIVERED]) AS FDONLY
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000]
  where [COMMISSIONER_CODE]='QE1' and [CONTRACT_NUMBER] = '6359440001') a
  full join 
 ( SELECT  [YEAR_MONTH]
,([URGENT_SAME_DAY_DELIVERED]+[URGENT_DIFF_DAY_DELIVERED]) AS ACT
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000]
    where [COMMISSIONER_CODE]='QE1' and [CONTRACT_NUMBER] = '6359440001') b
	on a.[YEAR_MONTH]= b.[YEAR_MONTH]
	order by [YEAR_MONTH]"

result <- dbSendQuery(con, sql)
QA1 <- dbFetch(result)
dbClearResult(result)

## CHANGE ICB CODE in file name AND contract number below
QA2 <- read.xlsx(paste0('~/Rprojects/SMT-Dental-Pack-PhODS/ICB_outputs\\Urgent700k_QE1_',update, '_update.xlsx'), sheet = "Urgent Delivered", startRow = 1)
QA2a<-QA2%>%filter(`CONTRACT_NUMBER`== "6359440001")
QA2_long <-reshape2::melt(subset(QA2a, select = -c(ICB_CODE, ICB_NAME,  PROVIDER_ID, PROVIDER_NAME)),
                                 id.vars= c("CONTRACT_NUMBER"),
                                 measure.vars= c( "2023-07-01",
                                                  "2023-08-01",
                                                  "2023-09-01",
                                                  "2023-10-01",
                                                  "2023-11-01",
                                                  "2023-12-01",
                                                  "2024-01-01",
                                                  "2024-02-01",
                                                  "2024-03-01",
                                                  "2024-04-01",
                                                  "2024-05-01",
                                                  "2024-06-01",
                                                  "2024-07-01",
                                                  "2024-08-01",
                                                  "2024-09-01",
                                                  "2024-10-01",
                                                  "2024-11-01"),
                                 variable.name = "YEAR_MONTH",
                                 value.name ="value")

check<-QA2_long%>%
  mutate(YEAR_MONTH=as.Date(YEAR_MONTH))%>%
  right_join(QA1, "YEAR_MONTH")%>%
  mutate(check = ifelse(value==total, TRUE, FALSE))

# if check is all TRUE, then pass QA meaning urgent delivered activities match
view(check)

sql="/****** Script for SelectTopNRows command from SSMS  ******/
select distinct b.CONTRACT_NUMBER, a.nonFD, b.FD, (a.nonFD+b.FD) AS TOTAL
from
(SELECT    [CONTRACT_NUMBER], sum([URGENT_SAME_DAY_DELIVERED]+[URGENT_DIFF_DAY_DELIVERED]) as nonFD
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000]
 group by [CONTRACT_NUMBER],[COMMISSIONER_CODE]
 HAVING [COMMISSIONER_CODE]='QE1'
 ) a
  full join 
 ( SELECT  [CONTRACT_NUMBER], sum([URGENT_SAME_DAY_DELIVERED]+[URGENT_DIFF_DAY_DELIVERED]) as FD
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000]
 group by [CONTRACT_NUMBER],[COMMISSIONER_CODE]
 HAVING [COMMISSIONER_CODE]='QE1' 
 
 ) b
	on  a.CONTRACT_NUMBER=b.CONTRACT_NUMBER
	WHERE (a.nonFD+b.FD)>0"
result <- dbSendQuery(con, sql)
QA3 <- dbFetch(result)
dbClearResult(result)

check2=ifelse(n_row(QA3$CONTRACT_NUMBER)==n_distinct(QA2$CONTRACT_NUMBER), TRUE, FALSE)

# if check2 is TRUE, then pass QA meaning number of contracts included in the ICB output is complete
view(check2)


#### check if total activities match 



