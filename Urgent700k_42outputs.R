library(openxlsx)
library(DBI)
library(magrittr) 
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(tidyr)

con <- dbConnect(odbc::odbc(), "NCDR")

#### Produce 42 outputs ----

############ Extract data from NCDR

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

############ Process data and prep master data as ready to be filtered by ICB

update<- format(Sys.Date(), '%b%y') ###<- Month of update (this will be used as part the ICB output file names)


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
  left_join(subset(u7_FDonly, select=c(YEAR_MONTH, ### <-- left_join to bring in FD only activity data 
                                     CONTRACT_NUMBER, 
                                     URGENT_SAME_DAY_DELIVERED, 
                                     URGENT_DIFF_DAY_DELIVERED, 
                                     UDA_URGENT_SAME_DAY_DELIVERED,
                                     UDA_URGENT_DIFF_DAY_DELIVERED,
                                     URGENT_SAME_DAY_DELIVERED_LATE,
                                     URGENT_DIFF_DAY_DELIVERED_LATE)), c("YEAR_MONTH","CONTRACT_NUMBER"))%>%
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



#### get 42 ICBs
ICB_list<-distinct(subset(master, select = c( ICB_CODE))) 


### function to prep wide tables for each ICB
extract_icb_data<-function(ICB="QRV"){
  
master<-filter(master, ICB_CODE == ICB )

contract<- distinct(subset(master, select = c( ICB_CODE, ICB_NAME, CONTRACT_NUMBER)))%>%
  filter( ICB_CODE == ICB )%>% 
  left_join(distinct(subset(u7_contract,
                   select = c(CONTRACT_NUMBER,
                              PROVIDER_ID, PROVIDER_NAME, LATEST_PPC_ADDRESS_POSTCODE, 
                              LSOA11_CODE, WARD_CODE, WARD_NAME,LOCAL_AUTHORITY_CODE, 
                              LOCAL_AUTHORITY_NAME))), "CONTRACT_NUMBER" )
  
  
urgent_delivered<- spread(data=subset(master, select = c(YEAR_MONTH, CONTRACT_NUMBER, URGENT_DELIVERED)),              
                                      key = "YEAR_MONTH",
                                      value = "URGENT_DELIVERED")

uda_urgent_delivered<- spread(data=subset(master, select = c(YEAR_MONTH, CONTRACT_NUMBER, UDA_URGENT_DELIVERED)),       
                          key = "YEAR_MONTH",
                          value = "UDA_URGENT_DELIVERED")

urgent_delivered_late<- spread(data=subset(master, select = c(YEAR_MONTH, CONTRACT_NUMBER, URGENT_DELIVERED_LATE)),                       
                               key = "YEAR_MONTH",
                               value = "URGENT_DELIVERED_LATE")

dataset_names <- list('Contracts'= contract,
                      'Urgent Delivered' = urgent_delivered, 
                      'UDA Urgent Delivered' = uda_urgent_delivered, 
                      'Urgent Delivered Late' = urgent_delivered_late)

openxlsx::write.xlsx(dataset_names, file = paste0('~/Rprojects/SMT-Dental-Pack-PhODS/ICB_outputs\\Urgent700k_',ICB,'_',update, '_update.xlsx')) 

}

##### Write 42 files


for (i in (1: nrow(ICB_list))) {
  extract_icb_data(ICB_list$ICB_CODE[i])
}


############################################################################
#### UDA urgent care Analysis ----
## Add monthly UDA target in by adjusting annual contracted UDA with number of workdays ##
working_days <- read_excel("workdays.xlsx",sheet = "workdays")
working_days<-working_days%>%mutate(`YEAR_MONTH`=as.Date(`Month`))

df <- u7_contract%>%
  select(`YEAR_MONTH`,`CONTRACT_NUMBER`
         #,`CONTRACTED_UDA`, `CONTRACTED_UOA`,`CONTRACTED_COT`
  )%>%
  inner_join(u7_UDA, c("YEAR_MONTH", "CONTRACT_NUMBER")) %>% 
  left_join(working_days, "YEAR_MONTH") %>% 
  mutate(mon_UDA_target = round(UDA_PERF_TARGET/`total workdays`*`no workdays`, 1),
         perc_UDA_delivered=ifelse(UDA_PERF_TARGET==0, NA, round(UDA_DELIVERED/mon_UDA_target, 3)), 
         perc_urgent_UDA = ifelse(UDA_DELIVERED==0, NA, round((UDA_URGENT_SAME_DAY_DELIVERED + UDA_URGENT_DIFF_DAY_DELIVERED)/UDA_DELIVERED,3)),
         uda_urgent= UDA_URGENT_SAME_DAY_DELIVERED + UDA_URGENT_DIFF_DAY_DELIVERED)

df<-distinct(df)

names(df) <- base::tolower(names(df))

df_12mon_avg<-df%>%
  filter(year_month>="2023-07-01", year_month<="2024-06-01")%>%
  group_by(contract_number, commissioner_code, commissioner_name, provider_id, provider_name, uda_perf_target)%>%
  summarise(no_uda_month =n(),
            uda_delivered_12mon_avg = sum(uda_delivered, na.rm = T),
            uda_delivered_fd_12mon_avg  = mean(uda_delivered_fd, na.rm = T),
            uda_band_1_delivered_12mon_avg  = mean(uda_band_1_delivered, na.rm = T),
            uda_band_2_delivered_12mon_avg  = mean(uda_band_2_delivered, na.rm = T),
            uda_band_2a_delivered_12mon_avg  = mean(uda_band_2a_delivered, na.rm = T),
            uda_band_2b_delivered_12mon_avg  = mean(uda_band_2b_delivered, na.rm = T),
            uda_band_2c_delivered_12mon_avg  = mean(uda_band_2c_delivered, na.rm = T),
            uda_band_3_delivered_12mon_avg  = mean(uda_band_3_delivered, na.rm = T),
            uda_urgent_same_day_delivered_12mon_avg  = mean(uda_urgent_same_day_delivered, na.rm = T),
            uda_urgent_diff_day_delivered_12mon_avg  = mean(uda_urgent_diff_day_delivered, na.rm = T),
            uda_band_other_12mon_avg  = mean(uda_band_other_delivered, na.rm = T),
            contracted_uda_12mon_avg = mean(mon_uda_target, na.rm=T),
            perc_udal_delivered_12mon_avg =mean(perc_uda_delivered, na.rm=T),
            perc_urgent_uda_avg =mean(perc_urgent_uda, na.rm=T))%>%
  collect()

df_12mon_sum<-df%>%
  filter(year_month>="2023-07-01", year_month<="2024-06-01")%>%
  group_by(contract_number, commissioner_code, commissioner_name, provider_id, provider_name, uda_perf_target)%>%
  summarise(uda_delivered_12mon = sum(uda_delivered, na.rm = T),
            uda_delivered_fd_12mon = sum(uda_delivered_fd, na.rm = T),
            uda_band_1_delivered_12mon = sum(uda_band_1_delivered, na.rm = T),
            uda_band_2_delivered_12mon = sum(uda_band_2_delivered, na.rm = T),
            uda_band_2a_delivered_12mon = sum(uda_band_2a_delivered, na.rm = T),
            uda_band_2b_delivered_12mon = sum(uda_band_2b_delivered, na.rm = T),
            uda_band_2c_delivered_12mon = sum(uda_band_2c_delivered, na.rm = T),
            uda_band_3_delivered_12mon = sum(uda_band_3_delivered, na.rm = T),
            uda_urgent_same_day_delivered_12mon = sum(uda_urgent_same_day_delivered, na.rm = T),
            uda_urgent_diff_day_delivered_12mon = sum(uda_urgent_diff_day_delivered, na.rm = T),
            uda_band_other_12mon = sum(uda_band_other_delivered, na.rm = T),
            contracted_uda_12mon= sum(mon_uda_target, na.rm=T))%>%
  mutate(perc_udal_delivered_12mon=ifelse(contracted_uda_12mon==0, NA, round(uda_delivered_12mon/contracted_uda_12mon, 3)),
         perc_urgent_uda_12mon=ifelse(uda_delivered_12mon==0, NA, round((uda_urgent_same_day_delivered_12mon+uda_urgent_diff_day_delivered_12mon)/uda_delivered_12mon, 3)))%>%
  collect()

dataset_names1 <- list('Monthly_Jul23toNov24' = subset(df, select = -c(`month`)), '12months_average' = df_12mon_avg, '12months_sum' = df_12mon_sum)

openxlsx::write.xlsx(dataset_names1, file = paste0('~/Rprojects/SMT-Dental-Pack-PhODS/\\Urgent700k_UDAdelivery.xlsx')) 