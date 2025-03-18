library(openxlsx)
library(DBI)
library(magrittr) 
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(tidyr)

con <- dbConnect(odbc::odbc(), "NCDR")

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

############################################################################
#### UDA urgent care Analysis ----
## Add monthly UDA target in by adjusting annual contracted UDA with number of workdays ##
working_days <- read_excel("workdays.xlsx",sheet = "workdays")
working_days<-working_days%>%mutate(`YEAR_MONTH`=as.Date(`Month`))

master<-u7_UDA%>%
  select(YEAR_MONTH, 
         COMMISSIONER_CODE, 
         COMMISSIONER_NAME, 
         CONTRACT_NUMBER, 
         UDA_PERF_TARGET,
         UDA_DELIVERED, 
         UDA_DELIVERED_FD,
         URGENT_SAME_DAY_DELIVERED, 
         URGENT_DIFF_DAY_DELIVERED, 
         UDA_URGENT_SAME_DAY_DELIVERED,
         UDA_URGENT_DIFF_DAY_DELIVERED,
         URGENT_SAME_DAY_DELIVERED_LATE,
         URGENT_DIFF_DAY_DELIVERED_LATE)%>%
  full_join(subset(u7_FDonly, select=c(YEAR_MONTH, ### <-- left_join to bring in FD only activity data 
                                       CONTRACT_NUMBER, 
                                       UDA_PERF_TARGET,
                                       UDA_DELIVERED, 
                                       URGENT_SAME_DAY_DELIVERED, 
                                       URGENT_DIFF_DAY_DELIVERED, 
                                       UDA_URGENT_SAME_DAY_DELIVERED,
                                       UDA_URGENT_DIFF_DAY_DELIVERED,
                                       URGENT_SAME_DAY_DELIVERED_LATE,
                                       URGENT_DIFF_DAY_DELIVERED_LATE)), c("YEAR_MONTH","CONTRACT_NUMBER"))%>%
  collect()

master[is.na(master)] = 0

master<-master%>%
  mutate(UDA_PERF_TARGET = ifelse(`UDA_PERF_TARGET.x`==0, `UDA_PERF_TARGET.y`, `UDA_PERF_TARGET.x`),
         UDA_DELIVERED = `UDA_DELIVERED.x`+ `UDA_DELIVERED.y`,
         URGENT_SAME_DAY_DELIVERED= `URGENT_SAME_DAY_DELIVERED.x`+`URGENT_SAME_DAY_DELIVERED.y`,  ###<-- This step to add urgent care delivered by non-FD and FD together
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
         UDA_PERF_TARGET,
         UDA_DELIVERED,
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


df <- u7_contract%>%
  select(`YEAR_MONTH`,`CONTRACT_NUMBER`
         #,`CONTRACTED_UDA`, `CONTRACTED_UOA`,`CONTRACTED_COT`
  )%>%
  right_join(master, c("YEAR_MONTH", "CONTRACT_NUMBER")) %>% 
  left_join(working_days, "YEAR_MONTH") %>% 
  mutate(mon_UDA_target = round(UDA_PERF_TARGET/`total workdays`*`no workdays`, 1),
         perc_UDA_delivered=ifelse(UDA_PERF_TARGET==0, NA, round(UDA_DELIVERED/mon_UDA_target, 3)), 
         perc_urgent_UDA = ifelse(UDA_DELIVERED==0, NA, round((UDA_URGENT_SAME_DAY_DELIVERED + UDA_URGENT_DIFF_DAY_DELIVERED)/UDA_DELIVERED,3)),
         uda_urgent= UDA_URGENT_SAME_DAY_DELIVERED + UDA_URGENT_DIFF_DAY_DELIVERED)

df<-distinct(df)

names(df) <- base::tolower(names(df))

df_12mon_avg<-df%>%
  filter(year_month>="2024-12-01", year_month<="2024-11-01")%>%
  group_by(contract_number, icb_code, icb_name, uda_perf_target)%>%
  summarise(no_uda_month =n(),
            uda_delivered_12mon_avg = mean(uda_delivered, na.rm = T),
            uda_urgent_same_day_delivered_12mon_avg  = mean(uda_urgent_same_day_delivered, na.rm = T),
            uda_urgent_diff_day_delivered_12mon_avg  = mean(uda_urgent_diff_day_delivered, na.rm = T),
            contracted_uda_12mon_avg = mean(mon_uda_target, na.rm=T),
            perc_udal_delivered_12mon_avg =mean(perc_uda_delivered, na.rm=T),
            perc_urgent_uda_avg =mean(perc_urgent_uda, na.rm=T))%>%
  collect()

df_12mon_sum<-df%>%
  filter(year_month>="2024-12-01", year_month<="2024-11-01")%>%
  group_by(contract_number, icb_code, icb_name, uda_perf_target)%>%
  summarise(no_uda_month =n(),
            uda_delivered_12mon = sum(uda_delivered, na.rm = T),
            uda_urgent_same_day_delivered_12mon = sum(uda_urgent_same_day_delivered, na.rm = T),
            uda_urgent_diff_day_delivered_12mon = sum(uda_urgent_diff_day_delivered, na.rm = T),
            contracted_uda_12mon= sum(mon_uda_target, na.rm=T))%>%
  mutate(perc_udal_delivered_12mon=ifelse(contracted_uda_12mon==0, NA, round(uda_delivered_12mon/contracted_uda_12mon, 3)),
         perc_urgent_uda_12mon=ifelse(uda_delivered_12mon==0, NA, round((uda_urgent_same_day_delivered_12mon+uda_urgent_diff_day_delivered_12mon)/uda_delivered_12mon, 3)))%>%
  collect()

dataset_names1 <- list('Monthly_Jul23toNov24' = subset(df, select = -c(`month`)), 'Last 12months_average' = df_12mon_avg, 'Last 12months_sum' = df_12mon_sum)

openxlsx::write.xlsx(dataset_names1, file = paste0('~/Rprojects/SMT-Dental-Pack-PhODS/\\Urgent700k_UDAdelivery.xlsx')) 