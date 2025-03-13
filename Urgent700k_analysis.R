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