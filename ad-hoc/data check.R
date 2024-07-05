pull_UDA_calendar_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity] a
  LEFT JOIN (SELECT DISTINCT
			[Region_Name]
			,[STP_Code]
			FROM [NHSE_Reference].[dbo].[tbl_Ref_ODS_Commissioner_Hierarchies]
			WHERE STP_Code NOT IN ('DUM001', 'DUM002', 'UNK','X24')) b
		ON a.[COMMISSIONER_CODE] = b.[STP_Code]"
  result <- dbSendQuery(con, sql)
  UDA_calendar_data <- dbFetch(result)
  dbClearResult(result)
  
  UDA_calendar_data
}

#only include contractors in contractual_dataset
UDA_calendar_data<-pull_UDA_calendar_data()%>%
  inner_join(contractual_dataset,by=c('YEAR_MONTH','CONTRACT_NUMBER'))

UDA_calendar_data <- UDA_calendar_data  %>%
  mutate(financial_year = case_when(YEAR_MONTH >= as.Date("2016-04-01") & YEAR_MONTH < as.Date("2017-04-01") ~ "2016/17",
                                    YEAR_MONTH >= as.Date("2017-04-01") & YEAR_MONTH < as.Date("2018-04-01") ~ "2017/18",
                                    YEAR_MONTH >= as.Date("2018-04-01") & YEAR_MONTH < as.Date("2019-04-01") ~ "2018/19",
                                    YEAR_MONTH >= as.Date("2019-04-01") & YEAR_MONTH < as.Date("2020-04-01") ~ "2019/20",
                                    YEAR_MONTH >= as.Date("2020-04-01") & YEAR_MONTH < as.Date("2021-04-01") ~ "2020/21",
                                    YEAR_MONTH >= as.Date("2021-04-01") & YEAR_MONTH < as.Date("2022-04-01") ~ "2021/22",
                                    YEAR_MONTH >= as.Date("2022-04-01") & YEAR_MONTH < as.Date("2023-04-01") ~ "2022/23",
                                    YEAR_MONTH >= as.Date("2023-04-01") & YEAR_MONTH < as.Date("2024-04-01") ~ "2023/24",
                                    YEAR_MONTH >= as.Date("2024-04-01") & YEAR_MONTH < as.Date("2025-04-01") ~ "2024/25")) %>%
  group_by(financial_year) %>%
  summarise(full_year_UDA_delivery = sum(UDA_DELIVERED, na.rm = TRUE),
            full_year_contracted_UDAs = sum(UDA_PERF_TARGET, na.rm = TRUE) / 12) %>%
  mutate(perc_UDA_deliverd = full_year_UDA_delivery * 100 / full_year_contracted_UDAs)



#############previous
################################################################################
pull_UDA_scheduled_historical_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled_historical]"
  result <- dbSendQuery(con, sql)
  UDA_scheduled_historical_data <- dbFetch(result)
  dbClearResult(result)
  
  UDA_scheduled_historical_data
}
UDA_calendar_his_data<-pull_UDA_scheduled_historical_data()
UDA_calendar_his_data <- UDA_calendar_his_data  %>%
  mutate(financial_year = case_when(data_month >= as.Date("2016-04-01") & data_month < as.Date("2017-04-01") ~ "2016/17",
                                    data_month >= as.Date("2017-04-01") & data_month < as.Date("2018-04-01") ~ "2017/18",
                                    data_month >= as.Date("2018-04-01") & data_month < as.Date("2019-04-01") ~ "2018/19",
                                    data_month >= as.Date("2019-04-01") & data_month < as.Date("2020-04-01") ~ "2019/20",
                                    data_month >= as.Date("2020-04-01") & data_month < as.Date("2021-04-01") ~ "2020/21",
                                    data_month >= as.Date("2021-04-01") & data_month < as.Date("2022-04-01") ~ "2021/22",
                                    data_month >= as.Date("2022-04-01") & data_month < as.Date("2023-04-01") ~ "2022/23",
                                    data_month >= as.Date("2023-04-01") & data_month < as.Date("2024-04-01") ~ "2023/24",
                                    data_month >= as.Date("2024-04-01") & data_month < as.Date("2025-04-01") ~ "2024/25")) %>%
  group_by(financial_year) %>%
  summarise(full_year_UDA_delivery = sum(UDA_delivered, na.rm = TRUE),
            full_year_contracted_UDAs = sum(annual_contracted_UDA, na.rm = TRUE) / 12) %>%
  mutate(perc_UDA_deliverd = full_year_UDA_delivery * 100 / full_year_contracted_UDAs)
