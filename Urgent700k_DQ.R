########################### QA ################ ----
#spot check outputs against SQL queries (PLEASE CHANGE ICB CODE AND CONTRACT NUMBER BELOW TO CHECK)

sql = "select a.*, b.*
from
(SELECT   [YEAR_MONTH_FD]=[YEAR_MONTH] 
 ,isnull([URGENT_SAME_DAY_DELIVERED],0)+isnull([URGENT_DIFF_DAY_DELIVERED],0) AS FDONLY
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000]
  where [COMMISSIONER_CODE]='QE1' and [CONTRACT_NUMBER] = '1573680015') a
  full join 
 ( SELECT  [YEAR_MONTH]
,isnull([URGENT_SAME_DAY_DELIVERED],0)+isnull([URGENT_DIFF_DAY_DELIVERED],0) AS ACT
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000]
    where [COMMISSIONER_CODE]='QE1' and [CONTRACT_NUMBER] = '1573680015') b
	on a.[YEAR_MONTH_FD]= b.[YEAR_MONTH]
	order by a.[YEAR_MONTH_FD]"

result <- dbSendQuery(con, sql)
QA1 <- dbFetch(result)
dbClearResult(result)

QA1<-QA1%>%mutate(YEAR_MONTH=as.Date(ifelse(is.na(YEAR_MONTH), YEAR_MONTH_FD, YEAR_MONTH)), 
                  total=ifelse(is.na(FDONLY), 0, FDONLY)+ifelse(is.na(ACT),0, ACT))

## CHANGE ICB CODE in file name AND contract number below
QA2 <- read.xlsx(paste0('~/Rprojects/SMT-Dental-Pack-PhODS/ICB_outputs\\Urgent700k_QE1_',update, '_update.xlsx'), sheet = "Urgent Delivered", startRow = 1)
QA2a<-QA2%>%filter(`CONTRACT_NUMBER`== "1573680015")
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
                                           "2024-11-01",
                                           "2024-12-01",
                                           "2025-01-01",
                                           "2025-02-01"),
                          variable.name = "YEAR_MONTH",
                          value.name ="value")

check<-QA2_long%>%
  mutate(YEAR_MONTH=as.Date(YEAR_MONTH))%>%
  right_join(QA1, "YEAR_MONTH")%>%
  filter(total!=0)%>%
  mutate(check = ifelse(value==total, TRUE, FALSE))

# if check is all TRUE, then pass QA meaning urgent delivered activities match for a random contract
view(distinct(subset(check, select=c('check'))))

sql="
select distinct b.CONTRACT_NUMBER, a.nonFD, b.FD, (a.nonFD+b.FD) AS TOTAL
from
(SELECT    [CONTRACT_NUMBER], sum(isnull([URGENT_SAME_DAY_DELIVERED],0)+isnull([URGENT_DIFF_DAY_DELIVERED],0)) as nonFD
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_urgent_700000]
 group by [CONTRACT_NUMBER],[COMMISSIONER_CODE]
 HAVING [COMMISSIONER_CODE]='QE1'
 ) a
  full join 
 ( SELECT  [CONTRACT_NUMBER], sum(isnull([URGENT_SAME_DAY_DELIVERED],0)+isnull([URGENT_DIFF_DAY_DELIVERED],0)) as FD
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_urgent_700000]
 group by [CONTRACT_NUMBER],[COMMISSIONER_CODE]
 HAVING [COMMISSIONER_CODE]='QE1' 
 
 ) b
	on  a.CONTRACT_NUMBER=b.CONTRACT_NUMBER
	WHERE (a.nonFD+b.FD)>0"
result <- dbSendQuery(con, sql)
QA3 <- dbFetch(result)
dbClearResult(result)

QA2<-QA2%>%mutate(total=select(., c( "2023-07-01",
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
                                     "2024-11-01",
                                     "2024-12-01",
                                     "2025-01-01",
                                     "2025-02-01"))%>% 
                    rowSums(na.rm=T))

check2<-QA2%>%
  left_join(QA3, "CONTRACT_NUMBER")%>%
  mutate(check=ifelse(total==TOTAL, TRUE, FALSE))


# if check2 is all TRUE, then pass QA meaning number of contracts included in the ICB output is complete
view(distinct(subset(check2, select=c('check'))))


#### check if total activities match 



