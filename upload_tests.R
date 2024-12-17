library(tidyverse)
library(odbc)
library(DBI)
library(testthat)

# read in NCDR tables......................
con <- dbConnect(odbc::odbc(), "NCDR")

bpe <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_BPE_backup]")
contract <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts_backup]")
npp <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_NPP_Eligible_Activity_backup]")
uda <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_backup]")
dcp <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_DCP_backup]")
uda_fd <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only_backup]")
unique <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Unique_rolling_backup]")
uoa <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UOA_Activity_backup]")


#### TESTS ####
test_that("No duplicated rows - NPP",{
  rows <- npp %>% 
    group_by(YEAR_MONTH) %>% 
    count()
  
  unique_contracts <- npp %>% 
    select(YEAR_MONTH, CONTRACT_NUMBER) %>% 
    distinct() %>% 
    group_by(YEAR_MONTH) %>% 
    count()
  
  print("Unique contract numbers match number of rows (no duplicates) - NPP")
  
  expect_equal(rows, unique_contracts)
})

test_that("No duplicated rows - UDA Activity",{
  rows <- uda %>% 
    group_by(YEAR_MONTH) %>% 
    count()
  
  unique_contracts <- uda %>% 
    select(YEAR_MONTH, CONTRACT_NUMBER) %>% 
    distinct() %>% 
    group_by(YEAR_MONTH) %>% 
    count()
  
  print("Unique contract numbers match number of rows (no duplicates) - UDA Activity")
  
  expect_equal(rows, unique_contracts)
})

test_that("No duplicated rows - UDA FD only",{
  rows <- uda_fd %>% 
    group_by(YEAR_MONTH) %>% 
    count()
  
  unique_contracts <- uda_fd %>% 
    select(YEAR_MONTH, CONTRACT_NUMBER) %>% 
    distinct() %>% 
    group_by(YEAR_MONTH) %>% 
    count()
  
  print("Unique contract numbers match number of rows (no duplicates) - UDA FD only")
  
  expect_equal(rows, unique_contracts)
})

test_that("No duplicated rows - UOA",{
  rows <- uoa %>% 
    group_by(YEAR_MONTH) %>% 
    count()
  
  unique_contracts <- uoa %>% 
    select(YEAR_MONTH, CONTRACT_NUMBER) %>% 
    distinct() %>% 
    group_by(YEAR_MONTH) %>% 
    count()
  
  print("Unique contract numbers match number of rows (no duplicates) - UOA")
  
  expect_equal(rows, unique_contracts)
})

#### DATA FRAME CHECKS ####
check_contract <- contract %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(n_contracts = n_distinct(CONTRACT_NUMBER),
            total_uda_target = sum(CONTRACTED_UDA, na.rm = TRUE), 
            total_uoa_target = sum(CONTRACTED_UOA, na.rm = TRUE)) %>% 
  arrange(YEAR_MONTH) %>% 
  mutate(monthly_contract_change = n_contracts - lag(n_contracts),
         monthly_uda_change = total_uda_target - lag(total_uda_target),
         monthly_uoa_change = total_uoa_target - lag(total_uoa_target))

check_npp <- npp %>% 
  filter(EXCLUDE_FROM_NPT == "N") %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(n_contracts = n_distinct(CONTRACT_NUMBER),
            total_adult = sum(BAND1_ADULT_COUNT, BAND23_ADULT_COUNT, na.rm = TRUE), 
            total_child = sum(BAND1_CHILD_COUNT, BAND23_CHILD_COUNT, na.rm = TRUE)) %>% 
  arrange(YEAR_MONTH) %>% 
  mutate(monthly_contract_change = n_contracts - lag(n_contracts),
         monthly_adult_change = total_adult - lag(total_adult),
         monthly_child_change = total_child - lag(total_child))

check_uda <- uda %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(n_contracts = n_distinct(CONTRACT_NUMBER), 
            total_uda_delivered = sum(UDA_DELIVERED, na.rm = TRUE)) %>% 
  arrange(YEAR_MONTH) %>% 
  mutate(monthly_contract_change = n_contracts - lag(n_contracts),
         monthly_uda_change = total_uda_delivered - lag(total_uda_delivered))

check_dcp <- dcp %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(n_contracts = n_distinct(CONTRACT_NUMBER), 
            total_uda_delivered = sum(UDA_DELIVERED, na.rm = TRUE)) %>% 
  arrange(YEAR_MONTH) %>% 
  mutate(monthly_contract_change = n_contracts - lag(n_contracts),
         monthly_uda_change = total_uda_delivered - lag(total_uda_delivered))

check_uda_fd <- uda_fd %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(n_contracts = n_distinct(CONTRACT_NUMBER), 
            total_uda_delivered = sum(UDA_DELIVERED, na.rm = TRUE)) %>% 
  arrange(YEAR_MONTH) %>% 
  mutate(monthly_contract_change = n_contracts - lag(n_contracts),
         monthly_uda_change = total_uda_delivered - lag(total_uda_delivered))

check_unique <- unique %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(total_all = sum(ALL_12M_COUNT, na.rm = TRUE)) %>% 
  arrange(YEAR_MONTH) %>% 
  mutate(monthly_change = total_all - lag(total_all))

check_uoa <- uoa %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(n_contracts = n_distinct(CONTRACT_NUMBER), 
            total_uoa_delivered = sum(UOA_DELIVERED, na.rm = TRUE)) %>% 
  arrange(YEAR_MONTH) %>% 
  mutate(monthly_contract_change = n_contracts - lag(n_contracts),
         monthly_uoa_change = total_uoa_delivered - lag(total_uoa_delivered))


check_uoa_2 <- uoa %>% 
  filter(COMMISSIONER_CODE=='QKK')%>% 
  group_by(YEAR_MONTH) %>% 
  summarise(n_contracts = n_distinct(CONTRACT_NUMBER), 
            total_uoa_delivered = sum(UOA_DELIVERED, na.rm = TRUE), 
            total_uoa_target = sum(UOA_PERF_TAR, na.rm = TRUE)) %>% 
  arrange(YEAR_MONTH) %>% 
  mutate(monthly_contract_change = n_contracts - lag(n_contracts),
         monthly_uoa_change = total_uoa_delivered - lag(total_uoa_delivered),
         monthly_uoa_target_change = total_uoa_target - lag(total_uoa_target))
