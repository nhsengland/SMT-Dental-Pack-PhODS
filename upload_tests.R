library(tidyverse)
library(odbc)
library(DBI)
library(testthat)

# read in NCDR tables
con <- dbConnect(odbc::odbc(), "NCDR")

bpe <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_BPE]")
contract <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Contracts]")
npp <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_NPP_Eligible_Activity]")
uda <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity]")
dcp <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_DCP]")
uda_fd <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity_FD_only]")
unique <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Unique_rolling]")
uoa <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UOA_Activity]")

dbDisconnect(con)

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
            total_uoa_target = sum(CONTRACTED_UOA, na.rm = TRUE))

check_npp <- npp %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(n_contracts = n_distinct(CONTRACT_NUMBER),
            total_adult = sum(BAND1_ADULT_COUNT, BAND23_ADULT_COUNT, na.rm = TRUE), 
            total_child = sum(BAND1_CHILD_COUNT, BAND23_CHILD_COUNT, na.rm = TRUE))

check_uda <- uda %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(n_contracts = n_distinct(CONTRACT_NUMBER), 
            total_uda_delivered = sum(UDA_DELIVERED, na.rm = TRUE))

check_dcp <- dcp %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(n_contracts = n_distinct(CONTRACT_NUMBER), 
            total_uda_delivered = sum(UDA_DELIVERED, na.rm = TRUE))

check_uda_fd <- uda_fd %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(n_contracts = n_distinct(CONTRACT_NUMBER), 
            total_uda_delivered = sum(UDA_DELIVERED, na.rm = TRUE))

check_unique <- unique %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(total_all = sum(ALL_12M_COUNT, na.rm = TRUE))

check_uoa <- uoa %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(n_contracts = n_distinct(CONTRACT_NUMBER), 
            total_uoa_delivered = sum(UOA_DELIVERED, na.rm = TRUE))
