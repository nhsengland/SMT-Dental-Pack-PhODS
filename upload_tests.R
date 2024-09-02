library(tidyverse)
library(odbc)
library(DBI)
library(testthat)

con <- dbConnect(odbc::odbc(), "NCDR")

unique_rolling <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_Unique_rolling]")

uda <- dbGetQuery(con, "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[Calendar_UDA_Activity]")

dbDisconnect(con)

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
