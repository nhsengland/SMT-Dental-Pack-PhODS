library(tidyverse)
library(testthat)

## MANUAL INPUTS ##
# each month, add an extra month and remove the earliest month
latest_expected_months <- c("202405", "202406", "202407")
# for UOA, add a new month but do not remove the earliest month unless it is the start of a new financial year
uoa_expected_months <- c("202404", "202405", "202406", "202407")

# find filepath of latest file and read in
read_latest_file <- function(folder_name){
  filepath <- list.files(paste("N:/_Everyone/Primary Care Group/SMT_Dental Calendar data format/BSA Calendar data/", folder_name, sep = ""),
                         pattern = "csv$", 
                         full.names = TRUE) %>%
    magrittr::extract(which.max(file.mtime(.)))
  
  read.csv(filepath)
}

bpe <- read_latest_file(folder_name = "BPE")
contract <- read_latest_file(folder_name = "Contract")
npp <- read_latest_file(folder_name = "NPP")
uda <- read_latest_file(folder_name = "UDA Activity")
dcp <- read_latest_file(folder_name = "UDA Activity DCP")
uda_fd <- read_latest_file(folder_name = "UDA Activity FD only")
unique <- read_latest_file(folder_name = "Unique rolling")
uoa <- read_latest_file(folder_name = "UOA Activity")

# get unique months
bpe_months <- bpe %>% select(YEAR_MONTH) %>% distinct()
contract_months <- contract %>% select(YEAR_MONTH) %>% distinct()
npp_months <- npp %>% select(YEAR_MONTH) %>% distinct()
uda_months <- uda %>% select(YEAR_MONTH) %>% distinct()
dcp_months <- dcp %>% select(YEAR_MONTH) %>% distinct()
uda_fd_months <- uda_fd %>% select(YEAR_MONTH) %>% distinct()
unique_months <- unique %>% select(YEAR_MONTH) %>% distinct()
uoa_months <- uoa %>% select(YEAR_MONTH) %>% distinct()

test_that("Latest files contain three months of data",{
  expected <- 3
  
  actual <- rbind(bpe_months, contract_months, npp_months, uda_months, 
                  dcp_months, uda_fd_months, unique_months) %>% 
    n_distinct()
  
  print("Latest files contain three months of data")
  
  expect_equal(expected, actual)
})

test_that("Latest files contain latest three months of data",{
  expected <- latest_expected_months
  
  months <- rbind(bpe_months, contract_months, npp_months, uda_months, 
                  dcp_months, uda_fd_months, unique_months) %>% 
    distinct() %>% 
    arrange(YEAR_MONTH)
  
  actual <- as.character(months$YEAR_MONTH)
  
  print("Latest files contain expected months")
  
  expect_equal(expected, actual)
})

test_that("Latest UOA file contains data for current financial year",{
  expected <- uoa_expected_months
  
  months <- uoa_months %>% 
    distinct() %>% 
    arrange(YEAR_MONTH)
  
  actual <- as.character(months$YEAR_MONTH)
  
  print("Latest UOA file contains expected months")
  
  expect_equal(expected, actual)
})

test_that("Sum of ICB unique patients seen is not equal to national",{
  unique_patients <- unique %>% 
    group_by(YEAR_MONTH) %>% 
    summarise(unique_seen_all = sum(ALL_12M_COUNT, na.rm = TRUE),
              unique_seen_child = sum(CHILD_12M_COUNT, na.rm = TRUE),
              unique_seen_adult = sum(ADULT_24M_COUNT, na.rm = TRUE))
  
  national <- unique %>% 
    group_by(YEAR_MONTH) %>% 
    summarise(nat_all = mean(ALL_12M_COUNT_CTRY, na.rm = TRUE),
              nat_child = mean(CHILD_12M_COUNT_CTRY, na.rm = TRUE),
              nat_adult = mean(ADULT_24M_COUNT_CTRY, na.rm = TRUE))
  
  icb_all <- unique_patients$unique_seen_all
  icb_child <- unique_patients$unique_seen_child
  icb_adult <- unique_patients$unique_seen_adult
  
  nat_all <- national$nat_all
  nat_child <- national$nat_child
  nat_adult <- national$nat_adult
  
  print("Sum of ICB unique patients seen is not equal to national")
  
  expect_failure(expect_equal(icb_all, nat_all))
  expect_failure(expect_equal(icb_child, nat_child))
  expect_failure(expect_equal(icb_adult, nat_adult))
})

test_that("DCP file has data for all roles",{
  expected <- c("Hygienist", "Nurse", "Technician", "Therapist")
  
  dcp_roles <- dcp %>% 
    select(DCP_DESC) %>% 
    distinct() %>% 
    arrange(DCP_DESC)
  
  actual <- dcp_roles$DCP_DESC
  
  print("DCP roles are as expected")
  
  expect_equal(expected, actual)
})
