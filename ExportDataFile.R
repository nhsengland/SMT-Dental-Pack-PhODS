library(tidyverse)
library(ggplot2)
library(ggpubr)
library(stringr)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(scales)
library(readxl)
library(textclean)
library(lubridate)

#### get raw data
source(knitr::purl("SQLpulls.Rmd", output = tempfile()), local = TRUE)
source(knitr::purl("Data_Processing.Rmd", output = tempfile()))
source(knitr::purl("plotting.Rmd", output = tempfile()))

#### No UDA delivered, contracted & percentage
###UDA
data_Nat_UDA=plot_UDA_UOA_delivery_wd(data = UDA_calendar_data, 
                         UDAorUOA = "UDA",
                         plotChart = FALSE, 
                         all_regions_and_STPs = FALSE,
                         level = "National",
                         region_STP_name = NULL)

data_Nat_UDA<- data_Nat_UDA %>% 
  select(`Calendar month`,`Monthly UDAs delivered`,`Annual contracted UDAs`,`no workdays`,`Standardised monthly percentage of contracted UDAs delivered`)%>%
  mutate(Geograph='England',`Geograph Name`='England')

data_reg_UDA=plot_UDA_UOA_delivery_all_regions(data = UDA_calendar_data, 
                                  UDAorUOA = "UDA",
                                  level = "National",
                                  region_STP_name = NULL,
                                  plotChart = FALSE, 
                                  all_regions_and_STPs = TRUE)
data_reg_UDA <- data_reg_UDA %>%
  select(`Calendar month`,`Geograph Name`=`Region Name`,`Monthly UDAs delivered`,
         `Annual contracted UDAs`,`no workdays`,`Standardised monthly percentage of contracted UDAs delivered`)%>%
  mutate(Geograph='Region')

data_ICB_UDA <- Table_UDA_UOA_delivery_all_ICBs(data = UDA_calendar_data, 
                                UDAorUOA = "UDA")

data_ICB_UDA <- data_ICB_UDA %>%
  select(`Calendar month`,`Geograph Name`=`Commissioner Name`,`Monthly UDAs delivered`,
         `Annual contracted UDAs`,`no workdays`,`Standardised monthly percentage of contracted UDAs delivered`)%>%
  mutate(Geograph='ICB')

data_UDA<- rbind(data_Nat_UDA, data_reg_UDA, data_ICB_UDA)

###UOA
data_Nat_UOA=plot_UDA_UOA_delivery_wd(data = UOA_calendar_data, 
                                      UDAorUOA = "UOA",
                                      plotChart = FALSE, 
                                      all_regions_and_STPs = FALSE,
                                      level = "National",
                                      region_STP_name = NULL)

data_Nat_UOA<- data_Nat_UOA %>% 
  select(`Calendar month`,`Monthly UOAs delivered`,`Annual contracted UOAs`,`no workdays`,`Standardised monthly percentage of contracted UOAs delivered`)%>%
  mutate(Geograph='England',`Geograph Name`='England')

data_reg_UOA=plot_UDA_UOA_delivery_all_regions(data = UOA_calendar_data, 
                                               UDAorUOA = "UOA",
                                               level = "National",
                                               region_STP_name = NULL,
                                               plotChart = FALSE, 
                                               all_regions_and_STPs = TRUE)
data_reg_UOA <- data_reg_UOA %>%
  select(`Calendar month`,`Geograph Name`=`Region Name`,`Monthly UOAs delivered`,
         `Annual contracted UOAs`,`no workdays`,`Standardised monthly percentage of contracted UOAs delivered`)%>%
  mutate(Geograph='Region')

data_ICB_UOA <- Table_UDA_UOA_delivery_all_ICBs(data = UOA_calendar_data, 
                                                UDAorUOA = "UOA")

data_ICB_UOA <- data_ICB_UOA %>%
  select(`Calendar month`,`Geograph Name`=`Commissioner Name`,`Monthly UOAs delivered`,
         `Annual contracted UOAs`,`no workdays`,`Standardised monthly percentage of contracted UOAs delivered`)%>%
  mutate(Geograph='ICB')

data_UOA<- rbind(data_Nat_UOA, data_reg_UOA, data_ICB_UOA)

data<-data_UDA%>%
  full_join(data_UOA,by=c('Calendar month','Geograph Name','Geograph'))


#export each data frame to separate sheets in same Excel file
openxlsx::write.xlsx(data, file = paste0('Data_', as.character(as.Date(Sys.Date()), '%B%Y'), '.xlsx')) 
