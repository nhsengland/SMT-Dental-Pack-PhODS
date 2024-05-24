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


######UDA
#### No UDA delivered, contracted & percentage
data_Nat_UDA=plot_UDA_UOA_delivery_wd(data = UDA_calendar_data, 
                         UDAorUOA = "UDA",
                         plotChart = FALSE, 
                         all_regions_and_STPs = FALSE,
                         level = "National",
                         region_STP_name = NULL)

data_Nat_UDA<- data_Nat_UDA %>% 
  select(`Calendar month`,`Annual contracted UDAs`,`Monthly UDAs delivered`,`no workdays`,`Standardised monthly percentage of contracted UDAs delivered`)%>%
  mutate(`Geography Level`='National',`Geography Name`='England')

data_reg_UDA=plot_UDA_UOA_delivery_all_regions(data = UDA_calendar_data, 
                                  UDAorUOA = "UDA",
                                  level = "National",
                                  region_STP_name = NULL,
                                  plotChart = FALSE, 
                                  all_regions_and_STPs = TRUE)
data_reg_UDA <- data_reg_UDA %>%
  select(`Calendar month`,`Geography Name`=`Region Name`,`Annual contracted UDAs`,`Monthly UDAs delivered`,
         `no workdays`,`Standardised monthly percentage of contracted UDAs delivered`)%>%
  mutate(`Geography Level`='Regional')

data_ICB_UDA <- Table_UDA_UOA_delivery_all_ICBs(data = UDA_calendar_data, 
                                UDAorUOA = "UDA")

data_ICB_UDA <- data_ICB_UDA %>%
  select(`Calendar month`,`Geography Name`=`Commissioner Name`,`Annual contracted UDAs`,`Monthly UDAs delivered`,
         `no workdays`,`Standardised monthly percentage of contracted UDAs delivered`)%>%
  mutate(`Geography Level`='ICB')

data_UDA_de_co<- rbind(data_Nat_UDA, data_reg_UDA, data_ICB_UDA) %>% 
  rename("Workdays" = `no workdays`)%>%
  select("Calendar month","Geography Level","Geography Name","Annual contracted UDAs","Monthly UDAs delivered",
          "Workdays","Standardised monthly percentage of contracted UDAs delivered")

####YTD delivery
data_Nat_YTD_UDA<- Table_YTD_UDA_UOA_delivery (data = UDA_calendar_data, 
                                       UDAorUOA = "UDA",
                                       level = "National",
                                       all_regions_and_STPs = FALSE)
data_Nat_YTD_UDA<- data_Nat_YTD_UDA %>% 
  mutate(`Geography Level`='National',`Geography Name`='England')

data_reg_YTD_UDA<- Table_YTD_UDA_UOA_delivery (data = UDA_calendar_data, 
                                               UDAorUOA = "UDA",
                                               level = "Regional",
                                               all_regions_and_STPs =TRUE)
data_reg_YTD_UDA <- data_reg_YTD_UDA %>%
  rename(`Geography Name`=`region_name`)%>%
  mutate(`Geography Level`='Regional')

data_icb_YTD_UDA<- Table_YTD_UDA_UOA_delivery (data = UDA_calendar_data, 
                                               UDAorUOA = "UDA",
                                               level = "STP",
                                               all_regions_and_STPs =TRUE)
data_icb_YTD_UDA <- data_icb_YTD_UDA %>%
  rename(`Geography Name`=`commissioner_name`)%>%
  mutate(`Geography Level`='ICB')

data_UDA_YTD<- rbind(data_Nat_YTD_UDA, data_reg_YTD_UDA, data_icb_YTD_UDA)

data_UDA<-data_UDA_de_co%>%
    full_join(data_UDA_YTD,by=c('Calendar month','Geography Name','Geography Level'))



######UOA
#### No UDA delivered, contracted & percentage
data_Nat_UOA=plot_UDA_UOA_delivery_wd(data = UOA_calendar_data, 
                                      UDAorUOA = "UOA",
                                      plotChart = FALSE, 
                                      all_regions_and_STPs = FALSE,
                                      level = "National",
                                      region_STP_name = NULL)

data_Nat_UOA<- data_Nat_UOA %>% 
  select(`Calendar month`,`Annual contracted UOAs`,`Monthly UOAs delivered`,`no workdays`,`Standardised monthly percentage of contracted UOAs delivered`)%>%
  mutate(`Geography Level`='National',`Geography Name`='England')

data_reg_UOA=plot_UDA_UOA_delivery_all_regions(data = UOA_calendar_data, 
                                               UDAorUOA = "UOA",
                                               level = "National",
                                               region_STP_name = NULL,
                                               plotChart = FALSE, 
                                               all_regions_and_STPs = TRUE)
data_reg_UOA <- data_reg_UOA %>%
  select(`Calendar month`,`Geography Name`=`Region Name`,`Annual contracted UOAs`,`Monthly UOAs delivered`,
         `no workdays`,`Standardised monthly percentage of contracted UOAs delivered`)%>%
  mutate(`Geography Level`='Regional')

data_ICB_UOA <- Table_UDA_UOA_delivery_all_ICBs(data = UOA_calendar_data, 
                                                UDAorUOA = "UOA")

data_ICB_UOA <- data_ICB_UOA %>%
  select(`Calendar month`,`Geography Name`=`Commissioner Name`,`Annual contracted UOAs`,`Monthly UOAs delivered`,
         `no workdays`,`Standardised monthly percentage of contracted UOAs delivered`)%>%
  mutate(`Geography Level`='ICB')

data_UOA_de_co<- rbind(data_Nat_UOA, data_reg_UOA, data_ICB_UOA) %>% 
  rename("Workdays" = `no workdays`)%>%
  select("Calendar month","Geography Level","Geography Name","Annual contracted UOAs","Monthly UOAs delivered",
         "Workdays","Standardised monthly percentage of contracted UOAs delivered")

####YTD delivery
data_Nat_YTD_UOA<- Table_YTD_UDA_UOA_delivery (data = UOA_calendar_data, 
                                               UDAorUOA = "UOA",
                                               level = "National",
                                               all_regions_and_STPs = FALSE)
data_Nat_YTD_UOA<- data_Nat_YTD_UOA %>% 
  mutate(`Geography Level`='National',`Geography Name`='England')

data_reg_YTD_UOA<- Table_YTD_UDA_UOA_delivery (data = UOA_calendar_data, 
                                               UDAorUOA = "UOA",
                                               level = "Regional",
                                               all_regions_and_STPs =TRUE)
data_reg_YTD_UOA <- data_reg_YTD_UOA %>%
  rename(`Geography Name`=`region_name`)%>%
  mutate(`Geography Level`='Regional')

data_icb_YTD_UOA<- Table_YTD_UDA_UOA_delivery (data = UOA_calendar_data, 
                                               UDAorUOA = "UOA",
                                               level = "STP",
                                               all_regions_and_STPs =TRUE)
data_icb_YTD_UOA <- data_icb_YTD_UOA %>%
  rename(`Geography Name`=`commissioner_name`)%>%
  mutate(`Geography Level`='ICB')

data_UOA_YTD<- rbind(data_Nat_YTD_UOA, data_reg_YTD_UOA, data_icb_YTD_UOA)

data_UOA<-data_UOA_de_co%>%
  full_join(data_UOA_YTD,by=c('Calendar month','Geography Name','Geography Level'))


data<- list('UDA' = data_UDA, 'UOA' = data_UOA)

openxlsx::write.xlsx(data, file = paste0('Data_', as.character(as.Date(Sys.Date()), '%B%Y'), '.xlsx')) 


#export each data frame to separate sheets in same Excel file
#data<-data_UDA%>%
#  full_join(data_UOA,by=c('Calendar month','Geography Name','Geography Level'))

#output <- createWorkbook()
#number_style <- createStyle(numFmt = "0%")

#addWorksheet(output, "UDA")
#writeData(output, "UDA", data_UDA)
#setColWidths(output, "UDA", cols = 1:7, widths = "auto")
#addStyle(output, "UDA", style = number_style, cols = 5, rows = 2:5000)

#addWorksheet(output, "UOA")
#writeData(output, "UOA", data_UOA)
#setColWidths(output, "UOA", cols = 1:7, widths = "auto")
#addStyle(output, "UOA", style = number_style, cols = 5, rows = 2:5000)

#saveWorkbook(output, paste0('Data_', as.character(as.Date(Sys.Date()), '%B%Y'), '.xlsx'), overwrite = TRUE) 
