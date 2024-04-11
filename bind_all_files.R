library(tidyverse)

source("bind_all_files_function.R")

bind_all_files(download_filepath = "Data/Dental Activity", 
               output_filepath = "Data/Dental Activity/time_series_dental_activity.csv")

bind_all_files(download_filepath = "Data/Orthodontic Activity", 
               output_filepath = "Data/Orthodontic Activity/time_series_orthodontic_activity.csv")

bind_all_files(download_filepath = "Data/Contractual", 
               output_filepath = "Data/Contractual/time_series_contractual.csv")