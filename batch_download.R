# For this to run, you need the following folders in your working directory
# Data/Dental Activity
# Data/Orthodontic Activity
# Data/Contractual

library(tidyverse)

source("batch_download_function.R")

download_files(url = "https://opendata.nhsbsa.net/dataset/english-contractor-monthly-general-dental-activity",
               destination = "Data/Dental Activity/")

download_files(url = "https://opendata.nhsbsa.net/dataset/english-contractor-monthly-orthodontic-activity",
               destination = "Data/Orthodontic Activity/")

download_files(url = "https://opendata.nhsbsa.net/dataset/english-contractor-monthly-general-dental-and-orthodontic-contractual-dataset",
               destination = "Data/Contractual/")