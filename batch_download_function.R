batch_download <- function(url, destination){
  # url of page with files to be downloaded
  url <- {url}
  
  # read HTML of the page
  html <- readLines(url)
  
  # extract links with start of dataset download URL
  pattern <- "<a href=\"https://opendata.nhsbsa.net/dataset/"
  
  data_links <- grep(pattern, html, value=TRUE)
  
  # extract links with .csv pattern
  csv_files <- as.data.frame(grep(".csv", data_links, value=TRUE))
  
  names(csv_files) <- "links"
  
  # remove start of HTML code
  files <- csv_files %>% 
    mutate(links = str_sub(links, 18), 
           links = str_replace(links, ' class="resource-url-analytics" target="_blank">', ""))
  
  # create destination file path for downloads
  files <- files %>% 
    mutate(dest = paste({destination}, str_sub(links, -11), sep = ""), 
           dest = str_sub(dest, end = -2))
  
  # create list of files to download and destination
  file_list <- files$links
  dest <- files$dest
  
  # loop through file_list and download each file
  getOption("timeout")
  
  options(timeout = 300)
  
  for (i in seq_along(file_list)){
    download.file(file_list[i], dest[i], mode = "wb")
  }
}