bind_all_files <- function(download_filepath, output_filepath){
  downloaded_files <- list.files(path = {download_filepath}, pattern = ".csv", full.names = TRUE)
  
  data <- downloaded_files %>% 
    map_df(~read_csv(.x, col_types = cols(.default = col_character())))
  
  write.csv(data, {output_filepath}, row.names = FALSE)
}