# Load necessary libraries
library(rmarkdown)
library(knitr)

#AS test trying to push to git again AGAIN

# Print the current working directory
current_wd <- getwd()
print(paste("Current working directory:", current_wd))

# Define the path for the 'reports' directory
reports_dir <- file.path(current_wd, "reports")

# Check if the 'reports' directory exists, if not, create it
if (!dir.exists(reports_dir)) {
  dir.create(reports_dir)
  # Verify if the directory was successfully created
  if (!dir.exists(reports_dir)) {
    stop("Failed to create 'reports' directory")
  }
}

# Print confirmation that the directory exists or was successfully created
print("Directory 'reports' exists or was successfully created.")

# Source the necessary RMarkdown files
source(knitr::purl("SQLpulls_2425.Rmd", output = tempfile()))
source(knitr::purl("Data_Processing.Rmd", output = tempfile()))
source(knitr::purl("plotting_2425.Rmd", output = tempfile()))
source(knitr::purl("appendix_functions.Rmd", output = tempfile()))
source("ExportDataFile.R")
source("planning_data_output.R")

# produce data file
create_export_file()

create_export_file_quartly()

# produce extract for PCDID
create_pcdid_extract()

# produce extract for UDA projections
create_uda_projections_extract()

# produce extract for NPP monitoring in ICB dashboard
create_npp_output()

# Render national report
latest_month <- paste(month.name[as.numeric(substr(max(data_dental_activity$calendar_month), 6,7))], 
                      substr(max(data_dental_activity$calendar_month), 1,4), 
                      sep = "-")

filename <- paste0(month(Sys.Date()), 
                   "-", 
                   year(Sys.Date()),
                   "-Dental-Pack-National-reporting-up-to-end-of-",
                   latest_month, 
                   '.pdf')


rmarkdown::render("SMT_dental_report_National_PDF.Rmd", 
                  output_file = filename)

# Define a function to render the report for a specific region
render_report = function(region) {
  filename <- paste0(reports_dir, 
                     '/',
                     month(Sys.Date()), 
                     "_", 
                     year(Sys.Date()),
                     "_Dental_Pack_",
                     gsub(" ", "_", region),
                     "_ICBs_reporting_to_",
                     gsub(" ", "_",latest_month), 
                     '.html')
  
  rmarkdown::render(
    "SMT_dental_report_region_ICB_level.rmd", 
    params = list(region = region),
    output_file = filename
  )
}

# Render the reports for the specified regions
render_report("South West")
render_report("Midlands")
render_report("North East And Yorkshire")
render_report("London")
render_report("South East")
render_report("North West")
render_report("East Of England")



##for debugging converting to pdf errors
#step 1
update.packages(ask = FALSE, checkBuilt = TRUE)
tinytex::tlmgr_update()
#step 2
tinytex::reinstall_tinytex()
#if still not working, go to debugging site  link in error message for further solutions.

