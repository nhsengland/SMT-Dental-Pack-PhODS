# Load necessary libraries
library(rmarkdown)
library(knitr)

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
source(knitr::purl("SQLpulls.Rmd", output = tempfile()), local = TRUE)
source(knitr::purl("Data_Processing.Rmd", output = tempfile()))
source(knitr::purl("plotting.Rmd", output = tempfile()))
source(knitr::purl("appendix_functions.Rmd", output = tempfile()))

# produce data file
source("ExportDataFile.R")
create_export_file()

# produce extract for PCDID
create_pcdid_extract()

# produce extract for UDA projections
create_uda_projections_extract()

# Define a function to render the report for a specific region
render_report = function(region) {
  rmarkdown::render(
    "SMT_dental_report_region_ICB_level.rmd", 
    params = list(region = region),
    output_file = file.path(reports_dir, paste0(gsub(" ", "_", region), "_Region_Report_", format(Sys.Date(), '%B%Y'), ".html"))
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

