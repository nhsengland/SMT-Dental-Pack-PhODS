# we will first create a folder to save our output
# Print the current working directory
current_wd <- getwd()
print(paste("Current working directory:", current_wd))

# Check if the reports directory exists
reports_dir <- file.path(current_wd, "reports")
if (!dir.exists(reports_dir)) {
  # Try to create the directory
  dir.create(reports_dir)
  # Verify if the directory was successfully created
  if (!dir.exists(reports_dir)) {
    stop("Failed to create 'reports' directory")
  }
}

# Print confirmation that the directory exists
print("Directory 'reports' exists or was successfully created.")

#render monthly report
library(rmarkdown)

source(knitr::purl("r/SQLpulls.Rmd", output = tempfile()), local = TRUE)
source(knitr::purl("r/Data_Processing.Rmd", output = tempfile()))
source(knitr::purl("r/plotting.Rmd", output = tempfile()))


# Render the RMarkdown document
rmarkdown::render(input = "rmarkdown/SMT_dental_report_National_PDF.Rmd",
                  output_format = "beamer_presentation",
                  output_file = paste0(reports_dir, "/SMT Dental Pack Monthly - National ", Sys.Date(), ".pdf"))

source("r/ExportDataFile.R")


##for debugging converting to pdf errors
#step 1
update.packages(ask = FALSE, checkBuilt = TRUE)
tinytex::tlmgr_update()
#step 2
tinytex::reinstall_tinytex()
#if still not working, go to debugging site  link in error message for further solutions.
