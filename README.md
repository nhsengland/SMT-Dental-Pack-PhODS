# SMT-Dental-Pack-PhODS

**Author:** Pharmacy, Optometry and Dental Strategy Analysis team, NHS England

**Contact:** [england.primarycare.phodsdata\@nhs.net](mailto:england.primarycare.phodsdata@nhs.net)

## Description

This repository contains code to produce the Dental SMT Pack created by the Pharmacy, Optometry and Dental Strategy Analysis team in NHS England.

## Project Structure

This code produces two outputs:

1.  National PDF report
2.  Excel file containing data presented in the report
3.  ICB HTML report

There is a further output which is not currently produced:

4.  Regional HTML report

### Dependencies

The process uses the following R packages:

1.  tidyverse
2.  readxl
3.  DBI
4.  odbc
5.  downloadthis
6.  lubridate
7.  ggrepel
8.  grid
9.  formattable
10. cowplot
11. data.table
12. ggpubr
13. reactable
14. knitr
15. rmarkdown
16. ggpubr
17. gridExtra
18. scales
19. textclean
20. openxlsx
21. xaringanExtra

To install the required packages, run `source("install_requirements.R")` in the console.

### Functions

The following files define the functions used to create the reports:

1.  SQLpulls.Rmd - extracts data needed from NCDR
2.  Data_Processing.Rmd - reformats data to be used in the report
3.  plotting.Rmd - creates the charts and tables in the report
4.  appendix_functions.Rmd - carries out analysis to understand the impact of moving the pack to a new data source

### Reports

The following files create the outputs:

1.  SMT_dental_report_National_PDF.Rmd - creates the national PDF report
2.  ExportDataFile.R - creates the Excel data file
3.  ExportDataFile_metadata.R - creates the metadata table for the Excel data file
4.  SMT_dental_report_region_ICB_level.rmd - creates the ICB HTML report
5.  DRAFT_SMT_dental_report_national_regional_level.rmd - creates the regional HTML report, note this file is not currently in use

### Running the reports

The SMT_dental_report_National_PDF.Rmd file needs to be rendered separately using the 'Knit' option in RStudio. The Excel data file and ICB level HTML reports for all regions can be automatically produced by running the auto_render_SMT_report_functions.R file.

All other files are required to produce the outputs, for example setting up the format of all slides and creating a table of workdays to use for standardisation, or for internal set up, such as uploading the tables to the data warehouse.

## Licence

Unless stated otherwise, the codebase is released under the [MIT Licence](https://github.com/nhsengland/SMT-Dental-Pack-PhODS/blob/main/LICENSE). This covers both the codebase and any sample code in the documentation.

The documentation is [© Crown copyright](https://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/) and available under the terms of the [Open Government 3.0 licence](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).
