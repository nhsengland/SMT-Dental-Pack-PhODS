# SMT-Dental-Pack-PhODS

**Author:** Pharmacy, Optometry and Dental Strategy Analysis team, NHS England

**Contact:** [england.primarycare.phodsdata\@nhs.net](mailto:england.primarycare.phodsdata@nhs.net)

## Description

This repository contains code to produce the Dental SMT Pack created by the Pharmacy, Optometry and Dental Strategy Analysis team in NHS England.

## Project Structure

This code produces two outputs:

1.  National PDF report
2.  Excel file containing data presented in the report

There are two further outputs which are not currently produced:

3.  Regional HTML report
4.  ICB HTML report

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
3.  DRAFT_SMT_dental_report_national_regional_level.rmd - creates the regional HTML report, note this file is a work in progress
4.  DRAFT_SMT_dental_report_region_ICB_level.rmd - creates the ICB HTML report, note this file is a work in progress

All other files are required to produce the outputs, for example adding the NHS England logo and setting up the format of all slides.

## Licence

Unless stated otherwise, the codebase is released under the [MIT Licence](https://github.com/nhsengland/SMT-Dental-Pack-PhODS/blob/main/LICENSE). This covers both the codebase and any sample code in the documentation.

The documentation is [© Crown copyright](https://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/) and available under the terms of the [Open Government 3.0 licence](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).
