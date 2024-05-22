
regions <- unique(UDA_calendar_data$region_name)

render_report <- function(region) {
  rmarkdown::render(
    "rmarkdown/SMT_dental_report_region_ICB_level.rmd", params = list(
      region = region),
    output_file = paste0("05_2024_SMT_Dental_Pack_", gsub(" ", "", region),"_ICBs_reporting_up_to_end_of_April_2024.html")
  )
}

for(r in regions){
  render_report(r)
}