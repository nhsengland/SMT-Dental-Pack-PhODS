#### UDA/UOA %delivered/contracted ####
# standardised by working days without historical data 
# include all ICBs
plot_UDA_UOA_delivery_all_ICBs <- function(data = UDA_calendar_data,
                                           UDAorUOA = "UOA",
                                           plotChart = TRUE,
                                           region_STP_name='London'){

  if(UDAorUOA == "UDA"){
    #include data only after 2020-04-01 and select fields needed
    data <- data %>%
      filter(region_name == region_STP_name)

    data <- data %>%
      mutate(month = as.Date(month)) %>%
      filter(month >= as.Date ("2020-04-01")) %>%
      select(month, contract_number, commissioner_name, region_name, annual_contracted_UDA, UDA_band_1,UDA_band_2,UDA_band_3,UDA_other,UDA_urgent,UDA_delivered)

    # get data into the right format
    # calculate the annual contracted UDA, delivered UDA and percentage of UDA delivered (standardized by working days)
    data <- get_delivery_data(data = data, UDAorUOA = "UDA", all_regions_and_STPs = TRUE)
    data <- data %>%
      filter(month>"2020-03-01")

    data <- data %>%
      group_by(month, region_name,commissioner_name) %>%
      summarise(monthly_UDA_UOAs_BAND1_delivered = sum(monthly_UDA_UOAs_BAND1_delivered, na.rm = TRUE),
              monthly_UDA_UOAs_BAND2_delivered = sum(monthly_UDA_UOAs_BAND2_delivered, na.rm = TRUE),
              monthly_UDA_UOAs_BAND3_delivered = sum(monthly_UDA_UOAs_BAND3_delivered, na.rm = TRUE),
              monthly_UDA_UOAs_BAND_OTHER_delivered = sum(monthly_UDA_UOAs_BAND_OTHER_delivered, na.rm = TRUE),
              monthly_UDA_UOAs_BAND_URGENT_delivered = sum(monthly_UDA_UOAs_BAND_URGENT_delivered, na.rm = TRUE),
              monthly_UDA_UOAs_delivered = sum(monthly_UDA_UOAs_delivered, na.rm = TRUE),
                annual_contracted_UDA_UOA = sum(annual_contracted_UDA_UOA, na.rm = TRUE)) %>%
      filter(!is.na(region_name))

    data <- data %>% left_join(working_days,by=c('month')) %>%
      mutate(perc_standardised_wd_int =
               100*(monthly_UDA_UOAs_delivered /(annual_contracted_UDA_UOA*(`no workdays`/`total workdays`))))%>%
      mutate(perc_standardised_wd = round(perc_standardised_wd_int, digits = 0))

    title <- "Calendar monthly percentage of usual annual contracted UDAs \nsubmitted across all contracts standardised by working days"
    ylab <- "% of contracted UDAs submitted"
    # captionTitle <- "*These are calendar months and data has been scaled up by 12."
    lineCol <- "coral"
    lineCol <- "#CC79A7"

  }else{

    #include only the region/ICB needed -- include data only after 2020-04-01 and select fields needed
    data <- data %>%
      filter(region_name == region_STP_name)

    data <- data %>%
      mutate(month = as.Date(month)) %>%
      filter(month >= as.Date ("2020-04-01")) %>%
      select(month, contract_number, commissioner_name, region_name,
             annual_contracted_UOA, UOA_band_1,UOA_band_2,UOA_band_3,UOA_other,UOA_urgent,UOA_delivered)

    #get data into the right format
    # calculate the annual contracted UOA, delivered UOA and percentage of UOA delivered (standardized by working days)
    data <- get_delivery_data(data = data, UDAorUOA = "UOA", all_regions_and_STPs = TRUE)
    data <- data %>%
      filter(month>"2020-03-01")

    data <- data %>%
      group_by(month, region_name,commissioner_name) %>%
      summarise(monthly_UDA_UOAs_BAND1_delivered = sum(monthly_UDA_UOAs_BAND1_delivered, na.rm = TRUE),
              monthly_UDA_UOAs_BAND2_delivered = sum(monthly_UDA_UOAs_BAND2_delivered, na.rm = TRUE),
              monthly_UDA_UOAs_BAND3_delivered = sum(monthly_UDA_UOAs_BAND3_delivered, na.rm = TRUE),
              monthly_UDA_UOAs_BAND_OTHER_delivered = sum(monthly_UDA_UOAs_BAND_OTHER_delivered, na.rm = TRUE),
              monthly_UDA_UOAs_BAND_URGENT_delivered = sum(monthly_UDA_UOAs_BAND_URGENT_delivered, na.rm = TRUE),
              monthly_UDA_UOAs_delivered = sum(monthly_UDA_UOAs_delivered, na.rm = TRUE),
                annual_contracted_UDA_UOA = sum(annual_contracted_UDA_UOA, na.rm = TRUE)) %>%
      filter(!is.na(region_name))

    data <- data %>%
      left_join(working_days,by=c('month')) %>%
      mutate(perc_standardised_wd_int =
               100*(monthly_UDA_UOAs_delivered /(annual_contracted_UDA_UOA*(`no workdays`/`total workdays`)))) %>%

      title <- "Calendar monthly percentage of usual annual contracted UOAs \nsubmitted across all contracts standardised by working days"
    ylab <- "% of contracted UOAs submitted"
    # captionTitle <- "*These are calendar months and data has been scaled up by 12."
    lineCol <- "coral"
    lineCol <- "#CC79A7"

  }

  if(plotChart == TRUE){

    #plot code
    p <- ggplot(data,
                aes(x = month,
                    y = perc_standardised_wd,
                    colour = commissioner_name)) +
      theme_bw() +
      geom_line(size = 1) +
      geom_point() +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b-%y") +
      scale_y_continuous(limits = c(0, max(c(data$perc_standardised_wd, 95), na.rm = T) + 5),
                         breaks = seq(0, max(c(data$perc_standardised_wd, 95), na.rm = T) + 5, 10)) +
      labs(title = title,
           x = "Month",
           y = ylab,
           #caption = captionTitle,
           colour = "commissioner_name") +
      theme(axis.text.x = element_text(angle = 90, vjust=-0.0001))

    p

  }else{

    #if we only need a table instead of a plot -- rename
   if(UDAorUOA == "UDA"){
      new_col_names <- c("calendar_month" = "month",
                         "UDAs_delivered_month" = "monthly_UDA_UOAs_delivered",
                         "UDAs_annual_contracted" = "annual_contracted_UDA_UOA",
                         "UDAs_delivered_month_percent_contracted_standardised" = "perc_standardised_wd")
    }else{

      new_col_names <- c("calendar_month" = "month",
                         "UOAs_delivered_month" = "monthly_UDA_UOAs_delivered",
                         "UOAs_annual_contracted" = "annual_contracted_UDA_UOA",
                         "UOAs_delivered_month_percent_contracted_standardised" = "perc_standardised_wd")
    }

    data$`month` <- format(as.Date(data$`month`), "%Y-%m")
    data <- data %>%
      rename(any_of(new_col_names))
  }

}

#### number of contracts ####
get_num_contracts <- function(data = UDA_calendar_data,
                              UDAorUOA = "UDA",
                              level = "National",
                              region_STP_name = NULL){

  #filter for STP or region in need
  if(level == "Regional"){

    data <- data %>%
      filter(region_name == region_STP_name)

  }else if(level == "STP"){

    data <- data %>%
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }

  data <- data %>%
    filter(month == max(data$month))

  #calculate the no of contractors
  nrow(data)
}

#### breakdown_111_referrals ####
plot_breakdown_111_referrals <- function(data = dental_data_111,
                                         plotChart = TRUE){

  #replace disposition_text == "Attend Emergency Dental Treatment Centre within 4" to "Attend Emergency Dental Treatment Centre within 4hrs"
  data <- data %>%
    mutate(disposition_text = if_else(disposition_text == "Attend Emergency Dental Treatment Centre within 4",
                                      "Attend Emergency Dental Treatment Centre within 4hrs",
                                      disposition_text)) %>%
    mutate(month = as.Date(month))

  #get disposition_text lines in the right order
  data$disposition_text <- factor(data$disposition_text,
                                  levels = c("Refer to Dental Treatment Centre within 1 hour",
                                             "Refer to Dental Treatment Centre within 4 hours",
                                             "To Speak to a Dental Service within 1 hour",
                                             "To Speak to a Dental Service within 2 hours",
                                             "Speak to a Dental Service within 2 hours",
                                             "To Speak to a Dental Service within 6 hours",
                                             "To Speak to a Dental Service within 12 hours",
                                             "To Speak to a Dental Service within 24 hours",
                                             "To Speak to a Dental Practice within 7 days",
                                             "Contact Orthodontist next working day"))

  #plot code
  p <- ggplot(data) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_line(aes(x = month,
                  y = monthly_case_volume,
                  colour = str_wrap(disposition_text, 35)),
              size = 1) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b-%y") +
    labs(title = "111 call volume recommended towards dental services",
         x = "Month",
         y = "Call volume",
         subtitle = "England",
         colour = "Disposition"#,
         #caption = "*services include: "
    )

  if(plotChart == TRUE){

    p
  }else{

    data %>%
      select(`Month` = month,
             `Disposition` = disposition_text,
             `Monthly case volume` = monthly_case_volume)
  }

}

#### DCP_analysis_regional_check ####
plot_DCP_analysis_regional_check <- function(data = UDA_calendar_data,
                                             dcp_data = DCP_data,
                                             UDA_or_FP17 = "UDA",
                                             level = "National",
                                             region_STP_name = NULL){

  dcp_data <- dcp_data %>%
    rename(month = Month)

  #filter for region or STP
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name )

    dcp_data <- dcp_data %>%
      filter(Region == region_STP_name)

    subtitle <- region_STP_name
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == region_STP_name)

    dcp_data <- dcp_data %>%
      filter(commissioner_name == region_STP_name)

    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }

  colnames(UDA_calendar_data)

   #calculate total FP19, UDA_B1, B2, B3 and urgent by month for "Total_dentist_only_and_DCP_assisted' by using UDA Calendar data
  delivery_total <-  data %>%
    rename(Region = region_name) %>%
    group_by(month, Region) %>%
    dplyr::summarise( total_FP17 = sum(general_FP17s, na.rm = TRUE),
                      total_B1 = sum(UDA_band_1, na.rm = TRUE),
                      total_B2 = sum(UDA_band_2, na.rm = TRUE),
                      total_B3 = sum(UDA_band_3, na.rm = TRUE),
                      total_urgent = sum(UDA_urgent, na.rm = TRUE)) %>%
    mutate (DCP_description = "Total_dentist_only_and_DCP_assisted") %>%
    select (month, Region, DCP_description, total_FP17,total_B1, total_B2, total_B3, total_urgent)

   #calculate total FP19, UDA_B1, B2, B3 and urgent by month for separate DCP description by using DCP data
  dcp_main_new <- dcp_data %>%
    filter(DCP_description != 'Clinical Technician') %>%
    filter(DCP_description != 'Technician') %>%
    mutate(DCP_description = replace(DCP_description, DCP_description== "Nurse", "Dental_Nurse_assisted"),
           DCP_description = replace(DCP_description, DCP_description=="Dental Hygienist", "Hygienist_assisted"),
           DCP_description = replace(DCP_description, DCP_description=="Dental Therapist", "Therapist_assisted"),
           DCP_description = replace(DCP_description, DCP_description=="Hygienist", "Hygienist_assisted"),
           DCP_description = replace(DCP_description, DCP_description=="Therapist", "Therapist_assisted"))

  dcp_summary <- dcp_main_new %>%
    group_by(month, Region, DCP_description) %>%
    summarise (total_FP17 = sum(FP17_Current_Year_total, na.rm = TRUE),
               total_B1 = sum(Band_1._UDA, na.rm = TRUE),
               total_B2 = sum(Band_2._UDA, na.rm = TRUE),
               total_B3 = sum(Band_3._UDA, na.rm = TRUE),
               total_urgent = sum(Urgent_UDA, na.rm = TRUE))%>%
    rename(`Band 1` = total_B1, `Band 2` = total_B2, `Band 3` = total_B3, Urgent = total_urgent)

  #change the format of total delivery and separate dcp and then get full data ready for plot
  dcp_summary_longer <- dcp_summary %>%
    pivot_longer(cols = starts_with("total"),
                 names_to = "Bands",
                 names_prefix = "dcp",
                 values_to = "numbers",
                 values_drop_na = TRUE)

  delivery_total_longer <- delivery_total %>%
    pivot_longer(cols = starts_with("total"),
                 names_to = "Bands",
                 names_prefix = "dcp",
                 values_to = "all_numbers",
                 values_drop_na = TRUE)

  all_lookup <- left_join(dcp_summary_longer, delivery_total_longer,
                          by = c("month", "Bands", "Region")) %>%
    select (month, DCP_description.x, Bands, numbers, DCP_description.y, all_numbers)

  #calculate the percentage of each dcp description/total delivery
  total <- all_lookup %>%
    mutate(assisted_percent = formattable::percent (numbers / all_numbers, digits=2))

  if(UDA_or_FP17 == "UDA"){

    #UDA is only for those with 'band'
    filtered_data_UDA = filter(total, Bands %in% c("Band 1",	"Band 2",	"Band 3", "Urgent")) %>%
      select(month, "DCP_description.x", "Bands", "assisted_percent") %>%
      rename (DCP_description = DCP_description.x) %>%
      mutate(month = as.Date(month)) %>%
      mutate(DCP_description = str_replace_all(DCP_description, "_", " "))

    max_month <- max(filtered_data_UDA$month)

    data_labels <- filtered_data_UDA %>%
      filter(month == max_month)


    UDA_plot <- ggplot(filtered_data_UDA,
                       aes(x=month, y= assisted_percent)) +
      facet_grid(cols = vars(DCP_description)) +
      geom_line(aes(colour = Bands),
                size = 1) +
      geom_text_repel(aes(label = assisted_percent),
                      data = data_labels,
                      colour = "black",
                      position = "dodge") +
      scale_fill_manual(values = c("#009E73", "#F0E442", "#D55E00"),
                        labels = c("Dental Nurse assisted", "Hygienist assisted", "Therapist assisted")) +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b-%y") +
      theme_bw() +
      labs(title = "Percentage of total UDAs delivered which had DCP* assistance by DCP roles",
           subtitle = subtitle,
           x = "Month",
           y = "Percentage of total UDAs delivered",
           colour = "Bands",
           caption = "*Dental Care Practitioner") +
      scale_y_continuous(labels = scales::percent)+
      theme(axis.text.x = element_text(angle = 90, vjust=-0.0001))

    UDA_plot

  }else{

    #fp17 is for those with 'total_FP17'
    filtered_data_FP17 <- total %>%
      filter( Bands %in% c("total_FP17")) %>%
      select("month", "DCP_description.x", "Bands", "assisted_percent") %>%
      mutate(month = as.Date(month)) %>%
      rename ( Percent_of_FP17 = Bands, DCP_description = DCP_description.x) %>%
      mutate(DCP_description = str_replace_all(DCP_description, "_", " "))


    #get the number for the latest month -- used for label in the plot
    latest_month <- max(filtered_data_FP17$month)
    latest_data <- filtered_data_FP17 %>%
    filter(month == latest_month)

    FP17_plot <- ggplot(filtered_data_FP17,
                        aes(x=month, y= assisted_percent)) +
      geom_line(aes(colour = DCP_description),
                size = 1) +
      geom_text_repel(data = latest_data, aes(label = assisted_percent),
                    colour = "black", size = 3.5, vjust = -0.25) +
      theme(legend.position="bottom") +
      theme_bw() +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b-%y") +
      labs(title = "Percentage of total Courses of Treatment (CoTs) delivered which had DCP* assistance",
           subtitle = subtitle,
           x = "Month",
           y = "Percentage of total CoTs delivered",
           colour = "DCP description",
           caption = "*Dental Care Practitioner") +
      scale_y_continuous(labels = scales::percent)  +
      theme(axis.text.x = element_text(angle = 90, vjust=-0.0001))

    FP17_plot

  }
}

#### DCP_analysis_ICB_check ####
plot_DCP_analysis_ICB_check <- function(data = UDA_calendar_data,
                                        dcp_data = DCP_data,
                                        UDA_or_FP17 = "UDA",
                                        level = "National",
                                        region_STP_name = NULL){

  dcp_data <- dcp_data %>%
    rename(month = Month)

  #filter for region or STP
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name)

    dcp_data <- dcp_data %>%
      filter(Region == region_STP_name)

    subtitle <- region_STP_name
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == region_STP_name)

    dcp_data <- dcp_data %>%
      filter(commissioner_name == region_STP_name)

    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }

  colnames(UDA_calendar_data)
  colnames(dcp_data)

#calculate total FP19, UDA_B1, B2, B3 and urgent by month for "Total_dentist_only_and_DCP_assisted' by using UDA Calendar data
  delivery_total <-  data %>%
    rename(Region = region_name) %>%
    group_by(month, commissioner_name) %>%
    dplyr::summarise(total_FP17 = sum(general_FP17s, na.rm = TRUE),
                     total_B1 = sum(UDA_band_1, na.rm = TRUE),
                     total_B2 = sum(UDA_band_2, na.rm = TRUE),
                     total_B3 = sum(UDA_band_3, na.rm = TRUE),
                     total_urgent = sum(UDA_urgent, na.rm = TRUE)) %>%
    mutate(DCP_description = "Total_dentist_only_and_DCP_assisted") %>%
    select(month, commissioner_name, DCP_description, total_FP17,total_B1, total_B2, total_B3, total_urgent)

  #calculate total FP19, UDA_B1, B2, B3 and urgent by month for separate DCP description by using DCP data
  dcp_main_new <- dcp_data %>%
    filter(DCP_description != 'Clinical Technician') %>%
    filter(DCP_description != 'Technician') %>%
    mutate(DCP_description = replace(DCP_description, DCP_description== "Nurse", "Dental_Nurse_assisted"),
           DCP_description = replace(DCP_description, DCP_description=="Dental Hygienist", "Hygienist_assisted"),
           DCP_description = replace(DCP_description, DCP_description=="Dental Therapist", "Therapist_assisted"),
           DCP_description = replace(DCP_description, DCP_description=="Hygienist", "Hygienist_assisted"),
           DCP_description = replace(DCP_description, DCP_description=="Therapist", "Therapist_assisted"))

  dcp_summary <- dcp_main_new %>%
    group_by(month, commissioner_name, DCP_description) %>%
    summarise(total_FP17 = sum(FP17_Current_Year_total, na.rm = TRUE),
              total_B1 = sum(Band_1._UDA, na.rm = TRUE),
              total_B2 = sum(Band_2._UDA, na.rm = TRUE),
              total_B3 = sum(Band_3._UDA, na.rm = TRUE),
              total_urgent = sum(Urgent_UDA, na.rm = TRUE)) %>%
    rename(`Band 1` = total_B1, `Band 2` = total_B2, `Band 3` = total_B3, Urgent = total_urgent)

  #change the format of total delivery and separate dcp and then get full data ready for plotting
  dcp_summary_longer <- dcp_summary %>%
    pivot_longer(cols = starts_with("total"),
                 names_to = "Bands",
                 names_prefix = "dcp",
                 values_to = "numbers",
                 values_drop_na = TRUE)

  delivery_total_longer <- delivery_total %>%
    pivot_longer(cols = starts_with("total"),
                 names_to = "Bands",
                 names_prefix = "dcp",
                 values_to = "all_numbers",
                 values_drop_na = TRUE)

  all_lookup <- left_join(dcp_summary_longer, delivery_total_longer,
                          by = c("month", "Bands", "commissioner_name")) %>%
    select(month, DCP_description.x, Bands, numbers, DCP_description.y, all_numbers)

   #calculate the percentage of each dcp description/total delivery
  total <- all_lookup %>%
    mutate (assisted_percent = formattable::percent (numbers / all_numbers, digits=2))

  if(UDA_or_FP17 == "UDA"){
    #UDA is only for those with 'band'
    filtered_data_UDA = filter(total, Bands %in% c("Band 1",	"Band 2",	"Band 3", "Urgent")) %>%
      select(month, "DCP_description.x", "Bands", "assisted_percent") %>%
      rename(DCP_description = DCP_description.x) %>%
      mutate(month = as.Date(month)) %>%
      mutate(DCP_description = str_replace_all(DCP_description, "_", " "))

    max_month <- max(filtered_data_UDA$month)

    data_labels <- filtered_data_UDA %>%
      filter(month == max_month)

    UDA_plot <- ggplot(filtered_data_UDA,
                       aes(x=month, y= assisted_percent)) +
      facet_grid(cols = vars(DCP_description)) +
      geom_line(aes(colour = Bands),
                size = 1) +
      geom_text_repel(aes(label = assisted_percent),
                      data = data_labels,
                      colour = "black",
                      position = "dodge") +
      scale_fill_manual(values = c("#009E73", "#F0E442", "#D55E00"),
                        labels = c("Dental Nurse assisted", "Hygienist assisted", "Therapist assisted")) +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b-%y") +
      theme_bw() +
      labs(title = "Percentage of total UDAs delivered which had DCP* assistance by DCP roles",
           subtitle = subtitle,
           x = "Month",
           y = "Percentage of total UDAs delivered",
           colour = "Bands",
           caption = "*Dental Care Practitioner") +
      scale_y_continuous(labels = scales::percent)+
      theme(axis.text.x = element_text(angle = 90, vjust=-0.0001))

    UDA_plot

  }else{
    #fp17 is for those with 'total_FP17'
    filtered_data_FP17 <- total %>%
      filter(Bands %in% c("total_FP17")) %>%
      select("month", "DCP_description.x", "Bands", "assisted_percent") %>%
      mutate(month = as.Date(month)) %>%
      rename(Percent_of_FP17 = Bands, DCP_description = DCP_description.x) %>%
      mutate(DCP_description = str_replace_all(DCP_description, "_", " "))

    #get the number for the latest month -- used for label in the plot
    latest_month <- max(filtered_data_FP17$month)
    latest_data <- filtered_data_FP17 %>%
    filter(month == latest_month)

    FP17_plot <- ggplot(filtered_data_FP17,
                        aes(x=month, y= assisted_percent)) +
      geom_line(aes(colour = DCP_description),
                size = 1) +
      geom_text_repel(data = latest_data, aes(label = assisted_percent),
                    colour = "black", size = 3.5, vjust = -0.25) +
      theme(legend.position="bottom") +
      theme_bw() +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b-%y") +
      labs(title = "Percentage of total Courses of Treatment (CoTs) delivered which had DCP* assistance",
           subtitle = subtitle,
           x = "Month",
           y = "Percentage of total CoTs delivered",
           colour = "DCP description",
           caption = "*Dental Care Practitioner") +
      scale_y_continuous(labels = scales::percent)  +
      theme(axis.text.x = element_text(angle = 90, vjust=-0.0001))

    FP17_plot


  }
}