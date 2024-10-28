library(tidyverse)
library(readxl)
library(DBI)
library(odbc)
library(reactable)
library(downloadthis)
library(lubridate)
library(knitr)
library(ggrepel)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(dplyr)


################### prepare you own data
# in this study, we first source all files (in SMT-Dental-Pack-PhODS github) to get source data we need
source(knitr::purl("SQLpulls.Rmd", output = tempfile()), local = TRUE)
source(knitr::purl("Data_Processing.Rmd", output = tempfile()))

# then clean source data
region_STP_name='South West' ## insert the region you need

# way to calculate percentage of UDA delivered which is standardized by no of working days 
# national level
data1 <- UDA_calendar_data %>%
  group_by(month) %>%
  summarise(UDA_delivery = sum(UDA_delivered, na.rm = TRUE),
            contracted_UDAs = sum(annual_contracted_UDA, na.rm = TRUE)) %>%
  filter(month >= as.Date("2023-04-01") & month <= as.Date("2024-07-01")) %>% 
  left_join(working_days,by=c('month')) %>% 
  mutate(perc_UDA_delivered = 100* (UDA_delivery /(contracted_UDAs*(`no workdays`/`total workdays`)))) %>%
  mutate(perc_UDA_delivered = round(perc_UDA_delivered, digits = 0))%>%
  mutate(region_name=region_STP_name,commissioner_name='National')

# region & ICB level, and then row bind with the national level data
data2 <- UDA_calendar_data %>%
  group_by(month,region_name,commissioner_name) %>%
  summarise(UDA_delivery = sum(UDA_delivered, na.rm = TRUE),
            contracted_UDAs = sum(annual_contracted_UDA, na.rm = TRUE)) %>% 
  filter(month >= as.Date("2023-04-01") & month <= as.Date("2024-07-01")) %>% 
  left_join(working_days,by=c('month')) %>% 
  mutate(perc_UDA_delivered = 100* (UDA_delivery /(contracted_UDAs*(`no workdays`/`total workdays`)))) %>%
  mutate(perc_UDA_delivered = round(perc_UDA_delivered, digits = 0))%>%
  rbind(data1)

# filter for STP or region in need to get data ready for plotting
  data <- data2 %>%
    filter(region_name == region_STP_name)
  subtitle <- region_STP_name

##################  plotting -- ICB level changing pattern, one line chart for one region
#set legend order (ICB order) alphabetically
  # Check the unique values of ICB
  unique_commissioners <- unique(data$commissioner_name)
  # Remove 'National' from the unique values
  other_commissioners <- unique_commissioners[unique_commissioners != "National"]
  # Sort the remaining commissioners alphabetically
  sorted_commissioners <- sort(other_commissioners)
  # Combine 'National' with the sorted list
  desired_order <- c("National", sorted_commissioners)
  # Convert commissioner_name to a factor with the desired order
  data$commissioner_name <- factor(data$commissioner_name, levels = desired_order)
  
#set color
  STPNationalColour<- setNames(c("#000000", "#E69F00", "#009E73", "#F0E442", 
                                 "#0072B2", "#D55E00", "#56B4E9", "#CC79A7", 
                                   "#999999","#FFCCFF", "#00CC00", "#FF00FF"), c("National", other_commissioners))
  STPNationalColour<-STPNationalColour[c("National",other_commissioners)]

#line plot
p1 <- ggplot(data = data,aes_string(x = "month", y = "perc_UDA_delivered", colour = "commissioner_name")) +
  geom_line(size = 1.5) +
  # Removed y-axis title and x-axis title
  xlab(NULL) +
  ylab(NULL) +
  #geom_text(data = data,
  #          aes(x = month, y = perc_UDA_delivered + 4, label = paste0(round(perc_UDA_delivered), "%")),
  #          size = 3) +
  scale_x_date(breaks = "1 month", date_labels ="%b-%Y") +
  scale_y_continuous(limits = c(min(c(data$perc_UDA_delivered, 95), na.rm = T)-1, max(c(data$perc_UDA_delivered, 95), na.rm = T) + 1),
                     breaks = seq(min(c(data$perc_UDA_delivered, 95), na.rm = T) - 1, max(c(data$perc_UDA_delivered, 95), na.rm = T) + 1, 5),
                     labels = scales::percent_format(scale = 1))  +
  scale_color_manual(values = STPNationalColour) +theme_bw() +
  # Bold and larger axes labels
  theme(axis.title.x = element_text(face = "bold", size = 13, color = "#231F20", angle = 90, vjust = -0.0001), 
        axis.title.y = element_text(face = "bold", size = 13, color = "#231F20"),
        # Larger and bold legend text
        legend.text = element_text(face = "bold", size = 13, color = "#231F20"),
        # Remove grid lines
        panel.grid = element_blank(),
        # Customize y-axis text
        axis.text.y = element_text(face = "bold", size = 13, color = "#231F20"),
        # Customize x-axis text
        axis.text.x = element_text(face = "bold",size = 13, angle = 45, hjust = 1, vjust = 1, color = "#231F20"),
        # Larger and bolder title
        #plot.title = element_text(face = "bold", size = 18, color = "#231F20"),
        # Legend settings for stretching
        legend.position = "bottom",
        legend.direction = "horizontal",      # Stretch legend items horizontally
        legend.box = "horizontal",            # Stretch the legend box horizontally
        legend.key.width = unit(3.5, "cm")) +      # Adjust width of legend items (optional)
  theme(legend.position = "bottom", legend.title = element_blank())# +
  #labs(title = paste("Monthly percentage of usual annual contracted UDAs standardised by working days -", region_STP_name))

p1


#bar plot
p2 <- ggplot(data = data, aes_string(x = "commissioner_name", y = "perc_UDA_delivered", fill = "commissioner_name")) + 
  geom_bar(stat = "identity") +  # Use geom_bar with stat="identity"
  # Removed y-axis title and x-axis title
  ylab(NULL) +
  xlab(NULL) +
  scale_y_continuous(limits = c(0, 
                                max(data$perc_UDA_delivered, na.rm = TRUE) + 10)) +  # Adjust limits here
  scale_fill_manual(values = STPNationalColour) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  # Bold and larger axes labels
  theme(axis.title.x = element_text(face = "bold", size = 13, color = "#231F20", angle = 90, vjust = -0.0001), 
        axis.title.y = element_text(face = "bold", size = 13, color = "#231F20"),
        # Remove grid lines
        panel.grid = element_blank(),
        # Customize y-axis text
        axis.text.y = element_text(face = "bold", size = 13, color = "#231F20"),
        # Customize x-axis text
        axis.text.x = element_text(face = "bold",size = 13, angle = 45, hjust = 1, vjust = 1, color = "#231F20")
        # Larger and bolder title
        #plot.title = element_text(face = "bold", size = 18, color = "#231F20"),
        )# +labs(title = paste("Percentage of contracted UDAs delivered for the latest month -", region_STP_name))

# Print the bar chart
print(p2)
