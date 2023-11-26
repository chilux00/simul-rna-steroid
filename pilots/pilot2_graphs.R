## September 27 2023 - Pilot 2
# Reagents: Ethyl Acetate (EA) and Dichloromethane (DCM)

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(cowplot)

# Initial read of csv data from Multiquant
pilot2_data <- read.csv("data/sep27data_DCMEA3buffer.csv", na.rm = TRUE)
pilot2_data

# Removing NA values
pilot2_data_cleaned <- pilot2_data %>%
  drop_na(area) %>%
  as.data.frame(complete.cases(pilot2_data$area))

pilot2_data_cleaned

pilot2_bar <- pilot2_data_cleaned %>%
  ggplot(aes(x = buffer, 
             y = area,
             fill = reagent)) +
  geom_bar(stat = "identity",
           position = "dodge",
           na.rm = TRUE) +
  
  labs(title = "Pilot 2: Dichloromethane and Ethyl Acetate extraction of
       steroids from Qiagen, Monarch and Zymo RNA Lysis Buffers",
       x = "Buffer Kit",
       y = "Area"
       ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))  

pilot2_bar  
