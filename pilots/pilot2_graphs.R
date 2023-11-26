## September 27 2023 - Pilot 2
# Reagents: Ethyl Acetate (EA) and Dichloromethane (DCM)

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(cowplot)

# Initial read of csv data from Multiquant
pilot2_data <- read.csv("data/sep27data_DCMEA3buffer.csv")
pilot2_data

# Removing NA values
pilot2_data_cleaned <- pilot2_data %>% drop_na(area)

pilot2_bar <- pilot2_data %>%
  ggplot(aes(x = buffer),
         color = reagent) +
  geom_bar(stat = identity) 

pilot2_bar  
