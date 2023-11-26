## September 27 2023 - Pilot 2
# Reagents: Ethyl Acetate (EA) and Dichloromethane (DCM)

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(cowplot)

pilot2_data <- read.csv("data/sep27data_DCMEA3buffer.csv")
pilot2_data

pilot2_bar <- pilot2_data %>%
  ggplot(aes(x = buffer,),
         color = reagent) +
  geom_bar() 

pilot2_bar  
