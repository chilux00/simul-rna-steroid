## October 22 2023 - Pilot 1
# Reagents: Ethyl Acetate (EA) and Dichloromethane (DCM)

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(forcats)
library(dplyr)

# Initial read of csv data from Multiquant
pilot3_data <- read.csv("data/oct22data_DCMdryvols.csv")
pilot3_data

# Cleaning data types
pilot3_data_cleaned <- pilot3_data %>%
  mutate(area_p4 = as.numeric(area_p4)) %>% 
  mutate(area_f = as.numeric(area_f)) %>% 
  mutate(area_b = as.numeric(area_b)) %>% 
  mutate(reagent = as.factor(reagent)) %>%
  mutate(volume = as.factor(volume)) %>%
  mutate(time = as.factor(time)) %>%
  mutate(dilution = as.factor(dilution)) %>%
  mutate(area_p4 = coalesce(area_p4, 0)) %>%  # Removal of NA values - keep?
  mutate(area_f = coalesce(area_f, 0)) %>% 
  mutate(area_b = coalesce(area_b, 0)) %>% 
  as.data.frame()

pilot3_data_cleaned

# Plotting bar graph of pilot 1 area data per treatment
# P4 Progesterone graph
pilot3_bar_p4 <- pilot3_data_cleaned %>%
  ggplot(aes(x = fct_inorder(reagent), 
             y = (area_p4),
             fill = as.factor(time)
             )
  ) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.7,
           na.rm = TRUE
  ) +
  
  labs(title = "Pilot 3: Dichloromethane extraction of
       Progesterone (P4) Steroid Hormone from Qiagen RNA Kit Flowthrough with 
       differing reagent volumes, buffer dilution and dry times",
       x = "Buffer Kit",
       y = "Area",
       fill = "Time (hours)"
  ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1)
  )  

pilot3_bar_p4
