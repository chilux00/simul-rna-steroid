## August 10 2023 - Pilot 1
# Reagents: Ethyl Acetate (EA) and Dichloromethane (DCM)

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(forcats)
library(dplyr)

# Initial read of csv data from Multiquant
pilot1_data <- read.csv("data/aug10data_DCMEA.csv")
pilot1_data

# Cleaning data types
pilot1_data_cleaned <- pilot1_data %>%
  mutate(area_p4 = as.numeric(area_p4)) %>% 
  mutate(area_f = as.numeric(area_f)) %>% 
  mutate(area_b = as.numeric(area_b)) %>% 
  mutate(reagent = as.factor(reagent)) %>%
  mutate(area_p4 = coalesce(area_p4, 0)) %>%  # Removal of NA values - keep?
  mutate(area_f = coalesce(area_f, 0)) %>% 
  mutate(area_b = coalesce(area_b, 0)) %>% 
  as.data.frame()

pilot1_data_cleaned

# Plotting bar graph of pilot 1 area data per treatment
# P4 Progesterone graph
pilot1_bar_p4 <- pilot1_data_cleaned %>%
  ggplot(aes(x = fct_inorder(reagent), 
             y = (area_p4),
             fill = as.factor(reagent)
  )
  ) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.7,
           na.rm = TRUE
  ) +
  
  labs(title = "Pilot 1: Dichloromethane and Ethyl Acetate extraction of
       Progesterone (P4) Steroid Hormone from Qiagen RNA Kit Flowthrough",
       x = "Buffer Kit",
       y = "Area",
       fill = "Reagent"
  ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10)
  )  

pilot1_bar_p4

# B Corticosterone graph
pilot1_bar_b <- pilot1_data_cleaned %>%
  ggplot(aes(x = fct_inorder(reagent), 
             y = (area_b),
             fill = as.factor(reagent)
             )
  ) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.7,
           na.rm = TRUE
  ) +
  
  labs(title = "Pilot 1: Dichloromethane and Ethyl Acetate extraction of
       Corticosterone (B) Steroid Hormone from Qiagen RNA Kit Flowthrough",
       x = "Buffer Kit",
       y = "Area",
       fill = "Reagent"
  ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10)
  )  

pilot1_bar_b

# F Cortisol graph
pilot1_bar_f <- pilot1_data_cleaned %>%
  ggplot(aes(x = fct_inorder(reagent), 
             y = (area_f),
             fill = as.factor(reagent)
  )
  ) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.7,
           na.rm = TRUE
  ) +
  
  labs(title = "Pilot 1: Dichloromethane and Ethyl Acetate extraction of
       Cortisol (F) Steroid Hormone from Qiagen RNA Kit Flowthrough",
       x = "Buffer Kit",
       y = "Area",
       fill = "Reagent"
  ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10)
  )  

pilot1_bar_f

