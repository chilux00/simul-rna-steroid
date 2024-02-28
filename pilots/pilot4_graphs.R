## February 4, 2024 - Pilot 4
# Reagents: Ethyl Acetate (EA) and Dichloromethane (DCM) - higher volumes

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(forcats)
library(dplyr)
set.seed(1234)

# Initial read of csv data from Multiquant
pilot4_data <- read.csv("data/feb4data_5mlDCMEA.csv")
pilot4_data

# Cleaning data types
pilot4_data_cleaned <- pilot4_data %>%
  mutate(area_p4 = as.numeric(area_p4)) %>% 
  mutate(area_f = as.numeric(area_f)) %>% 
  mutate(area_b = as.numeric(area_b)) %>% 
  mutate(reagent = as.factor(reagent)) %>%
  mutate(buffer = as.factor(buffer)) %>%
  mutate(area_p4 = coalesce(area_p4, 0)) %>%  # Removal of NA values - keep?
  mutate(area_f = coalesce(area_f, 0)) %>% 
  mutate(area_b = coalesce(area_b, 0)) %>% 
  as.data.frame()

pilot4_data_cleaned

# Plotting bar graph of pilot 4 area data per treatment
# P4 Progesterone graph
pilot4_bar_p4 <- 
  ggplot() + 
  geom_bar(data = pilot4_data_cleaned,
           aes(x = fct_inorder(reagent), 
               y = (area_p4),
               fill = as.factor(reagent)),
           stat = "identity",
           position = "dodge",
           width = 0.7,
           na.rm = TRUE
  ) +
  geom_jitter(data = pilot4_data_cleaned,
              aes(x = reagent,
                  y = area_p4),
              stat = "identity",
              position = position_jitter(height = 0,
                                         width = 0)) +
  
  labs(title = "Pilot 4: 5mL Dichloromethane and Ethyl Acetate extraction of
       Progesterone (P4) Steroid Hormone from Qiagen RNA Kit Flowthrough",
       x = "Buffer Kit",
       y = "Area",
       fill = "Reagent"
  ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10)
  )  

pilot4_bar_p4

# B Corticosterone graph
pilot4_bar_b <- 
  ggplot() + 
  geom_bar(data = pilot4_data_cleaned,
           aes(x = fct_inorder(reagent), 
               y = (area_b),
               fill = as.factor(reagent)),
           stat = "identity",
           position = "dodge",
           width = 0.7,
           na.rm = TRUE
  ) +
  geom_jitter(data = pilot4_data_cleaned,
              aes(x = reagent,
                  y = area_b),
              stat = "identity",
              position = position_jitter(height = 0,
                                         width = 0)) +
  
  labs(title = "Pilot 4: 5mL Dichloromethane and Ethyl Acetate extraction of
       Corticosterone (B) Steroid Hormone from Qiagen RNA Kit Flowthrough",
       x = "Buffer Kit",
       y = "Area",
       fill = "Reagent"
  ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10)
  )  

pilot4_bar_b

# F Cortisol graph
pilot4_bar_f <- 
  ggplot() + 
  geom_bar(data = pilot4_data_cleaned,
           aes(x = fct_inorder(reagent), 
               y = (area_f),
               fill = as.factor(reagent)),
           stat = "identity",
           position = "dodge",
           width = 0.7,
           na.rm = TRUE
  ) +
  geom_jitter(data = pilot4_data_cleaned,
              aes(x = reagent,
                  y = area_f),
              stat = "identity",
              position = position_jitter(height = 0,
                                         width = 0)) +
  
  labs(title = "Pilot 4: 5mL Dichloromethane and Ethyl Acetate extraction of
       Cortisol (F) Steroid Hormone from Qiagen RNA Kit Flowthrough",
       x = "Buffer Kit",
       y = "Area",
       fill = "Reagent"
  ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10)
  )  

pilot4_bar_f

