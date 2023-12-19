## October 22 2023 - Pilot 1
# Reagents: Ethyl Acetate (EA) and Dichloromethane (DCM)

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(forcats)
library(dplyr)

# Initial read of csv data from Multiquant
pilot3_indiv <- read.csv("data/oct22data_DCMdryvols.csv")
pilot3_data <- read.csv("data/oct22data_DCMdryvols_averaged.csv")

# Cleaning data types
pilot3_data_cleaned <- pilot3_data %>%
  mutate(avg_p4 = as.numeric(avg_p4)) %>% 
  mutate(avg_f = as.numeric(avg_f)) %>% 
  mutate(avg_b = as.numeric(avg_b)) %>% 
  mutate(reagent = as.factor(reagent)) %>%
  mutate(volume = as.factor(volume)) %>%
  mutate(time = as.factor(time)) %>%
  mutate(dilution = as.factor(dilution)) %>%
  mutate(avg_p4 = coalesce(avg_p4, 0)) %>%  # Removal of NA values - keep?
  mutate(avg_f = coalesce(avg_f, 0)) %>% 
  mutate(avg_b = coalesce(avg_b, 0)) %>% 
  mutate(volume = coalesce(volume, 0)) %>% 
  mutate(time = coalesce(time, 0)) %>% 
  mutate(dilution = coalesce(dilution, 0)) %>% 
  as.data.frame()

pilot3_data_cleaned

pilot3indiv_cleaned <- pilot3_indiv %>%
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

pilot3indiv_cleaned

# Plotting bar graph of pilot 1 area data per treatment
# P4 Progesterone graph - change fill as desired for dilution, time, volume
pilot3_bar_p4 <- 
  ggplot() +
  geom_bar(data = pilot3_data_cleaned,
           aes(x = fct_inorder(reagent),
               y = avg_p4,
               fill = dilution),
           stat = "identity",
           position = "dodge",
           width = 0.7,
           na.rm = TRUE,
  ) +
  geom_jitter(data = pilot3indiv_cleaned,
              aes(x = fct_inorder(reagent),
                  y = area_p4),
              stat = "identity",
              position = position_jitter(height = 0,
                                         width = 0)) +
  
  labs(title = "Pilot 3: Dichloromethane extraction of
       Progesterone (P4) Steroid Hormone from Qiagen RNA Kit Flowthrough with 
       differing reagent volumes, buffer dilution and dry times",
       x = "Buffer Kit",
       y = "Area",
       fill = "Dilution"
  ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1)
  )  

pilot3_bar_p4

# B Corticosterone Plot
pilot3_bar_b <- 
  ggplot() +
  geom_bar(data = pilot3_data_cleaned,
           aes(x = fct_inorder(reagent),
               y = avg_b,
               fill = dilution),
           stat = "identity",
           position = "dodge",
           width = 0.7,
           na.rm = TRUE,
  ) +
  geom_jitter(data = pilot3indiv_cleaned,
              aes(x = fct_inorder(reagent),
                  y = area_b),
              stat = "identity",
              position = position_jitter(height = 0,
                                         width = 0)) +
  
  labs(title = "Pilot 3: Dichloromethane extraction of
       Corticosterone (B) Steroid Hormone from Qiagen RNA Kit Flowthrough with 
       differing reagent volumes, buffer dilution and dry times",
       x = "Buffer Kit",
       y = "Area",
       fill = "Dilution"
  ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1)
  )  

pilot3_bar_b

# F Cortisol Plot
pilot3_bar_f <- 
  ggplot() +
  geom_bar(data = pilot3_data_cleaned,
           aes(x = fct_inorder(reagent),
               y = avg_f,
               fill = volume),
           stat = "identity",
           position = "dodge",
           width = 0.7,
           na.rm = TRUE,
  ) +
  geom_jitter(data = pilot3indiv_cleaned,
              aes(x = fct_inorder(reagent),
                  y = area_f,
                  fill = volume),
              stat = "identity",
              position = position_jitterdodge(),
              na.rm = TRUE) +
  
  labs(title = "Pilot 3: Dichloromethane extraction of
       Cortisol (F) Steroid Hormone from Qiagen RNA Kit Flowthrough with 
       differing reagent volumes, buffer dilution and dry times",
       x = "Buffer Kit",
       y = "Area",
       fill = "Volume (mL))"
  ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1)
  )  

pilot3_bar_f

