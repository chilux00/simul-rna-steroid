## September 27 2023 - Pilot 2
# Reagents: Ethyl Acetate (EA) and Dichloromethane (DCM)

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(forcats)
library(dplyr)

# Initial read of csv data from Multiquant
pilot2_data <- read.csv("data/sep27data_DCMEA3buffer.csv")
pilot2_data

# Cleaning data types
pilot2_data_cleaned <- pilot2_data %>%
  mutate(area_p4 = as.numeric(area_p4)) %>% 
  mutate(area_f = as.numeric(area_f)) %>% 
  mutate(area_b = as.numeric(area_b)) %>% 
  mutate(reagent = as.factor(reagent)) %>%
  mutate(area_p4 = coalesce(area_p4, 0)) %>%  # Removal of NA values - keep?
  mutate(area_f = coalesce(area_f, 0)) %>% 
  mutate(area_b = coalesce(area_b, 0)) %>% 
  as.data.frame()

pilot2_data_cleaned

# Plotting bar graph of pilot 2 area data per treatment
# P4 Progesterone graph
pilot2_bar <- pilot2_data_cleaned %>%
  ggplot(aes(x = fct_inorder(buffer), 
             y = (area_p4),
             fill = as.factor(reagent)
             )
         ) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.7,
           na.rm = TRUE
           ) +

  labs(title = "Pilot 2: Dichloromethane and Ethyl Acetate extraction of
       Progesterone (P4) Steroid Hormone from Qiagen, Monarch and Zymo RNA 
       Lysis Buffers",
       x = "Buffer Kit",
       y = "Area",
       fill = "Reagent"
       ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(angle = 80, hjust = 1)
        )  

pilot2_bar  

### -------------------------------------------------------------------------
# Initial read of averaged area data values, calculated from initial MQ data
pilot2_avgdata <- read.csv("data/sep27data_DCMEA3buffer_averaged.csv")

# Cleaning data types
pilot2_avgdata_cleaned <- pilot2_avgdata %>%
  mutate(avg_p4 = as.numeric(avg_p4)) %>% 
  mutate(avg_f = as.numeric(avg_f)) %>% 
  mutate(avg_b = as.numeric(avg_b)) %>% 
  mutate(reagent = as.factor(reagent)) %>%
  mutate(avg_p4 = coalesce(avg_p4, 0)) %>%  # Removal of NA values - keep?
  mutate(avg_f = coalesce(avg_f, 0)) %>% 
  mutate(avg_b = coalesce(avg_b, 0)) %>% 
  as.data.frame()

pilot2_avgdata_cleaned

# Create a new grouping variable
pilot2_avgdata_control <- pilot2_avgdata_cleaned %>%
  filter(buffer == "CONTROL")

pilot2_avgdata_control

# Plotting line graph of pilot 2 data average area per treatment
# P4 Progesterone Graph
pilot2_line <- ggplot() +
  geom_line(data = pilot2_avgdata_cleaned,
            aes(x = fct_inorder(buffer), 
                y = avg_p4, 
                group = reagent, 
                color = reagent)
            ) +
  geom_point(data = pilot2_avgdata_cleaned,
             aes(x = fct_inorder(buffer), 
                 y = avg_p4, 
                 group = reagent, 
                 color = reagent)
             ) +
  geom_bar(data = pilot2_avgdata_control,
           aes(x = buffer,
               y = avg_p4),
           stat = "identity",
           position = "dodge",
           fill = "light blue",
           width = 0.7,
           na.rm = TRUE
           ) +
  labs(title = "Pilot 2: Dichloromethane and Ethyl Acetate extraction of 
       Progesterone (P4) Steroid Hormone from Qiagen, Monarch and Zymo
       RNA Lysis Buffers, averaged across treatments",
       x = "Buffer Kit",
       y = "Average Area") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10))

pilot2_line
