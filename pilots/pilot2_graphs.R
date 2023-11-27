## September 27 2023 - Pilot 2
# Reagents: Ethyl Acetate (EA) and Dichloromethane (DCM)

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(forcats)

# Initial read of csv data from Multiquant
pilot2_data <- read.csv("data/sep27data_DCMEA3buffer.csv")
pilot2_data

# Cleaning data types
pilot2_data_cleaned <- pilot2_data %>%
  mutate(area = as.numeric(area)) %>% 
  mutate(reagent = as.factor(reagent)) %>%
  mutate(avg_area = coalesce(avg_area, 0)) %>%  # Removal of NA values - keep?
  as.data.frame()

pilot2_data_cleaned

# Plotting bar graph of pilot 2 area data per treatment
pilot2_bar <- pilot2_data_cleaned %>%
  ggplot(aes(x = fct_inorder(buffer), 
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

### -------------------------------------------------------------------------
# Initial read of averaged area data values, calculated from initial MQ data
pilot2_avgdata <- read.csv("data/sep27data_DCMEA3buffer_averaged.csv")

# Cleaning data types
pilot2_avgdata_cleaned <- pilot2_avgdata %>%
  mutate(avg_area = as.numeric(avg_area)) %>% 
  mutate(reagent = as.factor(reagent)) %>%
  mutate(avg_area = coalesce(avg_area, 0)) %>%  # Removal of NA values - keep?
  as.data.frame() 

pilot2_avgdata_cleaned

# Plotting line graph of pilot 2 data average area per treatment
pilot2_line <- pilot2_avgdata_cleaned %>%
  ggplot(aes(x = fct_inorder(buffer), 
             y = avg_area, 
             group = reagent, 
             color = reagent)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Area by Buffer Treatment and Reagent",
       x = "Buffer Treatment",
       y = "Average Area") +
  theme_minimal()

pilot2_line
