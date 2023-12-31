## September 27 2023 - Pilot 2
# Reagents: Ethyl Acetate (EA) and Dichloromethane (DCM)

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(forcats)
library(dplyr)

# Initial read of data from MultiQuant
pilot2_indiv <- read.csv("data/sep27data_DCMEA3buffer.csv")
pilot2_data <- read.csv("data/sep27data_DCMEA3buffer_averaged.csv")


# Cleaning data types
pilot2_data_cleaned <- pilot2_data %>%
  mutate(avg_p4 = as.numeric(avg_p4)) %>% 
  mutate(avg_f = as.numeric(avg_f)) %>% 
  mutate(avg_b = as.numeric(avg_b)) %>% 
  mutate(reagent = as.factor(reagent)) %>%
  mutate(avg_p4 = coalesce(avg_p4, 0)) %>%  # Removal of NA values - keep?
  mutate(avg_f = coalesce(avg_f, 0)) %>% 
  mutate(avg_b = coalesce(avg_b, 0)) %>% 
  as.data.frame()

pilot2_data_cleaned

pilot2indiv_cleaned <- pilot2_indiv %>%
  mutate(area_p4 = as.numeric(area_p4)) %>% 
  mutate(area_f = as.numeric(area_f)) %>% 
  mutate(area_b = as.numeric(area_b)) %>% 
  mutate(reagent = as.factor(reagent)) %>%
  mutate(area_p4 = coalesce(area_p4, 0)) %>%  # Removal of NA values - keep?
  mutate(area_f = coalesce(area_f, 0)) %>% 
  mutate(area_b = coalesce(area_b, 0)) %>% 
  as.data.frame()

pilot2indiv_cleaned

# Plotting bar graph of pilot 2 area data per treatment
# P4 Progesterone graph
pilot2_bar_p4 <- 
  ggplot() +
  geom_bar(data = pilot2_data_cleaned,
           aes(x = fct_inorder(buffer),
               y = avg_p4,
               fill = reagent),
           stat = "identity",
           position = "dodge",
           width = 0.7,
           na.rm = TRUE,
           ) +
  geom_jitter(data = pilot2indiv_cleaned,
             aes(x = fct_inorder(buffer),
                 y = area_p4,
                 fill = reagent),
             stat = "identity",
             position = position_jitterdodge()) +
  
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

pilot2_bar_p4

# B Corticosterone graph
pilot2_bar_b <- 
  ggplot() +
  geom_bar(data = pilot2_data_cleaned,
           aes(x = fct_inorder(buffer),
               y = avg_b,
               fill = reagent),
           stat = "identity",
           position = "dodge",
           width = 0.7,
           na.rm = TRUE,
  ) +
  geom_jitter(data = pilot2indiv_cleaned,
              aes(x = buffer,
                  y = area_b,
                  fill = reagent),
              stat = "identity",
              position = position_jitterdodge()) +
  
  labs(title = "Pilot 2: Dichloromethane and Ethyl Acetate extraction of
       Corticosterone (B) Steroid Hormone from Qiagen, Monarch and Zymo RNA 
       Lysis Buffers",
       x = "Buffer Kit",
       y = "Area",
       fill = "Reagent"
  ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(angle = 80, hjust = 1)
  )  

pilot2_bar_b

# F Cortisol graph
pilot2_bar_f <- 
  ggplot() +
  geom_bar(data = pilot2_data_cleaned,
           aes(x = fct_inorder(buffer),
               y = avg_f,
               fill = reagent),
           stat = "identity",
           position = "dodge",
           width = 0.7,
           na.rm = TRUE,
  ) +
  geom_jitter(data = pilot2indiv_cleaned,
              aes(x = buffer,
                  y = area_f,
                  fill = reagent),
              stat = "identity",
              position = position_jitterdodge()) +
  
  labs(title = "Pilot 2: Dichloromethane and Ethyl Acetate extraction of
       Cortisol (F) Steroid Hormone from Qiagen, Monarch and Zymo RNA 
       Lysis Buffers",
       x = "Buffer Kit",
       y = "Area",
       fill = "Reagent"
  ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(angle = 80, hjust = 1)
  )  

pilot2_bar_f

### Creating standard error of the mean column
std.error <- function(x) sd(x)/sqrt(length(x)) # Define function 

pilot2_avgdata_means <- pilot2_avgdata_cleaned %>%
  mutate(sem = )


