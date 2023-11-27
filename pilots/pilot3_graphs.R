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

pilot1_data_cleaned