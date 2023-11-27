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
