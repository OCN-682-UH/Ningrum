### This is my first script. I am learning how to import data
### Created by : Retno Kusuma Ningrum
### Created on: 2024-09-04
###############################################################

### Load libriaries #########
library(tidyverse)
library(here)

### Read in my data  #####
weight.data <- read_csv(here("week2", "data", "weightdata.csv"))
