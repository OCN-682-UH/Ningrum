### This is my first script. I am learning how to import data
### Created by : Retno Kusuma Ningrum
### Created on: 2024-09-04
###############################################################

### Load libriaries #########
library(tidyverse)
library(here)

### Read in my data  #####
weight.data <- read_csv(here("week2", "data", "weightdata.csv"))

### Data Analysis ####
head(weight.data) #looks at the top 6 line of the dataframe
tail(weight.data) #looks at the bottom 6 lines
view(weight.data) #to shows the entire dataset 

###this is for assignment week2