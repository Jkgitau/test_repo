#Epidemiological data analysis with R

## Install needed packages
library(tidyverse)
library(ggplot2)
library(sf)

## Task: identify errors and fix them in the provided dataset

rm(list=ls()) #clean the environment

#' setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #sets the folder where
#' #'this file is saved as current directory;
#' #'all paths are now expressed relative to the current directory

# Load the routine dataset
routine_data <- read.csv('Data/routine_data.csv',sep=";") #load routine dataset

# First understand the structure of the dataset
str(routine_data)


## Need to interogate each column
unique(routine_data$test_u5)

# For the year, replace 3018 and 18 with 2018 as thats the year of interest
routine_data$year[routine_data$year == '18' | routine_data$year == '3018'] <- '2018'

# Rename routine_data to New_dat to avoid manipulating the original df
New_dat <- routine_data

#-----------------------------------------------------------------------------------

# check the test_u5 column

unique(New_dat$test_u5)

# Check how many rows in the new dataframe
dim(New_dat)

# Check if there are missing values
which(is.na(New_dat$test_u5))

# Delete all rows with NAs
New_dat1 <- na.omit(New_dat)

# How many rows are removed? = 22
dim(New_dat1)


unique(New_dat1$conf_u5)

# Replace o with 0
New_dat$conf_u5[New_dat$conf_u5 == 'O'] <- 0

#Remove all rows with -999 for the variable test_u5
## Need to interogate each column

kkk <- routine_data
unique(kkk$test_u5)
kkk$conf_u5[New_dat$conf_u5 == 'O'] <- 0

str(New_dat1)
#example of tidyverse 
clean_routine_data <- routine_data %>% #pipe operator
  mutate(test_total = test_u5+test_ov5)

write_csv(clean_routine_data, './clean_routine_data.csv') #save cleaned