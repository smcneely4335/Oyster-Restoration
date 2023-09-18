#################################
### Zooniverse Data Wrangling ###
#################################

################################################################################

########################
### File Description ###
########################

# Filename: Zoo-wrangling.R
# Author: Sam McNeely
# Contributors: 
# Date Created: 07/06/2023
# Date Modified: 09/17/2023

# Purpose: 
# The purpose of this script is to extract all the pertinent citizen science
# data from the Zooniverse platform and store the clean data in a new file. 
# This script is currently operating under the assumption that the raw data has 
# been extracted from Zooniverse in the form of a CSV file.

# Place in the data collection process:
# This R script is meant to be run after the GoPro images have been scored by
# citizen scientists on the Zooniverse platform and the data file has been 
# downloaded and stored in the ./GitHub/Oysters/Zooniverse/Data directory. This
# script must be run before any analyses can be explored.

################################################################################

########################
### Preparatory Work ###
########################
# 
# Notes:
#
# Check the current working directory and ensure you are in the current location
# on your system.
#
# Load data wrangling packages.
#
# Run through each line of code in order (Ctrl + Enter).

## Check working directory
# it should be set to ./GitHub/Oysters/Zooniverse
getwd() # if not, then use setwd() to change the current working directory

## Load packages
# make sure pacman package is installed
if(!require(pacman)) {
  install.packages('pacman')
}

# p_load() from pacman package installs and loads packages called as arguments;
# tidyverse for data wrangling
pacman::p_load(tidyverse)

################################################################################

######################
### Data Wrangling ###
######################
# 
# Notes:
#
# Load the raw data and perform wrangling operations to achieve the desired
# output to be set in a clean CSV file.
#
# Run through each line of code in order (Ctrl + Enter).

# load the raw data
rawdata <- read.csv('.csv')
head(rawdata, n = 1)

# only keep columns with relevant data
colnames(rawdata)
data <- rawdata %>%
  select(user_name, annotations, subject_data) %>% 
  data.frame()
head(data, n = 2)

## Iterate through the raw data file and, using regex, extract the desired data
## for the variables that we are interested in.
# Create empty lists for the variables of interest to be appended appropriately.
Date <- list()
Location <- list()
Site <- list()
Score <- list()
Expert_Score <- list()

# iterate through data to extract pertinent information
for (i in 1:nrow(data)) {
  # extract the dates
  dates <- as.numeric(str_extract(data$subject_data[i], '(?<=Date":")[0-9]{8}'))
  # append the dates to the Date list
  Date <- append(Date, list(dates)) # dates must be converted into a list; if not, date will replace and not append
  
  # extract the locations
  loc <- str_extract(data$subject_data[i], '(?<=Location":").*(?=","Depth)')
  # append the locations to the Location list
  Location <- append(Location, list(loc)) # loc must be converted into a list; if not, loc will replace and not append
  
  # extract the sites
  text <- str_extract(data$subject_data[i], '(?<=Site":").*(?=","Location)')
  # append the site to the Site list
  Site <- append(Site, list(text)) # text must be converted into a list; if not, text will replace and not append
  
  # extract the amateur scores
  value_1 <- as.numeric(str_extract(data$annotations[i], '(?<=")[0-3]'))
  # append the amateur scores to the Score list
  Score <- append(Score, list(value_1)) # value_1 must be converted into a list; if not, value_1 will replace and not append

  # extract the expert scores
  value_2 <- as.numeric(str_extract(data$subject_data[i], '(?<=Expert_Score":")[0-3]'))
  # append the expert scores to the Expert_Score list
  Expert_Score <- append(Expert_Score, list(value_2)) # value_2 must be converted into a list; if not, value_2 will replace and not append
  
}

# remove 'annotations' and 'subject_data' columns
data <- data %>% 
  subset(select = -c(annotations, subject_data))

## begin adding lists to the dataframe
# add Date to data
data$Date <- as.numeric(Date)

# separate Date into Month, Day, Year
data <- data %>% 
  separate(col = Date, into = c('Sampling_Month', 'Sampling_Day', 'Sampling_Year'), sep = c(2, 4))

# reorder this to be Year, Month, Day
data <- data[, c(1, 4, 2, 3)]

# add the rest of the lists to the data frame
data$Location <- as.character(Location)
data$Site <- as.character(Site)
data$Score <- as.numeric(Score)
data$Expert_Score <- as.numeric(Expert_Score)

################################################################################

#######################
### Clean Data File ###
#######################
# 
# Notes:
#
# Take a look at the dataframe data.
#
# Write a new CSV file to store the clean data.
#
# Run through each line of code in order (Ctrl + Enter).

# View the first few rows of the dataframe data
head(data)

## Create path for the clean data file to be stored
# name the paths without changing the current working directory
wd <- "./Data"

# check if directory exists
if(!file.exists(wd)) { # if path wd does NOT exist, then it return TRUE
  dir.create(file.path(wd)) # since path wd does NOT exist, create it
} # no else statement necessary because if !file.exists(wd) is FALSE, then wd exists

# write the CSV file to store the clean data
write.csv(x = data, file = ".csv") # clean_zooniverse_DDMMYYYY.csv

################################################################################