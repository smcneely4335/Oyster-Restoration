##################################
### Photo Randomizer Functions ###
##################################

################################################################################

########################
### File Description ###
########################

# Filename: PhotoRandomizer-Functions.R
# Author: Sam McNeely
# Contributors: 
# Date Created: 09/13/2023
# Date Modified: 09/17/2023

# Purpose: 
# The purpose of this script is to organize the GoPro sampling metadata,
# randomize the image site numbers to minimize scoring bias, and combine it all 
# into an Excel workbook to be scored.

# Difference from PhotoRandomizer.R:
# This script is different from PhotoRandomizer.R because here we employ
# functions to do the work for us. This makes the process simpler to read and
# input necessary data because the data is input in one location of the script
# and the functions take care of the rest for us. It minimizes the editing and
# run time to get the same output.

# Place in the data collection process:
# This R script is meant to be run after images have been extracted from the 
# GoPro videos and before the images are qualitatively scored.

################################################################################

########################
### Preparatory Work ###
########################
# 
# Notes:
#
# Edit certain parts of this section if necessary:
# - Read in datasheets
#
# Do NOT edit certain parts of this section if necessary:
# - Check working directory
# - Load packages
#
# Run through each line of code in order (Ctrl + Enter) AFTER the datasheets 
# have been properly assigned to the correct file names.

## Check working directory
# it should be set to ./GitHub/Oysters/RAP
getwd() # if not, then use setwd() to change the current working directory

## Load packages
# make sure pacman package is installed
if(!require(pacman)) {
  install.packages('pacman')
}

# p_load() from pacman package installs and loads packages called as arguments;
# tidyverse for data wrangling
# openxlsx to read in Excel data files
# writexl to write the Excel workbook file
pacman::p_load(tidyverse, openxlsx, writexl)

## Read in datasheets
# Metadata
mdat <- read.xlsx(xlsxFile = './Scoring Metadata/RAP_Metadata.xlsx',
                  sheet = 1)

# Site coordinates
coord <- read.xlsx(xlsxFile = './Data/Site Coordinates/.xlsx',
                   sheet = 1)

################################################################################

##################################
### Input Variable Definitions ###
##################################
#
# Notes:
#
# EDIT THIS SECTION by following the input process defined to the right of each
# variable.
# 
# Simply run through each line of code to create the variables (Ctrl + Enter)
# AFTER they are defined.

## Defining the variables here will make the editing process easier and faster
# define variable parameters
Site_a <- c() # input only site #'s from camera A for images captured within the ()
Site_b <- c() # input only site #'s from camera b for images captured within the ()
missing_images_a <- c() # input only site #'s from camera A from images that were NOT captured within the ()
missing_images_b <- c() # input only site #'s from camera B from images that were NOT captured within the ()
Date <- '' # input the sampling date in the format MM/DD/YYYY within the ''
Location <- '' # input the sampling location within the ''
filename <- '.xlsx' # Siteabbrev_YearJuliandate_NoMaxScores.xlsx # Ex: RB_2023229_NoMaxScores.xlsx

################################################################################

############################
### Function Definitions ###
############################
#
# Notes:
#
# Do NOT modify the function definitions. Simply run through each function to
# create it (Ctrl + Enter).

## Write functions for the dataframes and, eventually, separate sheets in the 
## Excel file: side_a(), side_b(), and combine()

# function for side_a images
side_a <- function(Site_a = Site_a, m_images_a = missing_images_a,Date = Date, 
                   Location = Location, Metadata = mdat, Coord_data = coord) {
  
  # define set parameters: Date_a, Location_a, Random_Assignment_a, Habscore_a, and Notes_a
  Date_a <- rep(Date, length(Site_a))
  Location_a <- rep(Location, length(Site_a))
  Random_Assignment_a <- sample(1:length(Site_a), length(Site_a))
  Habscore_a <- rep(NA, length(Site_a))
  Notes_a <- rep("", length(Site_a))
  
  
  # create the dataframe for side A
  df_a <- data.frame(Date_a, Location_a, Site_a, Random_Assignment_a, 
                     Habscore_a, Notes_a)
  
  # append the dataframe with rows for missing images
  if(length(missing_images_a) >= 1) {
    for(i in 1:length(missing_images_a)) {
      df_a <- df_a %>%
        add_row(Date_a = Date, Location_a = Location, Site_a = missing_images_a[i],
                Habscore_a = NA, Notes_a = "Missing image")
    }
  }
  
  ## add coordinates to the dataframe if they're available
  # define empty coordinate vectors
  Coord_x_a <- c()
  Coord_y_a <- c()
  
  # iterate through coordinates data to fill empty coordinate vectors
  # with coordinates that align with each site that was sampled
  for(i in 1:nrow(df_a)){
    Coord_x_a[i] <- Coord_data$POINT_X[Coord_data$`OBJECTID.*` == df_a$Site_a[i]]
    Coord_y_a[i] <- Coord_data$POINT_Y[Coord_data$`OBJECTID.*` == df_a$Site_a[i]]
  }
}

# function for side_b images
side_b <- function(Site_b = Site_b, m_images_b = missing_images_b, Date = Date, Location = Location, 
                   Metadata = mdat, Coord_data = coord) {
  
  # define set parameters: Date_b, Location_b, Random_Assignment_b, Habscore_b, and Notes_b
  Date_b <- rep(Date, length(Site_b))
  Location_b <- rep(Location, length(Site_b))
  Random_Assignment_b <- sample(1:length(Site_b), length(Site_b))
  Habscore_b <- rep(NA, length(Site_b))
  Notes_b <- rep("", length(Site_b))
  
  
  # create the dataframe for side B
  df_b <- data.frame(Date_b, Location_b, Site_b, Random_Assignment_b, 
                     Habscore_b, Notes_b)
  
  # append the dataframe with rows for missing images
  if(length(missing_images_b) >= 1) {
    for(i in 1:length(missing_images_b)) {
      df_b <- df_b %>%
        add_row(Date_b = Date, Location_b = Location, Site_b = missing_images_b[i],
                Habscore_b = NA, Notes_b = "Missing image")
    }
  }
  
  ## add coordinates to the dataframe if they're available
  # define empty coordinate vectors
  Coord_x_b <- c()
  Coord_y_b <- c()
  
  # iterate through coordinates data to fill empty coordinate vectors
  # with coordinates that align with each site that was sampled
  for(i in 1:nrow(df_b)){
    Coord_x_b[i] <- Coord_data$POINT_X[Coord_data$`OBJECTID.*` == df_b$Site_b[i]]
    Coord_y_b[i] <- Coord_data$POINT_Y[Coord_data$`OBJECTID.*` == df_b$Site_b[i]]
  }
}

# create a function that runs side_a() and side_b(), and ends with an excel file
combine <- function(filename = filename) {
  
  # run the functions: side_a() and side_b()
  side_a()
  side_b()
  
  # Combine dataframes into a list
  sheets <- list("Metadata" = mdat, "Side A" = df_a, "Side B" = df_b)
  
  ## Create path for files to be stored
  # name the paths without changing the current working directory
  wd <- "./Data"
  
  # setting up the sub-directory
  sub_dir <- "NoMaxScores"
  
  # check if directory exists
  if(file.exists(wd)) {
    
    # check if sub-directory exists
    if(file.exists(file.path(wd, sub_dir))) {
    } else {
      
      # if it does NOT exist, create the sub directory
      dir.create(file.path(wd, sub_dir))
    }
  } else {
    
    # the path wd does NOT exist
    # create the new path with both wd and sub_dir simultaneously
    dir.create(file.path(wd, sub_dir))
  }
  
  ## Now that the path is created, we have a place to create the file
  # Export sheets list to an excel file
  write_xlsx(sheets, file.path(wd, sub_dir, filename))
}

################################################################################

#########################
### Run the Functions ###
#########################
#
# Notes:
#
# Do NOT edit this section. Simply run through each function to perform the 
# desired actions (Ctrl + Enter).

# side_a()
side_a()

# side_b()
side_b()

# combine()
combine()

################################################################################