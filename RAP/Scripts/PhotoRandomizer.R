########################
### Photo Randomizer ###
########################

################################################################################

########################
### File Description ###
########################

# Filename: PhotoRandomizer.R
# Author: Sam McNeely
# Contributors: 
# Date Created: 04/26/2023
# Date Modified: 09/17/2023

# Purpose: 
# The purpose of this script is to organize the GoPro sampling metadata,
# randomize the image site numbers to minimize scoring bias, and combine it all 
# into an Excel workbook to be scored.

# Difference from PhotoRandomizer-Functions.R:
# This script is different from PhotoRandomizer-Functions.R because this is the
# initial script to lay out the processes involved in randomizing the images for
# unbiased qualitative scoring and create the Excel workbook file to record the
# scores. PhotoRandomizer-Functions.R simplifies the editing process for future
# users by creating functions that perform limit the user's modification needs.

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

## ALWAYS check working directory
# it should be set to ./GitHub/Oysters/RAP
getwd() # if not, then use setwd() to change the current working directory

## load packages
# use pacman function p_load() to install/load in necessary packages
pacman::p_load(tidyverse, openxlsx, writexl)

## Read in datasheets
# Metadata
mdat <- read.xlsx(xlsxFile = './Scoring Metadata/RAP_Metadata.xlsx',
                  sheet = 1)

# Site coordinates
coord <- read.xlsx(xlsxFile = './Data/Site Coordinates/',
                   sheet = 1)

################################################################################

########################
### Build Datasheets ###
########################
# 
# Notes:
#
# Edit certain parts of this section that have "EDIT" commented out to the right
# of the line of code.
#
# Do NOT edit parts of this section that do NOT have "EDIT" commented out to the 
# right of the line of code.
#
# Run through each line of code in order (Ctrl + Enter) AFTER the datasheets 
# have been properly assigned to the correct file names.

## Side A
# Define variables
Site_a <- c() # EDIT
Date_a <- rep("", length(Site_a)) # EDIT
Location_a <- rep("", length(Site_a)) # EDIT
Random_Assignment_a <- sample(1:length(Site_a), length(Site_a))
Habscore_a <- rep(NA, length(Site_a))
Notes_a <- rep("", length(Site_a))

## extract site_a coordinates
# define Coord_x_a and Coord_y_a variables into empty vectors
Coord_x_a <- c()
Coord_y_a <- c()

# iterate through each row of coord
for(i in 1:length(Site_a)){
  Coord_x_a[i] <- coord$POINT_X[coord$`OBJECTID.*` == Site_a[i]]
  Coord_y_a[i] <- coord$POINT_Y[coord$`OBJECTID.*` == Site_a[i]]
}


# Create dataframe
df_a <- data.frame(Date_a, Location_a, Site_a, Coord_x_a, Coord_y_a, 
                   Random_Assignment_a, Habscore_a, Notes_a)
## ONLY IF THERE ARE MISSING IMAGES FROM CAMERA A:
# Add rows for missing camera images (Type "NA" in the Random_Assignment_a column for each row created in Excel)
# Uncomment the following 2 rows and edit the Site_a # only.
#df_a <- df_a %>%
#  add_row(Date_a = "05/11/2023", Location_a = "Eastern Bay", Site_a = 15, Habscore_a = NA, Notes_a = "Missing image")
# If there is more than one missing image, add %>% to the end of the first add_row()
# and copy and paste the first add_row() to the line beneath it and edit the 
# Site_a # only.

## Side B
# Define variables (edit variables Site_b, Date_b, & Location_b)
Site_b <- c() # EDIT
Date_b <- rep("", length(Site_b)) # EDIT
Location_b <- rep("", length(Site_b)) # EDIT
Random_Assignment_b <- sample(1:length(Site_b), length(Site_b))
Habscore_b <- rep(NA, length(Site_b))
Notes_b <- rep("", length(Site_b))

## extract site_b coordinates
# define Coord_x_b and Coord_y_b variables into empty vectors
Coord_x_b <- c()
Coord_y_b <- c()

# iterate through each row of coord
for(i in 1:length(Site_b)){
  Coord_x_b[i] <- coord$POINT_X[coord$`OBJECTID.*` == Site_b[i]]
  Coord_y_b[i] <- coord$POINT_Y[coord$`OBJECTID.*` == Site_b[i]]
}

# Create dataframe (do not edit)
df_b <- data.frame(Date_b, Location_b, Site_b, Coord_x_b, Coord_y_b, 
                   Random_Assignment_b, Habscore_b, Notes_b)

## ONLY IF THERE ARE MISSING IMAGES FROM CAMERA B:
# Add rows for missing camera images (Type "NA" in the Random_Assignment_b column for each row created in Excel)
# Uncomment the following 2 rows and edit the Site_b # only.
#df_b <- df_b %>%
#  add_row(Date_b = "05/11/2023", Location_b = "Eastern Bay", Site_b = 7, Habscore_b = NA, Notes_b = "Missing image")
# If there is more than one missing image, add %>% to the end of the first add_row()
# and copy and paste the first add_row() to the line beneath it and edit the 
# Site_b # only.

## Combine dataframes into a list
# create the list sheets that contains the sheets that will make up the Excel file
sheets <- list("Metadata" = mdat, "Side A" = df_a, "Side B" = df_b)

################################################################################

#############################
### Create the Excel File ###
#############################
# 
# Notes:
#
# Edit certain parts of this section that have "EDIT" commented out to the right
# of the line of code.
#
# Do NOT edit parts of this section that do NOT have "EDIT" commented out to the 
# right of the line of code.
#
# Run through each line of code in order (Ctrl + Enter) AFTER the datasheets 
# have been properly assigned to the correct file names.

## Create path for files
# name the paths without changing the current working directory
wd <- "./Data"

# setting up the sub directory
sub_dir <- "NoMaxScores"

# check if directory exists
if(file.exists(wd)) {
  
  # check if sub directory exists
  if(file.exists(file.path(wd, sub_dir))) {
    
    # if it does, print 'Directories already exist.' 
    print('Directories already exist.')
  } else {
    
    # if it does NOT, create the sub directory
    dir.create(file.path(wd, sub_dir))
    
    # double check that the sub directory now exists
    if(file.exists(file.path(wd, sub_dir))) {
      
      # if sub directory does exist, print 'Directories now exist.'
      print('Directories now exist.')
    } else {
      
      # if sub directory still does NOT exist, an error must have occurred.
      # print 'Error: check code.'
      print('Error: check code.')
    }
  }
} else {
  # the path wd does NOT exist
  # create the new path with both wd and sub_dir simultaneously
  dir.create(file.path(wd, sub_dir))
  
  # double check that the path now exists.
  if(file.exists(file.path(wd, sub_dir))) {
    
    # if path does exist, print 'Directories now exist.'
    print('Directories now exist.')
  } else {
    
    # if path still does NOT exist, an error must have occurred.
    # print 'Error: check code.'
    print('Error: check code.')
  }
}

# create the file name and location
filename <- '.xlsx'  # EDIT: Siteabbrev_YearJuliandate_NoMaxScores.xlsx # Ex: RB_2023229_NoMaxScores.xlsx

# Export list to excel 
write_xlsx(sheets, file.path(wd, sub_dir, filename))

################################################################################
