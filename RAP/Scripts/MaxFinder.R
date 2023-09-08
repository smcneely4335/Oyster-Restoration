### Comparison: run after GoPro images have been scored

##########################################################

### Author: Sam McNeely
### Collaborators: 
### Date created: 07/24/2023
### Date modified: 09/08/2023
### File name: MaxFinder.R

##########################################################
## set up for analysis
# load packages
library(writexl)
library(tidyverse)
library(openxlsx)

## ALWAYS check working directory
# it should be set to ./GitHub/Oysters/RAP
getwd() # if not, the use setwd() to change the current working directory

## read in datasheets
# metadata
mdat <- read.xlsx(xlsxFile = './Scoring Metadata/RAP_Metadata.xlsx',
                  sheet = 1)

# read excel file, side A; also make it a dataframe
datasheet_a <- read.xlsx(xlsxFile = "./Data/NoMaxScores/RB_2023229_NoMaxScores.xlsx", 
                         sheet = 2)
df_a <- data.frame(datasheet_a) %>%
  # to ensure the sites are in ascending order, use arrange()
  arrange(Site_a)

# read excel file, side B; also make it a dataframe
datasheet_b <- read.xlsx(xlsxFile = "./Data/NoMaxScores/RB_2023229_NoMaxScores.xlsx", 
                         sheet = 3)
df_b <- data.frame(datasheet_b) %>%
  # to ensure the sites are in ascending order, use arrange()
  arrange(Site_b)

# create variables for the comparison dataframe df_comp
Date <- df_a[, 'Date_a']
Location <- df_a[, 'Location_a']
Site <- df_a[, 'Site_a']
Coord_x <- df_a[, 'Coord_x_a']
Coord_y <- df_a[, 'Coord_y_a']
Habscore_a <- as.numeric(df_a[, 'Habscore_a'])
Notes_a <- df_a[, 'Notes_a']
Habscore_b <- as.numeric(df_b[, 'Habscore_b'])
Notes_b <- df_b[, 'Notes_b']

# create comparison sheet with df_comp
df_comp <- data.frame(Date, Location, Site, Coord_x, Coord_y,
                      Habscore_a, Notes_a, Habscore_b, Notes_b)

### Find max scores

# iterate through every row of df_comp
for(i in 1:nrow(df_comp)){
  
  # 9s are unscoreable images so take care of them first
  # Do so by setting the condition if there is a 9 and a non-9 score, the max score
  # is set to the non-9 score.
  if(df_comp$Habscore_a[i] == 9 & df_comp$Habscore_b[i] != 9){
    df_comp$Max_Habscore[i] <- df_comp$Habscore_b[i]
  } else if(df_comp$Habscore_a[i] != 9 & df_comp$Habscore_b[i] == 9){
    df_comp$Max_Habscore[i] <- df_comp$Habscore_a[i]
  } else {
    
    # Now that the 9s are taken care of, find the max score using pmax()
    df_comp$Max_Habscore[i] <- pmax(df_comp$Habscore_a[i], df_comp$Habscore_b[i])
  }
}

## Create warnings for abnormalities in the data
# check differences in habitat scoring
df_comp <- df_comp %>%
  mutate(Warning = case_when((Habscore_a != 9 & Habscore_b != 9) & (abs(Habscore_a - Habscore_b) >= 2) ~ 'Difference +2'))

# Create Check_Warnings columns to manually see if the warnings are justified
df_comp$Check_Warning <- NA

## create list of data frames
sheets <- list("Metadata" = mdat, "Side A" = df_a, "Side B" = df_b, "Comparison" = df_comp)

## Create path for files
# name the paths without changing the current working directory
wd <- "./Data"

# setting up the sub directory
sub_dir <- "MaxScores"

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

filename <- "RB_2023229_MaxScores.xlsx"

# Export list to excel (edit file name and location if necessary)
write_xlsx(sheets, file.path(wd, sub_dir, filename))
