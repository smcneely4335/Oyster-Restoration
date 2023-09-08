### Excel Datasheet and Randomizer
# load packages
library(writexl)
library(tidyverse)

## ALWAYS check working directory
# it should be set to ./GitHub/Oysters/RAP
getwd() # if not, the use setwd() to change the current working directory

## Read in datasheets
# Metadata
mdat <- read.xlsx(xlsxFile = './Scoring Metadata/RAP_Metadata.xlsx',
                  sheet = 1)

# Site coordinates
coord <- read.xlsx(xlsxFile = './Data/Site Coordinates/Rapp_22804_Presurvey_Points_BelleIsle.xlsx',
                   sheet = 1)

## Side A
# Define variables (edit variables Site_a, Date_a, & Location_a)
Site_a <- c(13, 21, 89, 103, 129, 142, 160, 239, 255, 296, 340, 360)
Date_a <- rep("08/17/2023", length(Site_a))
Location_a <- rep("Rappahannock Belle Isle", length(Site_a))
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


# Create dataframe (do not edit)
df_a <- data.frame(Date_a, Location_a, Site_a, Coord_x_a, Coord_y_a, 
                   Random_Assignment_a, Habscore_a, Notes_a)

# Add rows for missing camera images (Type "NA" in the Random_Assignment_b column for each row created in Excel)
#df_a <- df_a %>%
#  add_row(Date_a = "05/11/2023", Location_a = "Eastern Bay", Site_a = 15, Habscore_a = NA, Notes_a = "Missing image")

## Side B
# Define variables (edit variables Site_b, Date_b, & Location_b)
Site_b <- c(13, 21, 89, 103, 129, 142, 160, 239, 255, 296, 340, 360)
Date_b <- rep("08/17/2023", length(Site_b))
Location_b <- rep("Rappahannock Belle Isle", length(Site_b))
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

# Add rows for missing camera images (Type "NA" in the Random_Assignment_b column for each row created in Excel)
#df_b <- df_b %>%
#  add_row(Date_b = "05/11/2023", Location_b = "Eastern Bay", Site_b = 7, Habscore_b = NA, Notes_b = "Missing image")

## Combine dataframes into a list (do not edit)
sheets <- list("Metadata" = mdat, "Side A" = df_a, "Side B" = df_b)

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

# edit file name and location if necessary
filename <- 'RB_2023229_NoMaxScores.xlsx'

# Export list to excel 
write_xlsx(sheets, file.path(wd, sub_dir, filename))
