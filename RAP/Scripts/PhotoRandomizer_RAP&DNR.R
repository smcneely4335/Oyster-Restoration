### Excel Datasheet and Randomizer
# load packages
library(writexl)
library(readxl)
library(tidyverse)

## Metadata sheet
# read in the metadata file and convert it into a dataframe
metadata <- read_xlsx("S:/FishCon/Projects\\Oysters\\Datasheets\\Scoring Metadata\\RAP_DNR_Metadata.xlsx") %>% 
  data.frame()

## Side A
# Define variables (edit variables Date_a, Location_a, & Site_a)
Site_a <- c(1:10, 12:15, 17:18, 54:57, 64, 111)
Date_a <- rep("07/26/2023", length(Site_a))
Location_a <- rep("RCC", length(Site_a))
Random_Assignment_a <- sample(1:length(Site_a), length(Site_a))
Habscore_a <- rep(NA, length(Site_a))
Percent_Cover_a <- rep(NA, length(Site_a))
Substrate_Type_a <- rep(NA, length(Site_a))
Sedimentation_a <- rep(NA, length(Site_a))
Notes_a <- rep("", length(Site_a))

# Create dataframe (do not edit)
df_a <- data.frame(Date_a, Location_a, Site_a, Random_Assignment_a, Habscore_a, 
                   Percent_Cover_a, Substrate_Type_a, Sedimentation_a, Notes_a)

# Add rows for missing camera images 
# (Type "NA" in the Random_Assignment_b column for each row created in Excel)
df_a <- df_a %>%
  add_row(Date_a = "07/26/2023", Location_a = "RCC", Site_a = 11, 
          Habscore_a = 9, Percent_Cover_a = 9, Substrate_Type_a = 9, 
          Sedimentation_a = 9, Notes_a = "Missing image")

## Side B
# Define variables (edit variables Date_b, Location_b, & Site_b)
Site_b <- c(1:15, 17:18, 54:57, 64:65, 111)
Date_b <- rep("07/26/2023", length(Site_b))
Location_b <- rep("RCC", length(Site_b))
Random_Assignment_b <- sample(1:length(Site_b), length(Site_b))
Habscore_b <- rep(NA, length(Site_b))
Percent_Cover_b <- rep(NA, length(Site_b))
Substrate_Type_b <- rep(NA, length(Site_b))
Sedimentation_b <- rep(NA, length(Site_b))
Notes_b <- rep("", length(Site_b))

# Create dataframe (do not edit)
df_b <- data.frame(Date_b, Location_b, Site_b, Random_Assignment_b, Habscore_b, 
                   Percent_Cover_b, Substrate_Type_b, Sedimentation_b, Notes_b)

# Add rows for missing camera images (Type "NA" in the Random_Assignment_b column for each row created in Excel)
df_b <- df_b %>%
  add_row(Date_b = "07/26/2023", Location_b = "RCC", Site_b = 16, 
          Habscore_b = 9, Percent_Cover_b = 9, Substrate_Type_b = 9, 
          Sedimentation_b = 9, Notes_b = "Missing image")

## Combine dataframes into a list (do not edit)
sheets <- list("Metadata" = metadata, "Side A" = df_a, "Side B" = df_b)

## Create path for files
# set the working directory
wd <- "S:\\FishCon\\Projects\\Oysters\\2023 field projects\\Rappahannock River\\Data"

# setting up the sub directory
sub_dir <- "NoMaxScores"

# check if sub directory exists
if(file.exists(wd)) {
  # specifying the working directory
  setwd(file.path(wd))
  if(file.exists(sub_dir)) {
    setwd(file.path(wd, sub_dir))
  } else {
    dir.create(file.path(wd, sub_dir))
    setwd(file.path(wd, sub_dir))
  }
} else {
  # create a new sub directory inside
  # the main path
  dir.create(file.path(wd, sub_dir))
  # specifying the working directory
  setwd(file.path(wd, sub_dir))
}

# Export list to excel (edit file name and location if necessary)
write_xlsx(sheets, "RCC_2023207_NoMaxScores.xlsx")
