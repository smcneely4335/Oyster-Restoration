### Comparison: run after GoPro images have been scored

##########################################################

### Author: Sam McNeely
### Collaborators: 
### Date created: 07/24/2023
### Date modified: 07/24/2023
### File name: MaxFinder_.R

##########################################################
## set up for analysis
# load packages
library(writexl)
library(tidyverse)
library(openxlsx)

# set working directory
wd <- "S:/FishCon/Projects/Oysters/2023 field projects/Rappahannock River/Data"
setwd(wd)

# read excel file, side A; also make it a dataframe
datasheet_a <- read.xlsx("NoMaxScores/RB_2023228_NoMaxScores.xlsx", 2)
df_a <- data.frame(datasheet_a) %>%
  arrange(Site_a)

# read excel file, side B; also make it a dataframe
datasheet_b <- read.xlsx("NoMaxScores/RB_2023228_NoMaxScores.xlsx", 3)
df_b <- data.frame(datasheet_b) %>%
  arrange(Site_b)

# create variables for the comparison dataframe df_comp
Date <- df_a[, 'Date_a']
Location <- df_a[, 'Location_a']
Site_a <- df_a[, 'Site_a']
Habscore_a <- as.numeric(df_a[, 'Habscore_a'])
#Percent_Cover_a <- as.numeric(df_a[, 'Percent_Cover_a'])
#Substrate_Type_a <- as.numeric(df_a[, 'Substrate_Type_a'])
#Sedimentation_a <- as.numeric(df_a[, 'Sedimentation_a'])
Notes_a <- df_a[, 'Notes_a']
Site_b <- df_b[, 'Site_b']
Habscore_b <- as.numeric(df_b[, 'Habscore_b'])
#Percent_Cover_b <- as.numeric(df_b[, 'Percent_Cover_b'])
#Substrate_Type_b <- as.numeric(df_b[, 'Substrate_Type_b'])
#Sedimentation_b <- as.numeric(df_b[, 'Sedimentation_b'])
Notes_b <- df_b[, 'Notes_b']

# create comparison sheet with df_comp
df_comp <- data.frame(Date, Location, Site_a, Habscore_a, Notes_a, 
                      #Substrate_Type_a, Sedimentation_a, Percent_Cover_a, 
                      Site_b, Habscore_b, Notes_b
                      #Percent_Cover_b, Substrate_Type_b, Sedimentation_b
)

# Find max scores
# create max habitat score variable

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

# create max percent cover score variable
df_comp$Max_Percent_Cover <- pmax(df_comp$Percent_Cover_a, df_comp$Percent_Cover_b, na.rm = TRUE)

# create max substrate type score variable
df_comp <- df_comp %>% 
  mutate(Max_Substrate_Type = case_when(
    # if both sides are equivalent, assign the output to one of the sides
    Substrate_Type_a == Substrate_Type_b ~ Substrate_Type_a,
    # if one side is NA and the other is 0:4, output 0:4
    is.na(Substrate_Type_a) == TRUE & Substrate_Type_b %in% c(0:4) ~ Substrate_Type_b,
    is.na(Substrate_Type_b) == TRUE & Substrate_Type_a %in% c(0:4) ~ Substrate_Type_a,
    # if one side is 0 and the other is 1:4, output the side with 1:4
    Substrate_Type_a == 0 & Substrate_Type_b %in% c(1:4) ~ Substrate_Type_b,
    Substrate_Type_b == 0 & Substrate_Type_a %in% c(1:4) ~ Substrate_Type_a,
    # if one side is 1 and the other is 2:3, output the side with 2:3
    Substrate_Type_a == 1 & Substrate_Type_b %in% c(2:3) ~ Substrate_Type_b,
    Substrate_Type_b == 1 & Substrate_Type_a %in% c(2:3) ~ Substrate_Type_a,
    # if one side is 1 and the other is 4, output as 4
    Substrate_Type_a == 1 & Substrate_Type_b %in% c(4) ~ 4,
    Substrate_Type_b == 1 & Substrate_Type_a %in% c(4) ~ 4,
    # if one side is 2 and the other is 3:4, output as 4
    Substrate_Type_a == 2 & Substrate_Type_b %in% c(3:4) ~ 4,
    Substrate_Type_b == 2 & Substrate_Type_a %in% c(3:4) ~ 4,
    # if one side is 3 and the other is 4, output as 4
    Substrate_Type_a == 3 & Substrate_Type_b %in% c(4) ~ 4,
    Substrate_Type_b == 3 & Substrate_Type_a %in% c(4) ~ 4,
  ))

# create max sedimentation score variable
df_comp <- df_comp %>% 
  mutate(Max_Sedimentation = case_when(
    # if both sides are equivalent, assign the output to one of the sides
    Sedimentation_a == Sedimentation_b ~ Sedimentation_a,
    # if one side is NA and the other is 0:4, output 0:4
    is.na(Sedimentation_a) == TRUE & Sedimentation_b %in% c(0:4) ~ Sedimentation_b,
    is.na(Sedimentation_b) == TRUE & Sedimentation_a %in% c(0:4) ~ Sedimentation_a,
    # if one side is 0 and the other is 1:4, output the side with 1:4
    Sedimentation_a == 0 & Sedimentation_b %in% c(1:4) ~ Sedimentation_b,
    Sedimentation_b == 0 & Sedimentation_a %in% c(1:4) ~ Sedimentation_a,
    # if one side is 1 and the other is 2:4, output as 4
    Sedimentation_a == 1 & Sedimentation_b %in% c(2:4) ~ 4,
    Sedimentation_b == 1 & Sedimentation_a %in% c(2:4) ~ 4,
    # if one side is 2 and the other is 3:4, output side with 3:4
    Sedimentation_a == 2 & Sedimentation_b %in% c(3:4) ~ Sedimentation_b,
    Sedimentation_b == 2 & Sedimentation_a %in% c(3:4) ~ Sedimentation_a,
    # if one side is 3 and the other is 4, output as 4
    Sedimentation_a == 3 & Sedimentation_b %in% c(4) ~ 4,
    Sedimentation_b == 3 & Sedimentation_a %in% c(4) ~ 4,
  ))

## Create warnings for abnormalities in the data
# check differences in habitat scoring
df_comp <- df_comp %>%
  mutate(Warning = case_when(abs(Habscore_a - Habscore_b) >= 2 ~ 'Difference +2'))

# check differences in Percent Cover scoring
df_comp <- df_comp %>%
  mutate(Warning_Percent_Cover = case_when(abs(Percent_Cover_a - Percent_Cover_b) >= 2 ~ 'Difference +2'))

# Create Check_Warnings columns to see if the warnings are justified
df_comp$Check_Warnings <- NA

## create list of data frames
sheets <- list("Side A" = df_a, "Side B" = df_b, "Comparison" = df_comp)

## create the new datasheet
# desired sub directory within the "Data" folder
sub_dir <- "MaxScores"

# check if the sub directory exists
if(file.exists(sub_dir)) {
  # if the sub directory already exists, then set it as the working directory
  setwd(sub_dir)
} else { # if the sub directory does not exist, create it
  # create the sub directory
  dir.create(sub_dir)
  # set the new sub directory as the new working directory
  setwd(sub_dir)
}

# Export list to excel (edit file name and location if necessary)
write_xlsx(sheets, file.path("RappahannockRiver_2023205_MaxScores.xlsx"))