###############################
### Wrangle Zooniverse Data ###
###############################

## load packages
# load rjson
if(!require(rjson)){ # if package rjson is not installed
  install.packages("rjson") # then install it
  library(rjson) # and load it 
} else {
  library(rjson) # if rjson is already installed then load it
}

# load tidyverse
if(!require(tidyverse)){ # if package tidyverse is not installed
  install.packages("tidyverse") # then install it
  library(tidyverse) # and load it 
} else {
  library(tidyverse) # if tidyverse is already installed then load it
}

## Update Working Directory
# check cwd
getwd()

# change directory
setwd('C:/Users/mcneelys/OneDrive - Smithsonian Institution/Desktop/Oysters/Data/GoPro/Zooniverse')

# check cwd again
getwd()

## Data wrangling
# load the data
rawdata <- read.csv('chesapeake-reefs-classifications_1June23.csv')
head(rawdata, n = 1)

# only keep columns with relevant data
colnames(rawdata)
data <- rawdata %>%
  select(user_name, annotations, subject_data) %>% 
  data.frame()
head(data, n = 2)

Date <- list()
Location <- list()
Site <- list()
Score <- list()
Expert_Score <- list()
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
  # append the amateur scores to the Score list
  Site <- append(Site, list(text)) # text must be converted into a list; if not, text will replace and not append
  
  # extract the amateur scores
  value_1 <- as.numeric(str_extract(data$annotations[i], '(?<=")[0-3]'))
  # append the amateur scores to the Score list
  Score <- append(Score, list(value_1)) # value_1 must be converted into a list; if not, value_1 will replace and not append

  # extract the amateur scores
  value_2 <- as.numeric(str_extract(data$subject_data[i], '(?<=Expert_Score":")[0-3]'))
  # append the amateur scores to the Score list
  Expert_Score <- append(Expert_Score, list(value_2)) # value_2 must be converted into a list; if not, value_2 will replace and not append
  
}

# remove 'annotations' and 'subject_data' columns
data <- data %>% 
  subset(select = -c(annotations, subject_data))

## begin adding lists to the dataframe
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
head(data)

write.csv(x = data, file = "clean_zoodata_test.csv")
