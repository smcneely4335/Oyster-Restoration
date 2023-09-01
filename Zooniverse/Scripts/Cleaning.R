### Play with Zooniverse Data ###

# load tidyverse
if(!require(tidyverse)){
  install.packages(tidyverse)
  library('tidyverse')
} else {
  library
}

# check working directory
getwd()

# load in data
data <- read.csv('./Data/clean_zoodata_test.csv')
head(data)

# check if NAs exist in data$Score
for(i in 1:nrow(data)){
  if(is.na(data$Score[i])){
    print(paste(data$Site[i],"was not scored by",data$user_name[i]))
  } else {}  
}

# get the total scores per image

summ_site <- data %>% 
  group_by(Site, Score) %>% 
  summarize(Count = n())
summ_tot <- data %>% 
  group_by(Site) %>% 
  summarize(total = n())

summary <- spread(summ_site, Score, Count)
summary$Total <- summ_tot$total
summary$Expert_Score <- sort(data$Site)

sort(unique(data$Site))

es <- list()
site <- list()

# get just the sites into a list and then extract the expert score for each of those unique sites
for(i in 1:nrow(data)){
  if(!(data$Site[i] %in% site)){
    site <- append(site, list(data$Site[i]))
  }
}
