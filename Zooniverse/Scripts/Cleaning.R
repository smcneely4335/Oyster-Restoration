### Play with Zooniverse Data ###
## load packages
# make sure pacman package is installed
if(!require(pacman)) {
  install.packages('pacman')
}

# load packages using pacman package function p_load()
pacman::p_load(tidyverse)

# expert_score working directory
getwd()

# load in data
data <- read.csv('./Data/clean_zoodata_test.csv') %>% 
  data.frame()
head(data)

# expert_score if NAs exist in data$Score by iterating through data
for(i in 1:nrow(data)) {
  if(is.na(data$Score[i])) {
    print(paste(data$Site[i], "was not scored by", data$user_name[i]))
  } 
}

## extract the expert scores per site
# make sure gtools package is installed
if(!require(gtools)) {
  install.packages('gtools')
}

# sort data by Site using gtools function mixedorder()
# mixedorder() sorts via natural order sort, putting B1a before B10a for example
# could also use group_by(), but it will put B10a before B1a
data_sort <- data[gtools::mixedorder(data$Site),]
head(data_sort)

## extract sites and expert scores based on the sites
# create empty vectors for the sites and expert scores
sites <- c()
expert_score <- c()

# iterate through every row of data_sort
for(i in 1:nrow(data_sort)) {
  
  # check if the site in row i already exists in the sites vector
  if(data_sort$Site[i] %in% sites) {
    
    # if the site in row i does exist in the sites vector, go to the next row
    next
  } else {
    
    # append the sites and expert_score vectors with appropriate values
    sites <- append(x = sites, values = data_sort$Site[i], after = length(sites))
    expert_score <- append(x = expert_score, values = data_sort$Expert_Score[i], 
                           after = length(expert_score))
  }
}

# view both vectors to check that they look right
sites
expert_score

# summarize the data
# get the total scores per image
summ_site <- data_sort %>% 
  
  # group_by() sorts the data (i.e., data_sort), but not how we want it exactly
  # which is why I sorted using gtools:mixedorder() above
  # The primary reason for using it here is to prep the columns for summarization
  group_by(Site, Score) %>% 
  
  # summarize() essentially categorizes the two desired variables above (i.e., Site
  # and Score) and sorts, first, according to Site and then according to Score. Then
  # the call Count = n() creates a new variable (i.e., Count) and sums up every score
  # per site. These three variable are then tossed into a new dataframe.
  summarize(Count = n())

summ_tot <- data %>% 
  group_by(Site) %>% 
  summarize(total = n())
summary <- spread(summ_site, Score, Count)
colnames(summary) <- c('site', 'zero', 'one', 'two', 'three')
summary$total <- summ_tot$total
summary[is.na(summary)] <- 0
summary$expert_score <- Expert_Score

# look at data ordered alphanumerically by site to copy over the correct order of the expert score
data <- data[with(data, order(Site)), ]


# Find the % of student scores that are correct for each site
# df: site, expert_score, percent_correct
perc_correct_df <- data.frame(summary$site, summary$expert_score) %>% 
  mutate(percent_correct = case_when(summary$expert_score == 0 ~ round((100 * (summary$zero/summary$total)), digits = 2),
                                     summary$expert_score == 1 ~ round((100 * (summary$one/summary$total)), digits = 2),
                                     summary$expert_score == 2 ~ round((100 * (summary$two/summary$total)), digits = 2),
                                     summary$expert_score == 3 ~ round((100 * (summary$three/summary$total)), digits = 2)
  ))
colnames(perc_correct_df) <- c('site', 'expert_score', 'percent_correct')

# if a majority of students didn't get it correct, then what score did a majority assign
summ_perc <- data.frame()

for(i in 1:nrow(summary)){
   summ_perc[nrow(summ_perc) + 1, 'site'] <- summary$site[i]
   
   zero_perc <- round((100 * (summary$zero[i]/summary$total[i])), digits = 2)
   summ_perc[nrow(summ_perc), 'zero_perc'] <- zero_perc
   
   one_perc <- round((100 * (summary$one[i]/summary$total[i])), digits = 2)
   summ_perc[nrow(summ_perc), 'one_perc'] <- one_perc
   
   two_perc <- round((100 * (summary$two[i]/summary$total[i])), digits = 2)
   summ_perc[nrow(summ_perc), 'two_perc'] <- two_perc
   
   three_perc <- round((100 * (summary$three[i]/summary$total[i])), digits = 2)
   summ_perc[nrow(summ_perc), 'three_perc'] <- three_perc
   
   mostcomm <- list()
   if(zero_perc > one_perc & two_perc & three_perc)
   
   summ_perc[nrow(summ_perc), 'expert_score'] <- summary$expert_score[i]
}





tempdf <- data.frame(unique(data$Site), unique(data$Expert_Score))

site <- c()
for(i in 1:length(unique(data$Site))){
  site <- append(site, unique(data$Site)[i])
}
uniq_data <- data[unique(Site), ]



sort(unique(data$Site))


# get just the sites into a list and then extract the expert score for each of those unique sites
for(i in 1:nrow(data)){
  if(!(data$Site[i] %in% site)){
    site <- append(site, list(data$Site[i]))
    tempdf[nrow(tempdf) + 1,] <- data$Site[i]
  }
}

lst_obj <- list(rbind(as.character(site), es))
tempdf <- data.frame(lst_obj) %>% 
  t() %>% 
  data.frame()
summary$Expert_Score <- c()

