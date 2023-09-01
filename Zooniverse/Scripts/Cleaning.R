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
Expert_Score <- c(1, 2, 0, 3, 1, 1, 1, 1, 1, 3, 1, 1, 1, 0, 1, 2, 1, 1, 0, 1, 1, 0, 0,
                  0, 0, 0, 0, 2, 1, 0, 1, 1, 3, 2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 3, 1, 0,
                  1, 0, 1, 0, 3, 1, 1, 1, 1, 0, 1, 3, 3, 3, 0, 3, 2, 3, 3, 0, 3, 3, 1,
                  1, 3, 3, 3, 3, 1, 3, 3, 1, 3, 3, 3, 3, 3, 1, 1, 1, 0, 1, 0, 1, 1, 1,
                  2, 3, 0, 3, 1, 0, 3, 3)
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

