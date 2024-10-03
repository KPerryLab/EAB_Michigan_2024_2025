# 10/2/2024
# Aaron Tayal
# I want to run a generalized linear mixed model to determine if the number of 
# ash seedlings varies by hydroclass. My unit of replication is the transect.

library(ggplot2)
library(dplyr)
library(lme4)

seedlings_by_transect <- read.csv("Cleaned_data/seedlings_by_transect.csv")
seedlings_by_transect$Transect <- as.factor(seedlings_by_transect$Transect)
seedlings_by_transect$Park <- as.factor(seedlings_by_transect$Park)
seedlings_by_transect$mstrlvl <- as.factor(seedlings_by_transect$mstrlvl)



glmer(total_number_seedlings ~ mstrlvl + (1|Park), data=seedlings_by_transect,
      family="poisson")


