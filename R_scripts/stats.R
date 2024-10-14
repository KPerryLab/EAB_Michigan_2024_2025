# 10/2/2024
# Aaron Tayal
# I want to run a generalized linear mixed model to determine if the number of 
# ash seedlings varies by hydroclass. My unit of replication is the transect.

library(ggplot2)
library(dplyr)
library(lme4)

library(car)
library(emmeans)

seedlings_by_transect <- read.csv("Cleaned_data/seedlings_by_transect.csv")
seedlings_by_transect$Transect <- as.factor(seedlings_by_transect$Transect)
seedlings_by_transect$Park <- as.factor(seedlings_by_transect$Park)
seedlings_by_transect$mstrlvl <- as.factor(seedlings_by_transect$mstrlvl)

dotchart(seedlings_by_transect$total_number_seedlings, group = seedlings_by_transect$mstrlvl)
hist(seedlings_by_transect$total_number_seedlings)
boxplot(seedlings_by_transect$total_number_seedlings ~ seedlings_by_transect$mstrlvl)
stripchart(total_number_seedlings ~ mstrlvl, data = seedlings_by_transect, col = c("black"), vertical = TRUE,
           pch = 19, cex = 2, add = TRUE, method = "jitter", jitter = 0.2)

seedling_mod <- glmer(total_number_seedlings ~ mstrlvl + (1|Park), data=seedlings_by_transect,
      family="poisson")
summary(seedling_mod)
Anova(seedling_mod)
emmeans(seedling_mod, pairwise ~ mstrlvl)

qqnorm(resid(seedling_mod))
qqline(resid(seedling_mod))
