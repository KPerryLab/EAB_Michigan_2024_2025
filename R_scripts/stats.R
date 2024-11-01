# 10/2/2024
# Aaron Tayal
# Emerald ash borer Michigan 2024: analysis with transect as unit of replication

library(ggplot2)
library(dplyr)
library(lme4) # Linear Mixed-Effects Models

library(car) # Companion to applied regression
library(emmeans) # Estimated Marginal Means, aka Least Squares Means

# Transect is unit of replication - seedlings #####################################

# I want to run a generalized linear mixed-effects model to determine if the number of 
# ash seedlings varies by hydroclass. 

# Predictor variable: Hydroclass (xeric, mesic, hydric). The column is called 
# mstrlvl. It is a categorical predictor.

# Response variable: Total number of ash seedlings found in a transect (which 
# consists of 3 plots). It is a count (integer value), so it might be expected 
# to follow a Poisson distribution if there is no aggregation of seedlings in clusters.

# Grouping variable to account for spatial structure of the data: Park. There 
# are 7 Parks located on the western side of Detroit, Michigan. Not all hydroclasses
# are represented at each park, but at least 2 out of 3 are found at each park.

seedlings_by_transect <- read.csv("Cleaned_data/seedlings_by_transect.csv")
seedlings_by_transect$Park <- as.factor(seedlings_by_transect$Park) # Grouping variable
seedlings_by_transect$mstrlvl <- as.factor(seedlings_by_transect$mstrlvl) # Predictor

# Graph the data first:
dotchart(seedlings_by_transect$total_number_seedlings, 
         group = seedlings_by_transect$mstrlvl)
hist(seedlings_by_transect$total_number_seedlings, breaks=10)
boxplot(seedlings_by_transect$total_number_seedlings ~ seedlings_by_transect$mstrlvl)
stripchart(total_number_seedlings ~ mstrlvl, data = seedlings_by_transect, 
           col = c("black"), vertical = TRUE, pch = 19, cex = 2, add = TRUE, 
           method = "jitter", jitter = 0.2)
# There seems to be around 8 transects where high numbers of ash seedlings occur. 
# I wonder what commonality, if any, exists between those transects.

# Fit the model:
seedling_mod <- glmer(total_number_seedlings ~ mstrlvl + (1|Park), 
                      data=seedlings_by_transect, family="poisson")
summary(seedling_mod)
Anova(seedling_mod, type = "III")
emmeans(seedling_mod, pairwise ~ mstrlvl)

# Check model assumptions (I'm unclear exactly what the assumptions are):
qqnorm(resid(seedling_mod))
qqline(resid(seedling_mod))
plot(seedling_mod)
getME(seedling_mod, "b") # Look at the actual estimated intercepts for 
# each Park (apparently, the "conditional modes of the random effects")
levels(seedlings_by_transect$Park) # Looks like the 6th park is Pontiac, which
# had a higher number of seedlings
# Note: I have no idea how to actually make a q-q plot of these 

# Short seedlings model #######################################################

boxplot(seedlings_by_transect$total_number_short ~ seedlings_by_transect$mstrlvl)
stripchart(total_number_short ~ mstrlvl, data = seedlings_by_transect, 
           col = c("black"), vertical = TRUE, pch = 19, cex = 2, add = TRUE, 
           method = "jitter", jitter = 0.2)

short_seedling_mod <- glmer(total_number_short ~ mstrlvl + (1|Park), 
                      data=seedlings_by_transect, family="poisson")
summary(short_seedling_mod)
Anova(short_seedling_mod, type = "III")
emmeans(short_seedling_mod, pairwise ~ mstrlvl)

qqnorm(resid(short_seedling_mod))
qqline(resid(short_seedling_mod))
plot(short_seedling_mod)

# Tall seedlings model ########################################################

boxplot(seedlings_by_transect$total_number_tall ~ seedlings_by_transect$mstrlvl)
stripchart(total_number_tall ~ mstrlvl, data = seedlings_by_transect, 
           col = c("black"), vertical = TRUE, pch = 1, cex = 1, add = TRUE, 
           method = "jitter", jitter = 0.2)

tall_seedling_mod <- glmer(total_number_tall ~ mstrlvl + (1|Park), 
                            data=seedlings_by_transect, family="poisson")
summary(tall_seedling_mod)
Anova(tall_seedling_mod, type = "III")
emmeans(tall_seedling_mod, pairwise ~ mstrlvl)

qqnorm(resid(tall_seedling_mod))
qqline(resid(tall_seedling_mod))
plot(tall_seedling_mod)

# Percent cover seedlings model ###############################################

boxplot(seedlings_by_transect$mean_percent_cover ~ seedlings_by_transect$mstrlvl)
stripchart(mean_percent_cover ~ mstrlvl, data = seedlings_by_transect, 
           col = c("black"), vertical = TRUE, pch = 1, cex = 1, add = TRUE, 
           method = "jitter", jitter = 0.2)

# Mean percent cover is a numeric response variable, so I should be able to run
# a linear mixed effects model.
library(lmerTest)
percent_cov_seedling_mod <- lmer(mean_percent_cover ~ mstrlvl + (1|Park), 
                           data=seedlings_by_transect)
summary(percent_cov_seedling_mod)
Anova(percent_cov_seedling_mod, type = "III")
emmeans(percent_cov_seedling_mod, pairwise ~ mstrlvl)

qqnorm(resid(percent_cov_seedling_mod))
qqline(resid(percent_cov_seedling_mod))
plot(percent_cov_seedling_mod)

# Saplings model ##############################################################






