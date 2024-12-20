# 10/2/2024
# Aaron Tayal
# Emerald ash borer Michigan 2024: analysis with transect as unit of replication
# I have written the word "Question" where I need help

library(ggplot2)
library(dplyr)
library(lme4) # Linear Mixed-Effects Models

library(car) # Companion to applied regression - Used to get an Anova table for
# the generalized linear mixed-effects model

library(emmeans) # Estimated Marginal Means, aka Least Squares Means (enables 
# pairwise comparisons to be made)

# Seedlings model - transect is unit of replication ############################

# I want to run a generalized linear mixed-effects model to determine if the 
# number of ash seedlings varies by hydroclass. 

# Predictor variable: Hydroclass (xeric, mesic, hydric). The column is called 
# mstrlvl. It is a categorical predictor.

# Response variable: Total number of ash seedlings found in a transect (which 
# consists of 3 plots). It is a count (integer value), so it might be expected 
# to follow a Poisson distribution if there is no aggregation of seedlings in clusters.

# Grouping variable to account for spatial structure of the data: Park. There 
# are 7 Parks located on the western side of Detroit, Michigan. Not all hydroclasses
# are represented at each park, but at least 2 out of 3 are found at each park 
# (see ash_occurence.R)

ash_by_transect <- read.csv("Cleaned_data/ash_by_transect.csv")
ash_by_transect$Park <- as.factor(ash_by_transect$Park) # Grouping variable (random effect)
ash_by_transect$mstrlvl <- as.factor(ash_by_transect$mstrlvl) # Predictor

# Question: Major problem with data ###########################################
# Unfourtunately, the first trip to Michigan in 2024 we counted seedlings using 
# microplot PVCs that were too small (with an area of only 3.37 m^2). After that
# we switched to PVCs with an area of 4.06 m^2 in order to be consistent with 
# previous studies, which had a microplot area of 4 m^2. I corrected for this 
# problem in the seedling density variables. However, the seedling counts are 
# uncorrected. Thus, this is a weakness in the data. In 2025 we may revisit these
# plots.

# Back to stats...

# Graph the data first:
dotchart(ash_by_transect$total_number_seedlings, 
         group = ash_by_transect$mstrlvl)
hist(ash_by_transect$total_number_seedlings, breaks=seq(0,270,10))
ggplot(data=ash_by_transect, aes(x=mstrlvl, y=total_number_seedlings)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(aes(color=Park), height=0, width=0.1, alpha=0.6) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Number of seedlings in transect")
# There seems to be around 8 transects where high numbers of ash seedlings occur. 
# I wonder what commonality, if any, exists between those transects. It should 
# be noted that 6 out of 8 of these high-seedling transects were located at 
# Pontiac. Something about Pontiac (previous ash tree density? soil type? water
# availability in the soil?) is causing it to have many ash seedlings

# Fit the model:
seedling_mod <- glmer(total_number_seedlings ~ mstrlvl + (1|Park), 
                      data=ash_by_transect, family="poisson")
summary(seedling_mod)

# Below is code for running the model without a random effect:
seedling_mod_without_Park <- glm(total_number_seedlings ~ mstrlvl, 
                              data=ash_by_transect, family="poisson")
summary(seedling_mod_without_Park)


# To check the estimates of the fixed effect (hydroclass), I'll compute the mean
# number of seedlings in hydric, mesic, and xeric, and then compare that to
# what the model says:
seedling_means <- as.data.frame(tapply(ash_by_transect$total_number_seedlings, ash_by_transect$mstrlvl,
       mean)) # Hydric: 22.6 seedlings; Mesic: 154 seedlings; Xeric: 90.83 seedlings
colnames(seedling_means) <- "Mean_seedlings"
seedling_means$log_seedling_means <- log(seedling_means$Mean_seedlings) # Take the 
# logarithm because that is the link function in the generalized linear model
seedling_means$fixed_effects_in_GLM <- data.frame(coef(summary(seedling_mod_without_Park)))$Estimate # model without random effect
seedling_means$fixed_effects_in_GLMM <- data.frame(coef(summary(seedling_mod)))$Estimate # model with random effect
# The above code allows me to compare the group means to the estimates of the fixed
# effects. Because the residuals are Poisson distributed with a natural log link,
# the log of the group mean equals the estimate of the fixed effect WHEN THERE IS
# NO RANDOM EFFECT INCLUDED. Interestingly, inclusion of the random effect (Park)
# caused the estimated differences |mesic-hydric| and |xeric-hydric| to become
# smaller in absolute value. To me, this suggests that the grouping variable (Park) 
# is helping to account for structure in the dataset.

Anova(seedling_mod, type = "III")
emmeans(seedling_mod, pairwise ~ mstrlvl)

# Check model assumptions (Question: I'm unclear exactly what the assumptions are):
qqnorm(resid(seedling_mod)) # I don't know why we are checking the residuals
# for normality if we think the residuals are Poisson distributed
qqline(resid(seedling_mod)) 
plot(seedling_mod)
getME(seedling_mod, "b") # Look at the actual estimated intercepts for 
# each Park (the "conditional modes of the random effects")
levels(ash_by_transect$Park) # Looks like the 6th park is Pontiac, which
# had a higher number of seedlings
# Note: I have no idea how to actually make a q-q plot of these estimated intercepts

# Test for overdispersion: (this function was copied from Ben Bolker's GLMM FAQ:
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(seedling_mod) # Question: how do I interpret this result?

# Short seedlings model #######################################################

ggplot(data=ash_by_transect, aes(x=mstrlvl, y=total_number_short)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Number of short seedlings in transect")

short_seedling_mod <- glmer(total_number_short ~ mstrlvl + (1|Park), 
                      data=ash_by_transect, family="poisson")
summary(short_seedling_mod)
Anova(short_seedling_mod, type = "III")
emmeans(short_seedling_mod, pairwise ~ mstrlvl)

qqnorm(resid(short_seedling_mod))
qqline(resid(short_seedling_mod))
plot(short_seedling_mod)

# Tall seedlings model ########################################################

ggplot(data=ash_by_transect, aes(x=mstrlvl, y=total_number_tall)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Number of tall seedlings in transect")

tall_seedling_mod <- glmer(total_number_tall ~ mstrlvl + (1|Park), 
                            data=ash_by_transect, family="poisson")
tall_seedling_mod_without_Park <- glm(total_number_tall ~ mstrlvl, 
                                      data=ash_by_transect, family="poisson")
summary(tall_seedling_mod)
# Interestingly, the Park variable is not soaking up as much variation
# in number of tall seedlings (Variance of random effect = 0.38) as it
# did in the number of short seedlings (Variance of the random effect = 1.23).
Anova(tall_seedling_mod, type = "III")
emmeans(tall_seedling_mod, pairwise ~ mstrlvl)

qqnorm(resid(tall_seedling_mod))
qqline(resid(tall_seedling_mod))
plot(tall_seedling_mod)

# Percent cover seedlings model ###############################################

ggplot(data=ash_by_transect, aes(x=mstrlvl, y=mean_percent_cover_seedlings)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Mean percent cover ash seedlings")

# Mean percent cover is a numeric response variable, so I should be able to run
# a linear mixed effects model.
library(lmerTest)
percent_cov_seedling_mod <- lmer(mean_percent_cover_seedlings ~ mstrlvl + (1|Park), 
                           data=ash_by_transect)
summary(percent_cov_seedling_mod)
Anova(percent_cov_seedling_mod, type = "III")
emmeans(percent_cov_seedling_mod, pairwise ~ mstrlvl) # Question: do I only 
# run pairwise comparisons when the Anova table shows a significant effect?

# Assumptions of linear mixed-effects models: 
# 1. Relationship between response and predictor is linear
# 2. Residuals are independent (because the grouping factor accounted for non-independence)
# 3. Residuals are normally distributed
# 4. Residuals are homoscedastic
# 5. The random intercepts (or random slopes, if applicable) are normally distributed
qqnorm(resid(percent_cov_seedling_mod))
qqline(resid(percent_cov_seedling_mod))
plot(percent_cov_seedling_mod) # I'm definitely seeing heteroscedasticity in
# the residuals. The bigger the fitted value, the larger the residuals are in
# absolute value. 
intercepts_percent_cov_model <- ranef(percent_cov_seedling_mod)$Park
hist(intercepts_percent_cov_model$`(Intercept)`, breaks=10) 
# The intercepts do not appear to be normally distributed. Overall, this model
# appears to be invalid.

# Saplings model ##############################################################

# graph the data:
ggplot(data=ash_by_transect, aes(x=mstrlvl, y=total_number_saplings)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Total number of saplings")

saplings_model <- glmer(total_number_saplings ~ mstrlvl + (1|Park),
                        data=ash_by_transect, family="poisson")
summary(saplings_model)
# I'm noticing that the median number of saplings is highest in hydric, then
# mesic, then xeric, but the mean shows a different ranking.

