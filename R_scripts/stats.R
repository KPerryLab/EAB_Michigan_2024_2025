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
# to follow a Poisson distribution if there is no aggregation of seedlings in 
# clusters.

# Grouping variable to account for spatial structure of the data: Park. There 
# are 7 Parks located on the western side of Detroit, Michigan. Not all hydroclasses
# are represented at each park, but at least 2 out of 3 are found at each park 
# (see ash_occurence.R)

ash_by_transect <- read.csv("Cleaned_data/ash_by_transect.csv")
ash_by_transect$Park <- as.factor(ash_by_transect$Park) # Grouping variable (random effect)
ash_by_transect$mstrlvl <- as.factor(ash_by_transect$mstrlvl) # Predictor
table(ash_by_transect$mstrlvl) # 5 hydric transects, 7 mesic, 18 xeric

# Question: Major problem with data ###########################################
# Unfortunately, the first trip to Michigan in 2024 we counted seedlings using 
# microplot PVCs that were too small (with an area of only 3.37 m^2). After that
# we switched to PVCs with an area of 4.06 m^2 in order to be consistent with 
# previous studies, which had a microplot area of 4 m^2. I corrected for this 
# problem in the seedling density variables. However, the seedling counts are 
# uncorrected. Thus, this is a weakness in the data. In 2025 we may revisit these
# plots.

# Back to stats...

# Get a general idea of overdispersion in the data ############################
# Poisson models require that for a given fitted value, the variance is equal to
# the mean. Here, I'll compute the mean and variance for each variable in the 
# data, to see if they are approximately equal.
mean_summary <- ash_by_transect %>% group_by(mstrlvl) %>%
  summarize(across(mean_plotmstr:mean_basal_area_living_big_trees_m_squared_per_ha, 
                   mean))
variance_summary <- ash_by_transect %>% group_by(mstrlvl) %>%
  summarize(across(mean_plotmstr:mean_basal_area_living_big_trees_m_squared_per_ha, 
                   var))
# I can see that for the total # of seedlings, total # of short
# seedlings, total # of tall seedlings, total # of saplings, and total # of living
# small trees, the variances are generally more than 10 times the means. This 
# suggests to me (although does not prove) that Poisson regression would not be 
# appropriate. My plan is to try out the Poisson regression for total # of seedlings
# and then just stick with negative binomial from then on.

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

# Fit the Poisson regression model:
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

car::Anova(seedling_mod, type = "III")
emmeans(seedling_mod, pairwise ~ mstrlvl) # pairwise comparisons

# Check model assumptions (Question: I'm unclear exactly what the assumptions are):
plot(seedling_mod)# With a quick Google search, it seems that the "Pearson residuals" are 
# are standardized residuals, so we might (?) expect this graph to appear 
# homoscedastic (even though the residuals of the Poisson model are clearly
# not assumed to be homoscedastic)

getME(seedling_mod, "b") # Look at the actual estimated intercepts for 
# each Park (the "conditional modes of the random effects")
levels(ash_by_transect$Park) # Looks like the 6th park is Pontiac, which
# had a higher number of seedlings
# Note: I have no idea how to actually make a q-q plot of these estimated intercepts

# Test for overdispersion. The Poisson GLM with log link assumes that 1) the 
# logarithm of the response is linearly related to the predictor, and 2) the 
# residuals are Poisson distributed. For a random variable Y with a Poisson 
# distribuion with parameter lambda, the mean of Y is lambda and the variance
# of Y is lambda. However, in some ecological count data, the variance is higher
# than the mean for a given predictor value. The overdispersion tests can 
# determine whether the Poisson GLM adequately fits the distribution of the data.

# This function was copied from Ben Bolker's GLMM FAQ:
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

# Here is a different overdispersion test (here I'm testing the model that 
# omits the random effect of Park):
library(AER)
AER::dispersiontest(seedling_mod_without_Park, trafo = 1) # Assesses the null hypothesis that
# Var[y] = E[y] = u (basically, that the mean and variance are equal), against 
# a model like Var[y] = u + a*(u^trafo), where u is the mean and trafo is a 
# parameter. If trafo=1, then Var[y] = (1+a)*u

# The result seems to suggest that for seedling_mod_without_Park, the variance 
# is about 68 times the mean.

# Here is code (taken from Google AI) that can plot the raw, un-standardized 
# residuals against the fitted values. As you can see, there are three fitted
# values for the hydric, mesic, and xeric levels of the predictor. These are 
# ~25 seedlings (hydric), ~90 seedlings (xeric), and ~160 seedlings (mesic). The
# overdispersion
raw_residuals <- residuals(seedling_mod_without_Park, type = "response") 
plot(fitted(seedling_mod_without_Park), raw_residuals, xlab = "Fitted Values", 
     ylab = "Raw Residuals") 
abline(h = 0)
# I notice that the counts for xeric seedlings range between about 0 all the way
# to ~240. This is due to that bimodal pattern we saw when initially graphing
# the data:
dotchart(ash_by_transect$total_number_seedlings, 
         group = ash_by_transect$mstrlvl)

# In contrast, if the residuals were Poisson distributed with a mean of 90 
# seedlings, then the majority of the samples should fall between about 70 to 
# 110 seedlings:
hist(rpois(1000, 90), breaks = 50) # Poisson distribution with lambda=90

# Essentially, the residuals are NOT Poisson distributed.

# Seedlings model with negative binomial regression #############################

# To fix the model, I'd like to use a negative binomial model. Here is a website
# that can help explain it's distribution: https://homepage.divms.uiowa.edu/~mbognar/applets/nb1.html

# The following code was found in "ENTMLGY 6707 Entomological Techniques and 
# Data Analysis: R Activity 9: Generalized Linear (Mixed-Effects) Models (taught
# by Dr. Perry and Dr. Ward):
library(MASS)
seedling_mod_without_Park_negbin <- 
  MASS::glm.nb(total_number_seedlings~mstrlvl, data=ash_by_transect, 
         link=log)
summary(seedling_mod_without_Park_negbin) # I don't quite know how to interpret
# this model, but it does seem that the p-values are less miniscule than in the 
# Poisson model. I think that suggests that this model is a better fit.

# Now I can run a generalized linear mixed-effects model incorporating Park:
seedling_mod_negbin <- 
  lme4::glmer.nb(total_number_seedlings~mstrlvl + (1|Park), data=ash_by_transect)
summary(seedling_mod_negbin) # Incorporating the random effect Park actually
# decreased the p-values, which is not what I would expect.
Anova(seedling_mod_negbin, type="III")
emmeans(seedling_mod_negbin, pairwise~mstrlvl)

# Short seedlings model #######################################################

ggplot(data=ash_by_transect, aes(x=mstrlvl, y=total_number_short_seedlings)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Number of short seedlings in transect")

# Now I can run a generalized linear mixed-effects model incorporating Park:
short_seedling_mod_negbin <- 
  lme4::glmer.nb(total_number_short_seedlings~mstrlvl + (1|Park), 
                 data=ash_by_transect)
summary(short_seedling_mod_negbin)
Anova(short_seedling_mod_negbin, type="III")
emmeans(short_seedling_mod_negbin, pairwise~mstrlvl)

# Tall seedlings model ########################################################

ggplot(data=ash_by_transect, aes(x=mstrlvl, y=total_number_tall_seedlings)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Number of tall seedlings in transect")

# Now I can run a generalized linear mixed-effects model incorporating Park:
tall_seedling_mod_negbin <- 
  lme4::glmer.nb(total_number_tall_seedlings~mstrlvl + (1|Park), 
                 data=ash_by_transect)
summary(tall_seedling_mod_negbin)
Anova(tall_seedling_mod_negbin, type="III")
# Interestingly, the Park variable is not soaking up as much variation
# in number of tall seedlings (Variance of random effect = 0.29) as it
# did in the number of short seedlings (Variance of the random effect = 0.57).
emmeans(tall_seedling_mod_negbin, pairwise~mstrlvl)

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
# The random intercepts do not appear to be normally distributed. Overall, this 
# model appears to be invalid.

# I can try log-transforming the percent cover values in order to fix the violation
# of homoscedasticity:
ash_by_transect$log_mean_percent_cover_seedlings <- 
  log(ash_by_transect$mean_percent_cover_seedlings + 1) # Some
# transects had less than 1% mean cover or even 0% cover, so I had to use the 
# formula log((% cover) + 1)

# Graph the data:
ggplot(data=ash_by_transect, aes(x=mstrlvl, y=log_mean_percent_cover_seedlings)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(aes(color=Park),height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Log_e of (mean percent cover ash seedlings + 1)")

log_percent_cov_seedling_mod <- lmer(log_mean_percent_cover_seedlings ~ mstrlvl + 
                                       (1|Park), 
                                 data=ash_by_transect)
summary(log_percent_cov_seedling_mod)
Anova(log_percent_cov_seedling_mod, type = "III")
emmeans(log_percent_cov_seedling_mod, pairwise ~ mstrlvl)

# Check assumptions:
qqnorm(resid(log_percent_cov_seedling_mod))
qqline(resid(log_percent_cov_seedling_mod))
plot(log_percent_cov_seedling_mod) # The residuals of the model with 
# log-transformed response appear to be homoscedastic
intercepts_log_percent_cov_model <- ranef(log_percent_cov_seedling_mod)$Park
hist(intercepts_log_percent_cov_model$`(Intercept)`, breaks=10) 

# Saplings model ##############################################################

# graph the data:
ggplot(data=ash_by_transect, aes(x=mstrlvl, y=total_number_saplings)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Total number of saplings")
# I'm noticing that the median number of saplings is highest in hydric, but 
# the average number of saplings is lowest in hydric. This suggests that
# hydric transects may actually have more saplings, if only it had a bigger 
# sample size, or that xeric and mesic transects have more variability in 
# the number of saplings.

saplings_model_negbin <- lme4::glmer.nb(total_number_saplings ~ mstrlvl + (1|Park),
                        data=ash_by_transect)
# I got a singular fit (Question: don't know what that means), so I'm going to
# simply run a model excluding the Park variable:
saplings_model_without_Park_negbin <- 
  MASS::glm.nb(total_number_saplings ~ mstrlvl, data=ash_by_transect)
summary(saplings_model_without_Park_negbin)
Anova(saplings_model_without_Park_negbin, type="III")
emmeans(saplings_model_without_Park_negbin, pairwise~mstrlvl)

# Living small trees model ####################################################

# Graph the data:
ggplot(data=ash_by_transect, aes(x=mstrlvl, y=total_number_living_small_trees)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(aes(color=Park), height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Number of living small trees")

small_trees_model_negbin <- 
  lme4::glmer.nb(total_number_living_small_trees ~ mstrlvl + (1|Park),
                 data=ash_by_transect)
# The model did not converge, so I'll need to run it without the random effect
small_trees_model_without_Park_negbin <- 
  MASS::glm.nb(total_number_living_small_trees ~ mstrlvl, data=ash_by_transect)
summary(small_trees_model_without_Park_negbin)
Anova(small_trees_model_without_Park_negbin, type="III")
emmeans(small_trees_model_without_Park_negbin, pairwise ~ mstrlvl)

# Basal area of small trees model #############################################

# Graph the data:
ggplot(data=ash_by_transect, 
       aes(x=mstrlvl, y=mean_basal_area_living_small_trees_m_squared_per_ha)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(aes(color=Park), height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Mean basal area of living small trees (m^2 / ha)")

# I don't know what the appropriate model is for this data. It is continuous 
# data and yet there are many zeros, and a wide variability in the magnitude.



