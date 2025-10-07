# 10/2/2024
# Aaron Tayal
# Emerald ash borer Michigan 2024: analysis with transect as unit of replication

library(ggplot2)
library(dplyr)
library(lme4) # Linear Mixed-Effects Models
library(ggpubr) # for figure

library(car) # Companion to applied regression - Used to get an Anova table for
# the generalized linear mixed-effects model

library(emmeans) # Estimated Marginal Means, aka Least Squares Means (enables 
# pairwise comparisons to be made)

library(DHARMa)
# check out this package for checking model assumptions
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html



# Background information ######################################################

# Transect is unit of replication

# I want to run a generalized linear mixed-effects model to determine if the 
# number of short ash seedlings varies by hydroclass. 

# Predictor variable: Hydroclass (xeric, mesic, hydric). The column is called 
# mstrlvl. It is a categorical predictor.

# Response variable: Total number of short ash seedlings found in a transect (which 
# consists of 3 plots). It is a count (integer value), so it might be expected 
# to follow a Poisson distribution if there is no aggregation of seedlings in 
# clusters.

# Grouping variable to account for spatial structure of the data: Park. There 
# are 7 Parks located on the western side of Detroit, Michigan. Not all hydroclasses
# are represented at each park, but at least 2 out of 3 are found at each park 
# (see ash_occurence.R)

# Basic plan: For each response variable, run a regular poisson regression, check 
# for overdispersion, then run the neg binom if it is appropriate.

ash_by_transect <- read.csv("Cleaned_data/ash_by_transect.csv")
ash_by_transect$Park <- as.factor(ash_by_transect$Park) # Grouping variable (random effect)
ash_by_transect$mstrlvl <- as.factor(ash_by_transect$mstrlvl) # Predictor
table(ash_by_transect$mstrlvl) # 10 hydric transects, 8 mesic, 19 xeric

# Short seedlings model #######################################################

ggplot(data=ash_by_transect, aes(x=mstrlvl, y=total_number_short_seedlings)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(aes(color=Park), height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Number of short seedlings in transect")
# There seems to be around 8 transects where high numbers of ash seedlings occur. 
# I wonder what commonality, if any, exists between those transects. It should 
# be noted that 6 out of 8 of these high-seedling transects were located at 
# Pontiac. Something about Pontiac (previous ash tree density? soil type? water
# availability in the soil?) is causing it to have many ash seedlings

ggplot(data=ash_by_transect, aes(x=mstrlvl, y=mean_density_short_seedlings)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(aes(color=Park), height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Density short seedlings (stems/m^2)")
summary(ash_by_transect$mean_density_short_seedlings)

# First run a Poisson GLMM:
short_seedling_mod_poisson <- 
  lme4::glmer(total_number_short_seedlings~mstrlvl + (1|Park),
              data=ash_by_transect, family="poisson")
summary(short_seedling_mod_poisson)

# Now check for overdispersion in the Poisson GLMM:
# First run a simulation that simulates new values of the random effect intercepts:
short_seedling_mod_poisson_sim_resid1 <- simulateResiduals(
  fittedModel = short_seedling_mod_poisson, re.form = NA, plot = T)

# Second, run a simulation that conditions over the previously fitted values
# of the random effect intercepts:
short_seedling_mod_poisson_sim_resid2 <- simulateResiduals(
  fittedModel = short_seedling_mod_poisson, re.form = NULL, plot = T)

# Run a test for dispersion:
testDispersion(short_seedling_mod_poisson_sim_resid2)
# Interpretation: The Poisson model is overdispersed

# Now run a negative binomial GLMM:
short_seedling_mod_negbin <- 
  lme4::glmer.nb(total_number_short_seedlings~mstrlvl + (1|Park), 
                 data=ash_by_transect)
summary(short_seedling_mod_negbin)

# Now check for overdispersion in the negative binom GLMM:
# First run a simulation that simulates new values of the random effect intercepts:
short_seedling_mod_negbin_sim_resid1 <- simulateResiduals(
  fittedModel = short_seedling_mod_negbin, re.form = NA, plot = T)

# Second, run a simulation that conditions over the previously fitted values
# of the random effect intercepts:
short_seedling_mod_negbin_sim_resid2 <- simulateResiduals(
  fittedModel = short_seedling_mod_negbin, re.form = NULL, plot = T)

# Run a test for dispersion:
testDispersion(short_seedling_mod_negbin_sim_resid2)
# Interpretation: The negative binomial GLMM is not over- or underdispersed

# Compare the Poisson model to the negative binomial model in terms of AIC value:
stats::anova(short_seedling_mod_poisson, short_seedling_mod_negbin)
library(bbmle)
bbmle::AICctab(short_seedling_mod_poisson, short_seedling_mod_negbin)
# Result: the Poisson model has a higher AIC value than the negative binomial model

# Test the null hypothesis of no differences by hydrology class:
Anova(short_seedling_mod_negbin, type="III")

# Do pairwise comparisons:
emmeans(short_seedling_mod_negbin, pairwise~mstrlvl)

# Tall seedlings model ########################################################

ggplot(data=ash_by_transect, aes(x=mstrlvl, y=total_number_tall_seedlings)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(aes(color=Park), height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Number of tall seedlings in transect")

ggplot(data=ash_by_transect, aes(x=mstrlvl, y=mean_density_tall_seedlings)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(aes(color=Park), height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Density tall seedlings (stems/m^2)")
summary(ash_by_transect$mean_density_tall_seedlings)

# First run a Poisson GLMM:
tall_seedling_mod_poisson <- 
  lme4::glmer(total_number_tall_seedlings~mstrlvl + (1|Park),
              data=ash_by_transect, family="poisson")
summary(tall_seedling_mod_poisson)

# Now check for overdispersion in the Poisson GLMM:
# First run a simulation that simulates new values of the random effect intercepts:
tall_seedling_mod_poisson_sim_resid1 <- simulateResiduals(
  fittedModel = tall_seedling_mod_poisson, re.form = NA, plot = T)

# Second, run a simulation that conditions over the previously fitted values
# of the random effect intercepts:
tall_seedling_mod_poisson_sim_resid2 <- simulateResiduals(
  fittedModel = tall_seedling_mod_poisson, re.form = NULL, plot = T)

# Run a test for dispersion:
testDispersion(tall_seedling_mod_poisson_sim_resid2)
# Interpretation: The Poisson model is overdispersed

# Now run a negative binomial GLMM:
tall_seedling_mod_negbin <- 
  lme4::glmer.nb(total_number_tall_seedlings~mstrlvl + (1|Park), 
                 data=ash_by_transect)
summary(tall_seedling_mod_negbin)
# Interestingly, the Park variable is not soaking up as much variation
# in number of tall seedlings (Variance of random effect = 0.23) as it
# did in the number of short seedlings (Variance of the random effect = 0.43).

# Now check for overdispersion in the negative binom GLMM:
# First run a simulation that simulates new values of the random effect intercepts:
tall_seedling_mod_negbin_sim_resid1 <- simulateResiduals(
  fittedModel = tall_seedling_mod_negbin, re.form = NA, plot = T)

# Second, run a simulation that conditions over the previously fitted values
# of the random effect intercepts:
tall_seedling_mod_negbin_sim_resid2 <- simulateResiduals(
  fittedModel = tall_seedling_mod_negbin, re.form = NULL, plot = T)

# Run a test for dispersion:
testDispersion(tall_seedling_mod_negbin_sim_resid2)
# Interpretation: The negative binomial GLMM is not over- or underdispersed

# Compare the Poisson model to the negative binomial model in terms of AIC value:
stats::anova(tall_seedling_mod_poisson, tall_seedling_mod_negbin)
library(bbmle)
bbmle::AICctab(tall_seedling_mod_poisson, tall_seedling_mod_negbin)
# Result: the Poisson model has a higher AIC value than the negative binomial model

# Test the null hypothesis of no differences by hydrology class:
Anova(tall_seedling_mod_negbin, type="III")

# Pairwise comparisons:
emmeans(tall_seedling_mod_negbin, pairwise~mstrlvl)

# Percent cover seedlings model ###############################################

ggplot(data=ash_by_transect, aes(x=mstrlvl, y=mean_percent_cover_seedlings)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Mean percent cover ash seedlings (%)")

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
# https://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot
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

# How do the percent cover of ash seedlings differ between hydric, mesic, and 
# xeric plots?
ash_by_transect %>% group_by(mstrlvl) %>% 
  summarize(mean_percent_cover_seedlings = mean(mean_percent_cover_seedlings))

# Saplings model ##############################################################

# graph the data:
ggplot(data=ash_by_transect, aes(x=mstrlvl, y=total_number_saplings)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  theme_bw() +
  ylim(c(0,0.7))+
  xlab("Hydroclass") +
  ylab("Total number of saplings")
# I'm noticing that the median number of saplings is highest in hydric, but 
# the average number of saplings is lowest in hydric. This suggests that
# hydric transects may actually have more saplings, if only it had a bigger 
# sample size, or that xeric and mesic transects have more variability in 
# the number of saplings.

# First run a Poisson GLMM:
saplings_mod_poisson <- 
  lme4::glmer(total_number_saplings~mstrlvl + (1|Park),
              data=ash_by_transect, family="poisson")
summary(saplings_mod_poisson)

# Now check for overdispersion in the Poisson GLMM:
# First run a simulation that simulates new values of the random effect intercepts:
saplings_mod_poisson_sim_resid1 <- simulateResiduals(
  fittedModel = saplings_mod_poisson, re.form = NA, plot = T)

# Second, run a simulation that conditions over the previously fitted values
# of the random effect intercepts:
saplings_mod_poisson_sim_resid2 <- simulateResiduals(
  fittedModel = saplings_mod_poisson, re.form = NULL, plot = T)

# Run a test for dispersion:
testDispersion(saplings_mod_poisson_sim_resid2)
# Interpretation: The Poisson model is overdispersed

# Now try a negative binomial model:
saplings_mod_negbin <- lme4::glmer.nb(total_number_saplings ~ mstrlvl + (1|Park),
                        data=ash_by_transect)
summary(saplings_mod_negbin) # park explains 0 variance
# I got a singular fit (Question: don't know what that means)

saplings_mod_negbin_sim_resid1 <- simulateResiduals(
  fittedModel = saplings_mod_negbin, re.form = NA, plot = T)

getME(saplings_mod_negbin, "b") # Look at the actual estimated intercepts for 
# each Park (the "conditional modes of the random effects")
# It seems that the fitted random effect intercepts are all zero.

saplings_model_without_Park_negbin <- 
  MASS::glm.nb(total_number_saplings ~ mstrlvl, data=ash_by_transect)
summary(saplings_model_without_Park_negbin)
Anova(saplings_model_without_Park_negbin, type="III")
emmeans(saplings_model_without_Park_negbin, pairwise~mstrlvl)

anova(saplings_mod_negbin, saplings_model_without_Park_negbin) # The negative
# binomial model with and without the random intercepts appear to have very similar
# AIC values.

# Living small trees model ####################################################

# Graph the data:
ggplot(data=ash_by_transect, aes(x=mstrlvl, y=total_number_living_small_trees)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  theme_bw() +
  xlab("Hydroclass") +
  ylab("Number of living small trees")

# Create a Poisson GLMM:
small_tree_mod_poisson <- 
  lme4::glmer(total_number_living_small_trees ~ mstrlvl + (1|Park), 
              family="poisson", data=ash_by_transect)

# Now check for overdispersion in the Poisson GLMM:
# First run a simulation that simulates new values of the random effect intercepts:
small_tree_mod_poisson_sim_resid1 <- simulateResiduals(
  fittedModel = short_seedling_mod_poisson, re.form = NA, plot = T)

# Second, run a simulation that conditions over the previously fitted values
# of the random effect intercepts:
small_tree_mod_poisson_sim_resid2 <- simulateResiduals(
  fittedModel = short_seedling_mod_poisson, re.form = NULL, plot = T)

# Run a test for dispersion:
testDispersion(small_tree_mod_poisson_sim_resid2)
# Interpretation: The Poisson model is overdispersed

# Run a negative binomial GLMM:
small_trees_model_negbin <- 
  lme4::glmer.nb(total_number_living_small_trees ~ mstrlvl + (1|Park),
                 data=ash_by_transect) # I got a singular fit
summary(small_trees_model_negbin)

# Try running a negative binomial GLMM without the random effect:
small_trees_model_without_Park_negbin <- 
  MASS::glm.nb(total_number_living_small_trees ~ mstrlvl, data=ash_by_transect)
summary(small_trees_model_without_Park_negbin)

# Use the DHARMa package to assess model fit:
small_trees_model_without_Park_negbin_sim_resid <- simulateResiduals(
  fittedModel = small_trees_model_without_Park_negbin, plot = T)
plotResiduals(small_trees_model_without_Park_negbin_sim_resid,
              form = ash_by_transect$mstrlvl)

# Test for overdispersion:
testDispersion(small_trees_model_without_Park_negbin_sim_resid)
# The null hypothesis of equidispersion is not rejected.

Anova(small_trees_model_without_Park_negbin, type="III")
emmeans(small_trees_model_without_Park_negbin, pairwise ~ mstrlvl)

anova(small_trees_model_negbin, small_trees_model_without_Park_negbin) # The negative
# binomial model with and without the random intercepts appear to have very similar
# AIC values.

# Note: I think that there may have been parks where no small trees were found.
# This would likely cause problems for estimating the variance of the 
# random intercepts.

# Basal area of understory and canopy ash trees model ##########################

# Make another column for total basal area of living ash trees >=2.5 cm DBH
ash_by_transect$mean_basal_area_living_ash_trees_m_squared_per_ha <-
  ash_by_transect$mean_basal_area_living_small_trees_m_squared_per_ha +
  ash_by_transect$mean_basal_area_living_big_trees_m_squared_per_ha
# Note: the vast majority of basal area is contributed by the ash trees 
# that are 2.5-10 cm DBH.

# Summarize the data:
summary(ash_by_transect$mean_basal_area_living_ash_trees_m_squared_per_ha)

# Graph the data:
ggplot(data=ash_by_transect, 
       aes(x=mstrlvl, y=mean_basal_area_living_ash_trees_m_squared_per_ha)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(aes(color=Park), height=0, width=0.1, alpha=0.5) +
  theme_classic() +
  xlab("Hydroclass") +
  ylab("Mean basal area of \nliving ash trees (m^2 / ha)")

# I don't know what the appropriate model is for this data. It is continuous 
# data and yet there are MANY zeros (well, the same number of zeros as the 
# count of ash trees) and a wide variability in the magnitude. Honestly I
# don't know if this variable is fit for statistical analyses.

# Make a figure using ggpubr ###################################################

# Make a graph of total seedlings (short and tall)
seedlings_graph <- ggplot(data=ash_by_transect, aes(x=mstrlvl, y=mean_density_seedlings)) +
    geom_boxplot(outlier.colour = "white") +
    geom_jitter(height=0, width=0.05, alpha=0.5) +
    theme_bw() +
    xlab("Hydroclass") +
    ylab("Mean density of \nseedlings (stems/m^2)") +
    ylim(c(0,7))+
    theme(plot.margin = unit(c(0.5,0.2,0.2,0.6), "cm"))
seedlings_graph

saplings_graph <- ggplot(data=ash_by_transect, aes(x=mstrlvl, y=mean_density_saplings_stems_per_m_squared)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(height=0, width=0.05, alpha=0.5) +
  theme_bw() +
  xlab("Hydroclass") +
  ylab("Mean density of \nsaplings (stems/m^2))")+
  ylim(c(0,0.55))+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.6), "cm"))
saplings_graph

small_trees_graph <- ggplot(data=ash_by_transect, aes(x=mstrlvl, y=mean_density_living_small_trees_stems_per_ha)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(height=0, width=0.05, alpha=0.5) +
  theme_bw() +
  xlab("Hydroclass") +
  ylab("Mean density living \n small trees (stems / ha)") +
  ylim(c(0,1200))+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.6), "cm"))
small_trees_graph

BA_graph <- ggplot(data=ash_by_transect, 
                   aes(x=mstrlvl, y=mean_basal_area_living_ash_trees_m_squared_per_ha)) +
  geom_boxplot(outlier.colour = "white") +
  geom_jitter(height=0, width=0.05, alpha=0.5) +
  theme_bw() +
  xlab("Hydroclass") +
  ylab("Mean basal area of \nliving ash trees (m^2 / ha)")+
  theme(plot.margin = unit(c(0.5,0.2,0.2,0.6), "cm"))
BA_graph

ggarrange(seedlings_graph + rremove("xlab"), saplings_graph + rremove("xlab"), 
          small_trees_graph, BA_graph,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2, align = "hv")

#  Extra graphs for Presentation Skills #######################################

ggplot(data=ash_by_transect, aes(x=mstrlvl, y=mean_density_seedlings)) +
  geom_boxplot(outlier.colour = "white", color=c("dodgerblue3", "forestgreen", "orange")) +
  theme_classic() +
  xlab("Hydrology class") +
  ylab("Mean density of \nseedlings (stems/m^2)") +
  ylim(c(0,7))+
  theme(plot.margin = unit(c(0.5,0.2,0.2,0.6), "cm"))

ggplot(data=ash_by_transect, aes(x=mstrlvl, y=mean_density_saplings_stems_per_m_squared)) +
  geom_boxplot(outlier.colour = "white", color=c("dodgerblue3", "forestgreen", "orange")) +
  theme_classic() +
  xlab("Hydrology class") +
  ylab("Mean density of \nsaplings (stems/m^2)")+
  ylim(c(0,0.2))+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.6), "cm"))

ggplot(data=ash_by_transect, aes(x=mstrlvl, y=mean_density_living_small_trees_stems_per_ha)) +
  geom_boxplot(outlier.colour = "white", color=c("dodgerblue3", "forestgreen", "orange")) +
  theme_classic() +
  xlab("Hydrology class") +
  ylab("Mean density living \n small trees (stems/ha)") +
  ylim(c(0,1200))+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.6), "cm"))
  
  
  
  
  
  
  
  
  