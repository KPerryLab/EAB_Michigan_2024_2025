# Aaron Tayal
# Dec 27, 2024
# Statistics for ash signs and symptoms of EAB

library(dplyr)
library(ggplot2)
library(ggpubr)

trees0 <- read.csv("Cleaned_data/individual_trees.csv")

hist(as.numeric(trees0$distance_to_center_meters_simple)) # these trees are all
# within the plot. Most are small trees and are thus within the subplot

# Plot a histogram of diameters:
trees0$diameter_at_137_cm_in_cm <- as.numeric(trees0$diameter_at_137_cm_in_cm)
hist(trees0$diameter_at_137_cm_in_cm, breaks=50)

# Now exclude the one tree that is about 50 cm in diameter. It is a standing
# dead canopy tree and so it won't help answer questions about the ash 
# regeneration's signs and symptoms of EAB attack
trees <- trees0 %>% filter(diameter_at_137_cm_in_cm < 40)

hist(trees$diameter_at_137_cm_in_cm, breaks=20)

# Predictor variable: diameter at breast height (137 cm)
# To make things easier, I'll shorten the name to "DBH"
trees$DBH <- trees$diameter_at_137_cm_in_cm

# The response variables:
# Canopy condition (1-5)
# EAB exit holes (y/n)
# woodpecker marks (y/n)
# bark splitting (y/n)
# epicormic sprouts (y/n)
# basal sprouts (y/n)

# Canopy condition ############################################################
# Plot the canopy condition against DBH
ggplot(data=trees, aes(x=DBH, y=canopy_condition_1_5)) +
  geom_jitter(aes(color=Park), alpha=0.5, height=0.03, width=0)

sum(is.na(trees$canopy_condition_1_5)) # seems like we forgot to enter canopy condition for 1 tree
trees_without_missing_canopy_condition <- trees %>% 
  filter(!is.na(canopy_condition_1_5))

# How many trees were found of each canopy condition?
sum(trees_without_missing_canopy_condition$canopy_condition_1_5 == 1) # 220 with canopy condition 1
sum(trees_without_missing_canopy_condition$canopy_condition_1_5 == 2) # 31
sum(trees_without_missing_canopy_condition$canopy_condition_1_5 == 3) # 12
sum(trees_without_missing_canopy_condition$canopy_condition_1_5 == 4) # 7
sum(trees_without_missing_canopy_condition$canopy_condition_1_5 == 5) # 50 

# Create a variable called ash tree death, which is only equal to 1 if canopy
# condition is equal to 5
trees$ash_tree_death <- ifelse(trees$canopy_condition_1_5 == 5, 1, 0)
sum(trees$ash_tree_death == 1, na.rm=TRUE) # 50

# Create a variable called ash tree decline, which is only equal to 1 if canopy
# condition is 2,3,4, or 5
trees$ash_tree_decline <- ifelse(trees$canopy_condition_1_5 > 1, 1, 0)
sum(trees$ash_tree_decline == 1, na.rm=TRUE) # 100

# Convert the response variables to 0s (absences) and 1s (presences) ###########
trees$EAB_exit_holes_0_1 <- 
  as.integer(as.factor(trees$EAB_exit_holes_y_n)) - 1
trees$woodpecker_marks_0_1 <- 
  as.integer(as.factor(trees$woodpecker_marks_y_n)) - 1
trees$ash_bark_splitting_0_1 <- 
  as.integer(as.factor(trees$ash_bark_splitting_y_n)) - 1
trees$epicormic_sprouts_0_1 <- 
  as.integer(as.factor(trees$epicormic_sprouts_y_n)) - 1
trees$basal_sprouts_0_1 <-
  as.integer(as.factor(trees$basal_sprouts_y_n)) - 2 
# For basal sprouts, two trees have nothing written for it. Thus, 
# there ends up being an additional factor, and I need to subtract by 2
trees_without_missing_basal_sprout_data <- 
  trees %>% filter(basal_sprouts_y_n != "")

# EAB exit holes (descriptive) #################################################
# Plot presence/absence of EAB exit holes against DBH
ggplot(data=trees, aes(x=DBH, y=EAB_exit_holes_0_1)) +
  geom_jitter(alpha=0.5, height=0.03, width=0) +
  ylab("Presence of EAB\nexit holes") + xlab("Diameter at breast height (cm)") +
  theme_bw()
# I was confused, because I know we found EAB exit holes at Pontiac, but then I
# realized that the trees were filtered so that the small trees had to be within
# the subplot.

table(trees$EAB_exit_holes_0_1)
# There are only 8/321 recorded trees with EAB exit holes that were 
# spotted. This seems like too few to create a meaningful model

# Woodpecker marks (descriptive) ###############################################
# Plot presence/absence of woodpecker marks against DBH
ggplot(data=trees, aes(x=DBH, y=woodpecker_marks_0_1)) +
  geom_jitter(aes(color=Park), alpha=0.5, height=0.03, width=0)
table(trees$woodpecker_marks_0_1)
# There were 40/321 trees where woodpecker marks were seen

# Bark splitting (descriptive) #################################################
# Plot presence/absence of bark splitting against DBH
ggplot(data=trees, aes(x=DBH, y=ash_bark_splitting_0_1)) +
  geom_jitter(aes(color=Park), alpha=0.5, height=0.03, width=0)
table(trees$ash_bark_splitting_0_1)
# There were 179/321 trees where bark splitting was found

# Epicormic sprouts (descriptive) ##############################################
# Plot presence/absence of epicormic sprouts against DBH
ggplot(data=trees, aes(x=DBH, y=epicormic_sprouts_0_1)) +
  geom_jitter(aes(color=Park), alpha=0.5, height=0.03, width=0)
table(trees$epicormic_sprouts_0_1)
# There were 116/321 trees where epicormic sprouts were found

# Basal sprouts (descriptive) ##################################################
# Plot presence/absence of basal sprouts against DBH
ggplot(data=trees_without_missing_basal_sprout_data, 
       aes(x=DBH, y=basal_sprouts_0_1)) +
  geom_jitter(aes(color=Park), alpha=0.5, height=0.03, width=0)
table(trees$basal_sprouts_0_1)
# There were 46/319 trees where basal sprouts were found


# Subsetting for the models ####################################################
# How many trees are found at each Plot?
# For the binomial generalized linear mixed-effects models, I plan to only 
# consider trees within Plots where more than 10 trees were found.
ash_by_plot <- read.csv("Cleaned_data/Ash_by_plot.csv")
ash_by_plot$number_trees <- 
  ash_by_plot$number_small_trees + ash_by_plot$number_big_trees
sum(ash_by_plot$number_trees > 10, na.rm=TRUE) # There are 9 plots where total 
# number of trees is greater than 10
plots_for_sign_symptom_models <- ash_by_plot[which(ash_by_plot$number_trees > 10), 
                                             "center_tree_number"]
# Subset the tree data using the condition: plot %in% plots_for_sign_symptom_models:
trees_subset_for_models <- trees_without_missing_basal_sprout_data %>% 
  filter(center_tree_number %in% plots_for_sign_symptom_models) 
# This sub-setting reduces the sample size to 274 trees
table(trees_subset_for_models$Transect)
table(trees_subset_for_models$Park)
table(trees_subset_for_models[, c("mstrlvl", "Transect", "Park")])
# Note: the ~50 cm DBH tree is not in a plot that is included here, so no need 
# to mention it in the paper

# Models #######################################################################

# Code is from "ENTMLGY 6707 Entomological 
# Techniques and Data Analysis R Activity 9: Generalized Linear (Mixed-Effects) 
# Models" taught by Dr. Perry and Dr. Ward

# Predictor variable: diameter at breast height (137 cm)

# The response variables:
# EAB exit holes (DECIDED NOT TO RUN THIS MODEL - NOT ENOUGH 1s)
# woodpecker marks (0 or 1)
# bark splitting (0 or 1)
# epicormic sprouts (0 or 1)
# basal sprouts (0 or 1)

# Random intercepts: Plot (here the variable is center_tree_number)
trees_subset_for_models$center_tree_number <- 
  as.factor(trees_subset_for_models$center_tree_number)

# Woodpecker marks model #######################################################

# Plot the data:
ggplot(data=trees_subset_for_models, aes(x=DBH, y=woodpecker_marks_0_1)) +
  geom_jitter(aes(color=center_tree_number), alpha=0.5, height=0.03, width=0)

# Fit the model:

library(lme4)
fit_woodpecker_marks <- 
  lme4::glmer(woodpecker_marks_0_1 ~ DBH + (1|center_tree_number), 
                                    data=trees_subset_for_models,
                                    family=binomial(link="logit"))

summary(fit_woodpecker_marks)

# Plot the regression line. First I need to create some new predictor data.
# This includes the whole range of DBHs and all levels of the random effect, 
# center_tree_number
new_data <- data.frame(DBH = rep(seq(2.5, 12.49, 0.01), 9))
new_data$center_tree_number = as.factor(c(rep(18, 1000),rep(22,1000),rep(24,1000),
                                          rep(37,1000),rep(38,1000),rep(39,1000),
                                          rep(63,1000),rep(72,1000),rep(78,1000)))
new_data$predicted_woodpecker_marks <- 
  predict(fit_woodpecker_marks, newdata=new_data, type="response")

ggplot(data=trees_subset_for_models, aes(x=DBH, y=woodpecker_marks_0_1)) +
  geom_jitter(aes(color=center_tree_number), alpha=0.5, height=0.03, width=0) +
  geom_line(data=new_data, aes(x=DBH, y=predicted_woodpecker_marks, 
                               color=center_tree_number))

# Make a graph that shows the overall trend:
new_data2 <- data.frame(DBH = seq(2.5, 12.49, 0.01))
new_data2$predicted_woodpecker_marks <- 
  predict(fit_woodpecker_marks, newdata=new_data2, type="response", re.form=NA)
# Note: re.form=NA tells R to avoid trying to plot the line for each grouping
# variable and to just plot the global fit
ggplot(data=trees_subset_for_models, aes(x=DBH, y=woodpecker_marks_0_1)) +
  geom_jitter(alpha=0.5, height=0.02, width=0) +
  geom_line(data=new_data2, aes(x=DBH, y=predicted_woodpecker_marks)) +
  theme_bw() + xlab("Diameter at breast height (cm)") +
  ylab("Probability of woodpecker \npredation marks")

# Try out a method to test the accuracy of this model:
# Bin the data points by DBH, then compute the mean and standard deviation
# of each bin, and plot this to see if the binomial GLMM line runs through
# the binned means

trees_subset_for_models_1 <- trees_subset_for_models %>% 
  mutate(new_bin = cut(DBH, breaks = seq(1.5, 12.5, 1)))
table(trees_subset_for_models_1$new_bin) # Seems like there are no trees 
# in this subsetted dataset that are 11.5-12.5 cm DBH.
bins_for_plot <- trees_subset_for_models_1 %>% group_by(new_bin) %>%
  summarise(mean_woodpecker_marks = mean(woodpecker_marks_0_1), 
            SDs_woodpecker_marks = sd(woodpecker_marks_0_1),
            mean_bark_splitting = mean(ash_bark_splitting_0_1),
            SDs_bark_splitting = sd(ash_bark_splitting_0_1),
            mean_epicormics = mean(epicormic_sprouts_0_1),
            SDs_epicormics = sd(epicormic_sprouts_0_1),
            mean_basal_sprouts = mean(basal_sprouts_0_1),
            SDs_basal_sprouts = sd(basal_sprouts_0_1),
            mean_ash_death = mean(ash_tree_death),
            SDs_ash_death = sd(ash_tree_death),
            mean_ash_decline = mean(ash_tree_decline),
            SDs_ash_decline = sd(ash_tree_decline))
bins_for_plot$DBH <- seq(2, 11, 1)

woodpecker_fig <- ggplot(data=trees_subset_for_models, 
                         mapping=aes(x=DBH, y=woodpecker_marks_0_1))+
  geom_jitter(alpha=0.5, height=0.02, width=0) +
  geom_point(data=bins_for_plot, aes(x=DBH, y=mean_woodpecker_marks), col="red", 
             size=2, shape=17)+
  geom_line(data=new_data2, aes(x=DBH, y=predicted_woodpecker_marks), linewidth=1) +
  theme_bw() + xlab("Diameter at breast height (cm)") +
  ylab("Probability of woodpecker \npredation marks")
woodpecker_fig

# Bark splitting model #########################################################

# Plot the data:
ggplot(data=trees_subset_for_models, aes(x=DBH, y=ash_bark_splitting_0_1)) +
  geom_jitter(aes(color=center_tree_number), alpha=0.5, height=0.03, width=0)

# Fit the model:

fit_bark_splitting <- 
  lme4::glmer(ash_bark_splitting_0_1 ~ DBH + (1|center_tree_number), 
              data=trees_subset_for_models,
              family=binomial(link="logit"))

summary(fit_bark_splitting)

# Plot the regression line. 
new_data$predicted_bark_splitting <- 
  predict(fit_bark_splitting, newdata=new_data, type="response")

ggplot(data=trees_subset_for_models, aes(x=DBH, y=ash_bark_splitting_0_1)) +
  geom_jitter(aes(color=center_tree_number), alpha=0.5, height=0.03, width=0) +
  geom_line(data=new_data, aes(x=DBH, y=predicted_bark_splitting, 
                               color=center_tree_number))

# Make a graph that shows the overall trend:
new_data2$predicted_bark_splitting <- 
  predict(fit_bark_splitting, newdata=new_data2, type="response", re.form=NA)
# Note: re.form=NA tells R to avoid trying to plot the line for each grouping
# variable and to just plot the global fit
ggplot(data=trees_subset_for_models, aes(x=DBH, y=ash_bark_splitting_0_1)) +
  geom_jitter(alpha=0.5, height=0.02, width=0) +
  geom_line(data=new_data2, aes(x=DBH, y=predicted_bark_splitting)) +
  theme_bw() + xlab("Diameter at breast height (cm)") +
  ylab("Probability of bark splitting")

# Test accuracy of the model visually:
bark_splitting_fig <- ggplot(data=trees_subset_for_models, mapping=aes(x=DBH, y=ash_bark_splitting_0_1))+
  geom_jitter(alpha=0.5, height=0.02, width=0) + 
  geom_point(data=bins_for_plot, aes(x=DBH, y=mean_bark_splitting), col="red", 
             size = 2, shape = 17)+
  geom_line(data=new_data2, aes(x=DBH, y=predicted_bark_splitting), linewidth=1) +
  theme_bw() + xlab("Diameter at breast height (cm)") +
  ylab("Probability of \nbark splitting")
bark_splitting_fig

# Epicormics model #############################################################

# Plot the data:
ggplot(data=trees_subset_for_models, aes(x=DBH, y=epicormic_sprouts_0_1)) +
  geom_jitter(aes(color=center_tree_number), alpha=0.5, height=0.03, width=0)

fit_epicormic_sprouts <- 
  lme4::glmer(epicormic_sprouts_0_1 ~ DBH + (1|center_tree_number), 
              data=trees_subset_for_models,
              family=binomial(link="logit"))

summary(fit_epicormic_sprouts)

# Plot the regression line. 
new_data$predicted_epicormic_sprouts <- 
  predict(fit_epicormic_sprouts, newdata=new_data, type="response")

ggplot(data=trees_subset_for_models, aes(x=DBH, y=epicormic_sprouts_0_1)) +
  geom_jitter(aes(color=center_tree_number), alpha=0.5, height=0.03, width=0) +
  geom_line(data=new_data, aes(x=DBH, y=predicted_epicormic_sprouts, 
                               color=center_tree_number))

# Make a graph that shows the overall trend:
new_data2$predicted_epicormic_sprouts <- 
  predict(fit_epicormic_sprouts, newdata=new_data2, type="response", re.form=NA)
# Note: re.form=NA tells R to avoid trying to plot the line for each grouping
# variable and to just plot the global fit
ggplot(data=trees_subset_for_models, aes(x=DBH, y=epicormic_sprouts_0_1)) +
  geom_jitter(alpha=0.5, height=0.02, width=0) +
  geom_line(data=new_data2, aes(x=DBH, y=predicted_epicormic_sprouts)) +
  theme_bw() + xlab("Diameter at breast height (cm)") +
  ylab("Probability of \nepicormic sprouts")

# Test accuracy of the model visually:
epicormics_fig <- ggplot(data=trees_subset_for_models, mapping=aes(x=DBH, y=epicormic_sprouts_0_1))+
  geom_jitter(alpha=0.5, height=0.02, width=0) +
  geom_point(data=bins_for_plot, aes(x=DBH, y=mean_epicormics), col="red", 
             size=2, shape=17)+
  geom_line(data=new_data2, aes(x=DBH, y=predicted_epicormic_sprouts), linewidth=1) +
  theme_bw() + xlab("Diameter at breast height (cm)") +
  ylab("Probability of \nepicormic sprouts")
epicormics_fig

# Basal sprouts model ##########################################################

# Plot the data:
ggplot(data=trees_subset_for_models, aes(x=DBH, y=basal_sprouts_0_1)) +
  geom_jitter(aes(color=center_tree_number), alpha=0.5, height=0.03, width=0)

fit_basal_sprouts <- 
  lme4::glmer(basal_sprouts_0_1 ~ DBH + (1|center_tree_number), 
              data=trees_subset_for_models,
              family=binomial(link="logit"))

summary(fit_basal_sprouts)

# Plot the regression line. 
new_data$predicted_basal_sprouts <- 
  predict(fit_basal_sprouts, newdata=new_data, type="response")

ggplot(data=trees_subset_for_models, aes(x=DBH, y=basal_sprouts_0_1)) +
  geom_jitter(aes(color=center_tree_number), alpha=0.5, height=0.03, width=0) +
  geom_line(data=new_data, aes(x=DBH, y=predicted_basal_sprouts, 
                               color=center_tree_number))

# Make a graph that shows the overall trend:
new_data2$predicted_basal_sprouts <- 
  predict(fit_basal_sprouts, newdata=new_data2, type="response", re.form=NA)
# Note: re.form=NA tells R to avoid trying to plot the line for each grouping
# variable and to just plot the global fit
ggplot(data=trees_subset_for_models, aes(x=DBH, y=basal_sprouts_0_1)) +
  geom_jitter(alpha=0.5, height=0.02, width=0) +
  geom_line(data=new_data2, aes(x=DBH, y=predicted_basal_sprouts)) +
  theme_bw() + xlab("Diameter at breast height (cm)") +
  ylab("Probability of basal sprouts")

# Test accuracy of the model visually:
basal_sprouts_fig <- ggplot(data=trees_subset_for_models, mapping=aes(x=DBH, y=basal_sprouts_0_1))+
  geom_jitter(alpha=0.5, height=0.02, width=0) + 
  geom_point(data=bins_for_plot, aes(x=DBH, y=mean_basal_sprouts), col="red", 
             size=2, shape=17)+
  geom_line(data=new_data2, aes(x=DBH, y=predicted_basal_sprouts), linewidth=1) +
  theme_bw() + xlab("Diameter at breast height (cm)") +
  ylab("Probability of \nbasal sprouts")
basal_sprouts_fig

# Ash tree death model #########################################################

# Plot the data:
ggplot(data=trees_subset_for_models, aes(x=DBH, y=ash_tree_death)) +
  geom_jitter(aes(color=center_tree_number), alpha=0.5, height=0.03, width=0)

fit_death <- 
  lme4::glmer(ash_tree_death ~ DBH + (1|center_tree_number), 
              data=trees_subset_for_models,
              family=binomial(link="logit"))

summary(fit_death)

# Plot the regression line. 
new_data$predicted_death <- 
  predict(fit_death, newdata=new_data, type="response")

ggplot(data=trees_subset_for_models, aes(x=DBH, y=ash_tree_death)) +
  geom_jitter(aes(color=center_tree_number), alpha=0.5, height=0.03, width=0) +
  geom_line(data=new_data, aes(x=DBH, y=predicted_death, 
                               color=center_tree_number))

# Make a graph that shows the overall trend:
new_data2$predicted_death <- 
  predict(fit_death, newdata=new_data2, type="response", re.form=NA)
# Note: re.form=NA tells R to avoid trying to plot the line for each grouping
# variable and to just plot the global fit
ggplot(data=trees_subset_for_models, aes(x=DBH, y=ash_tree_death)) +
  geom_jitter(alpha=0.5, height=0.02, width=0) +
  geom_line(data=new_data2, aes(x=DBH, y=predicted_death)) +
  theme_bw() + xlab("Diameter at breast height (cm)") +
  ylab("Probability of ash \ntree being dead")

# Test accuracy of the model visually:
ash_death_fig <- ggplot(data=trees_subset_for_models, mapping=aes(x=DBH, y=ash_tree_death))+
  geom_jitter(alpha=0.5, height=0.02, width=0) + 
  geom_point(data=bins_for_plot, aes(x=DBH, y=mean_ash_death), col="red", 
             size=2, shape=17)+
  geom_line(data=new_data2, aes(x=DBH, y=predicted_death), linewidth=1) +
  theme_bw() + xlab("Diameter at breast height (cm)") +
  ylab("Probability of ash \ntree being dead")
ash_death_fig

# Ash tree decline model #########################################################

# Plot the data:
ggplot(data=trees_subset_for_models, aes(x=DBH, y=ash_tree_decline)) +
  geom_jitter(aes(color=center_tree_number), alpha=0.5, height=0.03, width=0)

fit_decline <- 
  lme4::glmer(ash_tree_decline ~ DBH + (1|center_tree_number), 
              data=trees_subset_for_models,
              family=binomial(link="logit"))

summary(fit_decline)

# Plot the regression line. 
new_data$predicted_decline <- 
  predict(fit_decline, newdata=new_data, type="response")

ggplot(data=trees_subset_for_models, aes(x=DBH, y=ash_tree_decline)) +
  geom_jitter(aes(color=center_tree_number), alpha=0.5, height=0.03, width=0) +
  geom_line(data=new_data, aes(x=DBH, y=predicted_decline, 
                               color=center_tree_number))

# Make a graph that shows the overall trend:
new_data2$predicted_decline <- 
  predict(fit_decline, newdata=new_data2, type="response", re.form=NA)
# Note: re.form=NA tells R to avoid trying to plot the line for each grouping
# variable and to just plot the global fit
ggplot(data=trees_subset_for_models, aes(x=DBH, y=ash_tree_decline)) +
  geom_jitter(alpha=0.5, height=0.02, width=0) +
  geom_line(data=new_data2, aes(x=DBH, y=predicted_decline)) +
  theme_bw() + xlab("Diameter at breast height (cm)") +
  ylab("Probability of ash tree \nshowing canopy decline")

ash_decline_fig <- ggplot(data=trees_subset_for_models, mapping=aes(x=DBH, y=ash_tree_decline))+
  geom_jitter(alpha=0.5, height=0.02, width=0) + 
  geom_point(data=bins_for_plot, aes(x=DBH, y=mean_ash_decline), col="red", 
             size=2, shape=17)+
  geom_line(data=new_data2, aes(x=DBH, y=predicted_decline), linewidth=1) +
  theme_bw() + xlab("Diameter at breast height (cm)") +
  ylab("Probability of ash tree \nshowing canopy decline")
ash_decline_fig

# Create a combined figure using ggpubr #######################################

ggarrange(woodpecker_fig + rremove("xlab"), bark_splitting_fig + rremove("xlab"), 
          epicormics_fig + rremove("xlab"), basal_sprouts_fig + rremove("xlab"),
          ash_death_fig, ash_decline_fig,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3)
# When saving the image, use Height = 700, Width=600





