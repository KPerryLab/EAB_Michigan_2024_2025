# Aaron Tayal
# Dec 27, 2024
# Statistics for ash signs and symptoms of EAB

library(dplyr)
library(ggplot2)

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

# Predictor variable: diameter at breast height (137 cm)
# To make things easier, I'll shorten the name to "DBH"
trees$DBH <- trees$diameter_at_137_cm_in_cm

# The response variables:
# EAB exit holes (y/n)
# woodpecker marks (y/n)
# bark splitting (y/n)
# epicormic sprouts (y/n)
# basal sprouts (y/n)

# Possible nuisance variables: park, transect, plot

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

# EAB exit holes ##############################################################
# Plot presence/absence of EAB exit holes against DBH
ggplot(data=trees, aes(x=DBH, y=EAB_exit_holes_0_1)) +
  geom_jitter(alpha=0.5, height=0.03, width=0)
table(trees$EAB_exit_holes_0_1)
# There are only 8/321 recorded trees with EAB exit holes that were 
# spotted. This seems like too few to create a meaningful model

# Woodpecker marks ############################################################
# Plot presence/absence of woodpecker marks against DBH
ggplot(data=trees, aes(x=DBH, y=woodpecker_marks_0_1)) +
  geom_jitter(aes(color=Park), alpha=0.5, height=0.03, width=0)
table(trees$woodpecker_marks_0_1)
# There were 40/321 trees where woodpecker marks were seen

# Run the generalized linear model (code is from "ENTMLGY 6707 Entomological 
# Techniques and Data Analysis R Activity 9: Generalized Linear (Mixed-Effects) 
# Models" taught by Dr. Perry and Dr. Ward
fit_woodpecker_marks <- glm(woodpecker_marks_0_1~DBH, data=trees,
                           family=binomial(link="logit"))
summary(fit_woodpecker_marks)
new_data <- data.frame(DBH = seq(2.5, 12.5, 0.01))
new_data$predicted_woodpecker_marks <- 
  predict(fit_woodpecker_marks, newdata=new_data, type="response")

ggplot(data=trees, mapping=aes(x=DBH, y=woodpecker_marks_0_1))+
  geom_point(alpha=0.5)+theme_classic()+
  geom_line(data=new_data, aes(x=DBH, y=predicted_woodpecker_marks), 
            color="red", linewidth=1)


# Bark splitting ###############################################################
# Plot presence/absence of bark splitting against DBH
ggplot(data=trees, aes(x=DBH, y=ash_bark_splitting_0_1)) +
  geom_jitter(aes(color=Park), alpha=0.5, height=0.03, width=0)
table(trees$ash_bark_splitting_0_1)
# There were 179/321 trees where bark splitting was found

fit_bark_splitting <- glm(ash_bark_splitting_0_1 ~ DBH, data=trees,
                            family=binomial(link="logit"))
summary(fit_bark_splitting)

new_data$predicted_bark_splitting <- 
  predict(fit_bark_splitting, newdata=new_data, type="response")

ggplot(data=trees, mapping=aes(x=DBH, y=ash_bark_splitting_0_1))+
  geom_point(alpha=0.5)+theme_classic()+
  geom_line(data=new_data, aes(x=DBH, y=predicted_bark_splitting), 
            color="red", linewidth=1)

# Epicormic sprouts ############################################################
# Plot presence/absence of epicormic sprouts against DBH
ggplot(data=trees, aes(x=DBH, y=epicormic_sprouts_0_1)) +
  geom_jitter(aes(color=Park), alpha=0.5, height=0.03, width=0)
table(trees$epicormic_sprouts_0_1)
# There were 116/321 trees where epicormic sprouts were found

fit_epicormic_sprouts <- glm(epicormic_sprouts_0_1 ~ DBH, data=trees,
                          family=binomial(link="logit"))
summary(fit_epicormic_sprouts)

new_data$predicted_epicormic_sprouts <- 
  predict(fit_epicormic_sprouts, newdata = new_data, type="response")

ggplot(data=trees, mapping=aes(x=DBH, y=epicormic_sprouts_0_1))+
  geom_point(alpha=0.5)+theme_classic()+
  geom_line(data=new_data, aes(x=DBH, y=predicted_epicormic_sprouts), 
            color="red", linewidth=1)

# Basal sprouts ###############################################################
# Plot presence/absence of basal sprouts against DBH
ggplot(data=trees_without_missing_basal_sprout_data, 
       aes(x=DBH, y=basal_sprouts_0_1)) +
  geom_jitter(aes(color=Park), alpha=0.5, height=0.03, width=0)
table(trees$basal_sprouts_0_1)
# There were 46/319 trees where basal sprouts were found

fit_basal_sprouts <- glm(basal_sprouts_0_1 ~ DBH, 
                         data=trees_without_missing_basal_sprout_data,
                         family=binomial(link="logit"))
summary(fit_basal_sprouts)

new_data$predicted_basal_sprouts <- 
  predict(fit_basal_sprouts, newdata = new_data, type="response")

ggplot(data=trees_without_missing_basal_sprout_data, 
       mapping=aes(x=DBH, y=basal_sprouts_0_1))+
  geom_point(alpha=0.5)+theme_classic()+
  geom_line(data=new_data, aes(x=DBH, y=predicted_basal_sprouts), 
            color="red", linewidth=1)
