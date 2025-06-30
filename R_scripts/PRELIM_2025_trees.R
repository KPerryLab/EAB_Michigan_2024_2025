# 6/23/2025
# Aaron Tayal
# Preliminary code for analysing the tree survey data for 2025 EAB Michigan 
# Project. Here are some goals:
# 1. See if the counts of ash trees that were done in 2025 match up to the data
# collected in 2024

library(dplyr)
library(ggplot2)

dat_2025 <- read.csv("Raw_data/PRELIM_EAB_Michigan_2025_trees.csv")
dat_2025$DBH <- dat_2025$diameter_at_137_cm_in_cm
summary(dat_2025$DBH)

# Look at the distribution of tree diameters:
ggplot(data=dat_2025, aes(x=DBH)) + geom_histogram(breaks=seq(0,190,10))

# Look at the distribution of tree diameters (specifically smaller diameter range)
ggplot(data=dat_2025, aes(x=DBH)) + geom_histogram(breaks=seq(0,50,1))

# Make species and living/dead a factor:
dat_2025$species <- as.factor(dat_2025$species)
table(dat_2025$species)
dat_2025$living_or_dead <- as.factor(dat_2025$living_or_dead)
table(dat_2025$living_or_dead)

# Investigate the counts of ash trees: #########################################

# Make a subset of just the ash trees:
ash <- dat_2025 %>% filter(species == "Fraxinus section Melioides" | 
                            species == "Fraxinus sp." | 
                            species == "FRNI"| species == "FRPE")

ggplot(data=ash, aes(x=DBH)) + geom_histogram(breaks=seq(0,50,1))

# Make a subset of just small ash trees < 10 cm DBH:
ash_small_trees <- ash %>% filter(diameter_at_137_cm_in_cm >= 2.5) %>%
  filter(diameter_at_137_cm_in_cm < 10)

# Summarize by plot and separate into living and dead:
ash_small_trees_by_plot <- ash_small_trees %>% group_by(center_tree_number) %>%
  summarise(number_small_trees_2025 = n(),
            basal_area_small_trees_in_m_squared_2025 = 
              sum(pi * (DBH / 2) ^ 2) / 10000,
            number_living_small_trees_2025 = sum(living_or_dead == "living"),
            basal_area_living_small_trees_in_m_squared_2025 = 
              sum(pi * ((ifelse(living_or_dead == "living", 
                                diameter_at_137_cm_in_cm, 0)) / 2) ^ 2) / 10000,
            
            number_healthy_small_trees_2025 = sum(canopy_condition_if_ash == 1, na.rm = TRUE),
            number_declining_small_trees_2025 = sum(canopy_condition_if_ash %in% c(2,3,4), na.rm = TRUE),
            number_dead_small_trees_2025 = sum(canopy_condition_if_ash == 5, na.rm = TRUE)
  )

# Make a subset of just big ash trees < 10 cm DBH:
ash_big_trees <- ash %>% filter(diameter_at_137_cm_in_cm > 10)

# Summarize by plot and separate into living and dead:
ash_big_trees_by_plot <- ash_big_trees %>% group_by(center_tree_number) %>%
  summarise(number_big_trees_2025 = n(),
            basal_area_big_trees_in_m_squared_2025 = 
              sum(pi * (DBH / 2) ^ 2) / 10000,
            number_living_big_trees_2025 = sum(living_or_dead == "living"),
            basal_area_living_big_trees_in_m_squared_2025 = 
              sum(pi * ((ifelse(living_or_dead == "living", 
                                diameter_at_137_cm_in_cm, 0)) / 2) ^ 2) / 10000,
            
            number_healthy_big_trees_2025 = sum(canopy_condition_if_ash == 1, na.rm = TRUE),
            number_declining_big_trees_2025 = sum(canopy_condition_if_ash %in% c(2,3,4), na.rm = TRUE),
            number_dead_big_trees_2025 = sum(canopy_condition_if_ash == 5, na.rm = TRUE)
  )

# I am trying to compare the stem counts and basal areas of ash between 2024 and
# 2025 to see if there are any major changes. Preliminary comparison suggests 
# that the counts are extremely similar. However, some of the basal areas differ.
# This could reflect growth of ash trees into the size class, growth within
# the size class, death and falling over of trees, and missing a few trees
# during surveying. But the data generally matches up. Another thing I could do
# is look at histograms.

# Investigate the stem counts of different tree genera: ########################

# Big trees >=12.5 cm ##########################################################

# Subset the data to just the trees bigger than 12.5 cm DBH, which are
# living: (Note: I assume the 2005 data was just recorded for living trees,
# but I need to check up on this)
dat_2025_living_big_trees <- dat_2025 %>% filter(DBH >= 12.5) %>% 
  filter(living_or_dead == "living")

table(dat_2025_living_big_trees$species)

living_big_trees_2025_by_plot <- dat_2025_living_big_trees %>% group_by(center_tree_number) %>% 
  summarise(num_trees = n(),
            living_BA = sum(3.141592654 * ( (DBH / 200) ^ 2 ), na.rm = T),
            Acer = sum(grepl("Acer", species, fixed = TRUE) | species == "asac"),
            Betula = sum(grepl("Betula", species, fixed = TRUE)),
            Carpinus = sum(grepl("Carpinus", species, fixed = TRUE)),
            Carya = sum(grepl("Carya", species, fixed = TRUE)),
            Fagus = sum(grepl("Fagus", species, fixed = TRUE)),
            Fraxinus = sum(species == "FRAM" | species == "FRNI" |
                             species == "FRPE" | grepl("Fraxinus", species, fixed = TRUE)),
            Larix = sum(grepl("Larix", species, fixed = TRUE)),
            Ostraya = sum(grepl("Ostraya", species, fixed = TRUE)),
            Populus = sum(grepl("Populus", species, fixed = TRUE)),
            Prunus = sum(grepl("Prunus", species, fixed = TRUE)),
            Quercus = sum(grepl("Quercus", species, fixed = TRUE)),
            Rhamnus = sum(grepl("Rhamnus", species, fixed = TRUE)),
            Tilia = sum(grepl("Tilia", species, fixed = TRUE)),
            Ulmus = sum(grepl("Ulmus", species, fixed = TRUE)),
            Unknown = sum(grepl("Unknown", species, fixed = TRUE)))

tree_genera_2025 <- c("Acer", "Betula", "Carpinus", "Carya", "Fagus", "Fraxinus",
                      "Larix","Ostraya","Populus","Prunus","Quercus","Rhamnus",
                      "Tilia","Ulmus","Unknown")

living_big_trees_2025_by_plot$row_sums_test <- rowSums(living_big_trees_2025_by_plot[,tree_genera_2025])
living_big_trees_2025_by_plot$num_trees - living_big_trees_2025_by_plot$row_sums_test










