# 6/23/2025
# Aaron Tayal
# Preliminary code for analysing the tree survey data for 2025 EAB Michigan 
# Project. Here are some goals:
# 1. See if the counts of ash trees that were done in 2025 match up to the data
# collected in 2024
# 2. Remove the 2025 counts of ash trees and instead replace them with the
# 2024 counts
# 3. Filter out the understory trees and the canopy trees
# 4. Make summaries of the densities and basal areas of each tree genus
# 5. Graph the data

library(dplyr)
library(ggplot2)
library(patchwork) # Used for stacking histograms one on top of the other
library(tidyr)
library(ggpubr)

dat_2025 <- read.csv("Raw_data/EAB_Michigan_2025_trees_raw.csv")
dat_2025$DBH <- dat_2025$diameter_at_137_cm_in_cm
dat_2025$string_DBH <- dat_2025$string_diameter_at_137_cm_in_cm
summary(dat_2025$DBH)

hydric_plots <- unique(dat$center_tree_number)

# Look at the distribution of tree diameters:
ggplot(data=dat_2025, aes(x=DBH)) + geom_histogram(breaks=seq(0,190,10))

# Look at the distribution of tree diameters (specifically smaller diameter range)
ggplot(data=dat_2025, aes(x=DBH)) + geom_histogram(breaks=seq(0,50,1))

# Make species and living/dead and declining_y_n into factor variables:
dat_2025$species <- as.factor(dat_2025$species)
table(dat_2025$species)
dat_2025$living_or_dead <- as.factor(dat_2025$living_or_dead)
table(dat_2025$living_or_dead)
dat_2025$declining_y_n <- as.factor(dat_2025$declining_y_n)
table(dat_2025$declining_y_n)

# Investigate the 2025 counts of ash trees: #########################################

# Make a subset of just the ash trees:
ash_2025 <- dat_2025 %>% filter(species == "Fraxinus section Melioides" | 
                            species == "Fraxinus sp." | 
                            species == "Fraxinus nigra"| 
                            species == "Fraxinus pennsylvanica"|
                            species == "Fraxinus americana")
  

ggplot(data=ash_2025, aes(x=DBH)) + geom_histogram(breaks=seq(0,50,1))

# Make a subset of just small ash trees < 10 cm DBH:
ash_small_trees_2025 <- ash_2025 %>% filter(diameter_at_137_cm_in_cm >= 2.5) %>%
  filter(diameter_at_137_cm_in_cm < 10)

# Summarize by plot and separate into living and dead:
ash_small_trees_by_plot_2025 <- ash_small_trees_2025 %>% group_by(center_tree_number) %>%
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
ash_big_trees_2025 <- ash_2025 %>% filter(diameter_at_137_cm_in_cm > 10)

# Summarize by plot and separate into living and dead:
ash_big_trees_by_plot_2025 <- ash_big_trees_2025 %>% group_by(center_tree_number) %>%
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

# Import data from 2024 on ash trees:

ash_official_data <- read.csv("Cleaned_data/ash_by_plot.csv")

# Merge the 2025 ash tree data:
ash_comparison_0 <- full_join(ash_official_data, ash_small_trees_by_plot_2025, 
                            by="center_tree_number")
ash_comparison <- full_join(ash_comparison_0, ash_big_trees_by_plot_2025, 
                            by="center_tree_number")

# Graph number of ash small trees in 2024 against 2025:
ggplot(data=ash_comparison, aes(x=number_small_trees, y=number_small_trees_2025)) +
  geom_point() + theme_bw() + geom_abline(slope = 1, intercept = 0)

# Graph the discrepancy between the years in number of small trees found:
ggplot(data=ash_comparison, aes(x=center_tree_number, 
                                y=number_small_trees_2025 - number_small_trees)) +
  geom_point() + theme_bw()

# Graph basal area of living ash in 2024 against 2025:
ggplot(data=ash_comparison, aes(x=basal_area_living_small_trees_in_m_squared,
                                y= basal_area_living_small_trees_in_m_squared_2025)) +
  geom_point() + theme_bw() + geom_abline(slope = 1, intercept = 0)

# Graph the discrepancy in living basal area of small ash trees between the 2 years:
ggplot(data=ash_comparison, aes(x=center_tree_number, 
                                y=basal_area_living_small_trees_in_m_squared_2025 -
                                  basal_area_living_small_trees_in_m_squared)) +
  geom_point() + theme_bw()

# Graph basal area of ash in 2024 against 2025:
ggplot(data=ash_comparison, aes(x=basal_area_small_trees_in_m_squared,
                                y=basal_area_small_trees_in_m_squared_2025)) +
  geom_point() + theme_bw() + geom_abline(slope = 1, intercept = 0)

# This file is of ash trees recorded mostly in 2024
individual_trees <- read.csv("Cleaned_data/individual_trees.csv") 

# Filter out any ash trees not belonging to hydric transects, because the 2025
# data is just for hydric transects.
individual_trees$mstrlvl <- as.factor(individual_trees$mstrlvl)
individual_trees <- individual_trees %>% filter(mstrlvl == "hydric")

plot_72_trees_2024 <- individual_trees %>% filter(center_tree_number==72)
plot_72_trees_2024$canopy_condition_1_5 <- as.factor(plot_72_trees_2024$canopy_condition_1_5)

plot_72_trees_2025 <- ash_2025 %>% filter(center_tree_number==72) %>% 
  filter(diameter_at_137_cm_in_cm != 14.8)
# disregard a 14.8 cm DBH dead ash tree
plot_72_trees_2025$canopy_condition_if_ash <- as.factor(plot_72_trees_2025$canopy_condition_if_ash)

hist_72_2024 <- ggplot(data=plot_72_trees_2024, aes(x=diameter_at_137_cm_in_cm, fill=canopy_condition_1_5)) +
  geom_histogram(binwidth = 0.5) + scale_fill_brewer()
hist_72_2025 <- ggplot(data=plot_72_trees_2025, aes(x=diameter_at_137_cm_in_cm, fill=canopy_condition_if_ash)) +
  geom_histogram(binwidth = 0.5) + scale_fill_brewer()

hist_72_2024 / hist_72_2025 
# At plot 72, the basal area of small trees decreased from 2024 to 2025.

# It appears that some of the trees died, which
# may have reduced the living basal area. Also, a few trees may have been 
# missed in the 2025 survey.

# I am trying to compare the stem counts and basal areas of ash between 2024 and
# 2025 to see if there are any major changes. Preliminary comparison suggests 
# that the counts are similar. However, some of the basal areas differ.
# This could reflect growth of ash trees into the size class, growth within
# the size class, growth out of the size class, death and falling over of trees, 
# and missing a few trees during surveying. But the data generally matches up. 

# Check if center trees are recorded ###########################################
# Sometimes I may have forgotten to enter the center tree, so I need to check
# if any of the center trees are missing.

dat_2025$quadrant_NE_SE_SW_NW <- as.factor(dat_2025$quadrant_NE_SE_SW_NW)
dat_2025_centers <- dat_2025[dat_2025$quadrant_NE_SE_SW_NW == "center",]
unique(dat_2025_centers$center_tree_number)
# Plots 24, 66, and 43 had missing or fallen center trees.

# Remove 2025 ash trees and add in the 2024 ash trees ##########################

no_ash_2025 <- dat_2025 %>% filter(species != "Fraxinus section Melioides" & 
         species != "Fraxinus sp." & 
         species != "Fraxinus nigra"& 
         species != "Fraxinus pennsylvanica"&
         species != "Fraxinus americana")

individual_trees$trees_date_surveyed <- as.character(individual_trees$year_of_data_collection)

# Rename the species names to match what I have for the 2025 tree survey:
individual_trees$species <- individual_trees$ash_species_simple
individual_trees$species[individual_trees$species == "green_white_or_pumpkin"] <- "Fraxinus section Melioides" # white or green ash
# Note: I'm not sure if pumpkin ash is in section Melioides
individual_trees$species[individual_trees$species == "black"] <- "Fraxinus nigra"
individual_trees$species[individual_trees$species == "unknown"] <- "Fraxinus sp."
table(individual_trees$species)

# Rename the column that shows if the tree is in the subplot or the main plot:
individual_trees$inside_8_meters_y_n <- individual_trees$distance_to_center_meters_simple
individual_trees$inside_8_meters_y_n[individual_trees$distance_to_center_meters_simple <= 8] <- "y"
individual_trees$inside_8_meters_y_n[individual_trees$distance_to_center_meters_simple > 8] <- "n"

individual_trees[individual_trees$inside_8_meters_y_n == "n",]$diameter_at_137_cm_in_cm
# Looks like all the ash trees outside the subplot were < 12.5 cm DBH. So I'll
# need to filter them out to stay consistent

individual_trees <- individual_trees %>% filter(!(inside_8_meters_y_n == "n" & 
                                                    diameter_at_137_cm_in_cm < 12.5))
table(individual_trees$inside_8_meters_y_n) # Now all the ash trees are in the subplot

individual_trees$DBH <- as.numeric(individual_trees$diameter_at_137_cm_in_cm)
individual_trees$string_DBH <- as.character(individual_trees$DBH)

table(individual_trees$canopy_condition_1_5)
# Rename the canopy condition into a variable living_or_dead:
individual_trees$living_or_dead <- 
  as.factor(ifelse(individual_trees$canopy_condition_1_5 == 5 & 
                     !is.na(individual_trees$canopy_condition_1_5), "dead", "living"))


individual_trees$declining_y_n <- as.factor(ifelse(individual_trees$canopy_condition_1_5 %in% c(2,3,4), "y", "n"))

individual_trees$trees_notes <- individual_trees$ash_trees_notes

# Finally I can merge the ash trees to the non-ash trees:
dat <- bind_rows(no_ash_2025, individual_trees) # no_ash_2025 is the non-ash trees 
# recorded in 2025, while individual_trees are the ash trees recorded mostly in
# 2024

# Overall counts of trees ######################################################



# Tree genera ##################################################################

table(dat$species)

# The following list is all the tree genera that we recorded in 2025:
tree_genera_2025 <- c("Acer", "Betula", "Carpinus", "Carya", "Cornus", "Fagus", 
                      "Frangula", "Fraxinus", "Juniperus", 
                      "Larix","Ostraya","Populus","Prunus","Quercus","Rhamnus",
                      "Tilia","Ulmus", "Unknown","Viburnum") # 19 genera

# Functions for calculating tree counts and basal areas ########################

# This function parses something like "2+3+4" then adds up the numbers
sum_from_string <- function(s) {
  parts <- strsplit(s, "\\+")[[1]]
  nums <- as.numeric(parts)
  sum(nums)
}


subplot_area <- pi*(8^2) / 10000 # 8 meter radius subplot, convert to hectares
main_plot_area <- pi*(18^2) / 10000 # 18 meter radius main plot, convert to hectares

# This function parses something like "2+3+4" then treats each number as a 
# diameter of a circle. So it divides each diameter by 2 and then squares each
# and multiplies by pi to find the area.
BA_from_string <- function(s) {
  parts <- strsplit(s, "\\+")
  nums <- as.numeric(unlist(parts))
  sum(pi*(nums / 2)^2)
}

# The following function finds all the trees in a given genus and then 
# calculates the basal area, which is reported in meters^2
genus_BA = function(data_frame, genus_string) {
  indices = grep(genus_string, data_frame$species, fixed = TRUE)
  string_DBHs <- data_frame$string_DBH[indices]
  BA <- BA_from_string(string_DBHs)
  BA / 10000 # Convert basal area to meters^2, from cm^2
}

# Big trees >=12.5 cm ##########################################################

# Subset the data to just the trees bigger than 12.5 cm DBH, which are
# living: (Note: I guess the 2005 data was just recorded for living trees,
# but I need to check up on this)
dat_big <- dat %>% filter(DBH >= 12.5) %>% 
  filter(living_or_dead == "living") # include only living trees

dat_big_dead <- dat %>% filter(DBH >= 12.5) %>% 
  filter(living_or_dead == "dead")

table(dat_big$species)

BA_big_by_plot <- dat_big %>% group_by(center_tree_number) %>% 
  summarise(Acer = genus_BA(pick(species, string_DBH), "Acer") / main_plot_area,
            Betula = genus_BA(pick(species, string_DBH), "Betula") / main_plot_area,
            Carpinus = genus_BA(pick(species, string_DBH), "Carpinus") / main_plot_area,
            Carya = genus_BA(pick(species, string_DBH), "Carya") / main_plot_area,
            Cornus = genus_BA(pick(species, string_DBH), "Cornus") / main_plot_area,
            Fagus = genus_BA(pick(species, string_DBH), "Fagus") / main_plot_area,
            Frangula = genus_BA(pick(species, string_DBH), "Frangula") / main_plot_area,
            Fraxinus = genus_BA(pick(species, string_DBH), "Fraxinus") / main_plot_area,
            Juniperus = genus_BA(pick(species, string_DBH), "Juniperus") / main_plot_area,
            Larix = genus_BA(pick(species, string_DBH), "Larix") / main_plot_area,
            Ostraya = genus_BA(pick(species, string_DBH), "Ostraya") / main_plot_area,
            Populus = genus_BA(pick(species, string_DBH), "Populus") / main_plot_area,
            Prunus = genus_BA(pick(species, string_DBH), "Prunus") / main_plot_area,
            Quercus = genus_BA(pick(species, string_DBH), "Quercus") / main_plot_area,
            Rhamnus = genus_BA(pick(species, string_DBH), "Rhamnus") / main_plot_area,
            Tilia = genus_BA(pick(species, string_DBH), "Tilia") / main_plot_area,
            Ulmus = genus_BA(pick(species, string_DBH), "Ulmus") / main_plot_area,
            Unknown = genus_BA(pick(species, string_DBH), "Unknown") / main_plot_area,
            Viburnum = genus_BA(pick(species, string_DBH), "Viburnum") / main_plot_area
  )

BA_big_by_plot_longer <- pivot_longer(BA_big_by_plot, cols = all_of(tree_genera_2025),
                                      names_to = "genus", values_to = "basal_area")

BA_big_by_plot$total <- rowSums(BA_big_by_plot[,tree_genera_2025])

counts_big_by_plot <- dat_big %>% group_by(center_tree_number) %>% 
  summarise(total = n() / main_plot_area,
            Acer = sum(grepl("Acer", species, fixed = TRUE)) / main_plot_area,
            Betula = sum(grepl("Betula", species, fixed = TRUE)) / main_plot_area,
            Carpinus = sum(grepl("Carpinus", species, fixed = TRUE)) / main_plot_area,
            Carya = sum(grepl("Carya", species, fixed = TRUE)) / main_plot_area,
            Cornus = sum(grepl("Cornus", species, fixed = TRUE)) / main_plot_area,
            Fagus = sum(grepl("Fagus", species, fixed = TRUE)) / main_plot_area,
            Frangula = sum(grepl("Frangula", species, fixed = TRUE)) / main_plot_area,
            Fraxinus = sum(grepl("Fraxinus", species, fixed = TRUE)) / main_plot_area,
            Juniperus = sum(grepl("Juniperus", species, fixed = TRUE)) / main_plot_area,
            Larix = sum(grepl("Larix", species, fixed = TRUE)) / main_plot_area,
            Ostraya = sum(grepl("Ostraya", species, fixed = TRUE)) / main_plot_area,
            Populus = sum(grepl("Populus", species, fixed = TRUE)) / main_plot_area,
            Prunus = sum(grepl("Prunus", species, fixed = TRUE)) / main_plot_area,
            Quercus = sum(grepl("Quercus", species, fixed = TRUE)) / main_plot_area,
            Rhamnus = sum(grepl("Rhamnus", species, fixed = TRUE)) / main_plot_area,
            Tilia = sum(grepl("Tilia", species, fixed = TRUE)) / main_plot_area,
            Ulmus = sum(grepl("Ulmus", species, fixed = TRUE)) / main_plot_area,
            Unknown = sum(grepl("Unknown", species, fixed = TRUE)) / main_plot_area,
            Viburnum = sum(grepl("Viburnum", species, fixed = TRUE) / main_plot_area)
            )

counts_big_by_plot_longer <- pivot_longer(counts_big_by_plot, cols = all_of(c(tree_genera_2025, "total")),
                                      names_to = "genus", values_to = "counts")

# Small trees 2.5-12.5 cm DBH ##################################################
# Also filter out the dead trees

dat_small <- dat %>% filter(DBH >= 2.5) %>% filter (DBH < 12.5) %>% 
  filter(living_or_dead == "living")

table(dat_small$species)

dat_small_dead <- dat %>% filter(DBH >= 2.5) %>% filter (DBH < 12.5) %>% 
  filter(living_or_dead == "dead")

BA_small_by_plot <- dat_small %>% group_by(center_tree_number) %>% 
  summarise(Acer = genus_BA(pick(species, string_DBH), "Acer") / subplot_area,
            Betula = genus_BA(pick(species, string_DBH), "Betula") / subplot_area,
            Carpinus = genus_BA(pick(species, string_DBH), "Carpinus") / subplot_area,
            Carya = genus_BA(pick(species, string_DBH), "Carya") / subplot_area,
            Cornus = genus_BA(pick(species, string_DBH), "Cornus") / subplot_area,
            Fagus = genus_BA(pick(species, string_DBH), "Fagus") / subplot_area,
            Frangula = genus_BA(pick(species, string_DBH), "Frangula") / subplot_area,
            Fraxinus = genus_BA(pick(species, string_DBH), "Fraxinus") / subplot_area,
            Juniperus = genus_BA(pick(species, string_DBH), "Juniperus") / subplot_area,
            Larix = genus_BA(pick(species, string_DBH), "Larix") / subplot_area,
            Ostraya = genus_BA(pick(species, string_DBH), "Ostraya") / subplot_area,
            Populus = genus_BA(pick(species, string_DBH), "Populus") / subplot_area,
            Prunus = genus_BA(pick(species, string_DBH), "Prunus") / subplot_area,
            Quercus = genus_BA(pick(species, string_DBH), "Quercus") / subplot_area,
            Rhamnus = genus_BA(pick(species, string_DBH), "Rhamnus") / subplot_area,
            Tilia = genus_BA(pick(species, string_DBH), "Tilia") / subplot_area,
            Ulmus = genus_BA(pick(species, string_DBH), "Ulmus") / subplot_area,
            Unknown = genus_BA(pick(species, string_DBH), "Unknown") / subplot_area,
            Viburnum = genus_BA(pick(species, string_DBH), "Viburnum") / subplot_area
            ) %>%
  complete(center_tree_number = hydric_plots) # This adds back in the plots where 
# no small trees were recorded (40 and 77)
BA_small_by_plot[is.na(BA_small_by_plot)] <- 0 # This replaces the NAs to 0s 
# for plots 40 and 77

BA_small_by_plot_longer <- pivot_longer(BA_small_by_plot, cols = all_of(tree_genera_2025),
                                      names_to = "genus", values_to = "basal_area")

BA_small_by_plot$total <- rowSums(BA_small_by_plot[,tree_genera_2025])

counts_small_by_plot <- dat_small %>% group_by(center_tree_number) %>% 
  summarise(total = n() / subplot_area,
            Acer = sum(grepl("Acer", species, fixed = TRUE)) / subplot_area,
            Betula = sum(grepl("Betula", species, fixed = TRUE)) / subplot_area,
            Carpinus = sum(grepl("Carpinus", species, fixed = TRUE)) / subplot_area,
            Carya = sum(grepl("Carya", species, fixed = TRUE)) / subplot_area,
            Cornus = sum(grepl("Cornus", species, fixed = TRUE)) / subplot_area,
            Fagus = sum(grepl("Fagus", species, fixed = TRUE)) / subplot_area,
            Frangula = sum(grepl("Frangula", species, fixed = TRUE)) / subplot_area,
            Fraxinus = sum(grepl("Fraxinus", species, fixed = TRUE)) / subplot_area,
            Juniperus = sum(grepl("Juniperus", species, fixed = TRUE)) / subplot_area,
            Larix = sum(grepl("Larix", species, fixed = TRUE)) / subplot_area,
            Ostraya = sum(grepl("Ostraya", species, fixed = TRUE)) / subplot_area,
            Populus = sum(grepl("Populus", species, fixed = TRUE)) / subplot_area,
            Prunus = sum(grepl("Prunus", species, fixed = TRUE)) / subplot_area,
            Quercus = sum(grepl("Quercus", species, fixed = TRUE)) / subplot_area,
            Rhamnus = sum(grepl("Rhamnus", species, fixed = TRUE)) / subplot_area,
            Tilia = sum(grepl("Tilia", species, fixed = TRUE)) / subplot_area,
            Ulmus = sum(grepl("Ulmus", species, fixed = TRUE)) / subplot_area,
            Unknown = sum(grepl("Unknown", species, fixed = TRUE)) / subplot_area,
            Viburnum = sum(grepl("Viburnum", species, fixed = TRUE) / subplot_area)
  ) %>% complete(center_tree_number=hydric_plots)

counts_small_by_plot[is.na(counts_small_by_plot)] <- 0

counts_small_by_plot_longer <- pivot_longer(counts_small_by_plot, cols = all_of(c(tree_genera_2025, "total")),
                                        names_to = "genus", values_to = "counts")

# Summaries of the data ########################################################

BA_big_summary <- BA_big_by_plot_longer %>% group_by(genus) %>%
  summarise(mean_BA = mean(basal_area))

BA_big_summary %>% arrange(-mean_BA)

counts_big_summary <- counts_big_by_plot_longer %>% group_by(genus) %>% 
  summarise(mean_counts = mean(counts))

counts_big_summary %>% arrange(-mean_counts)

BA_small_summary <- BA_small_by_plot_longer %>% group_by(genus) %>%
  summarise(mean_BA = mean(basal_area))

BA_small_summary %>% arrange(-mean_BA)

counts_small_summary <- counts_small_by_plot_longer %>% group_by(genus) %>% 
  summarise(mean_counts = mean(counts))

counts_small_summary %>% arrange(-mean_counts)

# Graph the data ###############################################################

genera_subset <- c("Acer", "Betula", "Carpinus", "Frangula", "Fraxinus", "Larix",
                   "Populus", "Quercus", "Tilia", "Ulmus")

BA_big_graph <- BA_big_by_plot_longer %>% filter(genus %in% genera_subset) %>%
  ggplot(aes(x=genus, y=basal_area)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(height=0, width=0.05, alpha=0.5) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16, margin = margin(r = 15))) +
  ylab("Living basal area of \ncanopy trees (m^2 / ha)") + xlab("")

BA_big_graph

BA_small_graph <- BA_small_by_plot_longer %>% filter(genus %in% genera_subset) %>%
  ggplot(aes(x=genus, y=basal_area)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(height=0, width=0.05, alpha=0.5) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16, margin = margin(r = 15))) +
  ylab("Living basal area of \nunderstory trees (m^2 / ha)") + xlab("")

BA_small_graph

counts_big_graph <- ggplot(data = counts_big_by_plot_longer, aes(x=genus, y=counts)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(height=0, width=0.05, alpha=0.5) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ylab("Density of big trees \n(living trunks / ha)") + xlab("")

counts_big_graph

counts_small_graph <- ggplot(data = counts_small_by_plot_longer, aes(x=genus, y=counts)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(height=0, width=0.05, alpha=0.5) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ylab("Density of small trees \n(living trunks / ha)") + xlab("")

counts_small_graph

ggarrange(BA_big_graph, 
          BA_small_graph,
          labels = c("A", "B"),
          ncol = 1, nrow = 2)




