# Aaron Tayal
# 3/31/2025
# Center trees comparison
# The goal is to merge the 2004-05 center tree data with the 2024 data so that 
# I can determine the growth rates of different trees

library(dplyr)
library(ggplot2)

plot_centers_2024 <- read.csv("Cleaned_data/EAB_Michigan_2024_plot_centers_with_hydro.csv")
plot_centers_2024$Center_tree_2024_total_DBH_cm <- 
  as.numeric(plot_centers_2024$Center_tree_2024_total_DBH_cm)

dat_04_05 <- read.csv("2004-05_data/EAB_Michigan_2004_2005_trees.csv")
center_trees_04_05 <- dat_04_05 %>% filter(grepl('center tree', Notes))
center_trees_04_05$Plot_ID <- center_trees_04_05$Plot.ID

dat <- full_join(plot_centers_2024, center_trees_04_05, by="Plot_ID")
center_trees <- dat %>% select(center_tree_number, Plot_ID, mstrlvl, Park, Lat,
                               Lon, Transect, Genus.Species, DBH.12.5cm.or.greater.measured.at.1.37m.from.base,
                               Center_tree, Center_tree_2024_species, Center_tree_2024_total_DBH_cm,
                               Center_tree_2024_number_of_stems) %>%
  rename(spp_04_05 = Genus.Species, DBH_04_05 = DBH.12.5cm.or.greater.measured.at.1.37m.from.base,
         spp_DBH_mystery_year = Center_tree, spp_2024 = Center_tree_2024_species,
         DBH_2024 = Center_tree_2024_total_DBH_cm)

