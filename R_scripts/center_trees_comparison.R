# Aaron Tayal
# 3/31/2025
# Center trees comparison
# The goal is to merge the 2004-05 center tree data with the 2024 data so that 
# I can determine the growth rates of different trees

library(dplyr)
library(ggplot2)
library(readxl)

center_trees_2024 <- read.csv("Cleaned_data/EAB_Michigan_2024_plot_centers_with_hydro.csv")
center_trees_2024$Center_tree_2024_total_DBH_cm <- 
  as.numeric(center_trees_2024$Center_tree_2024_total_DBH_cm) # some center trees 
# were not visited in 2024, or had fallen over, thus introducing some NA's

dat_04_05 <- read.csv("2004-05_data/EAB_Michigan_2004_2005_trees.csv")
center_trees_04_05 <- dat_04_05 %>% filter(grepl('center tree', Notes))
center_trees_04_05$Plot_ID <- center_trees_04_05$Plot.ID

center_trees_06_08 <- read_excel("2008-2010_data/Aaron_edited_Updated_Plot_Descriptions_2008.xlsx",
                                 sheet=1, col_names = T)

dat0 <- full_join(center_trees_04_05, center_trees_06_08, by="Plot_ID")
dat <- full_join(dat0, center_trees_2024, by = "Plot_ID", relationship = "many-to-many")

center_trees <- dat %>% select(center_tree_number.x, Plot_ID, mstrlvl, Park,
                               
                               #2004-2005 dataset:
                               Genus.Species, DBH.12.5cm.or.greater.measured.at.1.37m.from.base,
                               
                               #2006-2008 dataset:
                               dbh_2006_2008, year_2006_2008_data,
                               
                               # mystery year:
                               Center_tree, 
                               
                               # 2024 dataset:
                               Center_tree_2024_species, Center_tree_2024_total_DBH_cm,
                               Center_tree_2024_number_of_stems) %>%
  
  rename(spp_04_05 = Genus.Species, DBH_04_05 = DBH.12.5cm.or.greater.measured.at.1.37m.from.base,
         spp_DBH_mystery_year = Center_tree, spp_2024 = Center_tree_2024_species,
         DBH_2024 = Center_tree_2024_total_DBH_cm)

center_trees <- center_trees %>% arrange(center_tree_number.x)
