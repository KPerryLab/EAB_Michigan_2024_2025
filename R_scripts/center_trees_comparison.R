# Aaron Tayal
# 3/31/2025
# Center trees comparison
# The goal is to merge the 2004-05 center tree data with the 2024 data so that 
# I can determine the growth rates of different trees

library(dplyr)
library(ggplot2)
library(readxl)

center_trees_2024 <- read.csv("Cleaned_data/EAB_Michigan_2024_2025_plot_centers_with_hydro.csv")
# Note: there is an issue with plot 75 in that two trees very close to each
# other were tagged as 75. But this is not an issue for my study of hydric plots
#center_trees_2024$Center_tree_2024_2025_total_DBH_cm <- 
#  as.numeric(center_trees_2024$Center_tree_2024_2025_total_DBH_cm) # some center trees 
# were not visited in 2024, or had fallen over, thus introducing some NA's

dat_04_05 <- read.csv("2004-05_data/EAB_Michigan_2004_2005_trees.csv")
center_trees_04_05 <- dat_04_05 %>% filter(grepl('center tree', Notes))
center_trees_04_05$DBH.12.5cm.or.greater.measured.at.1.37m.from.base <- 
  as.numeric(center_trees_04_05$DBH.12.5cm.or.greater.measured.at.1.37m.from.base)
center_trees_04_05$Plot_ID <- center_trees_04_05$Plot.ID
plots_04_05 <- center_trees_04_05$Plot_ID
# Unfortunately, sometimes there are multiple rows for a single plot
# in the 04_05 data, when the tree had many stems. So I'll summarize:
center_trees_04_05_by_plot <- center_trees_04_05 %>% group_by(Plot_ID) %>%
  summarize(Genus.Species = first(Genus.Species),
            center_tree_04_05_total_DBH_cm = sum(DBH.12.5cm.or.greater.measured.at.1.37m.from.base),
            center_tree_04_05_number_stems = n())

center_trees_06_08 <- read_excel("2008-2010_data/Aaron_edited_Updated_Plot_Descriptions_2008.xlsx",
                                 sheet=1, col_names = T)
plots_06_08 <- center_trees_06_08$Plot_ID


dat0 <- full_join(center_trees_04_05_by_plot, center_trees_06_08, by="Plot_ID")
dat <- full_join(dat0, center_trees_2024, by = "Plot_ID")

center_trees <- dat %>% select(center_tree_number.x, Plot_ID, mstrlvl, Park,
                               
                               #2004-2005 dataset:
                               Genus.Species, center_tree_04_05_total_DBH_cm, 
                               center_tree_04_05_number_stems,
                               
                               #2006-2008 dataset:
                               dbh_2006_2008, year_2006_2008_data,
                               
                               # mystery year:
                               Center_tree, 
                               
                               # 2024 dataset:
                               Center_tree_2024_2025_species, Center_tree_2024_2025_total_DBH_cm,
                               Center_tree_2024_2025_number_of_stems)

center_trees <- center_trees %>% arrange(center_tree_number.x)

center_trees_hydric <- center_trees %>% filter(mstrlvl == "hydric")

#write.csv(center_trees_hydric, "plot_center_trees_hydric.csv")
