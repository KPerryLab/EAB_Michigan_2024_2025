# Aaron Tayal
# 4/24/2025
# 2004-2005 trees
# The goal is to generate a data table where each row is a plot, and each
# column is a species of tree. The values would be the basal area of trees
# of that species.

# The purpose is to try and understand the range of "initial" amounts of 
# ash trees in the plots, as well as "initial" amounts of non-ash trees. Then
# the goal of the 2025 research is to see whether non-ash tree species
# exhibited compensatory growth in response to ash-tree mortality (specifically
# within hydric forests)

# Did the basal area of non-ash trees increase within plots that had a high
# amount of ash trees initially?

# Another goal is to look at the waterlogging tolerance of the trees. The goal
# is to determine whether the composition of trees moved towards more flood
# tolerant trees. With that in mind, I'll look at data compiled in the paper
# Niinemets and Valladares 2006, as well as at wetland indicator status.

library(dplyr)
library(ggplot2)

dat_04_05 <- read.csv("2004-05_data/EAB_Michigan_2004_2005_trees.csv")
dat_04_05$DBH <- as.numeric(dat_04_05$DBH.12.5cm.or.greater.measured.at.1.37m.from.base)

# Look at the distribution of diameters:
ggplot(data=dat_04_05, aes(x=DBH)) + geom_histogram(breaks=seq(0,5,0.1))
# A few trees have DBH lower than 2.5 cm - not sure why
ggplot(data=dat_04_05, aes(x=DBH)) + geom_histogram(breaks=seq(0,12,0.5))
ggplot(data=dat_04_05, aes(x=DBH)) + geom_histogram(breaks=seq(0,130,0.5))
# The spike at 12.5 cm DBH is because this was the cutoff for measuring 
# trees in the 18 m radius main plot

# Look at the different species of tree:
dat_04_05$Genus.Species <- as.factor(dat_04_05$Genus.Species)
table(dat_04_05$Genus.Species)
species <- levels(dat_04_05$Genus.Species)

# Where was Elaeagnus umbellata found (Autumn olive)?
autumn_olive_rows <- dat_04_05[(dat_04_05$Genus.Species == "Elaeagnus umbellata"),]

# Where was Toxicodendron vernix found (Poison sumac)?
poison_sumac_rows <- dat_04_05[(dat_04_05$Genus.Species == "Toxicodendron vernix"),]


# Big trees >=12.5 cm ##########################################################

# Subset the data to just the trees bigger than 12.5 cm DBH:
dat_04_05_big_trees <- dat_04_05 %>% filter(DBH >= 12.5)

table(dat_04_05_big_trees$Genus.Species)

# Create a plot-level summary
big_trees_by_plot <- dat_04_05_big_trees %>% group_by(Plot.ID) %>% 
  summarise(total_trees = n(),
            total_BA = sum(3.141592654 * ( (DBH / 200) ^ 2 ), na.rm = T),
            Acer = sum(grepl("Acer", Genus.Species, fixed = TRUE)),
            Ailanthus = sum(grepl("Ailanthus", Genus.Species, fixed = TRUE)),
            Amelanchier = sum(grepl("Amelanchier", Genus.Species, fixed = TRUE)),
            Betula = sum(grepl("Betula", Genus.Species, fixed = TRUE)),
            Carpinus = sum(grepl("Carpinus", Genus.Species, fixed = TRUE)),
            Carya = sum(grepl("Carya", Genus.Species, fixed = TRUE)),
            Cornus = sum(grepl("Cornus", Genus.Species, fixed = TRUE)),
            Crataegus = sum(grepl("Crataegus", Genus.Species, fixed = TRUE)),
            Elaeagnus = sum(grepl("Elaeagnus", Genus.Species, fixed = TRUE)),
            Fagus = sum(grepl("Fagus", Genus.Species, fixed = TRUE)),
            Fraxinus = sum(Genus.Species == "FRAM" | Genus.Species == "FRNI" |
                             Genus.Species == "FRPE"),
            Gleditsia = sum(grepl("Gleditsia", Genus.Species, fixed = TRUE)),
            Hamamelis = sum(grepl("Hamamelis", Genus.Species, fixed = TRUE)),
            Juglans = sum(grepl("Juglans", Genus.Species, fixed = TRUE)),
            Juniperus = sum(grepl("Juniperus", Genus.Species, fixed = TRUE)),
            Larix = sum(grepl("Larix", Genus.Species, fixed = TRUE)),
            Liriodendron = sum(grepl("Liriodendron", Genus.Species, fixed = TRUE)),
            Ostraya = sum(grepl("Ostraya", Genus.Species, fixed = TRUE)),
            Pinus = sum(grepl("Pinus", Genus.Species, fixed = TRUE)),
            Populus = sum(grepl("Populus", Genus.Species, fixed = TRUE)),
            Prunus = sum(grepl("Prunus", Genus.Species, fixed = TRUE)),
            Quercus = sum(grepl("Quercus", Genus.Species, fixed = TRUE)),
            Rhamnus = sum(grepl("Rhamnus", Genus.Species, fixed = TRUE)),
            Robinia = sum(grepl("Robinia", Genus.Species, fixed = TRUE)),
            Sassafras = sum(grepl("Sassafras", Genus.Species, fixed = TRUE)),
            Tilia = sum(grepl("Tilia", Genus.Species, fixed = TRUE)),
            Toxicodendron = sum(grepl("Toxicodendron", Genus.Species, fixed = TRUE)),
            Ulmus = sum(grepl("Ulmus", Genus.Species, fixed = TRUE)),
            Unknown = sum(grepl("Unknown", Genus.Species, fixed = TRUE)),
            Viburnum = sum(grepl("Viburnum", Genus.Species, fixed = TRUE)))

tree_genera <- c("Acer","Ailanthus","Amelanchier","Betula","Carpinus","Carya",
                 "Cornus","Crataegus","Elaeagnus","Fagus","Fraxinus","Gleditsia",
                 "Hamamelis","Juglans","Juniperus","Larix","Liriodendron",
                 "Ostraya","Pinus","Populus","Prunus","Quercus","Rhamnus",
                 "Robinia","Sassafras","Tilia","Toxicodendron","Ulmus",
                 "Unknown","Viburnum")

big_trees_by_plot$row_sums_test <- rowSums(big_trees_by_plot[,tree_genera])
big_trees_by_plot$total_trees - big_trees_by_plot$row_sums_test

# Small trees 2.5 cm <= DBH < 12.5 cm #########################################

# Subset the data to just trees between 2.5 to 12.5 cm DBH:
dat_04_05_small_trees <- dat_04_05 %>% filter(DBH >= 2.5 & DBH < 12.5)

table(dat_04_05_small_trees$Genus.Species)

small_trees_by_plot <- dat_04_05_small_trees %>% group_by(Plot.ID) %>% 
  summarise(total_trees = n(),
            total_BA = sum(3.141592654 * ( (DBH / 200) ^ 2 ), na.rm = T),
            Acer = sum(grepl("Acer", Genus.Species, fixed = TRUE)),
            Ailanthus = sum(grepl("Ailanthus", Genus.Species, fixed = TRUE)),
            Amelanchier = sum(grepl("Amelanchier", Genus.Species, fixed = TRUE)),
            Betula = sum(grepl("Betula", Genus.Species, fixed = TRUE)),
            Carpinus = sum(grepl("Carpinus", Genus.Species, fixed = TRUE)),
            Carya = sum(grepl("Carya", Genus.Species, fixed = TRUE)),
            Cornus = sum(grepl("Cornus", Genus.Species, fixed = TRUE)),
            Crataegus = sum(grepl("Crataegus", Genus.Species, fixed = TRUE)),
            Elaeagnus = sum(grepl("Elaeagnus", Genus.Species, fixed = TRUE)),
            Fagus = sum(grepl("Fagus", Genus.Species, fixed = TRUE)),
            Fraxinus = sum(Genus.Species == "FRAM" | Genus.Species == "FRNI" |
                             Genus.Species == "FRPE"),
            Gleditsia = sum(grepl("Gleditsia", Genus.Species, fixed = TRUE)),
            Hamamelis = sum(grepl("Hamamelis", Genus.Species, fixed = TRUE)),
            Juglans = sum(grepl("Juglans", Genus.Species, fixed = TRUE)),
            Juniperus = sum(grepl("Juniperus", Genus.Species, fixed = TRUE)),
            Larix = sum(grepl("Larix", Genus.Species, fixed = TRUE)),
            Liriodendron = sum(grepl("Liriodendron", Genus.Species, fixed = TRUE)),
            Ostraya = sum(grepl("Ostraya", Genus.Species, fixed = TRUE)),
            Pinus = sum(grepl("Pinus", Genus.Species, fixed = TRUE)),
            Populus = sum(grepl("Populus", Genus.Species, fixed = TRUE)),
            Prunus = sum(grepl("Prunus", Genus.Species, fixed = TRUE)),
            Quercus = sum(grepl("Quercus", Genus.Species, fixed = TRUE)),
            Rhamnus = sum(grepl("Rhamnus", Genus.Species, fixed = TRUE)),
            Robinia = sum(grepl("Robinia", Genus.Species, fixed = TRUE)),
            Sassafras = sum(grepl("Sassafras", Genus.Species, fixed = TRUE)),
            Tilia = sum(grepl("Tilia", Genus.Species, fixed = TRUE)),
            Toxicodendron = sum(grepl("Toxicodendron", Genus.Species, fixed = TRUE)),
            Ulmus = sum(grepl("Ulmus", Genus.Species, fixed = TRUE)),
            Unknown = sum(grepl("Unknown", Genus.Species, fixed = TRUE)),
            Viburnum = sum(grepl("Viburnum", Genus.Species, fixed = TRUE)))

small_trees_by_plot$row_sums_test <- rowSums(small_trees_by_plot[,tree_genera])
small_trees_by_plot$total_trees - small_trees_by_plot$row_sums_test

# Stems < 2.5 cm ##############################################################

# Subset the data to just trees less than 2.5 cm DBH:
dat_04_05_below_2.5 <- dat_04_05 %>% filter(DBH < 2.5)

# Data from Niinemets and Valladares ##########################################

tolerance <- read.csv("Cleaned_data/niinemets_valladares_tree_tolerances.csv")

library(stringr)
#tolerance$Drought.tolerance.simple <- 













