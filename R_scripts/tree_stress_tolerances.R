# Stress tolerances from Niinemets and Valladares #############################

tolerance <- read.csv("Cleaned_data/niinemets_valladares_tree_tolerances.csv")

library(stringr)
#tolerance$Drought.tolerance.simple <- 

species_for_tolerances <- c("Fraxinus nigra", "Fraxinus pennsylvanica",
                            "Acer saccharinum", "Acer rubrum", "Acer saccharum", "Betula alleghaniensis",
                            "Carpinus caroliniana", "Frangula alnus", "Fagus grandifolia",
                            "Larix laricina", "Ostrya virginiana", "Populus deltoides", "Populus grandidentata",
                            "Prunus serotina", "Quercus bicolor", "Quercus macrocarpa", "Quercus rubra", 
                            "Quercus alba", "Quercus palustris",
                            "Tilia americana", "Ulmus americana", "Ulmus rubra", "Carya ovata")

tolerance_rows <- which(tolerance$Species %in% species_for_tolerances)

tolerance_table <- tolerance[tolerance_rows, c("Species", "Waterlogging.tolerance", "Shade.tolerance")]

#write.csv(tolerance_table, "Cleaned_data/tree_stress_tolerances.csv", row.names = F)
