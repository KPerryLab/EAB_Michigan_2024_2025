# Aaron Tayal
# 8/1/2025
# Testing whether non-ash trees increased their growth after ash trees died

# We hypothesized that forests which had greater basal area of ash trees before 
# EAB invasion would have greater increases in non-ash tree basal area following 
# the death of the ash canopy, due to competitive release (Flower et al. 2013). 
# Furthermore, we hypothesized that this pattern would only hold for 
# flood-tolerant tree species (Golet et al. 1993)

library(dplyr)
library(ggplot2)
library(readxl)

# Import data  ################################################################

# Ash basal area in 2008-2010:
ash_BA_08 <- read_excel("2008-2010_data/modified_Ash_IV_data_12.5_2012.08.02.xlsx",
                        sheet=5) %>% filter(mstrlvl=="hydric")

# Tree basal area by genus in 2008-2010:
tree_BA_08 <- read_excel("2008-2010_data/modified_Ash_IV_data_12.5_2012.08.02.xlsx",
                         sheet=3)

# Tree basal area by genus in (2024)-2025:
tree_BA_25 <- read.csv("Cleaned_data/basal_area_big_trees_by_transect_2025.csv")

# Summarize the 2008-10 data by transect: ######################################

# Subset to just hydric plots: 
tree_BA_08_hydric <- tree_BA_08 %>% filter(mstrlvl == "hydric")

# Summarize by transect:
tree_BA_08_transect <- tree_BA_08_hydric %>% group_by(Transect) %>%
  summarize(
    Acer = sum(ACER),
    Amelanchier = sum(`Amelanchier spp.`),
    Betula = sum(`Betula alleghaniensis`),
    Carpinus = sum(`Carpinus caroliniana`),
    Carya = sum(CARYA),
    Fagus = sum(`Fagus grandifolia`),
    Fraxinus = sum(FRAXINUS),
    Juglans = sum(JUGLANS),
    Juniperus = sum(`Juniperus virginiana`),
    Larix = sum(`Larix laricina`),
    Liriodendron = sum(`Liriodendron tulipifera`),
    Ostrya = sum(`Ostrya virginiana`),
    Pinus = sum(`Pinus strobus`),
    Populus = sum(POPULUS),
    Prunus = sum(PRUNUS),
    Quercus = sum(QUERCUS),
    Robinia = sum(`Robinia pseudoacacia`),
    Sassafras = sum(`Sassafras albidum`),
    Tilia = sum(`Tilia americana`),
    Ulmus = sum(ULMUS),
    TOTAL = sum(TOTAL)
  )

# Now calculate a basal area total that doesn't include Fraxinus:
tree_BA_08_transect$TOTAL_non_ash.x <- 
  rowSums(tree_BA_08_transect %>% select(-Transect, -Fraxinus, -TOTAL))

# Merge the 2008-10 data to the 2024-25 data:

BA <- full_join(tree_BA_08_transect, tree_BA_25, by="Transect")

BA$TOTAL_change <- BA$TOTAL.y - BA$TOTAL.x

BA$TOTAL_non_ash_change <- BA$TOTAL.y - BA$TOTAL_non_ash.x

# Graph the non-ash basal area in 2008-10 against the non-ash basal area in 2025:
ggplot(data=BA, aes(x=TOTAL_non_ash.x, y=TOTAL.y)) +
  geom_point() + theme_bw() + xlab("Basal area of non-ash big trees in 2008-10 (m^2)") +
  ylab("Basal area of living big trees in 2024-2025 (m^2)") + 
  geom_abline(slope = 1, intercept = 0)

# Graph the change in non-ash tree basal area against the pre-EAB basal area of ash:
ggplot(data=BA, aes(x=Fraxinus.x, y=TOTAL_non_ash_change)) +
  geom_point() + theme_bw() + xlab("Basal area of ash big trees in 2008-10 (m^2)") +
  ylab("Change in basal area of non-ash trees \nbetween 2008-10 to 2025 (m^2)")



