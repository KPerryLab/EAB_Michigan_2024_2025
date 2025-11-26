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
theme_set(theme_bw(base_size = 12))
library(readxl)
library(tidyr)

# Import data  ################################################################
# Note: Here the basal area has not been divided by area over which the trees 
# were measured. So for plot-level data the area is ~0.1 ha, while for 
# transect level data the area is ~0.3 ha

# Ash basal area in 2008-2010:
ash_BA_08 <- read_excel("2008-2010_data/modified_Ash_IV_data_12.5_2012.08.02.xlsx",
                        sheet=5) %>% filter(mstrlvl=="hydric")

# Tree basal area by genus in 2008-2010:
tree_BA_08 <- read_excel("2008-2010_data/modified_Ash_IV_data_12.5_2012.08.02.xlsx",
                         sheet=3)

# Tree basal area by genus in (2024)-2025:
tree_BA_25 <- read.csv("Cleaned_data/basal_area_big_trees_by_transect_2025.csv")

# Summarize the 2008-10 data by transect: ######################################

main_plot_area <- pi*(18^2) / 10000 # 18 meter radius main plot, convert to hectares

# Subset to just hydric plots: 
tree_BA_08_hydric <- tree_BA_08 %>% filter(mstrlvl == "hydric")

# Summarize by transect:
tree_BA_08_transect <- tree_BA_08_hydric %>% group_by(Transect) %>%
  summarize(
    Acer = sum(ACER) / (3*main_plot_area),
    Amelanchier = sum(`Amelanchier spp.`) / (3*main_plot_area),
    Betula = sum(`Betula alleghaniensis`) / (3*main_plot_area),
    Carpinus = sum(`Carpinus caroliniana`) / (3*main_plot_area),
    Carya = sum(CARYA) / (3*main_plot_area),
    Fagus = sum(`Fagus grandifolia`) / (3*main_plot_area),
    Fraxinus = sum(FRAXINUS) / (3*main_plot_area),
    Juglans = sum(JUGLANS) / (3*main_plot_area),
    Juniperus = sum(`Juniperus virginiana`) / (3*main_plot_area),
    Larix = sum(`Larix laricina`) / (3*main_plot_area),
    Liriodendron = sum(`Liriodendron tulipifera`) / (3*main_plot_area),
    Ostrya = sum(`Ostrya virginiana`) / (3*main_plot_area),
    Pinus = sum(`Pinus strobus`) / (3*main_plot_area),
    Populus = sum(POPULUS) / (3*main_plot_area),
    Prunus = sum(PRUNUS) / (3*main_plot_area),
    Quercus = sum(QUERCUS) / (3*main_plot_area),
    Robinia = sum(`Robinia pseudoacacia`) / (3*main_plot_area),
    Sassafras = sum(`Sassafras albidum`) / (3*main_plot_area),
    Tilia = sum(`Tilia americana`) / (3*main_plot_area),
    Ulmus = sum(ULMUS) / (3*main_plot_area),
    TOTAL = sum(TOTAL) / (3*main_plot_area)
  )

# Now calculate a basal area total that doesn't include Fraxinus:
tree_BA_08_transect$TOTAL_non_ash.x <- 
  rowSums(tree_BA_08_transect %>% select(-Transect, -Fraxinus, -TOTAL))
# If a variable name ends in .x, that will indicate the 2008-2010 data, and 
# if it ends in .y that will indicate the 2024-2025 data.

# Merge the 2008-10 data to the 2024-25 data: ##################################

BA <- full_join(tree_BA_08_transect, tree_BA_25, by="Transect")

# Investigate change in total basal area #######################################

BA$TOTAL_change <- BA$TOTAL.y - BA$TOTAL.x

ggplot(data=BA, aes(x=TOTAL.x, y=TOTAL.y)) +
  geom_point() + theme_bw() + xlab("Basal area of all big trees in 2008-10 (m^2 / ha)") +
  ylab("Basal area of all living big trees in 2024-2025 (m^2 / ha)") + 
  geom_abline(slope = 1, intercept = 0)

# Investigate change in total non-ash basal area ###############################

# Take the difference of non ash BA in 2024-25 to non ash BA 2008-10:
BA$TOTAL_non_ash_change <- BA$TOTAL.y - BA$TOTAL_non_ash.x # In 2024-2025, the
# living ash tree basal area in the >12.5 cm DBH range was 0 m^2 / ha. Thus,
# this calculation is really the change in non-ash tree basal area.

# Graph the non-ash basal area in 2008-10 against the non-ash basal area in 2025:
ggplot(data=BA, aes(x=TOTAL_non_ash.x, y=TOTAL.y)) +
  geom_point() + theme_bw() + xlab("Basal area of non-ash big trees in 2008-10 (m^2 / ha)") +
  ylab("Basal area of living big trees in 2024-2025 (m^2 / ha)") + 
  geom_abline(slope = 1, intercept = 0)

# Graph the percent change in non-ash tree basal area against the pre-EAB basal 
# area of ash:
ggplot(data=BA, aes(x=Fraxinus.x, y=TOTAL_non_ash_change, label=Transect)) +
  geom_point() + theme_bw() + xlab("Basal area of ash big trees in 2008-10 (m^2 / ha)") +
  ylab("Change in basal area of non-ash trees \nbetween 2008-10 to 2025 (m^2 / ha)") +
  geom_text()

# Create a linear model to investigate the correlation between pre-EAB ash BA
# and the change in BA of non-ash trees:
model_TOTAL_non_ash_change <- lm(TOTAL_non_ash_change ~ Fraxinus.x, data=BA)
summary(model_TOTAL_non_ash_change)

# Also try a model which includes the basal area of non-ash trees in 2008 as
# a predictor. 
model_TOTAL_non_ash_change_1 <- lm(TOTAL_non_ash_change ~ Fraxinus.x + 
                                     TOTAL_non_ash.x, data=BA)
summary(model_TOTAL_non_ash_change_1)

# Test assumptions:
#plot(model_TOTAL_non_ash_change)

# How does the pre-EAB BA of ash compare to the pre-EAB BA of non-ash?
ggplot(data=BA, aes(x=Fraxinus.x, y=TOTAL_non_ash.x)) +
  geom_point() + theme_bw() + xlab("Basal area of ash big trees in 2008-10 (m^2 / ha)") +
  ylab("Basal area of non-ash big trees in 2008-10 (m^2 / ha)")

# Investigate changes in basal area by genus ###################################

BA$change_Acer <- BA$Acer.y - BA$Acer.x

BA$change_Ulmus <- BA$Ulmus.y - BA$Ulmus.x

BA$change_Quercus <- BA$Quercus.y - BA$Quercus.x

BA$change_Tilia <- BA$Tilia.y - BA$Tilia.x

BA$change_Larix <- BA$Larix.y - BA$Larix.x

BA$change_Populus <- BA$Populus.y - BA$Populus.x

BA$change_Betula <- BA$Betula.y - BA$Betula.x

BA$change_Fraxinus <- BA$Fraxinus.y - BA$Fraxinus.x

# Acer #########################################################################

# Graph 2008-10 BA of Acer against 2024-2025 BA of Acer:
ggplot(data=BA, aes(x=Acer.x, y=Acer.y, label=Transect)) +
  geom_point() + theme_bw() + xlab("Basal area of Acer big trees in 2008-10 (m^2 / ha)") +
  ylab("Basal area of Acer living big trees in 2024-2025 (m^2 / ha)") + 
  geom_abline(slope = 1, intercept = 0) + geom_text()

# Graph the change in basal area of Acer, versus the BA of ash trees before EAB:
ggplot(data=BA, aes(x=Fraxinus.x, y=change_Acer, label=Transect)) +
  geom_point() + theme_bw() + xlab("Basal area of ash big trees in 2008-10 (m^2 / ha)") +
  ylab("Change in basal area of Acer trees \nbetween 2008-10 to 2025 (m^2 / ha)") +
  geom_text()

model_Acer_change <- lm(change_Acer ~ Fraxinus.x, data=BA)
summary(model_Acer_change)

# Test assumptions:
plot(model_Acer_change)

# Ulmus ########################################################################

# Graph 2008-10 BA of Ulmus against 2024-2025 BA of Ulmus:
ggplot(data=BA, aes(x=Ulmus.x, y=Ulmus.y)) +
  geom_point() + theme_bw() + xlab("Basal area of Ulmus big trees in 2008-10 (m^2 / ha)") +
  ylab("Basal area of Ulmus living big trees in 2024-2025 (m^2 / ha)") + 
  geom_abline(slope = 1, intercept = 0)

# Graph the change in basal area of Ulmus, versus the BA of ash trees before EAB:
ggplot(data=BA, aes(x=Fraxinus.x, y=change_Ulmus)) +
  geom_point() + theme_bw() + xlab("Basal area of ash big trees in 2008-10 (m^2 / ha)") +
  ylab("Change in basal area of Ulmus trees \nbetween 2008-10 to 2025 (m^2 / ha)")

model_Ulmus_change <- lm(change_Ulmus ~ Fraxinus.x, data=BA)
summary(model_Ulmus_change)

# Quercus ########################################################################

# Graph 2008-10 BA of Quercus against 2024-2025 BA of Quercus:
ggplot(data=BA, aes(x=Quercus.x, y=Quercus.y)) +
  geom_point() + theme_bw() + xlab("Basal area of Quercus big trees in 2008-10 (m^2 / ha)") +
  ylab("Basal area of Quercus living big trees in 2024-2025 (m^2 / ha)") + 
  geom_abline(slope = 1, intercept = 0)

# Graph the change in basal area of Quercus, versus the BA of ash trees before EAB:
ggplot(data=BA, aes(x=Fraxinus.x, y=change_Quercus)) +
  geom_point() + theme_bw() + xlab("Basal area of ash big trees in 2008-10 (m^2 / ha)") +
  ylab("Change in basal area of Quercus trees \nbetween 2008-10 to 2025 (m^2 / ha)")

model_Quercus_change <- lm(change_Quercus ~ Fraxinus.x, data=BA)
summary(model_Quercus_change)

# Tilia ########################################################################

# Graph 2008-10 BA of Tilia against 2024-2025 BA of Tilia:
ggplot(data=BA, aes(x=Tilia.x, y=Tilia.y)) +
  geom_point() + theme_bw() + xlab("Basal area of Tilia big trees in 2008-10 (m^2 / ha)") +
  ylab("Basal area of Tilia living big trees in 2024-2025 (m^2 / ha)") + 
  geom_abline(slope = 1, intercept = 0)

# Graph the change in basal area of Tilia, versus the BA of ash trees before EAB:
ggplot(data=BA, aes(x=Fraxinus.x, y=change_Tilia)) +
  geom_point() + theme_bw() + xlab("Basal area of ash big trees in 2008-10 (m^2 / ha)") +
  ylab("Change in basal area of Tilia trees \nbetween 2008-10 to 2025 (m^2 / ha)")

model_Tilia_change <- lm(change_Tilia ~ Fraxinus.x, data=BA)
summary(model_Tilia_change)

# Make a graph: ###############################################################
new <- data.frame(Fraxinus.x=seq(0, 12.5, 0.1))
new$TOTAL_non_ash_change <- predict(model_TOTAL_non_ash_change, newdata = new)

ggplot() +
  geom_point(data=BA, aes(x=Fraxinus.x, y=TOTAL_non_ash_change)) + 
  xlab("Pre-EAB ash basal area (m^2 / ha)") +
  ylab("Change in basal area of \nnon-ash trees (m^2 / ha)") +
  geom_text(data=BA, aes(x=Fraxinus.x, y=TOTAL_non_ash_change, label=Transect),
            vjust=1.5) +
  geom_line(data=new, aes(x=Fraxinus.x, y=TOTAL_non_ash_change)) + coord_equal() +
  scale_x_continuous(breaks = seq(0, 14, by = 4)) +
  scale_y_continuous(breaks = seq(-4, 14, by = 4))

# I'd like to make a graph that has the different tree genera on the x-axis. The
# y axis would be the change in basal area for each transect. Then the points
# would be connected based on their transect.

# I first need to pivot longer:
BA_long <- pivot_longer(BA, 
  cols=c("change_Acer", "change_Ulmus", "change_Quercus", "change_Tilia",
         "change_Larix", "change_Populus", "change_Betula", "change_Fraxinus"), 
                        names_to = "Genus", values_to = "change_in_BA", 
  names_prefix = "change_")

BA_long$Genus <- factor(BA_long$Genus, levels = c("Fraxinus", "Acer", "Quercus",
      "Tilia", "Ulmus", "Populus", "Larix", "Betula"))

ggplot(data=BA_long, aes(x=Genus, y=change_in_BA, group=Transect,
                         color=Transect, shape = Transect)) + geom_point() + geom_line(linewidth = 0.7) +
  ylab("Change in basal area (m^2 / ha)")

# Regeneration or failure of ash ##############################################

# Investigate ash regeneration in 2024-2025, versus ash in canopy in 2008-10:
# I hypothesize that if there was a lot of basal area of ash in the canopy
# before EAB, then there would be a lot of regeneration.

# Import data on ash regeneration in 2024-25:
ash_2025 <- read.csv("Cleaned_data/ash_by_transect.csv")

# Join the data to the 2024-25 ash regeneration data:
BA_with_regen <- left_join(BA, ash_2025, by="Transect")

# Plot the basal area of ash in the understory in 2024-25 against basal area
# of all living and dead ash in the canopy in 2008-10:

ggplot() +
  geom_point(data=BA_with_regen, aes(x=Fraxinus.x, 
            y=mean_basal_area_living_small_trees_m_squared_per_ha),
             alpha=1) + 
  xlab("Pre-EAB ash basal area \n in canopy (m^2 / ha)") +
  ylab("Post-EAB ash living basal area \n in understory (m^2 / ha)") +
  geom_text(data=BA_with_regen, aes(x=Fraxinus.x, 
      y=mean_basal_area_living_small_trees_m_squared_per_ha, label=Transect),
            hjust = 0, vjust=1.2) + coord_equal() + 
  scale_x_continuous(breaks=seq(0,13, 1), limits = c(0,13)) +
  scale_y_continuous(breaks=seq(0,3, 1), limits=c(-0.5,3))





