# Aaron Tayal
# July 7, 2025
# The goal is to combine the 2024 and 2025 datasets. Do all the same
# things as for the 2024 data but now include the 2025 ash seedlings, saplings,
# and trees.

# Note: We re-measured some of the plots at Pontiac because the microplot PVC was
# too small in May 2024. 

library(ggplot2)
library(dplyr)


# Hydroclass -----------------------------------------------------------------

hydro <- read.csv("Raw_data/MI-plot-hydroclasses.csv")
str(hydro)
hydro$mstrlvl <- as.factor(hydro$mstrlvl)
# The column "mstrlvl" is either xeric, mesic, or hydric. All plots in one
# transect (3 plots) will have the same assignment of mstrlvl. I will also 
# call this the "hydroclass". The column "plotmstr" has a value from 1 to 5 
# (1 being driest and 5 being the most flooded). The "plotmstr" value can
# vary within a transect. I think it is based on the results of some kind of 
# soil moisture test, and also from personal experience of the moisture level
# of the plot (W. Klooster, personal communication)

# Note: "MI-plot-hydroclasses.csv" has hydroclass data for 129 plots, including
# some that I do not plan to visit because they are designated "non-ash" plots
# or they are at Brighton.

# Plot centers - NEEDS UPDATED 2025 --------------------------------------------

plot_centers0 <- read.csv("Raw_data/EAB_Michigan_2024_plot_centers.csv")
str(plot_centers0)
# Kayla: for the center_tree column, I would encourage to split that into two 
# (one for species and one for dbh) same for the center tree updated column

# There are 111 plots (37 transects) that I plan to visit (and 2 extra rows due 
# to confusion about the true center for plot 75):
nrow(plot_centers0)

# Remove the 2 additional rows listing info on 75_north and 75_south:
plot_centers0 <- plot_centers0[plot_centers0$center_tree_number != "75_north",]
plot_centers0 <- plot_centers0[plot_centers0$center_tree_number != "75_south",]

nrow(plot_centers0)

# Make transect and park into factors:
plot_centers0$Transect <- as.factor(plot_centers0$Transect)
plot_centers0$Park <- as.factor(plot_centers0$Park)

# Make center tree number into an integer:
plot_centers0$center_tree_number <- as.integer(plot_centers0$center_tree_number)

# There are 98 plots that I have collected seedling data (not counting 75_north):
sum(plot_centers0$seedlings_done_y_n == "y")

# Similarly, there are 98 plots that I have collected sapling data (not counting 
# 75_north):
sum(plot_centers0$saplings_done_y_n == "y")

# There are 97 plots that I have collected tree data for (not counting 75_north).
# At plot 91 at Hudson Mills, we ran out of time to collect tree data due to a 
# thunderstorm
sum(plot_centers0$trees_done_y_n == "y")

# There are 2 plots where we found and measured the center tree but did not 
# collect any seedling, sapling, or tree data due to high waters and a time 
# crunch. These were plots 23 and 66, both at Island Lake.

# This leaves 11 plots that we have neither found the center or recorded data
# for. Four at Hudson Mills, six at Indian Springs, and one at Island Lake. 
# Note: we technically found plot 61 at Island Lake, but I forgot to write
# the center tree DBH, and thus I'm considering it completely not-started.

# Summary: 97 done + 3 partially done + 11 not-started = 111 plots

# Join the hydroclass data into the plot_centers dataframe:
plot_centers <- right_join(hydro, plot_centers0)
nrow(plot_centers) == nrow(plot_centers0)

# Try to understand which hydroclasses (hydric, mesic, xeric) are found at 
# each Park:
table((plot_centers %>% filter(Park == "Highland"))$mstrlvl)
table((plot_centers %>% filter(Park == "Hudson Mills"))$mstrlvl)
table((plot_centers %>% filter(Park == "Indian Springs"))$mstrlvl)
table((plot_centers %>% filter(Park == "Island Lake"))$mstrlvl)
table((plot_centers %>% filter(Park == "Kensington"))$mstrlvl)
table((plot_centers %>% filter(Park == "Pontiac Lake"))$mstrlvl)
table((plot_centers %>% filter(Park == "Proud Lake"))$mstrlvl)
table(plot_centers$mstrlvl)

#write.csv(plot_centers, file="EAB_Michigan_2024_plot_centers_with_hydro.csv",
#          row.names = FALSE)

# Seedlings -------------------------------------------------------------------

seedlings <- read.csv("Raw_data/EAB_Michigan_2024_2025_ash_seedlings.csv")
str(seedlings)
# At one plot (75 at Indian Springs), we found two center trees marked 75.
# The true, original plot 75 is the one I want to use. I called this one 
# "75_south" in my datasheet this year. So I need to remove "75_north" rows and
# rename "75_south" rows as simply "75".
seedlings <- seedlings[seedlings$center_tree_number != "75_north",]
seedlings$center_tree_number[seedlings$center_tree_number == "75_south"] <- "75"
str(seedlings)

seedlings$center_tree_number <- as.integer(seedlings$center_tree_number)

# To combine the data on plot centers with the seedling data, I want to
# keep all rows in seedlings:
seedlings2 <- right_join(plot_centers, seedlings, by="center_tree_number")
# Test if the number of rows are the same,
# indicating that the datasets combined sucessfully:
nrow(seedlings2) == nrow(seedlings)

# We recorded data for each quadrant of each microplot, so I need to add up the
# four quadrants for short seedlings (those that are shorter than 25 cm):
seedlings2$number_short <- 
  seedlings2$northeast_number_established_short +
  seedlings2$southeast_number_established_short +
  seedlings2$southwest_number_established_short +
  seedlings2$northwest_number_established_short

# Same for tall seedlings:
seedlings2$number_tall <- 
  seedlings2$northeast_number_established_tall +
  seedlings2$southeast_number_established_tall +
  seedlings2$southwest_number_established_tall +
  seedlings2$northwest_number_established_tall

# Now make a column for the total number of seedlings (both short and tall):
seedlings2$number_seedlings <- seedlings2$number_short + seedlings2$number_tall

# Now I need to divide the raw counts by the area of the circular microplot to
# get the density. The density will be in units of stems per meter^2. 
# Note: because our first trip used a slightly smaller microplot to record 
# seedling numbers, I have a column to record the area of the microplot used.
seedlings2$density_short <-
  (seedlings2$number_short / seedlings2$area_of_microplot_m_squared)

seedlings2$density_tall <-
  (seedlings2$number_tall / seedlings2$area_of_microplot_m_squared)

seedlings2$density_seedlings <- 
  (seedlings2$number_seedlings / seedlings2$area_of_microplot_m_squared)

# Note: We re-measured some of the plots at Pontiac and Kensington in 2025 
# because the microplot PVC was too small in May 2024. I need to make sure these 
# measurements are roughly equivalent.
table(seedlings2$center_tree_number) # Some were measured twice
sum(table(seedlings2$center_tree_number) == 8) # 12 plots
remeasured_plots <- c(1, 2, 3, 49, 50, 53, 94, 95, 96, 97, 98, 99)

remeasured_seedlings_2024 <- seedlings2 %>% 
  filter(year_of_data_collection==2024 & center_tree_number %in% remeasured_plots)
remeasured_seedlings_2025 <- seedlings2 %>% 
  filter(year_of_data_collection==2025 & center_tree_number %in% remeasured_plots)

plot(remeasured_seedlings_2024$number_seedlings, remeasured_seedlings_2025$number_seedlings)
lm(remeasured_seedlings_2025$number_seedlings ~ remeasured_seedlings_2024$number_seedlings)

plot(remeasured_seedlings_2024$density_seedlings, remeasured_seedlings_2025$density_seedlings)
lm(remeasured_seedlings_2025$density_seedlings ~ remeasured_seedlings_2024$density_seedlings)

# I'm just going to go with the 2025 data, because that used the correct
# microplot size that was used for all the other plots. Take the 2024 plots
# out that were remeasured in 2025:
seedlings2 <- seedlings2 %>% 
  filter(!(year_of_data_collection==2024 & center_tree_number %in% remeasured_plots))

# For a common sense check, I'll plot the density of short seedlings against
# the density of tall seedlings.  I predict (prior to seeing it) that they 
# will be correlated.
plot(seedlings2$density_short,
     seedlings2$density_tall)

# I will plot histograms for short and tall seedlings. I want to know what is 
# the qualitative shape of the distribution:
hist(seedlings2$density_short, breaks=100)
hist(seedlings2$density_tall, breaks=100)

hist(seedlings2$number_short, breaks=seq(0,50,1)) # I wanted to see the count data, too
hist(seedlings2$number_tall, breaks=seq(0,50,1))
hist(seedlings2$number_seedlings, breaks=seq(0,60,1))

# Note: the stem density variables are pseudo-continuous, but really they are
# discrete, because the counts are discrete, and so there are only a limited
# number of values that the density can take.

# Summarise seedlings by plot #################################################

# I want to take an average of the 4 microplots in order to report data at the 
# plot level. 

# IMPORTANT NOTE: sometimes, we forgot to write the percent cover
# for 1 out of 4 of the microplots. I have instructed the code to strip NA values
# before taking the mean.

seedlings_by_plot <- seedlings2 %>% group_by(center_tree_number) %>% 
  summarise(number_microplots = n(),
            mean_percent_cover_seedlings = mean(percent_cover_0_0.5_1_3.5_8_15.5_25.5_etc, 
                                                na.rm=TRUE),
            mean_density_short_seedlings = mean(density_short),
            mean_density_tall_seedlings = mean(density_tall),
            mean_density_seedlings = mean(density_seedlings),
            total_number_short_seedlings = sum(number_short),
            total_number_tall_seedlings = sum(number_tall),
            total_number_seedlings = sum(number_seedlings))

# Create an overall data table at the plot level that records all variables
ash_by_plot1 <- left_join(plot_centers, seedlings_by_plot, by="center_tree_number")

# Saplings --------------------------------------------------------------------

saplings <- read.csv("Raw_data/EAB_Michigan_2024_2025_ash_saplings.csv")
str(saplings)
# Remove 75_north and rename 75_south to 75 (see seedlings for details)
saplings <- saplings[saplings$center_tree_number != "75_north",]
saplings$center_tree_number[saplings$center_tree_number == "75_south"] <- "75"

saplings$center_tree_number <- as.integer(saplings$center_tree_number)

# Recode the ash species presence or absence as 0 or 1
saplings$black_ash_present_0_1 <- 
  ifelse(saplings$black_ash_present_y_n == "y", 1,
         ifelse(saplings$black_ash_present_y_n == "n", 0, NA))

saplings$green_white_pumpkin_ash_present_0_1 <- 
  ifelse(saplings$green_white_pumpkin_ash_present_y_n == "y", 1,
         ifelse(saplings$green_white_pumpkin_ash_present_y_n == "n", 0, NA))

# Compute the number of saplings showing signs or symptoms of EAB:
saplings$number_stems_with_signs_symptoms <- round(
  saplings$number_of_stems * saplings$percent_stems_with_signs_or_symptoms / 100,
  digits = 0)

# What was the most common sign or symptom for ash saplings?
sum(saplings$number_of_stems != 0) # 170 subplot quadrants where nonzero numbers of ash saplings
sum(saplings$EAB_exit_holes_y_n == "y", na.rm=TRUE) # no exit holes were found on saplings
sum(saplings$woodpecker_marks_y_n == "y", na.rm=T) # 1 quadrant with woodpecker marks on saplings
sum(saplings$ash_bark_splitting_y_n == "y", na.rm=T) # 38 quadrants with ash bark splitting on saplings
sum(saplings$epicormic_sprouts_y_n == "y", na.rm = T) # 10 quadrants with epicormics on saplings
sum(saplings$basal_sprouts_y_n == "y", na.rm = T) # 6 quadrants with basal sprouts on saplings

# Trying to add in the plot center info to this dataframe:
saplings2 <- right_join(plot_centers, saplings, by="center_tree_number")
nrow(saplings) == nrow(saplings2)


# Saplings were counted in each quadrant, so I want to sum up the quadrants
# to get a total number of saplings by plot
saplings_by_plot <- saplings2 %>% group_by(center_tree_number) %>% 
  summarise(number_subplot_quadrants=n(),
            number_saplings = sum(number_of_stems),
            number_subplot_quadrants_with_black_ash_saplings = 
              sum(black_ash_present_0_1, na.rm = F),
            number_subplot_quadrants_with_green_white_pumpkin_ash_saplings = 
              sum(green_white_pumpkin_ash_present_0_1, na.rm = F),
            number_saplings_with_signs_symptoms = 
              sum(number_stems_with_signs_symptoms, na.rm=T))

# Make a dataframe that shows all the plots with nonzero numbers of saplings:
plots_with_nonzero_saplings <- saplings_by_plot %>% filter(number_saplings != 0)

# To find the density of saplings, I need to divide the number of saplings 
# found in the subplot (8 meters in radius) by the area of that subplot. 
# The area of the subplot is 201.062 m^2. The unit will be stems per m^2.
saplings_by_plot$density_saplings_stems_per_m_squared <- 
  saplings_by_plot$number_saplings / 201.062

# Join saplings_by_plot to the overall ash_by_plot dataframe:
ash_by_plot2 <- left_join(ash_by_plot1, saplings_by_plot, by="center_tree_number")

# Small trees and trees -------------------------------------------------------

trees <- read.csv("Raw_data/EAB_Michigan_2024_2025_ash_trees.csv")

# Remove 75_north and rename 75_south to 75 (see seedlings for details)
trees <- trees[trees$center_tree_number != "75_north",]
trees$center_tree_number[trees$center_tree_number == "75_south"] <- "75"

trees$center_tree_number <- as.integer(trees$center_tree_number)
trees$ash_species <- as.factor(trees$ash_species)
trees$canopy_condition_1_5 <- as.factor(trees$canopy_condition_1_5)

# Add in plot center data
trees2 <- right_join(plot_centers, trees, by="center_tree_number")
nrow(trees) == nrow(trees2)

# Remove any rows where the species of tree is not ash (keep rows where 
# ash_species is NA)
# Note: the hickory and Zanthoxylum rows were tagged trees
levels(trees2$ash_species)
trees2 <- trees2[( trees2$ash_species != "hickory" | is.na(trees2$ash_species) ), ]
trees2 <- trees2[( trees2$ash_species != "Zanthoxylum (prickly ash)" | is.na(trees2$ash_species) ), ]
trees2$ash_species <- droplevels(trees2$ash_species)
levels(trees2$ash_species)

# I want to make a column that simplifies ash_species so that there are only 
# three categories: black, green/white/pumpkin, and unknown
trees2$ash_species_simple <- as.character(trees2$ash_species)
trees2$ash_species_simple[trees2$ash_species_simple == "green"] <- "green_white_or_pumpkin"
trees2$ash_species_simple[trees2$ash_species_simple == "green or pumpkin"] <- "green_white_or_pumpkin"
trees2$ash_species_simple[trees2$ash_species_simple == "green or white"] <- "green_white_or_pumpkin"
trees2$ash_species_simple[trees2$ash_species_simple == "green white or pumpkin"] <- "green_white_or_pumpkin"
trees2$ash_species_simple[trees2$ash_species_simple == "black?"] <- "unknown"
trees2$ash_species_simple[trees2$ash_species_simple == "?"] <- "unknown"
trees2$ash_species_simple[trees2$ash_species_simple == "green?"] <- "unknown"
trees2$ash_species_simple[trees2$ash_species_simple == "white?"] <- "unknown"
trees2$ash_species_simple <- as.factor(trees2$ash_species_simple)
table(trees2$ash_species_simple)

# Are all the center tree numbers that I visited covered in the ash tree dataset?
sort(unique(trees2$center_tree_number)) # Yes they are. I did not visit
# Brighton

# Make sure the variable that records which quadrant the tree was in is accurate:
trees2$quadrant_NE_SE_SW_NW <- as.factor(trees2$quadrant_NE_SE_SW_NW)
summary(trees2$quadrant_NE_SE_SW_NW)
trees2[trees2$quadrant_NE_SE_SW_NW == "?",] # The only rows with ? for quadrant
# are about 100 meters from the center (and so will be filtered out)

# Which rows are simply recording the absence of any trees?
c <- trees2$center_tree_number[trees2$quadrant_NE_SE_SW_NW == "none"] # center tree #s of rows recording the absence of a tree
c
unique(c)
d <- trees2$center_tree_number[trees2$quadrant_NE_SE_SW_NW != "none"] # center tree #s of rows recording the presence of a tree
unique(d)
intersect(unique(c), unique(d)) # Make sure no trees are recorded from plots
# also marked as having no trees.

# We only recorded compass direction at the beginning of summer 2024
# (it was a lot of work). But we can at least check the data against quadrant
# to see if they agree
trees2$compass_direction <- as.numeric(trees2$compass_direction)
ggplot(data=trees2, aes(x=quadrant_NE_SE_SW_NW, y=compass_direction)) +
  geom_point() +
  geom_hline(yintercept=90) +
  geom_hline(yintercept=180) +
  geom_hline(yintercept=270)
# Looks like there are three rows where compass direction and quadrant disagree:
trees2[trees2$compass_direction > 270 & trees2$quadrant_NE_SE_SW_NW != "NW" &
         is.na(trees2$compass_direction) == FALSE , ]
# All of these three rows were tagged ash. When it comes time to identify tagged
# ash in datasets from previous years, we will know about this issue. I'm not
# going to try to change anything right now.

# I want to create a new dataframe of trees so that small trees (2.5-10 cm DBH)
# are counted only if they occur within the 8 meter radius subplot. Furthermore,
# trees (10 cm DBH and up) are counted only if they occur within the 18 meter
# radius main plot

# First I need to change the variable distance_to_center_meters into a 
# numeric variable.  For many of the trees inside the 8 meter subplot, we
# simply wrote "less than 8" to indicate it was in the subplot. So now I 
# will name these as 4 (4 meters) even though they ranged from 0 m to 8 m
trees2$distance_to_center_meters_simple <- trees2$distance_to_center_meters
trees2$distance_to_center_meters_simple[trees2$distance_to_center_meters_simple == "less than 8"] <- "4"
trees2$distance_to_center_meters_simple[trees2$distance_to_center_meters_simple == "between 8 and 18"] <- "13"
trees2$distance_to_center_meters_simple[trees2$distance_to_center_meters_simple == "greater than 18"] <- "25"
trees2$distance_to_center_meters_simple[trees2$distance_to_center_meters_simple == "approx 100 m"] <- "100"
trees2$distance_to_center_meters_simple <- as.numeric(trees2$distance_to_center_meters_simple)
hist(trees2$distance_to_center_meters_simple, breaks=100)

# Filter small and big trees ###################################################

# Small trees must be >= 2.5 cm DBH AND < 10 cm DBH AND distance to the center
# must be <= 8 meters
small_trees <- trees2 %>% dplyr::filter(quadrant_NE_SE_SW_NW != "none") %>%
  dplyr::filter(diameter_at_137_cm_in_cm >= 2.5) %>%
  dplyr::filter(diameter_at_137_cm_in_cm < 10) %>%
  dplyr::filter(distance_to_center_meters_simple <= 8)
plot(small_trees$diameter_at_137_cm_in_cm)
hist(small_trees$diameter_at_137_cm_in_cm, breaks=50)
plot(small_trees$distance_to_center_meters_simple)

# Big trees must be >= 10 cm DBH AND distance to the center must be <= 18 meters
big_trees <- trees2 %>% dplyr::filter(quadrant_NE_SE_SW_NW != "none") %>%
  dplyr::filter(diameter_at_137_cm_in_cm >= 10) %>%
  dplyr::filter(distance_to_center_meters_simple <= 18)
plot(big_trees$diameter_at_137_cm_in_cm) # Note the one giant dead standing ash 
# tree. The rest of the trees are under 15 cm DBH.
plot(big_trees$distance_to_center_meters_simple)

# Make a dataframe that combines the (individual observations of) small trees 
# and big trees:
all.equal(colnames(small_trees), colnames(big_trees)) # column names are the same
small_and_big_trees <- bind_rows(small_trees, big_trees)

#write.csv(small_and_big_trees, file="Cleaned_data/individual_trees.csv", row.names = FALSE)

# Summarise ash tree occurence by plot #########################################

# Now, create a summary of how many ash small trees were found in each plot
# and what the basal area was. To calculate basal area, take the diameters, 
# and convert to areas and add them up, then convert the units from cm^2 to 
# meters^2 by dividing by 10,000. Note: I have not yet divided
# by the area of the subplot yet.
small_trees_by_plot <- small_trees %>% group_by(center_tree_number) %>%
  summarise(number_small_trees = n(),
            number_small_trees_green_white_or_pumpkin = sum(ash_species_simple=="green_white_or_pumpkin"),
            number_small_trees_black = sum(ash_species_simple=="black"),
            number_small_trees_unknown_species = sum(ash_species_simple=="unknown"),
            basal_area_small_trees_in_m_squared = 
              sum(pi * (diameter_at_137_cm_in_cm / 2) ^ 2) / 10000,
            number_living_small_trees = sum(canopy_condition_1_5 != 5 | is.na(canopy_condition_1_5)), # Note: there is one tree with no canopy condition entered (we forgot), but it
            # can be inferred by the comments that this tree was alive
            basal_area_living_small_trees_in_m_squared = 
              sum(pi * ((ifelse(canopy_condition_1_5 != 5 | is.na(canopy_condition_1_5), 
                                diameter_at_137_cm_in_cm, 0)) / 2) ^ 2) / 10000,
            
            number_healthy_small_trees = sum(canopy_condition_1_5 == 1, na.rm = TRUE),
            number_declining_small_trees = sum(canopy_condition_1_5 %in% c(2,3,4), na.rm = TRUE),
            number_dead_small_trees = sum(canopy_condition_1_5 == 5, na.rm = TRUE)
  )


# Now, for any center tree numbers not mentioned in small_trees_by_plot,
# but that are in trees2, I'd like to put another row with a zero in it:
b <- sort(unique(trees2$center_tree_number))
b # b is all the center tree numbers where presence/absence of trees was recorded
e <- small_trees_by_plot$center_tree_number
e # e is all the center tree numbers where small trees were found in the subplot
f <- sort(setdiff(b, e)) # f is all the center tree numbers were zero small
# trees were found in the subplot
f

# Add a row for each center tree where no small trees were found in the subplot:
for (k in (1:length(f))){
  info <- data.frame(center_tree_number=f[k])
  info$number_small_trees <- 0
  info$number_small_trees_green_white_or_pumpkin <- 0
  info$number_small_trees_black <- 0
  info$number_small_trees_unknown_species <- 0
  info$basal_area_small_trees_in_m_squared <- 0
  info$number_living_small_trees <- 0
  info$basal_area_living_small_trees_in_m_squared <- 0
  info$number_healthy_small_trees <- 0
  info$number_declining_small_trees <- 0
  info$number_dead_small_trees <- 0
  small_trees_by_plot <- small_trees_by_plot %>% bind_rows(info)
}

# Reorder the rows by center_tree_number
small_trees_by_plot <- small_trees_by_plot %>% arrange(center_tree_number)

small_trees_by_plot$center_tree_number

# To find the density of small trees, I need to divide the number of them 
# found in the subplot (8 meters in radius) by the area of that subplot. 
# The area of the subplot is 201.062 m^2. Then I'll multiply by 10,000 to convert
# to hectares:
small_trees_by_plot$density_small_trees_stems_per_ha <- 
  ( small_trees_by_plot$number_small_trees / 201.062 ) * 10000 # 10,000 m^2 in 1 ha

# Now find the basal area of small trees in meters^2 per hectare. I do this by 
# dividing by the area of the subplot over which small trees were recorded and
# then multiplying by 10,000 to convert into meters^2 of basal area per hectare
small_trees_by_plot$basal_area_small_trees_m_squared_per_ha <-
  ( small_trees_by_plot$basal_area_small_trees_in_m_squared / 201.062 ) * 10000

# Find the density of living small trees:
small_trees_by_plot$density_living_small_trees_stems_per_ha <- 
  ( small_trees_by_plot$number_living_small_trees / 201.062 ) * 10000

# Find the basal area of living small trees in m^2 / ha:
small_trees_by_plot$basal_area_living_small_trees_m_squared_per_ha <-
  ( small_trees_by_plot$basal_area_living_small_trees_in_m_squared / 201.062 ) * 10000


# Now the same type of summary for big trees
big_trees_by_plot <- big_trees %>% group_by(center_tree_number) %>%
  summarise(number_big_trees = n(),
            number_living_big_trees = sum(canopy_condition_1_5 != 5),
            basal_area_big_trees_in_m_squared = 
              sum(pi * (diameter_at_137_cm_in_cm / 2) ^ 2) / 10000,
            basal_area_living_big_trees_in_m_squared = 
              sum(pi * ((ifelse(canopy_condition_1_5 != 5, 
                                diameter_at_137_cm_in_cm, 0)) / 2) ^ 2) / 10000
  )

# Now merge big_trees_by_plot into the small_trees_by_plot dataframe:
trees_by_plot <- full_join(small_trees_by_plot, 
                           big_trees_by_plot, by="center_tree_number")
# Replace NAs with zeros for number of big trees and basal area of big trees:
trees_by_plot$number_big_trees[is.na(trees_by_plot$number_big_trees)] <- 0
trees_by_plot$number_living_big_trees[is.na(trees_by_plot$number_living_big_trees)] <- 0
trees_by_plot$basal_area_big_trees_in_m_squared[is.na(trees_by_plot$basal_area_big_trees_in_m_squared)] <- 0
trees_by_plot$basal_area_living_big_trees_in_m_squared[is.na(trees_by_plot$basal_area_living_big_trees_in_m_squared)] <- 0

# Calculate the density of big trees in stems per hectare:
trees_by_plot$density_big_trees_stems_per_ha <- 
  ( trees_by_plot$number_big_trees / 1017.876 ) * 10000

# Calculate the basal area of big trees in meters^2 per hectare
trees_by_plot$basal_area_big_trees_m_squared_per_ha <-
  ( trees_by_plot$basal_area_big_trees_in_m_squared / 1017.876 ) * 10000

# Calculate the density of *living* big trees in stems per hectare:
trees_by_plot$density_living_big_trees_stems_per_ha <- 
  ( trees_by_plot$number_living_big_trees / 1017.876 ) * 10000

# Calculate the basal area of *living* big trees in meters^2 per hectare
trees_by_plot$basal_area_living_big_trees_m_squared_per_ha <-
  ( trees_by_plot$basal_area_living_big_trees_in_m_squared / 1017.876 ) * 10000

# Combine trees_by_plot with the other ash data:
ash_by_plot <- left_join(ash_by_plot2, trees_by_plot, by="center_tree_number")

# Write the overall dataframe, ash_by_plot, to a csv file
#write.csv(ash_by_plot, file="Cleaned_data/ash_by_plot.csv", row.names = FALSE)

# Summarise ash occurence by transect #########################################

table(ash_by_plot[ash_by_plot$seedlings_done_y_n == "n","Transect"]) 
table(ash_by_plot[ash_by_plot$saplings_done_y_n == "n","Transect"]) 
table(ash_by_plot[ash_by_plot$trees_done_y_n == "n","Transect"]) 
# Note that not all three plots had been visited in 2024 for transects:
# AA (1 visited), DD (2 visited), P (2 visited), 
# Q (2 visited), Z (1 visited but not completed for trees), ZD (0 visited), 
# and ZE (0 visited). 

# UPDATE: ALL PLOTS HAVE BEEN COMPLETED by 2025. THE PLOT CENTERS DATASET NEEDS TO BE 
# UPDATED.

# Transect-level summary 
ash_by_transect <- ash_by_plot %>% group_by(Transect) %>%
  summarize(
    number_of_plots = n(),
    Park=first(Park),
    mstrlvl = first(mstrlvl),
    mean_plotmstr = mean(plotmstr),
    
    # seedlings:
    number_microplots = sum(number_microplots),
    mean_percent_cover_seedlings = mean(mean_percent_cover_seedlings),
    mean_density_short_seedlings = mean(mean_density_short_seedlings), # units are stems/m^2
    mean_density_tall_seedlings = mean(mean_density_tall_seedlings), # stems/m^2
    mean_density_seedlings = mean(mean_density_seedlings), # stems/m^2
    total_number_short_seedlings = sum(total_number_short_seedlings),
    total_number_tall_seedlings = sum(total_number_tall_seedlings),
    total_number_seedlings = sum(total_number_seedlings),
    
    # saplings:
    number_subplot_quadrants = sum(number_subplot_quadrants),
    total_number_saplings = sum(number_saplings),
    mean_density_saplings_stems_per_m_squared = mean(density_saplings_stems_per_m_squared), # stems/m^2
    total_number_saplings_with_signs_symptoms = sum(number_saplings_with_signs_symptoms),
    
    # small trees:
    total_number_small_trees = sum(number_small_trees),
    total_number_small_trees_green_white_or_pumpkin = sum(number_small_trees_green_white_or_pumpkin),
    total_number_small_trees_black = sum(number_small_trees_black),
    total_number_small_trees_unknown_species = sum(number_small_trees_unknown_species),
    total_number_living_small_trees = sum(number_living_small_trees),
    mean_density_small_trees_stems_per_ha = mean(density_small_trees_stems_per_ha),
    mean_density_living_small_trees_stems_per_ha = mean(density_living_small_trees_stems_per_ha),
    mean_basal_area_small_trees_m_squared_per_ha = mean(basal_area_small_trees_m_squared_per_ha),
    mean_basal_area_living_small_trees_m_squared_per_ha = mean(basal_area_living_small_trees_m_squared_per_ha),
    
    # big trees:
    total_number_big_trees = sum(number_big_trees),
    total_number_living_big_trees = sum(number_living_big_trees),
    mean_density_living_big_trees_stems_per_ha = mean(density_living_big_trees_stems_per_ha),
    mean_basal_area_living_big_trees_m_squared_per_ha = mean(basal_area_living_big_trees_m_squared_per_ha)
  )

#write.csv(ash_by_transect, file="Cleaned_data/ash_by_transect.csv", row.names = FALSE)

# Summarize ash occurrence by hydroclass ######################################
# I'd like to make a data table that shows the mean and standard error of 
# each of the ash density variables. This will become a table in my results.

ash_by_transect %>%
  group_by(mstrlvl) %>%
  summarize(
    n = n(),
    mean_density = mean(mean_density_seedlings),
    sd_density = sd(mean_density_seedlings),
    se_density = sd_density / sqrt(n)
  )

ash_by_transect %>% group_by(mstrlvl) %>%
  summarize(mean_percent_cover_seedlings = mean(mean_percent_cover_seedlings),
            std_error_percent_cover_seedlings = sd(mean_percent_cover_seedlings))

ash_by_hydro <- ash_by_transect %>% group_by(mstrlvl) %>%
  summarize(
    number_of_transects = n(),
    mean_plotmstr = mean(mean_plotmstr),
    
    # seedlings:
    number_microplots = sum(number_microplots),
    avrg_perc_cov_seedl = mean(mean_percent_cover_seedlings),
    stder_perc_cov_seedl = sd(mean_percent_cover_seedlings) / sqrt(n()),
    
    avrg_density_short_seedl = mean(mean_density_short_seedlings), # units are stems/m^2
    stder_density_short_seedl = sd(mean_density_short_seedlings) / sqrt(n()),
    
    avrg_density_tall_seedlings = mean(mean_density_tall_seedlings), # stems/m^2
    stder_density_tall_seedl = sd(mean_density_tall_seedlings) / sqrt(n()),
    
    avrg_density_seedl = mean(mean_density_seedlings), # stems/m^2
    stder_density_seedl = sd(mean_density_seedlings) / sqrt(n()),
    
    total_number_short_seedlings = sum(total_number_short_seedlings),
    total_number_tall_seedlings = sum(total_number_tall_seedlings),
    total_number_seedlings = sum(total_number_seedlings),
    
    # saplings:
    number_subplot_quadrants = sum(number_subplot_quadrants),
    
    avrg_density_saplings_stems_per_ha = 10000 * mean(mean_density_saplings_stems_per_m_squared), # stems/m^2
    stder_density_saplings_stems_per_ha = 10000 * sd(mean_density_saplings_stems_per_m_squared) / sqrt(n()),
    
    # small trees:
    
    avrg_density_small_trees_stems_per_ha = mean(mean_density_small_trees_stems_per_ha),
    
    avrg_density_living_small_trees_stems_per_ha = mean(mean_density_living_small_trees_stems_per_ha),
    stder_density_living_small_trees_stems_per_ha = sd(mean_density_living_small_trees_stems_per_ha) / sqrt(n()),
    
    avrg_basal_area_living_small_trees_m_squared_per_ha = mean(mean_basal_area_living_small_trees_m_squared_per_ha),
    stder_basal_area_living_small_trees_m_squared_per_ha = sd(mean_basal_area_living_small_trees_m_squared_per_ha) / 
      sqrt(n()),
    
    # big trees:
    
    avrg_density_living_big_trees_stems_per_ha = mean(mean_density_living_big_trees_stems_per_ha),
    stder_density_living_big_trees_stems_per_ha = sd(mean_density_living_big_trees_stems_per_ha) / sqrt(n()),
    
    avrg_basal_area_living_big_trees_m_squared_per_ha = mean(mean_basal_area_living_big_trees_m_squared_per_ha),
    stder_basal_area_living_big_trees_m_squared_per_ha = sd(mean_basal_area_living_big_trees_m_squared_per_ha) / 
      sqrt(n())
    
      )

#write.csv(ash_by_hydro, file="Cleaned_data/ash_by_hydro.csv", row.names = FALSE)

# NEEDS WORK Graph the data ###################################################

# How many plots had ash of any size class?
ash_by_plot$sum_of_seedlings_saplings_living_small_big_trees <-
  ash_by_plot$total_number_seedlings +
  ash_by_plot$number_saplings +
  ash_by_plot$number_living_small_trees +
  ash_by_plot$number_living_big_trees
sum(ash_by_plot$sum_of_seedlings_saplings_living_small_big_trees != 0, na.rm = T) # 105

# I want to make a violin plot that shows the distribution of seedling densities
# for each of the three hydroclasses (xeric, mesic, and hydric)
ggplot(data=seedlings2, aes(x=factor(mstrlvl), 
                            y=density_seedlings)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of ash seedlings \n in microplots" ~ (stems/m^2))) + theme_classic()

# Make a graph that shows the seedling densities by hydroclass, at the plot level
ggplot(data=ash_by_plot, aes(x=factor(mstrlvl), y=mean_density_seedlings)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass of plot") +
  ylab(bquote("Density of ash seedlings \n (stems/m^2)")) +
  scale_y_continuous(breaks=seq(0,9,2)) +
  theme_bw() + theme(text = element_text(size = 15))

# Make a similar graph for short seedlings (under 25 cm):
ggplot(data=ash_by_plot, aes(x=factor(mstrlvl), y=mean_density_short_seedlings)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of short ash seedlings <25 cm  " ~ (stems/m^2))) +
  theme_bw()

# Same for tall seedlings:
ggplot(data=ash_by_plot, aes(x=factor(mstrlvl), y=mean_density_tall_seedlings)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of tall ash seedlings 25-137 cm  " ~ (stems/m^2))) +
  theme_bw()

# And for percent cover:
ggplot(data=ash_by_plot, aes(x=factor(mstrlvl), y=mean_percent_cover_seedlings)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab("Percent cover of ash seedlings (%)") +
  theme_bw()

# Make a graph that shows the seedling densities by hydroclass, at the transect level:
ggplot(data=ash_by_transect, aes(x=mstrlvl, y=mean_density_seedlings)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of ash seedlings " ~ (stems/m^2))) +
  scale_y_continuous(breaks=seq(0,9,2)) +
  theme_bw()

# Make a graph that shows the number of seedlings by hydroclass, at the transect level:
ggplot(data=ash_by_transect, aes(x=mstrlvl, y=total_number_seedlings)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab("Total number of ash seedlings (stems)") +
  theme_bw()

# Make a graph that shows the percent cover by hydroclass, at the transect level
ggplot(data=ash_by_transect, aes(x=mstrlvl, y=mean_percent_cover_seedlings)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab("Percent cover of ash seedlings (%)") +
  theme_bw()

# Make a graph that shows the seedling densities by park, at the transect level:
ggplot(data=ash_by_transect, aes(x=Park, y=mean_density_seedlings)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of ash seedlings " ~ (stems/m^2))) +
  scale_y_continuous(breaks=seq(0,9,2)) +
  theme_bw()

# Now, make a violin plot that shows the density of saplings found at each plot, 
# as a function of hydroclass
ggplot(data=ash_by_plot, aes(x=factor(mstrlvl), 
                             y=density_saplings_stems_per_m_squared)) + 
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass of plot") +
  ylab(bquote("Density of ash saplings \n(stems/m^2)")) +
  theme_bw() + theme(text = element_text(size = 15))

# Now, make a violin plot that shows the mean density of saplings found at each 
# transect, as a function of hydroclass
ggplot(data=ash_by_transect, aes(x=mstrlvl, 
                                 y=mean_density_saplings_stems_per_m_squared)) + 
  geom_violin() + geom_jitter(height=0, width=0.1, alpha=0.5) + xlab("Hydroclass") +
  ylab(bquote("Density of ash saplings " ~ (stems/ha))) + theme_bw()

# Now, make a violin plot that shows the density of small trees found at each 
# plot, as a function of hydroclass
ggplot(data=ash_by_plot, 
       aes(x=mstrlvl, y=density_small_trees_stems_per_ha)) + 
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of ash small trees " ~ (stems/hectare))) +
  theme_bw()

# Make a violin plot that shows the density of small trees and big trees
# found at each plot, as a function of hydroclass
ggplot(data=ash_by_plot, 
       aes(x=mstrlvl, y=density_living_small_trees_stems_per_ha + 
             density_living_big_trees_stems_per_ha)) + 
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass of plot") +
  ylab("Combined density of living \nunderstory and overstory ash \n(stems/hectare)") +
  theme_bw() + theme(text = element_text(size = 15))


# Plot the basal area per hectare found at each plot as a function of hydroclass:
ggplot(data=ash_by_plot, aes(x=mstrlvl,
                             y=basal_area_small_trees_m_squared_per_ha)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Basal area of ash small trees " ~ (m^2/hectare))) +
  theme_bw()

# Now, make a violin plot that shows the number of big trees found at each 
# plot, as a function of hydroclass
ggplot(data=ash_by_plot, aes(x=mstrlvl, 
                             y=density_big_trees_stems_per_ha)) + 
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of ash big trees " ~ (stems/ha))) +
  theme_bw()

ggplot(data=ash_by_transect, aes(x=mstrlvl, 
                                 y=mean_density_small_trees_stems_per_ha)) + 
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of ash small trees " ~ (stems/hectare))) +
  theme_bw()

# Graph the basal area per ha of small trees by hydroclass, at the transect level:
ggplot(data=ash_by_transect, aes(x=mstrlvl, 
                                 y=mean_basal_area_small_trees_m_squared_per_ha)) + 
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Basal area of ash small trees " ~ (m^2/hectare))) +
  theme_bw()

# Ash tree histograms and scatters #############################################

# How many trees were found of various diameter classes, colored by ash species?
species_colors <- c(black="black", green_white_or_pumpkin="darkgreen", unknown="lightblue")
ggplot(data=small_and_big_trees) +
  geom_histogram(aes(x=diameter_at_137_cm_in_cm, fill=ash_species_simple),
                 position="stack", breaks=c(2.5,5,7.5,10,12.5), color="black") + 
  scale_fill_manual(values=species_colors) + 
  theme_classic() + ylab("Number of stems") + xlab("Diameter at breast height (cm)") +
  guides(fill = guide_legend(title = "Ash species"))

# How many trees were found of various diameter classes, colored by hydroclass?
ggplot(data=small_and_big_trees) +
  geom_histogram(aes(x=diameter_at_137_cm_in_cm, fill=mstrlvl),
                 position="stack", breaks=c(2.5,5,7.5,10, 12.5), color="black") + 
  theme_classic() + xlab("Diameter at breast height (cm)") + ylab("Number of stems") +
  scale_fill_discrete(name = "Hydrological\nclass")

# How many black ash trees were found of various diameter classes, colored by
# canopy condition (1-5)?
black_ash_in_hydric <- filter(small_and_big_trees, ash_species_simple=="black", 
                              mstrlvl=="hydric", !is.na(canopy_condition_1_5)) # drop one observation without a canopy condition
ggplot(data=black_ash_in_hydric) +
  geom_histogram(aes(x=diameter_at_137_cm_in_cm, fill=canopy_condition_1_5),
                 position="stack", breaks=seq(2.5,12.5,2.5), color="black") + 
  theme_classic() + scale_fill_brewer() + xlab("Diameter at breast height (cm)") +
  ylab("Number of stems") + ylim(0,85)+
  #guides(fill ="none")+
  guides(fill = guide_legend(title = "Canopy condition \nrating \n(1=healthy, \n5=defoliated)"))+
  labs(title="Black ash in hydric forests") +
  theme(text = element_text(size = 15))

# How many green/pumpkin ash trees were found of various diameter classes, colored by
# canopy condition (1-5)?
green_pumpkin_ash_in_hydric <- filter(small_and_big_trees, 
                                      ash_species_simple=="green_white_or_pumpkin", 
                                      mstrlvl=="hydric")
ggplot(data=green_pumpkin_ash_in_hydric) +
  geom_histogram(aes(x=diameter_at_137_cm_in_cm, fill=canopy_condition_1_5),
                 position="stack", breaks=seq(2.5,12.5,2.5), color="black") + 
  theme_classic() + scale_fill_brewer() + xlab("Diameter at breast height (cm)") +
  ylab("Number of stems") + ylim(0,85)+
  guides(fill = guide_legend(title = "Canopy condition \nrating \n(1=healthy, \n5=defoliated)"))+
  labs(title="Green and pumpkin ash in hydric forests")+
  theme(text = element_text(size = 15))


# Make a scatter plot of the trees with diameter in the x-axis and 
# canopy condition in the y-axis.
ggplot(data=small_and_big_trees, aes(x=diameter_at_137_cm_in_cm, 
                                     y=as.integer(canopy_condition_1_5),
                                     color=ash_species_simple)) +
  scale_color_manual(values = species_colors)+
  geom_jitter(height=0.05, width=0, alpha=0.4) +
  theme_classic() + xlim(c(2.5, 12.5))+
  xlab("Diameter at breast height (cm)")+
  ylab("Canopy condition rating \n(1=healthy, 5=defoliated)") +
  labs(color="Ash species")
# Eventually, I want to build a generalized linear mixed effects model to
# explore the relationship (canopy condition) ~ DBH + (1|Park)

# Make a bar graph to show species of ash for small ash trees at plots. The 
# x-axis is Plot_ID and the y-axis is the stacked number of occurences of black ash
# and green/white/pumpkin ash. **Note: only hydric plots are shown.**
ggplot(data=(ash_by_plot %>% filter(mstrlvl=="hydric")))+
  geom_col(aes(x=Plot_ID, y=number_small_trees), fill="darkgreen") +
  geom_col(aes(x=Plot_ID, y=(number_small_trees_black + number_small_trees_unknown_species)), fill="black")+
  geom_col(aes(x=Plot_ID, y=number_small_trees_unknown_species), fill="lightblue") +      
  scale_x_discrete(guide = guide_axis(angle = 90)) + theme_classic() + 
  ylab("Number of small trees \n(2.5-10 cm DBH)") # Note: warning messages are 
# just because some plots have not been visited yet, so they do not have a 
# value for number of small trees

# Make a bar graph to show species of ash for small ash trees at transects.
ggplot(data=ash_by_transect)+
  geom_col(aes(x=Transect, y=total_number_small_trees_green_white_or_pumpkin + total_number_small_trees_black + total_number_small_trees_unknown_species), fill="darkgreen") +
  geom_col(aes(x=Transect, y=total_number_small_trees_black + total_number_small_trees_unknown_species), fill="black")+
  geom_col(aes(x=Transect, y=total_number_small_trees_unknown_species), fill="lightblue") +      
  scale_x_discrete(guide = guide_axis(angle = 90)) + theme_classic() + 
  ylab("Number of small ash trees \n(2.5-10 cm DBH)")

# Looking at all ash trees, organized by species, how many are 
# canopy condition = 1, 2, 3, 4, and 5?
ggplot(data=filter(small_and_big_trees, mstrlvl=="hydric")) +
  geom_bar(aes(x=ash_species_simple, fill=canopy_condition_1_5),color="black") + 
  scale_fill_brewer() + theme_bw() # This would seem to suggest that more black 
# ash are dying than green/white/pumpkin. But when you stratify by DBH, you can
# see that the green ash are mostly smaller than the black ash in hydric forests
# we surveyed

# Create a histogram of number of small trees to investigate whether the 
# distribution follows a Poisson distribution.
ggplot(data=ash_by_plot, aes(x=number_small_trees)) + geom_histogram() + 
  theme_classic()
# The Poisson distribution assumes that events (here the presence of a small
# tree) occur independently of the position of other small trees. That assumption
# is obviously false

