# Aaron Tayal
# 8/26/2024
# The purpose is to report what the stem 
# density of ash seedlings, saplings, small trees, and trees is

library(ggplot2)
library(dplyr)

# Plot centers ---------------------------------------------------------------

plot_centers <- read.csv("Raw_data/EAB_Michigan_2024_plot_centers.csv")

# There are 111 plots (37 transects) that I plan to visit (and 2 extra rows due 
# to confusion about the true center for plot 75):
nrow(plot_centers)

# There are 98 plots that I have collected seedling data (not counting 75_north):
sum(plot_centers$seedlings_done_y_n == "y")

# Similarly, there are 98 plots that I have collected sapling data (not counting 
# 75_north):
sum(plot_centers$saplings_done_y_n == "y")

# There are 97 plots that I have collected tree data for (not counting 75_north).
# At plot 91 at Hudson Mills, we ran out of time to collect tree data due to a 
# thunderstorm
sum(plot_centers$trees_done_y_n == "y")

# There are 2 plots where we found and measured the center tree but did not 
# collect any seedling, sapling, or tree data due to high waters and a time 
# crunch. These were plots 23 and 66, both at Island Lake.
#View(plot_centers[plot_centers$seedlings_done_y_n == "n",])

# This leaves 11 plots that we have neither found the center or recorded data
# for. Four at Hudson Mills, six at Indian Springs, and one at Island Lake. 
# Note: we technically found plot 61 at Island Lake, but I forgot to write
# the center tree DBH, and thus I'm considering it completely not-started.
#View(plot_centers[plot_centers$seedlings_done_y_n == "",])

# Summary: 97 done + 3 partially done + 11 not-started = 111 plots

# Hydroclass -----------------------------------------------------------------

hydro <- read.csv("Raw_data/MI-plot-hydroclasses.csv")
# The column "mstrlvl" is either xeric, mesic, or hydric. All plots in one
# transect (3 plots) will have the same assignment of mstrlvl. I will also 
# call this the "hydroclass". The column "plotmstr" has a value from 1 to 5 
# (1 being driest and 5 being the most flooded). The "plotmstr" value can
# vary within a transect. I think it is based on the results of some kind of 
# soil moisture test.

# Note: "MI-plot-hydroclasses.csv" has hydroclass data for 129 plots, including
# some that I do not plan to visit because they are designated "non-ash" plots
# or they are at Brighton.

# Seedlings -------------------------------------------------------------------

seedlings <- read.csv("Raw_data/EAB_Michigan_2024_seedlings.csv")

# At one plot (75 at Indian Springs), we found two center trees marked 75.
# The true, original plot 75 is the one I want to use. I called this one 
# "75_south" in my datasheet this year. So I need to remove "75_north" rows and
# rename "75_south" rows as simply "75".
seedlings <- seedlings[seedlings$center_tree_number != "75_north",]
seedlings$center_tree_number[seedlings$center_tree_number == "75_south"] <- "75"

seedlings$center_tree_number <- as.integer(seedlings$center_tree_number)

# To combine the data on hydroclasses with the seedling data, I will
# use inner_join, which keeps only the rows which have center_tree_number
# matching between the two datasets:
seedlings2 <- inner_join(seedlings, hydro, by="center_tree_number")
# I notice that the number of rows of seedlings and seedlings2 are the same,
# indicating that the dataframe hydro contained all the center tree numbers
# that I have
nrow(seedlings) == nrow(seedlings2)

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
# get the density. The density will be in units of stems per meter^2
seedlings2$density_short <-
  (seedlings2$number_short / seedlings2$area_of_microplot_m_squared)

seedlings2$density_tall <-
  (seedlings2$number_tall / seedlings2$area_of_microplot_m_squared)

seedlings2$density_seedlings <- 
  (seedlings2$number_seedlings / seedlings2$area_of_microplot_m_squared)

# For a common sense check, I'll plot the density of short seedlings against
# the density of tall seedlings.  I predict (prior to seeing it) that they 
# will be correlated.
plot(seedlings2$density_short,
     seedlings2$density_tall)

# I will plot histograms for short and tall seedlings. I want to know what is 
# the qualitative shape of the distribution:
hist(seedlings2$density_short, breaks=100)
hist(seedlings2$density_tall, breaks=100)

# Note: these density variables are pseudo-continuous, but really they are
# discrete, because the counts are discrete, and so there are only a limited
# number of values that the density can take.

# I want to make a violin plot that shows the distribution of seedling densities
# for each of the three hydroclasses (xeric, mesic, and hydric)
ggplot(data=seedlings2, aes(x=factor(mstrlvl), 
                            y=density_seedlings)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of ash seedlings " ~ (stems/m^2)))

# I want to take an average of the 4 microplots in order to report data at the 
# plot level. 

# IMPORTANT NOTE: sometimes, we forgot to write the percent cover
# for 1 out of 4 of the microplots. I have instructed the code to strip NA values
# before taking the mean.  I'm assuming it's okay because we still have three other
# values, but I need to check on this.

seedlings_by_plot <- seedlings2 %>% group_by(center_tree_number) %>% 
  summarize(mean_percent_cover = mean(percent_cover_0_0.5_1_3.5_8_15.5_25.5_etc, 
                                 na.rm=TRUE),
            mean_density_short = mean(density_short),
            mean_density_tall = mean(density_tall),
            mean_density_seedlings = mean(density_seedlings),
            mstrlvl = first(mstrlvl))

# Make a graph that shows the seedling densities by hydroclass, at the plot level
ggplot(data=seedlings_by_plot, aes(x=factor(mstrlvl), y=mean_density_seedlings)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of ash seedlings " ~ (stems/m^2))) +
  scale_y_continuous(breaks=seq(0,9,2)) +
  theme_bw()

# Make a similar graph for short seedlings (under 25 cm):
ggplot(data=seedlings_by_plot, aes(x=factor(mstrlvl), y=mean_density_short)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of short ash seedlings <25 cm  " ~ (stems/m^2))) +
  scale_y_continuous(breaks=seq(0,9,2)) +
  ylim(-0.15, 6) +
  theme_bw()

# Same for tall seedlings:
ggplot(data=seedlings_by_plot, aes(x=factor(mstrlvl), y=mean_density_tall)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of tall ash seedlings 25-137 cm  " ~ (stems/m^2))) +
  ylim(-0.15, 6) +
  theme_bw()

# And for percent cover:
ggplot(data=seedlings_by_plot, aes(x=factor(mstrlvl), y=mean_percent_cover)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab("Percent cover of ash seedlings (%)") +
  theme_bw()

# Saplings --------------------------------------------------------------------

saplings <- read.csv("Raw_data/EAB_Michigan_2024_saplings.csv")
# Remove 75_north and rename 75_south to 75 (see seedlings for details)
saplings <- saplings[saplings$center_tree_number != "75_north",]
saplings$center_tree_number[saplings$center_tree_number == "75_south"] <- "75"

saplings$center_tree_number <- as.integer(saplings$center_tree_number)

# Trying to add in the hydroclass to this dataframe:
saplings2 <- inner_join(saplings, hydro, by="center_tree_number")
nrow(saplings) == nrow(saplings2)

# Saplings were counted in each quadrant, so I want to sum up the quadrants
# to get a total number of saplings by plot
saplings_by_plot <- saplings2 %>% group_by(center_tree_number) %>% 
  summarize(number_saplings = sum(number_of_stems),
            mstrlvl = first(mstrlvl))

# To find the density of saplings, I need to divide the number of saplings 
# found in the subplot (8 meters in radius) by the area of that subplot. 
# The area of the subplot is 201.062 m^2. The unit will be stems per m^2.
saplings_by_plot$density_saplings <- saplings_by_plot$number_saplings / 201.062

# I would like to also change the units to stems per hectare:
saplings_by_plot$density_saplings_stems_per_ha <- 
  saplings_by_plot$density_saplings * 10000

# Now, make a violin plot that shows the number of saplings found at each plot, 
# as a function of hydroclass
ggplot(data=saplings_by_plot, aes(x=factor(mstrlvl), 
                                  y=density_saplings)) + 
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of ash saplings " ~ (stems/m^2))) +
  theme_bw()

# Small trees and trees -------------------------------------------------------

trees <- read.csv("Raw_data/EAB_Michigan_2024_trees.csv")

# Remove 75_north and rename 75_south to 75 (see seedlings for details)
trees <- trees[trees$center_tree_number != "75_north",]
trees$center_tree_number[trees$center_tree_number == "75_south"] <- "75"

trees$center_tree_number <- as.integer(trees$center_tree_number)
trees$ash_species <- as.factor(trees$ash_species)

# Remove any rows where the species of tree is not ash (keep rows where ash_species is NA)
# Note: the hickory and Zanthoxylum rows were tagged trees
levels(trees$ash_species)
trees <- trees[( trees$ash_species != "hickory" | is.na(trees$ash_species) ), ]
trees <- trees[( trees$ash_species != "Zanthoxylum (prickly ash)" | is.na(trees$ash_species) ), ]
trees$ash_species <- droplevels(trees$ash_species)
levels(trees$ash_species)

# I want to make a column that simplifies ash_species so that there are only 
# three categories: black, green/white/pumpkin, and unknown
trees$ash_species_simple <- as.character(trees$ash_species)
trees$ash_species_simple[trees$ash_species_simple == "green"] <- "green_white_or_pumpkin"
trees$ash_species_simple[trees$ash_species_simple == "green or pumpkin"] <- "green_white_or_pumpkin"
trees$ash_species_simple[trees$ash_species_simple == "green or white"] <- "green_white_or_pumpkin"
trees$ash_species_simple[trees$ash_species_simple == "green white or pumpkin"] <- "green_white_or_pumpkin"
trees$ash_species_simple[trees$ash_species_simple == "black?"] <- "unknown"
trees$ash_species_simple[trees$ash_species_simple == "?"] <- "unknown"
trees$ash_species_simple[trees$ash_species_simple == "green?"] <- "unknown"
trees$ash_species_simple[trees$ash_species_simple == "white?"] <- "unknown"
trees$ash_species_simple <- as.factor(trees$ash_species_simple)
levels(trees$ash_species_simple)

# Which center_tree_number values are found in seedlings but not found in 
# trees?
a <- seedlings_by_plot$center_tree_number
b <- unique(trees$center_tree_number)
b
setdiff(a, b)
# Plot 91 at Hudson Mills is correct to be missing from trees

# Make sure the variable that records which quadrant the tree was in is accurate:
trees$quadrant_NE_SE_SW_NW <- as.factor(trees$quadrant_NE_SE_SW_NW)
summary(trees$quadrant_NE_SE_SW_NW)
trees[trees$quadrant_NE_SE_SW_NW == "?",]
# Which rows are simply recording the absence of any trees?
trees$center_tree_number[trees$quadrant_NE_SE_SW_NW == "none"]
unique(trees$center_tree_number[trees$quadrant_NE_SE_SW_NW == "none"])


# I want to create a new dataframe of trees so that small trees (2.5-10 cm DBH)
# are counted only if they occur within the 8 meter radius subplot. Furthermore,
# trees (10 cm DBH and up) are counted only if they occur within the 18 meter
# radius main plot

# First I need to change the variable distance_to_center_meters into a 
# numeric variable.  For many of the trees inside the 8 meter subplot, we
# simply wrote "less than 8" to indicate it was in the subplot. So now I 
# will name these as 4 (4 meters) even though they ranged from 0 m to 8 m
trees$distance_to_center_meters_simple <- trees$distance_to_center_meters
trees$distance_to_center_meters_simple[trees$distance_to_center_meters_simple == "less than 8"] <- "4"
trees$distance_to_center_meters_simple[trees$distance_to_center_meters_simple == "between 8 and 18"] <- "13"
trees$distance_to_center_meters_simple[trees$distance_to_center_meters_simple == "greater than 18"] <- "25"
trees$distance_to_center_meters_simple[trees$distance_to_center_meters_simple == "approx 100 m"] <- "100"
trees$distance_to_center_meters_simple <- as.numeric(trees$distance_to_center_meters_simple)
hist(trees$distance_to_center_meters_simple, breaks=100)

# Small trees must be >= 2.5 cm DBH AND < 12.5 cm DBH AND distance to the center
# must be <= 8 meters
small_trees_filtered <- trees %>% dplyr::filter(quadrant_NE_SE_SW_NW != "none") %>%
  dplyr::filter(diameter_at_137_cm_in_cm >= 2.5) %>%
  dplyr::filter(diameter_at_137_cm_in_cm < 10) %>%
  dplyr::filter(distance_to_center_meters_simple <= 8)

# Big trees must be >= 10 cm DBH AND distance to the center must be <= 18 meters
big_trees_filtered <- trees %>% dplyr::filter(quadrant_NE_SE_SW_NW != "none") %>%
  dplyr::filter(diameter_at_137_cm_in_cm >= 10) %>%
  dplyr::filter(distance_to_center_meters_simple <= 18)




