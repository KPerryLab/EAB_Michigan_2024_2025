# Aaron Tayal
# 7/8/2024
# The purpose is to report (with the data we have so far) what the stem 
# density of ash seedlings, saplings, small trees, and trees is

library(ggplot2)
library(dplyr)

# Seedlings -------------------------------------------------------------------

seedlings <- read.csv("Raw_data/07_08_2024_ash_seedlings.csv")
seedlings$center_tree_number <- as.integer(seedlings$center_tree_number)
hydro <- read.csv("Raw_data/MI-plot-hydroclasses.csv")

# To combine the data on hydroclasses with the seedling data, I will
# use inner_join, which keeps only the rows which have center_tree_number
# matching between the two datasets:
seedlings2 <- inner_join(seedlings, hydro, by="center_tree_number")

# We recorded data for each quadrant of each microplot, so I need to add up the
# four quadrants for short seedlings (those that are shorter than 25 cm):
seedlings2$number_established_short <- 
  seedlings2$northeast_number_established_short +
  seedlings2$southeast_number_established_short +
  seedlings2$southwest_number_established_short +
  seedlings2$northwest_number_established_short

# Same for tall seedlings:
seedlings2$number_established_tall <- 
  seedlings2$northeast_number_established_tall +
  seedlings2$southeast_number_established_tall +
  seedlings2$southwest_number_established_tall +
  seedlings2$northwest_number_established_tall

# Now I need to divide the raw counts by the area of the circular microplot to
# get the density. 
seedlings2$density_established_short_stems_per_m_squared <-
  (seedlings2$number_established_short / 
     seedlings2$area_of_microplot_m_squared)

seedlings2$density_established_tall_stems_per_m_squared <-
  (seedlings2$number_established_tall / 
     seedlings2$area_of_microplot_m_squared)

# I will make a column for the combined density of both short and tall seedlings:
seedlings2$density_established_seedlings_stems_per_m_squared <- 
  seedlings2$density_established_short_stems_per_m_squared +
  seedlings2$density_established_tall_stems_per_m_squared

# For a common sense check, I'll plot the density of short seedlings against
# the density of tall seedlings.  I predict (prior to seeing it) that they 
# will be correlated.
plot(seedlings2$density_established_short_stems_per_m_squared,
     seedlings2$density_established_tall_stems_per_m_squared)

# I will plot histograms for short and tall seedlings. I want to know what is 
# the qualitative shape of the distribution:
hist(seedlings2$density_established_short_stems_per_m_squared, breaks=100)
hist(seedlings2$density_established_tall_stems_per_m_squared, breaks=100)

# Note: these density variables are pseudo-continuous, but really they are
# discrete, because the counts are discrete, and so there are only a limited
# number of values that the density can take.

# I want to make a violin plot that shows the distribution of seedling densities
# for each of the three hydroclasses (xeric, mesic, and hydric)
ggplot(data=seedlings2, aes(x=factor(mstrlvl), 
                            y=density_established_seedlings_stems_per_m_squared)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of ash seedlings " ~ (stems/m^2)))

# I want to take an average of the 4 microplots in order to report data at the 
# plot level. IMPORTANT NOTE: sometimes, we forgot to write the percent cover
# for 1 out of 4 of the microplots. I have instructed the code to strip NA values
# before taking the mean.  I'm assuming it's okay because we still have three other
# values, but I need to check on this.
seedlings_by_plot <- seedlings2 %>% group_by(center_tree_number) %>% 
  summarize(mean_perc_cov = mean(percent_cover_0_0.5_1_3.5_8_15.5_25.5_etc, 
                                 na.rm=TRUE),
            mean_density_est_seedl = mean(density_established_seedlings_stems_per_m_squared),
            mstrlvl = first(mstrlvl))

# Make a graph that shows the seedling densities by hydroclass, at the plot level
ggplot(data=seedlings_by_plot, aes(x=factor(mstrlvl), 
                            y=mean_density_est_seedl)) +
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of ash seedlings " ~ (stems/m^2)))

# -----------------------------------------------------------------------------
# Saplings --------------------------------------------------------------------

saplings <- read.csv("Raw_data/07_09_2024_ash_saplings.csv")
saplings$center_tree_number <- as.integer(saplings$center_tree_number)

# Trying to add in the hydroclass to this dataframe:
saplings2 <- inner_join(saplings, hydro, by="center_tree_number")

# Saplings were counted in each quadrant, so I want to sum up the quadrants
# to get a total number of saplings by plot
saplings_by_plot <- saplings2 %>% group_by(center_tree_number) %>% 
  summarize(number_saplings = sum(number_of_stems),
            mstrlvl = first(mstrlvl))

# To find the density of saplings, I need to divide the number of saplings 
# found in the subplot (8 meters in radius) by the area of that subplot. 
# The area of the subplot is 201.062 m^2
saplings_by_plot$density_saplings_stems_per_m_squared <- 
  saplings_by_plot$number_saplings / 201.062

# Now, make a violin plot that shows the number of saplings found at each plot, 
# as a function of hydroclass
ggplot(data=saplings_by_plot, aes(x=factor(mstrlvl), 
                                  y=density_saplings_stems_per_m_squared)) + 
  geom_violin() +
  geom_jitter(height=0, width=0.1, alpha=0.5) +
  xlab("Hydroclass") +
  ylab(bquote("Density of ash saplings " ~ (stems/m^2)))









  

