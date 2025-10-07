# 10/1/2024
# Aaron Tayal
# I'm trying to make a graph of the growth rates of ash saplings

library(ggplot2)
library(dplyr)

tagged_ash <- read.csv("Raw_data/Matching_tagged_ash_to_2011.csv")
which(tagged_ash$diameter_at_breast_height_2011 == ".") # It seems this sapling
# was only barely taller than breast height, and so diameter at breast height
# was not measured. I'll replace it with a NA:
tagged_ash[36, "diameter_at_breast_height_2011"] <- NA
tagged_ash$diameter_at_breast_height_2011 <- as.numeric(tagged_ash$diameter_at_breast_height_2011)

tagged_ash$canopy_condition_1_5 <- as.integer(tagged_ash$canopy_condition_1_5)

# Because I think the diameter was measured in millimeters, I need to divide
# the 2011 number by 10 to get cm:
tagged_ash$diameter_at_breast_height_2011_in_cm <- tagged_ash$diameter_at_breast_height_2011 / 10

# Merge data on hydroclass:
tagged_ash$center_tree_number <- as.integer(tagged_ash$center_tree_number)
hydro <- read.csv("Raw_data/MI-plot-hydroclasses.csv")
tagged_ash_1 <- right_join(hydro, tagged_ash, by="center_tree_number")

# Now graph the relationship between DBH in 2011 and DBH in 2024, 13 years later:

# First create a trendline:
growth_model <- lm(tagged_ash$diameter_at_137_cm_in_cm ~ 
                     tagged_ash$diameter_at_breast_height_2011_in_cm)
summary(growth_model)

ggplot(data=tagged_ash, mapping=aes(x=diameter_at_breast_height_2011_in_cm,
                                    y=diameter_at_137_cm_in_cm,
                                    color=canopy_condition_1_5)) +
  geom_point()+
  xlab("Ash sapling diameter in 2011 (cm)")+
  ylab("Ash sapling diameter in 2024 (cm)")+
  scale_x_continuous(breaks=seq(0,11, by=1), limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,11, by=1), limits=c(0,10))+
  scale_fill_brewer()+ labs(color = "Canopy \ncondition") +
  coord_fixed()+
  theme_classic()

ggplot(data=tagged_ash_1, mapping=aes(x=diameter_at_breast_height_2011_in_cm,
                                    y=diameter_at_137_cm_in_cm,
                                    color=plotmstr, label=canopy_condition_1_5)) +
  geom_point() + geom_text(vjust=1) +
  xlab("Ash sapling diameter in 2011 (cm)")+
  ylab("Ash sapling diameter in 2024 (cm)")+
  scale_x_continuous(breaks=seq(0,11, by=1), limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,11, by=1), limits=c(0,10))+
  coord_fixed()+
  theme_classic()

tagged_ash$change_in_diameter <- tagged_ash$diameter_at_137_cm_in_cm - tagged_ash$diameter_at_breast_height_2011_in_cm
tagged_ash$diameter_change_per_year_mm <- tagged_ash$change_in_diameter * 10 / 13

# Now make a graph of the change in diameter per year:
ggplot(data=tagged_ash, aes(x=0, y=diameter_change_per_year_mm, color=canopy_condition_1_5)) +
  geom_violin() +
  geom_jitter(height=0, width=0.03, alpha=1)+
  theme_classic() +
  ylab("Change in diameter per year (mm)") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  xlab("")
  




