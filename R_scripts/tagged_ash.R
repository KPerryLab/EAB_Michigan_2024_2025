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

# Because I think the diameter was measured in millimeters, I need to divide
# the 2011 number by 10 to get cm:
tagged_ash$diameter_at_breast_height_2011_in_cm <- tagged_ash$diameter_at_breast_height_2011 / 10

# Now graph the relationship between DBH in 2011 and DBH in 2024, 13 years later:
ggplot(data=tagged_ash, mapping=aes(x=diameter_at_breast_height_2011_in_cm,
                                    y=diameter_at_137_cm_in_cm)) +
  geom_point()+
  xlab("Ash sapling diameter in 2011 (cm)")+
  ylab("Ash sapling diameter in 2024 (cm)")+
  scale_x_continuous(breaks=seq(0,11, by=1), limits=c(0,5))+
  
  scale_y_continuous(breaks=seq(0,11, by=1), limits=c(0,10))+
  
  coord_fixed()+
  theme_classic()

tagged_ash$change_in_diameter <- tagged_ash$diameter_at_137_cm_in_cm - tagged_ash$diameter_at_breast_height_2011_in_cm

# Now make a graph of the 




