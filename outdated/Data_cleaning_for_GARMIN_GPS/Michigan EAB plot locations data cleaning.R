library(dplyr)

setwd("C:/Users/Aaron/Desktop")
unordered <- read.csv("unordered.csv")

# Remove the breaking-space (it looks like a space character, but it is 
# really a strange character used in HTML that appears as a space)
unordered$Park <- gsub("\u00A0", "", unordered$Park, fixed = TRUE)
unordered$Plot_number <- gsub("\u00A0", "", unordered$Plot_number, fixed = TRUE)
unordered$Lat <- gsub("\u00A0", "", unordered$Lat, fixed = TRUE)
unordered$Lon <- gsub("\u00A0", "", unordered$Lon, fixed = TRUE)

# Change the column types to reflect the type of data
unordered$Plot_number <- as.integer(unordered$Plot_number)
unordered$Lat <- as.numeric(unordered$Lat)
unordered$Lon <- as.numeric(unordered$Lon)

containing_directions <- read.csv("containing_directions.csv")
containing_directions$Plot_number <- as.integer(containing_directions$Plot_number)
containing_directions <- containing_directions %>% select(Plot_number, Transect,
                                                          Plot_ID, Directions,
                                                          Center_tree)

#Join together the two datasets
dat <- dplyr::full_join(unordered, containing_directions, by="Plot_number")

#Order the rows by plot number
dat <- dplyr::arrange(dat, Plot_number)

write.csv(dat, file="Michigan_plots.csv")
