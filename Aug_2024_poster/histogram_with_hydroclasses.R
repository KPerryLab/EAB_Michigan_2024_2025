# Aaron Tayal
# 7/8/2024
# The purpose is to create a histogram to understand whether the hydroclass
# (xeric, mesic, or hydric) is a good representation of the hydrology of 
# the plots

library(ggplot2)

dat <- read.csv("Raw_data/MI-plot-hydroclasses.csv")
dat$mstrlvl <- as.factor(dat$mstrlvl)

ggplot(data=dat) + 
  geom_histogram(data=dat, aes(x=plotmstr, fill=mstrlvl), position="dodge") +
  theme_classic()
