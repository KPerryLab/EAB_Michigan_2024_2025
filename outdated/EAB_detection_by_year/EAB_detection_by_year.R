
dat <- read.csv("EAB detection EABinfo 2_8_2024.csv")
dat$State <- as.factor(dat$State)
dat$County <- as.factor(dat$County)

hist(dat$Year_detected, breaks=2000:2023)
dat$State
unique(dat$State)
levels(dat$State)
