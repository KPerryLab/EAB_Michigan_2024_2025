# 8/21/2024
# Aaron Tayal
# I want to make a bar graph showing the trap catches of emerald ash borer

library(ggplot2)
library(dplyr)
library(tidyr)

dat <- read.csv("Raw_data/EAB_Michigan_2024_prism_trap_collections.csv")
dat$collection_interval <- as.factor(dat$collection_interval)
dat$park <- as.factor(dat$park)

# Make a new variable for the combined count of EAB males and females
dat$number_EABs <- dat$number_EAB_females_lab_count + 
  dat$number_EAB_males_lab_count

ggplot(data=dat, aes(fill=collection_interval, x=park, y=number_EABs)) +
  geom_bar(position="dodge", stat="identity", width=0.5) +
  theme_bw() +
  scale_y_continuous(breaks=seq(0,13,2)) +
  xlab("Park") +
  ylab("Number of EAB on prism trap") +
  labs(fill="Collection \nInterval") +
  scale_fill_manual(values = c("light blue", "orange"))

# I'm wondering if the numbers of males and females captured varied based on 
# sampling interval. Peak emergence of males is before females (Tobin et al. 2021)

# The function group_by groups all the rows from each sampling interval together
# and creates a "grouped tibble" so that the function summarize can count up all
# the males and females in each sampling interval

# The function pivot_longer increases the  number of rows. Instead of having 
# separate columns for male EABs and female EABs, we simply have a single column 
# for number of EABs, and then a different column which indicates whether each 
# row is male or female
dat_summary <- dat %>% group_by(collection_interval) %>% 
  summarize(number_EAB_females_lab_count = sum(number_EAB_females_lab_count),
            number_EAB_males_lab_count = sum(number_EAB_males_lab_count)) %>%
  pivot_longer(cols=c("number_EAB_females_lab_count", "number_EAB_males_lab_count"),
               names_to="Sex",
               values_to="number_EAB_lab_count")
dat_summary$Sex[dat_summary$Sex=="number_EAB_females_lab_count"] <- "female"
dat_summary$Sex[dat_summary$Sex=="number_EAB_males_lab_count"] <- "male"

ggplot(data=dat_summary, aes(x=collection_interval, fill=Sex, y=number_EAB_lab_count)) +
  geom_bar(position="dodge", stat="identity", width=0.5) +
  theme_bw() +
  scale_y_continuous(breaks=seq(0,14,2)) +
  xlab("Collection interval") +
  ylab("Total number of EABs captured") +
  labs(fill="Sex")+
  scale_fill_manual(values = c("dark green", "dark red"))
