# Aaron Tayal
# July 7, 2025
# EAB Michigan plots: percent cover of the entire plot

# Goal is to create a graph of the recorded data.

library(dplyr)
library(ggplot2)
library(tidyr)

dat <- read.csv("Raw_data/EAB_Michigan_2025_plot_percent_cover.csv")

# How many missing values are there?
sum(is.na(dat)) # one missing value

vars <- c("water", "graminoids", "skunk_cabbage", "ferns", "spicebush", "ilex",
          "glossy_buckthorn", "poison_sumac", "other_woody_shrub")

dat_by_plot <- dat %>% group_by(center_tree_number) %>% 
  summarize(across(all_of(vars), ~ mean(., na.rm=T)))

#write.csv(dat_by_plot, "Cleaned_data/percent_cover_by_plot.csv", row.names=F)

dat_by_plot_longer <- pivot_longer(dat_by_plot, vars)
dat_by_plot_longer$name <- factor(dat_by_plot_longer$name,
                                     levels=vars)

ggplot(data = dat_by_plot_longer, aes(x=name, y=value)) +
  geom_jitter(height=0, width=0.05, alpha=0.5) + theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ylab("Percent cover (%)") + xlab("")

dat_by_var <- dat_by_plot_longer %>% group_by(name) %>% summarize(
  mean = mean(value),
  stdev = sd(value),
  stder = sd(value) / sqrt(n())
)

#write.csv(dat_by_var, file = "Cleaned_data/percent_cover_by_type_table.csv", row.names = F)

