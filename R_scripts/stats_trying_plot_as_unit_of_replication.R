# 10/30/2024
# Aaron Tayal
# Emerald ash borer Michigan 2024: analysis with plot as unit of replication

library(ggplot2)
library(dplyr)
library(lme4)

library(car)
library(emmeans)

# Plot is unit of replication - seedlings #####################################
# I'll run an analysis where my unit of replication is the plot. Instead 
# of using hydroclass (aka. mstrlvl) (hydric, mesic, or xeric), I'll use the 
# plot-level moisture rating plotmstr (on a sclae from 1 to 5, 5 being wettest) 
# as my predictor variable. 
# Note that Dr. Klooster and others used plotmstr levels to decide hydroclass 
# (mstrlvl) classifications.

seedlings_by_plot <- read.csv("Cleaned_data/seedlings_by_plot.csv")
# If I treat plotmstr as an integer, I think the model will be estimating an
# intercept and slope. If I instead treat plotmstr as a factor, then a coefficient
# will be estimated for each level (1, 2, 3, 4, and 5). I think I want to treat
# plotmstr as an integer because that will allow me to easily interpret the results.
seedlings_by_plot$plotmstr <- as.integer(seedlings_by_plot$plotmstr)


# Graph the data:
ggplot(data=seedlings_by_plot) +
  geom_point(aes(x=plotmstr, y=total_number_seedlings, color=Park), 
             alpha=0.5) + theme_classic()
hist(seedlings_by_plot$total_number_seedlings, breaks=20)

# Grouping variables to account for spatial structure: Park and transect.
# Transect is nested within Park. The transects are all uniquely named:
table(seedlings_by_plot$Transect) # Note that not all 111 plots have been
# visited in 2024, thus transects AA, DD, P, Q, and Z have some plots missing
# and a few transects were not visited yet at all.

# Fit the model:
seedling_mod_by_plot <- glmer(total_number_seedlings ~ plotmstr + 
                                (1|Park) + (1|Transect),
                              data=seedlings_by_plot, family="poisson")
summary(seedling_mod_by_plot)

# Check model assumptions:
qqnorm(resid(seedling_mod_by_plot))
qqline(resid(seedling_mod_by_plot))
plot(seedling_mod_by_plot)

# Make a graph that includes the trend line:
new_data_pois_me <- data.frame(plotmstr = seq(1, 5, 1))
new_data_pois_me$Predicted_seedlings_poisson <- predict(seedling_mod_by_plot,
                                                    newdata=new_data_pois_me, 
                                                    type="response",
                                                    re.form=NA)

ggplot(data=seedlings_by_plot, mapping=aes(x=plotmstr, y=total_number_seedlings))+
  geom_point(alpha=0.5)+theme_classic()+
  geom_line(data=new_data_pois_me,
            aes(x=plotmstr, y=Predicted_seedlings_poisson), linewidth=0.5) +
  xlab("Plot moisture level (1=dry, 5=flooded)") +
  ylab("Number of seedlings in plot")
# It appears that the trend line does not match the data very well, which tells
# me that the model is not explaining the data very well.

