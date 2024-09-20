# Aaron Tayal
# 9/16/2024
# Practicing with the code from Bates et al. 2015
# Bates, Douglas, Martin Mächler, Ben Bolker, and Steve Walker. “Fitting Linear Mixed-Effects Models Using Lme4.” Journal of Statistical Software 67, no. 1 (2015). https://doi.org/10.18637/jss.v067.i01.

library(lme4)
library(lattice)

dat <- sleepstudy
coef(lm(Reaction ~ Days, data=dat))[1]
xyplot(Reaction ~ Days | Subject, sleepstudy, type = c("g","p","r"),
       index = function(x,y) coef(lm(y ~ x))[2], # I think this orders the graphs based on 
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)", aspect = "xy")

fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
summary(fm1)
