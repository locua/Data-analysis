data("anchoring")
dat<-anchoring
library(sdamr)

#1.
# Inspect everest feet data by anchor group
plot_raincloud(anchoring, everest_feet, groups=anchor)

# 2.
# Create dummy variable
anchoring$dummy <- ifelse(anchoring$anchor == "low", 0, 1)
# Estimate regression model predicting everest_feet from dummy
summary(lm(everest_feet~dummy, data=anchoring))
# We have a significant result for the intercept and the dummy predictor
# and therefore evidence that there is a difference between the mean
# of the low and high anchored predictions of everest in feet.

# 3.
anchoring$anchor <- as.factor(anchoring$anchor)
# anchor high = 0.5
# anchor low = -0.5
#anchoring$effecta <- ifelse(anchoring$anchor=="high",0.5,-0.5)
#summary(lm(everest_feet~effecta, data=anchoring))
# Using contrasts
dat$anchor <- as.factor(dat$anchor)
contrasts(dat$anchor)  
contrasts(dat$anchor)  <- c(.5,-.5)
summary(lm(everest_feet~anchor, data=dat))

# 4.













  