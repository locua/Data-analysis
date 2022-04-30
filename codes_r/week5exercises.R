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
summary(lm(everest_feet~effecta, data=anchoring))
# Using contrasts
dat$anchor <- as.factor(dat$anchor)
contrasts(dat$anchor)  
contrasts(dat$anchor)  <- c(.5,-.5)
summary(lm(everest_feet~anchor, data=dat))

# 4.
contrasts(dat$anchor)  <- c(1,-1)
summary(lm(everest_feet~anchor, data=dat))

# 5.
plot_raincloud(subset(anchoring, anchor == 'low' & referrer %in% c("tilburg","wisc","laurier")), everest_feet, groups=referrer)

# 6.
dat1<-subset(anchoring, anchor == 'low' & referrer %in% c("tilburg","wisc","laurier"))
codes <- cbind(c(-1/3,-1/3,2/3), c(-1/2,1/2,0))
colnames(codes) <- c(" us-v-inter", " inter-only")
dat1$referrer <- as.factor(dat1$referrer)
contrasts(dat1$referrer) <- codes
summary(lm(everest_feet ~ referrer, data=dat1))

# 7. Overall omnibus test for referrer
car::Anova(lm(everest_feet~referrer, data=dat1), type=3)

# 8.
contrasts(dat1$referrer) <- contr.sum(3)
summary(lm(everest_feet ~ referrer, data=dat1))
car::Anova(lm(everest_feet~referrer, data=dat1), type=3)

# 9.
dat2<-subset(anchoring, anchor == 'low' & referrer %in% c("tilburg","wisc","laurier", "wpi"))
plot_raincloud(dat2, everest_feet, groups=referrer)

dat2$referrer<-as.factor(dat2$referrer)
