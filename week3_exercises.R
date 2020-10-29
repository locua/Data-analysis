library(car)
library(ggplot2)

dat <- read.csv("data/legacy2015.csv")

# 1. Exploratory plots
ggplot(dat, aes(x=age)) + geom_histogram(binwidth =1, col="black", fill="brown")
ggplot(dat, aes(x=intention)) + geom_histogram(binwidth =1, col="black", fill="brown")
ggplot(dat, aes(x=education)) + geom_histogram(binwidth =1, col="black", fill="brown")
ggplot(dat, aes(x=income)) + geom_histogram(binwidth =1, col="black", fill="brown")
ggplot(dat, aes(x=donation)) + geom_histogram(binwidth =1, col="black", fill="brown")
ggplot(dat, aes(x=belief)) + geom_histogram(binwidth =1, col="black", fill="brown")

library(GGally)
ggpairs(dat[,c("legacy", "belief", "intention", "donation")], upper=list(continuous= wrap(ggally_cor, size=5, family="sans")))

# 2. Predicting intention from legacy
#summary(lm(intention~1, data=dat))
summary(lm(intention~legacy, data=dat))
# Reject the null hypothesis p < 0.000

# 3. Predict intention from belief
#anova(lm(intention~1, data=dat), lm(intention~belief, data=dat))
summary(lm(intention~belief, data=dat))
# Reject the null hypothesis H0: B1=0 p < 0.000
# Belief has a larger F stat and SSR so is a better predictor of intention than legacy

# 4. 
summary(lm(donation~legacy+belief+intention, data=dat))
#Anova(lm(donation~legacy+belief+intention, data=dat), type=3)
