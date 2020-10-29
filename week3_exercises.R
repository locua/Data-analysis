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


ggplot(dat, aes(x=intention, y=age)) + geom_point()
ggplot(dat, aes(x=education, y=age)) + geom_point()
ggplot(dat, aes(x=income, y=donation)) + geom_point()
ggplot(dat, aes(x=education, y=donation)) + geom_point()
ggplot(dat, aes(x=education, y=legacy)) + geom_point()
ggplot(dat, aes(x=education, y=intention)) + geom_point()
ggplot(dat, aes(x=intention, y=belief)) + geom_point()
ggplot(dat, aes(x=intention, y=legacy)) + geom_point()

# 2. Predicting intention from legacy
summary(lm(intention~1, data=dat))

anova(lm(intention~1, data=dat), lm(intention~legacy, data=dat))
# Reject the null hypothesis p < 0.000

# 3. Predict intention from belief
anova(lm(intention~1, data=dat), lm(intention~belief, data=dat))
# Reject the null hypothesis H0: B1=0 p < 0.000
# Belief has a larger F stat and SSR so is a better predictor of intention than legacy

# 4. 
summary(lm(donation~legacy+belief+intention, data=dat))
#Anova(lm(donation~legacy+belief+intention, data=dat), type=3)
