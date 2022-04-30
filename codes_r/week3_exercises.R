library(car)
library(ggplot2)

dat <- read.csv("data/legacy2015.csv")

# 1. Exploratory plots
ggplot(dat, aes(x=age))       + geom_histogram(binwidth =1, col="black", fill="brown")
ggplot(dat, aes(x=intention)) + geom_histogram(binwidth =1, col="black", fill="brown")
ggplot(dat, aes(x=education)) + geom_histogram(binwidth =1, col="black", fill="brown")
ggplot(dat, aes(x=income))    + geom_histogram(binwidth =1, col="black", fill="brown")
ggplot(dat, aes(x=donation))  + geom_histogram(binwidth =1, col="black", fill="brown")
ggplot(dat, aes(x=belief))    + geom_histogram(binwidth =1, col="black", fill="brown")

ggplot(dat, aes(x=belief, y=legacy)) + geom_point(size=2,shape=3)

library(GGally)
ggpairs(dat[,c("legacy", "belief", "intention", "donation")], upper=list(continuous= wrap(ggally_cor, size=3.5, family="sans")))

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
modr <- lm(donation~legacy+belief+intention, data=dat)
summary(modr)
#Anova(lm(donation~legacy+belief+intention, data=dat), type=3)

# 5.
# Histogram of errors
hist(residuals(modg))
plot(predict(modg), residuals(modg), xlab="Predicted", ylab="Residual")
plot(modg)

# 6. Predict donation from legacy, belief, intention, education and income
modg <- lm(donation~legacy+belief+intention+education+income, data=dat)
summary(modg)

# 7. Modal comparison of 5 predictor model with 3 predictor model
#Anova(lm(donation~1, data=dat), modg)
anova(modr, modg)

