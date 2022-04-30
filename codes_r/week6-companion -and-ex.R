library(sdamr)
library(ggplot2)
#rm(list=ls()) # remove all objects in global environment (?)
data("expBelief")
head(expBelief)

dat<-expBelief
dat$condition <- interaction(dat$primeCond, dat$experimenterBelief)
levels(dat$condition)

plot_raincloud(dat, ApproachAdvantage, groups=condition)

# Rename variable labels
# turn primeCond and experimenterBelief in factors and change the labels
#dat$primeCond <- factor(dat$primeCond, labels=c("PH","PL"))
#dat$experimenterBelief <- factor(dat$experimenterBelief, labels=c("EH","EL"))
# now create an interaction factor, and change the separation sign to "-" instead of "."
#dat$condition <- interaction(dat$primeCond, dat$experimenterBelief, sep="-")
#plot_raincloud(dat, ApproachAdvantage, groups=condition)

plot_raincloud(dat, ApproachAdvantage, groups=experimenterBelief) + facet_grid(~primeCond)

# or.. ?facet_wrap for more info
plot_raincloud(dat, ApproachAdvantage, groups=experimenterBelief) + facet_wrap(~primeCond)

# Check variables are factors
class(dat$primeCond)
class(dat$experimenterBelief)

dat$primeCond <- as.factor(dat$primeCond)
dat$experimenterBelief <- as.factor(dat$experimenterBelief)

contrasts(dat$primeCond)
contrasts(dat$primeCond) <- c(1/2, -1/2)

contrasts(dat$experimenterBelief)
contrasts(dat$experimenterBelief) <- c(1/2, -1/2)

modg <- lm(ApproachAdvantage ~ primeCond*experimenterBelief, data=dat)
summary(modg)
# or 
car::Anova(modg, type=3) # ?car::Anova ,types

class(dat$exptrNum) # integer
# Convert to factor
dat$exptrNum <- factor(dat$exptrNum, labels=paste0("E",1:4))

contrasts(dat$exptrNum)
contrasts(dat$exptrNum) <- cbind(c(-1/2, 1/2,   0,  0), 
                                 c(-1/3,-1/3, 2/3,  0),
                                 c(-1/4,-1/4,-1/4,3/4))
contrasts(dat$exptrNum)

plot_raincloud(dat, ApproachAdvantage, groups = experimenterBelief) + facet_grid(primeCond ~ exptrNum)

modg_exp <- lm(ApproachAdvantage ~ primeCond*experimenterBelief*exptrNum, data=dat)
summary(modg_exp)

car::Anova(modg_exp,type=3)

ftable(exptrNum ~ primeCond + experimenterBelief, data=dat)

car::Anova(modg_exp, type=2)

anova(modg_exp)

# Type 1 procedure
anova(lm(ApproachAdvantage ~ experimenterBelief*exptrNum*primeCond, data=dat))

library(emmeans)
emmeans(modg, specs = ~ primeCond:experimenterBelief)

emmeans(modg, specs = ~ primeCond)
emmeans(modg, specs = ~ experimenterBelief)

#####################################################################
# Week 6 exercises
dat<-read.csv("data/woolley.csv")

dat<-dat[dat$role=="management",]

plot_raincloud(dat, strike_days, groups=consumption)
plot_raincloud(dat, strike_days, groups=relationship)

class(dat$consumption)
dat$consumption <- as.factor(dat$consumption)
dat$relationship <- as.factor(dat$relationship)

contrasts(dat$consumption) <- c(1/2, -1/2)
contrasts(dat$relationship) <- c(-1/2, 1/2)

contrasts(dat$consumption)
contrasts(dat$relationship)

mod <- lm(strike_days ~ consumption*relationship, data=dat)

summary(mod)

plot(mod) # Check if normal        

qplot(mod$residuals, geom="density") +
  labs(title = "Density of residuals", x = "residual") 
# Skew present

ggplot(mod, aes(x=mod$residuals)) +
  geom_histogram(aes(y = ..density..),bins=30,fill="grey") +
  geom_density(colour="red") 










