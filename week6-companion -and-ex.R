library(sdamr)
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

