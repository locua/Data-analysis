library(sdamr)
data(anchoring)
head(anchoring)

data("tetris2015")
head(tetris2015)
dat<-tetris2015

dat$intrusions <-dat$Days_One_to_Seven_Number_of_Intrusions

set.seed(20201104)
plot_raincloud(dat, intrusions, groups=Condition)

dat<-subset(dat, Condition %in% c("Tetris_Reactivation", "Reactivation"))

dat$dummy <- 0
dat$dummy[dat$Condition == "Reactivation"] <- 1

mod <- lm(intrusions ~ dummy, data=dat)
summary(mod)

dat$effect <- ifelse(dat$Condition=="Reactivation",.5,-.5)

mod <- lm(intrusions~effect, data=dat)
summary(mod)

dat<-tetris2015
dat$intrusions <-dat$Days_One_to_Seven_Number_of_Intrusions
dat$c1 <- ifelse(dat$Condition == "Control", 3/4, -1/4)
dat$c2<-0
dat$c2[dat$Condition=="Tetris"] <- 2/3
dat$c2[dat$Condition %in% c("Tetrix_Reactivation", "Reactivation")] <- -1/3

# use and sapply and switch
dat$c3 <- sapply(as.character(dat$Condition), switch, "Control"=0, "Tetris_Reactivation"=-1/2, "Tetris"=0, "Reactivation"=1/2)

modg<-lm(intrusions~c1+c2+c3, data=dat)
summary(modg)

car::Anova(modg, type=3)

# This would be useful if dat$Condition is a character vector
# it is not needed here!
#dat$Condition <- as.factor(dat$Condition)

contrasts(dat$Condition)

codes<-cbind(c(3/4, -1/4, -1/4, -1/4),
             c(0,   -1/3,  2/3, -1/3),
             c(0,   -1/2,  0,    1/2))

colnames(codes) <- c(" ctrl-vs-other"," tetr-vs-memory"," react-vs-t+r")
codes
contrasts(dat$Condition)<-codes

modg <- lm(intrusions~Condition, data=dat)
summary(modg)

head(model.matrix(modg))
