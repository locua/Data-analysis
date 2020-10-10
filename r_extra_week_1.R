library(ggplot2)
library(sdamr)

ax1 <- rbinom(20, 8, 0.5)
ax2 <- rbinom(20, 8, 0.5)
ax3 <- rbinom(20, 10000, 0.5)
ax4 <- rbinom(10000, 8, 0.5)
df1 <- data.frame(ax1, ax2, ax3, ax4)
mean(ax1)
median(ax1)
sample_mode(ax1)
sample_sd(ax1)
IQR(ax1)
#sub1 <- paste("Mean: ", mean(ax1), "Median: ", median(ax1), "Mode: ", sample_mode(ax1), "IQR: ", IQR(ax1), sep=" ")
#hist(ax1  , breaks=10, main=" ", sub=sub1, xlab="Correct guesses")
p1 <- ggplot(df1, aes(x=ax1)) + geom_histogram(binwidth=1, colour="black", fill='#8C8279') + 
  xlab("Correct guesses") +
  ggtitle("20 trials of 8 guesses")

p2 <- ggplot(df1, aes(x=ax2)) + geom_histogram(binwidth=1, colour="black", fill='#8C8279') + 
  xlab("Correct guesses") +
  ggtitle("20 trials of 8 guesses")

p3 <- ggplot(df1, aes(x=ax3)) + geom_histogram(binwidth=1, colour="black", fill='#8C8279') + 
  xlab("Correct guesses") +
  ggtitle("20 trials of 10,000 guesses")

p4 <- ggplot(df1, aes(x=ax4)) + geom_histogram(binwidth=1, colour="black", fill='#8C8279') + 
  xlab("Correct guesses") +
  ggtitle("10,000 trials of 8 guesses")
#plot(p1)
#plot(p2)
multiplot(p1, p2, p3, p4, cols=2)

#ggplot(df, aes(x=a1)) + geom_histogram(bins=8, colour="black", fill='#8C8279') + xlab("Correct guesses")

