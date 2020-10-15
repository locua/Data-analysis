library(sdamr)
# 1. dat <- subset(anchoring, lab_or_online == "Online")
# 2. datlow <- subset(dat, anchor=="low")
# 3. dat_new <- subset(anchoring , referrer=="jmu" | referrer=="charles")
# 4. dat_hard <- subset(anchoring, referrer=="jmu" & anchor=="high" | referrer=="charles" & anchor=="high")

data("anchoring")
# 1.
df1 <- anchoring[ which( anchoring$us_or_international == "US" & anchoring$lab_or_online=="In-lab" & anchoring$anchor=="high") , ]
#df1_ <- subset
# 2.
hist(df1$everest_feet)

plot_raincloud(df1, y=everest_feet)

m1 <- mean(anchoring$everest_feet)
print(m1)
s1 = sd(anchoring$everest_meters)

t.test(df1$everest_feet, mu=29029) # Two sided one-sample t-test

t.test(df1$everest_feet, mu=29030, alternative = "greater")

t.test(anchoring$everest_feet, mu=29029)

plot_raincloud(anchoring, y=everest_feet)
