data("anchoring")

library(sdamr)

# Inspect everest feet data by anchor group
plot_raincloud(anchoring, everest_feet, groups=anchor)

# Create dummy variable
anchoring$dummy <- ifelse(anchoring$anchor == "low", 0, 1)
# Estimate regression model predicting everest_feet from dummy
summary(lm(everest_feet~dummy, data=anchoring))
  