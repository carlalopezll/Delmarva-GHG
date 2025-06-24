# Playing around w/ regressions and the ggstatsplot package

library(ggstatsplot)

# This is pretty cool

ggscatterstats(data = SW, x = CO2_uM, y = CH4_uM) +
  theme

ggscatterstats(data = SW, x = NPOC, y = CO2_uM) +
  theme
