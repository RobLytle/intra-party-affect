library(tidyverse)
library(haven)
library(broom)

########
## This script is experimental at the moment
## Pay it no mind
########
cdf <- read_rds("data/tidy-cdf.rds")%>%
  glimpse

therm_ideo_fit <- lm(parties_therm_dif ~ parties_ideo_dif, data = cdf)
gtif<- glance(therm_ideo_fit)
gtif

tif_obs <- augment(therm_ideo_fit)
tif_obs


ggplot(tif_obs, aes(x = .fitted, y = .resid)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red")
