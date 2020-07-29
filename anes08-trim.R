library(tidyverse)
library(foreign)

off_waves <- read.spss("data/raw/anes2008_2009paneloffwaves.sav")%>%
	write_rds("data/raw/2008_off_waves.rds")%>%
	glimpse()

ANES_2008 <- rio::import("data/raw/anes2008_2009panelpor.zip", which = "anes2008_2009panel.por")%>%
	glimpse()

rbind(off_waves, ANES_2008)
