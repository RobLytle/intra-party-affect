library(tidyverse)

poll_df <- read_csv("data/polling/pres_primary_avgs_1980-2016.csv")%>%
	filter(state == "National")%>%
	mutate(year_race = as.numeric(str_remove(race, ".$")))%>%
	glimpse()