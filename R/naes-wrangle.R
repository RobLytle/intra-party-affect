library(tidyverse)

naes_df <- read_rds("data/raw/naes-trim.rds")%>%
	pivot_longer(wave_1:wave_b, values_to = "wave", names_to = "dropwave")%>%
	mutate(wave = recode(wave,
											 "101" = "1",
											 "102" = "2",
											 "103" = "3",
											 "104" = "4",
											 "105" = "5",
											 "201" = "a",
											 "202" = "b"))%>%
	pivot_longer(ma01_1:ma01_5, values_to = "pid_strength", names_to = "wave_match")%>%
	mutate(wave_match = as.numeric(str_replace(wave_match, "ma01_", "")))%>%
	filter(wave == wave_match)%>%
	select(-wave_match)%>%
	pivot_longer(date_1:date_b, values_to = "date", names_to = "wave_match")%>%
	mutate(wave_match = as.numeric(str_replace(wave_match, "date_", "")))%>%
	filter(wave == wave_match)%>%
	glimpse()

	pivot_longer(mb01_1:mb01_5, values_to = "elect_gov_att", names_to = "dropattention")%>%
	pivot_longer(mb02_1:mb02_5, values_to = "parties_gov_att", names_to = "dropattparties")%>%
	glimpse()