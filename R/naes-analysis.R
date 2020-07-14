library(tidyverse)

naes_08_wave <- read_rds("data/tidy-naes-08.rds")%>%
	select(wave,
				 pid3,
				 loser_first,
				 strong_part,
				 wave)%>%
	filter(pid3 == "Republican" | pid3 == "Democrat")%>%
	group_by(wave, loser_first, pid3)%>%
	summarize(prop_strong = mean(strong_part, na.rm = TRUE))%>%
	mutate(wave = recode(wave,
											 "1" = "first",
											 "2" = "second",
											 "3" = "third",
											 "4" = "fourth",
											 "5" = "fifth",
											 "6" = "sixth"))%>%
	pivot_wider(names_from = wave,
							values_from = prop_strong)%>%
	group_by(loser_first, pid3)%>%
	summarize(diff_prop = fifth - third)%>%
	glimpse()

naes_08_date <- read_rds("data/tidy-naes-08.rds")%>%
	select(convention_dummy,
				 pid3,
				 loser_first,
				 strong_part,
				 wave)%>%
	filter(pid3 == "Republican" | pid3 == "Democrat")%>%
	group_by(convention_dummy, loser_first, pid3)%>%
	summarize(prop_strong = mean(strong_part, na.rm = TRUE))%>%
	glimpse()

# very little to see here. 


