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
	mutate(convention_dummy = recode(convention_dummy,
																	 'Post Convention' = "post_convention",
																	 'Pre Convention' = "pre_convention"))%>%
	filter(pid3 == "Republican" | pid3 == "Democrat")%>%
	group_by(convention_dummy, loser_first, pid3)%>%
	summarize(prop_strong = mean(strong_part, na.rm = TRUE))%>%
	pivot_wider(names_from = convention_dummy,
							values_from = prop_strong)%>%
	group_by(pid3, loser_first)%>%
	summarize(difference = post_convention - pre_convention)%>%
	pivot_wider(names_from = loser_first,
							values_from = difference)%>%
	group_by(pid3)%>%
	summarize(losers_dif_winners = loser - winner)%>%
		glimpse()

naes_loess_df <- read_rds("data/tidy-naes-08-online.rds")%>%
	filter(pid3 == "Republican" | pid3 == "Democrat" & !is.na(loser_first))%>%
	glimpse()


partisan_loess_08 <- ggplot(naes_loess_df, aes(x = date, y = strong_part, color = loser_first)) +
	geom_smooth() +
	geom_vline(aes(xintercept = convention_end), linetype=4) +
	geom_vline(aes(xintercept = presumptive_date), linetype=1) +
	geom_vline(aes(xintercept = general_election), linetype=2) +
	facet_wrap(vars(pid3))
partisan_loess_08

ggsave("fig/partisan_loess_08.png", plot = partisan_loess_08, width = 6, height = 4, units = "in")
# dif in dif of about#parallel trends association

# run a few models loess over dates

