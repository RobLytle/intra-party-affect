library(tidyverse)
theme_set(theme_minimal())

naes_08 <- read_rds("data/naes-08.rds")%>%
	filter(pid_3_1 == "Republican" | pid_3_1 == "Democrat")%>%
	group_by(first_choice_dum_2,
					 pid_3_1)%>%
	summarize(prop_inparty_pres = mean(pres_election_inparty_num, na.rm = TRUE),
						prop_inparty_mc = mean(mc_election_inparty_num, na.rm = TRUE),
						prop_inparty_sen = mean(sen_election_inparty_num, na.rm = TRUE),
						prop_inparty_gov = mean(gov_election_inparty_num, na.rm = TRUE))%>%
	pivot_wider(names_from = first_choice_dum_2,
							values_from = prop_inparty_pres:prop_inparty_gov)%>%
	janitor::clean_names()%>%
	select(-ends_with("NA"))%>%
	pivot_longer(prop_inparty_pres_winner:prop_inparty_gov_loser, 
							 names_to = c("election", ".value"), 
							 names_pattern="(.*)_([a-z]*)")%>%
	group_by(pid_3_1,
					 election,)%>%
	summarize(prop_difference = winner-loser)%>%
#	pivot_longer(prop_inparty_pres:prop_inparty_gov, names_to = "election", values_to = "prop_inparty")%>%
#	filter(!is.na(prop_inparty) & !is.na(first_choice_dum_2) & pid_3_1 != "Independent")%>%
	glimpse()


ggplot(naes_08, aes(x = prop_difference, y = election, color = pid_3_1, shape = pid_3_1)) +
	scale_color_manual(values = c("Democrat" = "dodgerblue3",
																"Republican" = "firebrick3")) +
	geom_vline(xintercept = 0.00) +
	geom_point()