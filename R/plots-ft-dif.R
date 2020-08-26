library(tidyverse)
library(ggExtra)
library(ggridges)
library(goji)
library(gridExtra)
library(janitor)
theme_set(theme_minimal())

#below_50_dum = if_else(therm_inparty < 50, 1, 0),

opinion_proportions_df <- read_rds("data/tidy-cdf.rds")%>%
	filter(year >= 1978 & pid_3 != "Independent")%>%
	mutate(below_50_qual = case_when(therm_inparty < 50 & pid_3 != "Independent" ~ "cold",
																	therm_parties_mean < 50 & pid_3 == "Independent" ~ "cold",
																	therm_inparty >= 50 | therm_parties_mean >= 50 ~ "warm",
																	TRUE ~ NA_character_))%>%
	group_by(year, pid_3, below_50_qual)%>%
	summarize(prop_dissat = weighted.mean(dis_democ_dum, weight, na.rm = TRUE),
						prop_distrust = weighted.mean(distrust_gov_dum, weight, na.rm = TRUE),
						prop_gov_few = weighted.mean(gov_run_for_few_dum, weight, na.rm = TRUE),
						prop_wrong_track = weighted.mean(wrong_track_dum, weight, na.rm = TRUE),
						prop_officials_dont_care = weighted.mean(officials_dont_care_dum, weight, na.rm = TRUE),
						prop_wealth_gap_larger = weighted.mean(wealth_gap_larger_dum, weight, na.rm = TRUE),
						prop_vote_general = weighted.mean(general_vote_dum, weight, na.rm = TRUE))%>%
	pivot_wider(names_from = below_50_qual,
							values_from = prop_dissat:prop_vote_general)%>%
	select(-ends_with("NA"))%>%
	group_by(year, pid_3)%>%
	summarize(dif_dissat = prop_dissat_cold - prop_dissat_warm,
						dif_distrust = prop_distrust_cold - prop_distrust_warm,
						dif_gov_few = prop_gov_few_cold - prop_gov_few_warm,
						dif_wrong_track = prop_wrong_track_cold - prop_wrong_track_warm,
						dif_offs_dont_care = prop_officials_dont_care_cold - prop_officials_dont_care_warm,
						dif_wealth_gap_larger = prop_wealth_gap_larger_cold - prop_wealth_gap_larger_warm)%>%
	pivot_longer(cols = dif_dissat:dif_wealth_gap_larger,
							 names_to = "which_question",
							 values_to = "prop_difference")%>%
	mutate(which_question = as.factor(which_question))%>%
	filter(!is.na(prop_difference))%>%
	glimpse()


# making the plot

gg_prop_diffs <- ggplot(opinion_proportions_df, aes(x = year, y = prop_difference)) +
	#  geom_errorbar(aes(ymin = prop_50_below - se_50_below, ymax = prop_50_below + se_50_below, width = .2)) +
	geom_line(aes(linetype = pid_3, color = pid_3), size = 1) + 
	#  geom_smooth(aes(linetype = pid_3_sort, color = pid_3_sort), span = .3, se = FALSE) +
	geom_point(aes(shape = pid_3, size = 1, color = pid_3)) +
	geom_hline(yintercept = 0) +
	scale_color_manual(values = c("Democrat" = "dodgerblue3",
																"Republican" = "firebrick3",
																"Independent" = "darkorchid3")) +
	#  scale_linetype_manual(values = c("Democrat" = "longdash",
	#                                   "Republican" = "solid",
	#                                   "Independent" = "dotted")) +
	facet_wrap(vars(which_question)) +
	theme(legend.position = c(0.9, 0.2)) +
	scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
	guides(size = FALSE) +
	labs(x = "Year", 
#			 subtitle = "Grouped by warmth towards in-party (partisans) or average warmth towards parties (independents)",
			 y = "Cold - Warm Proportion",
			 color = "Party ID",
			 linetype = "Party ID",
			 shape = "Party ID",
			 title = "Differences in Opinion Between Cold and Warm Partisans") #+
gg_prop_diffs

#Ideology (party more extreme dummy?)
#Activism/voting/donating

ggsave("fig/cold-minus-warm-props.png", gg_prop_diffs, width = 12, height = 8, units = "in")




behavior_proportions_df <- read_rds("data/tidy-cdf.rds")%>%
	filter(year >= 1978 & pid_3 != "Independent")%>%
	mutate(below_50_qual = case_when(therm_inparty < 50 & pid_3 != "Independent" ~ "cold",
																	 therm_parties_mean < 50 & pid_3 == "Independent" ~ "cold",
																	 therm_inparty >= 50 | therm_parties_mean >= 50 ~ "warm",
																	 TRUE ~ NA_character_))%>%
#	glimpse()
	group_by(year, pid_3, below_50_qual, pres_election)%>%
	summarize(prop_vote_general = weighted.mean(general_vote_dum, weight, na.rm = TRUE),
						prop_split_ticket = weighted.mean(split_ticket_dum, weight, na.rm = TRUE),
						prop_meetings = weighted.mean(meetings_dum, weight, na.rm = TRUE),
						prop_work_cand = weighted.mean(work_cand_dum, weight, na.rm = TRUE),
						prop_display_merch = weighted.mean(display_merch_dum, weight, na.rm = TRUE),
						prop_donate = weighted.mean(donate_dum, weight, na.rm = TRUE),
						prop_watch_campaign_tv = weighted.mean(watch_campaign_tv_dum, weight, na.rm = TRUE),
						prop_know_house_pre = weighted.mean(knows_house_pre_dum, weight, na.rm = TRUE),
						prop_know_house_post = weighted.mean(knows_house_post_dum, weight, na.rm = TRUE),
						prop_talk_pol_most = weighted.mean(talk_politics_most_days_dum, weight, na.rm = TRUE),
						prop_early_vote = weighted.mean(early_vote_dum, weight, na.rm = TRUE),
						prop_vote_inparty_house = weighted.mean(vote_inparty_house_dum, weight, na.rm = TRUE),
						prop_vote_inparty_pres = weighted.mean(vote_inparty_pres_dum, weight, na.rm = TRUE))%>%
	pivot_wider(names_from = below_50_qual,
							values_from = prop_vote_general:prop_vote_inparty_pres)%>%
	select(-ends_with("NA"))%>%
#	glimpse()
	pivot_longer(prop_vote_general_cold:prop_vote_inparty_pres_warm, 
							 names_to = c("prop_name", ".value"), 
							 names_pattern="(.*)_([a-z]*)")%>%
	mutate(prop_name = as.factor(prop_name))%>%
	group_by(year,
						pid_3,
						prop_name,
					 	pres_election)%>%
	summarize(prop_dif = cold - warm)%>%
	filter(!is.na(prop_dif))%>%
glimpse()

gg_behav_difs <- ggplot(behavior_proportions_df, aes(x = year, y = prop_dif)) +
	#  geom_errorbar(aes(ymin = prop_50_below - se_50_below, ymax = prop_50_below + se_50_below, width = .2)) +
	geom_line(aes(linetype = pid_3, color = pid_3), size = 1) + 
	#  geom_smooth(aes(linetype = pid_3_sort, color = pid_3_sort), span = .3, se = FALSE) +
	geom_point(aes(shape = pid_3, size = 1, color = pid_3)) +
	geom_hline(yintercept = 0) +
	scale_color_manual(values = c("Democrat" = "dodgerblue3",
																"Republican" = "firebrick3",
																"Independent" = "darkorchid3")) +
	#  scale_linetype_manual(values = c("Democrat" = "longdash",
	#                                   "Republican" = "solid",
	#                                   "Independent" = "dotted")) +
	facet_wrap(vars(prop_name)) +
	theme(legend.position = c(0.8, 0.1)) +
	scale_x_continuous(breaks = seq(1976, 2020, by = 4),
										 guide = guide_axis(n.dodge = 2)) +
	guides(size = FALSE) +
	labs(x = "Year", 
			 subtitle = "Behavioral Items",
			 y = "Cold - Warm Proportion",
			 color = "Party ID",
			 linetype = "Party ID",
			 shape = "Party ID",
			 title = "Differences in Behavior Between Cold and Warm Partisans") #+
gg_behav_difs

ggsave("fig/cold-minus-warm-behav.png", gg_behav_difs, width = 12, height = 8, units = "in")
