library(MASS)
library(ordinal)
library(tidyverse)
library(broom)
library(ordinal)
library(margins)
library(patchwork)
set.seed(2001)
theme_set(theme_minimal())
dodge <- position_dodge(width=0.5)

naes_08 <- read_rds("data/naes-08.rds")%>%
	glimpse()

behavior_outcomes_df <- naes_08%>%
	filter(pid_3_1 == "Republican" | pid_3_1 == "Democrat")%>%
	group_by(first_choice_dum_1,
					 pid_3_1)%>%
	summarize(prop_inparty_pres = mean(pres_election_inparty_num, na.rm = TRUE),
						prop_inparty_mc = mean(mc_election_inparty_num, na.rm = TRUE),
						prop_inparty_sen = mean(sen_election_inparty_num, na.rm = TRUE),
						prop_inparty_gov = mean(gov_election_inparty_num, na.rm = TRUE))%>%
	pivot_wider(names_from = first_choice_dum_1,
							values_from = prop_inparty_pres:prop_inparty_gov)%>%
	janitor::clean_names()%>%
	select(-ends_with("NA"))%>%
	pivot_longer(prop_inparty_pres_winner:prop_inparty_gov_loser, 
							 names_to = c("election", ".value"), 
							 names_pattern="(.*)_([a-z]*)")%>%
	group_by(pid_3_1,
					 election)%>%
	summarize(prop_difference = winner-loser)%>%
#	pivot_longer(prop_inparty_pres:prop_inparty_gov, names_to = "election", values_to = "prop_inparty")%>%
#	filter(!is.na(prop_inparty) & !is.na(first_choice_dum_1) & pid_3_1 != "Independent")%>%
	glimpse()

behavior_boot_df <- data.frame(boot = 1:500)%>%
	group_by(boot)%>%
	do(sample_n(naes_08, nrow(naes_08), replace = TRUE))%>% #creating 2000 new datasets of equal size to the original
	group_by(boot,
					 first_choice_dum_1,
					 pid_3_1)%>%
	summarize(prop_inparty_pres = mean(pres_election_inparty_num, na.rm = TRUE),
						prop_inparty_mc = mean(mc_election_inparty_num, na.rm = TRUE),
						prop_inparty_sen = mean(sen_election_inparty_num, na.rm = TRUE),
						prop_inparty_gov = mean(gov_election_inparty_num, na.rm = TRUE))%>%
	filter(!is.na(first_choice_dum_1))%>%
	pivot_wider(names_from = first_choice_dum_1,
							values_from = prop_inparty_pres:prop_inparty_gov)%>%
	janitor::clean_names()%>%
	pivot_longer(prop_inparty_pres_winner:prop_inparty_gov_loser, 
							 names_to = c("election", ".value"), 
							 names_pattern="(.*)_([a-z]*)")%>%
	group_by(boot,
					 pid_3_1,
					 election)%>%
	summarize(prop_dif = (loser-winner))%>%
	group_by(
		pid_3_1,
		election)%>%
	#	summarize(prop_se = parameters::standard_error(prop_dif))%>%
	summarize(prop_se = sd(prop_dif))%>%
	glimpse()

behavior_joined <- left_join(behavior_outcomes_df, behavior_boot_df)%>%
	mutate(election = recode(election,
													 "prop_inparty_sen" = "Senate",
													 "prop_inparty_pres" = "President",
													 "prop_inparty_mc" = "House",
													 "prop_inparty_gov" = "Governor"))%>%
	glimpse()

gg_behav_difs <- ggplot(behavior_joined, aes(x = prop_difference, y = election, color = pid_3_1, shape = pid_3_1)) +
	geom_linerange(aes(xmin = prop_difference - 1.96*prop_se, 
										 xmax = prop_difference + 1.96*prop_se,
										 color = pid_3_1), 
								 position = dodge) +
	scale_color_manual(values = c("Democrat" = "dodgerblue3",
																"Republican" = "firebrick3")) +
	geom_vline(xintercept = 0.00) +
	geom_point(position = dodge, size = 3) +
	theme(legend.position = c(.2, .5),
				legend.title = element_blank()) +
	labs(title = "Difference in Voting Between Primary Winners/Losers",
			 x = "Difference in Proportion Voting Inparty",
			 y = "Election",
			 color = "Party ID",
			 shape = "Party ID",
			 caption = "Bootstrapped 95% CIs")
gg_behav_difs

ggsave("fig/gg-behav-difs.png", gg_behav_difs, width = 6, height = 4, units = "in")



#testing the effect of losing the primary on pid_str

#Doing the strength change by group rather than paired. lots of right-censoring when paired
pid_str_boot_df <- data.frame(boot = 1:500)%>%
	group_by(boot)%>%
	do(sample_n(naes_08, nrow(naes_08), replace = TRUE))%>% #creating 2000 new datasets of equal size to the original
	group_by(boot,
					 first_choice_dum_1,
					 pid_3_1)%>%
	summarize(prop_strong_1_2 = mean(strong_part_dum_2, na.rm=TRUE) - mean(strong_part_dum_1, na.rm=TRUE),
						prop_strong_2_3 = mean(strong_part_dum_3, na.rm=TRUE) - mean(strong_part_dum_2, na.rm=TRUE),
						prop_strong_3_4 = mean(strong_part_dum_4, na.rm=TRUE) - mean(strong_part_dum_3, na.rm=TRUE))%>%
	group_by(first_choice_dum_1,
					 pid_3_1)%>%
	summarize(se_strong_1_2 = sd(prop_strong_1_2, na.rm=TRUE),
						se_strong_2_3 =  sd(prop_strong_2_3, na.rm=TRUE),
						se_strong_3_4 =  sd(prop_strong_3_4, na.rm=TRUE))%>%
	pivot_longer(se_strong_1_2:se_strong_3_4, 
							 names_to = c(".value", "wave"), 
							 names_pattern="(^[a-z]+_[a-z]+).+(._.$)")%>%
	mutate(wave = recode(wave,
											 "1_2" = "Wave 1 to Wave 2",
											 "2_3" = "Wave 2 to Wave 3",
											 "3_4" = "Wave 3 to Wave 4"))%>%
	ungroup()%>%
	mutate(wave_presump = case_when(pid_3_1 == "Democrat" & wave == "Wave 2 to Wave 3" ~ "Before Presumptive",
																	pid_3_1 == "Republican" & wave == "Wave 1 to Wave 2" ~ "Before Presumptive",
																	pid_3_1 == "Democrat" & wave == "Wave 3 to Wave 4" ~ "After Presumptive",
																	pid_3_1 == "Republican" & wave == "Wave 2 to Wave 3" ~ "After Presumptive",
																	TRUE ~ NA_character_))%>%
	#	rename(prop_strong_change = prop)%>%
	filter(!is.na(wave_presump))%>%
	mutate(wave_presump = fct_rev(as.factor(wave_presump)))%>%
	glimpse()


pid_str_dum_df <- naes_08%>%
	filter(date_2 < presumptive_date & pid_3_1 == "Republican" | date_3 < presumptive_date & pid_3_1 == "Democrat")%>%
	group_by(pid_3_1,
					 first_choice_dum_1)%>%
	summarize(prop_strong_1_2 = mean(strong_part_dum_2, na.rm=TRUE) - mean(strong_part_dum_1, na.rm=TRUE),
						prop_strong_2_3 = mean(strong_part_dum_3, na.rm=TRUE) - mean(strong_part_dum_2, na.rm=TRUE),
						prop_strong_3_4 = mean(strong_part_dum_4, na.rm=TRUE) -mean(strong_part_dum_3, na.rm=TRUE))%>%
	pivot_longer(prop_strong_1_2:prop_strong_3_4, 
							 names_to = c(".value", "wave"), 
							 names_pattern="(^[a-z]+_[a-z]+).+(._.$)")%>%
	mutate(wave = recode(wave,
											 "1_2" = "Wave 1 to Wave 2",
											 "2_3" = "Wave 2 to Wave 3",
											 "3_4" = "Wave 3 to Wave 4"))%>%
	ungroup()%>%
	mutate(wave_presump = case_when(pid_3_1 == "Democrat" & wave == "Wave 2 to Wave 3" ~ "Before Presumptive",
																	pid_3_1 == "Republican" & wave == "Wave 1 to Wave 2" ~ "Before Presumptive",
																	pid_3_1 == "Democrat" & wave == "Wave 3 to Wave 4" ~ "After Presumptive",
																	pid_3_1 == "Republican" & wave == "Wave 2 to Wave 3" ~ "After Presumptive",
																	TRUE ~ NA_character_))%>%
	#	rename(prop_strong_change = prop)%>%
	filter(!is.na(wave_presump))%>%
	mutate(wave_presump = fct_rev(as.factor(wave_presump)))%>%
	glimpse()

joined_prop_str_df <- left_join(pid_str_dum_df, pid_str_boot_df)%>%
	glimpse()


pid_str_df <- read_rds("data/naes-08.rds")%>%
	filter(date_2 < presumptive_date & pid_3_1 == "Republican" | date_3 < presumptive_date & pid_3_1 == "Democrat")%>%
	group_by(pid_3_1,
					 first_choice_dum_1)%>%
	summarize(mean_change_pid_1_2 = mean(change_pid_1_2, na.rm=TRUE),
						mean_change_pid_2_3 = mean(change_pid_2_3, na.rm=TRUE),
						mean_change_pid_3_4 = mean(change_pid_3_4, na.rm=TRUE),
						se_change_pid_1_2 = sd(change_pid_1_2, na.rm = TRUE)/sqrt(length(.)),
						se_change_pid_2_3 = sd(change_pid_2_3, na.rm = TRUE)/sqrt(length(.)),
						se_change_pid_3_4 = sd(change_pid_3_4, na.rm = TRUE)/sqrt(length(.)),
						prop_increase_change_pid_1_2 = mean(increase_pid_1_2, na.rm=TRUE),
						prop_increase_change_pid_2_3 = mean(increase_pid_2_3, na.rm=TRUE),
						prop_increase_change_pid_3_4 = mean(increase_pid_3_4, na.rm=TRUE),
						se_increase_change_pid_1_2 = sd(increase_pid_1_2, na.rm = TRUE)/sqrt(length(.)),
						se_increase_change_pid_2_3 = sd(increase_pid_2_3, na.rm = TRUE)/sqrt(length(.)),
						se_increase_change_pid_3_4 = sd(increase_pid_3_4, na.rm = TRUE)/sqrt(length(.)),
						prop_decrease_change_pid_1_2 = mean(decrease_pid_1_2, na.rm=TRUE),
						prop_decrease_change_pid_2_3 = mean(decrease_pid_2_3, na.rm=TRUE),
						prop_decrease_change_pid_3_4 = mean(decrease_pid_3_4, na.rm=TRUE),
						se_decrease_change_pid_1_2 = sd(decrease_pid_1_2, na.rm = TRUE)/sqrt(length(.)),
						se_decrease_change_pid_2_3 = sd(decrease_pid_2_3, na.rm = TRUE)/sqrt(length(.)),
						se_decrease_change_pid_3_4 = sd(decrease_pid_3_4, na.rm = TRUE)/sqrt(length(.)),)%>%
	filter(!is.na(first_choice_dum_1) & !is.na(pid_3_1))%>%
	pivot_longer(mean_change_pid_1_2:se_decrease_change_pid_3_4, 
							 names_to = c(".value", "wave"), 
							 names_pattern="(^[a-z]+_[a-z]+).+(._.$)")%>%
#	mutate(value = if_else(str_detect(waves, "mean"), "Mean", "SE"))%>%
	mutate(wave = recode(wave,
				 "1_2" = "Wave 1 to Wave 2",
				 "2_3" = "Wave 2 to Wave 3",
				 "3_4" = "Wave 3 to Wave 4"))%>%
	ungroup()%>%
	mutate(wave_presump = case_when(pid_3_1 == "Democrat" & wave == "Wave 2 to Wave 3" ~ "Before Presumptive",
																	pid_3_1 == "Republican" & wave == "Wave 1 to Wave 2" ~ "Before Presumptive",
																	pid_3_1 == "Democrat" & wave == "Wave 3 to Wave 4" ~ "After Presumptive",
																	pid_3_1 == "Republican" & wave == "Wave 2 to Wave 3" ~ "After Presumptive",
																	TRUE ~ NA_character_))%>%
#	rename(prop_strong_change = prop)%>%
	filter(!is.na(wave_presump))%>%
	mutate(wave_presump = fct_rev(as.factor(wave_presump)))%>%
	glimpse()

#joining the vote choicedfs
joined_df <- left_join(pid_str_df, joined_prop_str_df)%>%
	write_rds("data/gg-three-wave-str.rds")%>%
	glimpse()



# Party ID STR dummy
gg_three_wave_str <- ggplot(joined_df, aes(x = fct_rev(first_choice_dum_1), y = prop_strong, color = pid_3_1)) +
	geom_linerange(aes(ymin = prop_strong - 1.95*se_strong, ymax = prop_strong + 1.95*se_strong, color = pid_3_1), position = dodge) +
	geom_point(position = dodge) +
	geom_hline(yintercept = 0) +
	scale_color_manual(values = c("Democrat" = "dodgerblue3",
																"Republican" = "firebrick3")) +
	facet_wrap(vars(wave_presump)) +
	theme(legend.position = c(0.12, 0.2)) +
	coord_cartesian(ylim = c(-.3, .3)) +
	labs(title = "Change in Proportion of Strong Partisans",
			 subtitle = "Change Between Waves",
			 x = "Primary Vote Choice",
			 y = "Difference Between Waves",
			 color = "Party ID at Wave 1",
			 caption = "Bootstrapped 95% CIs") +
	scale_y_continuous(n.breaks = 10)
gg_three_wave_str

ggsave("fig/gg-three-wave-str.png", gg_three_wave_str, width = 6, height = 4, units = "in")


probit_df <- read_rds("data/naes-08.rds")%>%
#	mutate_at(vars(starts_with("change_pid"), list(decrease = ~ if_else(. < 0, 1, 0))))%>%
	select(pid_str,
				 pid_3_1,
				 first_choice_dum_1,
				 pid_7_1,
				 pid_7_2,
				 starts_with("change_pid"))%>%
	group_by(pid_3_1)%>%
	filter(pid_3_1 == "Republican" | pid_3_1 == "Democrat")%>%
	mutate(first_choice_dum_1 = as.numeric(recode(as.character(first_choice_dum_1),
																		 "Winner" = "1",
																		 "Loser" = "0")))%>%
	glimpse()

probit_results_2 <- do(probit_df,
#	tidy(polr(as.factor(change_pid_1_2) ~ first_choice_dum_1, data = probit_df)))%>%
	tidy(ordinal::clm(as.factor(change_pid_1_2) ~ first_choice_dum_1, data = ., link = "probit")))%>%
	janitor::clean_names()%>%
	mutate(term = factor(recode(term,
															"first_choice_dum_1" = "Estimate",
															"1|2" = "2",
															"0|1" = "1",
															"-6|-5" = "-5",
															"-5|-4" = "-4",
															"-4|-3" = "-3",
															"-3|-2" = "-2",
															"-2|-1" = "-1",
															"-1|0" = "0"), levels = c("-5",
																												"-4",
																												"-3",
																												"-2",
																												"-1",
																												"0",
																												"1",
																												"2")))%>%
	mutate(waves = "Wave 1 to 2")%>%
#	filter(coef_type == "location")%>%
glimpse()

ggplot(comb, aes(x = change_pid_1_2, y = fit)) +
	geom_point()

probit_results_3 <- do(probit_df,
											#	tidy(polr(as.factor(change_pid_1_2) ~ first_choice_dum_1, data = probit_df)))%>%
											tidy(ordinal::clm(as.factor(change_pid_2_3) ~ first_choice_dum_1, data = ., link = "probit",)))%>%
	janitor::clean_names()%>%
	mutate(term = factor(recode(term,
															"first_choice_dum_1" = "Estimate",
															"1|2" = "2",
															"0|1" = "1",
															"-6|-5" = "-5",
															"-5|-4" = "-4",
															"-4|-3" = "-3",
															"-3|-2" = "-2",
															"-2|-1" = "-1",
															"-1|0" = "0"), levels = c("-5",
																												"-4",
																												"-3",
																												"-2",
																												"-1",
																												"0",
																												"1",
																												"2")))%>%
	mutate(waves = "Wave 2 to 3")%>%
	#	filter(coef_type == "location")%>%
	glimpse()

probit_results_4 <- do(probit_df,
											#	tidy(polr(as.factor(change_pid_1_2) ~ first_choice_dum_1, data = probit_df)))%>%
											tidy(ordinal::clm(as.factor(change_pid_3_4) ~ first_choice_dum_1, data = ., link = "probit")))%>%
	janitor::clean_names()%>%
	mutate(term = factor(recode(term,
															"first_choice_dum_1" = "Estimate",
															"1|2" = "2",
															"0|1" = "1",
															"-6|-5" = "-5",
															"-5|-4" = "-4",
															"-4|-3" = "-3",
															"-3|-2" = "-2",
															"-2|-1" = "-1",
															"-1|0" = "0"), levels = c("-5",
																												"-4",
																												"-3",
																												"-2",
																												"-1",
																												"0",
																												"1",
																												"2")))%>%
	mutate(waves = "Wave 3 to 4")%>%
	#	filter(coef_type == "location")%>%
	glimpse()

joined_probit <- rbind(probit_results_2,
											probit_results_3,
											probit_results_4)%>%
  filter(coef_type == "location")%>%
	mutate(wave_presump = case_when(pid_3_1 == "Democrat" & waves == "Wave 2 to 3" ~ "Before Presumptive",
																	pid_3_1 == "Republican" & waves == "Wave 1 to 2" ~ "Before Presumptive",
																	pid_3_1 == "Democrat" & waves == "Wave 3 to 4" ~ "After Presumptive",
																	pid_3_1 == "Republican" & waves == "Wave 2 to 3" ~ "After Presumptive",
																	TRUE ~ NA_character_))%>%
	filter(!is.na(wave_presump))%>%
	mutate(wave_presump = fct_rev(as.factor(wave_presump)))%>%
	glimpse()

joined_probit%>%
	filter(coef_type == "intercept")%>%
ggplot(aes(y = estimate, x = term)) +
	geom_point() +
	facet_wrap(vars(wave_presump))
	
ggplot(joined_probit, aes(x = pid_3_1, y = estimate, color = pid_3_1)) +
	geom_linerange(aes(ymin = estimate - 1.96*std_error, ymax = estimate + 1.96*std_error, color = pid_3_1), position = dodge) +
	geom_point(position = dodge) +
	geom_hline(yintercept = 0) +
	facet_wrap(vars(wave_presump)) +
	scale_color_manual(values = c("Democrat" = "dodgerblue3",
																"Republican" = "firebrick3")) 





