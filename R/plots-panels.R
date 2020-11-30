library(MASS)
library(ordinal)
library(tidyverse)
library(broom)
library(ordinal)
library(margins)
theme_set(theme_minimal())
dodge <- position_dodge(width=0.5)

behavior_outcomes <- read_rds("data/naes-08.rds")%>%
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


ggplot(behavior_outcomes, aes(x = prop_difference, y = election, color = pid_3_1, shape = pid_3_1)) +
	scale_color_manual(values = c("Democrat" = "dodgerblue3",
																"Republican" = "firebrick3")) +
	geom_vline(xintercept = 0.00) +
	geom_point()


#testing the effect of losing the primary on pid_str

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
						prop_strong_change_pid_1_2 = mean(str_pid_1_2, na.rm=TRUE),
						prop_strong_change_pid_2_3 = mean(str_pid_2_3, na.rm=TRUE),
						prop_strong_change_pid_3_4 = mean(str_pid_3_4, na.rm=TRUE),
						se_strong_change_pid_1_2 = sd(str_pid_1_2, na.rm = TRUE)/sqrt(length(.)),
						se_strong_change_pid_2_3 = sd(str_pid_2_3, na.rm = TRUE)/sqrt(length(.)),
						se_strong_change_pid_3_4 = sd(str_pid_3_4, na.rm = TRUE)/sqrt(length(.)),)%>%
	filter(!is.na(first_choice_dum_1) & !is.na(pid_3_1))%>%
	pivot_longer(mean_change_pid_1_2:se_strong_change_pid_3_4, 
							 names_to = c(".value", "wave"), 
							 names_pattern="(^[a-z]+).+(._.$)")%>%
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
	filter(!is.na(wave_presump))%>%
	mutate(wave_presump = fct_rev(as.factor(wave_presump)))%>%
	glimpse()

gg_three_wave <- ggplot(pid_str_df, aes(x = fct_rev(first_choice_dum_1), y = mean, color = pid_3_1)) +
	geom_linerange(aes(ymin = mean - 1.645*se, ymax = mean + 1.645*se, color = pid_3_1), position = dodge) +
	geom_point(position = dodge) +
	geom_hline(yintercept = 0) +
	scale_color_manual(values = c("Democrat" = "dodgerblue3",
																"Republican" = "firebrick3")) +
	facet_wrap(vars(wave_presump)) +
	theme(legend.position = c(0.12, 0.2)) +
	coord_cartesian(ylim = c(-.75, .75)) +
	labs(title = "Effect of Primary Victory on Strength of Party ID",
			 subtitle = "Change in Party Strength Between Waves",
			 x = "Primary Vote Choice",
			 y = "Mean Change in Partisanship Strength Between Waves",
			 color = "Party ID at Wave 1")
gg_three_wave

ggsave("fig/gg-three-wave-pid.png", gg_three_wave, width = 4, height = 6, units = "in")






## Oprobit



probit_df <- read_rds("data/naes-08.rds")%>%
	mutate_at(vars(starts_with("change_pid"), list( ~ if_else(. <= 0, 1, 0))))%>%
	select(pid_3_1,
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





