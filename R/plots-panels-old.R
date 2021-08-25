library(tidyverse)


naes_08 <- read_rds("data/naes-08.rds")%>%
	glimpse()
#testing the effect of losing the primary on pid_str

#Doing the strength change by group rather than paired. lots of right-censoring when paired
pid_str_boot_df <- data.frame(boot = 1:1000)%>%
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