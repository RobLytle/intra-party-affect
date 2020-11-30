library(tidyverse)

# I want to keep everything as wide data, which is why I do it like this. one row; one respondent--all waves
naes_08 <- read_rds("data/raw/naes-trim-online.rds")%>%
	mutate_all(~ na_if(., 999))%>% #NAES codes skips as 999
	rename_all(list(~ str_replace(., "mb01", "election_attitude")))%>%
	rename_all(list(~ str_replace(., "ma01", "party_id")))%>%
	rename_all(list(~ str_replace(., "mb02", "parties_att")))%>%
	rename_all(list(~ str_replace(., "mb05", "offs_not_interested")))%>%
	rename_all(list(~ str_replace(., "mb06", "no_gov_influence")))%>%
	rename_all(list(~ str_replace(., "rba01", "repubs_first_choice")))%>%
	rename_all(list(~ str_replace(., "rba02", "repubs_sec_choice")))%>%
	rename_all(list(~ str_replace(., "rba03", "dems_first_choice")))%>%
	rename_all(list(~ str_replace(., "rba04", "dems_sec_choice")))%>%
	rename_all(list(~ str_replace(., "rea01", "conf_votes_count")))%>%
	rename_all(list(~ str_replace(., "reb01", "primaries_good_bad")))%>%
	rename_all(list(~ str_replace(., "rcb04", "turnout_general")))%>%
	rename_all(list(~ str_replace(., "rcb05", "pres_vote_choice_general")))%>%
	rename_all(list(~ str_replace(., "rcc01", "mc_vote_general")))%>%
	rename_all(list(~ str_replace(., "rcc02", "sen_vote_general")))%>%
	rename_all(list(~ str_replace(., "rcc04", "gov_vote_general")))%>%
	mutate_at(vars(starts_with("party_id")), list( ~ factor(recode(., .default = NA_character_, #recodes as a factor
																														"1" = "Strong Republican",
																														"2" = "Republican",
																														"3" = "Lean Republican",
																														"4" = "Independent",
																														"5" = "Lean Democrat",
																														"6" = "Democrat",
																														"7" = "Strong Democrat"),
																													levels = c("Strong Republican",
																																		 "Republican",
																																		 "Lean Republican",
																																		 "Independent",
																																		 "Lean Democrat",
																																		 "Democrat",
																																		 "Strong Democrat"))))%>%
	mutate_at(vars(starts_with("party_id")), list(three = ~ case_when(
		str_detect(., "Republican") ~ "Republican",
		str_detect(., "Independent") ~ "Independent",
		str_detect(., "Democrat") ~ "Democrat")
	))%>%
	rename_at(vars(ends_with("_three")), list(~ paste("three", gsub("_three", "", .), sep = "_")))%>%
	rename_all(list(~ str_replace(., "three_party_id", "pid_3")))%>%
	rename_all(list(~ str_replace(., "party_id", "pid_7")))%>%
	mutate_at(vars(starts_with("repubs_first_choice")), list( ~ recode(., .default = "Other Republican",
																																	 "2" = "Rudy Giuliani",
																																	 "3" = "Mike Huckabee",
																																	 "5" = "John McCain",
																																	 "6" = "Ron Paul")))%>%
	mutate_at(vars(starts_with("dems_first_choice")), list( ~ recode(., .default = "Other Democrat",
																																		 "2" = "Hillary Clinton",
																																		 "7" = "Barack Obama")))%>%
	mutate_at(vars(contains("vote_general")), list( ~ recode(.,
																													 "1" = "Republican",
																													 "2" = "Democrat",
																													 "3" = "Other",
																													 "4" = "Didn't Vote",
																													 "999" = "Skipped",
																													 "9999" = NA_character_)))%>%
	mutate(turnout_general_5 = recode(turnout_general_5,
																		"1" = "Didn't Vote",
																		"2" = "Didn't Vote",
																		"3" = "Voted",
																		"4" = "Voted"))%>%
	mutate(pres_vote_choice_general_5 = recode(pres_vote_choice_general_5,
																					 "1" = "McCain",
																					 "2" = "Obama",
																					 "3" = "Nader",
																					 "4" = "Barr",
																					 "5" = "Other"))%>%
	mutate(first_choice_1 = as.factor(if_else(!is.na(dems_first_choice_1), dems_first_choice_1, repubs_first_choice_1)))%>%
	mutate(first_choice_2 = as.factor(if_else(!is.na(dems_first_choice_2), dems_first_choice_2, repubs_first_choice_2)))%>%
	mutate(first_choice_dum_1 = recode(first_choice_1, .default = "Loser",
																		 "John McCain" = "Winner",
																		 "Barack Obama" = "Winner" ))%>%
	mutate(first_choice_dum_2 = recode(first_choice_2, .default = "Loser",
																		 "John McCain" = "Winner",
																		 "Barack Obama" = "Winner" ))%>%
	mutate(pres_election_inparty = as.factor(case_when(pid_3_1 == "Democrat" & pres_vote_choice_general_5 == "Obama" ~ "Voted Inparty",
																							pid_3_2 == "Democrat" & pres_vote_choice_general_5 != "Obama" ~ "Voted Outparty",
																							pid_3_2 == "Republican" & pres_vote_choice_general_5 == "McCain" ~ "Voted Inparty",
																							pid_3_2 == "Republican" & pres_vote_choice_general_5 != "McCain" ~ "Voted Outparty",
																							pid_3_2 == "Democrat" | pid_3_2 == "Republican" ~ "Didn't Vote/Skipped",
																							TRUE ~ NA_character_)))%>%
	mutate(mc_election_inparty = as.factor(case_when(pid_3_1 == "Democrat" & mc_vote_general_5 == "Democrat" ~ "Voted Inparty",
																										 pid_3_2 == "Democrat" & mc_vote_general_5 != "Democrat" ~ "Voted Outparty",
																										 pid_3_2 == "Republican" & mc_vote_general_5 == "Republican" ~ "Voted Inparty",
																										 pid_3_2 == "Republican" & mc_vote_general_5 != "Republican" ~ "Voted Outparty",
																										 pid_3_2 == "Democrat" | pid_3_2 == "Republican" ~ "Didn't Vote/Skipped",
																										 TRUE ~ NA_character_)))%>%
	mutate(sen_election_inparty = as.factor(case_when(pid_3_1 == "Democrat" & sen_vote_general_5 == "Democrat" ~ "Voted Inparty",
																									 pid_3_2 == "Democrat" & sen_vote_general_5 != "Democrat" ~ "Voted Outparty",
																									 pid_3_2 == "Republican" & sen_vote_general_5 == "Republican" ~ "Voted Inparty",
																									 pid_3_2 == "Republican" & sen_vote_general_5 != "Republican" ~ "Voted Outparty",
																									 pid_3_2 == "Democrat" | pid_3_2 == "Republican" ~ "Didn't Vote/Skipped",
																									 TRUE ~ NA_character_)))%>%
	mutate(gov_election_inparty = as.factor(case_when(pid_3_1 == "Democrat" & gov_vote_general_5 == "Democrat" ~ "Voted Inparty",
																										pid_3_2 == "Democrat" & gov_vote_general_5 != "Democrat" ~ "Voted Outparty",
																										pid_3_2 == "Republican" & gov_vote_general_5 == "Republican" ~ "Voted Inparty",
																										pid_3_2 == "Republican" & gov_vote_general_5 != "Republican" ~ "Voted Outparty",
																										pid_3_2 == "Democrat" | pid_3_2 == "Republican" ~ "Didn't Vote/Skipped",
																										TRUE ~ NA_character_)))%>%
	mutate(pres_election_inparty_num = as.numeric(as.character(recode(pres_election_inparty,
																													"Voted Inparty" = "1",
																													"Voted Outparty" = "0",
																													"Didn't Vote/Skipped" = NA_character_))))%>%
	mutate(mc_election_inparty_num = as.numeric(as.character(recode(mc_election_inparty,
																																			 "Voted Inparty" = "1",
																																			 "Voted Outparty" = "0",
																																			 "Didn't Vote/Skipped" = NA_character_))))%>%
	mutate(sen_election_inparty_num = as.numeric(as.character(recode(sen_election_inparty,
																																			 "Voted Inparty" = "1",
																																			 "Voted Outparty" = "0",
																																			 "Didn't Vote/Skipped" = NA_character_))))%>%
	mutate(gov_election_inparty_num = as.numeric(as.character(recode(gov_election_inparty,
																																			 "Voted Inparty" = "1",
																																			 "Voted Outparty" = "0",
																																			 "Didn't Vote/Skipped" = NA_character_))))%>%
	mutate(first_pid = pid_7_1)%>%
	mutate_at(vars(starts_with("pid_7")), list(num = ~ as.numeric(.)))%>%
	mutate_at(vars(matches("\\d_num")), list(generic = ~ if_else(str_detect(first_pid, "Rep"), as.numeric(recode(., #creating a partisanship measure relative to the first_pid where 1 is strong partisan of the frmr outparty, 7 is strong partisan of inparty
																																																"1" = "7",
																																																"2" = "6",
																																																"3" = "5",
																																																"4" = "4",
																																																"5" = "3",
																																																"6" = "2",
																																																"7" = "1")), .)))%>%
	mutate_at(vars(ends_with("generic")), list(dum = ~ if_else(. == 7, 1, 0)))%>%
	rename(strong_part_dum_1 = pid_7_1_num_generic_dum)%>%
	rename(strong_part_dum_2 = pid_7_2_num_generic_dum)%>%
	rename(strong_part_dum_3 = pid_7_3_num_generic_dum)%>%
	rename(strong_part_dum_4 = pid_7_4_num_generic_dum)%>%
	rename(strong_part_dum_5 = pid_7_5_num_generic_dum)%>%
	mutate(change_pid_1_2 = pid_7_2_num_generic - pid_7_1_num_generic)%>%
	mutate(change_pid_2_3 = pid_7_3_num_generic - pid_7_2_num_generic)%>%
	mutate(change_pid_3_4 = pid_7_4_num_generic - pid_7_3_num_generic)%>%
	mutate(change_pid_4_5 = pid_7_5_num_generic - pid_7_4_num_generic)%>%
	mutate(strong_part_1_2 = strong_part_dum_2 - strong_part_dum_1)%>%
	mutate(strong_part_2_3 = strong_part_dum_3 - strong_part_dum_2)%>%
	mutate(strong_part_3_4 = strong_part_dum_4 - strong_part_dum_3)%>%
	mutate(strong_part_4_5 = strong_part_dum_5 - strong_part_dum_4)%>%
	mutate(increase_pid_1_2 = if_else(change_pid_1_2 > 0, 1, 0))%>%
	mutate(increase_pid_2_3 = if_else(change_pid_2_3 > 0, 1, 0))%>%
	mutate(increase_pid_3_4 = if_else(change_pid_3_4 > 0, 1, 0))%>%
	mutate(increase_pid_4_5 = if_else(change_pid_4_5 > 0, 1, 0))%>%
	mutate(decrease_pid_1_2 = if_else(change_pid_1_2 < 0, 1, 0))%>%
	mutate(decrease_pid_2_3 = if_else(change_pid_2_3 < 0, 1, 0))%>%
	mutate(decrease_pid_3_4 = if_else(change_pid_3_4 < 0, 1, 0))%>%
	mutate(decrease_pid_4_5 = if_else(change_pid_4_5 < 0, 1, 0))%>%
	#	select(-ends_with("generic"))%>%
#	rename_at(vars(ends_with(pid_7_1_generic_.:pid_7_5_generic.), str_remove(., "_.")))%>%
#	rename_all(list(~ str_replace(., "_.", "")))%>%
	select(-starts_with("dems"),
				 -starts_with("rep"),
				 -contains("r3v"))%>%
	mutate(convention_end = as.Date((case_when(
																pid_3_1 == "Democrat" ~ "2008-08-28",
																pid_3_1 == "Republican" ~ "2008-09-04",
																TRUE ~ NA_character_))),
				presumptive_date = as.Date((case_when(
																pid_3_1 == "Democrat" ~ "2008-06-03",
																pid_3_1 == "Republican" ~ "2008-03-04",
																TRUE ~ NA_character_))),
				general_election = as.Date("2008-11-04"),)%>%
	mutate_at(vars(starts_with("date")), list( ~ as.Date(as.character(.),format="%Y%m%d")))%>%
	write_rds("data/naes-08.rds")%>%
	write_csv("data/naes-08.csv")%>%
	glimpse()
