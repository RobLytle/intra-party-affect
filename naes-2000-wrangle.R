library(tidyverse)
#since the reinterviews have more columns, I'm pivot_longer()ing them individually before rbind()ing with the rest of panels

naes_reint_a <- rio::import("data/raw/naes/2000/NAES 2000 Mult Reint Panel A Data.sav")%>%
	select(contains("key"),
				 contains("date"),
				 contains("r11"), #intend to vote for, rep
				 contains("r12"), #intend to vote, dem
				 contains("r17"), #actual vote for rep
				 contains("r18"), #actual vote for dem
				 contains("v01"), #pid_str
				 contains("v02"), #pid3
				 -ends_with("$"))%>%
	mutate(xdem_primary_vote = case_when(is.na(cr18) ~ as.character(cr12), #when intended ballot (cr12) blank and they're dem, use actual ballot
																			 !is.na(cr18) ~ as.character(cr18),
																			 TRUE  ~ NA_character_))%>% #same but for reps
	mutate(xrep_primary_vote = case_when(is.na(cr17) ~ as.character(cr11),
																			 !is.na(cr17) ~ as.character(cr17),
																			 TRUE ~ NA_character_))%>%
	pivot_longer(ckey:zv02,
							 names_to = c("wave", ".value"),
							 names_pattern = "(^.)([^\1]*)")%>%
	glimpse()

naes_reint_b <- rio::import("data/raw/naes/2000/NAES 2000 Mult Reint Panel B Data.sav")%>%
	select(contains("key"),
				 contains("date"),
				 contains("r11"), #intend to vote for, rep
				 contains("r12"), #intend to vote, dem
				 contains("r17"), #actual vote for rep
				 contains("r18"), #actual vote for dem
				 contains("v01"), #pid_str
				 contains("v02"), #pid3
				 -ends_with("$"))%>%
	mutate(xdem_primary_vote = case_when(is.na(cr18) ~ as.character(cr12), #when intended ballot (cr12) blank and they're dem, use actual ballot
																			 !is.na(cr18) ~ as.character(cr18),
																			 TRUE  ~ NA_character_))%>% #same but for reps
	mutate(xrep_primary_vote = case_when(is.na(cr17) ~ as.character(cr11),
																			 !is.na(cr17) ~ as.character(cr17),
																			 TRUE ~ NA_character_))%>%
	pivot_longer(ckey:yv02,
							 names_to = c("wave", ".value"),
							 names_pattern = "(^.)([^\1]*)")%>%
	glimpse()

mult_reint <- rbind(naes_reint_a,
										naes_reint_b)%>%
	mutate(xdem_primary_vote = recode(xdem_primary_vote,
																		"1" = "Bradley",
																		"2" = "Gore",
																		"3" = "Other",
																		"4" = "Not Voting",
																		"998" = NA_character_,
																		"999" = NA_character_))%>%
	mutate(xrep_primary_vote = recode(xrep_primary_vote,
																		"1" = "Bauer",
																		"2" = "Bush",
																		"3" = "Forbes",
																		"4" = "Hatch",
																		"5" = "Keyes",
																		"6" = "McCain",
																		"7" = "Other",
																		"8" = "Not Voting",
																		"998" = NA_character_,
																		"999" = NA_character_))%>%
	mutate(primary_vote = as.factor(case_when(!is.na(xdem_primary_vote) ~ xdem_primary_vote,
																						!is.na(xrep_primary_vote) ~ xrep_primary_vote,
																						TRUE ~ NA_character_)))%>%
	mutate(loser_first = as.factor(case_when(primary_vote == "Bush" | primary_vote == "Gore" ~ "winner",
																					 primary_vote != "Bush" & primary_vote != "Gore" ~ "loser",
																					 TRUE ~ NA_character_)))%>%
	glimpse()






naes_2000_tidy <- rio::import("data/raw/naes-2000-all.rds")%>%
	select(contains("key"),
				 contains("date"),
				 contains("r11"), #intend to vote for, rep
				 contains("r12"), #intend to vote, dem
				 contains("r17"), #actual vote for rep
				 contains("r18"), #actual vote for dem
				 contains("v01"), #pid_str
				 contains("v02"), #pid3
				 -ends_with("$"))%>%
	glimpse()%>%
	mutate(xdem_primary_vote = case_when(is.na(cr18) ~ as.character(cr12), #when intended ballot (cr12) blank and they're dem, use actual ballot
																			 !is.na(cr18) ~ as.character(cr18),
																			 TRUE  ~ NA_character_))%>% #same but for reps
	mutate(xrep_primary_vote = case_when(is.na(cr17) ~ as.character(cr11),
																			 !is.na(cr17) ~ as.character(cr17),
																			 TRUE ~ NA_character_))%>%
	mutate(xdem_primary_vote = recode(xdem_primary_vote,
																	 "1" = "Bradley",
																	 "2" = "Gore",
																	 "3" = "Other",
																	 "4" = "Not Voting",
																	 "998" = NA_character_,
																	 "999" = NA_character_))%>%
	mutate(xrep_primary_vote = recode(xrep_primary_vote,
																					"1" = "Bauer",
																					"2" = "Bush",
																					"3" = "Forbes",
																					"4" = "Hatch",
																					"5" = "Keyes",
																					"6" = "McCain",
																					"7" = "Other",
																					"8" = "Not Voting",
																					"998" = NA_character_,
																					"999" = NA_character_))%>%
	mutate(primary_vote = as.factor(case_when(!is.na(xdem_primary_vote) ~ xdem_primary_vote,
																						!is.na(xrep_primary_vote) ~ xrep_primary_vote,
																						TRUE ~ NA_character_)))%>%
	mutate(loser_first = as.factor(case_when(primary_vote == "Bush" | primary_vote == "Gore" ~ "winner",
																 					 primary_vote != "Bush" & primary_vote != "Gore" ~ "loser",
																					  TRUE ~ NA_character_)))%>%
	pivot_longer(ckey:rv02,
							 names_to = c("wave", ".value"),
							 names_pattern = "(^.)([^\1]*)")%>%
	rbind(mult_reint)%>% #add the reinterviews
	select(-starts_with("x"))%>%
	rename(pid3 = v01)%>%
	rename(pid_str = v02)%>%
	mutate(pid3 = factor(recode(pid3,
											 "1" = "Republican",
											 "2" = "Democrat",
											 "3" = "Independent",
											 "4" = "Other",
											 "998" = "Don't Know",
											 "999" = NA_character_),
											 levels = c("1" = "Democrat",
											 					 "2" = "Independent",
											 					 "3" = "Republican",
											 					 "4" = "Other",
											 					 "5" = "Don't Know",
											 					 "6" = "No Answer")),
				 pid_str = factor(recode(pid_str,
				 												"1" = "Strong",
				 												"2" = "Not Very Strong",
				 												"998" = "Don't Know",
				 												"999" = "No Answer")),
				 strong_part = if_else(pid_str == "Strong", 1, 0),
				 date = as.Date(as.character(date),format="%Y%m%d"),
				 wave = recode_factor(wave,
				 										 "c" = "Cross Section",
				 										 "r" = "Reinterview"),
				 convention_end = as.Date((case_when(
				 	pid3 == "Democrat" ~ "2000-08-17",
				 	pid3 == "Republican" ~ "2000-08-03",
				 	TRUE ~ NA_character_))),
				 presumptive_date = as.Date((case_when(
				 	pid3 == "Democrat" ~ "2000-03-09",
				 	pid3 == "Republican" ~ "2000-03-07",
				 	TRUE ~ NA_character_))),
				 general_election = as.Date("2000-11-07"),
	)%>%
	glimpse()%>%
	write_rds("data/tidy-naes-2000.rds")%>%
	write_csv("data/tidy-naes-2000.csv")
