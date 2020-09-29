library(tidyverse)

naes_2008_online_df <- read_rds("data/raw/naes-trim.rds")%>%
	mutate(dem_loser_wave_1 = as.character(if_else(rba03_1 != 7, "Loser", "Winner")),
				 rep_loser_wave_1 = as.character(if_else(rba01_1 != 5, "Loser", "Winner")))%>%
	# select(ma01_1,
	# 			 rba03_1,
	# 			 rba03_2,
	# 			 rba01_1,
	# 			 rba01_2)%>%
	glimpse()%>%
	mutate(winner_loser = as.factor(case_when(
		rba03_1 == 7 ~ "Obama", #creates a variable that includes
		rba03_2 == 7 ~ "Obama",
		rba03_1 == 2 ~ "Clinton",
		rba03_2 == 2 ~ "Clinton",
		rba01_1 == 5 ~ "McCain",
		rba01_2 == 5 ~ "McCain",
		!is.na(rba03_1) & rba03_1 != 7 ~ "Other Democrat",
		!is.na(rba03_2) & rba03_1 != 7~ "Other Democrat",
		!is.na(rba01_1) & rba01_1 != 5 ~ "Other Republican",
		!is.na(rba01_2) & rba01_2 != 5 ~ "Other Republican",
		rba03_1 == 999 ~ "skipped",
		rba03_2 == 999 ~ "skipped",
		rba01_1 == 999 ~ "skipped",
		rba01_2 == 999 ~ "skipped",
		TRUE ~ NA_character_)))%>%
	mutate(winner_loser_na = as.factor(case_when(
		ma01_1 > 4 & is.na(rba03_1) & rba03_2 == 7 ~ "Obama",
		ma01_1 > 4 & rba03_1 == 7 ~ "Obama", #creates a variable that includes
		ma01_1 < 4 & is.na(rba01_1) & rba01_2 == 5 ~ "McCain",
		ma01_1 < 4 & rba01_1 == 5 ~ "McCain",
		ma01_1 > 4 & is.na(rba03_1) & rba03_2 == 999 ~ "Skipped",
		ma01_1 > 4 & rba03_1 == 999 ~ "Skipped",
		ma01_1 < 4 & is.na(rba01_1) & rba01_2 == 999 ~ "Skipped",
		ma01_1 < 4 & rba01_1 == 999 ~ "Skipped",
		ma01_1 > 4 & is.na(rba03_1) & rba03_2 != 7 ~ "Other Candidate",
		ma01_1 > 4 & rba03_1 != 7 ~ "Other Candidate",
		ma01_1 < 4 & is.na(rba01_1) & rba01_2 != 5 ~ "Other Candidate",
		ma01_1 < 4 & rba01_1 != 5 ~ "Other Candidate",
		TRUE ~ NA_character_)))%>%
	mutate(winner_loser_rep = as.factor(case_when(
		ma01_1 > 4 & ma01_2 < 4 ~ NA_character_,
		ma01_1 < 4 & is.na(rba01_1) & rba01_2 == 5 ~ "McCain",
		ma01_1 < 4 & rba01_1 == 5 ~ "McCain",
		ma01_1 < 4 & is.na(rba01_1) & rba01_2 == 999 ~ "Skipped",
		ma01_1 < 4 & rba01_1 == 999 ~ "Skipped",
		ma01_1 < 4 & is.na(rba01_1) & rba01_2 != 5 ~ "Other Candidate",
		ma01_1 < 4 & rba01_1 != 5 ~ "Other Candidate",
		TRUE ~ NA_character_)))%>%
	mutate(winner_loser_dem = as.factor(case_when(
		ma01_1 < 4 & ma01_2 > 4 ~ NA_character_,
		ma01_1 > 4 & is.na(rba03_1) & rba03_2 == 7 ~ "Obama",
		ma01_1 > 4 & rba03_1 == 7 ~ "Obama", #creates a variable that includes
		ma01_1 > 4 & is.na(rba03_1) & rba03_2 == 999 ~ "Skipped",
		ma01_1 > 4 & rba03_1 == 999 ~ "Skipped",
		ma01_1 > 4 & is.na(rba03_1) & rba03_2 != 7 ~ "Other Candidate",
		ma01_1 > 4 & rba03_1 != 7 ~ "Other Candidate",
		TRUE ~ NA_character_)))%>%
	mutate(winner_loser_party = as.factor(case_when(
		!is.na(winner_loser_dem) ~ as.character(winner_loser_dem),
		!is.na(winner_loser_rep) ~ as.character(winner_loser_rep),
		TRUE ~ NA_character_)
	))%>%
	pivot_longer(wave_1:reb01_3, 
							 names_to = c(".value", "set"), 
							 names_sep="_")%>%
	rename(pid7 = ma01)%>%
	rename(elect_att = mb01)%>%
	rename(parties_att = mb02)%>%
	rename(offs_not_int = mb05)%>%
	rename(no_gov_infl = mb06)%>%
	rename(repubs_first_choice = rba01)%>%
	rename(repubs_sec_choice = rba02)%>%
	rename(dems_first_choice = rba03)%>%
	rename(dems_sec_choice = rba04)%>%
	rename(conf_votes_count = rea01)%>%
	rename(primaries_good_bad = reb01)%>%
	mutate(wave = as.factor(set),
				 date = as.Date(as.character(date),format="%Y%m%d"),
				 pid7 = fct_rev(recode_factor(pid7, #reversing factors here either so that higher numbers = more agreement, or to match ANES coding scheme (eg pid7)
				 														 "7" = "Strong Democrat",
				 														 "6" = "Not strong Democrat",
				 														 "5" = "Leans Democrat",
				 														 "4" = "Independent / other / undecided",
				 														 "3" = "Leans Republican",
				 														 "2" = "Not strong Republican",
				 														 "1" = "Strong Republican",
				 														 "999" = "Skipped")),
				 pid3 = as.factor(case_when(str_detect(as.character(pid7), "Democrat") ~ "Democrat",
				 													 str_detect(as.character(pid7), "Republican") ~ "Republican",
				 													 TRUE ~ as.character(pid7))),
				 elect_att = fct_rev(recode_factor(elect_att,
				 																	"1" = "A good deal",
				 																	"2" = "Some",
				 																	"3" = "Not much",
				 																	"999" = "Skipped")),
				 parties_att = fct_rev(recode_factor(parties_att,
				 																		"1" = "A good deal",
				 																		"2" = "Some",
				 																		"3" = "Not much",
				 																		"999" = "Skipped")),
				 offs_not_int = fct_rev(recode_factor(offs_not_int,
				 																		 "1" = "Strongly Agree",
				 																		 "2" = "Agree",
				 																		 "3" = "Neither Agree Nor Disagree",
				 																		 "4" = "Disagree",
				 																		 "5" = "Strongly Disagree",
				 																		 "999" = "Skipped")),
				 no_gov_infl = na_if(no_gov_infl, 9999), #9999 is missing data
				 no_gov_infl = fct_rev(recode_factor(no_gov_infl,
				 																		"1" = "Strongly Agree",
				 																		"2" = "Agree",
				 																		"3" = "Neither Agree Nor Disagree",
				 																		"4" = "Disagree",
				 																		"5" = "Strongly Disagree",
				 																		"999" = "Skipped")),
				 repubs_first_choice  = recode_factor(repubs_first_choice,
				 																		 "1" = "Sam Brownback",
				 																		 "2" = "Rudy Giuliani",
				 																		 "3" = "Mike Huckabee",
				 																		 "4" = "Duncan Hunter",
				 																		 "5" = "John McCain",
				 																		 "6" = "Ron Paul",
				 																		 "7" = "Mitt Romney",
				 																		 "8" = "Tom Tancredo",
				 																		 "9" = "Fred Thompson",
				 																		 "999" = "Skipped"),
				 repubs_sec_choice  = recode_factor(repubs_sec_choice,
				 																	 "1" = "Sam Brownback",
				 																	 "2" = "Rudy Giuliani",
				 																	 "3" = "Mike Huckabee",
				 																	 "4" = "Duncan Hunter",
				 																	 "5" = "John McCain",
				 																	 "6" = "Ron Paul",
				 																	 "7" = "Mitt Romney",
				 																	 "8" = "Tom Tancredo",
				 																	 "9" = "Fred Thompson",
				 																	 "999" = "Skipped"),
				 dems_first_choice = recode_factor(dems_first_choice,
				 																	"1" = "Joe Biden",
				 																	"2" = "Hillary Clinton",
				 																	"3" = "Chris Dodd",
				 																	"4" = "John Edwards",
				 																	"5" = "Mike Gravel",
				 																	"6" = "Dennis Kucinich",
				 																	"7" = "Barack Obama",
				 																	"8" = "Bill Richardson",
				 																	"999" = "Skipped"),
				 dems_sec_choice = recode_factor(dems_sec_choice,
				 																"1" = "Joe Biden",
				 																"2" = "Hillary Clinton",
				 																"3" = "Chris Dodd",
				 																"4" = "John Edwards",
				 																"5" = "Mike Gravel",
				 																"6" = "Dennis Kucinich",
				 																"7" = "Barack Obama",
				 																"8" = "Bill Richardson",
				 																"9" = "Other Candidtate",
				 																"999" = "Skipped"),
				 first_choice = as.factor(case_when(!is.na(repubs_first_choice) ~ as.character(repubs_first_choice), #combining partisan vote choices
				 																	 !is.na(dems_first_choice) ~ as.character(dems_first_choice),
				 																	 TRUE ~ NA_character_)),
				 sec_choice = as.factor(case_when(!is.na(repubs_sec_choice) ~ as.character(repubs_sec_choice),
				 																 !is.na(dems_sec_choice) ~ as.character(dems_sec_choice),
				 																 TRUE ~ NA_character_)),
				 conf_votes_count = fct_rev(recode_factor(conf_votes_count,
				 																				 "1" = "Very Confident",
				 																				 "2" = "Somewhat Confident",
				 																				 "3" = "Not Too Confident",
				 																				 "4" = "Not at all Confident",
				 																				 "999" = "Skipped")),
				 primaries_good_bad = fct_rev(recode_factor(primaries_good_bad,
				 																					 "1" = "A good way of picking the best candidates",
				 																					 "2" = "A bad way of picking the best candidates",
				 																					 "999" = "Skipped")),
				 loser_first = as.factor(case_when(!is.na(rep_loser_wave_1) ~ rep_loser_wave_1, #combining partisan vote choices
				 																	!is.na(dem_loser_wave_1) ~ dem_loser_wave_1,
				 																	TRUE ~ NA_character_)),
				 strong_part = if_else(str_detect(pid7, "Strong"), 1, 0),
				 convention_dummy = case_when(pid3 == "Democrat" & date < 20080825 ~ "Pre Convention", #only appropriate for partisans, all independents
				 														 pid3 == "Republican" & date < 20080901 ~ "Pre Convention",
				 														 pid3 != "Democrat" & pid3 != "Republican" ~ NA_character_,
				 														 TRUE ~ "Post Convention"),
				 pid_str = factor(case_when(str_detect(pid7, "Strong") ~ "Strong",
				 													 str_detect(pid7, "Not") ~ "Weak",
				 													 str_detect(pid7, "Leans") ~ "Leaning",
				 													 TRUE ~ NA_character_),
				 								 levels = c("Leaning", 
				 								 					 "Weak", 
				 								 					 "Strong")),
				 convention_end = as.Date((case_when(
				 	pid3 == "Democrat" ~ "2008-08-28",
				 	pid3 == "Republican" ~ "2008-09-04",
				 	TRUE ~ NA_character_))),
				 presumptive_date = as.Date((case_when(
				 	pid3 == "Democrat" ~ "2008-06-03",
				 	pid3 == "Republican" ~ "2008-03-04",
				 	TRUE ~ NA_character_))),
				 general_election = as.Date("2008-11-04"),
	)%>%
	mutate(loser_first = na_if(loser_first, -9))%>%
	select(-set,
				 -dem_loser_wave_1,
				 -rep_loser_wave_1)%>%
	glimpse()%>%
	write_rds("data/tidy-naes-08-online.rds")%>%
	write_csv("data/tidy-naes-08-online.csv")
