library(tidyverse)

naes_df <- read_rds("data/raw/naes-trim.rds")%>%
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
	rename(primaries_good_way = reb01)%>%
	mutate(wave = as.factor(set),
				 pid7 = fct_rev(recode_factor(pid7, #reversing factors here either so that higher numbers = more agreement, or to match ANES coding scheme (eg pid7)
				 										 "7" = "Strong Democrat",
				 										 "6" = "Not strong Democrat",
				 										 "5" = "Leans Democrat",
				 										 "4" = "Independent / other / undecided",
				 										 "3" = "Leans Republican",
				 										 "2" = "Not strong Republican",
				 										 "1" = "Strong Republican",
				 										 "999" = "Skipped")),
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
				 																	"999" = "Skipped"),
				 conf_votes_count = fct_rev(recode_factor(conf_votes_count,
				 																				 "1" = "Very Confident",
				 																				 "2" = "Somewhat Confident",
				 																				 "3" = "Not Too Confident",
				 																				 "4" = "Not at all Confident",
				 																				 "999" = "Skipped")),
				 primaries_good_bad = fct_rev(recode_factor(primaries_good_way,
				 																					 "1" = "A good way of picking the best candidates",
				 																					 "2" = "A bad way of picking the best candidates",
				 																					 "999" = "Skipped")))%>%
	select(-set)%>%
	glimpse()

test_df <- read_rds("data/raw/naes-trim.rds")%>%
	pivot_longer(wave_1:reb01_3, 
							 names_to = c(".value", "set"), 
							 names_sep="_")%>%
	mutate(testdummy = if_else(rba04 == 9, 1, 0))%>%
	mutate(testdummy1 = if_else(rba03 == 99, 1, 0))%>%
	summarise(td = sum(testdummy, na.rm = TRUE),
						td1 = sum(testdummy1, na.rm = TRUE))%>%
	glimpse()
# contains("wave"), #wave--not sure how coded
# contains("date"), #date
# contains("ma01"), #PID7 most recent
# contains("mb01"), #elections make gov pay attention 1-3 "a good deal" -- "not much"
# contains("mb02"), # Parties make government pay attention to people 1-3 "a good deal" -- "not much"
# contains("mb05"), #officials not interested in avg person 1--5 strongly agree to strongly disagree
# contains("mb06"), # someone like me can't influence government 1--5 strongly agree to strongly disagree
# contains("rba01"), #rep primary choice 1
# contains("rba02"), #rep primary choice 2
# contains("rba03"), #dem primary choice 1
# contains("rba04"), #dem primary choice 2
# contains("rea01"), #confident votes counted
# contains("reb01") #primary good or bad
