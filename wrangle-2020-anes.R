library(tidyverse)
library(haven)

anes_2020_df <- read_dta("data/raw/anes_timeseries_2020_stata_20210211.dta")%>%
	sjlabelled::remove_all_labels()%>%
	select(therm_dem = V201156,
				 therm_rep = V201157,
				 primary_vote = V201020,
				 primary_vote_choice = V201021,
				 pid_7 = V201231x,
	)%>%
	mutate(pid_7_num = as.numeric(fct_rev(as.factor(pid_7))),  #reording so it matches other datasets
				 pid_7 = recode(pid_7,
				 							 "7" = "Strong Republican",
				 							 "6" = "Weak Republican",
				 							 "5" = "Lean Republican",
				 							 "4" = "Independent",
				 							 "3" = "Lean Democrat",
				 							 "2" = "Weak Democrat",
				 							 "1" = "Strong Democrat",
				 							 "-9" = NA_character_),
				 pid_7 = as.factor(reorder(pid_7, pid_7_num)))%>%
	mutate(pid_3 = as.factor(case_when(str_detect(pid_7, "Democrat") ~ "Democrat",
																		 str_detect(pid_7, "Republican") ~ "Republican",
																		 str_detect(pid_7, "Independent") ~ "Independent",
																		 TRUE ~ NA_character_)))%>%
	mutate(therm_inparty = case_when(pid_3 == "Democrat" ~ therm_dem,
																	 pid_3 == "Republican" ~ therm_rep,
																	 TRUE ~ NA_real_))%>%
	mutate(therm_outparty = case_when(pid_3 == "Democrat" ~ therm_rep,
																		pid_3 == "Republican" ~ therm_dem,
																		TRUE ~ NA_real_))%>%
	mutate(therm_dem = na_if(therm_dem, 999),
				 therm_rep = na_if(therm_rep, 999),
				 therm_parties_mean = ((therm_dem + therm_rep)/2),
				 year = 2020,
				 weight = 1)%>%
	write_rds("data/tidy-anes-2020.rds")%>%
	glimpse()
