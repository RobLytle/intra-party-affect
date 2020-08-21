library(tidyverse)

anes_2020_pilot <- rio::import("data/raw/anes_pilot_2020ets_dta.zip", which = "anes_pilot_2020ets_dta.dta")%>%
	select(ftdemocraticparty,
				 ftrepublicanparty,
				 primaryvote,
				 pid7,
	)%>%
	rename(therm_dem = ftdemocraticparty,
				 therm_rep = ftrepublicanparty,
				 pid_7 = pid7)%>%
	mutate(pid_7_num = as.numeric(fct_rev(as.factor(pid_7))),  #reording so it matches other datasets
				 pid_7 = recode(pid_7,
												"1" = "Strong Republican",
												"2" = "Weak Republican",
												"3" = "Lean Republican",
												"4" = "Independent",
												"5" = "Lean Democrat",
												"6" = "Weak Democrat",
												"7" = "Strong Democrat",
												"9" = NA_character_),
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

