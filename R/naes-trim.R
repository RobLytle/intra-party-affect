library(tidyverse)


online_08_trim <- rio::import("data/raw/naes/2008/dta08.zip", which = "naes08_online_all_waves_data_full.dta")%>%
	select(rkey,
				 contains("wave"), #wave--not sure how coded
				 contains("date"), #date
				 contains("ma01"), #PID7 most recent
				 contains("mb01"), #elections make gov pay attention 1-3 "a good deal" -- "not much"
				 contains("mb02"), # Parties make government pay attention to people 1-3 "a good deal" -- "not much"
				 contains("mb05"), #officeials not interested in avg person 1--5 strongly agree to strongly disagree
				 contains("mb06"), # someone like me can't influence government 1--5 strongly agree to strongly disagree
				 contains("rba01"), #rep primary choice 1
				 contains("rba02"), #rep primary choice 2
				 contains("rba03"), #dem primary choice 1
				 contains("rba04"), #dem primary choice 2
				 contains("rea01"), #confident votes counted
				 contains("reb01") #primary good or bad
				 
				 
				 )%>%
	select(-rba04r9v_2)%>%
#	rename(rkey = respondent,
#				 ma01 = pid_7,
#				 ma02 = pid_3)%>%
	glimpse()%>%
	write_rds("data/raw/naes-trim.rds")%>%
	write_csv("data/raw/naes-trim.csv")