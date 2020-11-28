library(tidyverse)


online_08_trim <- rio::import("data/raw/naes/2008/dta08.zip", which = "naes08_online_all_waves_data_full.dta")%>%
	select(
				 rkey,
				 contains("wave"), #wave--not sure how coded
				 contains("date"), #date
				 contains("ma01"), #PID7 most recent
				 contains("mb01"), #elections make gov pay attention 1-3 "a good deal" -- "not much"
				 contains("mb02"), # Parties make government pay attention to people 1-3 "a good deal" -- "not much"
				 contains("mb05"), #officials not interested in avg person 1--5 strongly agree to strongly disagree
				 contains("mb06"), # someone like me can't influence government 1--5 strongly agree to strongly disagree
				 contains("rba01"), #rep primary choice 1
				 contains("rba02"), #rep primary choice 2
				 contains("rba03"), #dem primary choice 1
				 contains("rba04"), #dem primary choice 2
				 contains("rea01"), #confident votes counted
				 contains("reb01"), #primary good or bad
				 contains("rCb05"), #For whom did you vote in the general? 1 mccain, 2 obama, 3 nader, 4, bob barr, 5 other, 999 skipped
				 contains("rCb04"), # did you vote in genearl? 1 no, 2 no, 3 yes, 4 yes, 999 skipped
				 contains("rcc01"), # did you vote for MC? 1 - Republican, 2 - Democrat, 3 other, 4 didn't vote, 999 - skipped, 9999- not asked
				 contains("rcc02"), #same coding as rcc01 but for senate
				 contains("rcc04"), #same as rcc01/2 but for governor
				 )%>%
	select(-rba04r9v_2)%>%
#	rename(rkey = respondent,
#				 ma01 = pid_7,
#				 ma02 = pid_3)%>%
	glimpse()%>%
	write_rds("data/raw/naes-trim-online.rds")%>%
	write_csv("data/raw/naes-trim-online.csv")


phone_08_trim <- rio::import("data/raw/naes/2008/dta08.zip", which = "naes08_phone_nat_rcs_reint_data_full.dta")%>%
	select(
		rkey, #wave--not sure how coded
		contains("date"), #date
		contains("ma01"), #PID7 most recent
		contains("mb01"), #elections make gov pay attention 1-3 "a good deal" -- "not much"
		contains("mb02"), # Parties make government pay attention to people 1-3 "a good deal" -- "not much"
		contains("mb05"), #officials not interested in avg person 1--5 strongly agree to strongly disagree
		contains("mb06"), # someone like me can't influence government 1--5 strongly agree to strongly disagree
		contains("rba01"), #rep primary choice 1
		contains("rba02"), #rep primary choice 2
		contains("rba03"), #dem primary choice 1
		contains("rba04"), #dem primary choice 2
		contains("rea01"), #confident votes counted
		contains("reb01") #primary good or bad
	)%>%
#	select(-ends_with(c("v_c", "v_r")))%>%
	#	rename(rkey = respondent,
	#				 ma01 = pid_7,
	#				 ma02 = pid_3)%>%
	glimpse()%>%
	write_rds("data/raw/naes-trim-phone.rds")%>%
	write_csv("data/raw/naes-trim-phone.csv")

