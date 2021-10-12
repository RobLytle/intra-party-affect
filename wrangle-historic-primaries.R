library(tidyverse)
library(anesr)

data(timeseries_1980) #these import the datasets from anesr
data(timeseries_1988)
data(timeseries_1992)
data(timeseries_cum)

df_cdf <- timeseries_cum
df_80<-timeseries_1980
df_88<-timeseries_1988
df_92<-timeseries_1992



df_tid_80 <- df_80 %>%
	select(case = V800004,
				 pid_3 = V800775,
				 prim_vote_dum = V800371,
				 prim_vote_party = V800372,
				 prim_vote_cand = V800373) %>%
	sjlabelled::remove_all_labels() %>% 
	mutate(case = paste("1980", case, sep = "_"),
				 prim_vote_dum = dplyr::recode(prim_vote_dum, .default = NA_character_,
				 											 "1" = "Voted",
				 											 "5" = "Didn't Vote"),
				 prim_vote_party = dplyr::recode(prim_vote_party, .default = NA_character_,
				 											 "1" = "Republican",
				 											 "5" = "Democrat"),
				 pid_3 = case_when(pid_3 <= 2 ~ "Democrat",
				 									pid_3 == 3 ~ "Independent",
				 									pid_3 <= 4 & pid_3 < 7 ~ "Republican",
				 									TRUE ~ NA_character_),
				 prim_vote_simple = case_when(prim_vote_dum == "Didn't Vote" ~ "Didn't Vote",
				 														 prim_vote_party != pid_3 ~ "Voted in Other Primary",
				 														 prim_vote_cand == 10 | prim_vote_cand == 50 ~ "Winner",
				 														 prim_vote_cand != 10 | prim_vote_cand != 50 ~ "Loser",
				 														 TRUE ~ NA_character_)) %>%
	select(case,
				 prim_vote_simple) %>% 
	glimpse()


#1992 
# Primary Votes: VAR 923301-923304
# Therms: D-923317
#					R-923318
# Weight: 927000

df_tid_92 <- df_92 %>% 
	select(case = V900004,
				 pid_3 = V900320,
				 prim_vote_dum = V923301,
				 prim_vote_party = V923302,
				 prim_vote_cand_r = V923303,
				 prim_vote_cand_d = V923304) %>%
	sjlabelled::remove_all_labels() %>% 
	mutate(case = paste("1992", case, sep = "_"),
				 prim_vote_dum = dplyr::recode(prim_vote_dum, .default = NA_character_,
				 											 "1" = "Voted",
				 											 "5" = "Didn't Vote"),
				 prim_vote_party = dplyr::recode(prim_vote_party, .default = NA_character_,
				 												 "1" = "Republican",
				 												 "5" = "Democrat"),
				 pid_3 = case_when(pid_3 <= 2 ~ "Democrat",
				 									pid_3 == 3 ~ "Independent",
				 									pid_3 <= 4 & pid_3 < 6 ~ "Republican", #doesn't code 3rd parties
				 									TRUE ~ NA_character_),
				 prim_vote_simple = case_when(prim_vote_dum == "Didn't Vote" ~ "Didn't Vote",
				 														 prim_vote_party != pid_3 ~ "Voted in Other Primary",
				 														 prim_vote_cand_r == 01 | prim_vote_cand_d == 01 ~ "Winner",
				 														 prim_vote_cand_r != 01 | prim_vote_cand_d != 01 ~ "Loser",
				 														 TRUE ~ NA_character_)) %>%
	select(colnames(df_tid_80)) %>% 
	glimpse()


#1988
# Primary Votes: 880148-880151
# Therms: D-880164
#					R-880165
#	Weight: none
#	Race: 880412
# Income: 880520
#	Ideology: S-880231
#						D-880235
#						R-880234
#	Sex: 880413
#	Age: 880417
# Education: 880422


df_tid_88 <- df_88 %>% 
	select(case = V880004,
				 pid_3 = V880274,
				 prim_vote_dum = V880148,
				 prim_vote_party = V880149,
				 prim_vote_cand_r = V880150,
				 prim_vote_cand_d = V880151) %>%
	sjlabelled::remove_all_labels() %>% 
	mutate(case = paste("1988", case, sep = "_"),
				 prim_vote_dum = dplyr::recode(prim_vote_dum, .default = NA_character_,
				 											 "1" = "Voted",
				 											 "5" = "Didn't Vote"),
				 prim_vote_party = dplyr::recode(prim_vote_party, .default = NA_character_,
				 												 "1" = "Republican",
				 												 "5" = "Democrat"),
				 pid_3 = case_when(pid_3 <= 2 ~ "Democrat",
				 									pid_3 == 3 ~ "Independent",
				 									pid_3 <= 4 & pid_3 < 6 ~ "Republican", #doesn't code 3rd parties
				 									TRUE ~ NA_character_),
				 prim_vote_simple = case_when(prim_vote_dum == "Didn't Vote" ~ "Didn't Vote",
				 														 prim_vote_party != pid_3 ~ "Voted in Other Primary",
				 														 prim_vote_cand_r == 01 | prim_vote_cand_d == 10 ~ "Winner",
				 														 prim_vote_cand_r != 01 | prim_vote_cand_d != 10 ~ "Loser",
				 														 TRUE ~ NA_character_)) %>%
	select(colnames(df_tid_80)) %>% 
	glimpse()







tidy_cdf <- read_rds("data/tidy-cdf.rds") %>%
	filter(year >= 1980) %>%
	select(pid_3,
				 case,
				 year,
				 therm_inparty,
				 therm_outparty,
				 income_num,
				 ideo_self = respondent_ideo_num,
				 ideo_self_in_dif,
				 ideo_inparty,
				 weight) %>% 
	#	select(case) %>%
	glimpse()



df_2020 <- read_csv("data/tidy-primaries.csv") %>%
	filter(year == 2020) %>% 
	select(case,
				 prim_vote_simple = primary_vote_simple,
				 year,
				 therm_inparty,
				 therm_outparty,
				 income_num = income,
				 ideo_self,
				 pid_3,
				 weight,
				 ideo_self_in_dif,
				 ideo_inparty) %>% 
	glimpse()

# Joining all the primaries into one
new_prims <- read_csv("data/tidy-primaries.rds") %>% #this file is made in wrangle-primaries.R. I will probably put everythingg into one file.
	filter(year != "2020") %>% 
	select(prim_vote_simple = primary_vote_simple,
				 case) %>%
	glimpse()

all_primaries_df<-rbind(df_tid_80,
											df_tid_88,
											df_tid_92,
											new_prims) %>%
	left_join(tidy_cdf) %>%
	rbind(df_2020) %>% 
	mutate(prim_vote_simple = factor(prim_vote_simple,
																	 levels = c("Winner", "Didn't Vote", "Loser", "Voted in Other Primary")),
				 therm_inparty = as.integer(therm_inparty),
				 therm_outparty = as.integer(therm_outparty),
				 ideo_self_std = (ideo_self-4)/3,
				 ideo_self_prt_std = zero1(case_when(pid_3 == "Democrat" ~ ideo_self_std*-1, # recodes so that high numbers are party-specific extreme 1 = ext. lib for dems, 1 = ext con for reps
				 																		pid_3 == "Republican" ~ ideo_self_std,
				 																		TRUE ~ NA_real_)),
				 ideo_inparty_std = (ideo_inparty-4)/3,
				 ideo_inparty_prt_std = zero1(case_when(pid_3 == "Democrat" ~ ideo_inparty_std*-1, # recodes so that high numbers are party-specific extreme 1 = ext. lib for dems, 1 = ext con for reps
				 																			 pid_3 == "Republican" ~ ideo_inparty_std,
				 																			 TRUE ~ NA_real_)),
				 ideo_self_in_dif_std = ideo_self_prt_std - ideo_inparty_prt_std,) %>%
	filter(pid_3 != "Independent") %>% 
	write_rds("data/tidy-primaries-80-20.rds") %>% 
	glimpse()

#Going to wrangle all remaining years with primary info

# CDF Case-id number: VCF0006




#1980
# Primary Votes: 800371-800373
# Therms: D-800168
#					R-800169

