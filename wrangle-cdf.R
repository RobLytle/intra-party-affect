---
title: "wrangle-cdf.R"
author: "Rob Lytle"
date: "9/16/2021"
output: html_document
---

```{r setup, include=FALSE}
library(tidyverse)
library(sjlabelled)

## Functions

zero1 <- function(x, minx = NA, maxx = NA) {
	
	stopifnot(identical(typeof(as.numeric(x)), "double"))
	
	if (typeof(x) == "character") x <- as.numeric(x)
	
	res <- NA
	
	if (is.na(minx)) {
		res <- (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
	}
	
	if (!is.na(minx)) res <- (x - minx) / (maxx - minx)
	
	res
}


```

```{r}
cdf_raw_trim <- rio::import("data/raw/anes/anes_timeseries_cdf_dta.zip", which = "anes_timeseries_cdf.dta") #Imports the .dta file from the .zip file
```

```{r}
cdf_raw_trim %>% 
remove_all_labels()%>%
	select(year = VCF0004, # Year of response
					pid_7 = VCF0301, #7 scale Party ID val: 1-7. Strong Democrat 2. Weak Democrat3. Independent - Democrat4. Independent - Independent5. Independent - Republican6. Weak Republican7. Strong Republican
					pid_3 = VCF0303, # Party ID 3 categories val: "Republican", "Independent", "Democrat" (Dem/Rep include Leaners)
					pid_str = VCF0305, # PID strength val: 1. Independent 2. Leaning Independent 3. Weak Partisan 4. Strong Partisan Kept this because I wanted to create basically this variable later
					win_care_pres = VCF0311, # How much do you care which party wins presidency? val: 1. Don't care very much or DK, pro-con, depends, and other, 2. Care a great deal
					win_care_cong = VCF0312, # How much do you care which party wins congress? val: 1. Don't care very much or DK, pro-con, depends, and other, 2. Care a great deal notes: only asked through 2008
					respondent_ideo = VCF0803, # Liberal-conservative scale val: 1(extremely liberal)- 7(extremely conservative) 9. DK; haven't much thought about it
					therm_dem = VCF0218, # val 00-96 cold-warm as coded; 97: 97-100, 98: DK, 99. NA
					therm_rep = VCF0224, # val 00-96 cold-warm as coded; 97: 97-100, 98: DK, 99. NA
				  therm_lib = VCF0211, #libs FT
				  therm_con = VCF0212, #cons FT
					activist_6cat = VCF0723,#val: 1-6 low-high participation 0. DKN/NA
					ideo_dem = VCF0503, # val: 1-7 lib-con
					ideo_rep = VCF0504,# val: 1-7 lib-con
					general_vote = VCF0702,
					primary_vote = VCF9265,
				 age = VCF0101,
				  case_id = VCF0006,
				  weight = VCF0009z,
				 VCF0806, #insurance Government Health Insurance Scale #1-7 Gov ins- Private ins 9DK, 0NA
				 VCF0809, #jobs Jobs Gurantee, same scale as above
				 VCF0839, # services Gov should provide 1 (few services)--7 (many services) 9DK, 0NA
				 VCF9049, #ss SS: 1 (increase), 2 (same), 3 Decreased, 7 cut entirely 8DK 9NA
				 VCF0834, #women 1(equal) -- 7 (home) 9DK, 0NA
				 VCF0838, #abortion 1 (never), 2(rape, incest, danger), 3(need est.), 4(always) 9DK, 0NA
				 VCF0876a, #gayrights 1(Favor Strongly),2, 4, 5 (Oppose Strongly). 7DK, 9NA
				 VCF0110, #education 1 (grade school), 2(High School), 3(Some College), 4(College/advanced) 0DK/NA
				 VCF0105a, #race 1(white), 2(Black), 3 (asian/pacific), 4(Am. indian/alaska native), 5(hispanic), 6 (other/mult), 7(non-white/non-black), 9NA
				 VCF0113, #south 1(south), 2(nonsouth)
				 VCF0114,
				 VCF0310, #interest 1(not much), 2(somewhat), 3(very) 9dk, 0na
				 VCF0130, #worship 1(every week), 2(almost every week), 3 (once or twice a month), 4 (few times a year), 5(never), 7(no relig.), 890na
				 VCF0050a, # iwrpkpre 1(very high)-5(very low)
				 VCF0050b, # iwrpkpst (same above), take mean
				 VCF9255, #satisfied_democ 1(very), 2(fairly), 3(not very), 4(not at all) -8,-9NA
				 VCF9036, #know_sen 1-2(correct), 3-4(wrong), 7-9NA
				 VCF0104, #gender 1 male, 2 female, 3 other (2016 only).
				 VCF0128, # Regligions preference. 1 protestant, 2 catholic, 3 jewish, 4 other/none/dk, 0 na
				 VCF0604, # Trust in Gov to do what's right 1, never, 2, some of time, 3 most of time, 4 almost always, 9, dk
				 VCF0605, # Gov run for a few interests (1) or benefit of all (2), 9 DK, 0 NA
				 VCF0609, # Officials care what people like R think 1 agree, 2 disagree, 3 neither 9/0 dk/na
				 VCF9222, # are things in this country on the right track (1), or have things gone off on wrong track (2), -8/-9 DK/NA
				 VCF9227, # (1) Larger (2) same, (3) smaller than 20 years ago
				 #Behavior items
				 VCF0705,
				 VCF0708,
				 VCF0716, # Straight (2), Split (1) 0dk/na ticket voting
				 VCF0717, #R influence voters? 1 no (1) 2 yes (2) 0na0 ---Same coding
				 VCF0718, # attend political meetings/rallies during campaign?
				 VCF0719, # Work for party or candidate during campaign?
				 VCF0720, # Display candidate Button/sticker
				 VCF0721, # Donate money to party or Candidate?
				 VCF0724, # watch tb programs about campaigns
				 VCF0729, # Correctly identifies party with house majority in washington pre election
				 VCF0730, # correctly identifies house majority after election
				 VCF0731, # Do you ever discuss politics with Family and Friends? 1 yes, 5 no 8dk # CODED WRONG, has values 1, 5, 6, 7
				 VCF0733, # How often do you discuss politics 0 - never, 1-7 days 9dkna
				 VCF0736, # Party voted for for house 1 Dem, 5, Rep, 7 other
				 VCF0748, # On or before election day? 1 on , 2 before 9na
	)%>%
  unite("case", year:case_id, remove = FALSE)%>%
  select(-case_id)%>%
  mutate(case = as.numeric(str_remove(case, "_")),
  			 pres_election = if_else(year %in% seq(1964, 2016, by=4), 1, 0))%>% #dummy variable for pres election
	rename(female = VCF0104)%>%
  mutate(female = na_if(female, 3))%>%
  mutate(female = na_if(female, 0))%>%
	mutate(female = as.numeric(recode(female, 
																		"1" = "0",
																		"2" = "1")))%>%
	rename(insurance = VCF0806)%>%
	mutate(insurance = na_if(insurance, 9))%>%
	mutate(insurance = na_if(insurance, 0))%>%
	mutate(insurance = zero1(insurance))%>%#standardize between 0-1
	rename(jobs = VCF0809)%>%
	mutate(jobs = na_if(jobs, 9))%>%
	mutate(jobs = na_if(jobs, 0))%>%
	mutate(jobs = zero1(jobs))%>%
	rename(services = VCF0839)%>%
	mutate(services = na_if(services, 9))%>%
	mutate(services = na_if(services, 0))%>%
	mutate(services = as.numeric(recode(services, #recoding so that liberal values are lower, in accordance with everything else..
																			"1" = "7",
																			"2" = "6",
																			"3" = "5",
																			"4" = "4",
																			"5" = "3",
																			"6" = "2",
																			"7" = "1"
	)))%>%
	mutate(services = zero1(services))%>%
	rename(ss = VCF9049)%>%
	mutate(ss = na_if(ss, 8))%>%
	mutate(ss = na_if(ss, 9))%>%
	mutate(no_ss = if_else(ss == 7, 1, 0))%>%
	mutate(ss = na_if(ss, 7))%>%
	mutate(ss = zero1(ss))%>%
	rename(women = VCF0834)%>%
	mutate(women = na_if(women, 9))%>%
	mutate(women = na_if(women, 0))%>%
	mutate(women = zero1(women))%>%
	rename(abortion = VCF0838)%>%
  mutate(abortion = na_if(abortion, 9))%>%
  mutate(abortion = na_if(abortion, 0))%>%
	mutate(abortion = as.numeric(recode(abortion, #recoding so that liberal values are lower, in accordance with everything else..
																			"1" = "4",
																			"2" = "3",
																			"3" = "2",
																			"4" = "1"
	)))%>%
	mutate(abortion = zero1(abortion))%>%
	rename(gayrights = VCF0876a)%>%
	mutate(gayrights = na_if(gayrights, 7))%>%
	mutate(gayrights = na_if(gayrights, 9))%>%
	mutate(gayrights = zero1(gayrights))%>%
	rename(education = VCF0110)%>%#education 1 (grade school), 2(High School), 3(Some College), 4(College/advanced) 0DK/NA
	mutate(education = na_if(education, 0))%>%
	mutate(high_school = if_else(education == 2, 1, 0))%>% #creating education dummies. grade school is ref. cat.
	mutate(some_college = if_else(education == 3, 1, 0))%>%
	mutate(college_adv = if_else(education == 4, 1, 0))%>%
  rename(race_num = VCF0105a)%>%
  mutate(race_num = na_if(race_num, 9))%>%
  mutate(race = as.factor(recode(race_num, 
                                 "1" = "White",
                                 "2" = "Black",
                                 "3" = "Asian or Pacific Islander",
                                 "4" = "American Indian",
                                 "5" = "Hispanic",
                                 "6" = "Other",
                                 "7" = "Non-white and Non-black")))%>%
  mutate(black_white_flag = if_else(race=="Black", "Black", if_else(race=="White", "White", "other")))%>%
	mutate(race_4cat = recode_factor(race, .default = levels(race), 
																	 "Asian or Pacific Islander" = "Other",
																	 "American Indian" = "Other",
																	 "Non-white and Non-black" = "Other"))%>%
  mutate(black_white_flag = recode(race_num,
                                   .default = "other",
                                  "1" = "White",
                                  "2" = "Black"))%>%
  mutate(black_white_flag = na_if(black_white_flag, "other"))%>%
	mutate(white = if_else(race=="White", 1, 0))%>%
  mutate(black = if_else(race=="Black", 1, 0))%>%
  rename(religion_num = VCF0128)%>%
  mutate(religion_num = na_if(religion_num, 0))%>%
  mutate(religion = as.factor(recode(religion_num,
                              "1" = "Protestant",
                              "2" = "Catholic",
                              "3" = "Jewish",
                              "4" = "Other"
                              )))%>%
  mutate(cath_prot_flag = as.factor(recode(religion_num,
                                 .default = "other",
                                 "1" = "Protestant",
                                 "2" = "Catholic")))%>%
  mutate(cath_prot_flag = na_if(cath_prot_flag, "other"))%>%
	rename(south = VCF0113)%>%
	mutate(south = if_else(south==1, 1, 0))%>%
	rename(interest = VCF0310)%>%
	mutate(interest = na_if(interest, 9))%>%
	mutate(interest = na_if(interest, 0))%>%
	rename(worship = VCF0130)%>%
	mutate(worship = if_else(worship == 1 | worship == 2, 1, 0))%>%
	rename(iwrpk_pre = VCF0050a)%>%
	mutate(iwrpk_pre = na_if(iwrpk_pre, 9))%>%
	mutate(iwrpk_pre = as.numeric(recode(iwrpk_pre, #recoding so that liberal values are lower, in accordance with everything else..
																			"1" = "5",
																			"2" = "4",
																			"3" = "3",
																			"4" = "2",
																			"5" = "1"
	)))%>%
	mutate(iwrpk_pre = zero1(iwrpk_pre))%>%
	rename(iwrpk_post = VCF0050b)%>%
	mutate(iwrpk_post = na_if(iwrpk_post, 9))%>%
	mutate(iwrpk_post = na_if(iwrpk_post, 0))%>%
	mutate(iwrpk_post = as.numeric(recode(iwrpk_post, #recoding so that liberal values are lower, in accordance with everything else..
																			 "1" = "5",
																			 "2" = "4",
																			 "3" = "3",
																			 "4" = "2",
																			 "5" = "1"
	)))%>%
	mutate(iwrpk_post = zero1(iwrpk_post))%>%
	mutate(high_knowledge_dum = if_else(iwrpk_post < 3, 1, 0))%>%
#	mutate(iwrpk_mean = (iwrpk_post + iwrpk_pre)/2)%>%
	rename(dis_democ = VCF9255)%>%
	mutate(dis_democ = na_if(dis_democ, -9))%>%
	mutate(dis_democ = as.numeric(recode(dis_democ,
														"3" = "1",
														"4" = "1",
														"1" = "0",
														"2" = "0",
														"-8" = "-8")))%>%
	mutate(dis_democ_qual = recode(dis_democ,
																 "1" = "Not at all/Very Dissatisfied",
																 "0" = "Very/Fairly Satisfied",
																 "-8" = "Don't Know"))%>%
	mutate(dis_democ_dum = na_if(dis_democ, -8))%>%
#	mutate(know_house = na_if(VCF0729, 0))%>%
#	mutate(know_house = if_else(know_house==1, 1, 0))%>%
	rename(know_sen = VCF9036)%>%
	mutate(know_sen = na_if(know_sen, 0))%>%
	mutate(know_sen = if_else(know_sen==1, 1, 0))%>%
#	mutate(know_cong = (know_sen + know_house)/2)%>%
#	rowwise()%>%
#	mutate(cult_att = mean(c(abortion, gayrights, women), na.rm = TRUE))%>%
#	mutate(cult_att = (abortion + gayrights + women)/3)%>%
#	mutate(cult_att = zero1(cult_att))%>%
#	mutate(econ_att = (ss + services + jobs + insurance)/4)%>%
#	mutate(econ_att = zero1(econ_att))%>%
	mutate(win_care_pres = na_if(win_care_pres, 0))%>% #these functions set the specified value to NA, (per the ANES codebook)
	mutate(win_care_cong = na_if(win_care_cong, 0))%>%
	mutate(respondent_ideo = na_if(respondent_ideo, 9))%>%#the recode() function is used in the next 4 pipes to apply new values to observation in the columns. ANES uses numerical values to represent factors.
	mutate(ideo_rep = na_if(ideo_rep, 8))%>%
	mutate(ideo_rep = na_if(ideo_rep, 0))%>%
	mutate(ideo_dem = na_if(ideo_dem, 8))%>%
	mutate(ideo_dem = na_if(ideo_dem, 0))%>%
	mutate(primary_vote_dum = na_if(primary_vote, -8))%>%
	mutate(primary_vote_dum = na_if(primary_vote, -9))%>%
	mutate(primary_vote_dum = as.numeric(recode(primary_vote, 
																		"1" = "1",
																		"2" = "0")))%>%
	mutate(strong_partisan = if_else(pid_7 == 1|pid_7 ==7, 1, 0))%>%
	mutate(pid_7_num = as.numeric(pid_7),
				 pid_7 = recode(pid_7, 
				 							 "1" = "Strong Democrat", 
				 							 "2" = "Weak Democrat", 
				 							 "3" = "Independent - Democrat", 
				 							 "4" = "Independent - Independent", 
				 							 "5" = "Independent - Republican", 
				 							 "6" = "Weak Republican", 
				 							 "7" = "Strong Republican"),
				 pid_7 = reorder(pid_7, pid_7_num))%>%
	mutate(pid_3_num = as.numeric(pid_3),
				 pid_3 = recode(pid_3, 
				 							 "1" = "Democrat", 
				 							 "2" = "Independent", 
				 							 "3" = "Republican"),
				 pid_3 = reorder(pid_3, pid_3_num))%>%
	mutate(pid_str_num = as.numeric(pid_str),
				 pid_str = recode(pid_str, 
				 								 "1" = "Independent", 
				 								 "2" = "Leaning Independent", 
				 								 "3" = "Weak Partisan", 
				 								 "4" = "Strong Partisan"),
				 pid_str = reorder(pid_str, pid_str_num))%>%
	mutate(respondent_ideo_num = as.numeric(respondent_ideo),
				 respondent_ideo = recode(respondent_ideo, 
				 												 "1" = "Extremely Liberal", 
				 												 "2" = "Liberal", 
				 												 "3" = "Somewhat Liberal", 
				 												 "4" = "Moderate", 
				 												 "5" = "Somewhat Conservative", 
				 												 "6" = "Conservative", 
				 												 "7" = "Extremely Conservative"),
				 respondent_ideo = reorder(respondent_ideo, respondent_ideo_num))%>%
	mutate(ideo_dem_num = as.numeric(ideo_dem),
				 ideo_dem = recode(ideo_dem, 
				 												 "1" = "Extremely Liberal", 
				 												 "2" = "Liberal", 
				 												 "3" = "Somewhat Liberal", 
				 												 "4" = "Moderate", 
				 												 "5" = "Somewhat Conservative", 
				 												 "6" = "Conservative", 
				 												 "7" = "Extremely Conservative"),
				 ideo_dem = reorder(ideo_dem, ideo_dem_num))%>%
	mutate(ideo_rep_num = as.numeric(ideo_rep),
				 ideo_rep = recode(ideo_rep, 
				 												 "1" = "Extremely Liberal", 
				 												 "2" = "Liberal", 
				 												 "3" = "Somewhat Liberal", 
				 												 "4" = "Moderate", 
				 												 "5" = "Somewhat Conservative", 
				 												 "6" = "Conservative", 
				 												 "7" = "Extremely Conservative"),
				 ideo_rep = reorder(ideo_rep, ideo_rep_num))%>%
  mutate(pid_3_sort = factor(recode(pid_7_num, #different from regular pid_3 which codes 3, 5 as partisan
                             "1" = "Democrat",
                             "2" = "Democrat",
                             "3" = "Independent",
                             "4" = "Independent",
                             "5" = "Independent",
                             "6" = "Republican",
                             "7" = "Republican"),
         levels = c("Democrat",
                    "Independent",
                    "Republican")))%>%
  mutate(ideo_3_sort = factor(recode(respondent_ideo_num, #anes codes leaning independents as partisans; not ideal
                                    "1" = "Liberal", #see not on pid_3_sort
                                    "2" = "Liberal",
                                    "3" = "Moderate",
                                    "4" = "Moderate",
                                    "5" = "Moderate",
                                    "6" = "Conservative",
                                    "7" = "Conservative"),
                             levels = c("Liberal",
                                        "Moderate",
                                        "Conservative")))%>%
  mutate(pid_2_sort = na_if(pid_3_sort, "Independent"))%>% #better just to filter(pid_3_sort != "Independent"), but used to build other vars
	mutate(pid_2 = na_if(pid_3, "Independent"))%>% 
	mutate(ideo_inparty_recode = if_else(pid_2 == "Democrat", 
																		(8-ideo_dem_num), ideo_rep_num))%>% #7= extremely liberal for dems, extremely conservative for reps
	mutate(ideo_self_recode = if_else(pid_2 == "Democrat",
																		(8-respondent_ideo_num), respondent_ideo_num))%>% #Recodes so that 7 is most lib for dems/con for reps
	mutate(ideo_self_party_dif = ideo_self_recode - ideo_inparty_recode)%>% #negative values indicate that R thinks party is more moderate than them.
	mutate(self_more_moderate_dum = if_else(ideo_self_party_dif < 0, 1, 0))%>%
	#mutate(ideo_inparty_dif = )
	mutate(ideo_2_sort = na_if(ideo_3_sort, "Moderate"))%>%# see pid_2_sort
  mutate(therm_dem = na_if(therm_dem, 98))%>%
  mutate(therm_dem = na_if(therm_dem, 99))%>%
  mutate(therm_rep = na_if(therm_rep, 98))%>%
  mutate(therm_rep = na_if(therm_rep, 99))%>%
#  mutate(therm_inparty = if_else(pid_3=="Democrat", therm_dem, therm_rep))%>%
	mutate(therm_inparty = if_else(pid_3=="Democrat" | pid_3 == "Republican",
																	if_else(pid_3=="Democrat", therm_dem, therm_rep),
																		(therm_dem + therm_rep)/2))%>% #therm in/out are equal
	mutate(therm_inparty = na_if(therm_inparty, -9))%>%
  mutate(therm_outparty = if_else(pid_3=="Democrat" | pid_3 == "Republican",
  																if_else(pid_3=="Democrat", therm_rep, therm_dem),
  																(therm_dem + therm_rep)/2))%>%
  mutate(npa_party = therm_inparty - therm_outparty)%>%
	mutate(therm_parties_mean = (therm_dem + therm_rep)/2)%>%
  mutate(therm_lib = na_if(therm_lib, 98))%>%
  mutate(therm_lib = na_if(therm_lib, 99))%>%
  mutate(therm_con = na_if(therm_con, 98))%>%
  mutate(therm_con = na_if(therm_con, 99))%>%
  mutate(therm_ideo_ingroup = if_else(ideo_2_sort=="Liberal", therm_lib, therm_con))%>%
  mutate(therm_ideo_outgroup = if_else(ideo_2_sort=="Liberal", therm_con, therm_lib))%>%
  mutate(net_ideo = therm_ideo_ingroup - therm_ideo_outgroup)%>%
	mutate(parties_therm_dif = therm_inparty - therm_outparty)%>% #creates a variable showing the difference in thermometer ratings for each party
	mutate(parties_ideo_dif = abs(ideo_dem_num - ideo_rep_num))%>%
	rename(income = VCF0114)%>%
	mutate(income_num = na_if(income, 0))%>%
	mutate(income = recode(income_num,
												 "1" = "0 -- 16 Percentile",
												 "2" = "17 -- 33 Percentile",
												 "3" = "34 -- 67 Percentile",
												 "4" = "68 -- 95 Percentile",
												 "5" = "96 -- 100 Percentile"))%>%
	mutate(income_bottom_third = if_else(income_num <= 3, 1, 0))%>%
	rename(distrust_gov_num = VCF0604)%>%
	mutate(distrust_gov = recode(distrust_gov_num,
														"1" = "Never",
														"2" = "Some of the Time",
														"3" = "Most of the Time",
														"4" = "Almost Always",
														"9" = NA_character_), #Don't know
				 distrust_gov = reorder(distrust_gov, distrust_gov_num),
				 distrust_gov_dum = case_when(distrust_gov_num == 1 | distrust_gov_num == 2 ~ 1,
				 														 distrust_gov_num == 3 | distrust_gov_num == 4 ~ 0,
				 														 TRUE ~ NA_real_),
				 distrust_gov_dum_qual = as.factor(case_when(distrust_gov == "Never" | distrust_gov == "Some of the Time" ~ "Low Trust",
																					distrust_gov == "Most of the Time" | distrust_gov == "Almost Always" ~ "High Trust",
									 												TRUE ~ NA_character_)))%>%
#				 distrust_gov_dum = recode(distrust_gov_dum_qual,
#				 											 "Low Trust" = "1",
#				 											 "High Trust" = "0"))%>%
	rename(gov_run_for_few_dum = VCF0605)%>%
	mutate(gov_run_for_few_dum = as.numeric(recode(gov_run_for_few_dum,
																			"1" = "1",
																			"2" = "0",
																			"9" = "0", #Coding "Don't Know" as 0, since I'm interested in those who think gov is run for few interests. I'm "working against myself" by adding to the denominator of proportion
																			"0" = NA_character_)))%>%
	rename(wrong_track_dum = VCF9222)%>%
	mutate(wrong_track_dum = as.numeric(recode(wrong_track_dum,
																	"1" = "0",
																	"2" = "1",
																	"-8" = "0",
																	"-9" = NA_character_)))%>%
	rename(officials_dont_care_dum = VCF0609)%>%
	mutate(officials_dont_care_dum = as.numeric(recode(officials_dont_care_dum,
																							"1" = "1",
																							"2" = "0",
																							"3" = "0",
																							"9" = "0",
																							"0" = NA_character_)))%>%
	rename(wealth_gap_larger_dum = VCF9227)%>%
	mutate(wealth_gap_larger_dum = as.numeric(recode(wealth_gap_larger_dum,
																				"1" = "1",
																				"2" = "0",
																				"3" = "0",
																				"-8" = "0",
																				"-9" = NA_character_)))%>%
	mutate(general_vote_dum = as.numeric(recode(general_vote,
																	 "1" = "0",
																	 "2" = "1", 
																	 "0" = NA_character_)))%>% #1 = "Voted"
	select(-ends_with("flag"))%>%
	#Behavioral items
	mutate(split_ticket_dum = as.numeric(recode(VCF0716,
																	 "1" = "0",
																	 "2" = "1",
																	 "0" = NA_character_)),
				 influence_dum = as.numeric(recode(VCF0717,
				 																	"1" = "0",
				 																	"2" = "1",
				 																	"0" = NA_character_)),
				 meetings_dum = as.numeric(recode(VCF0718,
				 																	"1" = "0",
				 																	"2" = "1",
				 																	"0" = NA_character_)),
				 work_cand_dum = as.numeric(recode(VCF0719,
				 																	"1" = "0",
				 																	"2" = "1",
				 																	"0" = NA_character_)),
				 display_merch_dum = as.numeric(recode(VCF0720,
				 																	"1" = "0",
				 																	"2" = "1",
				 																	"0" = NA_character_)),
				 donate_dum = as.numeric(recode(VCF0721,
				 																	"1" = "0",
				 																	"2" = "1",
				 																	"0" = NA_character_)),
				 watch_campaign_tv_dum = as.numeric(recode(VCF0724,
				 															 "1" = "0",
				 															 "2" = "1",
				 															 "0" = NA_character_)),
				 knows_house_pre_dum = as.numeric(recode(VCF0729,
				 																					"1" = "0",
				 																					"2" = "1",
				 																					"0" = NA_character_)),
				 knows_house_post_dum = as.numeric(recode(VCF0730,
				  																					"1" = "0",
				  																					"2" = "1",
				  																					"0" = NA_character_)),
				 talk_politics_most_days_dum = as.numeric(recode(VCF0733,
				 																					"0" = "0",
				 																					"1" = "0",
				 																					"2" = "0",
				 																					"3" = "0",
				 																					"4" = "1",
				 																					"5" = "1",
				 																					"6" = "1",
				 																					"7" = "1",
				 																					"9" = NA_character_)),
				 early_vote_dum = as.numeric(recode(VCF0748,
				 																"1" = "0",
				 																"5" = "1",
				 																"9" = NA_character_)),
				 vote_inparty_pres_dum = case_when(pres_election != 1 ~ NA_real_,
				 																	 pid_3 == "Democrat" & VCF0705 == 1 ~ 1,
				 																	 pid_3 == "Republican" & VCF0705 == 2 ~ 1,
				 																	 pid_3 != "Independent" & !is.na(VCF0705) ~ 0,
				 																	 TRUE ~ NA_real_),
				 vote_outparty_pres_dum = case_when(pres_election != 1 ~ NA_real_, #dummy for voting for MAJOR outparty
				 																	pid_3 == "Democrat" & VCF0705 == 2 ~ 1, #dem votes for Rep
				 																	pid_3 == "Republican" & VCF0705 == 1 ~ 1, #  Rep votes for dem
				 																	pid_3 != "Independent" & !is.na(VCF0705)  ~ 0, #independents are 0
				 																	TRUE ~ NA_real_),
				 vote_thirdparty_pres_dum = case_when(pres_election != 1 ~ NA_real_, #VCF0705 Presidential vote
				 																	 pid_3 == "Democrat" & VCF0705 == 3 ~ 1, # Dem votes for Thirdparty
				 																	 pid_3 == "Republican" & VCF0705 == 3 ~ 1, # Rep votes for Thirdparty
				 																	 pid_3 != "Independent" & !is.na(VCF0705) ~ 0, #independents are 0
				 																	 TRUE ~ NA_real_),
				 vote_inparty_house_dum = case_when(pid_3 == "Democrat" & VCF0736 == 1 ~ 1,
				 																	 pid_3 == "Republican" & VCF0736 == 5 ~ 1,
				 																	 pid_3 != "Independent" | VCF0736 == 7 ~ 0,
				 																	 TRUE ~ NA_real_),
				 activist_6cat = na_if(activist_6cat, 0),
				 activist_6cat = ((activist_6cat - 1)/6), #rescale activism index to be between 0-1
	 			 below_50_qual_lax = case_when(therm_inparty < 50 & pid_3 != "Independent" ~ "cold",
																	 therm_parties_mean < 50 & pid_3 == "Independent" ~ "cold",
																	 therm_inparty >= 50 | therm_parties_mean >= 50 ~ "warm",
																	 TRUE ~ NA_character_),
				 below_50_qual_strict = case_when(therm_inparty < 50  ~ "cold",
																					therm_inparty >= 70 ~ "warm",
																					TRUE ~ NA_character_))%>%
#	select(test,
#				 talk_politics_dum)%>%
#	filter(talk_politics_dum == 0 & test != 5)%>%
#	mutate(cult_att = if_else(pid_3_num == 3, (1-cult_att), cult_att))%>%
#	mutate(econ_att = if_else(pid_3_num == 3, (1-econ_att), econ_att))%>% #DO NOT USE UNLESS YOU ARE WORKING WITH DEMS AND REPS ONLY
#	select(-ends_with("_num")) %>%   # drop the numeric versions of the factors that i used for reordering above
#	select(-starts_with("VCF"))%>%
#	filter(pres_election == 1)%>% #BAD REMEMBER TO COMMENT OUT WHEN NOT USING BADBADNOTGOOD
	glimpse()%>%
	write_rds("data/tidy-cdf.rds")%>%
	write_csv("data/tidy-cdf.csv")

```