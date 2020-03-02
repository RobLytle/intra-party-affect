library(tidyverse)
library(sjlabelled)

#anes_df <- read_dta("data/raw/anes_timeseries_2016.dta")%>%
#  select(V161095, V161096, V161086, V161087, V161126, V161128, V161129, V161130, V161131, V161021, V161021a)%>%
#  glimpse()

anes_raw <- read_rds("data/raw/cdf-raw-trim.rds")%>% # Loads RDS created in `anes-cdf-trim.R`
glimpse()

anes_char <- anes_raw %>%
  remove_all_labels()%>%
  rename(year = VCF0004)%>% # Year of response
  rename(pid_7 = VCF0301)%>% #7 scale Party ID val: 1-7. Strong Democrat 2. Weak Democrat3. Independent - Democrat4. Independent - Independent5. Independent - Republican6. Weak Republican7. Strong Republican
  rename(pid_3 = VCF0303)%>% # Party ID 3 categories val: "Republican", "Independent", "Democrat" (Dem/Rep include Leaners)
  rename(pid_str = VCF0305)%>% # PID strength val: 1. Independent 2. Leaning Independent 3. Weak Partisan 4. Strong Partisan Kept this because I wanted to create basically this variable later
  rename(win_care_pres = VCF0311)%>% # How much do you care which party wins presidency? val: 1. Don't care very much or DK, pro-con, depends, and other, 2. Care a great deal
  rename(win_care_cong = VCF0312)%>% # How much do you care which party wins congress? val: 1. Don't care very much or DK, pro-con, depends, and other, 2. Care a great deal notes: only asked through 2008
  rename(respondent_ideo = VCF0803)%>% # Liberal-conservative scale val: 1(extremely liberal)- 7(extremely conservative) 9. DK; haven't much thought about it
  rename(therm_dem = VCF0218)%>% # val 00-96 cold-warm as coded; 97: 97-100, 98: DK, 99. NA
  rename(therm_rep = VCF0224)%>% # val 00-96 cold-warm as coded; 97: 97-100, 98: DK, 99. NA
  rename(activist_6cat = VCF0723)%>%#val: 1-6 low-high participation 0. DKN/NA
  rename(ideo_dem = VCF0503)%>% # val: 1-7 lib-con
  rename(ideo_rep = VCF0504)%>%# val: 1-7 lib-con
  rename(primary_vote = VCF9265)%>%
  select(year,  
         pid_7, 
         pid_3, 
         pid_str, 
         win_care_pres,
         win_care_cong,
         respondent_ideo,
         therm_dem,
         therm_rep,
         activist_6cat,
         ideo_dem,
         ideo_rep,
         primary_vote,
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
         VCF0310, #interest 1(not much), 2(somewhat), 3(very) 9dk, 0na
         VCF0130, #worship 1(every week), 2(almost every week), 3 (once or twice a month), 4 (few times a year), 5(never), 7(no relig.), 890na
         VCF0050a, # iwrpkpre 1(very high)-5(very low)
         VCF0050b, # iwrpkpst (same above), take mean
         VCF9255, #satisfied_democ 1(very), 2(fairly), 3(not very), 4(not at all) -8,-9NA
         VCF0729, #know_house which party has the most seats in house 1(wrong), 2(right), 0NA
         VCF9036 #know_sen 1-2(correct), 3-4(wrong), 7-9NA
         )%>%
  rename(insurance = VCF0806)%>%
    mutate(insurance = na_if(insurance, 9))%>%
    mutate(insurance = na_if(insurance, 0))%>%
    mutate(insurance = (insurance-1)/7)%>%#standardize between 0-1
  rename(jobs = VCF0809)%>%
    mutate(jobs = na_if(jobs, 9))%>%
    mutate(jobs = na_if(jobs, 0))%>%
    mutate(jobs = (jobs-1)/7)%>%
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
    mutate(services = (services-1)/7)%>%
  rename(ss = VCF9049)%>%
    mutate(ss = na_if(ss, 8))%>%
    mutate(ss = na_if(ss, 9))%>%
    mutate(no_ss = if_else(ss == 7, 1, 0))%>%
    mutate(ss = na_if(ss, 7))%>%
    mutate(ss = (ss-1)/4)%>%
  rename(women = VCF0834)%>%
    mutate(women = na_if(women, 9))%>%
    mutate(women = na_if(women, 0))%>%
  rename(abortion = VCF0838)%>%
    mutate(abortion = as.numeric(recode(abortion, #recoding so that liberal values are lower, in accordance with everything else..
                                      "1" = "4",
                                      "2" = "3",
                                      "3" = "2",
                                      "4" = "1"
    )))%>%
  mutate(abortion = (abortion-1)/4)%>%
  rename(gayrights = VCF0876a)%>%
    mutate(gayrights = na_if(gayrights, 7))%>%
    mutate(gayrights = na_if(gayrights, 9))%>%
    mutate(gayrights = (gayrights - 1)/5)%>%
  rename(education = VCF0110)%>%#education 1 (grade school), 2(High School), 3(Some College), 4(College/advanced) 0DK/NA
    mutate(education = na_if(education, 0))%>%
    mutate(high_school = if_else(education == 2, 1, 0))%>% #creating education dummies. grade school is ref. cat.
    mutate(some_college = if_else(education == 3, 1, 0))%>%
    mutate(college_adv = if_else(education == 4, 1, 0))%>%
  rename(white = VCF0105a)%>%
    mutate(white = na_if(white, 9))%>%
    mutate(white = if_else(white==1, 1, 0))%>%
  rename(south = VCF0113)%>%
    mutate(south = if_else(south==1, 1, 0))%>%
  rename(interest = VCF0310)%>%
    mutate(interest = na_if(interest, 9))%>%
    mutate(interest = na_if(interest, 0))%>%
  rename(worship = VCF0130)%>%
    mutate(worship = if_else(worship == 1 | worship == 2, 1, 0))%>%
  rename(iwrpk_pre = VCF0050a)%>%
    mutate(iwrpk_pre = na_if(iwrpk_pre, 9))%>%
  rename(iwrpk_post = VCF0050b)%>%
    mutate(iwrpk_post = na_if(iwrpk_post, 9))%>%
    mutate(iwrpk_post = na_if(iwrpk_post, 0))%>%
  rename(dis_democ = VCF9255)%>%
    mutate(dis_democ = na_if(dis_democ, -9))%>%
    mutate(dis_democ = na_if(dis_democ, -8))%>%
    mutate(dis_democ = if_else(dis_democ == 3 | dis_democ ==4, 1, 0))%>%
  rename(know_house = VCF0729)%>%
    mutate(know_house = na_if(know_house, 0))%>%
    mutate(know_house = if_else(know_house==1, 1, 0))%>%
  rename(know_sen = VCF9036)%>%
    mutate(know_sen = na_if(know_sen, 0))%>%
    mutate(know_sen = if_else(know_sen==1, 1, 0))%>%
  mutate(know_cong = (know_sen + know_house)/2)%>%
  mutate(cult_att = (abortion + women + gayrights)/ 3)%>%
  mutate(econ_att = (ss + services + jobs + insurance)/4)%>%
  glimpse()%>%# adds only these variables to the df
  mutate(win_care_pres = na_if(win_care_pres, 0))%>% #these functions set the specified value to NA, (per the ANES codebook)
  mutate(win_care_cong = na_if(win_care_cong, 0))%>%
  mutate(respondent_ideo = na_if(respondent_ideo, 9))%>%#the recode() function is used in the next 4 pipes to apply new values to observation in the columns. ANES uses numerical values to represent factors.
  mutate(ideo_rep = na_if(ideo_rep, 8))%>%
  mutate(ideo_rep = na_if(ideo_rep, 0))%>%
  mutate(ideo_dem = na_if(ideo_dem, 8))%>%
  mutate(ideo_dem = na_if(ideo_dem, 0))%>%
  mutate(primary_vote = na_if(primary_vote, -8))%>%
  mutate(primary_vote = na_if(primary_vote, -9))%>%
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
  mutate(parties_therm_dif = sqrt((therm_dem - therm_rep)^2))%>% #creates a variable showing the difference in thermometer ratings for each party
  mutate(parties_ideo_dif = abs(ideo_dem - ideo_rep))%>%
  select(-ends_with("_num")) %>%   # drop the numeric versions of the factors that i used for reordering above
  glimpse()%>%
  write_rds("data/tidy-cdf.rds") %>%
  write_csv("data/tidy-cdf.csv")

