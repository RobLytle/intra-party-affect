library(tidyverse)
library(haven)
library(stringr)
library(rio)
library(lubridate)
#2008

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


df_2008 <- import("data/raw/anes/anes_timeseries_2008.zip", 
                  which = "anes_timeseries_2008_rawdata.txt")%>%
  select(case = V080001,
         age = V081104,
         sex = V081101,
         race = V081102,
         income = V083248x,
#         sex_iwr = V083311,
         weight = V081001,
         primary_vote_choice = V083077a,
         primary_vote = V083077,
         ideo_self = V083069, #1-7 lib con, -na
         ideo_rep  = V083071b,
         ideo_dem = V083071a,
         therm_dem = V083044a, #1-100, 100 warmest. -99: NA
         therm_rep = V083044b, #1-100, 100 warmest. -99: NA
         pid_3 = V083097, #Party ID: Does R think of self as Dem, Rep, Ind or what 0: no pref, 1. Democrat, 2. Republican, 3. Independent
#         date = V082001c
  )%>%
  mutate(age = na_if(age, -9),
         sex = recode(sex,
                      "1" = "Male",
                      "2" = "Female"),
         pid_3 = recode(pid_3, 
                        "5" = "No Preference", 
                        "1" = "Democrat", 
                        "2" = "Republican", 
                        "3" = "Independent",
                        "4" = "Other",
                        "-8" = NA_character_,
                        "-9" = NA_character_),
         race = case_when(race == 1 ~ "White",
                          race == 2 ~ "Black",
                          race >= 3 ~ "Other",
                          TRUE ~ NA_character_),
         income = if_else(income <= 0, NA_integer_, income),
         below_35k_dum = if_else(income <= 13, 1, 0)) %>% 
  mutate(therm_dem = na_if(therm_dem, -9),
         therm_dem = na_if(therm_dem, -8),
         therm_dem = na_if(therm_dem, -6))%>%
  mutate(therm_rep = na_if(therm_rep, -9),
         therm_rep = na_if(therm_rep, -8),
         therm_rep = na_if(therm_rep, -6))%>%
  mutate(therm_inparty = case_when(pid_3 == "Democrat" ~ therm_dem,
                                   pid_3 == "Republican" ~ therm_rep,
                                   TRUE ~ NA_integer_))%>%
  mutate(therm_outparty = case_when(pid_3 == "Democrat" ~ therm_rep,
                                    pid_3 == "Republican" ~ therm_dem,
                                    TRUE ~ NA_integer_))%>%
  mutate(primary_vote_dum = recode(primary_vote,
                                   "1" = "1",
                                   "5" = "0",
                                   "-8" = NA_character_,
                                   "-9" = NA_character_))%>%
  mutate(primary_vote_choice = recode(primary_vote_choice, # Unlike the factor vectors above, I'm not reorder this one, as there's nothing substantive about it
                                      "-1" = NA_character_,
                                      "1" = "Joe Biden", 
                                      "2" = "Hillary Clinton",
                                      "3" = "Chris Dodd",
                                      "4" = "John Edwards",
                                      "5" = "Rudy Giuliani",
                                      "6" = "Mike Gravel",
                                      "7" = "Mike Huckabee",
                                      "8" = "Duncan Hunter",
                                      "9" = "Alan Keyes",
                                      "10" = "Dennis Kucinich",
                                      "11" = "John McCain",
                                      "12" = "Barack Obama",
                                      "13" = "Ron Paul",
                                      "14" = "Bill Richardson",
                                      "15" = "Mitt Romney",
                                      "16" = "Tom Tancredo",
                                      "17" = "Fred Thompson",
                                      "30" = "Someone Else",
                                      "-8" = NA_character_,
                                      "-9" = NA_character_
  ))%>%
  mutate(cand_party = recode(primary_vote_choice,
                             "Joe Biden" = "Democrat",
                             "Hillary Clinton" = "Democrat",
                             "Chris Dodd" = "Democrat",
                             "John Edwards" = "Democrat",
                             "Rudy Giuliani" = "Republican",
                             "Mike Gravel" = "Democrat",
                             "Mike Huckabee" = "Republican",
                             "Alan Keyes" = "Republican",
                             "Dennis Kucinich" = "Democrat",
                             "John McCain" = "Republican",
                             "Barack Obama" = "Democrat",
                             "Ron Paul" = "Republican",
                             "Bill Richardson" = "Democrat",
                             "Tom Tancredo" = "Republican",
                             "Fred Thompson" = "Republican",
                             "Someone Else" = "Other/Third Party"))%>%
  mutate(primary_vote_choice = as.factor(if_else(is.na(primary_vote_choice) & primary_vote_dum == 0, "Didn't Vote", primary_vote_choice)))%>%
  mutate(primary_vote_simple = case_when(pid_3 != cand_party ~ "Voted in Other Primary",
                                         pid_3 == "Democrat" & primary_vote_choice == "Barack Obama" ~ "Winner",
                                         pid_3 == "Republican" & primary_vote_choice == "John McCain" ~ "Winner",
                                         pid_3 != "Indpendent" & primary_vote_choice != "Didn't Vote" ~ "Loser",
                                         pid_3 != "Indpendent" & primary_vote_choice == "Didn't Vote" ~ "Didn't Vote",
                                         TRUE ~ NA_character_),
         primary_vote = recode(primary_vote, .default = NA_character_,
                               "1" = "1",
                               "5" = "0"),)%>%
  mutate(year = "2008",
         case = paste(year, case, sep = "_"),)%>%
  glimpse()


#
##################
#################



df_2012 <- import("data/raw/anes/anes_timeseries_2012.zip", which = "anes_timeseries_2012_rawdata.txt")%>%
  select(case = caseid,
         age = dem_age_r_x,
         race = dem_raceeth_x,
         sex = gender_respondent_x,
         income  = inc_incgroup_pre,
         weight = weight_full,
         primary_vote = prevote_primv,
         primary_vote_choice = prevote_primvwho,
         therm_dem = ft_dem, #1-100, 100 warmest. -99: NA
         therm_rep = ft_rep, #1-100, 100 warmest. -99: NA
         ideo_self = libcpre_self,
         ideo_dem = libcpre_ptyd,
         ideo_rep = libcpre_ptyr,
         pid_3 = pid_self, #Party ID: Does R think of self as Dem, Rep, Ind or what 0: no pref, 1. Democrat, 2. Republican, 3. Independent
         pre_ftf_date = admin_pre_ftf_iwdatebeg,
         pre_web_1_date = admin_pre_web1_iwdatebeg,
         pre_web_2_date = admin_pre_web2_iwdatebeg,
  )%>%
  mutate(age = na_if(age, -9),
         age = na_if(age, -8),
         age = na_if(age, -2),
         income = if_else(income <= 0, NA_integer_, income),
         below_35k_dum = if_else(income <= 11, 1, 0),
         sex = recode(sex,
                      "1" = "Male",
                      "2" = "Female"),
         race = case_when(race == 1 ~ "White",
                          race == 2 ~ "Black",
                          race >= 3 ~ "Other",
                          TRUE ~ NA_character_),
         pid_3 = recode(pid_3, 
                        "0" = "No Preference", 
                        "1" = "Democrat", 
                        "2" = "Republican", 
                        "3" = "Independent",
                        "5" = "Other",
                        "-8" = NA_character_,
                        "-9" = NA_character_))%>%
  mutate(therm_dem = na_if(therm_dem, -99),
         therm_dem = na_if(therm_dem, -88),
         therm_dem = na_if(therm_dem, -89))%>%
  mutate(therm_rep = na_if(therm_rep, -99),
         therm_rep = na_if(therm_rep, -88),
         therm_rep = na_if(therm_rep, -89))%>%
  mutate(therm_inparty = case_when(pid_3 == "Democrat" ~ therm_dem,
                                   pid_3 == "Republican" ~ therm_rep,
                                   TRUE ~ NA_integer_))%>%
  mutate(therm_outparty = case_when(pid_3 == "Democrat" ~ therm_rep,
                                    pid_3 == "Republican" ~ therm_dem,
                                    TRUE ~ NA_integer_))%>%
  mutate(primary_vote_dum = recode(primary_vote,
                                   "1" = "1",
                                   "2" = "0",
                                   "-8" = NA_character_,
                                   "-9" = NA_character_))%>%
  mutate(primary_vote_choice = recode(primary_vote_choice, # Unlike the factor vectors above, I'm not reorder this one, as there's nothing substantive about it
                                      "-1" = NA_character_,
                                      "1" = "Mitt Romney", 
                                      "2" = "Barack Obama",
                                      "3" = "Rick Santorum",
                                      "4" = "Newt Gingrich",
                                      "5" = "Ron Paul",
                                      "6" = "Rick Perry",
                                      "7" = "Michelle Bachmann",
                                      "8" = "Jon Huntsman",
                                      "9" = "Herman Cain",
                                      "95" = "Someone Else",
                                      "-8" = NA_character_,
                                      "-9" = NA_character_
  ))%>%
  mutate(cand_party = recode(primary_vote_choice, .default = "Republican", #supplying Republican as default to save time
                             "Barack Obama" = "Democrat",
                             "Someone Else" = "Other/Third Party"))%>%
  mutate(primary_vote_choice = as.factor(if_else(is.na(primary_vote_choice) & primary_vote_dum == 0, "Didn't Vote", primary_vote_choice)))%>%
  mutate(primary_vote_simple = case_when(pid_3 != cand_party ~ "Voted in Other Primary",
                                         pid_3 == "Democrat" & primary_vote_choice == "Barack Obama" ~ "Winner",
                                         pid_3 == "Republican" & primary_vote_choice == "Mitt Romney" ~ "Winner",
                                         pid_3 != "Indpendent" & primary_vote_choice != "Didn't Vote" ~ "Loser",
                                         pid_3 != "Indpendent" & primary_vote_choice == "Didn't Vote" ~ "Didn't Vote",
                                         TRUE ~ NA_character_),
         primary_vote = recode(primary_vote, .default = NA_character_,
                               "1" = "1",
                               "2" = "0"),)%>%
  mutate(year = 2012,
         pre_ftf_date = ymd(pre_ftf_date),
         pre_web_1_date = ymd(pre_web_1_date),
         pre_web_2_date = ymd(pre_web_2_date),
         num_w1 = as.numeric(pre_web_1_date),
         num_w2 = as.numeric(pre_web_2_date),
         web_date = as_date(round((num_w1 + num_w2)/2, 0)),
         date = if_else(!is.na(pre_ftf_date), pre_ftf_date, web_date),
         case = paste(year, case, sep = "_"),
)%>%
  select(colnames(df_2008))%>%
  glimpse()%>%#
  write_rds("data/tidy-2012.rds")%>%
  write_csv("data/tidy-2012.csv")


#2016
df_2016 <- rio::import("data/raw/anes/anes_timeseries_2016.zip", which = "anes_timeseries_2016_rawdata.txt")%>%
  select(case = V160001,
         age = V161267,
         sex = V161342,
         race = V161310x,
         income = V161361x,
         weight = V160101,
         primary_vote = V161021,
         primary_vote_choice = V161021a,
         therm_dem = V161095, #1-100, 100 warmest. -99: NA
         therm_rep = V161096, #1-100, 100 warmest. -99: NA
         ideo_self = V161126,
         ideo_dem = V161130,
         ideo_rep = V161131,
         pid_7 = V161158x, # Does R think of themeselves as a Dem, Rep, Ind or what? We are using this in addition to registration because we are interested in people's conceptions of party This is coded 1: strong-dem, 2: weak-dem 3: ind-dem, 4: ind, 5: ind-rep, 6: weak-rep, 7: strong-rep -8 DK, -9 NA
         pid_3 = V161155,#, #Party ID: Does R think of self as Dem, Rep, Ind or what 0: no pref, 1. Democrat, 2. Republican, 3. Independent
         date = V164002,
  )%>%
  mutate(age = na_if(age, -9),
         sex = recode(sex,
                      "1" = "Male",
                      "2" = "Female",
                      "3" = "Other/Refused",
                      "-9" = "Other/Refused"),
         race = case_when(race == 1 ~ "White",
                          race == 2 ~ "Black",
                          race >= 3 ~ "Other",
                          TRUE ~ NA_character_),
         pid_7 = na_if(pid_7, -9),
         pid_7 = na_if(pid_7, -8))%>%
  mutate(pid_7_num = as.numeric(pid_7), # Recoding party_id as a factor, making sure to order it in a substantive way.
         pid_7 = recode(pid_7, 
                        "1" = "Strong Democrat", 
                        "2" = "Weak Democrat", 
                        "3" = "Independent - Democrat", 
                        "4" = "Independent - Independent", 
                        "5" = "Independent - Republican", 
                        "6" = "Weak Republican", 
                        "7" = "Strong Republican"
         ), # We are leaving off `"-9" = NA`, because we have already set na_if(pid_7, -9) above.
         pid_7 = reorder(pid_7, pid_7_num))%>%
  mutate(pid_3 = recode(pid_3, 
                        "0" = "No Preference", 
                        "1" = "Democrat", 
                        "2" = "Republican", 
                        "3" = "Independent",
                        "5" = "Other",
                        "-8" = NA_character_,
                        "-9" = NA_character_))%>%
  mutate(therm_dem = na_if(therm_dem, -99),
         therm_dem = na_if(therm_dem, -88),
         therm_dem = na_if(therm_dem, -89))%>%
  mutate(therm_rep = na_if(therm_rep, -99),
         therm_rep = na_if(therm_rep, -88),
         therm_rep = na_if(therm_rep, -89))%>%
  mutate(therm_inparty = case_when(pid_3 == "Democrat" ~ therm_dem,
                                   pid_3 == "Republican" ~ therm_rep,
                                   TRUE ~ NA_integer_))%>%
  mutate(therm_outparty = case_when(pid_3 == "Democrat" ~ therm_rep,
                                    pid_3 == "Republican" ~ therm_dem,
                                    TRUE ~ NA_integer_))%>%
  mutate(primary_vote_dum = recode(primary_vote,
                                   "1" = "1",
                                   "2" = "0",
                                   "-8" = NA_character_,
                                   "-9" = NA_character_))%>%
  mutate(primary_vote_choice = recode(primary_vote_choice, # Unlike the factor vectors above, I'm not reorder this one, as there's nothing
                                      "-1" = NA_character_,
                                      "1" = "Hillary Clinton", #substantively meaningful about Hillary being "1" and Marco being "7"
                                      "2" = "Bernie Sanders",
                                      "3" = "Another Democrat",
                                      "4" = "Donald Trump",
                                      "5" = "Ted Cruz",
                                      "6" = "John Kasich",
                                      "7" = "Marco Rubio",
                                      "8" = "Another Republican",
                                      "9" = "A Third Party Candidate",
                                      "-8" = NA_character_, #"Don't Know",
                                      "-9" = NA_character_#"Refused"
  ))%>%
  mutate(primary_vote_choice = as.factor(if_else(is.na(primary_vote_choice) & primary_vote_dum == 0, "Didn't Vote", primary_vote_choice)))%>%
  mutate(cand_party = recode(primary_vote_choice, .default = NA_character_,
                             "Hillary Clinton" = "Democrat",
                             "Bernie Sanders" = "Democrat",
                             "Another Democrat" = "Democrat",
                             "Donald Trump" = "Republican",
                             "Ted Cruz" = "Republican",
                             "John Kasich" = "Republcian",
                             "Marco Rubio" = "Republican",
                             "Another Republican" = "Republican",
                             "A Third Party Candidate" = "Other/Third Party"
  ))%>%
  mutate(primary_vote_simple = case_when(pid_3 != cand_party ~ "Voted in Other Primary",
                                         pid_3 == "Democrat" & primary_vote_choice == "Hillary Clinton" ~ "Winner",
                                         pid_3 == "Republican" & primary_vote_choice == "Donald Trump" ~ "Winner",
                                         pid_3 != "Indpendent" & primary_vote_choice != "Didn't Vote" ~ "Loser",
                                         pid_3 != "Indpendent" & primary_vote_choice == "Didn't Vote" ~ "Didn't Vote",
                                         TRUE ~ NA_character_),
         primary_vote = recode(primary_vote, .default = NA_character_,
                               "1" = "1",
                               "5" = "0"),
         income = if_else(income <= 0, NA_integer_, income),
         below_35k_dum = if_else(income <= 11, 1, 0))%>%
  mutate(year = 2016,
         date = ymd(date),
         case = paste(year, case, sep = "_"),
         )%>%
  select(colnames(df_2008))%>% #selecting only columns that exist in the 2008 dataset
  glimpse()%>%#
  write_rds("data/tidy-2016.rds")%>%
  write_csv("data/tidy-2016.csv")


###########################
###########################

df_2020 <- read_rds("data/tidy-2020.rds")%>%
  select(colnames(df_2008))%>%
  glimpse()



####
####

primaries <- rbind(df_2016, df_2008, df_2012, df_2020)%>%
  mutate(pid_3 = factor(pid_3, 
                        levels = c("1" = "Democrat", 
                                   "2" = "Independent", 
                                   "3" = "Republican")),
         primary_vote_choice = as.factor(primary_vote_choice),
         year = as.factor(year),
         primary_vote_simple = factor(primary_vote_simple,
                                      levels = c("1" = "Winner",
                                                 "2" = "Loser",
                                                 "3" = "Didn't Vote",
                                                 "4" = "Voted in Other Primary")),
         cand_party = factor(cand_party,
                             levels = c("1" = "Democrat",
                                        "2" = "Republican",
                                        "3" = "Other/Third Party")),
         primary_vote_dum = as.numeric(primary_vote_dum))%>%
  mutate(across(matches("ideo"), ~ if_else(. >= 1 & . <= 7, ., NA_integer_)),
         ideo_inparty = case_when(pid_3 == "Democrat" ~ ideo_dem,
                                  pid_3 == "Republican" ~ ideo_rep,
                                  TRUE ~ NA_integer_),
         ideo_outparty = case_when(pid_3 == "Democrat" ~ ideo_rep,
                                  pid_3 == "Republican" ~ ideo_dem,
                                  TRUE ~ NA_integer_),
         ideo_self_in_dif = ideo_self - ideo_inparty, #positive values mean R thinks party is more conservative than themselves
         ideo_self_out_dif = ideo_self - ideo_outparty,
         ideo_in_out_dif = abs(ideo_inparty - ideo_outparty),
         age_group = as.factor(ntile(age, 5)),
         ideo_self_std = (ideo_self-4)/3,
         ideo_self_prt_std = zero1(case_when(pid_3 == "Democrat" ~ ideo_self_std*-1, # recodes so that high numbers are party-specific extreme 1 = ext. lib for dems, 1 = ext con for reps
                                       pid_3 == "Republican" ~ ideo_self_std,
                                       TRUE ~ NA_real_)),
         ideo_inparty_std = (ideo_inparty-4)/3,
         ideo_inparty_prt_std = zero1(case_when(pid_3 == "Democrat" ~ ideo_inparty_std*-1, # recodes so that high numbers are party-specific extreme 1 = ext. lib for dems, 1 = ext con for reps
                                       pid_3 == "Republican" ~ ideo_inparty_std,
                                       TRUE ~ NA_real_)),
         ideo_self_in_dif_std = ideo_self_prt_std - ideo_inparty_prt_std,
         primary_vote = as.numeric(primary_vote),
         male_dum = if_else(sex == "Male", 1, 0),
         race = as.factor(race),
         white_dum = if_else(race == "White", 1, 0))%>% #0 values are more moderate, relative to the party
  glimpse()%>%
  filter(pid_3 != "Independent") %>% 
  write_rds("data/tidy-primaries.rds")

primaries %>%  #1-7 lib-con
  ggplot(aes(x = ideo_self_in_dif, y = ideo_self_out_dif)) +
  facet_wrap(vars(pid_3)) +
  geom_point(position = "jitter")

levels(primaries$race)
