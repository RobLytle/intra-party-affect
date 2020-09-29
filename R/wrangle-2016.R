library(tidyverse)
library(sjlabelled)

data_2016 <- rio::import("data/raw/anes_timeseries_2016.zip", which = "anes_timeseries_2016_rawdata.txt")%>%
  rename(weight = V160101,
         primary_vote_choice = V161021a,
         therm_dem = V161095, #1-100, 100 warmest. -99: NA
         therm_rep = V161096, #1-100, 100 warmest. -99: NA
         pid_7 = V161158x, # Does R think of themeselves as a Dem, Rep, Ind or what? We are using this in addition to registration because we are interested in people's conceptions of party This is coded 1: strong-dem, 2: weak-dem 3: ind-dem, 4: ind, 5: ind-rep, 6: weak-rep, 7: strong-rep -8 DK, -9 NA
         pid_3 = V161155#, #Party ID: Does R think of self as Dem, Rep, Ind or what 0: no pref, 1. Democrat, 2. Republican, 3. Independent
         )%>%
  select(weight,
         primary_vote_choice, # Pulling out the variables we just renamed from the massive dataset
         therm_dem,
         therm_rep,
         self_ideo_7,
         dem_ideo_7,
         rep_ideo_7,
         pid_7,
         pid_3
         )%>%
  mutate(pid_7 = na_if(pid_7, -9),
         pid_7 = na_if(pid_7, -8))%>%
  mutate(therm_dem = na_if(therm_dem, -99),
         therm_dem = na_if(therm_dem, -88),
         therm_dem = na_if(therm_dem, -89))%>%
  mutate(therm_rep = na_if(therm_rep, -99),
         therm_rep = na_if(therm_rep, -88),
         therm_rep = na_if(therm_rep, -89))%>%
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
  mutate(therm_inparty = case_when(pid_3 == "Democrat" ~ therm_dem,
                                   pid_3 == "Republican" ~ therm_rep,
                                   TRUE ~ NA_integer_))%>%
  mutate(therm_outparty = case_when(pid_3 == "Democrat" ~ therm_rep,
                                   pid_3 == "Republican" ~ therm_dem,
                                   TRUE ~ NA_integer_))%>%
  mutate(primary_vote_choice = recode(primary_vote_choice, # Unlike the factor vectors above, I'm not reorder this one, as there's nothing
                                      "-1" = "Didn't Vote",
                                      "1" = "Hillary Clinton", #substantively meaningful about Hillary being "1" and Marco being "7"
                                      "2" = "Bernie Sanders",
                                      "3" = "Another Democrat",
                                      "4" = "Donald Trump",
                                      "5" = "Ted Cruz",
                                      "6" = "John Kasich",
                                      "7" = "Marco Rubio",
                                      "8" = "Another Republican",
                                      "9" = "A Third Party Candidate",
                                      "-8" = "Don't Know",
                                      "-9" = "Refused"))%>% 
glimpse()%>%#
write_rds("data/tidy-2016.rds")%>%
write_csv("data/tidy-2016.csv")
