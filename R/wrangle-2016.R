library(tidyverse)
library(sjlabelled)

data_2016 <- rio::import("data/raw/anes_timeseries_2016.zip", which = "anes_timeseries_2016_rawdata.txt")%>%
  rename(primary_vote_choice = V161021a,
         pre_therm_dem = V161095, #1-100, 100 warmest. -99: NA
         pre_therm_rep = V161096, #1-100, 100 warmest. -99: NA
         pre_self_ideo_7 = V161126, #1-7: Lib-Con
         pre_dem_ideo_7 = V161130, # R's rating of Democratic party's ideology on 1-7 (lib-con) scale
         pre_rep_ideo_7 = V161131, # R's rating of Republican party's ideology on 1-7 (lib-con) scale
         pre_pid_7 = V161158x, # Does R think of themeselves as a Dem, Rep, Ind or what? We are using this in addition to registration because we are interested in people's conceptions of party This is coded 1: strong-dem, 2: weak-dem 3: ind-dem, 4: ind, 5: ind-rep, 6: weak-rep, 7: strong-rep -8 DK, -9 NA
         pre_pid_3 = V161155#, #Party ID: Does R think of self as Dem, Rep, Ind or what 0: no pref, 1. Democrat, 2. Republican, 3. Independent
#         pre_pid_lean = V161157 # Do you think of yourself as closer to the Republican Party or to the Democratic Party? if ind#
         )%>%
  select(primary_vote_choice, # Pulling out the variables we just renamed from the massive dataset
         pre_therm_dem,
         pre_therm_rep,
         pre_self_ideo_7,
         pre_dem_ideo_7,
         pre_rep_ideo_7,
         pre_pid_7,
         pre_pid_3
         )%>%
  mutate(dem_ideo_dk = (pre_dem_ideo_7 == -8))%>%
  mutate(ft_dk = ((pre_therm_dem == -88) | (pre_therm_rep == -88) & (pre_therm_dem == -88)) | ((pre_therm_dem == -89) | (pre_therm_rep == -89) & (pre_therm_dem == -89)))%>% #this line creates a dummy variable (ft_dk) to indicate if the NA value for ft is due to a "don't know" or "don't recognize" response, rather than a "refuse"
  mutate(pre_pid_7 = na_if(pre_pid_7, -9),
         pre_pid_7 = na_if(pre_pid_7, -8))%>%
  mutate(pre_rep_ideo_7 = na_if(pre_rep_ideo_7, -9),
         pre_rep_ideo_7 = na_if(pre_rep_ideo_7, -8))%>%
  mutate(pre_dem_ideo_7 = na_if(pre_dem_ideo_7, -9),
         pre_dem_ideo_7 = na_if(pre_dem_ideo_7, -8))%>%
  mutate(ideo_7_dk = (pre_self_ideo_7 == c(-8, 99)))%>% #this line creates a dummy variable (ideo_7_dk) to indicate if the NA value for R ideo is due to a "don't know" or "don't recognize" response, rather than a "refuse"
  mutate(pre_self_ideo_7 = na_if(pre_self_ideo_7, -9),
         pre_self_ideo_7 = na_if(pre_self_ideo_7, -8),
         pre_self_ideo_7 = na_if(pre_self_ideo_7, 99))%>%
  mutate(pre_therm_dem = na_if(pre_therm_dem, -99),
         pre_therm_dem = na_if(pre_therm_dem, -88),
         pre_therm_dem = na_if(pre_therm_dem, -89))%>%
  mutate(pre_therm_rep = na_if(pre_therm_rep, -99),
         pre_therm_rep = na_if(pre_therm_rep, -88),
         pre_therm_rep = na_if(pre_therm_rep, -89))%>%
  mutate(pre_pid_7_num = as.numeric(pre_pid_7), # Recoding party_id as a factor, making sure to order it in a substantive way.
         pre_pid_7 = recode(pre_pid_7, 
                        "1" = "Strong Democrat", 
                        "2" = "Weak Democrat", 
                        "3" = "Independent - Democrat", 
                        "4" = "Independent - Independent", 
                        "5" = "Independent - Republican", 
                        "6" = "Weak Republican", 
                        "7" = "Strong Republican"
                        ), # We are leaving off `"-9" = NA`, because we have already set na_if(pre_pid_7, -9) above.
         pre_pid_7 = reorder(pre_pid_7, pre_pid_7_num))%>%
  mutate(pid_3_np_other = (pre_pid_3 == c(5, -8, 0)))%>% #Creates a dummy variable which is TRUE if R didn't know or preferred a third party. FALSE if refused
  mutate(pre_pid_3_num = as.numeric(pre_pid_3),
         pre_pid_3_num = recode(pre_pid_3,  # The ANES (My cruel mistress), codes Reps as "2" and Ind as "3", this flips them so the order makes more sense
                                "0" = "0",
                                "1" = "1",
                                "3" = "2",
                                "2" = "3"),# Recoding party_id as a factor, making sure to order it in a substantive way.
         pre_pid_3 = recode(pre_pid_3, 
                            "0" = "No Preference", 
                            "1" = "Democrat", 
                            "2" = "Republican", 
                            "3" = "Independent"
         ),
         pre_pid_3 = reorder(pre_pid_3, pre_pid_3_num))%>%
  mutate(pre_dem_ideo_7_num = as.numeric(pre_dem_ideo_7),
         pre_dem_ideo_7 = recode(pre_dem_ideo_7, 
                                 "1" = "Extremely Liberal", 
                                 "2" = "Liberal", 
                                 "3" = "Somewhat Liberal", 
                                 "4" = "Moderate", 
                                 "5" = "Somewhat Conservative", 
                                 "6" = "Conservative", 
                                 "7" = "Extremely Conservative"
         ),
         pre_dem_ideo_7 = reorder(pre_dem_ideo_7, pre_dem_ideo_7_num))%>%
  mutate(pre_rep_ideo_7_num = as.numeric(pre_rep_ideo_7),
         pre_rep_ideo_7 = recode(pre_rep_ideo_7, 
                                 "1" = "Extremely Liberal", 
                                 "2" = "Liberal", 
                                 "3" = "Somewhat Liberal", 
                                 "4" = "Moderate", 
                                 "5" = "Somewhat Conservative", 
                                 "6" = "Conservative", 
                                 "7" = "Extremely Conservative"
         ),
         pre_rep_ideo_7 = reorder(pre_rep_ideo_7, pre_rep_ideo_7_num))%>%
  mutate(primary_vote_choice = recode(primary_vote_choice, # Unlike the factor vectors above, I'm not reorder this one, as there's nothing
                                      "-1" = "Didn't Vote",
                                      "1" = "Hillary Clinton", #substantively meaningful about Hillary being "1" and Marco being "7"
                                      "2" = "Bernie Sanders",
                                      "3" = "Another Democrat",
                                      "4" = "Donald Trump",
                                      "5" = "Ted Cruz",
                                      "6" = "John Kasich",
                                      "7" = "Marco Rubio",
                                      "8" = "Another Republican"
  ))%>% # Running these recode() functions will give us a warning that unreplaced values have been treated as NA. For our purposes, this is fine, we wanted these to be NA, anyway
  mutate(pre_self_ideo_7_num = as.numeric(pre_self_ideo_7),
         pre_self_ideo_7 = recode(pre_self_ideo_7, 
                        "1" = "Extremely Liberal", 
                        "2" = "Liberal", 
                        "3" = "Somewhat Liberal", 
                        "4" = "Moderate", 
                        "5" = "Somewhat Conservative", 
                        "6" = "Conservative", 
                        "7" = "Extremely Conservative"
                        ),
         pre_self_ideo_7 = reorder(pre_self_ideo_7, pre_self_ideo_7_num))%>%
  mutate(dem_self_ideo_dif = sqrt((as.numeric(pre_self_ideo_7) - as.numeric(pre_dem_ideo_7))^2))%>% #creates a variable showing the difference in ideology assessments per party.
  mutate(parties_therm_dif = sqrt((pre_therm_dem - pre_therm_rep)^2))%>% #creates a variable showing the difference in thermometer ratings for each party
  mutate(parties_ideo_dif = sqrt((pre_rep_ideo_7_num - pre_dem_ideo_7_num)^2))%>% #creates a variable showing the difference in ideology assessments per party.
#  select(-ends_with("_num")) %>%# drop the numeric versions of the factors used for reordering above
  #This creates a new variable reflecting the absolute value of the difference between R's assessment of Rep and Dem ideology 
glimpse()%>%#
write_rds("data/tidy-2016.rds")%>%
write_csv("data/tidy-2016.csv")
  
#num_test <- data_2016%>%
#  select(pre_pid_3,
#         pre_pid_3_num)%>%
#  glimpse()
#rds <- read_rds("data/tidy-2016.rds")%>%
#  glimpse()
#dem feeling thermometer, pre/post
#activist level

#potential future variables:
##V162265 POST: Most politicians only care about interests of rich and powerful
## V162253x POST: SUMMARY- Need to Evaluate score

#post_party_reg = V162030,
#post_self_ideo = V162171,
#post_dem_house_ideo = V162172,
#post_rep_house_ideo = V162173

