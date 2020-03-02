library(tidyverse)
library(haven)
library(stringr)
library(rio)
df2016 <- read_rds("data/tidy-2016.rds")%>% #loads pre-tidied 2016 df
  select(primary_vote_choice, # Pulling out the variables we just renamed from the massive dataset
         pre_therm_dem,
         pre_therm_rep,                                                                                                       
         pre_self_ideo_7,
#         pre_pid_lean,
         pre_pid_3,
         ft_dk,
         pid_3_np_other,
         ideo_7_dk,
         pre_dem_ideo_7,
         dem_ideo_dk,
         dem_self_ideo_dif
  )%>%
  mutate(year = "2016")%>%
  glimpse()


#
##################
#################



df2012 <- import("data/raw/anes_timeseries_2012.zip", which = "anes_timeseries_2012_rawdata.txt")%>%
  rename(primary_vote_choice = prevote_primv,
         pre_therm_dem = ft_dem, #1-100, 100 warmest. -99: NA
         pre_therm_rep = ft_rep, #1-100, 100 warmest. -99: NA
         pre_self_ideo_7 = libcpre_self, #1-7: Lib-Con
#         pre_pid_lean = pid_lean, # Does R think of themeselves as a Dem, Rep, Ind or what? We are using this in addition to registration because we are interested in people's conceptions of party This is coded 1: strong-dem, 2: weak-dem 3: ind-dem, 4: ind, 5: ind-rep, 6: weak-rep, 7: strong-rep -8 DK, -9 NA
         pre_pid_3 = pid_self #Party ID: Does R think of self as Dem, Rep, Ind or what 0: no pref, 1. Democrat, 2. Republican, 3. Independent
  )%>%
  select(primary_vote_choice, # Pulling out the variables we just renamed from the massive dataset
         pre_therm_dem,
         pre_therm_rep,
         pre_self_ideo_7,
#         pre_pid_lean,
         pre_pid_3
  )%>%
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
  mutate(ideo_7_dk = (pre_self_ideo_7 == c(-8, -2)))%>% #this line creates a dummy variable (ideo_7_dk) to indicate if the NA value for R ideo is due to a "don't know" or "don't recognize" response, rather than a "refuse"
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
         pre_self_ideo_7 = reorder(pre_self_ideo_7, pre_self_ideo_7_num))%>%#this line creates a dummy variable (ideo_7_dk) to indicate if the NA value for R ideo is due to a "don't know" or "don't recognize" response, rather than a "refuse"
  
  mutate(pre_self_ideo_7 = na_if(pre_self_ideo_7, -9),
         pre_self_ideo_7 = na_if(pre_self_ideo_7, -8),
         pre_self_ideo_7 = na_if(pre_self_ideo_7, -2))%>%
#  subset(pre_therm_dem >= 0)%>%
  mutate(ft_dk = ((pre_therm_dem == -8) | (pre_therm_rep == -8) & (pre_therm_dem == -8)) | ((pre_therm_dem == -6) | (pre_therm_rep == -6) & (pre_therm_dem == -6)))%>% #this line creates a dummy variable (ft_dk) to indicate if the NA value for ft is due to a "don't know" or "don't recognize" response, rather than a "refuse"
  mutate(year = "2012")%>%
  glimpse()



###########################
###########################



df2008 <- import("data/raw/anes_timeseries_2008.zip", which = "anes_timeseries_2008_rawdata.txt")%>%
  rename(primary_vote_choice = V083077a,
         pre_therm_dem = V083044a, #1-100, 100 warmest. -99: NA
         pre_therm_rep = V083044b, #1-100, 100 warmest. -99: NA
         pre_self_ideo_7 = V083069, #1-7: Lib-Con
#         pre_pid_lean = V083098b, # Does R think of themeselves as a Dem, Rep, Ind or what? We are using this in addition to registration because we are interested in people's conceptions of party This is coded 1: strong-dem, 2: weak-dem 3: ind-dem, 4: ind, 5: ind-rep, 6: weak-rep, 7: strong-rep -8 DK, -9 NA
         pre_pid_3 = V083097, #Party ID: Does R think of self as Dem, Rep, Ind or what 0: no pref, 1. Democrat, 2. Republican, 3. Independent
         pre_dem_ideo_7 = V083071a
  )%>%
  select(primary_vote_choice, # Pulling out the variables we just renamed from the massive dataset
         pre_therm_dem,
         pre_therm_rep,
         pre_self_ideo_7,
#         pre_pid_lean,
         pre_pid_3,
         pre_dem_ideo_7

  )%>%
  mutate(dem_ideo_dk = (pre_dem_ideo_7 == -8))%>%
  mutate(pre_dem_ideo_7 = na_if(pre_dem_ideo_7, -9),
         pre_dem_ideo_7 = na_if(pre_dem_ideo_7, -8))%>%
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
  mutate(ideo_7_dk = (pre_self_ideo_7 == c(-8, -7)))%>%
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
         pre_self_ideo_7 = reorder(pre_self_ideo_7, pre_self_ideo_7_num))%>%#this line creates a dummy variable (ideo_7_dk) to indicate if the NA value for R ideo is due to a "don't know" or "don't recognize" response, rather than a "refuse"
  mutate(pre_self_ideo_7 = na_if(pre_self_ideo_7, -9),
         pre_self_ideo_7 = na_if(pre_self_ideo_7, -8),
         pre_self_ideo_7 = na_if(pre_self_ideo_7, -7))%>%
  mutate(pid_3_np_other = (pre_pid_3 == c(-8, 4)))%>% #Creates a dummy variable which is TRUE if R didn't know or preferred a third party. FALSE if refused
  mutate(pre_pid_3_num = as.numeric(pre_pid_3),
         pre_pid_3_num = recode(pre_pid_3,  # The ANES (My cruel mistress), codes Reps as "2" and Ind as "3", this flips them so the order makes more sense
                                "0" = "0",
                                "1" = "1",
                                "3" = "2",
                                "2" = "3"),# Recoding party_id as a factor, making sure to order it in a substantive way.
         pre_pid_3 = recode(pre_pid_3, 
                            "5" = "No Preference", 
                            "1" = "Democrat", 
                            "2" = "Republican", 
                            "3" = "Independent"
         ),
         pre_pid_3 = reorder(pre_pid_3, pre_pid_3_num))%>%
  mutate(primary_vote_choice = recode(primary_vote_choice, # Unlike the factor vectors above, I'm not reorder this one, as there's nothing substantive about it
                                      "-1" = "Didn't Vote",
                                      "1" = "Joe Biden", 
                                      "2" = "Hillary Clinton",
                                      "4" = "John Edwards",
                                      "5" = "Rudy Giuliani",
                                      "7" = "Mike Huckabee",
                                      "11" = "John McCain",
                                      "12" = "Barack Obama",
                                      "13" = "Ron Paul",
                                      "14" = "Bill Richardson",
                                      "15" = "Mitt Romney",
                                      "17" = "Fred Thompson",
                                      "30" = "Someone Else" #The vast majority of NAs are "Didn't votes"
  ))%>%# Running these recode() functions will give us a warning that unreplaced values have been treated as NA. For our purposes, this is fine, we wanted these to be NA, anyway
  mutate(ft_dk = ((pre_therm_dem == -8) | (pre_therm_rep == -8) & (pre_therm_dem == -8)) | ((pre_therm_dem == -6) | (pre_therm_rep == -6) & (pre_therm_dem == -6)))%>% #this line creates a dummy variable (ft_dk) to indicate if the NA value for ft is due to a "don't know" or "don't recognize" response, rather than a "refuse"
  mutate(year = "2008")%>%
  mutate(dem_self_ideo_dif = sqrt((as.numeric(pre_self_ideo_7) - as.numeric(pre_dem_ideo_7))^2))%>% #creates a variable showing the difference in ideology assessments per party.
  select(-ends_with("_num")) %>%
  mutate(pre_therm_dem = na_if(pre_therm_dem, -8))%>%
  mutate(pre_therm_rep = na_if(pre_therm_rep, -8))%>%
  glimpse()

####
####

primaries <- full_join(df2016, df2008)%>%
  mutate(primary_vote_choice = as_factor(primary_vote_choice))%>%
  mutate(net_party_affect = sqrt((pre_therm_dem - pre_therm_rep)^2))%>% ##Should you use Abvalue for net party affect?
  glimpse()%>%
  write_rds("data/tidy-primaries.rds")%>%
  write_csv("data/tidy-primaries.csv")

##Have not added df2012, since there was no substantial dem primary in 2012