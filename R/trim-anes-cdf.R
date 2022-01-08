library(tidyverse)
library(haven)
library(sjlabelled)

######                                                                                                #########
# This Script extracts only the relevant variables from the raw cdf file, then writes them to an rds          #
# This is done to avoid the lengthy rio::import() from a zip file every time the wrangling dplyr pipe is run  #
#codebook available here:                                                                                     #
#https://electionstudies.org/wp-content/uploads/2018/12/anes_timeseries_cdf_codebook_var.pdf                  #
######                                                                                                #########

cdf_raw_trim <- rio::import("data/raw/anes/anes_timeseries_cdf_stata_20211118.zip", which = "anes_timeseries_cdf_stata_20211118.dta")%>% #Imports the .dta file from the .zip file
  filter(VCF0004 >= 1964)%>%
select(VCF0004,
       VCF0006, #unique case ID
       VCF0009z, #weight
       VCF0114,
       VCF0301,
       VCF0303,
       VCF0305,
       VCF0311,
       VCF0312,
       VCF0803,
       VCF0218,
       VCF0224,
       VCF0723,
       VCF0503,
       VCF0504,
       VCF9265,
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
       VCF9036, #know_sen 1-2(correct), 3-4(wrong), 7-9NA
       VCF0104,
       VCF0212, #FT Conservatives
       VCF0211, # FT Liberals
       VCF0201, #FT Democrats (old)
       VCF0202, #FT Republicans (old)
       VCF0203, #FT Protestants
       VCF0204, #FT Catholics
       VCF0206, #FT Blacks
       VCF0207, #FT Whites
       VCF0128, # Regligions preference. 1 protestant, 2 catholic, 3 jewish, 4 other/none/dk, 0 na
       VCF0604, # Trust in Gov to do what's right 1, never, 2, some of time, 3 most of time, 4 almost always, 9, dk
       VCF0605, # Gov run for a few interests (1) or benefit of all (2), 9 DK, 0 NA
       VCF0609, # Officials care what people like R think 1 agree, 2 disagree, 3 neither 9/0 dk/na
       VCF9222, # are things in this country on the right track (1), or have things gone off on wrong track (2), -8/-9 DK/NA
       VCF9227, # (1) Larger (2) same, (3) smaller than 20 years ago
       VCF0702,
       #Behavior items
       VCF0716, # Straight (2), Split (1) 0dk/na ticket voting
       VCF0717, #R influence voters? 1 no (1) 2 yes (2) 0na0 ---Same coding
       VCF0718, # attend political meetings/rallies during campaign?
       VCF0719, # Work for party or candidate during campaign?
       VCF0720, # Display candidate Button/sticker
       VCF0721, # Donate money to party or Candidate?
       VCF0724, # watch tb programs about campaigns
       VCF0729, # Correctly identifies party with house majority in washington pre election
       VCF0730, # correctly identifies house majority after election
       VCF0731, # Do you ever discuss politics with Family and Friends? 1 yes, 2 no 8dk
       VCF0733, # How often do you discuss politics 0 - never, 1-7 days 9dkna
       VCF0736, # Party voted for for house 1 Dem, 5, Rep, 7 other
       VCF0748, # On or before election day? 1 on , 2 before 9na
       VCF0705, # Vote for Pres 1 Dem, 2 Rep, 3 other, 0 DK/NA
       VCF0707, # Vote for house (no "other") 1 dem, 2 rep, 0 NA
       VCF0708, # Vote for senator 1 Dem, 2 Rep, 0 NA
       VCF0101,
       )%>%
glimpse()%>%
  write_rds("data/raw/cdf-raw-trim.rds")%>%
  write_csv("data/raw/cdf-raw-trim.csv")
