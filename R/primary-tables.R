library(tidyverse)
library(kableExtra)
library(haven)

df_primary <- read_rds("data/tidy-primaries.rds")%>%
  filter(primary_vote_choice == c("Hillary Clinton",
                                  "Bernie Sanders",
                                  "Didn't Vote",
                                  "Barack Obama") & 
           pre_pid_3 == "Democrat")%>%
  glimpse()

#pid_3 <- df_2016%>%
#  subset(primary_vote_choice == c("Hillary Clinton", "Bernie Sanders"))%>%
#  group_by(pre_pid_3, primary_vote_choice)%>%
#  summarize(therm_dif = mean(parties_therm_dif, na.rm = TRUE),
#            ideo_dif = mean(parties_ideo_dif, na.rm = TRUE),
#            ideo_self = mean(pre_self_ideo_7_num, na.rm = TRUE),
#            n = n()
#            )%>%
#  glimpse()

#makes a table with those stats

#####
###Not currently being used
#####
#pid_7 <- df_2016%>%
#  subset(primary_vote_choice == c("Hillary Clinton", "Bernie Sanders"))%>%
#  group_by(pre_pid_7, primary_vote_choice)%>%
#  summarize(therm_dif = mean(parties_therm_dif, na.rm = TRUE),
#            ideo_dif = mean(parties_ideo_dif, na.rm = TRUE),
#             n =n())%>%
#  glimpse()

#creates a dataframe showing the stats we want for Bernie and Hillary voters
pid_3 <- df_primary%>%
  group_by(year, primary_vote_choice)%>%
  summarize(therm_dif = mean(net_party_affect, na.rm = TRUE),
            therm_dem = mean(pre_therm_dem, na.rm = TRUE),
            therm_rep = mean(pre_therm_rep, na.rm = TRUE)#,
            #            ideo_dif = mean(as.numeric(dem_self_ideo_dif), na.rm = TRUE),
            #            ideo_dem = mean(as.numeric(pre_dem_ideo_7), na.rm = TRUE),
            #            ideo_self = mean(as.numeric(pre_self_ideo_7), na.rm = TRUE),
            #            n = n()
  )%>%
  glimpse()
#


#makes a table with those stats
names_spaced <- c("Year",
                  "Vote Choice", 
                  "Net Partisan Affect",
                  "Dem Affect",
                  "Rep Affect"#,
                  #                  "Self - Dem Ideology",
                  #                  "Dem Ideology",
                  #                  "Self Ideology"
)

table_2016 <- pid_3 %>%
  kable(format = "latex",
        digits = 2,
        col.names = names_spaced,
        booktabs = TRUE,
        escape = FALSE
  )%>%
  row_spec(0,bold=TRUE)%>%
  column_spec(3:8, width = "3cm")%>%
  kable_styling(full_width = FALSE)

cat(table_2016, file = "fig/table-primaries.tex")
#######




#df_subzero <- read_rds("data/tidy-primaries.rds")%>%
# filter(pre_pid_3 == "Democrat")%>%
#  filter(primary_vote_choice == c("Hillary Clinton", "Bernie Sanders", "Didn't Vote", "Barack Obama") & pre_pid_3 == "Democrat"& pre_therm_dem <= 50)%>%
#  glimpse()

#pid_3_sz <- df_subzero%>%
#  group_by(year, primary_vote_choice)%>%
#  summarize(therm_dif = mean(net_party_affect, na.rm = TRUE),
#            therm_dem = mean(pre_therm_dem, na.rm = TRUE),
#            therm_rep = mean(pre_therm_rep, na.rm = TRUE),
#            ideo_dif = mean(as.numeric(dem_self_ideo_dif), na.rm = TRUE),
#            ideo_self = mean(as.numeric(pre_self_ideo_7), na.rm = TRUE),
#            n = n()
#  )%>%
#  glimpse()

#makes a table with those stats
#table_2016 <- pid_3_sz %>%
# kable(format = "latex",
#       digits = 2,
#       col.names = c("Year",
#                     "Vote Choice", 
#                     "NPA",
#                    "Dem Affect",
#                     "Rep Affect",
#                     "Self - Dem Ideology",
#                  "Self Ideology")
# )%>%
# kable_styling(full_width = FALSE)

#cat(table_2016, file = "fig/table-subzero.tex")
#df_NA <- df_primary%>%
#  group_by(year, primary_vote_choice)%>%
#  summarize(ft_dk = sum(ft_dk),
#  ideo_dk = sum(ideo_7_dk),
#            n = n())%>%
#  glimpse()

#table_NA <- df_NA %>%
#  kable(format = "latex",
#  col.names = c("Year",
#                      "Primary Candidate",
#                      "Don't Know, Net Party Affect",
#                      "Don't Know, Self Ideology",
#                      "n"))%>%
#  kable_styling(full_width = FALSE)


#cat(table_NA, file = "fig/table-primaries-NA.tex")


#view(pid_3)
#view(exp)