library(tidyverse)
library(ggExtra)
library(ggridges)
theme_set(theme_bw())
#### CDF Time Series Dataframe
tidy_cdf <- read_rds("data/tidy-cdf.rds")%>%
  filter(year >= 1978)%>%
  glimpse()

party_fts <- tidy_cdf%>% # Making a DF of the party-year SD
  filter(pid_3_sort != "Independent" & year != 2002)%>%
  select(year,
         weight,
         pid_3_sort,
         therm_inparty,
         therm_outparty)%>%
  group_by(year, pid_3_sort)%>%
  summarise(mean_in = weighted.mean(therm_inparty, weight, na.rm = TRUE),
            mean_out = weighted.mean(therm_outparty, weight, na.rm = TRUE),
            sd_in = radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE),
            sd_out = radiant.data::weighted.sd(therm_outparty, weight, na.rm = TRUE))%>%
  pivot_longer(mean_in:sd_out, names_to = "group", values_to = "result")%>%
  unite("group", pid_3_sort:group)%>%
  mutate(stat = as.factor(if_else(str_detect(group, "mean"), "mean", "sd")))%>%
  mutate(group = as.factor(recode(group,
                       "Democrat_mean_in" = "Democrat - In Party",
                       "Democrat_mean_out" = "Democrat - Out Party",
                       "Democrat_sd_in" = "Democrat - In Party",
                       "Democrat_sd_out" = "Democrat - Out Party",
                       "Republican_mean_in" = "Republican - In Party",
                       "Republican_mean_out" = "Republican - Out Party",
                       "Republican_sd_in" = "Republican - In Party",
                       "Republican_sd_out" = "Republican - Out Party")))%>%
  glimpse()

#print(party_fts)
### Replication of fig. 1a

cdf_mean <- party_fts%>%
  filter(stat == "mean")%>%
  glimpse()

cdf_sd <- party_fts%>%
  filter(stat == "sd")%>%
  glimpse()

mean_ft <- ggplot(cdf_mean, aes(x = year, y = result)) +
  geom_point(aes(shape = group)) +
  geom_smooth(aes(linetype = group), color = "darkgrey", se=F) + 
  scale_linetype_manual(values = c("Democrat - In Party" = "longdash",
                                   "Democrat - Out Party" = "dotted",
                                   "Republican - In Party" = "solid",
                                   "Republican - Out Party" = "twodash")) +
  scale_shape_manual(values = c("Democrat - In Party" = 3,
                                   "Democrat - Out Party" = 2,
                                   "Republican - In Party" = 16,
                                   "Republican - Out Party" = 19)) +
  #scale_x_continuous(limits = c(1978,2020), breaks = c(0:5)) +
  scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
  scale_y_continuous(limits = c(20,80)) +
  labs(y = "Mean Thermometer Ratings of Partisans",
       x = "Year",
       linetype = " ",
       shape = " ") +
  theme(legend.position = c(0.2, 0.2))
mean_ft

ggsave("fig/mean-ft.png", mean_ft, width = 6, height = 4, units = "in")

ggplot(cdf_sd, aes(x = year, y = result)) +
  geom_point(aes(shape = group)) +
  geom_smooth(aes(linetype = group), color = "darkgrey", se=F) + 
  scale_linetype_manual(values = c("Democrat - In Party" = "longdash",
                                   "Democrat - Out Party" = "dotted",
                                   "Republican - In Party" = "solid",
                                   "Republican - Out Party" = "twodash")) +
  scale_shape_manual(values = c("Democrat - In Party" = 3,
                                "Democrat - Out Party" = 2,
                                "Republican - In Party" = 16,
                                "Republican - Out Party" = 19)) +
  #scale_x_continuous(limits = c(1978,2020), breaks = c(0:5)) +
  scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
#  scale_y_continuous(limits = c(20,80)) +
  labs(y = "Partisan Feeling Thermometer Standard Deviations",
       x = "Year",
       linetype = " ",
       shape = " ") +
  theme(legend.position = c(0.2, 0.2))

ggsave("fig/sd-ft.png", sd_ft, width = 6, height = 4, units = "in")

##ggridges
ridge_df <- tidy_cdf%>%
  filter(pid_3_sort != "Independent")%>%
  glimpse()
  
ggplot(ridge_df, aes(x = therm_inparty, y = year, group = year)) +
  geom_density_ridges() + 
  scale_y_continuous(trans = "reverse", breaks = seq(1978, 2016, by = 4)) +
  scale_x_continuous(limits = c(20,100), breaks = seq(20, 100, by = 5)) +
  facet_wrap(vars(pid_3_sort))
  ####################################
####################################
### GRAVEYARD OF DEPRECATED PLOTS###
### "FEAR YE ALL WHO ENTER HERE!"###
####################################
####################################

#Old avg plot that used a shitty data wrangle


#cdf_avg <- read_rds("data/tidy-cdf.rds")%>% # Making a DF of the party-year SD
#  filter(pid_3 == "Democrat" & year != 2002)%>%
#  group_by(year)%>%
#  summarize(therm_dem_avg = mean(therm_dem, na.rm = TRUE),
#            therm_rep_avg = mean(therm_rep, na.rm = TRUE))%>%
#  pivot_longer(cols = therm_dem_avg:therm_rep_avg)%>%
#  rename(party_ft = name,
#          mean = value)%>%
#   mutate(party_ft = recode(party_ft,
#                            "therm_dem_avg" = "Democrats",
#                            "therm_rep_avg" = "Republicans"))%>%
#   mutate(party_ft = as.factor(party_ft))%>%
#   glimpse()
# 
# 
# avgp <- ggplot(cdf_avg, aes(x = as.numeric(year), y = mean, linetype = party_ft)) +
#   geom_line(size = 1.5) +
#   theme_bw() +
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank()) +
#   ylim(0, 100) +
#   labs(x = "Year",
#        y = "Feeling Thermometer Average") +
#   labs(linetype = "Thermometer Towards:")
# avgp
# 
# ggsave("fig/cdf-avg.png", plot = avgp, width = 8, height = 6, units = "in")


#Data are not linear, inappropriate to use correlation coefficient
#cdf_years<-group_by(cdf, years)%>%
#  summarize(R = cor(cdf$therm_dem, cdf$therm_rep, use = "pairwise.complete.obs"),
#            cor(cdf$therm_dem, cdf$l, use = "pairwise.complete.obs"))


#cor(cdf$therm_dem, cdf$therm_rep, use = "pairwise.complete.obs")

# #### CDF Time Series Dataframe of ideology
# cdf_ideo <- tidy_cdf%>% # Making a DF of the party-year SD
#   filter(pid_3 != "Independent" & year != 2002 & respondent_ideo != "Moderate")%>%
#   rename(pid_2 = "pid_3",
#          ideo_self = "respondent_ideo",
#          ideo_self_num = "respondent_ideo_num")%>%
#   glimpse()
#   select(year,
#          pid_2,
#          ideo_self,
#          ideo_dem,
#          ideo_rep,
#          ideo_self_num)%>%
#   glimpse()
#   mutate(ideo_self = as.numeric(ideo_self))%>% #changes 7 very cons - 1 very lib
#   pivot_longer(ideo_self:ideo_rep, names_to = "ideology_of", values_to = "ideo")%>%
#   mutate(pid_2 = factor(pid_2, levels = c("Democrat", 
#                                           "Republican",
#                                           "Self")))%>% #self only included to match later
#   mutate(ideology_of = recode(ideology_of,
#                              "ideo_self" = "Self",
#                              "ideo_dem" = "Democrat",
#                              "ideo_rep" = "Republican"))%>%
#   group_by(year, pid_2, ideology_of)%>%
#   summarize(mean = mean(ideo, na.rm = TRUE),
#             sd = sd(ideo, na.rm = TRUE),
#             recode_mean = mean((ideo/7)*100, na.rm = TRUE),
#             recode_sd = sd((ideo/7)*100, na.rm = TRUE),
#             n = n())%>%
#   mutate(ideology_of = factor(ideology_of, levels = c("Democrat", 
#                                                       "Republican",
#                                                       "Self")))%>%
#   mutate(in_out_self = factor(if_else(ideology_of == "Self", "Self", if_else(ideology_of == pid_2, "In-Party", "Out-Party")), 
#                               levels = c("In-Party",
#                                          "Out-Party",
#                                           "Self")))%>%
#   select(-ideology_of)%>%
#   glimpse()
#   
# #######
# # Plot of partisans' ideology
# #######
# 
# # SD
# party_evals <- cdf_ideo%>%
#   filter(in_out_self != "Self")%>%
#   glimpse()
# 
# base_party_ideo <- ggplot(party_evals, aes(x = year, color = in_out_self, linetype = pid_2)) +
#   geom_line(size = 1.5) +
#   theme_bw() +
#   scale_color_grey()+
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank()) 
# 
# ideo_sd_plot <- base_party_ideo + 
#   aes(y = sd) +
#   ylim(0, 2.5) +
#   labs(x = "Year",
#        y = "Ideology SD",
#        color = "Evaluation of",
#        linetype = "Respondents' Party",
#        title = "Variation of Evaluations of In/Out-Party Ideologies",
#        subtitle = "Out-Party in Grey, In-Party in Black")
# ideo_sd_plot
# ggsave("fig/parties-ideo-sd.png", plot = ideo_sd_plot, width = 6, height = 4, units = "in")
# 
# #mean of parties
# 
# ideo_mean_plot <- base_party_ideo + 
#   aes(y = mean) +
#   ylim(1, 7) +
#   labs(x = "Year",
#        y = "Ideology Mean",
#        color = "Evaluation of",
#        linetype = "Respondents' Party",
#        title = "Evaluation of Parties' Ideology",
#        subtitle = "Out-Party in Grey, In-Party in Black")
# ideo_mean_plot
# 
# ggsave("fig/parties-ideo-mean.png", plot = ideo_mean_plot, width = 6, height = 4, units = "in")
# 
# 
# ## Self Evals
# 
# self_evals <- cdf_ideo%>%
#   filter(in_out_self == "Self")%>%
#   glimpse()
# 
# #Means
# base_self_ideo <- ggplot(self_evals, aes(x = year, linetype = pid_2)) +
#   geom_line(size = 1.5) +
#   theme_bw() +
#   scale_color_grey()+
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank()) 
# 
# ideo_self_mean_plot <- base_self_ideo + 
#   aes(y = mean) +
#   ylim(1, 7) +
#   labs(x = "Year",
#        y = "Ideology Mean",
#        color = "Evaluation of",
#        linetype = "Respondents' Party",
#        title = "Self Assessed Ideology Over Time") +
#   geom_line(aes(y = ))
# ideo_self_mean_plot
# ggsave("fig/self-ideo-mean.png", plot = ideo_self_mean_plot, width = 6, height = 4, units = "in")
# 
# # SD
# 
# ideo_self_sd_plot <- base_self_ideo + 
#   aes(y = sd) +
#   ylim(0, 3) +
#   labs(x = "Year",
#        y = "Ideology SD",
#        color = "Evaluation of",
#        linetype = "Respondents' Party",
#        title = "Variation in Self Assessed Ideology") +
#   geom_line(aes(y = ))
# ideo_self_sd_plot
# ggsave("fig/self-ideo-sd.png", plot = ideo_self_sd_plot, width = 6, height = 4, units = "in")
# 
# 
# #####
# ## Plot of Partisans' Affect Towards in and outparty
# #####
# 
# base_ft <- ggplot(cdf_fts, aes(x = as.numeric(year), color = in_out, linetype = pid_2)) +
#   geom_line(size = 1.5) +
#   theme_bw() +
#   scale_color_grey()+
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank()) 
# 
# sdp <- base_ft + 
#   aes(y = sd) +
#   ylim(15, 25) +
#   labs(x = "Year",
#        y = "Feeling Thermometer SD",
#        color = "Towards In/Out-Party",
#        linetype = "Respondents' Party",
#        title = "Feeling Thermometer Deviation")
# sdp
# ggsave("fig/cdf-sd.png", plot = sdp, width = 8, height = 6, units = "in")
# 
# avgp <- base_ft + 
#   aes(y = mean) +
#   labs(x = "Year",
#        y = "Feeling Thermometer Mean",
#        color = "Towards In/Out-Party",
#        linetype = "Respondents' Party",
#        title = "Feeling Thermometer Mean")
# avgp
# 
# ggsave("fig/cdf-avg.png", plot = avgp, width = 8, height = 6, units = "in")
# 
# 
# 
# ##########
# cdf_dem <- read_rds("data/tidy-cdf.rds")%>%
#   filter(year != 2002 & pid_3 == "Democrat")%>%
#   glimpse()
# 
# 
# scatter_dem <- ggplot(cdf_dem, aes(therm_dem, therm_rep), alpha = .3) +
#   geom_point() +
#   theme_bw() +
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank()) +
#   facet_wrap(vars(year)) +
# #  geom_smooth(method = "lm")+
#   labs(x = "Warmth Towards Democratic Party",
#        y = "Warmth Towards Republican Party")
# scatter_dem
# 
# ggsave("fig/cdf-scatter-dem.png", plot = scatter_dem, width = 8, height = 6, units = "in")
# #####################
# ####################
# #NPA PLOT
# ##########
# ##########
# #### CDF Standard Deviation NPA
# npa_stats <- read_rds("data/tidy-cdf.rds")%>% # Making a DF of the party-year SD
#   filter(pid_3 == "Democrat" & year != 2002)%>%
#   group_by(year)%>%
#   summarize(SD = sd(parties_therm_dif, na.rm = TRUE),
#             Mean = mean(parties_therm_dif, na.rm = TRUE)
#             )%>%
#   pivot_longer(cols = SD:Mean)%>%
#   glimpse()
# 
# npa_line <- ggplot(npa_stats, aes(x = year, y = value, linetype = name)) +
#   geom_line(size = 1.5) +
#   theme_bw() +
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank()) +
#   ylim(15, 60) +
#   labs(x = "Year",
#        y = "Net Partisan Affect",
#        linetype = "Statistic")
# npa_line
# ggsave("fig/cdf-npa.png", plot = npa_line, width = 8, height = 6, units = "in")
# 
# 
# 
# 
# ##############################
# ################################
# ##############################
# #SCATTER PLOT
# ############
# #################
# cdf_rep <- read_rds("data/tidy-cdf.rds")%>%
#   filter(year != 2002 & pid_3 == "Republican")%>%
#   glimpse()
# scatter_rep <- ggplot(cdf_rep, aes(therm_dem, therm_rep), alpha = .3) +
#   geom_point() +
#   theme_bw() +
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank()) +
#   facet_wrap(vars(year)) +
#   #  geom_smooth(method = "lm")+
#   labs(x = "Democrat Feeling Thermometer",
#        y = "Republican Feeling Thermometer")
# scatter_rep
# ggsave("fig/cdf-scatter-rep.png", plot = scatter_rep, width = 8, height = 6, units = "in")
# 
# 
# #######################
# 
# ########################
# 
# #########################
# 
# #####
# ### THese are the plots for "independent Dems"
# ###
# ###############
# 
# cdf_sd_ind <- read_rds("data/tidy-cdf.rds")%>% # Making a DF of the party-year SD
#   filter(pid_7 == "Independent - Democrat" & year != 2002)%>%
#   group_by(year)%>%
#   summarize(therm_dem_sd = sd(therm_dem, na.rm = TRUE),
#             therm_rep_sd = sd(therm_rep, na.rm = TRUE))%>%
#   pivot_longer(cols = therm_dem_sd:therm_rep_sd)%>%
#   rename(party_ft = name,
#          sd = value)%>%
#   mutate(party_ft = recode(party_ft,
#                            "therm_dem_sd" = "Democrat",
#                            "therm_rep_sd" = "Republican"))%>%
#   mutate(party_ft = as.factor(party_ft))%>%
#   glimpse()
# 
# 
# 
# 
# sdp_ind <- ggplot(cdf_sd_ind, aes(x = as.numeric(year), y = sd, linetype = party_ft)) +
#   geom_line(size = 1.5) +
#   theme_bw() +
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank()) +
#   ylim(15, 25) +
#   labs(x = "Year",
#        y = "Feeling Thermometer SD") +
#   labs(linetype = "Party")
# sdp_ind
# 
# 
# 
# cdf_avg_ind <- read_rds("data/tidy-cdf.rds")%>% # Making a DF of the party-year SD
#   filter(pid_7 == "Independent - Democrat" & year != 2002)%>%
#   group_by(year)%>%
#   summarize(therm_dem_avg = mean(therm_dem, na.rm = TRUE),
#             therm_rep_avg = mean(therm_rep, na.rm = TRUE))%>%
#   pivot_longer(cols = therm_dem_avg:therm_rep_avg)%>%
#   rename(party_ft = name,
#          mean = value)%>%
#   mutate(party_ft = recode(party_ft,
#                            "therm_dem_avg" = "Democrats",
#                            "therm_rep_avg" = "Republicans"))%>%
#   mutate(party_ft = as.factor(party_ft))%>%
#   glimpse()
# 
# 
# avgp_ind <- ggplot(cdf_avg_ind, aes(x = as.numeric(year), y = mean, linetype = party_ft)) +
#   geom_line(size = 1.5) +
#   theme_bw() +
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank()) +
#   ylim(0, 100) +
#   labs(x = "Year",
#        y = "Feeling Thermometer Average") +
#   labs(linetype = "Thermometer Towards:")
# avgp_ind
# 
# ggsave("fig/cdf-avg-ind.png", plot = avgp_ind, width = 8, height = 6, units = "in")
# ggsave("fig/cdf-sd-ind.png", plot = sdp_ind, width = 8, height = 6, units = "in")