library(tidyverse)
library(ggExtra)
library(ggridges)
library(goji)
theme_set(theme_minimal())
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

n_df <- tidy_cdf%>% # Making a DF of the party-year SD
  filter(pid_3_sort != "Independent" & year != 2002)%>%
  select(year,
         weight,
         pid_3_sort,
         therm_inparty,
         therm_outparty)%>%
  group_by(year, pid_3_sort)%>%
  summarise(n(),
            mean_in = weighted.mean(therm_inparty, weight, na.rm = TRUE),
            mean_out = weighted.mean(therm_outparty, weight, na.rm = TRUE),
            sd_in = radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE),
            sd_out = radiant.data::weighted.sd(therm_outparty, weight, na.rm = TRUE))%>%
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
  scale_y_continuous(breaks = seq(20, 80, by = 5), limits = c(20,80)) +
  labs(y = "Mean Thermometer Ratings of Partisans",
       x = "Year",
       linetype = " ",
       shape = " ") +
  theme(legend.position = c(0.2, 0.2))
mean_ft

ggsave("fig/cdf-mean.png", mean_ft, width = 6, height = 4, units = "in")

sd_ft <- ggplot(cdf_sd, aes(x = year, y = result)) +
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
  scale_y_continuous(limits = c(10,25)) +
  labs(y = "Partisan Feeling Thermometer Standard Deviations",
       x = "Year",
       linetype = " ",
       shape = " ") +
  theme(legend.position = c(0.2, 0.2))
sd_ft

ggsave("fig/cdf-sd.png", sd_ft, width = 8, height = 6, units = "in")
#####
## Not sure if things in this section will end up in a manuscript, just trying to get a better idea of the data.
#####
##ggridges
ridge_df <- tidy_cdf%>%
  filter(pid_3_sort != "Independent")%>%
  glimpse()
  
ggplot(ridge_df, aes(x = therm_inparty, y = year, group = year, alpha=.5)) +
#  geom_ridgeline() + 
  geom_density_ridges(rel_min_height = 0.015) +
  scale_y_continuous(trans = "reverse", breaks = seq(1978, 2016, by = 4)) +
#  scale_x_continuous(limits = c(20,100), breaks = seq(20, 100, by = 5)) +
#  scale_x_continuous(limits = c(.2,1), breaks = seq(.2, 1, by = .1)) +
  facet_wrap(vars(pid_3_sort))

ggplot(ridge_df, aes(x = year, y = therm_inparty)) +
  geom_point(aes(alpha=.1), position="jitter") +
  geom_smooth(se=TRUE) +
  facet_grid(rows = vars(pid_3_sort))

######
### NPA Example
######

npa_parties_df <- tidy_cdf%>% # Making a DF of the party-year SD
  filter(pid_3_sort != "Independent" & year != 2002)%>%
  select(year,
         weight,
         pid_3_sort,
         npa_party)%>%
  group_by(year, pid_3_sort)%>%
  summarise(mean_npa = weighted.mean(npa_party, weight, na.rm = TRUE),
            sd_npa = radiant.data::weighted.sd(npa_party, weight, na.rm = TRUE))%>%
  pivot_longer(mean_npa:sd_npa, names_to = "group", values_to = "result")%>%
  unite("group", pid_3_sort:group)%>%
  mutate(stat = as.factor(if_else(str_detect(group, "mean"), "mean", "sd")))%>%
  mutate(group = as.factor(recode(group,
                                  "Democrat_mean_npa" = "Democrat - Mean",
                                  "Democrat_sd_npa" = "Democrat - SD",
                                  "Republican_mean_npa" = "Republican - Mean",
                                  "Republican_sd_npa" = "Republican - SD")))%>%
  glimpse()

cdf_npa <- ggplot(npa_parties_df, aes(x = year, y = result)) +
  geom_point(aes(shape = group)) +
  geom_smooth(aes(linetype = group), color = "darkgrey", se=F) + 
  #scale_x_continuous(limits = c(1978,2020), breaks = c(0:5)) +
  scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
  scale_y_continuous(breaks = seq(20, 60, by = 5), limits = c(20,60)) +
  labs(y = "Mean Net Partisan Affect",
       x = "Year",
       linetype = " ",
       shape = " ") +
  theme(legend.position = c(0.2, 0.8))
cdf_npa
ggsave("fig/cdf-npa.png", cdf_npa, width = 6, height = 4, units = "in")

#########
### Partisans Below Median
########

# spatstat::weighted.median(tidy_cdf$therm_inparty, weight, na.rm = TRUE)
# weighted.mean(tidy_cdf$therm_inparty, tidy_cdf$weight, na.rm = TRUE)

######################################################
### Party Medians (one for Dems and REps each)
###

# These make it easy to get party medians without too much pivoting
reps_df <- tidy_cdf%>%
  filter(pid_3_sort == "Republican")
dems_df <- tidy_cdf%>%
  filter(pid_3_sort == "Democrat")

below_med_party <- tidy_cdf%>%
  select(year,
         weight,
         pid_3_sort,
         therm_inparty)%>%
  filter(pid_3_sort != "Independent" & year != 2002)%>%
  mutate(median_in = if_else(pid_3_sort == "Republican", 
                             spatstat::weighted.median(reps_df$therm_inparty, reps_df$weight, na.rm = TRUE),
                             spatstat::weighted.median(dems_df$therm_inparty, dems_df$weight, na.rm = TRUE)))%>%
  mutate(below_med_dum = if_else(therm_inparty < median_in, 1, 0))%>%
  glimpse()

below_med_party_prop <- below_med_party%>%
  group_by(year, pid_3_sort)%>%
  summarise(prop_below = weighted.mean(below_med_dum, weight, na.rm = TRUE),
            se_below = diagis::weighted_se(below_med_dum, weight, na.rm = TRUE))%>%
  glimpse()

cdf_below_party_meds <- ggplot(below_med_party_prop, aes(x = year, y = prop_below)) +
  geom_point(aes(shape = pid_3_sort, size = 1, color = pid_3_sort)) +
  geom_errorbar(aes(ymin = prop_below - se_below, ymax = prop_below + se_below, width = .2)) +
  geom_smooth(aes(linetype = pid_3_sort, color = pid_3_sort), span = .3, se = FALSE) +
  scale_color_manual(values = c("Democrat" = "dodgerblue3",
                                "Republican" = "firebrick3")) +
  theme(legend.position = c(0.2, 0.8)) +
  guides(size = FALSE)
cdf_below_party_meds
ggsave("fig/cdf-below-parties.png", cdf_below_party_meds, width = 6, height = 4, units = "in")

#####################################################
### Single Median
###

below_mct <- tidy_cdf%>% # MCT = Measure of Central Tendency
  select(year,
         weight,
         pid_3_sort,
         therm_inparty)%>%
  filter(pid_3_sort != "Independent" & year != 2002)%>%
  mutate(below_mean_dum = if_else(therm_inparty < weighted.mean(therm_inparty, weight, na.rm = TRUE), 1, 0),
         below_mean_sd_dum = if_else(therm_inparty < weighted.mean(therm_inparty, weight, na.rm = TRUE) - radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE), 1, 0),
         below_med_dum = if_else(therm_inparty < spatstat::weighted.median(therm_inparty, weight, na.rm = TRUE), 1, 0),
         below_med_sd_dum = if_else(therm_inparty < spatstat::weighted.median(therm_inparty, weight, na.rm = TRUE) - radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE), 1, 0))%>%
  glimpse()

below_mct_prop <- below_mct%>%
  group_by(year, pid_3_sort)%>%
  summarise(prop_mean_below = weighted.mean(below_mean_dum, weight, na.rm = TRUE),
            prop_mean_sd_below = weighted.mean(below_mean_sd_dum, weight, na.rm = TRUE),
            prop_med_below = weighted.mean(below_med_dum, weight, na.rm = TRUE), #prop_below is those below the MCT, prop_sd_below is those one SD below med
            prop_med_below_sd = weighted.mean(below_med_sd_dum, weight, na.rm = TRUE),
            se_mean_below = diagis::weighted_se(below_mean_dum, weight, na.rm = TRUE),
            se_mean_sd_below = diagis::weighted_se(below_mean_sd_dum, weight, na.rm = TRUE),
            se_med_below = diagis::weighted_se(below_med_dum, weight, na.rm = TRUE),
            se_med_sd_below = diagis::weighted_se(below_med_sd_dum, weight, na.rm = TRUE))%>%
  glimpse()

cdf_med_below <- ggplot(below_mct_prop, aes(x = year, y = prop_med_below)) +
  geom_point(aes(shape = pid_3_sort, size = 1, color = pid_3_sort)) +
  geom_errorbar(aes(ymin = prop_med_below - se_med_below, ymax = prop_med_below + se_med_below, width = .2)) +
  geom_smooth(aes(linetype = pid_3_sort, color = pid_3_sort), span = .3, se = FALSE) +
  scale_color_manual(values = c("Democrat" = "dodgerblue3",
                                "Republican" = "firebrick3")) +
  theme(legend.position = c(0.1, 0.85)) +
  guides(size = FALSE) +
  labs(x = "Year",
       y = "Proportion of Partisans Below Median In-Party FT",
       color = "Party ID",
       linetype = "Party ID",
       shape = "Party ID")
cdf_med_below
ggsave("fig/cdf-below-med.png", cdf_med_below, width = 6, height = 4, units = "in")


cdf_below_mean_sd <- ggplot(below_mct_prop, aes(x = year, y = prop_mean_sd_below)) +
  geom_point(aes(shape = pid_3_sort, size = 1, color = pid_3_sort)) +
  geom_errorbar(aes(ymin = prop_mean_sd_below - se_mean_sd_below, ymax = prop_mean_sd_below + se_mean_sd_below, width = .2)) +
  geom_smooth(aes(linetype = pid_3_sort, color = pid_3_sort), span = .3, se = FALSE) +
  scale_color_manual(values = c("Democrat" = "dodgerblue3",
                                "Republican" = "firebrick3")) +
  theme(legend.position = c(0.1, 0.85)) +
  guides(size = FALSE) +
  labs(x = "Year",
       y = "Proportion of Partisans < 1 SD Below Mean",
       color = "Party ID",
       linetype = "Party ID",
       shape = "Party ID")
cdf_below_mean_sd

ggsave("fig/cdf-below-mean-sd.png", cdf_below_mean_sd, width = 6, height = 4, units = "in")

test_df <- read_rds("data/tidy-cdf.rds")%>%
  filter(year >= 1978)%>%
  select(year,
         weight,
         pid_3,
         pid_3_sort,
         pid_7)%>%
  mutate(partisan_ns = if_else(pid_3 == "Republican" | pid_3 == "Democrat", 1, 0))%>%
  mutate(partisan_s = if_else(pid_3_sort == "Republican" | pid_3_sort == "Democrat", 1, 0))%>%
  group_by(year)%>%
  summarize(prop_part_s = weighted.mean(partisan_s, weight, na.rm = TRUE),
            prop_part_ns = weighted.mean(partisan_ns, weight, na.rm = TRUE))%>%
  glimpse()

## Have FTs of cold partisans changed?
# below_med_fts <- below_med%>%
#   filter(below_med_dum == 1)%>%
#   group_by(year, pid_3_sort)%>%
#   summarise(mean_in = weighted.mean(therm_inparty, weight, na.rm = TRUE))%>%
#   glimpse()
# 
# mean_ft <- ggplot(below_med_fts, aes(x = year, y = mean_in)) +
#   geom_point(aes(shape = pid_3_sort)) +
#   geom_smooth(aes(linetype = pid_3_sort), color = "darkgrey", se=F) + 
# #   scale_linetype_manual(values = c("Democrat - In Party" = "longdash",
# #                                    "Republican - In Party" = "solid")) +
# #   scale_shape_manual(values = c("Democrat - In Party" = 3,
# #                                 "Republican - In Party" = 16)) +
# #   #scale_x_continuous(limits = c(1978,2020), breaks = c(0:5)) +
# #   scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
#   scale_y_continuous(breaks = seq(40, 70, by = 5), limits = c(40,70)) +
# #   labs(y = "Mean Thermometer Ratings of Partisans",
# #        x = "Year",
# #        linetype = " ",
# #        shape = " ") +
#   theme(legend.position = c(0.2, 0.2))
# mean_ft

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