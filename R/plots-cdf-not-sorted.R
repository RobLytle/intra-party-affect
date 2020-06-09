
library(tidyverse)
library(ggExtra)
library(ggridges)
library(goji)
theme_set(theme_minimal())
#### CDF Time Series Dataframe
tidy_cdf_ns <- read_rds("data/tidy-cdf.rds")%>%
  filter(year >= 1978)%>%
  glimpse()

party_fts_ns <- tidy_cdf_ns%>% # Making a DF of the party-year SD
  filter(pid_3 != "Independent" & year != 2002)%>%
  select(year,
         weight,
         pid_3,
         therm_inparty,
         therm_outparty,
         pid_7)%>%
  group_by(year, pid_3)%>%
  summarise(mean_in = weighted.mean(therm_inparty, weight, na.rm = TRUE),
            mean_out = weighted.mean(therm_outparty, weight, na.rm = TRUE),
            sd_in = radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE),
            sd_out = radiant.data::weighted.sd(therm_outparty, weight, na.rm = TRUE))%>%
  pivot_longer(mean_in:sd_out, names_to = "group", values_to = "result")%>%
  unite("group", pid_3:group)%>%
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

n_df_ns <- tidy_cdf_ns%>% # Making a DF of the party-year SD
  filter(pid_3 != "Independent" & year != 2002)%>%
  select(year,
         weight,
         pid_3,
         therm_inparty,
         therm_outparty)%>%
  group_by(year, pid_3)%>%
  summarise(n(),
            mean_in = weighted.mean(therm_inparty, weight, na.rm = TRUE),
            mean_out = weighted.mean(therm_outparty, weight, na.rm = TRUE),
            sd_in = radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE),
            sd_out = radiant.data::weighted.sd(therm_outparty, weight, na.rm = TRUE))%>%
  glimpse()
  
#print(party_fts_ns)
### Replication of fig. 1a

cdf_mean_ns <- party_fts_ns%>%
  filter(stat == "mean")%>%
  glimpse()

cdf_sd_ns <- party_fts_ns%>%
  filter(group == "Democrat - In Party" | group == "Republican - In Party")%>%
  filter(stat == "sd")%>%
  glimpse()

mean_ft_ns <- ggplot(cdf_mean_ns, aes(x = year, y = result)) +
  geom_point(aes(shape = group)) +
#  geom_smooth(aes(linetype = group, color = group), span = .3, se=F) + 
  geom_line(aes(linetype = group, color = group), size = 1) + 
  scale_linetype_manual(values = c("Democrat - In Party" = "longdash",
                                   "Democrat - Out Party" = "dotted",
                                   "Republican - In Party" = "solid",
                                   "Republican - Out Party" = "twodash")) +
  scale_shape_manual(values = c("Democrat - In Party" = 3,
                                   "Democrat - Out Party" = 2,
                                   "Republican - In Party" = 16,
                                   "Republican - Out Party" = 19)) +
  scale_color_manual(values = c("Democrat - In Party" = "dodgerblue4",
                                   "Democrat - Out Party" = "dodgerblue1",
                                   "Republican - In Party" = "firebrick4",
                                   "Republican - Out Party" = "firebrick1")) +
  #scale_x_continuous(limits = c(1978,2020), breaks = c(0:5)) +
  scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
  scale_y_continuous(breaks = seq(20, 80, by = 5), limits = c(20,80)) +
  labs(y = "Mean Thermometer Ratings of Partisans",
       x = "Year", 
       subtitle = "Includes Leaning Independents",
       linetype = " ",
       color = " ",
       shape = " ") +
  theme(legend.position = c(0.2, 0.2)) +
  guides(size = FALSE)
mean_ft_ns

ggsave("fig/cdf-mean-ns.png", mean_ft_ns, width = 6, height = 4, units = "in")

sd_ft_ns <- ggplot(cdf_sd_ns, aes(x = year, y = result, color = group)) +
#  geom_smooth(aes(linetype = group), span = .3, se=F) + 
  geom_line(aes(linetype = group), size = 1) +
  geom_point(aes(shape = group, size = 1)) +
  scale_linetype_manual(values = c("Democrat - In Party" = "longdash",
                                   "Republican - In Party" = "solid")) +
  scale_shape_manual(values = c("Democrat - In Party" = 3,
                                "Republican - In Party" = 16)) +
  scale_color_manual(values = c("Democrat - In Party" = "dodgerblue3",
                                "Republican - In Party" = "firebrick3")) +
  #scale_x_continuous(limits = c(1978,2020), breaks = c(0:5)) +
  scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
  scale_y_continuous(limits = c(12,25)) +
  labs(y = "Partisan Feeling Thermometer Standard Deviations",
       x = "Year",
       subtitle = "Includes Leaning Independents",
       linetype = " ",
       color = " ",
       shape = " ") +
  theme(legend.position = c(0.2, 0.8))
sd_ft_ns

ggsave("fig/cdf-sd-ns.png", sd_ft_ns, width = 8, height = 6, units = "in")
#####
## Not sure if things in this section will end up in a manuscript, just trying to get a better idea of the data.
#####
##ggridges






ridge_df_partisan_ns <- tidy_cdf_ns%>%
  filter(year != 2002 & pid_3 != is.na(TRUE) & pid_3 != "Independent")%>%
  mutate(year_fct = fct_rev(as.factor(year)))%>%
  glimpse()
  
cdf_ridge_ns <- ggplot(ridge_df_partisan_ns, aes(x = therm_inparty, 
                                        y = year_fct, 
                                        color = "white",
                                        fill = stat(x),
)) +
#  geom_ridgeline() + 
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.02, gradient_lwd = 1) +
  coord_cartesian(clip = "off") +
#  scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.25))) +
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, by = 10)) +
#  scale_fill_viridis_c(name = "In-Party FT", option = "B") +
  scale_fill_gradient(
    low = "blue4",
#    mid = "darkorchid2",
    high = "red1"
#    midpoint = 50
  )+
#  scale_x_continuous(limits = c(.2,1), breaks = seq(.2, 1, by = .1)) +
 #  scale_discrete_manual(aesthetics = "fill", 
 #                        values = c("Democrat" = "dodgerblue3",
 #                                "Republican" = "firebrick3",
 #                               "Independent" = "darkorchid3")) +
  scale_discrete_manual(aesthetics = "color",
                       values = c("white")) +
  geom_vline(xintercept = 50, color = "white") +
  guides(color = FALSE,
         fill = FALSE) +
  labs(title = "In-Party Feeling Thermometers",
       subtitle = "Republicans and Democrats",
       y = "Year",
       x = "Feeling Thermometer",
       caption = " ") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
cdf_ridge_ns
ggsave("fig/cdf-ridge-ns.png", cdf_ridge_ns, width = 8, height = 6, units = "in")

ridge_df_ns <- tidy_cdf_ns%>%
  filter(year != 2002 & pid_3 != is.na(TRUE))%>%
  mutate(year_fct = fct_rev(as.factor(year)))%>%
  glimpse()

# mean ft
ridge_mean_all <- tidy_cdf_ns%>%
  filter(year != 2002)%>%
  mutate(year_fct = fct_rev(as.factor(year)))%>%
  glimpse()

cdf_ridge_all <- ggplot(ridge_mean_all, aes(x = therm_parties_mean, 
                                                 y = year_fct, 
                                                 color = "white",
                                                 fill = stat(x),
)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.02, gradient_lwd = 1) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, by = 10)) +
  scale_fill_gradient(
    low = "blue4",
    high = "red1"
  )+
  scale_discrete_manual(aesthetics = "color",
                        values = c("white")) +
  geom_vline(xintercept = 50, color = "white") +
  guides(color = FALSE,
         fill = FALSE) +
  labs(title = "Mean Partisan Feeling Thermometer",
       subtitle = "All Respondents",
       y = "Year",
       x = "Feeling Thermometer",
       caption = " ") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
cdf_ridge_all
ggsave("fig/cdf-ridge-all.png", cdf_ridge_all, width = 8, height = 6, units = "in")
###########
#### Dissatisfaction with Democ.
########

# Mean of all
ridge_mean_dis <- ridge_mean_all%>%
  filter(year >= 2004)%>%
  glimpse()


cdf_ridge_all_dis <- ggplot(ridge_mean_dis, aes(x = therm_parties_mean, 
                                            y = year_fct, 
                                            color = "white",
                                            fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.02, gradient_lwd = 1) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, by = 10)) +
  scale_fill_gradient(
    low = "blue4",
    high = "red1")+
  scale_discrete_manual(aesthetics = "color",
                        values = c("white")) +
  geom_vline(xintercept = 50, color = "white") +
  guides(color = FALSE,
         fill = FALSE) +
  labs(title = "Mean Partisan Feeling Thermometer",
       subtitle = "All Respondents, by satisfaction with Democracy",
       y = "Year",
       x = "Feeling Thermometer",
       caption = " ") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  facet_grid(rows = vars(dis_democ_qual))
cdf_ridge_all_dis 
ggsave("fig/cdf-ridge-all-dis.png", cdf_ridge_all_dis, width = 8, height = 6, units = "in")





#### Just In-party

ridge_in_dis <- ridge_df_partisan_ns%>%
  filter(year >= 2004)%>%
  glimpse()

ggplot(ridge_in_dis, aes(x = therm_inparty, y = ..density..)) +
  geom_histogram() +
  facet_grid(rows = vars(year), cols = vars(dis_democ_qual))

cdf_ridge_in_dis <- ggplot(ridge_in_dis, aes(x = therm_inparty, 
                                                y = year_fct, 
                                                color = "white",
                                                fill = stat(x))) +
#  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.02, gradient_lwd = 1) +
  geom_density_ridges_gradient(stat = "binline", scale = 3, rel_min_height = 0.02, gradient_lwd = 1) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, by = 10)) +
  scale_fill_gradient(
    low = "blue4",
    high = "red1")+
  scale_discrete_manual(aesthetics = "color",
                        values = c("white")) +
  geom_vline(xintercept = 50, color = "white") +
  guides(color = FALSE,
         fill = FALSE) +
  labs(title = "In-Party Feeling Thermometers",
       subtitle = "Democrats and Republicans, by satisfaction with Democracy",
       y = "Year",
       x = "Feeling Thermometer",
       caption = " ") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  facet_grid(rows = vars(dis_democ_qual))
cdf_ridge_in_dis

ridge_df_ns <- tidy_cdf_ns%>%
  filter(year != 2002 & pid_3 != is.na(TRUE))%>%
  mutate(year_fct = fct_rev(as.factor(year)))%>%
  glimpse()







#by parties
cdf_ridge_ns <- ggplot(ridge_df_ns, aes(x = therm_inparty, y = year_fct, color = pid_3, fill = pid_3, group = year)) +
  #  geom_ridgeline() + 
  geom_density_ridges(rel_min_height = 0.015, alpha = 1) +
  #  scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.25))) +
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, by = 10)) +
  #  scale_x_continuous(limits = c(.2,1), breaks = seq(.2, 1, by = .1)) +
  scale_fill_manual(values = c("Democrat" = "dodgerblue3",
                               "Republican" = "firebrick3",
                               "Independent" = "darkorchid3")) +
  scale_color_manual(values = c("Democrat" = "dodgerblue1",
                                "Republican" = "firebrick1",
                                "Independent" = "darkorchid1")) +
  facet_grid(cols = vars(pid_3)) +
  geom_vline(xintercept = 50, color = "white") +
  guides(color = FALSE,
         fill = FALSE) +
  labs(title = "Democrat and Republican In-Party Feeling Thermometers",
       subtitle = "Compared to Mean Partisan FT of Independents",
       y = "Year",
       x = "Feeling Thermometer",
       caption = "Independents increasingly likely to have cold average party FT, partisans increasingly cold toward in-party") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
cdf_ridge_ns
ggsave("fig/cdf-ridge-split-ns.png", cdf_ridge_ns, width = 8, height = 6, units = "in")

# ggplot(ridge_df_ns, aes(x = year, y = therm_inparty)) +
#   geom_point(aes(alpha=.1), position="jitter") +
#   geom_smooth(se=TRUE) +
#   facet_grid(rows = vars(pid_3))

######
### NPA Example
######

npa_parties_df_ns <- tidy_cdf_ns%>% # Making a DF of the party-year SD
  filter(pid_3 != "Independent" & year != 2002)%>%
  select(year,
         weight,
         pid_3,
         npa_party)%>%
  group_by(year, pid_3)%>%
  summarise(mean_npa = weighted.mean(npa_party, weight, na.rm = TRUE),
            sd_npa = radiant.data::weighted.sd(npa_party, weight, na.rm = TRUE))%>%
  pivot_longer(mean_npa:sd_npa, names_to = "group", values_to = "result")%>%
  unite("group", pid_3:group)%>%
  mutate(stat = as.factor(if_else(str_detect(group, "mean"), "mean", "sd")))%>%
  mutate(group = as.factor(recode(group,
                                  "Democrat_mean_npa" = "Democrat - Mean",
                                  "Democrat_sd_npa" = "Democrat - SD",
                                  "Republican_mean_npa" = "Republican - Mean",
                                  "Republican_sd_npa" = "Republican - SD")))%>%
  glimpse()

cdf_npa_ns <- ggplot(npa_parties_df_ns, aes(x = year, y = result)) +
  geom_point(aes(shape = group)) +
  geom_smooth(aes(linetype = group), color = "darkgrey", se=F) + 
  #scale_x_continuous(limits = c(1978,2020), breaks = c(0:5)) +
  scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
  scale_y_continuous(breaks = seq(20, 60, by = 5), limits = c(20,60)) +
  labs(y = "Mean Net Partisan Affect",
       x = "Year", subtitle = "Includes Leaning Independents",
       linetype = " ",
       shape = " ") +
  theme(legend.position = c(0.2, 0.8))
cdf_npa_ns
ggsave("fig/cdf-npa-ns.png", cdf_npa_ns, width = 6, height = 4, units = "in")

#########
### Partisans Below Median
########

# spatstat::weighted.median(tidy_cdf_ns$therm_inparty, weight, na.rm = TRUE)
# weighted.mean(tidy_cdf_ns$therm_inparty, tidy_cdf_ns$weight, na.rm = TRUE)

######################################################
### Party Medians (one for Dems and REps each)
###

# These make it easy to get party medians without too much pivoting
reps_df_ns <- tidy_cdf_ns%>%
  filter(pid_3 == "Republican")
dems_df_ns <- tidy_cdf_ns%>%
  filter(pid_3 == "Democrat")

below_med_party_ns <- tidy_cdf_ns%>%
  select(year,
         weight,
         pid_3,
         therm_inparty)%>%
  filter(pid_3 != "Independent" & year != 2002)%>%
  mutate(median_in = if_else(pid_3 == "Republican", 
                             spatstat::weighted.median(reps_df_ns$therm_inparty, reps_df_ns$weight, na.rm = TRUE),
                             spatstat::weighted.median(dems_df_ns$therm_inparty, dems_df_ns$weight, na.rm = TRUE)))%>%
  mutate(below_med_dum = if_else(therm_inparty < median_in, 1, 0))%>%
  glimpse()

below_med_party_prop_ns <- below_med_party_ns%>%
  group_by(year, pid_3)%>%
  summarise(prop_below = weighted.mean(below_med_dum, weight, na.rm = TRUE),
            se_below = diagis::weighted_se(below_med_dum, weight, na.rm = TRUE))%>%
  glimpse()

cdf_below_party_meds_ns <- ggplot(below_med_party_prop_ns, aes(x = year, y = prop_below)) +
  geom_point(aes(shape = pid_3, size = 1, color = pid_3)) +
  geom_errorbar(aes(ymin = prop_below - se_below, ymax = prop_below + se_below, width = .2)) +
  geom_smooth(aes(linetype = pid_3, color = pid_3), span = .3, se = FALSE) +
  scale_color_manual(values = c("Democrat" = "dodgerblue3",
                                "Republican" = "firebrick3")) +
  theme(legend.position = c(0.2, 0.8)) +
  guides(size = FALSE)
cdf_below_party_meds_ns
ggsave("fig/cdf-below-parties-ns.png", cdf_below_party_meds_ns, width = 6, height = 4, units = "in")

#####################################################
### Single Median
###

below_mct_ns <- tidy_cdf_ns%>% # MCT = Measure of Central Tendency
  select(year,
         weight,
         pid_3,
         therm_inparty)%>%
  filter(pid_3 != "Independent" & year != 2002)%>%
  mutate(above_80_dum = if_else(therm_inparty > 80, 1, 0),
         above_mean_sd_dum = if_else(therm_inparty > weighted.mean(therm_inparty, weight, na.rm = TRUE) + radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE), 1, 0),
         below_50_dum = if_else(therm_inparty < 50, 1, 0),
         below_mean_dum = if_else(therm_inparty < weighted.mean(therm_inparty, weight, na.rm = TRUE), 1, 0),
         below_mean_sd_dum = if_else(therm_inparty < weighted.mean(therm_inparty, weight, na.rm = TRUE) - radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE), 1, 0),
         below_med_dum = if_else(therm_inparty < spatstat::weighted.median(therm_inparty, weight, na.rm = TRUE), 1, 0),
         below_med_sd_dum = if_else(therm_inparty < spatstat::weighted.median(therm_inparty, weight, na.rm = TRUE) - radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE), 1, 0))%>%
  glimpse()

below_mct_prop_ns <- below_mct_ns%>%
  group_by(year, pid_3)%>%
  summarise(prop_mean_below = weighted.mean(below_mean_dum, weight, na.rm = TRUE),
            prop_mean_sd_below = weighted.mean(below_mean_sd_dum, weight, na.rm = TRUE),
            prop_med_below = weighted.mean(below_med_dum, weight, na.rm = TRUE), #prop_below is those below the MCT, prop_sd_below is those one SD below med
            prop_med_below_sd = weighted.mean(below_med_sd_dum, weight, na.rm = TRUE),
            prop_50_below = weighted.mean(below_50_dum, weight, na.rm = TRUE),
            prop_80_above = weighted.mean(above_80_dum, weight, na.rm = TRUE),
            prop_mean_sd_above = weighted.mean(above_mean_sd_dum, weight, na.rm = TRUE),
            se_mean_below = diagis::weighted_se(below_mean_dum, weight, na.rm = TRUE),
            se_mean_sd_below = diagis::weighted_se(below_mean_sd_dum, weight, na.rm = TRUE),
            se_med_below = diagis::weighted_se(below_med_dum, weight, na.rm = TRUE),
            se_med_sd_below = diagis::weighted_se(below_med_sd_dum, weight, na.rm = TRUE),
            se_50_below = diagis::weighted_se(below_50_dum, weight, na.rm = TRUE),
            se_80_above = diagis::weighted_se(above_80_dum, weight, na.rm = TRUE),
            se_mean_sd_above = diagis::weighted_se(above_mean_sd_dum, weight, na.rm = TRUE)
  )%>%
  glimpse()

cdf_med_below_ns <- ggplot(below_mct_prop_ns, aes(x = year, y = prop_med_below)) +
  geom_point(aes(shape = pid_3, size = 1, color = pid_3)) +
  geom_errorbar(aes(ymin = prop_med_below - se_med_below, ymax = prop_med_below + se_med_below, width = .2)) +
  geom_smooth(aes(linetype = pid_3, color = pid_3), span = .3, se = FALSE) +
  scale_color_manual(values = c("Democrat" = "dodgerblue3",
                                "Republican" = "firebrick3")) +
  theme(legend.position = c(0.1, 0.85)) +
  guides(size = FALSE) +
  labs(x = "Year", subtitle = "Includes Leaning Independents",
       y = "Proportion of Partisans Below Median In-Party FT",
       color = "Party ID",
       linetype = "Party ID",
       shape = "Party ID")
cdf_med_below_ns
ggsave("fig/cdf-below-med-ns.png", cdf_med_below_ns, width = 6, height = 4, units = "in")


cdf_below_mean_sd_ns <- ggplot(below_mct_prop_ns, aes(x = year, y = prop_mean_sd_below)) +
  geom_point(aes(shape = pid_3, size = 1, color = pid_3)) +
  geom_errorbar(aes(ymin = prop_mean_sd_below - se_mean_sd_below, ymax = prop_mean_sd_below + se_mean_sd_below, width = .2)) +
  geom_smooth(aes(linetype = pid_3, color = pid_3), span = .3, se = FALSE) +
  scale_color_manual(values = c("Democrat" = "dodgerblue3",
                                "Republican" = "firebrick3")) +
  theme(legend.position = c(0.1, 0.85)) +
  guides(size = FALSE) +
  labs(x = "Year", 
       title = "Proportion of Partisans < 1 SD Below Mean",
       subtitle = "Includes Leaning Independents",
       y = "Proportion",
       color = "Party ID",
       linetype = "Party ID",
       shape = "Party ID")
cdf_below_mean_sd_ns
ggsave("fig/cdf-below-mean-sd-ns.png", cdf_below_mean_sd_ns, width = 6, height = 4, units = "in")

### Above 80:

cdf_above_80_ns <- ggplot(below_mct_prop_ns, aes(x = year, y = prop_80_above)) +
  geom_point(aes(shape = pid_3, size = 1, color = pid_3)) +
  geom_errorbar(aes(ymin = prop_80_above - se_80_above, ymax = prop_80_above + se_80_above, width = .2)) +
  geom_smooth(aes(linetype = pid_3, color = pid_3), span = .3, se = FALSE) +
  scale_color_manual(values = c("Democrat" = "dodgerblue3",
                                "Republican" = "firebrick3")) +
  theme(legend.position = c(0.1, 0.7)) +
  guides(size = FALSE) +
  labs(x = "Year", 
       subtitle = "Includes Leaning Independents",
       y = "Proportion",
       color = "Party ID",
       linetype = "Party ID",
       shape = "Party ID",
       title = "Proportion of Partisans Below 50 In-Party FT")
cdf_above_80_ns

ggsave("fig/cdf-above-80-ns.png", cdf_above_80_ns, width = 8, height = 6, units = "in")

### Above Mean + SD

cdf_above_mean_sd_ns <- ggplot(below_mct_prop_ns, aes(x = year, y = prop_mean_sd_above)) +
  geom_point(aes(shape = pid_3, size = 1, color = pid_3)) +
  geom_errorbar(aes(ymin = prop_mean_sd_above - se_mean_sd_above, ymax = prop_mean_sd_above + se_mean_sd_above, width = .2)) +
  geom_smooth(aes(linetype = pid_3, color = pid_3), span = .3, se = FALSE) +
  scale_color_manual(values = c("Democrat" = "dodgerblue3",
                                "Republican" = "firebrick3")) +
  theme(legend.position = c(0.1, 0.85)) +
  guides(size = FALSE) +
  labs(x = "Year", 
       title = "Proportion of Partisans < 1 SD above Mean",
       subtitle = "Includes Leaning Independents",
       y = "Proportion",
       color = "Party ID",
       linetype = "Party ID",
       shape = "Party ID")
cdf_above_mean_sd_ns

ggsave("fig/cdf-above-mean-sd-ns.png", cdf_below_mean_sd_ns, width = 6, height = 4, units = "in")


### Below 50:

cdf_below_50_ns <- ggplot(below_mct_prop_ns, aes(x = year, y = prop_50_below)) +
  geom_errorbar(aes(ymin = prop_50_below - se_50_below, ymax = prop_50_below + se_50_below, width = .2)) +
  geom_line(aes(linetype = pid_3, color = pid_3), size = 1) + 
  #  geom_smooth(aes(linetype = pid_3_sort, color = pid_3_sort), span = .3, se = FALSE) +
  geom_point(aes(shape = pid_3, size = 1, color = pid_3)) +
  scale_color_manual(values = c("Democrat" = "dodgerblue3",
                                "Republican" = "firebrick3")) +
  scale_linetype_manual(values = c("Democrat" = "longdash",
                                   "Republican" = "solid")) +
  theme(legend.position = c(0.1, 0.7)) +
  guides(size = FALSE) +
  labs(x = "Year", 
       subtitle = "Includes Leaning Independents",
       y = "Proportion",
       color = "Party ID",
       linetype = "Party ID",
       shape = "Party ID",
       title = "Proportion of Partisans Below 50 In-Party FT") #+
#  facet_wrap(vars(pid_str))
cdf_below_50_ns

ggsave("fig/cdf-below-50-ns.png", cdf_below_50_ns, width = 8, height = 6, units = "in")

# Proportion of partisans

prop_ind_df <- tidy_cdf%>%
  select(year,
         weight,
         pid_str,
         pid_3)%>%
  mutate(ind_dummy = if_else(pid_str == "Independent", 1, 0))%>%
  mutate(strong_dummy = if_else(pid_str == "Strong Partisan", 1, 0))%>%
  group_by(year)%>%
  summarize(prop_ind = weighted.mean(ind_dummy, weight, na.rm = TRUE))%>%
  glimpse()

prop_strong_df <- tidy_cdf%>%
  select(year,
         weight,
         pid_str,
         pid_3)%>%
  mutate(strong_dummy = if_else(pid_str == "Strong Partisan", 1, 0))%>%
  glimpse()
  group_by(year)%>%
  summarize(prop_strong = weighted.mean(strong_dummy, weight, na.rm = TRUE))%>%
  glimpse()




prop_ind <- ggplot(prop_ind_df, aes(x = year, y = prop_ind)) +
#  geom_errorbar(aes(ymin = prop_50_below - se_50_below, ymax = prop_50_below + se_50_below, width = .2)) +
  geom_line(aes()) + 
  #  geom_smooth(aes(linetype = pid_3_sort, color = pid_3_sort), span = .3, se = FALSE) +
 # geom_point(aes(shape = pid_3, size = 1, color = pid_3)) +
#  scale_color_manual(values = c("Democrat" = "dodgerblue3",
#                                "Republican" = "firebrick3")) +
#  scale_linetype_manual(values = c("Democrat" = "longdash",
#                                   "Republican" = "solid")) +
  theme(legend.position = c(0.1, 0.7)) +
  guides(size = FALSE) +
  labs(x = "Year", 
       subtitle = "Includes Leaning Independents",
       y = "Proportion",
       color = "Party ID",
       linetype = "Party ID",
       shape = "Party ID",
       title = "Proportion of Independents") #+
#  facet_wrap(vars(pid_str))
prop_ind



prop_strong <- ggplot(prop_strong_df, aes(x = year, y = prop_strong)) +
  #  geom_errorbar(aes(ymin = prop_50_below - se_50_below, ymax = prop_50_below + se_50_below, width = .2)) +
 # geom_line(aes(linetype = pid_3, color = pid_3), size = 1) + 
  geom_line() + 
  #  geom_smooth(aes(linetype = pid_3_sort, color = pid_3_sort), span = .3, se = FALSE) +
  # geom_point(aes(shape = pid_3, size = 1, color = pid_3)) +
#  scale_color_manual(values = c("Democrat" = "dodgerblue3",
#                                "Republican" = "firebrick3")) +
#  scale_linetype_manual(values = c("Democrat" = "longdash",
#                                   "Republican" = "solid")) +
  theme(legend.position = c(0.1, 0.7)) +
  guides(size = FALSE) +
  labs(x = "Year", 
       title = "Proportion of Strong Partisans",
       y = "Proportion",
       color = "Party ID",
       linetype = "Party ID",
       shape = "Party ID",
       subtitle = " ") #+
#  facet_wrap(vars(pid_str))
prop_strong


prop_dissatisfied <- tidy_cdf_ns%>%
  filter(year >= 2004)%>%
  select(year,
         weight,
         dis_democ_dum,
         pid_3)%>%
  glimpse()
group_by(year)%>%
  summarize(prop_dis = weighted.mean(dis_democ, weight, na.rm = TRUE))%>%
  glimpse()


