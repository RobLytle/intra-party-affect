library(tidyverse)
library(ggExtra)
library(ggridges)
library(gridExtra)
theme_set(theme_minimal())
set.seed(2001)
#### CDF Time Series Dataframe
tidy_2020_df <- read_rds("data/tidy-2020.rds")%>%
  select(pid_3,
         pid_7,
         year,
         therm_inparty,
         therm_outparty,
#         therm_parties_mean,
         weight)%>%
  filter(pid_3 != "Independent")%>%
  glimpse()


cdf_trimmed <- read_rds("data/tidy-cdf.rds")%>%
  filter(year >= 1978)%>%
  select(pid_3,
         pid_7,
         year,
#         race_4cat,
         therm_inparty,
         therm_outparty,
#         therm_parties_mean,
         weight,
         )%>%
  rbind(tidy_2020_df)%>%
  glimpse()

party_fts_ns <- cdf_trimmed%>% # Making a DF of the party-year SD
  filter(pid_3 != "Independent" & year != 2002)%>%
  select(year,
         weight,
         pid_3,
         therm_inparty,
         therm_outparty,
         pid_7,
#         race_4cat
         )%>%
#  rbind(tidy_2020_df)%>%
  group_by(year, 
#           race_4cat, 
           pid_3)%>%
  summarise(mean_in = weighted.mean(therm_inparty, weight, na.rm = TRUE),
            mean_out = weighted.mean(therm_outparty, weight, na.rm = TRUE),
            sd_in = radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE),
            sd_out = radiant.data::weighted.sd(therm_outparty, weight, na.rm = TRUE))%>%
  pivot_longer(mean_in:sd_out, names_to = "subset", values_to = "result")%>%
  unite("subset", pid_3:subset)%>%
  ungroup()%>%
  mutate(stat = as.factor(if_else(str_detect(subset, "mean"), "mean", "sd")))%>%
  mutate(group = recode(subset,
                       "Democrat_mean_in" = "Democrat - In Party",
                       "Democrat_mean_out" = "Democrat - Out Party",
                       "Democrat_sd_in" = "Democrat - In Party",
                       "Democrat_sd_out" = "Democrat - Out Party",
                       "Republican_mean_in" = "Republican - In Party",
                       "Republican_mean_out" = "Republican - Out Party",
                       "Republican_sd_in" = "Republican - In Party",
                       "Republican_sd_out" = "Republican - Out Party"))%>%
  select(-subset)%>%
  glimpse()

n_df_ns <- cdf_trimmed%>% # Making a DF of the party-year SD
  filter(pid_3 != "Independent" & year != 2002)%>%
  select(year,
         weight,
         pid_3,
         therm_inparty,
         therm_outparty)%>%
  group_by(year, pid_3)%>%
  summarise(n = n(),
            mean_in = weighted.mean(therm_inparty, weight, na.rm = TRUE),
            mean_out = weighted.mean(therm_outparty, weight, na.rm = TRUE),
            sd_in = radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE),
            sd_out = radiant.data::weighted.sd(therm_outparty, weight, na.rm = TRUE))%>%
  glimpse()


df_for_sampling <- cdf_trimmed%>% # Making a DF of the party-year SD
  filter(pid_3 != "Independent" & year != 2002)%>%
  select(year,
         weight,
         pid_3,
         therm_inparty,
         therm_outparty,
         pid_7
  )%>%
#  filter(pid_3 == "Democrat" & year == 2020)%>%
glimpse()
#bootstrapping the SE of the SD
boot_sd_in <- data.frame(boot = 1:50)%>%
  group_by(boot)%>%
  do(sample_n(df_for_sampling, 
              nrow(df_for_sampling),
              replace = TRUE))%>% #creating 100 new datasets of equal size to the original
  group_by(boot,
           year,
           pid_3)%>%
  summarise(mean_in = weighted.mean(therm_inparty, weight, na.rm = TRUE),
            mean_out = weighted.mean(therm_outparty, weight, na.rm = TRUE),
            sd_in = radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE),
            sd_out = radiant.data::weighted.sd(therm_outparty, weight, na.rm = TRUE))%>%
  group_by(year,
           pid_3)%>%
  summarise(se_mean_in = sd(mean_in),
            se_mean_out = sd(mean_out),
            se_sd_in = sd(sd_in),
            se_sd_out = sd(sd_out))%>%
  pivot_longer(se_mean_in:se_sd_out, names_to = "group", values_to = "result")%>%
  unite("group", pid_3:group)%>%
  mutate(stat = as.factor(if_else(str_detect(group, "mean"), "se_mean", "se_sd")))%>%
  mutate(group = as.factor(recode(group,
                                  "Democrat_se_mean_in" = "Democrat - In Party",
                                  "Democrat_se_mean_out" = "Democrat - Out Party",
                                  "Democrat_se_sd_in" = "Democrat - In Party",
                                  "Democrat_se_sd_out" = "Democrat - Out Party",
                                  "Republican_se_mean_in" = "Republican - In Party",
                                  "Republican_se_mean_out" = "Republican - Out Party",
                                  "Republican_se_sd_in" = "Republican - In Party",
                                  "Republican_se_sd_out" = "Republican - Out Party")))%>%
  glimpse()

  ggplot(boot_sd_in, aes(x = sd_in)) +
    geom_density() +
    facet_grid(rows = vars(year),
               cols = vars(pid_3))
  
  
party_fts_ns_joined <- rbind(party_fts_ns, boot_sd_in)%>%
#party_fts_ns_joined <- full_join(party_fts_ns, boot_sd_in)%>%
  pivot_wider(names_from = stat,
              values_from = result)%>%
  glimpse()


dodge <- position_dodge(width=0.75)

gg_mean_ft_ns <- ggplot(party_fts_ns_joined, aes(x = year, y = mean)) +
#  geom_smooth(aes(linetype = group, color = group), span = .3, se=F) +
  geom_line(aes(linetype = group, color = group)) +
  geom_point(aes(shape = group, color = group), size = 3) +
  scale_linetype_manual(values = c("Democrat - In Party" = "longdash",
                                   "Democrat - Out Party" = "dotted",
                                   "Republican - In Party" = "solid",
                                   "Republican - Out Party" = "twodash")) +
  scale_shape_manual(values = c("Democrat - In Party" = 17,
                                   "Democrat - Out Party" = 2,
                                   "Republican - In Party" = 16,
                                   "Republican - Out Party" = 1)) +
  scale_color_manual(values = c("Democrat - In Party" = "dodgerblue4",
                                   "Democrat - Out Party" = "dodgerblue1",
                                   "Republican - In Party" = "firebrick4",
                                   "Republican - Out Party" = "firebrick1")) +
  #scale_x_continuous(limits = c(1978,2020), breaks = c(0:5)) +
  scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
  scale_y_continuous(breaks = seq(15, 80, by = 5), limits = c(15,80)) +
  labs(y = "Mean",
       x = "Year",
       title = "Mean Thermometer Ratings of Partisans",
       subtitle = "Includes Leaning Independents",
       linetype = " ",
       color = " ",
       shape = " ") +
  theme(legend.position = c(0.2, 0.2)) +
  guides(size = FALSE
#         shape = guide_legend(override.aes = list(size = 1.5))
         )
gg_mean_ft_ns

ggsave("fig/gg-mean-ns.png", gg_mean_ft_ns, width = 6, height = 4, units = "in")


gg_sd_ft_ns <- party_fts_ns_joined%>%
  filter(!str_detect(group, "Out"))%>%
  ggplot(aes(x = year, y = sd, color = group)) +
#  geom_smooth(aes(linetype = group), span = .3, se=F) + 
  geom_line(aes(linetype = group), size = 1, position = dodge) +
#  geom_errorbar(aes(ymin = sd - 1.96*se_sd, ymax = sd + 1.96*se_sd), width = 2, position = dodge) +
  geom_linerange(aes(ymin = sd - 1.96*se_sd, ymax = sd + 1.96*se_sd, color = group), position = dodge) +
  geom_point(aes(shape = group), position = dodge, size = 3) +
  scale_linetype_manual(values = c("Democrat - In Party" = "longdash",
                                   "Republican - In Party" = "solid")) +
  scale_shape_manual(values = c("Democrat - In Party" = 17,
                                "Republican - In Party" = 16)) +
  scale_color_manual(values = c("Democrat - In Party" = "dodgerblue3",
                                "Republican - In Party" = "firebrick3")) +
  #scale_x_continuous(limits = c(1978,2020), breaks = c(0:5)) +
  scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
#  scale_y_continuous(limits = c(14,25)) +
  labs(y = "Standard Deviations",
       x = "Year",
       title = "Standard Deviation of In-Party Feeling Thermometers",
       subtitle = "Includes Leaning Independents",
       linetype = " ",
       color = " ",
       shape = " ",
       caption = "Bootstrapped 95% CI Given By Vertical Bars") +
  theme(legend.position = c(0.2, 0.8),
        legend.key.width = unit(.1, "npc"),
        legend.key.height = unit(.075, "npc")) +
  guides(size = "none")
gg_sd_ft_ns

ggsave("fig/gg-sd-ns.png", gg_sd_ft_ns, width = 8, height = 6, units = "in")




ridge_df_partisan_ns <- cdf_trimmed%>%
#ridge_df_partisan_ns <- cdf_extended%>%
  filter(year != 2002 & pid_3 != is.na(TRUE) & pid_3 != "Independent")%>%
  mutate(year_fct = fct_rev(as.factor(year)))%>%
  glimpse()
  

ggsave("fig/gg-ridge-ns.png", gg_ridge_ns, width = 8, height = 6, units = "in")


# mean ft
ridge_df_all <- cdf_trimmed%>%
#ridge_df_all <- cdf_extended%>%
  filter(year != 2002)%>%
  mutate(year_fct = fct_rev(as.factor(year)))%>%
  glimpse()

gg_ridge_all <- ggplot(ridge_df_all, aes(x = therm_parties_mean, 
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
  )
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
gg_ridge_all
ggsave("fig/gg-ridge-all.png", gg_ridge_all, width = 8, height = 6, units = "in")


ridge_plots <- grid.arrange(gg_ridge_all,
                            gg_ridge_ns,
                            ncol = 2)
ridge_plots

ggsave("fig/gg-ridge-grid.png", ridge_plots, width = 10, height = 4, units = "in")

###########
#### Dissatisfaction with Democ.
########

# Mean of all
ridge_mean_dis <- ridge_mean_all%>%
  filter(year >= 2004)%>%
  glimpse()


gg_ridge_all_dis <- ggplot(ridge_mean_dis, aes(x = therm_parties_mean, 
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
gg_ridge_all_dis 
ggsave("fig/gg-ridge-all-dis.png", gg_ridge_all_dis, width = 8, height = 6, units = "in")



##### Political Knowledge #####

gg_ridge_knowledge <- ggplot(ridge_mean_dis, aes(x = therm_parties_mean, 
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
gg_ridge_knowledge 

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

ridge_df_ns <- cdf_trimmed%>%
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
ggsave("fig/gg-ridge-split-ns.png", cdf_ridge_ns, width = 8, height = 6, units = "in")

# ggplot(ridge_df_ns, aes(x = year, y = therm_inparty)) +
#   geom_point(aes(alpha=.1), position="jitter") +
#   geom_smooth(se=TRUE) +
#   facet_grid(rows = vars(pid_3))

######
### NPA Example
######

npa_parties_df_ns <- cdf_trimmed%>% # Making a DF of the party-year SD
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
ggsave("fig/gg-npa-ns.png", cdf_npa_ns, width = 6, height = 4, units = "in")

#########
### Partisans Below Median
########

# spatstat::weighted.median(cdf_trimmed$therm_inparty, weight, na.rm = TRUE)
# weighted.mean(cdf_trimmed$therm_inparty, cdf_trimmed$weight, na.rm = TRUE)

######################################################
### Party Medians (one for Dems and REps each)
###

# These make it easy to get party medians without too much pivoting
reps_df_ns <- cdf_trimmed%>%
  filter(pid_3 == "Republican")
dems_df_ns <- cdf_trimmed%>%
  filter(pid_3 == "Democrat")

below_med_party_ns <- cdf_trimmed%>%
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
ggsave("fig/gg-below-parties-ns.png", cdf_below_party_meds_ns, width = 6, height = 4, units = "in")

#####################################################
### Single Median
###

below_mct_ns <- cdf_trimmed%>% # MCT = Measure of Central Tendency
#below_mct_ns <- cdf_extended%>% # with 2020
  select(year,
         weight,
         pid_3,
         therm_inparty)%>%
  filter(pid_3 != "Independent" & year != 2002)%>%
  mutate(above_80_dum = if_else(therm_inparty > 80, 1, 0),
         above_75_dum = if_else(therm_inparty > 75, 1, 0),
         above_mean_sd_dum = if_else(therm_inparty > weighted.mean(therm_inparty, weight, na.rm = TRUE) + radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE), 1, 0),
         below_50_dum = if_else(therm_inparty < 50, 1, 0),
#         below_mean_dum = if_else(therm_inparty < weighted.mean(therm_inparty, weight, na.rm = TRUE), 1, 0),
#         below_mean_sd_dum = if_else(therm_inparty < weighted.mean(therm_inparty, weight, na.rm = TRUE) - radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE), 1, 0),
#         below_med_dum = if_else(therm_inparty < spatstat::weighted.median(therm_inparty, weight, na.rm = TRUE), 1, 0),
#         below_med_sd_dum = if_else(therm_inparty < spatstat::weighted.median(therm_inparty, weight, na.rm = TRUE) - radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE), 1, 0)
)%>%
  glimpse()

mct_prop_ns <- below_mct_ns%>% #way more measures here than are needed
  group_by(year, pid_3)%>%
  summarise(#prop_mean_below = weighted.mean(below_mean_dum, weight, na.rm = TRUE),
            #prop_mean_sd_below = weighted.mean(below_mean_sd_dum, weight, na.rm = TRUE),
#            prop_med_below = weighted.mean(below_med_dum, weight, na.rm = TRUE), #prop_below is those below the MCT, prop_sd_below is those one SD below med
#            prop_med_below_sd = weighted.mean(below_med_sd_dum, weight, na.rm = TRUE),
            prop_50_below = weighted.mean(below_50_dum, weight, na.rm = TRUE),
            prop_80_above = weighted.mean(above_80_dum, weight, na.rm = TRUE),
            prop_75_above = weighted.mean(above_80_dum, weight, na.rm = TRUE),
#            prop_mean_sd_above = weighted.mean(above_mean_sd_dum, weight, na.rm = TRUE),
#            se_mean_below = diagis::weighted_se(below_mean_dum, weight, na.rm = TRUE),
#            se_mean_sd_below = diagis::weighted_se(below_mean_sd_dum, weight, na.rm = TRUE),
#            se_med_below = diagis::weighted_se(below_med_dum, weight, na.rm = TRUE),
#            se_med_sd_below = diagis::weighted_se(below_med_sd_dum, weight, na.rm = TRUE),
            se_50_below = diagis::weighted_se(below_50_dum, weight, na.rm = TRUE),
            se_80_above = diagis::weighted_se(above_80_dum, weight, na.rm = TRUE),
#            se_mean_sd_above = diagis::weighted_se(above_mean_sd_dum, weight, na.rm = TRUE)
  )%>%
  glimpse()

### Below 50:

gg_below_50_ns <- ggplot(mct_prop_ns, aes(x = year, y = prop_50_below)) +
  geom_line(aes(linetype = pid_3, color = pid_3), position = dodge, size = 1) + 
  #  geom_smooth(aes(linetype = pid_3_sort, color = pid_3_sort), span = .3, se = FALSE) +
  geom_linerange(aes(color = pid_3, ymin = prop_50_below - 2*se_50_below, ymax = prop_50_below + 2*se_50_below), position = dodge) +
  geom_point(aes(shape = pid_3, size = 1, color = pid_3), position = dodge) +
  scale_color_manual(values = c("Democrat" = "dodgerblue3",
                                "Republican" = "firebrick3")) +
  scale_linetype_manual(values = c("Democrat" = "longdash",
                                   "Republican" = "solid")) +
  theme(legend.position = c(0.1, 0.7)) +
  guides(size = "none") +
  labs(x = "Year", 
       y = "Proportion",
       color = "Party ID",
       linetype = "Party ID",
       shape = "Party ID",
       title = "Proportion of Partisans Below 50 In-Party FT",
       subtitle = "95% CI Given With Errorbar") #+
#  facet_wrap(vars(pid_str))
gg_below_50_ns

ggsave("fig/gg-below-50-ns.png", gg_below_50_ns, width = 8, height = 6, units = "in")

#above 75
gg_above_75_ns <- ggplot(mct_prop_ns, aes(x = year, y = prop_75_above)) +
  geom_errorbar(aes(ymin = prop_80_above - 2*se_50_below, ymax = prop_80_above + 2*se_50_below, width = .2)) +
  geom_line(aes(linetype = pid_3, color = pid_3), size = 1) + 
  #  geom_smooth(aes(linetype = pid_3, color = pid_3), span = .3, se = FALSE) +
  geom_point(aes(shape = pid_3, size = 1, color = pid_3)) +
  scale_color_manual(values = c("Democrat" = "dodgerblue3",
                                "Republican" = "firebrick3")) +
  scale_linetype_manual(values = c("Democrat" = "longdash",
                                   "Republican" = "solid")) +
  theme(legend.position = c(0.8, 0.9)) +
  guides(size = FALSE) +
  labs(x = "Year", 
       subtitle = "Includes Leaning Independents",
       y = "Proportion",
       color = "Party ID",
       linetype = "Party ID",
       shape = "Party ID",
       title = "Proportion of Partisans Above 75 in-party FT")
gg_above_75_ns

ggsave("fig/gg-above-70-ns.png", gg_above_75_ns, width = 8, height = 6, units = "in")


#histogram of therms by pid_str

