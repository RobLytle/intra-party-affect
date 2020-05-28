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
  filter(group == "Democrat - In Party" | group == "Republican - In Party")%>%
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

sd_ft <- ggplot(cdf_sd, aes(x = year, y = result, color = group)) +
#  geom_smooth(aes(linetype = group), span = .3, se=F) + 
  geom_line(aes(linetype = group), size = 1.5) +
  geom_point(aes(shape = group), size = 2) +
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
       subtitle = "Excludes Leaning Independents",
       linetype = " ",
       color = " ",
       shape = " ") +
  theme(legend.position = c(0.2, 0.8))
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
#  filter(pid_str == "Strong Partisan")%>%
  select(year,
         weight,
         pid_3_sort,
         therm_inparty)%>%
  filter(pid_3_sort != "Independent" & year != 2002)%>%
  mutate(above_80_dum = if_else(therm_inparty > 80, 1, 0),
         above_mean_sd_dum = if_else(therm_inparty > weighted.mean(therm_inparty, weight, na.rm = TRUE) + radiant.data::weighted.sd(therm_inparty, weight, na.rm = TRUE), 1, 0),
         below_50_dum = if_else(therm_inparty < 50, 1, 0),
         below_mean_dum = if_else(therm_inparty < weighted.mean(therm_inparty, weight, na.rm = TRUE), 1, 0),
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

# Below 50

cdf_below_50 <- ggplot(below_mct_prop, aes(x = year, y = prop_50_below)) +
  geom_errorbar(aes(ymin = prop_50_below - se_50_below, ymax = prop_50_below + se_50_below, width = .2)) +
  geom_smooth(aes(linetype = pid_3_sort, color = pid_3_sort), span = .3, se = FALSE) +
  geom_point(aes(shape = pid_3_sort, size = 1, color = pid_3_sort)) +
  scale_color_manual(values = c("Democrat" = "dodgerblue3",
                                "Republican" = "firebrick3")) +
  theme(legend.position = c(0.1, 0.7)) +
  guides(size = FALSE) +
  labs(x = "Year", 
       subtitle = "Excludes Leaning Independents",
       y = "Proportion",
       color = "Party ID",
       linetype = "Party ID",
       shape = "Party ID",
       title = "Proportion of Partisans Below 50 In-Party FT")
cdf_below_50
ggsave("fig/cdf-below-50.png", cdf_below_50, width = 8, height = 6, units = "in")
