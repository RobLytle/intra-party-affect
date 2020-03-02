library(tidyverse)
library(ggExtra)

#### CDF Time Series Dataframe
cdf_ts <- read_rds("data/tidy-cdf.rds")%>% # Making a DF of the party-year SD
  filter(pid_3 != "Independent" & year != 2002)%>%
  rename(pid_2 = "pid_3")%>%
  select(year,
         pid_2,
         therm_dem,
         therm_rep)%>%
  pivot_longer(therm_dem:therm_rep, names_to = "ft_towards", values_to = "ft")%>%
  mutate(pid_2 = factor(pid_2, levels = c("Democrat", 
                                          "Republican")))%>%
  mutate(ft_towards = recode(ft_towards,
                             "therm_dem" = "Democrat",
                             "therm_rep" = "Republican"))%>%
  group_by(year, pid_2, ft_towards)%>%
  summarize(mean = mean(ft, na.rm = TRUE),
            sd = sd(ft, na.rm = TRUE))%>%
  mutate(ft_towards = factor(ft_towards, levels = c("Democrat", 
                                                    "Republican")))%>%
  mutate(in_out = factor(if_else((pid_2 == ft_towards), "In-Party", "Out-Party"), levels = c("In-Party",
                                                                                             "Out-Party")))%>%
  select(-ft_towards)%>%
  glimpse()


#####
## Plot of Dems' Affect Towards themselves and outparty
#####

base <- ggplot(cdf_ts, aes(x = as.numeric(year), color = in_out, linetype = pid_2)) +
  geom_line(size = 1.5) +
  theme_bw() +
  scale_color_grey()+
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()) 

sdp <- base + 
  aes(y = sd) +
#  ylim(15, 25) +
  labs(x = "Year",
       y = "Feeling Thermometer SD",
       color = "Towards In/Out-Party",
       linetype = "Respondents' Party",
       title = "Feeling Thermometer Deviation")
sdp
ggsave("fig/cdf-sd.png", plot = sdp, width = 8, height = 6, units = "in")

avgp <- base + 
  aes(y = mean) +
  labs(x = "Year",
       y = "Feeling Thermometer Mean",
       color = "Towards In/Out-Party",
       linetype = "Respondents' Party",
       title = "Feeling Thermometer Mean")
avgp

ggsave("fig/cdf-avg.png", plot = avgp, width = 8, height = 6, units = "in")



##########
cdf_dem <- read_rds("data/tidy-cdf.rds")%>%
  filter(year != 2002 & pid_3 == "Democrat")%>%
  glimpse()


scatter_dem <- ggplot(cdf_dem, aes(therm_dem, therm_rep), alpha = .3) +
  geom_point() +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()) +
  facet_wrap(vars(year)) +
#  geom_smooth(method = "lm")+
  labs(x = "Warmth Towards Democratic Party",
       y = "Warmth Towards Republican Party")
scatter_dem

ggsave("fig/cdf-scatter-dem.png", plot = scatter_dem, width = 8, height = 6, units = "in")
#####################
####################
#NPA PLOT
##########
##########
#### CDF Standard Deviation NPA
npa_stats <- read_rds("data/tidy-cdf.rds")%>% # Making a DF of the party-year SD
  filter(pid_3 == "Democrat" & year != 2002)%>%
  group_by(year)%>%
  summarize(SD = sd(parties_therm_dif, na.rm = TRUE),
            Mean = mean(parties_therm_dif, na.rm = TRUE)
            )%>%
  pivot_longer(cols = SD:Mean)%>%
  glimpse()

npa_line <- ggplot(npa_stats, aes(x = year, y = value, linetype = name)) +
  geom_line(size = 1.5) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()) +
  ylim(15, 60) +
  labs(x = "Year",
       y = "Net Partisan Affect",
       linetype = "Statistic")
npa_line
ggsave("fig/cdf-npa.png", plot = npa_line, width = 8, height = 6, units = "in")




##############################
################################
##############################
#SCATTER PLOT
############
#################
cdf_rep <- read_rds("data/tidy-cdf.rds")%>%
  filter(year != 2002 & pid_3 == "Republican")%>%
  glimpse()
scatter_rep <- ggplot(cdf_rep, aes(therm_dem, therm_rep), alpha = .3) +
  geom_point() +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()) +
  facet_wrap(vars(year)) +
  #  geom_smooth(method = "lm")+
  labs(x = "Democrat Feeling Thermometer",
       y = "Republican Feeling Thermometer")
scatter_rep
ggsave("fig/cdf-scatter-rep.png", plot = scatter_rep, width = 8, height = 6, units = "in")


#######################

########################

#########################

#####
### THese are the plots for "independent Dems"
###
###############

cdf_sd_ind <- read_rds("data/tidy-cdf.rds")%>% # Making a DF of the party-year SD
  filter(pid_7 == "Independent - Democrat" & year != 2002)%>%
  group_by(year)%>%
  summarize(therm_dem_sd = sd(therm_dem, na.rm = TRUE),
            therm_rep_sd = sd(therm_rep, na.rm = TRUE))%>%
  pivot_longer(cols = therm_dem_sd:therm_rep_sd)%>%
  rename(party_ft = name,
         sd = value)%>%
  mutate(party_ft = recode(party_ft,
                           "therm_dem_sd" = "Democrat",
                           "therm_rep_sd" = "Republican"))%>%
  mutate(party_ft = as.factor(party_ft))%>%
  glimpse()




sdp_ind <- ggplot(cdf_sd_ind, aes(x = as.numeric(year), y = sd, linetype = party_ft)) +
  geom_line(size = 1.5) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()) +
  ylim(15, 25) +
  labs(x = "Year",
       y = "Feeling Thermometer SD") +
  labs(linetype = "Party")
sdp_ind



cdf_avg_ind <- read_rds("data/tidy-cdf.rds")%>% # Making a DF of the party-year SD
  filter(pid_7 == "Independent - Democrat" & year != 2002)%>%
  group_by(year)%>%
  summarize(therm_dem_avg = mean(therm_dem, na.rm = TRUE),
            therm_rep_avg = mean(therm_rep, na.rm = TRUE))%>%
  pivot_longer(cols = therm_dem_avg:therm_rep_avg)%>%
  rename(party_ft = name,
         mean = value)%>%
  mutate(party_ft = recode(party_ft,
                           "therm_dem_avg" = "Democrats",
                           "therm_rep_avg" = "Republicans"))%>%
  mutate(party_ft = as.factor(party_ft))%>%
  glimpse()


avgp_ind <- ggplot(cdf_avg_ind, aes(x = as.numeric(year), y = mean, linetype = party_ft)) +
  geom_line(size = 1.5) +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()) +
  ylim(0, 100) +
  labs(x = "Year",
       y = "Feeling Thermometer Average") +
  labs(linetype = "Thermometer Towards:")
avgp_ind

ggsave("fig/cdf-avg-ind.png", plot = avgp_ind, width = 8, height = 6, units = "in")
ggsave("fig/cdf-sd-ind.png", plot = sdp_ind, width = 8, height = 6, units = "in")

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

