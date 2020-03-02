library(tidyverse)
library(ggthemes)
library(ggExtra)
library(kableExtra)
library(scales)
library(gridExtra)

#This chunk trims down the big ol' tidy-2016.rds into only Democrats who voted for Bernie or Hillary
dem_primary <- read_rds("data/tidy-primaries.rds")%>%
  # filter(pre_pid_3 == "Democrat")%>%
  filter(primary_vote_choice == c("Hillary Clinton", "Bernie Sanders", "Barack Obama", "Didn't Vote") & pre_pid_3 == "Democrat")%>%
  glimpse()
###########
###########
## # #  ###
# The first chunk of code creates a histogram of Bernie and Hillary voters party FT differences
base_therm_hist_2016 <- ggplot(dem_primary, aes(x = net_party_affect, y = ..density.., fill = primary_vote_choice, alpha = .1)) + 
  guides(alpha = FALSE) +
  geom_density() +
  theme_bw()+
  facet_wrap(vars(year)) +
  xlim(0,100) +
  scale_color_colorblind() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank())+
  labs(x = "Net Partisan Affect",
       y = "Density",
       alpha = FALSE,
       fill = "Primary Vote Choice")
base_therm_hist_2016
ggsave("fig/therm-hist-2016.png", width = 10, height = 6, units = "in")

#####################
#####################
#####################


#Wrangles 
dem_2008_primary <- dem_primary%>%
  filter(year == 2008)%>%
  glimpse()

dem_08_means <- dem_2008_primary%>%
  group_by(primary_vote_choice)%>%
  summarise(ft_rep = mean(pre_therm_rep, na.rm = TRUE),
            ft_dem = mean(pre_therm_dem, na.rm = TRUE))%>%
  glimpse()

dem_2016_primary <- dem_primary%>%
  filter(year == 2016)%>%
  glimpse()

dem_16_means <- dem_2016_primary%>%
  group_by(primary_vote_choice)%>%
  summarise(ft_rep = mean(pre_therm_rep, na.rm = TRUE),
            ft_dem = mean(pre_therm_dem, na.rm = TRUE))%>%
  glimpse()

# Plots
scat_08 <- ggplot(dem_2008_primary, aes(pre_therm_dem, pre_therm_rep, alpha = .4))+
  guides(alpha = FALSE, x = FALSE) +
  geom_vline(aes(xintercept = mean(dem_2008_primary$pre_therm_dem, na.rm = TRUE), alpha = .4)) +
  facet_wrap(vars(primary_vote_choice))+
  geom_point()+
  theme_bw()+
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x=element_blank())+
#    axis.text.x=element_blank(),
#    axis.ticks.x=element_blank()) +
  labs(#subtitle = "2008",
       y = "Republican FT, 2008") +
  xlim(0,100)+
  ylim(0,100)
scat_08
#  geom_vline(data = dem_08_means, mapping = aes(xintercept = ft_dem), alpha = .4) +
#  geom_hline(data = dem_08_means, mapping = aes(yintercept = ft_rep), alpha = .4) +

#  geom_hline(aes(yintercept = mean(pre_therm_rep)), alpha = .4) +
#  geom_vline(aes(xintercept = mean(pre_therm_dem)), alpha = .4) +

dem_16 <- ggplot(dem_2016_primary, aes(pre_therm_dem, pre_therm_rep, alpha = .4))+
  geom_vline(aes(xintercept = mean(dem_2016_primary$pre_therm_dem, na.rm = TRUE), alpha = .4)) +
  facet_wrap(vars(primary_vote_choice)) +
  guides(alpha = FALSE) +
  geom_point()+
  theme_bw()+
  theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank()) +
  labs(#subtitle = "2016",
       y = "Republican FT, 2016",
       x = "Democrat FT") +
  xlim(0,100) +
  ylim(0,100)
dem_16

primary_dem <- grid.arrange(dem_08,
                              dem_16,
                              nrow = 2)
primary_dem

ggsave("fig/primary-scatter.png", plot = primary_scats, width = 8, height = 6, units = "in")
#####
#__xxXX-GRAVEYARD OF DEPRECATED PLOTS-XXxx__
#
#THESE PLOTS ARE NO LONGER IN USE, BUT I'M HOLDING ONTO THEM FOR POSTERITY
#DECOMMENT IF YOU WANT TO SEE...
#
#IF YOU DARE!
#####




#ideo_hist <- ggplot(dem_primary, aes(x = pre_self_ideo_7, alpha = .1)) + 
#  geom_bar() +
#  theme_bw()+
#  facet_grid(rows = year, cols = primary_vote_choice) +
#  theme(
#    plot.background = element_blank(),
#   panel.grid.major = element_blank(),
#    panel.grid.minor = element_blank(),
#    panel.border = element_blank())
#ideo_hist
###
# This chunk creates a scatterplot of B/H supporters 
#base_scatter_2016 <- ggplot(clinton_sanders_2016, aes(x = dem_self_ideo_dif, y = parties_therm_dif, color = pre_pid_7)) +
#  geom_point() +
#  geom_smooth(method = "lm", se = FALSE, aes(group=1),colour="black") +
#  theme_bw()+
#  theme(
#    plot.background = element_blank(),
#    panel.grid.major = element_blank(),
#    panel.grid.minor = element_blank(),
#    panel.border = element_blank()) +
#  facet_wrap(vars(primary_vote_choice))
#base_scatter_2016

#scatter_2016 <- base_scatter_2016 +
#  scale_color_tableau(palette = 'Color Blind') +
#  labs(title = "Ideology and Feeling Thermometer Differences",
#       subtitle = "Between Clinton and Sanders Supporters",
#       y = "Net Partisan Affect",
#       x = "Difference in Democrat and Self Ideology Assessment",
#      color = "Party Identification")
#scatter_2016

#ggsave("fig/therm-ideo-scatter-2016.png", width = 10, height = 6, units = "in")


# This plot reports avg FT distance by primary vote choice. Bar chart is probably not the best way to display these data
#therm_dif <- ggplot(clinton_sanders_2016, aes(x = primary_vote_choice, y = parties_therm_dif))+
#geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
#  theme_minimal()+
#  labs(y = "Difference of Partisan FT",
#       x = "Primary Vote Choice",
#       title = "Average Pre-measured Party FT Distance",
#       subtitle = "By Respondents' Vote Choice in the 2016 Primary")

#therm_dif

#ggsave("fig/party-therm-dif.png", width = 10, height = 6, units = "in")


#

# This chart reports avg ideo-self assessments by primary vote. Again, should probably present in a table.
# This chart has now been deprecated by a table
#ideo_self <- ggplot(clinton_sanders_2016, aes(x = primary_vote_choice, y = as.numeric(pre_self_ideo_7)))+
#  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
#  theme_minimal()+
#  labs(y = "Average Ideological Self Assessment",
#       x = "Primary Vote Choice",
#       title = "Average Ideological Self Assessment",
#       subtitle = "By Respondents' Vote Choice in the 2016 Primary")
#ideo_self
##sum(is.na(dem$pre_self_ideo_7))
# ggsave("fig/ideo-self.png", width = 10, height = 6, units = "in")

# This chart reports avg ideo assessments of the parties by primary vote. Again, should probably present in a table.
# This chart has now been deprecated by a table
#ideo_dif <- ggplot(clinton_sanders_2016, aes(x = primary_vote_choice, y = parties_ideo_dif))+
#  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
#  theme_minimal()+
#  labs(y = "Average Ideological Self Assessment",
#       x = "Primary Vote Choice",
#       title = "Perc. Difference in Dem and Rep Party Ideology",
#       subtitle = "By Respondents' Vote Choice in the 2016 Primary")
#ideo_dif  #calls the ggplot object, displaying chart in console window
#sum(is.na(dem_2016$parties_ideo_dif))

#ggsave("fig/party-ideo-dif.png", width = 10, height = 6, units = "in")


#fit_bs <- lm(subset = primary_vote_choice == "Bernie Sanders", parties_therm_dif ~ parties_ideo_dif, data = dem)


#ggplot(dem, aes(parties_ideo_dif, parties_therm_dif))+
#  geom_point(position = "jitter")+
#  geom_smooth(method = "lm")



#ggplot(dem_2016, aes(x = parties_therm_dif)) +
#  geom_histogram(binwidth = 10) +
#  facet_wrap(vars(pre_pid_7))

#ggsave("fig/2016-therm-dif-histogram.png", width = 10, height = 6, units = "in")