library(tidyverse)
library(ggthemes)
library(ggExtra)
library(kableExtra)
library(scales)
library(gridExtra)

#This chunk trims down the big ol' tidy-2016.rds into only Democrats who voted for Bernie or Hillary
rep_primary <- read_rds("data/tidy-primaries.rds")%>%
  # filter(pre_pid_3 == "Democrat")%>%
  filter(pre_pid_3 == "Republican")%>%
  glimpse()
###########
###########
## # #  ###
# The first chunk of code creates a histogram of Bernie and Hillary voters party FT differences

#####################
#####################
#####################



#Wrangles 
rep_2008_primary <- rep_primary%>%
  filter(year == 2008 & (primary_vote_choice == "John McCain" | 
                           primary_vote_choice == "Mitt Romney" | 
                           primary_vote_choice == "Mike Huckabee" | 
                           primary_vote_choice == "Didn't Vote" ))%>%
  glimpse()

rep_08_means <- rep_2008_primary%>%
  group_by(primary_vote_choice)%>%
  summarise(ft_rep = mean(pre_therm_rep, na.rm = TRUE),
            ft_dem = mean(pre_therm_dem, na.rm = TRUE))%>%
  glimpse()

rep_2016_primary <- rep_primary%>%
  filter(year == 2016 & (primary_vote_choice == "Donald Trump" |
                          primary_vote_choice == "Ted Cruz" |
                          primary_vote_choice == "John Kasich" |
                          primary_vote_choice == "Marco Rubio" |
                          primary_vote_choice == "Didn't Vote"))%>%
  glimpse()

rep_16_means <- rep_2016_primary%>%
  group_by(primary_vote_choice)%>%
  summarise(ft_rep = mean(pre_therm_rep, na.rm = TRUE),
            ft_rep_sd = sd(pre_therm_rep, na.rm = TRUE), 
            ft_dem = mean(pre_therm_dem, na.rm = TRUE),
            ft_dem_sd = sd(pre_therm_dem, na.rm = TRUE),
            n = n())%>%
  glimpse()

print(rep_16_means)
print(dem_16_means)
# Plots

#Histogram
base_therm_hist_2016 <- ggplot(rep_2016_primary, aes(x = pre_therm_rep, y = ..density.., fill = primary_vote_choice, alpha = .1)) + 
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
  labs(x = "Intra-Partisan Affect",
       y = "Density",
       alpha = FALSE,
       fill = "Primary Vote Choice")
base_therm_hist_2016


###
rep_08 <- ggplot(rep_2008_primary, aes(pre_therm_rep, pre_therm_dem, alpha = .4))+
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
    y = "Democrat FT, 2008") +
  xlim(0,100)+
  ylim(0,100)
rep_08
#  geom_vline(data = dem_08_means, mapping = aes(xintercept = ft_dem), alpha = .4) +
#  geom_hline(data = dem_08_means, mapping = aes(yintercept = ft_rep), alpha = .4) +

#  geom_hline(aes(yintercept = mean(pre_therm_rep)), alpha = .4) +
#  geom_vline(aes(xintercept = mean(pre_therm_dem)), alpha = .4) +

rep_16 <- ggplot(rep_2016_primary, aes(pre_therm_rep, pre_therm_dem, alpha = .4))+
  geom_vline(aes(xintercept = mean(rep_2016_primary$pre_therm_rep, na.rm = TRUE), alpha = .4)) +
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
    y = "Democrat FT, 2016",
    x = "Republican FT") +
  xlim(0,100) +
  ylim(0,100)
rep_16

primary_rep <- grid.arrange(rep_08,
                            rep_16,
                            nrow = 2)
primary_rep

ggsave("fig/primary-scatter.png", plot = primary_scats, width = 8, height = 6, units = "in")
#####