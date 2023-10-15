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
         therm_parties_mean,
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
         therm_parties_mean,
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
  ) +
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

