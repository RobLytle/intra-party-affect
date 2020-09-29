library(tidyverse)
theme_set(theme_minimal())
library(ggthemes)
library(ggExtra)
library(kableExtra)
library(scales)
library(gridExtra)

df_primaries <- read_rds("data/tidy-primaries.rds")%>%
	filter(pid_3 != "Independent" & !is.na(primary_vote_simple))%>%
	mutate(primary_vote_simple = recode(primary_vote_simple, .default = levels(primary_vote_simple),
																			"Didn't Vote" = "Other/Third Party/Didn't Vote",
																			"Voted in Other Party Primary" = "Other/Third Party/Didn't Vote"))%>%
	mutate(primary_vote_simple = factor(primary_vote_simple,
																			levels = c("Winner", "Loser", "Other/Third Party/Didn't Vote")))%>%
	filter(primary_vote_simple != "Other/Third Party/Didn't Vote")
	glimpse()

df_in_means <- df_primaries%>%
	group_by(primary_vote_simple, year, pid_3)%>%
	summarize(mean_ft = weighted.mean(therm_inparty, weight, na.rm = TRUE))%>%
	glimpse()

gg_primary_in <- ggplot(df_primaries, aes(x = therm_inparty, color = primary_vote_simple)) +
	geom_density(size = .75) +
	facet_grid(rows = c(vars(year)), 
						 cols = c(vars(pid_3))) +
#	geom_vline(aes(color = primary_vote_simple, xintercept = mean(therm_inparty, na.rm = TRUE)))
	geom_vline(data = df_in_means, 
						 aes(xintercept=mean_ft, 
								 color = primary_vote_simple,
								 linetype = primary_vote_simple),
						 size = 1) +
	theme(legend.key.height = unit(.05, "npc"),
				legend.position = c(.15, .55),
				legend.text = element_text(size = 8),
				legend.title = element_text(size = 8)) +
	scale_color_manual(values = c("Winner" = "#1E88E5", #colorblind safe palette
										 "Loser" = "#D21C1C",
										 "Other/Third Party/Didn't Vote" = "#FFC107"))+
	scale_linetype_manual(values = c("Other/Third Party/Didn't Vote" = "solid",
																	 "Winner" = "longdash",
																	 "Loser" = "dotted")) +
	labs(x = "Inparty FT",
			 y = "Density",
			 title = "In Party Feeling Thermometer Distribution by Primary Vote Choice",
			 color = "Primary Vote",
			 linetype = "Primary Vote")
gg_primary_in

ggsave("fig/gg-primary-in-hist.png", gg_primary_in, width = 8, height = 6, units = "in")
