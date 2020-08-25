library(tidyverse)
library(ggExtra)
library(ggridges)
library(goji)
library(gridExtra)
theme_set(theme_minimal())

#below_50_dum = if_else(therm_inparty < 50, 1, 0),

tidy_cdf <- read_rds("data/tidy-cdf.rds")%>%
	filter(year >= 1978)%>%
	mutate(below_50_qual = case_when(therm_inparty < 50 & pid_3 != "Independent" ~ "Cold",
																	therm_parties_mean < 50 & pid_3 == "Independent" ~ "Cold",
																	therm_inparty >= 50 | therm_parties_mean >= 50 ~ "Warm/Indifferent",
																	TRUE ~ NA_character_))%>%
	glimpse()


prop_dissatisfied <- tidy_cdf%>%
	filter(year >= 2004)%>%
	select(year,
				 weight,
				 dis_democ_dum,
				 pid_3,
				 below_50_qual)%>%
	group_by(year, pid_3, below_50_qual)%>%
	summarize(prop_dis = weighted.mean(dis_democ_dum, weight, na.rm = TRUE),
						n = n())%>%
	filter(!is.na(below_50_qual) & !is.na(pid_3))%>%
	glimpse()


cdf_dissat <- ggplot(prop_dissatisfied, aes(x = year, y = prop_dis)) +
	#  geom_errorbar(aes(ymin = prop_50_below - se_50_below, ymax = prop_50_below + se_50_below, width = .2)) +
	geom_line(aes(linetype = pid_3, color = pid_3), size = 1) + 
	#  geom_smooth(aes(linetype = pid_3_sort, color = pid_3_sort), span = .3, se = FALSE) +
	facet_wrap(vars(below_50_qual)) +
	geom_point(aes(shape = pid_3, size = 1, color = pid_3)) +
	scale_color_manual(values = c("Democrat" = "dodgerblue3",
																"Republican" = "firebrick3",
																"Independent" = "darkorchid3")) +
	#  scale_linetype_manual(values = c("Democrat" = "longdash",
	#                                   "Republican" = "solid",
	#                                   "Independent" = "dotted")) +
	theme(legend.position = c(0.1, 0.2)) +
	guides(size = FALSE) +
	labs(x = "Year", 
			 subtitle = "Grouped by warmth towards in-party (partisans) or average warmth towards parties (independents)",
			 y = "Proportion",
			 color = "Party ID",
			 linetype = "Party ID",
			 shape = "Party ID",
			 title = "Proportion of Partisans Dissatisfied With Democracy") #+
#  facet_wrap(vars(pid_str))
cdf_dissat


#trust

prop_distrusting <- tidy_cdf%>%
	filter(year >= 1996)%>%
	select(year,
				 weight,
				 distrust_gov_dum,
				 pid_3,
				 below_50_qual)%>%
	glimpse()%>%
	group_by(year, pid_3, below_50_qual)%>%
	summarize(prop_distrust = weighted.mean(distrust_gov_dum, weight, na.rm = TRUE),
						n = n())%>%
	filter(!is.na(below_50_qual) & !is.na(pid_3))%>%
	glimpse()

cdf_distrust <- ggplot(prop_distrusting, aes(x = year, y = prop_distrust)) +
	#  geom_errorbar(aes(ymin = prop_50_below - se_50_below, ymax = prop_50_below + se_50_below, width = .2)) +
	geom_line(aes(linetype = pid_3, color = pid_3), size = 1) + 
	#  geom_smooth(aes(linetype = pid_3_sort, color = pid_3_sort), span = .3, se = FALSE) +
	facet_wrap(vars(below_50_qual)) +
	geom_point(aes(shape = pid_3, size = 1, color = pid_3)) +
	scale_color_manual(values = c("Democrat" = "dodgerblue3",
																"Republican" = "firebrick3",
																"Independent" = "darkorchid3")) +
	#  scale_linetype_manual(values = c("Democrat" = "longdash",
	#                                   "Republican" = "solid",
	#                                   "Independent" = "dotted")) +
	theme(legend.position = c(0.1, 0.2)) +
	guides(size = FALSE) +
	labs(x = "Year", 
			 subtitle = "Grouped by warmth towards in-party (partisans) or average warmth towards parties (independents)",
			 y = "Proportion",
			 color = "Party ID",
			 linetype = "Party ID",
			 shape = "Party ID",
			 title = "Proportion of Partisans Distrusting Federal Gov") #+
#  facet_wrap(vars(pid_str))
cdf_distrust

