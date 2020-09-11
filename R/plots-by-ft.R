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


gg_dissat <- ggplot(prop_dissatisfied, aes(x = year, y = prop_dis)) +
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
	scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
	guides(size = FALSE) +
	labs(x = "Year", 
			 subtitle = "Grouped by warmth towards in-party (partisans) or average warmth towards parties (independents)",
			 y = "Proportion",
			 color = "Party ID",
			 linetype = "Party ID",
			 shape = "Party ID",
			 title = "Proportion of Partisans Dissatisfied With Democracy") #+
#  facet_wrap(vars(pid_str))
gg_dissat


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

gg_distrust <- ggplot(prop_distrusting, aes(x = year, y = prop_distrust)) +
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
	scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
	guides(size = FALSE) +
	labs(x = "Year", 
			 subtitle = "Grouped by warmth towards in-party (partisans) or average warmth towards parties (independents)",
			 y = "Proportion",
			 color = "Party ID",
			 linetype = "Party ID",
			 shape = "Party ID",
			 title = "Proportion of Partisans Distrusting Federal Gov") #+
#  facet_wrap(vars(pid_str))
gg_distrust


#Gov Run for a few interests or benefit of all

prop_gov_few <- tidy_cdf%>%
	filter(year >= 1990)%>%
	select(year,
				 weight,
				 gov_run_for_few_dum,
				 pid_3,
				 below_50_qual)%>%
	glimpse()%>%
	group_by(year, pid_3, below_50_qual)%>%
	summarize(prop_gov_few = weighted.mean(gov_run_for_few_dum, weight, na.rm = TRUE),
						n = n())%>%
	filter(!is.na(below_50_qual) & !is.na(pid_3))%>%
	glimpse()

gg_gov_few <- ggplot(prop_gov_few, aes(x = year, y = prop_gov_few)) +
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
	scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
	guides(size = FALSE) +
	labs(x = "Year", 
#			 subtitle = "Grouped by warmth towards in-party (partisans) or average warmth towards parties (independents)",
			 y = "Proportion",
			 color = "Party ID",
			 linetype = "Party ID",
			 shape = "Party ID",
			 title = "The Government is Run for a Few People at the Top") #+
#  facet_wrap(vars(pid_str))
gg_gov_few
#Country on Wrong Track

prop_wrong_track <- tidy_cdf%>%
	filter(year >= 1990 & year != 1996 & year != 2000)%>%
	select(year,
				 weight,
				 wrong_track_dum,
				 pid_3,
				 below_50_qual)%>%
	glimpse()%>%
	group_by(year, pid_3, below_50_qual)%>%
	summarize(prop_wrong_track = weighted.mean(wrong_track_dum, weight, na.rm = TRUE),
						n = n())%>%
	filter(!is.na(below_50_qual) & !is.na(pid_3))%>%
	glimpse()

gg_wrong_track <- ggplot(prop_wrong_track, aes(x = year, y = prop_wrong_track)) +
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
	scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
	guides(size = FALSE) +
	labs(x = "Year", 
			 #			 subtitle = "Grouped by warmth towards in-party (partisans) or average warmth towards parties (independents)",
			 y = "Proportion",
			 color = "Party ID",
			 linetype = "Party ID",
			 shape = "Party ID",
			 title = "The Country is on the Wrong Track") #+
#  facet_wrap(vars(pid_str))
gg_wrong_track

# Officials care what people like me think
prop_officials_dont_care <- tidy_cdf%>%
	filter(year >= 1990)%>%
	select(year,
				 weight,
				 officials_dont_care_dum,
				 pid_3,
				 below_50_qual)%>%
	glimpse()%>%
	group_by(year, pid_3, below_50_qual)%>%
	summarize(prop_officials_dont_care = weighted.mean(officials_dont_care_dum, weight, na.rm = TRUE),
						n = n())%>%
	filter(!is.na(below_50_qual) & !is.na(pid_3))%>%
	glimpse()

gg_offs_dont_care <- ggplot(prop_officials_dont_care, aes(x = year, y = prop_officials_dont_care)) +
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
	scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
	guides(size = FALSE) +
	labs(x = "Year", 
			 #			 subtitle = "Grouped by warmth towards in-party (partisans) or average warmth towards parties (independents)",
			 y = "Proportion",
			 color = "Party ID",
			 linetype = "Party ID",
			 shape = "Party ID",
			 title = "Government Officials Don't Care What People Like Me Think") #+
#  facet_wrap(vars(pid_str))
gg_offs_dont_care
# Gap between poor and wealthy larger

prop_wealth_gap_larger <- tidy_cdf%>%
	filter(year >= 2000)%>%
	select(year,
				 weight,
				 wealth_gap_larger_dum,
				 pid_3,
				 below_50_qual)%>%
	group_by(year, pid_3, below_50_qual)%>%
	summarize(prop_wealth_gap_larger = weighted.mean(wealth_gap_larger_dum, weight, na.rm = TRUE),
						n = n())%>%
	filter(!is.na(below_50_qual) & !is.na(pid_3))%>%
	glimpse()

gg_gap_larger <- ggplot(prop_wealth_gap_larger, aes(x = year, y = prop_wealth_gap_larger)) +
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
	scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
	guides(size = FALSE) +
	labs(x = "Year", 
			 #			 subtitle = "Grouped by warmth towards in-party (partisans) or average warmth towards parties (independents)",
			 y = "Proportion",
			 color = "Party ID",
			 linetype = "Party ID",
			 shape = "Party ID",
			 title = "The Gap Between Rich and Poor is than 20 Years Ago") #+
#  facet_wrap(vars(pid_str))
gg_gap_larger

#putting together

gg_by_ft <- grid.arrange(gg_distrust,
						 gg_dissat,
						 gg_gov_few,
						 gg_offs_dont_care,
						 gg_wrong_track,
						 gg_gap_larger,
						 ncol = 2)
gg_by_ft
ggsave("fig/timeseries-by-ft.png", gg_by_ft, width = 12, height = 12, units = "in")


