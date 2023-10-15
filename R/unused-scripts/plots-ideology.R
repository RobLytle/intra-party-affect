library(tidyverse)
library(ggExtra)
library(ggridges)
library(goji)
theme_set(theme_minimal())
#### CDF Time Series Dataframe
tidy_cdf_ideo <- read_rds("data/tidy-cdf.rds")%>%
	mutate(below_50_dum = if_else(therm_inparty < 50, "Below 50", "50 or Above"))%>%
	filter(year >= 1978)%>%
	glimpse()

tidy_2010s <- tidy_cdf_ideo%>%
	filter(year >= 2012)%>%
	glimpse()

model_quant <- lm(therm_inparty ~ ideo_self_party_dif, data = tidy_2010s, weights = weight)
stargazer::stargazer(type = "text", model_quant)

model_dum <- lm(therm_inparty ~ self_more_moderate_dum, data = tidy_2010s, weights = weight)
stargazer::stargazer(type = "text", model_dum)

model_complex <- lm(therm_inparty ~ self_more_moderate_dum + ideo_self_party_dif + self_more_moderate_dum*ideo_self_party_dif,
										data = tidy_2010s, weights = weight)
stargazer::stargazer(type = "text", model_complex)

ideo_dif_df <- tidy_cdf_ns%>%
	filter(pid_3 != "Independent" & year != 2002 & below_50_dum != is.na(TRUE))%>%
#	filter(year==2000)%>%glimpse()
	group_by(year, pid_2, below_50_dum)%>%
	summarize(prop_more_moderate = weighted.mean(self_more_moderate_dum, weight, na.rm = TRUE))%>%
	glimpse()

ideo_missing_df <- tidy_cdf_ideo%>%
	filter(pid_3 == "Democrat" & year == 2000 & ideo_self_party_dif != is.na(TRUE) & below_50_dum == "Below 50")%>%
select(year, pid_2, below_50_dum, ideo_self_party_dif, ideo_self_recode, ideo_inparty_recode)%>%
	glimpse()
missing_data <- finalfit::missing_plot(ideo_missing_df)
missing_data
ggsave("fig/ideology-missing.png", missing_data, width = 6, height = 4, units = "in")

prop_more_mod <- ggplot(ideo_missing_df, aes(x = year, y = prop_more_moderate)) +
#	geom_errorbar(aes(ymin = prop_50_below - se_50_below, ymax = prop_50_below + se_50_below, width = .2)) +
	geom_line(aes(linetype = pid_2, color = pid_2), size = 1) + 
	#  geom_smooth(aes(linetype = pid_2, color = pid_2), span = .3, se = FALSE) +
	geom_point(aes(shape = pid_2, size = 1, color = pid_2)) +
	scale_color_manual(values = c("Democrat" = "dodgerblue3",
																"Republican" = "firebrick3")) +
	scale_linetype_manual(values = c("Democrat" = "longdash",
																	 "Republican" = "solid")) +
	theme(legend.position = c(0.1, 0.8)) +
	guides(size = FALSE) +
	#scale_y_continuous(breaks = seq(.25, .55, by = .05), limits = c(.25,.55)) +
	labs(x = "Year", 
			 subtitle = "Based on Self-Reported 7-Point Ideology",
			 y = "Proportion",
			 color = "Party ID",
			 linetype = "Party ID",
			 shape = "Party ID",
			 title = "Proportion Who View their Party as More Extreme") +
	facet_wrap(vars(below_50_dum))
prop_more_mod

ggsave("fig/cdf-prop-more-mod.png", prop_more_mod, width = 6, height = 4, units = "in")