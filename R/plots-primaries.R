library(tidyverse)
theme_set(theme_minimal())
library(ggthemes)
library(ggExtra)
library(kableExtra)
library(scales)
library(gridExtra)
library(ggpubr)

df_primaries <- read_rds("data/tidy-primaries.rds")%>%
	filter(pid_3 != "Independent" & !is.na(primary_vote_simple))%>%
	mutate(primary_vote_simple = recode(primary_vote_simple, .default = levels(primary_vote_simple),
																			"Didn't Vote" = "Other/Third Party/\n Didn't Vote",
																			"Voted in Other Party Primary" = "Other/Third Party/\n Didn't Vote"))%>%
	mutate(primary_vote_simple = factor(primary_vote_simple,
																			levels = c("Winner", "Loser", "Other/Third Party/\n Didn't Vote")))%>%
#	filter(primary_vote_simple != "Other/Third Party/\n Didn't Vote")%>%
	glimpse()

	###
	# Inparty FT
	###
df_in_means <- df_primaries%>%
	group_by(primary_vote_simple, year, pid_3)%>%
	summarize(mean_ft = weighted.mean(therm_inparty, weight, na.rm = TRUE))%>%
	glimpse()

gg_primary_in <- ggplot(df_primaries, aes(x = therm_inparty, color = primary_vote_simple)) +
	geom_density(size = .75,
							 alpha = .0) +
	facet_grid(rows = c(vars(year)), 
						 cols = c(vars(pid_3))) +
#	geom_vline(aes(color = primary_vote_simple, xintercept = mean(therm_inparty, na.rm = TRUE)))
	geom_vline(data = df_in_means, 
						 aes(xintercept=mean_ft, 
								 color = primary_vote_simple,
								 linetype = primary_vote_simple),
						 size = 1,
						 alpha = .5) +
	theme(legend.key.height = unit(.05, "npc"),
				legend.position = c(.15, .55),
				legend.text = element_text(size = 8),
				legend.title = element_text(size = 8),
				plot.title = element_text(hjust = 0.5)) +
	scale_color_manual(values = c("Winner" = "#1E88E5", #colorblind safe palette
										 "Loser" = "#D21C1C",
										 "Other/Third Party/\n Didn't Vote" = "#FFC107"))+
	scale_linetype_manual(values = c("Other/Third Party/\n Didn't Vote" = "solid",
																	 "Winner" = "longdash",
																	 "Loser" = "dotted")) +
	labs(x = "Inparty FT",
			 y = "Density",
			 title = "In Party",
			 color = "Primary Vote",
			 linetype = "Primary Vote")
gg_primary_in

ggsave("fig/gg-primary-in-hist.png", gg_primary_in, width = 8, height = 6, units = "in")


#Outparty FTS

df_out_means <- df_primaries%>%
	group_by(primary_vote_simple, year, pid_3)%>%
	summarize(mean_ft = weighted.mean(therm_outparty, weight, na.rm = TRUE))%>%
	glimpse()

gg_primary_out <- ggplot(df_primaries, aes(x = therm_outparty, color = primary_vote_simple)) +
	geom_density(size = .75) +
	facet_grid(rows = vars(year), 
						 cols = vars(pid_3)) +
#	facet_grid(year ~ pid_3) +
	#	geom_vline(aes(color = primary_vote_simple, xintercept = mean(therm_inparty, na.rm = TRUE)))
	geom_vline(data = df_out_means, 
						 aes(xintercept=mean_ft, 
						 		color = primary_vote_simple,
						 		linetype = primary_vote_simple,
						 		alpha = .5),
						 size = 1) +
	theme(legend.key.height = unit(.05, "npc"),
				legend.position = c(1, .23), # legend position optimized for the two plots together
				legend.text = element_text(size = 8),
				legend.title = element_text(size = 8),
				plot.title = element_text(hjust = 0.5)) +
	guides(alpha = FALSE) +
	scale_color_manual(values = c("Winner" = "#1E88E5", #colorblind safe palette
																"Loser" = "#D21C1C",
																"Other/Third Party/\n Didn't Vote" = "#FFC107"))+
	scale_linetype_manual(values = c("Other/Third Party/\n Didn't Vote" = "solid",
																	 "Winner" = "longdash",
																	 "Loser" = "dotted")) +
	labs(x = "Outparty FT",
			 y = "Density",
			 title = "Out Party",
			 color = " ",
			 linetype = " ")
gg_primary_out

#putting them together

p1 <- gg_primary_in + theme(legend.position = "none",
														axis.title.x = element_blank(),
													  axis.title.y = element_blank(),
														axis.text.y = element_blank(),
												#		title = element_text(size = 8)
														)
p1

p2 <- gg_primary_out + theme(
	axis.title.x = element_blank(),
	axis.title.y = element_blank(),
	strip.text.y = element_blank()
#	title = element_text(size = 8)
)
p2


gg_primaries <- grid.arrange(arrangeGrob(p2, p1,
																				 ncol = 2,
																				 top = text_grob("Distribution of Partisan Warmth\n by Primary Vote", vjust = 1, face = "bold"),
																				 left = text_grob("Density", rot = 90, vjust = 1, face = "bold"),
																			   bottom = text_grob("Feeling Thermometer", face = "bold")))
gg_primaries

ggsave("fig/gg-primaries-grid.png", gg_primaries, width = 8, height = 6, units = "in")

