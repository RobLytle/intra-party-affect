library(tidyverse)
library(broom)
library(rsample)
library(ggExtra)
library(ggridges)
library(gridExtra)
library(ggpubr)
library(janitor)
library(diagis)
library(forcats)
library(cowplot)
set.seed(2001)
theme_set(theme_minimal())
# Fix this so that we use 2020 in all of these

##
# Differences in opinion and behavior between cold/warm partisans
# Cold <50, warm greater=50
##
tidy_2020_df <- read_rds("data/tidy-2020.rds")%>% # Including 
	select(weight, #just selecting the relevant vars because the resampling takes a long time.
				 pid_3,
				 below_50_qual_strict,
				 dis_democ_dum,
				 distrust_gov_dum,
				 gov_run_for_few_dum,
				 wrong_track_dum,
				 officials_dont_care_dum,
				 wealth_gap_larger_dum)%>%
	glimpse()
##
# Bootstrapping opinion SEs
#
opinion_df <- read_rds("data/tidy-cdf.rds")%>%
	filter(year >= 1978 & pid_3 != "Independent")%>%
	select(weight, #just selecting the relevant vars because the resampling takes a long time.
				 pid_3,
				 below_50_qual_strict,
				 dis_democ_dum,
				 distrust_gov_dum,
				 gov_run_for_few_dum,
				 wrong_track_dum,
				 officials_dont_care_dum,
				 wealth_gap_larger_dum)%>%
	rbind(tidy_2020_df)%>%
	glimpse()

opinion_boot_df <- data.frame(boot = 1:500)%>%
	group_by(boot)%>%
	do(sample_n(opinion_df, nrow(opinion_df), replace = TRUE))%>% #creating 2000 new datasets of equal size to the original
	group_by(boot,
					 pid_3,
					 below_50_qual_strict)%>%
	summarize(
		prop_dissat = weighted.mean(dis_democ_dum, weight, na.rm = TRUE),
		prop_distrust = weighted.mean(distrust_gov_dum, weight, na.rm = TRUE),
		prop_gov_few = weighted.mean(gov_run_for_few_dum, weight, na.rm = TRUE),
		prop_wrong_track = weighted.mean(wrong_track_dum, weight, na.rm = TRUE),
		prop_officials_dont_care = weighted.mean(officials_dont_care_dum, weight, na.rm = TRUE),
		prop_wealth_gap_larger = weighted.mean(wealth_gap_larger_dum, weight, na.rm = TRUE)
	)%>%
	glimpse()


se_op_df <- opinion_boot_df%>% #putting this here so I don't have to run the resample every time. Once troubleshooted, I will put in one pipe
	filter(!is.na(below_50_qual_strict))%>%
	pivot_wider(names_from = below_50_qual_strict,
							values_from = prop_dissat:prop_wealth_gap_larger)%>%
	pivot_longer(prop_dissat_cold:prop_wealth_gap_larger_warm, 
							 names_to = c("which_question", ".value"), 
							 names_pattern="(.*)_([a-z]*)")%>%
	group_by(boot,
					 pid_3,
					 which_question)%>%
	summarize(prop_dif = (cold-warm)/cold + warm,
						prop_dif_simple = (cold-warm))%>%
	group_by(
		pid_3,
		which_question)%>%
	#	summarize(prop_se = parameters::standard_error(prop_dif))%>%
	summarize(prop_se = sd(prop_dif),
						simple_prop_se = sd(prop_dif_simple))%>%
	mutate(which_question = recode(which_question,
																 "prop_dissat" = "Very/Fairly Dissatisfied With Democracy",
																 "prop_distrust" = "Distrusts Gov. \"Most\" or \"Almost All\" of the Time",
																 "prop_gov_few" = "\"Government is run for a few at the top\"",
																 "prop_officials_dont_care" = "Officials don\'t care what people like me think",
																 "prop_wealth_gap_larger" = "Wealth gap greater today than 20 years ago",
																 "prop_wrong_track" = "Country is on Wrong Track"))%>%
	glimpse()


#opinion_means_df <- read_rds("data/tidy-cdf.rds")%>%
opinion_means_df <- opinion_df%>%
	filter(pid_3 != "Independent")%>%
#	mutate(ltet_2004 = if_else(year <= 2004, 1, 0))%>%
	group_by(pid_3, below_50_qual_strict)%>%
	summarize(prop_dissat = weighted.mean(dis_democ_dum, weight, na.rm = TRUE),
						prop_distrust = weighted.mean(distrust_gov_dum, weight, na.rm = TRUE),
						prop_gov_few = weighted.mean(gov_run_for_few_dum, weight, na.rm = TRUE),
						prop_wrong_track = weighted.mean(wrong_track_dum, weight, na.rm = TRUE),
						prop_officials_dont_care = weighted.mean(officials_dont_care_dum, weight, na.rm = TRUE),
						prop_wealth_gap_larger = weighted.mean(wealth_gap_larger_dum, weight, na.rm = TRUE))%>%
	pivot_wider(names_from = below_50_qual_strict,
							values_from = prop_dissat:prop_wealth_gap_larger)%>%
	select(-ends_with("NA"))%>%
	pivot_longer(prop_dissat_cold:prop_wealth_gap_larger_warm, 
							 names_to = c("which_question", ".value"), 
							 names_pattern="(.*)_([a-z]*)")%>%
	mutate(which_question = as.factor(which_question))%>%
	group_by(
		pid_3,
		which_question)%>%
	#	summarize(prop_dif = (cold - warm))%>%
	summarize(prop_difference = (cold - warm)/(cold+warm),#difference is divided to normalize
						prop_difference_simple = (cold-warm)
						#	se_dif
	)%>% 
	mutate(which_question = as.factor(which_question))%>%
	filter(!is.na(prop_difference))%>%
	mutate(which_question = recode(which_question,
																 "prop_dissat" = "Very/Fairly Dissatisfied With Democracy",
																 "prop_distrust" = "Distrusts Gov. \"Most\" or \"Almost All\" of the Time",
																 "prop_gov_few" = "\"Government is run for a few at the top\"",
																 "prop_officials_dont_care" = "Officials don\'t care what people like me think",
																 "prop_wealth_gap_larger" = "Wealth gap greater today than 20 years ago",
																 "prop_wrong_track" = "Country is on Wrong Track"))%>%
	mutate(which_question = reorder(which_question, prop_difference),)%>%
	glimpse()

opinion_pooled_df <- full_join(opinion_means_df, se_op_df)%>%
	mutate(which_question = reorder(which_question, prop_difference),
				 which_question_simple = reorder(which_question, prop_difference_simple),
				 sig_dum = case_when(prop_difference < 0 & prop_difference + 1.96*prop_se < 0 ~ TRUE,
				 										prop_difference > 0 & prop_difference - 1.96*prop_se > 0 ~ TRUE,
				 										TRUE ~ FALSE),
				 sig_dum_simple = case_when(prop_difference_simple < 0 & prop_difference_simple + 1.96*simple_prop_se < 0 ~ TRUE,
				 										prop_difference_simple > 0 & prop_difference_simple - 1.96*simple_prop_se > 0 ~ TRUE,
				 										TRUE ~ FALSE))%>%
	write_rds("data/tidy-opinion.rds")%>%
	glimpse()

#The Plot
dodge <- position_dodge(width=0.5)

gg_strict_opinion_pooled <- ggplot(opinion_pooled_df, aes(x = prop_difference, y = fct_relabel(which_question, str_wrap, width = 20))) +
	geom_linerange(aes(xmin = prop_difference - 1.96*prop_se, xmax = prop_difference + 1.96*prop_se, color = pid_3), position = dodge) +
	#	geom_point(data=opinion_pooled_df[opinion_pooled_df$sig_dum == TRUE,],size=5, aes(position = pid_3), shape = 1, position = dodge) + #overlays a shape on sig 
	geom_point(aes(color = pid_3, shape = pid_3), size = 3, position = dodge) +
	scale_color_manual(values = c("Democrat" = "dodgerblue3",
																"Republican" = "firebrick3")) +
	geom_vline(xintercept = 0.00) +
	coord_cartesian(xlim = c(-.75, .75)) +
	scale_x_continuous(n.breaks = 6) +	
	labs(x = "Difference",
			 y = "Question",
			 #			 title = "Difference in Proportion Whom Agree\n between Cold/Warm Partisans",
			 subtitle = "Opinion Items",
			 color = "Party",
			 shape = "Party")
gg_strict_opinion_pooled

ggsave("fig/gg-pooled-opinion-strict.png", gg_strict_opinion_pooled, width = 6, height = 4, units = "in")


###
## Pooled Behavior
## First, bootstrapping SE
###
behavior_df <- read_rds("data/tidy-cdf.rds")%>%
	filter(year >= 1978 & pid_3 != "Independent")%>%
	select(weight, #just selecting the relevant vars because the resampling takes a long time.
				 pid_3,
				 below_50_qual_strict,
				 general_vote_dum,
				 activist_6cat,
				 split_ticket_dum,
				 meetings_dum,
				 work_cand_dum,
				 display_merch_dum,
				 donate_dum,
				 watch_campaign_tv_dum,
				 knows_house_pre_dum,
				 knows_house_post_dum,
				 talk_politics_most_days_dum,
				 primary_vote_dum,
				 early_vote_dum,
				 vote_inparty_house_dum,
				 vote_outparty_pres_dum,
				 vote_thirdparty_pres_dum,
				 vote_inparty_pres_dum)%>%
	glimpse()

behavior_boot_df <- data.frame(boot = 1:2000)%>%
	group_by(boot)%>%
	do(sample_n(behavior_df, nrow(behavior_df), replace = TRUE))%>%
	group_by(boot,
					 pid_3,
					 below_50_qual_strict)%>%
	summarize(
		prop_vote_general = weighted.mean(general_vote_dum, weight, na.rm = TRUE),
		mean_activist_index = weighted.mean(activist_6cat, weight, na.rm = TRUE), #6 measures of activism, additive index
#		prop_split_ticket = weighted.mean(split_ticket_dum, weight, na.rm = TRUE),
		prop_meetings = weighted.mean(meetings_dum, weight, na.rm = TRUE),
		prop_work_cand = weighted.mean(work_cand_dum, weight, na.rm = TRUE),
		prop_display_merch = weighted.mean(display_merch_dum, weight, na.rm = TRUE),
		prop_donate = weighted.mean(donate_dum, weight, na.rm = TRUE),
		prop_watch_campaign_tv = weighted.mean(watch_campaign_tv_dum, weight, na.rm = TRUE),
		prop_know_house_pre = weighted.mean(knows_house_pre_dum, weight, na.rm = TRUE),
#		prop_know_house_post = weighted.mean(knows_house_post_dum, weight, na.rm = TRUE),
		prop_talk_pol_most = weighted.mean(talk_politics_most_days_dum, weight, na.rm = TRUE),
		prop_vote_primary = weighted.mean(primary_vote_dum, weight, na.rm = TRUE),
#		prop_early_vote = weighted.mean(early_vote_dum, weight, na.rm = TRUE),
		prop_vote_inparty_house = weighted.mean(vote_inparty_house_dum, weight, na.rm = TRUE),
		prop_vote_outparty_pres = weighted.mean(vote_outparty_pres_dum, weight, na.rm = TRUE),
		prop_vote_thirdparty_pres = weighted.mean(vote_thirdparty_pres_dum, weight, na.rm = TRUE),
		prop_vote_inparty_pres = weighted.mean(vote_inparty_pres_dum, weight, na.rm = TRUE)
	)%>%
	glimpse()%>%
	write_rds("data/bootstrapped-behav-means-strict.rds")%>%
	write_csv("data/bootstrapped-behav-means-strict.csv")

#ci_df <- behavior_boot_df%>% #putting this here so I don't have to run the resample every time
ci_df <- read_rds("data/bootstrapped-behav-means-strict.rds")%>% #putting this here so I don't have to run the resample every time
	filter(!is.na(below_50_qual_strict))%>%
	select(-prop_display_merch, #comment this select() to include vars captured in activist_6cat
				 -prop_donate,
				 -prop_meetings,
				 -prop_work_cand,
#				 -prop_split_ticket,
#				 -prop_early_vote,
#				 -prop_know_house_post)%>%
)%>%
	pivot_wider(names_from = below_50_qual_strict,
							values_from = prop_vote_general:prop_vote_inparty_pres)%>%
	pivot_longer(prop_vote_general_cold:prop_vote_inparty_pres_warm, 
							 names_to = c("which_question", ".value"), 
							 names_pattern="(.*)_([a-z]*)")%>%
	group_by(boot,
					 pid_3,
					 which_question)%>%
	summarize(prop_dif = (cold-warm)/cold + warm,
						prop_dif_simple = cold-warm)%>%
	group_by(
		pid_3,
		which_question)%>%
	#	summarize(prop_se = parameters::standard_error(prop_dif))%>%
	summarize(prop_se = sd(prop_dif),
						simple_prop_se = sd(prop_dif_simple))%>%
	mutate(which_question = recode(which_question,
																 "prop_display_merch" = "Display Sticker/Pin",
																 "mean_activist_index" = "6-Item Campaign Participation Index",
																 "prop_donate" = "Donate to Candidate/Campaign",
																 "prop_early_vote" = "Vote Early",
#																 "prop_know_house_post" = "Know Party Won the House",
																 "prop_know_house_pre" = "Know Party in Control Before Election",
																 "prop_meetings" = "Attended Political Meetings/Rallies",
																 "prop_split_ticket" = "Voted Split Ticket",
																 "prop_talk_pol_most" = "Talk about Politics Most Days",
																 "prop_vote_general" = "Voted in General Election",
																 "prop_vote_primary" = "Voted in Primary Election",
																 "prop_vote_inparty_house" = "Voted for Inparty House",
																 "prop_vote_inparty_pres" = "Voted Inparty for President",
																 "prop_vote_outparty_pres" = "Voted Outparty for President",
																 "prop_vote_thirdparty_pres" = "Voted Thirdparty for President",
																 "prop_watch_campaign_tv" = "Watch Campaign Related TV",
																 "prop_work_cand" = "Worked for a Candidate/Campaign"
	))%>%
	glimpse()


####
## Now, calculating the simple proportions
###
behavior_means_df <- read_rds("data/tidy-cdf.rds")%>%
	filter(year >= 1978 & pid_3 != "Independent")%>%
	#	glimpse()
	#	group_by( pid_3, below_50_qual_strict, pres_election)%>% #strict cutoff
	group_by(pid_3, 
					 below_50_qual_strict)%>% 
	summarize(
		prop_vote_general = weighted.mean(general_vote_dum, weight, na.rm = TRUE),
		mean_activist_index = weighted.mean(activist_6cat, weight, na.rm = TRUE), #6 measures of activism, additive index
		prop_split_ticket = weighted.mean(split_ticket_dum, weight, na.rm = TRUE),
		prop_meetings = weighted.mean(meetings_dum, weight, na.rm = TRUE),
		prop_work_cand = weighted.mean(work_cand_dum, weight, na.rm = TRUE),
		prop_display_merch = weighted.mean(display_merch_dum, weight, na.rm = TRUE),
		prop_donate = weighted.mean(donate_dum, weight, na.rm = TRUE),
		prop_watch_campaign_tv = weighted.mean(watch_campaign_tv_dum, weight, na.rm = TRUE),
		prop_know_house_pre = weighted.mean(knows_house_pre_dum, weight, na.rm = TRUE),
		prop_know_house_post = weighted.mean(knows_house_post_dum, weight, na.rm = TRUE),
		prop_talk_pol_most = weighted.mean(talk_politics_most_days_dum, weight, na.rm = TRUE),
		prop_vote_primary = weighted.mean(primary_vote_dum, weight, na.rm = TRUE),
		prop_early_vote = weighted.mean(early_vote_dum, weight, na.rm = TRUE),
		prop_vote_inparty_house = weighted.mean(vote_inparty_house_dum, weight, na.rm = TRUE),
		prop_vote_outparty_pres = weighted.mean(vote_outparty_pres_dum, weight, na.rm = TRUE),
		prop_vote_thirdparty_pres = weighted.mean(vote_thirdparty_pres_dum, weight, na.rm = TRUE),
		prop_vote_inparty_pres = weighted.mean(vote_inparty_pres_dum, weight, na.rm = TRUE)
	)%>%
	select(-prop_display_merch, #comment this select() to include vars captured in activist_6cat
				 -prop_donate,
				 -prop_meetings,
				 -prop_work_cand,
				 -prop_split_ticket,
				 -prop_early_vote,
				 -prop_know_house_post)%>%
	pivot_wider(names_from = below_50_qual_strict,
							values_from = prop_vote_general:prop_vote_inparty_pres)%>%
	select(-ends_with("NA"))%>%
	pivot_longer(prop_vote_general_cold:prop_vote_inparty_pres_warm, 
							 names_to = c("which_question", ".value"), 
							 names_pattern="(.*)_([a-z]*)")%>%
	mutate(which_question = as.factor(which_question))%>%
	group_by(
		pid_3,
		which_question)%>%
	#	summarize(prop_dif = (cold - warm))%>%
	summarize(prop_difference = (cold - warm)/(cold+warm),
						prop_difference_simple = (cold - warm)#,
						#	se_dif
	)%>% 
	#	filter(!is.na(prop_dif))%>%
	select(-starts_with("var"))%>%
	mutate(which_question = recode(which_question,
																 "prop_display_merch" = "Display Sticker/Pin",
																 "mean_activist_index" = "6-Item Campaign Participation Index",
#																 "prop_donate" = "Donate to Candidate/Campaign",
#																 "prop_early_vote" = "Vote Early",
#																 "prop_know_house_post" = "Know Party Won the House",
																 "prop_know_house_pre" = "Know Party in Control Before Election",
																 "prop_meetings" = "Attended Political Meetings/Rallies",
#																 "prop_split_ticket" = "Voted Split Ticket",
																 "prop_talk_pol_most" = "Talk about Politics Most Days",
																 "prop_vote_general" = "Voted in General Election",
																 "prop_vote_primary" = "Voted in Primary Election",
																 "prop_vote_inparty_house" = "Voted for Inparty House",
																 "prop_vote_inparty_pres" = "Voted Inparty for President",
																 "prop_vote_outparty_pres" = "Voted Outparty for President",
																 "prop_vote_thirdparty_pres" = "Voted Thirdparty for President",
																 "prop_watch_campaign_tv" = "Watch Campaign Related TV",
																 "prop_work_cand" = "Worked for a Candidate/Campaign"))%>%
	mutate(which_question = reorder(which_question, prop_difference))%>%
	glimpse()

behavior_pooled_df <- full_join(behavior_means_df, ci_df)%>%
	mutate(which_question = reorder(which_question, prop_difference),
				 which_question_simple = reorder(which_question, prop_difference_simple),
				 sig_dum = case_when(prop_difference < 0 & prop_difference + 1.96*prop_se < 0 ~ TRUE,
				 										prop_difference > 0 & prop_difference - 1.96*prop_se > 0 ~ TRUE,
				 										TRUE ~ FALSE),
				 sig_dum_simple = case_when(prop_difference_simple < 0 & prop_difference_simple + 1.96*simple_prop_se < 0 ~ TRUE,
				 													 prop_difference_simple > 0 & prop_difference_simple - 1.96*simple_prop_se > 0 ~ TRUE,
				 													 TRUE ~ FALSE))%>%
	write_rds("data/tidy-behavior.rds")%>%
	glimpse()

###r

gg_strict_behavior_pooled <- ggplot(behavior_pooled_df, aes(x = prop_difference, y = fct_relabel(which_question, str_wrap, width = 20))) +
	geom_linerange(aes(xmin = prop_difference - 1.96*prop_se, xmax = prop_difference + 1.96*prop_se, color = pid_3), position = dodge) +
	#	geom_point(data=behavior_pooled_df[behavior_pooled_df$sig_dum == TRUE,],size=5, aes(position = pid_3), shape = 1, position = dodge) + #overlays a shape on sig 
	geom_point(aes(color = pid_3, shape = pid_3), size = 3, position = dodge) +
	scale_color_manual(values = c("Democrat" = "dodgerblue3",
																"Republican" = "firebrick3")) +
	geom_vline(xintercept = 0.00) +
	coord_cartesian(xlim = c(-.75, .75)) +
	scale_x_continuous(n.breaks = 10) +	
	theme(legend.position = c(0.8, 0.3)) +
	labs(x = "Difference",
			 y = "Question",
			 subtitle = "Behavior and Knowledge Items",
			 caption = "Bootstrapped 90% CI given by horizontal bars \n Cold Partisans < 50 inparty FT, Warm >= 70",
			 color = "Party",
			 shape = "Party")
gg_strict_behavior_pooled

ggsave("fig/gg-pooled-behavior-strict.png", gg_strict_behavior_pooled, width = 6, height = 8, units = "in")

#####
## Putting them into one plot
#####

#Removing redundant formatting from the plots
p1 <- gg_strict_opinion_pooled + theme(legend.position = "none",
																axis.title.x = element_blank(),
																axis.title.y = element_blank(),
																axis.text.x = element_blank())
p2 <- gg_strict_behavior_pooled + theme(
	axis.title.x = element_blank(),
	axis.title.y = element_blank())

pg <- plot_grid(p1, p2,  align = "v", nrow = 2, rel_heights = c(1/3, 2/3), label_x = "Difference") 
pg


gg_strict_pooled_combined <- grid.arrange(arrangeGrob(pg,
																							 top = text_grob("Difference in Proportion Between Cold and Warm Partisans", vjust = 1, face = "bold"),
																							 left = text_grob("Question Asked", rot = 90, vjust = 1, face = "bold"),
																							 bottom = text_grob("Cold - Warm / Total Affirmative", face = "bold")))
gg_strict_pooled_combined
ggsave("fig/gg-pooled-combined-strict.png", gg_strict_pooled_combined, width = 6, height = 10, units = "in")
