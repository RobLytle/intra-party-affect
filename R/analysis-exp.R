library(lubridate)
library(MASS)
library(brglm)
library(tidyverse)
set.seed(1234)

exp_df <- read_csv("data/raw/ap-experiment-08-21.csv")%>%
#	glimpse()#%>%
	select(StartDate,
				 party_id,
				 -party_id_1,
					lean_party,
					d_loss,
					d_win,
					r_loss,
					r_win,
					control,
					therm_dem = therms_1,
					therm_rep = therms_2,
					therm_ind = therms_3,
					satisfied_democracy,
					trust_gov,
					r_gen_vote_loss:d_gen_vote_win_con,
				 comments)%>%
	.[-1,]%>%
	.[-1,]%>% #trimming off the label rows from qualtrics
	mutate(pid_3 = case_when(party_id != "Independent" & party_id != "Something Else" ~ party_id,
													 lean_party == "Neither Republican or Democrat" ~ "Independent",
													 lean_party == "Democratic" ~ "Democrat",
													 lean_party == "Republican" ~ "Republican",
													 lean_party == "Something Else" ~ "Independent",
													 TRUE ~ lean_party),
				 loss_dum = if_else(!is.na(d_loss) | !is.na(r_loss), 1, 0),
				 win_dum = if_else(!is.na(d_win) | !is.na(r_win), 1, 0),
				 control_dum = if_else(!is.na(control), 1, 0),
				 loss_dum_nc = if_else(control_dum == "1", NA_real_, loss_dum),
				 group = as.factor(case_when(loss_dum == "1" ~ "Loss",
				 									win_dum == "1" ~ "Win",
				 									control_dum == "1" ~ "Control")),
				 date = ymd_hms(StartDate),
				 therm_inparty = as.numeric(case_when(pid_3 == "Democrat" ~ therm_dem,
				 													pid_3 == "Republican" ~ therm_rep,
				 													TRUE ~ NA_character_)),
				 therm_outparty = as.numeric(case_when(pid_3 == "Democrat" ~ therm_rep,
				 																		 pid_3 == "Republican" ~ therm_dem,
				 																		 TRUE ~ NA_character_)),
				 vote_likely = factor(case_when(pid_3 == "Democrat" & group == "Loss" ~ d_gen_vote_loss,
				 												pid_3 == "Democrat" & group == "Win" ~ d_gen_vote_win_con,
				 												pid_3 == "Republican" & group == "Loss" ~ r_gen_vote_loss,
				 												pid_3 == "Republican" & group == "Win" ~ d_gen_vote_loss,
				 												TRUE ~ NA_character_),
				 										 levels = c("Extremely unlikely",
				 										 					 "Somewhat unlikely",
				 										 					 "Neither likely nor unlikely", 
				 										 					 "Somewhat likely",
				 										 					 "Extremely likely")),
				 satisfied_democracy = factor(satisfied_democracy,
				 														 levels = c("Extremely Dissatisfied",
				 														 					 "Somewhat Dissatisfied",
				 														 					 "Neither Satisfied nor Dissatisfied", 
				 														 					 "Somewhat Satisfied",
				 														 					 "Extremely Satisfied")),
				 trust_gov = factor(trust_gov,
				 									 levels = c("Not at all",
				 									 					 "A little",
				 									 					 "A moderate amount",
				 									 					 "A lot",
				 									 					 "A great deal")))%>%
	filter(date > ymd("2021-08-19"))%>%
	filter(!is.na(group))%>%
#	filter(comments == "test")%>%
	glimpse()

group_df <- exp_df%>%
	filter(pid_3 != "Independent")%>%
	group_by(group,
					 pid_3)%>%
	summarize(ft_in = mean(therm_inparty),
						se_in = sd(therm_inparty)/sqrt(length(therm_inparty)))%>%
	glimpse()

m1 <- exp_df%>%
	filter(pid_3 != "Independent")%>%
#	filter(pid_3 == "Republican")%>%
lm(therm_inparty ~ group, data = .)

summary(m1)

m2 <-exp_df%>%
	filter(pid_3 != "Independent")%>%
	select(vote_likely,
				 group,
				 loss_dum_nc)%>%
	glimpse()
	MASS::polr(vote_likely ~ group, data = ., Hess = TRUE)

ctable <- coef(summary(m2))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE * 2)

table <- cbind(ctable, "p_value" = p)%>%
	glimpse()