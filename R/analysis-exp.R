library(lubridate)
library(MASS)
library(brglm)
library(tidyverse)
library(modelsummary)
library(AER)
set.seed(1234)

# Function to extract p from polr
tidy_custom.polr <- function(x, ...) {
	s <- coeftest(x)
	out <- data.frame(
		term = row.names(s),
		p.value = s[, "Pr(>|z|)"])
	out
}

exp_df <- read_csv("data/raw/ap-experiment-08-22.csv")%>%
#	glimpse()#%>%
	select(StartDate,
#				 party_id...66,
				 party_id = party_id...18, #not sure why this is happening... something on the qtrix side of things. THis seems to be the correct pid variable.
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
				 														 levels = c("Very Dissatisfied",
				 														 					 "Somewhat Dissatisfied",
				 														 					 "Neither Satisfied nor Dissatisfied",
				 														 					 "Somewhat Satisfied",
				 														 					 "Very Satisfied")),
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
						se_in = sd(therm_inparty)/sqrt(length(therm_inparty)),
						n = n())%>%
	glimpse()

m1 <- exp_df%>%
	filter(pid_3 != "Independent")%>%
#	glimpse()
#	filter(pid_3 == "Republican")%>%



summary(m1)

m_df <- exp_df%>%
	filter(pid_3 != "Independent")%>%
	select(vote_likely,
				 therm_inparty,
				 therm_outparty,
				 group,
				 loss_dum_nc,
				 satisfied_democracy,
				 trust_gov)

ggplot(m_df, aes(x = therm_inparty)) +
	geom_density() + 
	facet_wrap(vars(group))


# In and outparty therms

cm_1 <- c("(Intercept)" = "Constant", "groupLoss" = "Loss", "groupWin" = "Win" )

list("Inparty" = lm(therm_inparty ~ group, data = m_df),
		 "Outparty" = lm(therm_outparty ~ group, data = m_df))%>%
	modelsummary(output = "gt",
							 stars = TRUE,
							 coef_map = cm_1)


# comparing Winners and Losers Political Optimism
cm_2 <- c("loss_dum_nc" = "Primary Loss")
gm_2 <- tibble::tribble(
	~raw,        ~clean,          ~fmt,
	"nobs",      "N",             0,
	"AIC", "AIC", 2,
	"logLik" , "Log Likelihood", 2)

list("Vote General" = polr(vote_likely ~ loss_dum_nc, data = m_df, Hess = TRUE),
			 "Trust Gov." = polr(trust_gov ~ loss_dum_nc, data = m_df, Hess = TRUE),
			 "Democ. Satisfaction" = polr(satisfied_democracy ~ loss_dum_nc, data = m_df, Hess = TRUE))%>%
modelsummary(coef_map = cm_2,
						 gof_map = gm_2,
						 stars = TRUE)



	
