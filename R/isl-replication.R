library(tidyverse)
library(stargazer)
library(goji)
library(purrr)
library(GGally)

cdfa <- read_rds("data/tidy-cdf.rds")%>%
	mutate(parties_therm_dif = zero1(parties_therm_dif))%>%
	filter(year == 1988 | year == 2004 | year == 2016)%>%
	filter(pid_3 != "Independent")%>%
	mutate(therm_in = zero1(ifelse(pid_3 == "Democrat", therm_dem, therm_rep)))%>%
	mutate(therm_out = zero1(ifelse(pid_3 == "Democrat", therm_rep, therm_dem)))%>%
	mutate(lean_dummy = ifelse(pid_str == "Leaning Independent", 1, 0))%>%
	glimpse()
#  finalfit::missing_plot(cdfa)
cdfb <- cdfa%>%
	rowwise()%>%
	mutate(cult_att = mean(c(abortion, gayrights, women),na.rm = TRUE))%>%
	glimpse()

cdfc <- cdfb%>%
	rowwise()%>%
	mutate(econ_att = mean(c(ss, insurance, services, jobs), na.rm = TRUE))%>%
	glimpse()

cdfd <- cdfc%>%
	rowwise()%>%
	mutate(iwrpk_mean = mean(c(iwrpk_pre, iwrpk_post), na.rm = TRUE))%>%
	glimpse()

cdf <- cdfd%>%
	mutate(cult_att = if_else(pid_3_num == 1, (1-cult_att), cult_att))%>%
	mutate(econ_att = if_else(pid_3_num == 1, (1-econ_att), econ_att))%>%
	select(year,
				 pid_7_num,
				 pid_3,
				 cult_att, 
				 econ_att, 
				 strong_partisan,
				 south, 
				 white, 
				 female,
				 iwrpk_mean,
				 high_school, 
				 some_college, 
				 college_adv,
				 parties_therm_dif,
				 therm_in,
				 therm_out)%>%
	glimpse()

#finalfit::missing_plot(cdf)

#######
### Replication of original Table 3
###	
#######
cdf88 <- cdf%>%
	filter(year == 1988)%>%
	mutate(npa88 = parties_therm_dif)%>%
	mutate(out88 = therm_out)%>%
	glimpse()

#finalfit::missing_plot(cdf88)
cdf04 <- cdf%>%
	filter(year == 2004)%>%
	mutate(npa04 = parties_therm_dif)%>%
	mutate(out04 = therm_out)%>%
	glimpse()

rep88 <- cdf88%>%
	filter(pid_3 == "Republican")%>%
	glimpse()

dem88 <- cdf88%>%
	filter(pid_3 == "Democrat")%>%
	glimpse()

rep04 <- cdf04%>%
	filter(pid_3 == "Republican")%>%
	glimpse()

dem04 <- cdf04%>%
	filter(pid_3 == "Democrat")%>%
	glimpse()



rep88_model <- lm(npa88 ~ cult_att + econ_att + strong_partisan +
										south + white + female +
										iwrpk_mean +
										high_school + some_college + college_adv, data = rep88)

dem88_model <- lm(npa88 ~ cult_att + econ_att + strong_partisan + iwrpk_mean +
										female + south + white +
										high_school + some_college + college_adv, data = dem88)

rep04_model <- lm(npa04 ~ cult_att + econ_att + strong_partisan +
										south + white +  female +
										iwrpk_mean +
										high_school + some_college + college_adv, data = rep04)
dem04_model <- lm(npa04 ~ cult_att + econ_att + strong_partisan +
										south + white + female +
										iwrpk_mean +
										high_school + some_college + college_adv, data = dem04)

isl_model = stargazer(dem88_model, rep88_model, dem04_model, rep04_model, align = TRUE, no.space = FALSE,
											table.placement = "H",
											title = "Replicating ISL's Models",
											dep.var.labels = c("1988", "2004"),
											column.labels = c("Democrats", "Republicans", "Democrats", "Republicans"),
											covariate.labels = c("Cultural Attitudes", "Economic Attitudes", "Strong Partisan", "Political Knowledge",
																					 "Gender: Female", "Region: South", "Race: White",
																					 "High School", "Some College", "College or Advanced Degree"),
											column.sep.width = "-5pt",
											dep.var.caption = "Covariates of Net Partisan Affect",
											omit.stat=c("f", "rsq", "ser"),
											digits = 2
											)
cat(isl_model, sep = '\n', file = 'fig/isl-model.tex')

####
## Extension - Using 2004 and 2016; "Outparty FT"
##
####

#####
##	1
#####
cdf16<- cdf%>%
	filter(year == 2016)%>%
	mutate(npa16 = parties_therm_dif)%>%
	mutate(out16 = therm_out)%>%
	glimpse()

rep16 <- cdf16%>%
	filter(pid_3 == "Republican")%>%
	glimpse()

dem16 <- cdf16%>%
	filter(pid_3 == "Democrat")%>%
	glimpse()

rep88_ext_1 <- lm(out88 ~ cult_att + econ_att + therm_in +
										south + white + female +
										iwrpk_mean +
										high_school + some_college + college_adv, data = rep88)

dem88_ext_1 <- lm(out88 ~ cult_att + econ_att + therm_in  + iwrpk_mean +
										female + south + white +
										high_school + some_college + college_adv, data = dem88)

rep04_ext_1 <- lm(out04 ~ cult_att + econ_att + therm_in +
										south + white +  female +
										iwrpk_mean +
										high_school + some_college + college_adv, data = rep04)
dem04_ext_1 <- lm(out04 ~ cult_att + econ_att + therm_in +
										south + white + female +
										iwrpk_mean +
										high_school + some_college + college_adv, data = dem04)

rep16_ext_1 <- lm(out16 ~ cult_att + econ_att + therm_in +
										south + white + female +
										iwrpk_mean +
										high_school + some_college + college_adv, data = rep16)

dem16_ext_1 <- lm(out16 ~ cult_att + econ_att + therm_in +
										south + white + female +
										iwrpk_mean +
										high_school + some_college + college_adv, data = dem16)

ext_1_model = stargazer(dem88_ext_1, rep88_ext_1, dem04_ext_1, rep04_ext_1, dem16_ext_1, rep16_ext_1, align = TRUE, no.space = FALSE,
											table.placement = "H",
											title = "Original Models Using Outparty Affect as DV",
											dep.var.labels = c("1988", "2004", "2016"),
											column.labels = c("Dems", "Reps", "Dems", "Reps", "Dems", "Reps"),
											covariate.labels = c("Cultural Attitudes", "Economic Attitudes",
																					 "In-Party Warmth", "Political Knowledge",
																					 "Gender: Female", "Region: South", "Race: White",
																					 "High School", "Some College", "College/Adv. Degree"),
											column.sep.width = "-10pt",
											dep.var.caption = "Covariates of Out Party Affect",
											omit.stat=c("f", "ser", "rsq"),
											digits = 2
)
cat(ext_1_model, sep = '\n', file = 'fig/ext-1-model.tex')


#####
##	2
#####


rep88_ext_2 <- lm(npa88 ~ cult_att + econ_att + strong_partisan + 
										south + white + female +
										iwrpk_mean +
										high_school + some_college + college_adv, data = rep88)

dem88_ext_2 <- lm(npa88 ~ cult_att + econ_att + strong_partisan + iwrpk_mean +
										female + south + white +
										high_school + some_college + college_adv, data = dem88)

rep04_ext_2 <- lm(npa04 ~ cult_att + econ_att + strong_partisan +  
										south + white +  female +
										iwrpk_mean +
										high_school + some_college + college_adv, data = rep04)
dem04_ext_2 <- lm(npa04 ~ cult_att + econ_att + strong_partisan +  
										south + white + female +
										iwrpk_mean +
										high_school + some_college + college_adv, data = dem04)

rep16_ext_2 <- lm(npa16 ~ cult_att + econ_att + strong_partisan + 
										south + white + female +
										iwrpk_mean +
										high_school + some_college + college_adv, data = rep16)

dem16_ext_2 <- lm(npa16 ~ cult_att + econ_att + strong_partisan + 
										south + white + female +
										iwrpk_mean +
										high_school + some_college + college_adv, data = dem16)

ext_2_model = stargazer(dem88_ext_2, rep88_ext_2, dem04_ext_2, rep04_ext_2, dem16_ext_2, rep16_ext_2, align = TRUE, no.space = FALSE,
												table.placement = "H",
												title = "Original Models (Extended to 2016)",
												dep.var.labels = c("1988", "2004", "2016"),
												column.labels = c("Dems", "Reps", "Dems", "Reps", "Dems", "Reps"),
												covariate.labels = c("Cultural Attitudes", "Economic Attitudes", "Strong Partisan", 
																						 "Political Knowledge",
																						 "Gender: Female", "Region: South", "Race: White",
																						 "High School", "Some College", "College/Adv. Degree"),
												column.sep.width = "-10pt",
												dep.var.caption = "Covariates of Net Partisan Affect",
												omit.stat=c("f", "ser", "rsq"),
												digits = 2
)
cat(ext_2_model, sep = '\n', file = 'fig/ext-2-model.tex')

###
#	3
###

# rep16_ext_3 <- lm(out16 ~ cult_att + econ_att + strong_partisan + therm_in +
# 										south + white + female +
# 										iwrpk_mean +
# 										high_school + some_college + college_adv, data = rep16)
# 
# dem16_ext_3 <- lm(out16 ~ cult_att + econ_att + strong_partisan + therm_in +
# 										south + white + female +
# 										iwrpk_mean +
# 										high_school + some_college + college_adv, data = dem16)
# 
# ext_3_model = stargazer(dem04_ext_3, rep04_ext_3, dem16_ext_3, rep16_ext_3, align = TRUE, no.space = FALSE,
# 												table.placement = "H",
# 												title = "Extending ISL's Models",
# 												dep.var.labels = c("2004", "2016"),
# 												column.labels = c("Democrats", "Republicans", "Democrats", "Republicans"),
# 												covariate.labels = c("Cultural Attitudes", "Economic Attitudes", "Strong Partisan", "In-Party Therm",
# 																						 "Political Knowledge",
# 																						 "Gender: Female", "Region: South", "Race: White",
# 																						 "High School", "Some College", "College or Advanced Degree"),
# 												column.sep.width = "-5pt",
# 												dep.var.caption = "Covariates of Outparty Affect",
# 												omit.stat=c("ser", "rsq"),
# 												digits = 2
# )
# cat(ext_3_model, sep = '\n', file = 'fig/ext-3-model.tex')
# ME plots

#interplot::interplot(dem16_ext_2, var1 = econ_att, var2 = therm_in, hist = TRUE)
