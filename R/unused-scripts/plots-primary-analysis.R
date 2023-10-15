library(tidyverse)
library(tidymodels)
theme_set(theme_minimal())
library(ggthemes)
library(ggExtra)
library(kableExtra)
library(scales)
library(gridExtra)
library(ggpubr)
library(ggridges)
set.seed(1234)

########
#function to split into training and test dfs in one go
split_rob <- function(df, vars, name, ...) {
	split <- initial_split(df, ...)
	
	train_df <- training(split)%>%
		glimpse()
	
	test_df <- testing(split)%>%
		glimpse()
	
	assign(paste(name, sep = "_", "train_df"), train_df, envir = .GlobalEnv)
	assign(paste(name, sep = "_", "test_df"), test_df, envir =  .GlobalEnv)
}


split_rob(df_primaries, name = "prim", strata = year, prop = 1/2) #write 2 dfs for training and testing

nest_train <- prim_train_df%>%
	nest(-pid_3)%>%
	glimpse()

nest_test <- prim_test_df%>%
	nest(-pid_3)%>%
	glimpse()



primary_rec <-
	recipe(therm_inparty ~ therm_outparty + primary_vote_simple + ideo_self_in_dif + weight + pid_3, data = prim_train_df)%>%
	update_role(weight, new_role = "case weight")%>%
	update_role(pid_3, new_role = "dataset split variable")%>%
	step_dummy(all_nominal(), -all_outcomes())
summary(primary_rec)

lm_mod <- 
	linear_reg(mode = "regression") %>% 
	set_engine("lm")

primary_wrkf <- workflow()%>%
	add_model(lm_mod)%>%
	add_recipe(primary_rec)
primary_wrkf

prim_fit <- primary_wrkf%>%
	fit(data = prim_train_df)

prim_fit%>%
	pull_workflow_fit()%>%
	tidy()



mutate(predictions = map(data, possibly(predict_hpc, otherwise = NA)))
fit_1 <-
	lm_mod%>%
	fit(therm_inparty ~ therm_outparty + primary_vote_simple + ideo_self_in_dif, data = df_primaries)%>%
	tidy()%>%
	glimpse()

mod_1 <- df_primaries%>%
	nest(-pid_3)%>%
	mutate(fit = map(data, ~
									 	lm(therm_inparty ~ therm_outparty + primary_vote_simple + ideo_self_in_dif, data = .)),
				 glance = map(fit, glance),
				 tidy = map(fit, tidy),
				 augment = map(fit, augment))

full_glance <- unnest(mod_1, glance)
full_tidy <- unnest(mod_1, tidy)
full_augment <- unnest(mod_1, augment)


full_tidy%>%
	select(pid_3,
				 term:p.value)%>%
	select(-statistic)%>%
	pivot_wider(names_from = term,
							values_from = c("estimate", "std.error", "p.value"))%>%
	glimpse() -> me_coefs

me_df <- full_augment%>%
	select(pid_3,
				 therm_inparty:ideo_self_in_dif)%>%
	inner_join(me_coefs)%>%
	select(-contains("Intercept"))%>%
	glimpse()
	# mutate_at(vars(therm_inparty:ideo_self_in_dif),
	# 					list(#me =  ~estimate*.,
	# 						def = ~exp(.))) %>% #list(log = ~if_else(. > 0, log(.)
	glimpse()

## Tuning Models
dems_df <- full_tidy%>%
	filter(pid_3 == "Democrat")







data("hpc_data")

hpc_data <- hpc_data %>%
	select(-protocol, -class)

lm_mod <-
	linear_reg(mode = "regression") %>%
	set_engine("lm")

wf <-
	workflow() %>%
	add_model(lm_mod)

## big function of model fitting and predicting
predict_hpc <- function(df) {
	split <- initial_split(df)
	train_df <- training(split)
	test_df <- testing(split)
	
	#create recipe
	recipe_train <-
		recipe(compounds ~., data = train_df) %>%
		step_normalize(all_predictors())
	
	#fit workflow on train data
	fit_wf <-
		wf %>%
		add_recipe(recipe_train) %>%
		fit(data = train_df)
	
	#predict on test data
	predict(fit_wf, test_df) 
	
}

hpc_nested <- hpc_data %>%
	group_by(day) %>%
	nest()

hpc_nested %>%
	mutate(predictions = map(data, possibly(predict_hpc, otherwise = NA)))