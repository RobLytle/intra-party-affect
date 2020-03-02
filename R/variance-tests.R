library(tidyverse)
library(knitr)
library(kableExtra)

#######
#Variance Tests between 2004 and 2016
#######



### Wrangling Data for Levene and Fligner Tests
levene_df <- read_rds("data/tidy-cdf.rds")%>%
  filter(pid_3 == "Democrat")%>%
  filter(year == (2016) | year ==(2004))%>%
  select(year,
         pid_3,
         therm_dem,
         therm_rep)%>%
  mutate(year = as.factor(year))%>%
  glimpse()

#Shapiro test of normality
# (These aren't)
shapiro_04 <- levene_df%>%
  filter(year == 2004)%>%
  glimpse()

shapiro_16 <- levene_df%>%
  filter(year == 2016)%>%
  glimpse()

shapiro_results_04 <- shapiro.test(shapiro_04$therm_dem)%>%
  print()
shapiro_results_16 <- shapiro.test(shapiro_16$therm_dem)%>%
  print()
#Distributions are Not normal, so Levene and Fligner-Killeen tests are used


# Running in sjmisc and in car::
#rstatix::levene_test()
rstatix::levene_test(levene_df, therm_dem ~ year)
#car::leveneTest
lev_car <- car::leveneTest(therm_dem ~ year, data = levene_df, center = mean)

#  glimpse()%>%
#  kable(format = "latex",
#        booktabs = TRUE,
#        escape = FALSE)%>%
#  row_spec(0,bold=TRUE)%>%
#  column_spec(1:3, width = "3cm")%>%
#  kable_styling(full_width = FALSE)
#cat(lev_results, file = "fig/table-levene.tex")

##Fligner
fligner_results <- fligner.test(formula = (levene_df$therm_dem ~ levene_df$year), data = levene_df)
print(fligner_results)
  kable(fligner_results, format = "latex",
        booktabs = TRUE,
        escape = FALSE)%>%
  row_spec(0, bold = TRUE)%>%
  column_spec(1:3, width = "3cm")%>%
  kable_stying(full_width = FALSE)

lev_table_gathered <- lev_results_gathered%>%
kable(format = "latex",
      booktabs = TRUE,
      escape = FALSE)%>%
  row_spec(0,bold=TRUE)%>%
  column_spec(1:3, width = "3cm")%>%
  kable_styling(full_width = FALSE)

  cat(lev_table, file = "fig/table-levene-gathered.tex") # save levene kable

#Now we're going to perform Levene tests on Sanders and Clinton Supporters, as well as Clinton/Obama Supporters
#Load primaries data  
  
  #create DF of voters and non-voters
  variance_2016_nonvoters <- read_rds("data/tidy-primaries.rds")%>%
    filter(pre_pid_3 == "Democrat")%>%
    filter(year == "2016")%>%
    subset(primary_vote_choice %in% c("Hillary Clinton", "Bernie Sanders", "Didn't Vote"))%>%
    glimpse()%>%
    select(pre_therm_dem,
           primary_vote_choice)%>%
    glimpse()

  #Levene
  lev_nonvoters <- rstatix::levene_test(variance_2016_nonvoters, pre_therm_dem ~ primary_vote_choice)%>%
    print()
 
  #Fligner-Killeen
  flig_nonvoters <- fligner.test(formula = (pre_therm_dem ~ primary_vote_choice), data = variance_2016_nonvoters)%>%
    print()
  #
  
  
  variance_2016_voters <- read_rds("data/tidy-primaries.rds")%>%
    filter(pre_pid_3 == "Democrat")%>%
    filter(year == "2016")%>%
    subset(primary_vote_choice %in% c("Hillary Clinton", "Bernie Sanders"))%>%
    drop_na(pre_therm_dem)%>%
   # filter(primary_vote_choice == c("Hillary Clinton", "Bernie Sanders"))%>%
    select(pre_therm_dem,
           primary_vote_choice)%>%
    glimpse()
  
  #Levene
  lev_voters <- rstatix::levene_test(variance_2016_voters, pre_therm_dem ~ primary_vote_choice)%>%
    print()
  #Flig with voters
  flig_voters <- fligner.test(formula = (pre_therm_dem ~ primary_vote_choice), data = variance_2016_voters)%>%
    print()
  
  ## All tests run in this section are significant at p < 0.001
  #############
  ###T-test, to determine if the mean Dem Therm of Clinton Supporters is different from Sanders
  ############
  
  ttest_2016 <- t.test(pre_therm_dem ~ primary_vote_choice, alternative = "greater", data = variance_2016_voters)%>%
    print()
  
  wilcox_2016 <- wilcox.test(pre_therm_dem ~ primary_vote_choice, data = variance_2016_voters)%>%
    print()
  
  #both the wilcox and ttest are signficant, p < .0001
  ################# 
  
  # I'm not sure if this implementation is correct. I'm not sure what running two grouping variables does to the
  #results. See above for a test I have more confidence in.
  #wrangle data to only 2004 and 2016
  # levene_gathered_df <- read_rds("data/tidy-cdf.rds")%>%
  #   filter(pid_3 == "Democrat")%>%
  #   filter(year == (2016) | year ==(2004))%>%
  #   select(year,
  #          pid_3,
  #          therm_dem,
  #          therm_rep)%>%
  #   gather(therm_dem, therm_rep,
  #          key = "therm_towards",
  #          value = "therm_score")%>%
  #   mutate(therm_towards = recode(therm_towards,
  #                                 "therm_rep" = "Republicans",
  #                                 "therm_dem" = "Democrats"))%>%
  #   mutate(therm_towards = as.factor(therm_towards))%>%
  #   mutate(year = as.factor(year))%>% # Grouping variable for Levene Test must be Factor
  #   glimpse()
  
  #visual inspection of thermometer scores
  #ggplot(levene_df, aes(x = therm_dem, y = ..density..))+
  #  geom_histogram()+
  #  facet_wrap(vars(year))
  
  #lev_results_gathered <- leveneTest(therm_score ~ year*therm_towards, data = levene_df, center = mean)%>%
  #  glimpse()