library(tidyverse)
library(ggthemes)
library(kableExtra)

#####
## This script was included to illustrate a problem dealing with factors.
## I'm leaving it in for posterity, but the problem has been resolved
#####

rds <- read_rds("data/tidy-2016.rds")%>%
  glimpse()


numeric_plot <- ggplot(rds, aes(x = primary_vote_choice, y = pre_self_ideo_7_num))+
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  theme_minimal()+
  labs(y = "Average Ideological Self Assessment",
       x = "Primary Vote Choice",
       title = "Average Ideological Self Assessment",
       subtitle = "By Respondent's Vote Choice in the 2016 Primary (using a numeric variable)")

numeric_plot #calls the ggplot object, displaying chart in console window
gg

as_numeric_plot <- ggplot(rds, aes(x = primary_vote_choice, y = as.numeric(pre_self_ideo_7)))+
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  theme_minimal()+
  labs(y = "Average Ideological Self Assessment",
       x = "Primary Vote Choice",
       title = "Average Ideological Self Assessment",
       subtitle = "By Respondent's Vote Choice in the 2016 Primary (using as.numeric() on a factor)")

as_numeric_plot  #calls the ggplot object, displaying chart in console window

#In my understanding of ordered factors, both of these plots should yield the same results, but they are radically different.
#I assume the problem lies in `wrangle-2016.R`, but I'm not sure why this is happening.

glimpse(df)
