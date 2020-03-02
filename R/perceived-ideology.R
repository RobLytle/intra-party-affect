library(tidyverse)
library(ggthemes)
library(kableExtra)

df <- read.csv("data/tidy-2016.csv")%>%
 # mutate(parties_ideo_dif = as.numeric(parties_ideo_dif))%>%
#varx <- varx=na.omit(df$parties_ideo_dif)
#varx <- as.data.frame(varx)


primary_plot <- ggplot(df, aes(x = primary_vote_choice, y = parties_ideo_dif))+
geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  theme_minimal()+
  labs(y = "Average Ideological Distance",
       x = "Primary Vote Choice",
       title = "Average Difference in Reported Likert Score Ideology of Republicans and Democrats",
       subtitle = "By Respondent's Vote Choice in the 2016 Primary")

primary_plot

#ggsave("fig/perceived-ideology.png", width = 10, height = 6, units = "in")




