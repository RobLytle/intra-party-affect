library(tidyverse)
library(haven)

cdf <- read_rds("data/tidy-cdf.rds")%>%
  glimpse()


#
#####
#This histogram is just to get an idea of what the data look like, won't wind up in the doc
ggplot(cdf, aes(x = parties_therm_dif)) + 
  geom_histogram(binwidth = 10)

ggsave("fig/cdf-therm-dif-histogram.png", width = 10, height = 6, units = "in")


####
#This code chunk is intended to create a line graph of the yearly difference in party thermometers, by party strength
therm_year <- cdf%>%
  group_by(year, pid_str)%>%
  summarise(
    mean_therm_dif = mean(parties_therm_dif, na.rm = TRUE))%>%
  glimpse()


cdf_therm_line <- ggplot(therm_year, aes(x = year, y = mean_therm_dif, group = pid_str, color = pid_str))+
  geom_line()+
  stat_summary(fun.y = "mean")

cdf_therm_line
ggsave("fig/cdf-therm-line.png", plot = cdf_therm_line, width = 8, height = 6, units = "in")

####
#This code chunk is intended to create a line graph of the yearly difference in party ideology assessments, by partisan strength
ideo_year <- cdf%>%
  group_by(year, pid_str)%>%
  summarise(
    mean_ideo_dif = mean(parties_ideo_dif, na.rm = TRUE))%>%
  glimpse()



cdf_ideo_line <- ggplot(ideo_year, aes(x = year, y = mean_ideo_dif, group = pid_str, color = pid_str))+
  geom_line()+
  stat_summary(fun.y = "mean")

cdf_ideo_line
ggsave("fig/cdf-ideo-line.png", plot = cdf_ideo_line, width = 8, height = 6, units = "in")

