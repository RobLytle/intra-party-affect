library(tidyverse)
library(stargazer)


dems88 <- read_csv("replication-data/coefs/dem88coefs.csv")%>%
  glimpse()

reps88 <- read_csv("replication-data/coefs/rep88coefs.csv")%>%
  glimpse()

dems04 <- read_csv("replication-data/coefs/dem04coefs.csv")%>%
  glimpse()

reps04 <- read_csv("replication-data/coefs/rep04coefs.csv")%>%
  glimpse()

stargazer(dems88)
