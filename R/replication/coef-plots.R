library(tidyverse)
library(stargazer)
library(knitr)
library(kableExtra)

dems88 <- read_csv("replication-data/coefs/dem88coefs.csv")%>%
  glimpse()

reps88 <- read_csv("replication-data/coefs/rep88coefs.csv")%>%
  glimpse()

dems04 <- read_csv("replication-data/coefs/dem04coefs.csv")%>%
  glimpse()

reps04 <- read_csv("replication-data/coefs/rep04coefs.csv")%>%
  glimpse()

dems1 <- dems88 %>%
	kable(format = "latex",
				digits = 2,
				booktabs = TRUE
	)%>%
#	row_spec(0,bold=TRUE)%>%
	kable_styling(full_width = FALSE)
dems1
cat(dems1, file = "fig/dems1.tex")

###
####
dems2 <- dems04 %>%
	kable(format = "latex",
				digits = 2,
				booktabs = TRUE
	)%>%
	#	row_spec(0,bold=TRUE)%>%
	kable_styling(full_width = FALSE)
cat(dems2, file = "fig/dems2.tex")

######
reps1 <- reps88 %>%
	kable(format = "latex",
				digits = 2,
				booktabs = TRUE
	)%>%
	#	row_spec(0,bold=TRUE)%>%
	kable_styling(full_width = FALSE)

cat(reps1, file = "fig/reps1.tex")
######
reps2 <- reps04 %>%
	kable(format = "latex",
				digits = 2,
				booktabs = TRUE
	)%>%
	#	row_spec(0,bold=TRUE)%>%
	kable_styling(full_width = FALSE)
cat(reps2, file = "fig/reps2.tex")





kable(dems88, format = "latex")
