library(tidyverse)
library(anesr)
data(timeseries_1980) #these import the datasets from anesr
data(timeseries_1988)
data(timeseries_1992)
data(timeseries_cum)

df_cdf <- timeseries_cum
df_80<-timeseries_1980
df_88<-timeseries_1988
df_92<-timeseries_1992

df_tid_80 <- df_80 %>%
	select(case = V800004,
				 V800371:V800373,
				 therm_dem = V800168,
				 therm_rep = V800169) %>% 
	glimpse()



df_tid_92 <- df_92 %>% 
select(V92301:V92304,
				therm_dem = V923317,
				therm_rep = V923318) %>% 
	glimpse()

#Going to wrangle all remaining years with primary info

# CDF Case-id number: VCF0006

#1992 
# Primary Votes: VAR 92301-92304
# Therms: D-923317
#					R-923318
# Weight: 927000

#1988
# Primary Votes: 880148-880151
# Therms: D-880164
#					R-880165
#	Weight: none
#	Race: 880412
# Income: 880520
#	Ideology: S-880231
#						D-880235
#						R-880234
#	Sex: 880413
#	Age: 880417
# Education: 880422

#1980
# Primary Votes: 800371-800373
# Therms: D-800168
#					R-800169

tidy_cdf <- read_rds("data/tidy-cdf.rds") %>%
	filter(year == 1980) %>% 
	select(case) %>% 
	glimpse()
