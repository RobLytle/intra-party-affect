library(tidyverse)

#This script binds all the individuals into the same df. Equivalent to the *-trim.R files.
#Because they have more columns, the reinterview panels are handled in naes-2000-wrangle.R

naes_ia <- rio::import("data/raw/naes/2000/NAES 2000 IA Panel Data.sav")%>%
	glimpse()

naes_nh <- rio::import("data/raw/naes/2000/NAES 2000 NH Panel Data.sav")%>%
	glimpse()

naes_sc <- rio::import("data/raw/naes/2000/NAES 2000 SC Panel Data.sav")%>%
	glimpse()

naes_super <- rio::import("data/raw/naes/2000/NAES 2000 Super Panel Data.sav")%>%
	glimpse()

naes_mi <- rio::import("data/raw/naes/2000/NAES 2000 MI Panel Data.sav")%>%
	glimpse()

naes_dem_conv <- rio::import("data/raw/naes/2000/NAES 2000 Dem Conv Panel Data.sav")%>%
	glimpse()

naes_rep_conv <- rio::import("data/raw/naes/2000/NAES 2000 GOP Conv Panel Data.sav")%>%
	glimpse()

naes_oct_3 <- rio::import("data/raw/naes/2000/NAES 2000 3 Oct Deb Panel Data.sav")%>%
	glimpse()

naes_oct_3 <- rio::import("data/raw/naes/2000/NAES 2000 3 Oct Deb Panel Data.sav")%>%
	glimpse()

naes_oct_11 <- rio::import("data/raw/naes/2000/NAES 2000 11 Oct Deb Panel Data.sav")%>%
	glimpse()

naes_oct_17 <- rio::import("data/raw/naes/2000/NAES 2000 17 Oct Deb Panel Data.sav")%>%
	glimpse()

naes_general_elect <- rio::import("data/raw/naes/2000/NAES 2000 Elect Panel Data.sav")%>%
	glimpse()


naes_2000 <- rbind(naes_ia,
									 naes_nh,
									 naes_sc,
									 naes_super,
									 naes_mi,
									 naes_dem_conv,
									 naes_rep_conv,
									 naes_oct_3,
									 naes_oct_11,
									 naes_oct_17,
									 naes_general_elect)%>%
	select(contains("key"),
				 contains("date"),
				 contains("r11"), #intend to vote for, rep
				 contains("r12"), #intend to vote, dem
				 contains("r17"), #actual vote for rep
				 contains("r18"), #actual vote for dem
				 contains("v01"), #pid_str
				 contains("v02"), #pid3
				 -ends_with("$"))%>%
	write_rds("data/raw/naes-2000-all.rds")%>%
	write_csv("data/raw/naes-2000-all.csv")%>%
	glimpse()