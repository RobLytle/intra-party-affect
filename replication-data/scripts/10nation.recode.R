##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##   											##
##      Polarization
##		Ten Nation Dataset (US and UK here)
##		Last Edited: 2/18/11  
##   	Gaurav Sood								##
##    Last Edited: 2/23/20
##    Rob Lytle
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++


# Set Working dir.
	#setwd(basedir)

# Sourcing Common Functions
	#source("func/func.R")
	#source("func/match.R")

## Load Data
if(FALSE){
	us10 <- spss("replication-data/10nat/country/US/us.sav")
	save(us10, file="replication-data/10nat/us10.rdata")
}

# 
# United States
# ~~~~~~~~~~~~~~~~~~~~~~~~~

# Load data
	load("replication-data/10nat/us10.rdata")

# PID
	#pid7 7 point Party ID
	#pid3 3 point party ID
	#1 dem, #2 rep # 3 ind
	us10$rd <- car::recode(us10$pid3, "1='dem'; 2='rep'; else=NA")

#Feeling if son or daughter married someone who supported the
	#PARTY Party
	#Count Code Label
	#----- ---- -----
	#		114 1 I'd be very unhappy
	#		127 2 Somewhat unhappy
	#		498 3 Neither happy nor unhappy
	#		74 4 Somewhat happy
	#		185 5 Very happy
	#		2 8 Skipped
	#		0 9 Not Asked
	us10$marry.r <- ifelse(clean(us10$marry_text)=='republican', us10$marry, NA)
	us10$marry.d <- ifelse(clean(us10$marry_text)=='democratic', us10$marry, NA)
	us10$marry.out <- out(us10$marry.d, us10$marry.r, !is.na(us10$pid3) & us10$pid3 == 2, !is.na(us10$pid3) & us10$pid3 == 1)

	with(subset(us10, us10$rd=='dem'), xtabs(weight ~ rd + marry.r)/rowSums(xtabs(weight ~ rd + marry.r)))
	with(subset(us10, us10$rd=='rep'), xtabs(weight ~ rd + marry.d)/rowSums(xtabs(weight ~ rd + marry.d)))

# Therm
	#therm1 Thermometer - Democratic Party
	#therm2 Thermometer - Republican Party
	us10$d.therm <- car::recode(us10$therm1, "997=NA")
	us10$d.thermr <- car::recode(us10$therm1, "997=50")
	us10$r.therm <- car::recode(us10$therm2, "997=NA")
	us10$r.thermr <- car::recode(us10$therm2, "997=50")

	#therm3 Thermometer - Evangelical Protestants
	#therm4 Thermometer - Catholics
	#therm5 Thermometer - African-Americans
	#therm6 Thermometer - White Americans
	#therm7 Thermometer - Arab-Americans

# Sociodem
	#internet Internet access
	#birthyr Birth Year
	#gender Gender

# Save File
	save(us10, file="polar/data/us10r.rdata")

# 
# United Kingdom
# ~~~~~~~~~~~~~~~~~~~~~~~~~

# Load data
	load("replication-data/10nat/ten.rdata")
	uk10 <- subset(ten, !is.na(ten$weightuk))

# PID
	# 1 conservative, 2 lib dem, 3 labour
	# sum(uk10$dpiduk==3, na.rm=t) # 269 codebook confirmed
	uk10$cl <- car::recode(uk10$dpiduk, "1='con'; 3='lab'; else=NA")

# Marriage
	uk10$marry.r <- ifelse(clean(uk10$amarryt)=='conservative', us10$marry, NA)
	uk10$marry.d <- ifelse(clean(uk10$amarryt)=='labour', us10$marry, NA)
	
	uk10$marry.out <- out(uk10$marry.d, uk10$marry.r, !is.na(uk10$cl) & us10$cl == 'con', !is.na(uk10$cl) & uk10$cl == 'lab')

# Save File
	save(uk10, file="replication-data/10nat/uk10.rdata")
	