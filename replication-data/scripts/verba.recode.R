##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##   													##
##      Polarization
##		Verba Dataset	
##		Last Edited: 2/28/10 		
##   	Gaurav Sood										##
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

# Set Working dir.
	#setwd("C:/Users/gsood/Dropbox/")

# Sourcing Common Functions
	#source("func/func.R")
	#source("polar/polr.func.R")

## Verba Data
	verba <- foreign::read.spss("replication-data/verba/verba.sav")
	verba <- as.data.frame(verba)
	names(verba) <- tolower(names(verba))

	#as.data.frame(attributes(v)["variable.labels"])

# Country
#V2                      COUNTRY
#us <- subset(verba, tolower(verba$v2)=="united states")
#V4                       WEIGHT
names(verba)[4] <- "weight"
#V161                    R'S SEX
#V162                   R'S RACE

# Pid var 066 -
# US  10.  DEMOCRATIC PARTY SUPPORTER; 206       20.  REPUBLICAN PARTY SUPPORTER; 169       30.  INDEPENDENT 
# UK  10.  CONSERVATIVE     20.  LABOUR   30.  LIBERAL 
# DK  10.  CDU/CSU      20.  SPD      30.  FDP  
# uk v71
# pid leaner 067      1.  LEANS TOWARD DEMOCRATIC PARTY; 2.  LEANS TOWARD REPUBLICAN PARTY;     
#5.  LEANS TOWARD NO PARTY    7.  LEANS TOWARD OTHER PARTY   

# verba$v70,
temp <- paste(verba$v66, verba$v67, sep="")
verba$pid5 <- NA
verba$pid5 <- ifelse(tolower(verba$v2)=="united states", car::recode(temp, "100=1; 301=2; 305=3; 302=4; 200=5; else=NA"), verba$pid5 )
temp2 <- paste(verba$v66, verba$v71, verba$v72, sep="")
verba$pid5 <- ifelse(tolower(verba$v2)=="united kingdom", car::recode(temp2, "unique(temp2)[grep(20, unique(temp2))]=1; unique(temp2)[grep(10, unique(temp2))]=5;else=NA"), verba$pid5)

# Party ID
	verba$partid <- NA
	verba$partid[tolower(verba$v2)=="united states" &  verba$pid5 > 3 & !is.na(verba$pid5)]  <- "Republican"
	verba$partid[tolower(verba$v2)=="united states" &  verba$pid5 < 3 & !is.na(verba$pid5)]  <- "Democrat"
	verba$partid[tolower(verba$v2)=="united kingdom" &  verba$pid5 > 3 & !is.na(verba$pid5)] <- "Conservative"
	verba$partid[tolower(verba$v2)=="united kingdom" &  verba$pid5 < 3 & !is.na(verba$pid5)] <- "Labour"

verba$src <- NA
verba$src[tolower(verba$v2)=="united states"] <- "US"
verba$src[tolower(verba$v2)=="united kingdom"] <- "UK"

#verba$pid5[tolower(verba$v2)=="united kingdom"] <- car::recode(temp2, "unique(temp2)[grep(20, unique(temp2))]=1; unique(temp2)[grep(10, unique(temp2))]=5;else=NA")[tolower(verba$v2)=="united kingdom"]
#verba$pid5[tolower(verba$v2)=="united states"]  <- car::recode(temp, "100=1; 301=2; 305=3; 302=4; 200=5; else=NA")[tolower(verba$v2)=="united states"]

## Marriage Q # 1 pleased, 2 =indifferent, 3 = displeased
	## 86 - REPUBLICAN (U.S.), CONSERVATIVE (U.K.), CDU (GERMANY), DC (ITALY), AND PRI (MEXICO)
	## 87 - DEMOCRAT (U.S.), LABOUR (U.K.), SPD (GERMANY), PCI (ITALY), AND PAN (MEXICO)
	## 88 - LIBERAL (U.K.), FDP (GERMANY), PSI (ITALY), AND PP (MEXICO)
	## 89 - AN EXTREME RIGHT-WING PARTY LIKE THE DRP (GERMANY) OR THE  MSI (ITALY)
	
	verba$marry.r <- car::recode(verba$v86, "1=1; 3=.5; 5=0;else=NA")
	verba$marry.d <- car::recode(verba$v87, "1=1; 3=.5; 5=0;else=NA")
	verba$marry.l <- car::recode(verba$v88, "1=1; 3=.5; 5=0;else=NA")
	verba$marry.n <- car::recode(verba$v89, "1=1; 3=.5; 5=0;else=NA")
	
	verba$marry.out <- ifelse(verba$pid5 > 3, verba$marry.d, verba$marry.r)
 
	xtabs(verba$weight ~ verba$pid5 + verba$marry.r)

## Traits
	### 90.  WE'RE INTERESTED IN WHAT SORTS OF PEOPLE SUPPORT AND VOTE FOR THE DIFFERENT PARTIES.  
	###  IF YOU HAD TO GENERALIZE, WHICH EXPRESSIONS IN THIS LIST (HAND LIST 7) COME CLOSEST
	#90 - 90a, 90b, 90c- COMMENTS ABOUT REPUBLICANS, CONSERVATIVES, CDU, DC, AND PRI
	#91 - 91a, 91b, 91c - COMMENTS ABOUT DEMOCRATS, LABOURITES, SPD, PCI, AND PAN
	#92 - 92a, 92b, 92c - COMMENTS ABOUTS FDP, PSI, AND PP  
	#93 - PARTY OF THE RIGHT LIKE MSI
	# Coding ---
	#11. People interested in national strength and independence, 
	#12. Selfish people ? Interested in their own welfare at the expense of others, 
	#13. Intelligent people
	#15. Betrayers of freedom and the country?s welfare, 
	#16. ignorant and misguided people
	#17. fascists, and militarists
	#18. people interested in the welfare of humanity
	#14. Religious people 19. atheists, godless people, 
	#20. all sorts, can?t generalize.
	#temp <- paste(verba$v90a, verba$v90b, verba$v90c, verba$v90d, sep="")
	
	# Strength and Independence
	r.indp <- unique(c(grep(11, verba$v90a), grep(11, verba$v90b),  grep(11, verba$v90c)))
	verba$r.independence <- ifelse(rownames(verba) %in% r.indp, 1, 0)
	
	d.indp <- unique(c(grep(11, verba$v91a), grep(11, verba$v91b),  grep(11, verba$v91c)))
	verba$d.independence <- ifelse(rownames(verba) %in% d.indp, 1, 0)
	
	# Betrayer
	r.betr <- unique(c(grep(15, verba$v90a), grep(15, verba$v90b),  grep(15, verba$v90c)))
	verba$r.betrayer <- ifelse(rownames(verba) %in% r.betr, 1, 0)
	
	d.betr <- unique(c(grep(15, verba$v91a), grep(15, verba$v91b),  grep(15, verba$v91c)))
	verba$d.betrayer <- ifelse(rownames(verba) %in% d.betr, 1, 0)
	
	# Selfish
	r.self <- unique(c(grep(12, verba$v90a), grep(12, verba$v90b),  grep(12, verba$v90c)))
	verba$r.selfish <- ifelse(rownames(verba) %in% r.self, 1, 0)
	
	d.self <- unique(c(grep(12, verba$v91a), grep(12, verba$v91b),  grep(12, verba$v91c)))
	verba$d.selfish <- ifelse(rownames(verba) %in% d.self, 1, 0)
	
	# Intelligent
	r.intel <- unique(c(grep(13, verba$v90a), grep(13, verba$v90b),  grep(13, verba$v90c)))
	verba$r.intelligent <- ifelse(rownames(verba) %in% r.intel, 1, 0)
	
	d.intel <- unique(c(grep(13, verba$v91a), grep(13, verba$v91b),  grep(13, verba$v91c)))
	verba$d.intelligent <- ifelse(rownames(verba) %in% d.intel, 1, 0)
	
	# Ignorant
	r.igno <- unique(c(grep(16, verba$v90a), grep(16, verba$v90b),  grep(16, verba$v90c)))
	verba$r.ignorant <- ifelse(rownames(verba) %in% r.igno, 1, 0)
	
	d.igno <- unique(c(grep(16, verba$v91a), grep(16, verba$v91b),  grep(16, verba$v91c)))
	verba$d.ignorant <- ifelse(rownames(verba) %in% d.igno, 1, 0)
	
	# Fascist
	r.fasc <- unique(c(grep(17, verba$v90a), grep(17, verba$v90b),  grep(17, verba$v90c)))
	verba$r.fascist <- ifelse(rownames(verba) %in% r.fasc, 1, 0)
	
	d.fasc <- unique(c(grep(17, verba$v91a), grep(17, verba$v91b),  grep(17, verba$v91c)))
	verba$d.fascist <- ifelse(rownames(verba) %in% d.fasc, 1, 0)
	
	# Welfare
	r.welf <- unique(c(grep(18, verba$v90a), grep(18, verba$v90b),  grep(18, verba$v90c)))
	verba$r.welfare <- ifelse(rownames(verba) %in% r.welf, 1, 0)
	
	d.welf <- unique(c(grep(18, verba$v91a), grep(18, verba$v91b),  grep(18, verba$v91c)))
	verba$d.welfare <- ifelse(rownames(verba) %in% d.welf, 1, 0)
	
	verba$r.pos <- with(verba, rowMeans(cbind(r.intelligent, r.independence, r.welfare), na.rm=T))
	verba$d.pos <- with(verba, rowMeans(cbind(d.intelligent, d.independence, d.welfare), na.rm=T))
	verba$r.neg <- with(verba, rowMeans(cbind(r.fascist, r.ignorant, r.selfish, r.betrayer), na.rm=T))
	verba$d.neg <- with(verba, rowMeans(cbind(d.fascist, d.ignorant, d.selfish, d.betrayer), na.rm=T))
	
	#verba$d.welfare <- 0
	#verba$d.welfare[d.welf] <- 1
	with(verba, xtabs(~ pid5 + d.welfare))

# PID
	dl <- verba$pid5 %in% c(1,2)
	rc <- verba$pid5 %in% c(4,5)

# rep
	verba$out.welfare <- with(verba, out(r.welfare, d.welfare, dl, rc))
	verba$in.welfare  <- with(verba, out(r.welfare, d.welfare, rc, dl))
	
	verba$out.independence <- with(verba, out(r.independence, d.independence, dl, rc))
	verba$in.independence  <- with(verba, out(r.independence, d.independence, rc, dl))
	
	verba$out.selfish <- with(verba, out(r.selfish, d.selfish, dl, rc))
	verba$in.selfish  <- with(verba, out(r.selfish, d.selfish, rc, dl))
	
	verba$out.ignorant <- with(verba, out(r.ignorant, d.ignorant, dl, rc))
	verba$in.ignorant  <- with(verba, out(r.ignorant, d.ignorant, rc, dl))
	
	verba$out.intelligent <- with(verba, out(r.intelligent, d.intelligent, dl, rc))
	verba$in.intelligent  <- with(verba, out(r.intelligent, d.intelligent, rc, dl))
	
	verba$out.betrayer <- with(verba, out(r.betrayer, d.welfare, dl, rc))
	verba$in.betrayer  <- with(verba, out(r.betrayer, d.welfare, rc, dl))
	
	verba$out.fascist <- with(verba, out(r.fascist, d.fascist, dl, rc))
	verba$in.fascist  <- with(verba, out(r.fascist, d.fascist, rc, dl))
	
	verba$out.pos <- with(verba, out(r.pos, d.pos, dl, rc))
	verba$in.pos  <- with(verba, out(r.pos, d.pos, rc, dl))
	
	verba$out.neg <- with(verba, out(r.neg, d.neg, dl, rc))
	verba$in.neg  <- with(verba, out(r.neg, d.neg, rc, dl))


	mean(verba$out.welfare[verba$pid5 %in% c(4,5)], na.rm=T)
	mean(verba$in.welfare[verba$pid5 %in% c(4,5)], na.rm=T)

#94     PARTY IN POWER BAD                              67  
#95     DEMOCRATIC PARTY BAD FOR COUNTRY'S WELFARE      68  
#96     LIBERAL PARTY BAD FOR COUNTRY'S WELFARE         69  
#97     PSDI BAD FOR COUNTRY'S WELFARE                      
#98     MSI BAD FOR COUNTRY'S WELFARE                   70 

# Save File
	save(verba, file="replication-data/wrangled/verba.rdata")
