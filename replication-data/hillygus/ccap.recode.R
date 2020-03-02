##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##   											
##      CCAP 2008
##		Last Edited: 12.01.11  		
##   	Gaurav Sood										
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

# Set Working dir.
	setwd("C:/Users/gsood/Dropbox/")

# Sourcing Common Functions
	source("func/func.R")
	source("func/polr.func.R")

# Load Data
	# 2 Separate files were received. CCAP did not have a transparently named DMA so DMAs from a separate file
	ccap <- foreign::read.dta("data/CCAP/currentV3.dta")
	dma  <- foreign::read.dta("data/CCAP/CCAPdmas.dta")	

# Splitting dma var into DMA and state; Not the best way to do it. 
	dma$dmar  <- sapply(as.character(dma$dma), function (a) strsplit(a, ", ")[[1]][1])
	dma$state <- sapply(as.character(dma$dma), function (a) strsplit(a, ", ")[[1]][2])
	# Some state issues
	dma$state[nchar(dma$state)>2] <- substr(dma$state[nchar(dma$state)>2], 1,2)

	# Merge DMA into CCAP
	ccap2 <- merge(ccap, dma, by="caseid", all.x=T, all.y=F)
	save(ccap2, file="data/CCAP/ccap2.rdata")

# load("data/CCAP/ccap2.rdata")
# Weight: weight

# Battleground/Non-Battleground: battleground

# ocap is the October wave - same wave from which we will be harvesting fav. ratings
# We [should] use previous wave pids
# pid - ccap$ocap8_3, ccap$ocap8 
# Previously coded - party, dem, rep - don't know their ancestry

ccap2$pid3c  <- car::recode(ccap2$ocap8_3, "c('Other ', 'Not sure')=NA")

ccap2$pid3   <- car::recode(as.integer(ccap2$ocap8), "c(1,2,3)='Democrat'; c(4,8)='Independent'; c(5,6,7)='Republican'; else=NA")
ccap2$strpid <- car::recode(as.integer(ccap2$ocap8), "c(1,7)=1; c(2,6)=.5; c(3,5)=0; else=NA")

# Favorability
# OCAP300REP, OCAP300DEM, OCAP300CONS, OCAP300LIBS 
## ******* ## Not asked:: NCAP300dem or NCAP300rep
	
	ccap2$favrep <- car::recode(as.integer(ccap2$ocap300rep), "5=0;4=.25;1=.5;2=.75;3=1;6=.5;else=NA")
	ccap2$favdem <- car::recode(as.integer(ccap2$ocap300dem), "5=0;4=.25;1=.5;2=.75;3=1;6=.5;else=NA")
	ccap2$favcon <- car::recode(as.integer(ccap2$ocap300cons), "5=0;4=.25;1=.5;2=.75;3=1;6=.5;else=NA")
	ccap2$favlib <- car::recode(as.integer(ccap2$ocap300libs), "5=0;4=.25;1=.5;2=.75;3=1;6=.5;else=NA")
	
	ccap2$favrepr <- car::recode(as.integer(ccap2$ocap300rep), "5=0;4=.25;1=.5;2=.75;3=1;else=NA")
	ccap2$favdemr <- car::recode(as.integer(ccap2$ocap300dem), "5=0;4=.25;1=.5;2=.75;3=1;else=NA")
	ccap2$favconr <- car::recode(as.integer(ccap2$ocap300cons), "5=0;4=.25;1=.5;2=.75;3=1;else=NA")
	ccap2$favlibr <- car::recode(as.integer(ccap2$ocap300libs), "5=0;4=.25;1=.5;2=.75;3=1;else=NA")
	
	# Independents are taken out during analyses so the following is fine
	ccap2$inpart  <- ifelse(ccap2$pid3=="Democrat", ccap2$favdem, ccap2$favrep) 
	ccap2$outpart <- ifelse(ccap2$pid3=="Democrat", ccap2$favrep, ccap2$favdem) 
	ccap2$inout   <- ccap2$inpart - ccap2$outpart
	
	ccap2$inpartr  <- ifelse(ccap2$pid3=="Democrat", ccap2$favdemr, ccap2$favrepr) 
	ccap2$outpartr <- ifelse(ccap2$pid3=="Democrat", ccap2$favrepr, ccap2$favdemr) 
	ccap2$inoutr   <- ccap2$inpartr - ccap2$outpartr

# To check if coding was ok
# a <- with(ccap, out(favdem - favrep, favrep - favdem, !is.na(pid3) & ccap$pid3=="Democrat", !is.na(pid3) & ccap$pid3=="Republican"))
# identical(mean(ccap$inout[!is.na(ccap$pid3) & ccap$pid3!='Independent'], na.rm=T), mean(a, na.rm=T))

# self-ideology ocap700s 

# Pol. Int. from Jan
	ccap2$polint <- car::recode(as.integer(ccap2$jcap813), "1=1; 2=.5;3=0;else=NA")
# sociodem
# age, white, black, hispanic, male, educ, income, single

# Exposure to ads
# OCAP400_1 PRES CAMPAIGN OCCURRENCES - SAW TV AD
# Thinking about the Presidential candidates and their campaigns, did any of the following things happen to you 
# YESTERDAY?
# SCAP400_1, CAP400, JCAP400, MCAP400
# <8> Heard a radio ad for a candidate
# OCAP402_1 MCCAIN CAMPAIGN - SAW TV AD
	
	ccap2$expose   <- with(ccap2, rowMeans(cbind(bcap400_1=='Yes', jcap400_1=='Yes', mcap400_1=='Yes', scap400_1=='Yes'), na.rm=T))
	ccap2$tvads    <- with(ccap2, rowMeans(cbind(bcap400_1=='Yes', jcap400_1=='Yes', mcap400_1=='Yes', scap400_1=='Yes', ocap400_1=='Yes'), na.rm=T))
	ccap2$radioads <- with(ccap2, rowMeans(cbind(jcap400_8=='Yes', mcap400_8=='Yes', scap400_8=='Yes', ocap400_8=='Yes'), na.rm=T))

# Television Viewing
#MCAP2_1 TELEVISION LAST 7 DAYS - SPORTS
#MCAP2_2 TELEVISION LAST 7 DAYS - MOVIES
#MCAP2_3 TELEVISION LAST 7 DAYS - PRIME TIME SHOWS
#MCAP2_4 TELEVISION LAST 7 DAYS - NEWS PROGRAMS
#MCAP2_5 TELEVISION LAST 7 DAYS - LATE NIGHT SHOWS
#MCAP2_6 TELEVISION LAST 7 DAYS - DAYTIME TALK SHOWS
#MCAP2_7 TELEVISION LAST 7 DAYS - POLITICAL TALK SHOWS
#MCAP2_8 TELEVISION LAST 7 DAYS - SATIRE SHOWS
#MCAP2_9 TELEVISION LAST 7 DAYS - OTHER
	
	ccap2$inadv1 <- with(ccap2, rowMeans(cbind(bcap2_1=='Yes', bcap2_2=='Yes', bcap2_3=='Yes', bcap2_4=='Yes', bcap2_5=='Yes', bcap2_6=='Yes', bcap2_7=='Yes', bcap2_8=='Yes', bcap2_9=='Yes'), na.rm=T))
	ccap2$inadv2 <- with(ccap2, rowMeans(cbind(mcap2_1=='Yes', mcap2_2=='Yes', mcap2_3=='Yes', mcap2_4=='Yes', mcap2_5=='Yes', mcap2_6=='Yes', mcap2_7=='Yes', mcap2_8=='Yes', mcap2_9=='Yes'), na.rm=T))
	ccap2$inadv  <- with(ccap2, rowMeans(cbind(bcap2_1=='Yes', bcap2_2=='Yes', bcap2_3=='Yes', bcap2_4=='Yes', bcap2_5=='Yes', bcap2_6=='Yes', bcap2_7=='Yes', bcap2_8=='Yes', bcap2_9=='Yes', mcap2_1=='Yes', mcap2_2=='Yes', mcap2_3=='Yes', mcap2_4=='Yes', mcap2_5=='Yes', mcap2_6=='Yes', mcap2_7=='Yes', mcap2_8=='Yes', mcap2_9=='Yes'), na.rm=T))
	
	ccap2$inadv4  <- with(ccap2, rowMeans(cbind(bcap2_1=='Yes', bcap2_2=='Yes', bcap2_3=='Yes', bcap2_6=='Yes', mcap2_1=='Yes', mcap2_2=='Yes', mcap2_3=='Yes', mcap2_6=='Yes'), na.rm=T))
	
	# Battleground Issues
	table(ccap2$profile66[ccap2$battleground=='Battleground'])
	# Battleground states: CO, FL, IA, MI, MN, NC, NM, NH, NV, OH, PA, WI, WV
	# McDonald Measure: CO, FL, IN, IA, MO, NV, NC, OH, PA, VA 
	ccap2$bground2 <- ifelse(ccap2$profile66 %in% c('Colorado', 'Florida', 'Indiana', 'Iowa', 'Missouri', 'Nevada', 'North Carolina', 'Ohio', 'Pennsylvania', 'Virginia'), 1, 0)

	# NJ (NY Versus PA) - Inadv. Exposure
	ccap2$pamarket <- NA
	ccap2$pamarket[ccap2$profile66=='New Jersey' & !is.na(ccap2$dma) & ccap2$dma=='Philadelphia, PA'] <- 1
	ccap2$pamarket[ccap2$profile66=='New Jersey' & !is.na(ccap2$dma) & ccap2$dma=='New York, NY'] <- 0
	
	# Non-Battleground state residents served by Battleground stations
	# Non-Battleground state resident served by Non-Battleground stations
	battle <- c('CO', 'FL', 'IA', 'MI', 'MN', 'NC', 'NM', 'NH', 'NV', 'OH', 'OR','PA', 'WI', 'WV')
	ccap2$spill <- NA
	ccap2$spill[ccap2$battleground=='Non-Battleground' & !is.na(ccap2$state) & !(ccap2$state %in% battle)] <- 'nbnb'
	ccap2$spill[ccap2$battleground=='Non-Battleground' & !is.na(ccap2$state) & (ccap2$state %in% battle)] <- 'nbb'
	ccap2$spill[ccap2$battleground=='Battleground' & !is.na(ccap2$state) & !(ccap2$state %in% battle)] <- 'bnb'
	ccap2$spill[ccap2$battleground=='Battleground' & !is.na(ccap2$state) & (ccap2$state %in% battle)] <- 'bb'
	
	ccap2$spill2 <- NA
	ccap2$spill2[ccap2$battleground=='Non-Battleground' & !is.na(ccap2$state) & !(ccap2$state %in% battle)] <- 0
	ccap2$spill2[ccap2$battleground=='Non-Battleground' & !is.na(ccap2$state) & (ccap2$state %in% battle)] <- 1
	ccap2$spill2[ccap2$battleground=='Battleground' & !is.na(ccap2$state) & !(ccap2$state %in% battle)] <- 0
	ccap2$spill2[ccap2$battleground=='Battleground' & !is.na(ccap2$state) & (ccap2$state %in% battle)] <- 1
	
	ccap2$spill3 <- NA
	ccap2$spill3[ccap2$battleground=='Non-Battleground' & !is.na(ccap2$state) & !(ccap2$state %in% battle)] <- 0
	ccap2$spill3[ccap2$battleground=='Non-Battleground' & !is.na(ccap2$state) & (ccap2$state %in% battle)] <- 1
	ccap2$spill3[ccap2$battleground=='Battleground' & !is.na(ccap2$state) & !(ccap2$state %in% battle)] <- NA
	ccap2$spill3[ccap2$battleground=='Battleground' & !is.na(ccap2$state) & (ccap2$state %in% battle)] <- NA
	
	ccap2$spill4 <- NA
	ccap2$spill4[ccap2$bground2==1 & !is.na(ccap2$state) & !(ccap2$state %in% battle)] <- 'nbnb'
	ccap2$spill4[ccap2$bground2==1 & !is.na(ccap2$state) & (ccap2$state %in% battle)] <- 'nbb'
	ccap2$spill4[ccap2$bground2==1 & !is.na(ccap2$state) & !(ccap2$state %in% battle)] <- 'bnb'
	ccap2$spill4[ccap2$bground2==1 & !is.na(ccap2$state) & (ccap2$state %in% battle)] <- 'bb'

#ccap2$battleground[ccap2$state %in% c('AL', 'DC', 'GA', 'MA', 'ME', 'ND', 'SD', 'TX', 'UT')]
select.cols <- c("caseid", "dma", "dmar", "state", "weight", "pid3", "strpid", "polint", 
				 "expose", "tvads", "radioads", "ocap400_1", "inadv", "inadv1", "inadv2", "inadv4", 
				 "age", "white", "black", "hispanic", "male", "educ", "income", "white",
				 "inpart", "outpart", "inout", "favrep", "favdem", "favlib", "favcon",
				 "inpartr", "outpartr", "inoutr", "favrepr", "favdemr", "favlibr", "favconr",
				 "battleground", "bground2", "profile66", "pamarket", "spill", "spill2", "spill3", "spill4")
ccaps <- subset(ccap2, select=select.cols)		 
save(ccaps, file="data/CCAP/ccaps.rdata")
# cbind(as.character(ccap2$profile66), ccap2$profile72market.x)
# To merge CCAP and ads3, we need a cross-walk
load("C:/Users/Gaurav/Desktop/R/data/CCAP/ccaps.rdata")
ccap3 <- ccaps[!duplicated(ccaps$dma),] 				# Remove all duplicate DMAs
ccap3 <- ccap3[,c("caseid", "dmar", "dma", "state", "profile66")]	# Select Relevant Columns
write.csv(ccap3, file="C:/Users/Gaurav/Desktop/R/data/WiscAds/2008/ccap3.csv")

load("C:/Users/Gaurav/Desktop/R/data/WiscAds/2008/ad083.rdata")
ad083$caseid <- seq(1, nrow(ad083), by=1)
write.csv(ad083, file="C:/Users/Gaurav/Desktop/R/data/WiscAds/2008/ads083.csv")

# Crosswalk between DMA Names in CCAP and WiscAds
matchy <- read.csv("C:/Users/Gaurav/Desktop/R/data/WiscAds/2008/matchid.csv")

# Merging via the cross-walk
m <- merge(ccap3, matchy, by="caseid", all.x=T, all.y=F)  			  # Merged CCAP with Matchy
p <- merge(m, ad083, by.x="matchid", by.y="caseid", all.x=T, all.y=F) # Merge Resulting File with Ad083
r <- p[,c("dma.x","netattack","netattack2", "netattacknoissue", "netattackpart", "netpart", "netads", 
				"affiliate", "program", "time", "party", "ad_tone", "cnt_prp", "cnt_fin", 
				"prty_mn", "enegment", "per_ply", "none", "state")]

ccapsm <- merge(ccaps, r, by.x="dma", by.y="dma.x", all.x=T, all.y=F)

save(ccapsm, file="data/CCAP/ccapsm.rdata")

ccapsm$state.y[grep('NJ', ccapsm$state.y)]
