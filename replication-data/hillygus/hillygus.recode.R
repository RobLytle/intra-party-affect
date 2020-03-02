##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##   												
##      Hillygus 2004
##		Last Edited: 12.07.11  		
##   	Gaurav Sood											
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

setwd(dir)

# Load Functions
source("polar/polr.func.R")

# Load Data
#load("data/Hillygus/BlairCenter2004ElectionStudy.RData")
#names(x)

wedge <- foreign::read.spss("C:/Users/Gaurav/Desktop/R/data/Hillygus/BlairCenter2004ElectionStudy.sav", 
		to.data.frame=TRUE)

# Weight: weight

# Battleground states (confirmed with SI) -
# FL, OH, PA, WI, IA, MN,  NM, CO, NH,  HI, NV, MO
# FL, OH, PA,  WI, IA, MN, NM,     NH,       NV, MO, OR,  TN  
wedge$battleground  <- car::recode(wedge$ppstaten, "c('FL', 'OH', 'PA', 'WI', 'IA', 'MN',  'NM', 'CO', 'NH',  'HI', 'NV', 'MO')='Battleground'; else='Non-Battleground'")
wedge$battleground2 <- car::recode(wedge$ppstaten, "c('FL', 'OH', 'PA', 'WI', 'IA', 'MN',  'NM', 'NH', 'NV', 'MO', 'OR', 'TN')='Battleground'; else='Non-Battleground'")
wedge$south         <- car::recode(wedge$ppstaten, "c('AL', 'AR', 'FL', 'GA', 'LA', 'MS', 'NC', 'SC', 'TN', 'TX', 'VA')='South'; else='Non-South'")

# PID
# q44, q45, q46
temp <- paste(wedge$q44, wedge$q45, wedge$q46, sep="")
wedge$pid7 <- NA
wedge$pid7[temp=='DemocratStrongNA'] <- '7 Strong Democrat'
wedge$pid7[temp=='DemocratNot very strongNA'] <- '6 Not very strong Democrat'
wedge$pid7[temp=='RepublicanStrongNA'] <- '1 Strong Republican'
wedge$pid7[temp=='RepublicanNot very strongNA'] <- '2 Not very strong Republican'
wedge$pid7[temp=='IndependentNADemocratic Party'] <- '5 Lean Democrat'
wedge$pid7[temp=='IndependentNARepublican Party'] <- '3 Lean Republican'
wedge$pid7[temp=='IndependentNANeither party'] <- '4 Independent'

wedge$pid3   <- car::recode(as.integer(factor(wedge$pid7)), "c(1,2,3)='Republican'; c(4)='Independent'; c(5,6,7)='Democrat'; else=NA")
wedge$strpid <- car::recode(as.integer(factor(wedge$pid7)), "c(1,7)=1; c(2,6)=.5; c(3,5)=0; else=NA")

# self-ideology ocap700s 
wedge$q51

# Feelings towards Rep./Dem.
wedge$favdem    <- zero1(as.integer(as.character(wedge$q6_6)), 0, 100)
wedge$favrep    <- zero1(as.integer(as.character(wedge$q6_7)), 0, 100)
wedge$favlibdem <- zero1(as.integer(as.character(wedge$q6_8)), 0, 100)
wedge$favconrep <- zero1(as.integer(as.character(wedge$q6_9)), 0, 100)

# This messes up measure for independents (which shld. be NA) but independents will be taken out during analyses
wedge$inpart  <- ifelse(wedge$q44=="Democrat", wedge$favdem, wedge$favrep) 
wedge$outpart <- ifelse(wedge$q44=="Democrat", wedge$favrep, wedge$favdem) 
wedge$inout   <- wedge$inpart - wedge$outpart

# Pol. Int. 
wedge$polint <- car::recode(as.integer(wedge$q1), "1=1; 2=.66; 3=.33; 4=0; else=NA")

# sociodem
# ppage, ppagecat, ppagect4, ppdualin,ppeduc, ppeducat, ppeth, ppgender, ppincimp, ppmarit, ppmsacat    

save(wedge, file="data/Hillygus/wedge.rdata")

# *****************************************************
# Integrating Wedge Data with the Ads Data from 2004
# ********************************************************
load("data/Hillygus/wedge.rdata")
load("data/WiscAds/2004/ad04.rdata")
wedges <- subset(wedge, select=c("ppstaten", "battleground", "battleground2", "south", "ppeth", "inpart", "outpart", "inout", "pid3", "pid7", "strpid", 
								"ppage", "ppgender", "ppeducat", "polint", "weight2"))
wedges$ppstaten <- as.character(wedges$ppstaten)
# Merge Wedge with ads043m by state
wedgeads <- merge(wedges, ad04, by.x="ppstaten", by.y="state", all.x=T, all.y=F, suffixes=c(".w", ".ad"))

wedges[,names(ad04)] <- ad04[match(wedges$ppstaten, ad04$state),]

wedgeads <- subset(wedges, !is.na(wedges$state))

# Battleground states: 'FL', 'OH', 'PA', 'WI', 'IA', 'MN',  'NM', 'CO', 'NH',  'HI', 'NV', 'MO'
# Inadvertent exposure cannot be done because we are joining at the levels of states

save(wedgeads, file="data/Hillygus/wedgeads.rdata")
