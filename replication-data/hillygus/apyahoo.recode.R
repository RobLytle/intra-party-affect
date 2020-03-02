##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##   												
##      Hillygus 2008/AP Yahoo
##		Last Edited: 12.14.11  		
##   	Gaurav Sood											
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

setwd(dir)

# Load Functions
source("polar/polr.func.R")

# Get Data
apy <- read.csv("data/ap.2008/AP_Election_2008_public.csv")
names(apy) <- tolower(names(apy))

geo <- foreign::read.spss("data/ap.2008/APYahooGeocodes.sav", to.data.frame=TRUE)
names(geo) <- tolower(names(geo))

geo$state <- substr(as.character(geo$fips), nchar(as.character(geo$fips))-1, nchar(as.character(geo$fips)))
geo$state[!is.na(geo$state) & geo$state=='ia'] <- 'DC'
geo$state[!is.na(geo$state) & geo$state=='st'] <- NA
geo$state[!is.na(geo$state) & geo$state=='ar'] <- NA

# Merge FIPS and DMA
apy <- merge(apy, geo, by='caseid', all.x=T, all.y=F)

# See CCAP
apy$battleground <- ifelse(apy$state %in% c('CO', 'FL', 'IA', 'MI', 'MN', 'NC', 'NM', 'NH', 'NV', 'OH',  'OR', 'PA', 'WI', 'WV'), 1, 0)
apy$battleground[is.na(apy$state)] <- NA

# Spill in NBG
#table(apy$state[!is.na(apy$battleground) & apy$battleground==0])
#table(as.character(apy$ppdma[grep("AL", geo$fips)]))

apy$spill <- NA
apy$spill[grep("AK", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("AK", geo$fips)]), NA, 0)
apy$spill[grep("AL", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("AL", geo$fips)]), NA, 0)
# AR gets TN, MO
apy$spill[grep("AR", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("AR", geo$fips)]), NA, 0)
apy$spill[grep("AZ", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("AZ", geo$fips)]), NA, 0)
apy$spill[grep("CA", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("CA", geo$fips)]), NA, 0)
# CT gets NY
apy$spill[grep("CT", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("CT", geo$fips)]), NA, 0)
# DE gets PA and MD
apy$spill[grep("DE", geo$fips)] <- ifelse(apy$ppdma[grep("DE", geo$fips)]=='PHILADELPHIA', 1, 0)
# GA gets FL, SC
apy$spill[grep("GA", geo$fips)] <- ifelse(apy$ppdma[grep("GA", geo$fips)] %in% c('JACKSONVILLE,BRUNSWICK', 'TALLAHASSE-THOMASVILLE'), 1, 0)
apy$spill[grep("HI", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("HI", geo$fips)]), NA, 0)
# ID gets WA
apy$spill[grep("ID", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("ID", geo$fips)]), NA, 0)
# IL gets IN, MO
apy$spill[grep("IL", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("IL", geo$fips)]), NA, 0)
# IN gets IL, OH, KY
apy$spill[grep("IN", geo$fips)] <- ifelse(apy$ppdma[grep("GA", geo$fips)] %in% c('CINCINNATI', 'DAYTON'), 1, 0)
apy$spill[grep("KS", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("KS", geo$fips)]), NA, 0)
# KY gets IL, TN
apy$spill[grep("KY", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("KY", geo$fips)]), NA, 0)
# MA gets NY, RI
apy$spill[grep("MA", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("MA", geo$fips)]), NA, 0)
# MD gets DC
apy$spill[grep("MD", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("MD", geo$fips)]), NA, 0)
apy$spill[grep("ME", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("ME", geo$fips)]), NA, 0)
# MO gets IL
apy$spill[grep("MO", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("MO", geo$fips)]), NA, 0)
# MS gets TN
apy$spill[grep("MS", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("MS", geo$fips)]), NA, 0)
# MT gets WA
apy$spill[grep("MT", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("MT", geo$fips)]), NA, 0)
# ND
apy$spill[grep("ND", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("ND", geo$fips)]), NA, 0)
# NE gets IA
apy$spill[grep("IN", geo$fips)] <- ifelse(apy$ppdma[grep("GA", geo$fips)] %in% c('SIOUX CITY'), 1, 0)
# NJ gets PA, NY
apy$spill[grep("NJ", geo$fips)] <- ifelse(apy$ppdma[grep("NJ", geo$fips)] %in% c('PHILADELPHIA'), 1, 0)
# NY
apy$spill[grep("NY", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("NY", geo$fips)]), NA, 0)
# OK gets AR
apy$spill[grep("OK", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("OK", geo$fips)]), NA, 0)
# RI
apy$spill[grep("RI", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("RI", geo$fips)]), NA, 0)
# SC gets GA, NC
apy$spill[grep("SC", geo$fips)] <- ifelse(apy$ppdma[grep("SC", geo$fips)] %in% c('CHARLOTTE', 'GREENVILLE-SPARTANBURG-ASHVILLE-AND'), 1, 0)
# SD
apy$spill[grep("SD", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("SD", geo$fips)]), NA, 0)
# TN
apy$spill[grep("TN", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("TN", geo$fips)]), NA, 0)
# TX
apy$spill[grep("TX", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("TX", geo$fips)]), NA, 0)
# UT
apy$spill[grep("UT", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("UT", geo$fips)]), NA, 0)
# VA gets DC
apy$spill[grep("VA", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("VA", geo$fips)]), NA, 0)
# VT gets boston
apy$spill[grep("VT", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("VT", geo$fips)]), NA, 0)
# WA gets OR
apy$spill[grep("WA", geo$fips)] <- ifelse(apy$ppdma[grep("WA", geo$fips)] %in% c('PORTLAND, OR', 'GREENVILLE-SPARTANBURG-ASHVILLE-AND'), 1, 0)
# WY
apy$spill[grep("WY", geo$fips)] <- ifelse(is.na(apy$ppdma[grep("WY", geo$fips)]), NA, 0)

pidmaker <- function(temp){
	res <- rep(NA, length(temp))
	res[temp=="11NA"] <- '1 Strong Dem'
	res[temp=="12NA"] <- '2 Not Strong Dem'
	res[temp=="3NA1"] <- '3 Lean Dem'
	res[temp=="3NA3"] <- '4 Independent'
	res[temp=="3NA2"] <- '5 Lean Rep'
	res[temp=="22NA"] <- '6 Not Strong Rep'
	res[temp=="21NA"] <- '7 Strong Rep'
	res
}

# PID Wave 1
temp <- paste(apy$pid1, apy$pid2, apy$pid3, sep="")
apy$pid7 <- pidmaker(temp)
apy$pid3r <- car::recode(as.numeric(factor(apy$pid7)), "c(1,2,3)='Democrat'; c(5,6,7)='Republican'; c(4)='Independent'")

# PID Wave 2
temp <- paste(apy$pid1_w2, apy$pid2_w2, apy$pid3_w2, sep="")
apy$pid7r_w2 <-  pidmaker(temp)
apy$pid3r_w2 <- car::recode(as.numeric(factor(apy$pid7r_w2)), "c(1,2,3)='Democrat'; c(5,6,7)='Republican'; c(4)='Independent'")

# PID Wave 3
temp <- paste(apy$pid1_w3, apy$pid2_w3, apy$pid3_w3, sep="")
apy$pid7r_w3 <-  pidmaker(temp)
apy$pid3r_w3 <- car::recode(as.numeric(factor(apy$pid7r_w3)), "c(1,2,3)='Democrat'; c(5,6,7)='Republican'; c(4)='Independent'")

# PID Wave 4
temp <- paste(apy$pid1_w4, apy$pid2_w4, apy$pid3_w4, sep="")
apy$pid7r_w4 <-  pidmaker(temp)
apy$pid3r_w4 <- car::recode(as.numeric(factor(apy$pid7r_w4)), "c(1,2,3)='Democrat'; c(5,6,7)='Republican'; c(4)='Independent'")

# PID Wave 5
temp <- paste(apy$pid1_w5, apy$pid2_w5, apy$pid3_w5, sep="")
apy$pid7r_w5 <-  pidmaker(temp)
apy$pid3r_w5 <- car::recode(as.numeric(factor(apy$pid7r_w5)), "c(1,2,3)='Democrat'; c(5,6,7)='Republican'; c(4)='Independent'")

# PID Wave 6
temp <- paste(apy$pid1_w6, apy$pid2_w6, apy$pid3_w6, sep="")
apy$pid7r_w6 <-  pidmaker(temp)
apy$pid3r_w6 <- car::recode(as.numeric(factor(apy$pid7r_w6)), "c(1,2,3)='Democrat'; c(5,6,7)='Republican'; c(4)='Independent'")

# PID Wave 7
temp <- paste(apy$pid1_w7, apy$pid2_w7, apy$pid3_w7, sep="")
apy$pid7r_w7 <-  pidmaker(temp)
apy$pid3r_w7 <- car::recode(as.numeric(factor(apy$pid7r_w7)), "c(1,2,3)='Democrat'; c(5,6,7)='Republican'; c(4)='Independent'")

# PID Wave 8
temp <- paste(apy$pid1_w8, apy$pid2_w8, apy$pid3_w8, sep="")
apy$pid7r_w8 <-  pidmaker(temp)
apy$pid3r_w8 <- car::recode(as.numeric(factor(apy$pid7r_w8)), "c(1,2,3)='Democrat'; c(5,6,7)='Republican'; c(4)='Independent'")

# PID Wave 9
temp <- paste(apy$pid1_w9, apy$pid2_w9, apy$pid3_w9, sep="")
apy$pid7r_w9 <-  pidmaker(temp)
apy$pid3r_w9 <- car::recode(as.numeric(factor(apy$pid7r_w9)), "c(1,2,3)='Democrat'; c(5,6,7)='Republican'; c(4)='Independent'")

# Favorability Wave 1
apy$favdem.w1 <- car::recode(apy$fav2_1, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$favrep.w1 <- car::recode(apy$fav2_2, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$inpart.w1  <- ifelse(apy$pid3r=="Democrat", apy$favdem.w1, apy$favrep.w1) 
apy$outpart.w1 <- ifelse(apy$pid3r=="Democrat", apy$favrep.w1, apy$favdem.w1) 
apy$inout.w1   <- apy$inpart.w1 - apy$outpart.w1

apy$inpart.w1r  <- ifelse(apy$pid3r=="Democrat", apy$favdem.w1, apy$favrep.w1) 
apy$outpart.w1r <- ifelse(apy$pid3r=="Democrat", apy$favrep.w1, apy$favdem.w1) 
apy$inout.w1r   <- apy$inpart.w1r - apy$outpart.w1r

# Favorability Wave 2
apy$favdem.w2 <- car::recode(apy$fav2_1_w2, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$favrep.w2 <- car::recode(apy$fav2_2_w2, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$inpart.w2  <- ifelse(apy$pid3r=="Democrat", apy$favdem.w2, apy$favrep.w2) 
apy$outpart.w2 <- ifelse(apy$pid3r=="Democrat", apy$favrep.w2, apy$favdem.w2) 
apy$inout.w2   <- apy$inpart.w2 - apy$outpart.w2

apy$inpart.w2r  <- ifelse(apy$pid3r_w2=="Democrat", apy$favdem.w2, apy$favrep.w2) 
apy$outpart.w2r <- ifelse(apy$pid3r_w2=="Democrat", apy$favrep.w2, apy$favdem.w2) 
apy$inout.w2r   <- apy$inpart.w2r - apy$outpart.w2r

# Favorability Wave 3
apy$favdem.w3 <- car::recode(apy$fav2_1_w3, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$favrep.w3 <- car::recode(apy$fav2_2_w3, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$inpart.w3  <- ifelse(apy$pid3r=="Democrat", apy$favdem.w3, apy$favrep.w3) 
apy$outpart.w3 <- ifelse(apy$pid3r=="Democrat", apy$favrep.w3, apy$favdem.w3) 
apy$inout.w3   <- apy$inpart.w3 - apy$outpart.w3

apy$inpart.w3r  <- ifelse(apy$pid3r_w3=="Democrat", apy$favdem.w3, apy$favrep.w3) 
apy$outpart.w3r <- ifelse(apy$pid3r_w3=="Democrat", apy$favrep.w3, apy$favdem.w3) 
apy$inout.w3r   <- apy$inpart.w3r - apy$outpart.w3r

# Favorability Wave 4
apy$favdem.w4 <- car::recode(apy$fav2_1_w4, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$favrep.w4 <- car::recode(apy$fav2_2_w4, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$inpart.w4  <- ifelse(apy$pid3r=="Democrat", apy$favdem.w4, apy$favrep.w4) 
apy$outpart.w4 <- ifelse(apy$pid3r=="Democrat", apy$favrep.w4, apy$favdem.w4) 
apy$inout.w4   <- apy$inpart.w4 - apy$outpart.w4

apy$inpart.w4r  <- ifelse(apy$pid3r_w4=="Democrat", apy$favdem.w4, apy$favrep.w4) 
apy$outpart.w4r <- ifelse(apy$pid3r_w4=="Democrat", apy$favrep.w4, apy$favdem.w4) 
apy$inout.w4r   <- apy$inpart.w4r - apy$outpart.w4r


# Favorability Wave 5
apy$favdem.w5 <- car::recode(apy$fav2_1_w5, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$favrep.w5 <- car::recode(apy$fav2_2_w5, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$inpart.w5  <- ifelse(apy$pid3r=="Democrat", apy$favdem.w5, apy$favrep.w5) 
apy$outpart.w5 <- ifelse(apy$pid3r=="Democrat", apy$favrep.w5, apy$favdem.w5) 
apy$inout.w5   <- apy$inpart.w5 - apy$outpart.w5

apy$inpart.w5r  <- ifelse(apy$pid3r_w5=="Democrat", apy$favdem.w5, apy$favrep.w5) 
apy$outpart.w5r <- ifelse(apy$pid3r_w5=="Democrat", apy$favrep.w5, apy$favdem.w5) 
apy$inout.w5r   <- apy$inpart.w5r - apy$outpart.w5r


# Favorability Wave 6
apy$favdem.w6 <- car::recode(apy$fav2_1_w6, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$favrep.w6 <- car::recode(apy$fav2_2_w6, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$inpart.w6  <- ifelse(apy$pid3r=="Democrat", apy$favdem.w6, apy$favrep.w6) 
apy$outpart.w6 <- ifelse(apy$pid3r=="Democrat", apy$favrep.w6, apy$favdem.w6) 
apy$inout.w6   <- apy$inpart.w6 - apy$outpart.w6

apy$inpart.w6r  <- ifelse(apy$pid3r_w6=="Democrat", apy$favdem.w6, apy$favrep.w6) 
apy$outpart.w6r <- ifelse(apy$pid3r_w6=="Democrat", apy$favrep.w6, apy$favdem.w6) 
apy$inout.w6r   <- apy$inpart.w6r - apy$outpart.w6r


# Favorability Wave 7
apy$favdem.w7 <- car::recode(apy$fav2_1_w7, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$favrep.w7 <- car::recode(apy$fav2_2_w7, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$inpart.w7  <- ifelse(apy$pid3r=="Democrat", apy$favdem.w7, apy$favrep.w7) 
apy$outpart.w7 <- ifelse(apy$pid3r=="Democrat", apy$favrep.w7, apy$favdem.w7) 
apy$inout.w7   <- apy$inpart.w7 - apy$outpart.w7

apy$inpart.w7r  <- ifelse(apy$pid3r_w7=="Democrat", apy$favdem.w7, apy$favrep.w7) 
apy$outpart.w7r <- ifelse(apy$pid3r_w7=="Democrat", apy$favrep.w7, apy$favdem.w7) 
apy$inout.w7r   <- apy$inpart.w7r - apy$outpart.w7r


# Favorability Wave 8
apy$favdem.w8 <- car::recode(apy$fav2_1_w8, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$favrep.w8 <- car::recode(apy$fav2_2_w8, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$inpart.w8  <- ifelse(apy$pid3r=="Democrat", apy$favdem.w8, apy$favrep.w8) 
apy$outpart.w8 <- ifelse(apy$pid3r=="Democrat", apy$favrep.w8, apy$favdem.w8) 
apy$inout.w8   <- apy$inpart.w8 - apy$outpart.w8

apy$inpart.w8r  <- ifelse(apy$pid3r_w8=="Democrat", apy$favdem.w8, apy$favrep.w8) 
apy$outpart.w8r <- ifelse(apy$pid3r_w8=="Democrat", apy$favrep.w8, apy$favdem.w8) 
apy$inout.w8r   <- apy$inpart.w8r - apy$outpart.w8r


# Favorability Wave 9
apy$favdem.w9 <- car::recode(apy$fav2_1_w9, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$favrep.w9 <- car::recode(apy$fav2_2_w9, "1 =1; 2 =.66;3 =.33;4=0; else=NA")
apy$inpart.w9  <- ifelse(apy$pid3r=="Democrat", apy$favdem.w9, apy$favrep.w9) 
apy$outpart.w9 <- ifelse(apy$pid3r=="Democrat", apy$favrep.w9, apy$favdem.w9) 
apy$inout.w9   <- apy$inpart.w9 - apy$outpart.w9

apy$inpart.w9r  <- ifelse(apy$pid3r_w9=="Democrat", apy$favdem.w9, apy$favrep.w9) 
apy$outpart.w9r <- ifelse(apy$pid3r_w9=="Democrat", apy$favrep.w9, apy$favdem.w9) 
apy$inout.w9r   <- apy$inpart.w9r - apy$outpart.w9r

save(apy, file="C:/Users/Gaurav/Desktop/R/data/ap.2008/apy.dma.rdata")

# summary(lm(apy$inout.w9r ~ apy$spill + apy$ppage + apy$ppgender + ns(apy$ppeducat,2)))


# Analysis
load("C:/Users/Gaurav/Desktop/R/data/ap.2008/apy.dma.rdata")
# "favdem", "favrep", "inpart", "outpart", 
selection <-  c(paste(c("inout"), ".w", rep(1:9), sep=""), 
				"caseid", "state", "pid3r", "pid7", "battleground", "weight", "ppage", "ppgender", "ppeducat", "ppdma")
apyrd <- subset(apy, subset=apy$pid3r!='Independent', select=selection)
library(reshape)
names(apyrd)[1:9] <- 1:9 
a <- reshape(apyrd, idvar="caseid", varying=list(c(1:9)), direction="long", v.names="inout.w")

library(lme4)
a$time <- zero1(a$time)
b <- lmer(inout.w ~ time*battleground + as.factor(pid3r) + ppage + as.factor(ppgender) + as.factor(ppeducat) + (1|ppdma) + (1|caseid),data=a, weight=weight)
source("func/mer.R")
combo(b)
p.values.lmer(b)

# Past
summary(lm(inout ~ battleground, weight=weight, data=apyrd))
summary(lm(inout.w2 ~ battleground, weight=weight, data=apyrd))
summary(lm(inout.w3 ~ battleground, weight=weight, data=apyrd))
summary(lm(inout.w4 ~ battleground, weight=weight, data=apyrd))
summary(lm(inout.w5 ~ battleground, weight=weight, data=apyrd))
summary(lm(inout.w6 ~ battleground, weight=weight, data=apyrd))
summary(lm(inout.w7 ~ battleground, weight=weight, data=apyrd))
summary(lm(inout.w8 ~ battleground, weight=weight, data=apyrd))






