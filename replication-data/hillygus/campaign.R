### R code from vignette source 'C:/Users/Gaurav/Desktop/R/polar/dma/campaign.Rnw'

###################################################
### code chunk number 1: data
###################################################
# Load Libraries
library(splines)
library(apsrtable)
library(sem)

# Load Functions
source("C:/Users/Gaurav/Desktop/R/tools/functions/pr.lmer.R")

# Load Data
load("C:/Users/Gaurav/Desktop/R/data/WiscAds/2004/ad04.rdata")
load("C:/Users/Gaurav/Desktop/R/data/WiscAds/2008/ad083.rdata")

load("C:/Users/Gaurav/Desktop/R/data/Hillygus/wedgeads.rdata")

load("C:/Users/Gaurav/Desktop/R/data/CCAP/ccapsm.rdata")
load("C:/Users/Gaurav/Desktop/R/data/CCAP/ccap2.rdata")

# Creating Relevant Subsets
wedgeind <- subset(wedgeads, pid3!='Independent')
wedgb    <- subset(wedgeads, (battleground.w=='Battleground'     & pid3!='Independent'))
wedgnb   <- subset(wedgeads, (battleground.w=='Non-Battleground' & pid3!='Independent'))



###################################################
### code chunk number 2: "wedge.lm"
###################################################
# need weight1 but only have weight2; weight=weight2, 
a <- lm(inout ~   I(battleground.w=='Battleground'), data=wedgeind)
b <- lm(inout ~   I(battleground.w=='Battleground') + ppage  + ppgender + ppeducat + as.factor(pid3) + strpid + polint, weight=weight2, data=wedgeind)
c <- lm(outpart ~ I(battleground.w=='Battleground') + ppage  + ppgender + ppeducat + as.factor(pid3) + strpid + polint, weight=weight2, data=wedgeind)
d <- lm(inpart ~  I(battleground.w=='Battleground') + ppage  + ppgender + ppeducat + as.factor(pid3), weight=weight2, data=wedgeind)
e <- lm(inpart ~  I(battleground.w=='Battleground') + ppage  + ppgender + ppeducat + as.factor(pid3) + strpid + polint, weight=weight2, data=wedgeind)

apsrtable(a,b,c,d,e, label="2004inout", model.names=c("In-Out", "In-Out", "Out", "In", "In"), caption="2008: Predicting In Minus Out Using Battleground Status")


###################################################
### code chunk number 3: campaign.Rnw:229-232
###################################################
a <- lm(inout   ~ zero1(netattackge) + ppage  + ppgender + ppeducat, weight=weight2, data=wedgeind)
b <- lm(outpart ~ zero1(netattackge) + ppage  + ppgender + ppeducat, weight=weight2, data=wedgeind)
c <- lm(inpart  ~ zero1(netattackge) + ppage  + ppgender + ppeducat, weight=weight2, data=wedgeind)


###################################################
### code chunk number 4: "wedge.lm2"
###################################################
apsrtable(a,b,c, label="2004inout", model.names=c("In-Out","Out","In"), caption="2008: Predicting In Minus Out Using Net Attack in States")



###################################################
### code chunk number 5: campaign.Rnw:258-263
###################################################
# DMA - Naive Model
a <- lm(inout ~  zero1(netads), weight=weight, data=ccapsm[!is.na(ccapsm$pid3) & ccapsm$pid3!='Independent',])
b <- lm(inout ~  zero1(netads) + I(pid3=='Republican') + strpid + unclass(ns(age,2)) + male + unclass(ns(educ,2)), weight=weight, data=ccapsm[!is.na(ccapsm$pid3) & ccapsm$pid3!='Independent',])
c <- lm(inout ~  zero1(netattack), weight=weight, data=ccapsm[!is.na(ccapsm$pid3) & ccapsm$pid3!='Independent',])
#lmer(inout ~  zero1(netattack) +polint+ I(pid3=='Republican') + strpid + unclass(ns(age,2)) + male + unclass(ns(educ,2)) + (1|dma), weight=weight, data=ccapsm[!is.na(ccapsm$pid3) & ccapsm$pid3!='Independent',])


###################################################
### code chunk number 6: campaign.Rnw:265-266
###################################################
apsrtable(a,b,c, label="2008netads", model.names=c("NetAds","NetAds","NetAttack"), caption="2008: Predicting In Minus Out Using Net Ads in DMA")


###################################################
### code chunk number 7: campaign.Rnw:271-276
###################################################

a <- lm(tvads ~  I(battleground=='Battleground'), weight=weight, data=ccap2[!is.na(ccap2$pid3) & ccap2$pid3!='Independent',])
b <- lm(tvads ~  I(battleground=='Battleground') + polint + I(pid3=='Republican') + strpid + unclass(ns(age,2))  + male + unclass(ns(educ,2)), weight=weight, data=ccap2[!is.na(ccap2$pid3) & ccap2$pid3!='Independent',])

apsrtable(a,b, label="2008expose", caption="2008: Predicting Self-Reported Exposure by Battleground Status")


###################################################
### code chunk number 8: campaign.Rnw:281-284
###################################################
# Naive Model: Affective Differences between Battleground and Non-Battleground states
ccapb <- subset(ccap2, (ccap2$battleground=='Battleground' & ccap2$pid3!='Independent' & !is.na(ccap2$pid3)))
ccapnb <- subset(ccap2, (ccap2$battleground=='Non-Battleground' & ccap2$pid3!='Independent' & !is.na(ccap2$pid3)))


###################################################
### code chunk number 9: "2008.battleground.model"
###################################################
a <- lm(inout ~  I(battleground=='Battleground'), weight=weight, data=ccap2[!is.na(ccap2$pid3) & ccap2$pid3!='Independent',])
b <- lm(inout ~  I(battleground=='Battleground') + polint + I(pid3=='Republican') + strpid + unclass(ns(age,2))  + male + unclass(ns(educ,2)), weight=weight, data=ccap2[!is.na(ccap2$pid3) & ccap2$pid3!='Independent',])
apsrtable(a,b, label="2008battle", caption="2008: Predicting In Minus Out Using Battleground")


###################################################
### code chunk number 10: "2008.battleground.model"
###################################################
a <- lm(inout   ~  tvads  + polint + I(pid3=='Republican') + strpid + unclass(ns(age,2))  + male + unclass(ns(educ,2)), weight=weight, data=ccap2[!is.na(ccap2$pid3) & ccap2$pid3!='Independent',])
d <- lm(inout  ~   expose + I(ocap400_1=='Yes') + polint + I(pid3=='Republican') + strpid + unclass(ns(age,2))  + male + unclass(ns(educ,2)), weight=weight, data=ccap2[!is.na(ccap2$pid3) & ccap2$pid3!='Independent',])
b <- lm(outpart ~  tvads  + polint + I(pid3=='Republican') + strpid + unclass(ns(age,2))  + male + unclass(ns(educ,2)), weight=weight, data=ccap2[!is.na(ccap2$pid3) & ccap2$pid3!='Independent',])
e <- lm(outpart  ~ expose + I(ocap400_1=='Yes') + polint + I(pid3=='Republican') + strpid + unclass(ns(age,2))  + male + unclass(ns(educ,2)), weight=weight, data=ccap2[!is.na(ccap2$pid3) & ccap2$pid3!='Independent',])
c <- lm(inpart  ~  tvads  + polint + I(pid3=='Republican') + strpid + unclass(ns(age,2))  + male + unclass(ns(educ,2)), weight=weight, data=ccap2[!is.na(ccap2$pid3) & ccap2$pid3!='Independent',])

apsrtable(a,d, b, e, c, label="2008regress", model.names=c("In-Out", "In-Out", "Out", "Out", "In"), caption="2008, Regress - Predicting Partisan Affect Using Self-Reported Exposure")


###################################################
### code chunk number 11: "2sls"
###################################################
data <- ccap2[!is.na(ccap2$expose) & !is.na(ccap2$battleground) & !is.na(ccap2$pid3) & ccap2$pid3!='Independent',]
abc <- lm(tvads ~ I(battleground=='Battleground'), data=data)
phat = predict(abc)
bbc <- lm(inout ~ phat, weight=weight, data=data)

ccap2$bground2 <- ccap2$battleground
ccap2$bground2[ccap2$state %in% c("AL-Pensacola", "DC", "GA", "MA", "ME", "SD", "VT", "TX", "UT")] <- 'Non-Battleground'


#bbc <- tsls(inout ~ expose, ~ battleground, data =data)
apsrtable(bbc, label="2008tsls", caption="2008, 2SLS: Predicting In Minus Out Using Battleground")


###################################################
### code chunk number 12: "inadv"
###################################################



###################################################
### code chunk number 13: campaign.Rnw:337-340
###################################################
detach(package:sem)
detach(package:splines)
detach(package:apsrtable)


