##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##   											
##     CFA - Affective Polarization
##	   Last Edited: 4.18.12   	
##     Yph Lelkes (edited, expanded, amended, and parts of it tested by Gaurav Sood)
## 	   GS Notes: 9/3/12: 
##		1. Not sure why absolute value of in-out therm is regressed. Think it is incorrect
##		2. Separate out In and Out therm and you see that relationship with in-therm ratings is non-existent, mostly out-therm rel.
## 		2004 Dems: In-therm: SW and CU both insignificant, Out highly significant for SW (-.16 for SW); Reps: In-therm: SQ and CU both insignificant, Out highly significant for SW (-.19 for SW)
##		1988 Dems: In-therm: SW is significant (.10); Out highly significant (-.16); Reps: In-terms: NS; Out: -.15 (for SW)
##		3. Note that coef. on Strong PID increased in 2004 (diff. with 1988 is sig.)
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

# Set Working dir. 
	setwd(basedir)
	
# Sourcing Common Functions
	source("func/func.R")
	source("func/polr.func.R")

# Load libraries
	library(Amelia)
	library(sem)
	library(mitools)
	library(arm)

#
# 2004 NES  ~~
# ~~~~~~~~~~~~~~~~~~~~~

# Load data
	a2004			<- foreign::read.spss("data/nes04/anes2004.POR", to.data.frame=T)

# Issues
	# Econ.
	a2004$insurance <- car::recode(as.numeric(a2004$V043150),"8:1000=NA")
	a2004$jobs 		<- car::recode(as.numeric(a2004$V043152),"8:1000=NA")
	a2004$services  <- abs(car::recode(as.numeric(a2004$V043136),"8:1000=NA")-8) # nice
	a2004$ss 		<- car::recode(as.numeric(a2004$V043165),"1=1; 2=3;4=3;3=2;else=NA")
	
	# Moral
	a2004$women 	<- car::recode(as.numeric(a2004$V043196),"8:1000=NA")
	a2004$abortion  <- abs(car::recode(as.numeric(a2004$V045132),"5:7=NA")-5)
	a2004$gayrights <- car::recode(as.numeric(a2004$V045156a),"5=NA")

# Demographics, PID etc.
	a2004$demrep 	<- car::recode(as.numeric(a2004$V043116),"8:10=NA")
	a2004$libcon 	<- car::recode(as.numeric(a2004$V043085),"8:10=NA")

	a2004$educ      <- as.numeric(a2004$V043254)
	a2004$education <- factor(car::recode(as.numeric(a2004$V043254),"3:4='High school';5:6='Some college';7:9='College or Higher';else='Less'"))
	a2004$education <- relevel(a2004$education,ref="Less")
	
	
	a2004$race 		<- car::recode(as.numeric(a2004$V043299a),"5='White';7:8=NA;1:4='Other';6='Other'")
	a2004$region 	<- a2004$V041205
	a2004$south 	<- car::recode(as.numeric(a2004$V041205),"3=1;else=0")
	a2004$gender 	<- a2004$V041109a
	a2004$interest 	<- zero1(car::recode(as.numeric(a2004$V043001),"3=1;2=2;1=3;else=NA"))
	
	a2004$worship 	<- car::recode(as.numeric(a2004$V043224),"6:7=NA")

# Party Feeling therms.
	a2004$fthermdem <- car::recode(a2004$V043049,"101:1000=NA")
	a2004$fthermrep <- car::recode(a2004$V043050,"101:1000=NA")

	a2004$fthermout <- NA 
	a2004$fthermout[which(as.numeric(a2004$demrep)<4)] <- a2004$fthermrep[which(as.numeric(a2004$demrep)<4)]
	a2004$fthermout[which(as.numeric(a2004$demrep)>4)] <- a2004$fthermdem[which(as.numeric(a2004$demrep)>4)]
	
	a2004$fthermin <- NA 
	a2004$fthermin[which(as.numeric(a2004$demrep)>4)] <- a2004$fthermrep[which(as.numeric(a2004$demrep)>4)]
	a2004$fthermin[which(as.numeric(a2004$demrep)<4)] <- a2004$fthermdem[which(as.numeric(a2004$demrep)<4)]

	a2004 <- subset(a2004,is.na(demrep)==F)

#
#  Political Knowledge
	
	# Interviewer Assesses Knowledge (2004)
	a2004$iwrpk_pre <- car::recode(a2004$V043403, "0=NA; 1=1; 2=.75; 3=.5; 4=.25; 5=0")
	a2004$iwrpk_pst <- car::recode(a2004$V045303, "0=NA; 1=1; 2=.75; 3=.5; 4=.25; 5=0")
	a2004$iwrpk = with(a2004, rowMeans(cbind(iwrpk_pre, iwrpk_pst), na.rm=T))
	
	# Office Recognition (2004) 
	# Items from Post-election questionnaire; there is also information on whether DK probe was used)
	a2004$know_hastert   <- car::recode(a2004$V045162,"1=1; c(8, 5)=0")
	a2004$know_cheney    <- car::recode(a2004$V045163,"1=1; c(8, 5)=0")
	a2004$know_blair     <- car::recode(a2004$V045164,"1=1; c(8, 5)=0")
	a2004$know_rehnquist <- car::recode(a2004$V045165,"1=1; c(8, 5)=0")
	
	# Maj. Party
	a2004$know_house  <- car::recode(a2004$V045089,"5=1; c(8, 1)=0")
	a2004$know_senate <- car::recode(a2004$V045090,"5=1; c(8, 1)=0")
	
	a2004$pk = with(a2004, rowMeans(cbind(know_hastert, know_cheney, know_blair,  know_rehnquist,  know_house,  know_senate), na.rm=T))

# Party and Candidate Placement
# Ideology or libcon scale; lc is short for libcon
	a2004$lc_bush   <- car::recode(as.numeric(a2004$V043087),"c(8,9)=NA")
	a2004$lc_kerry  <- car::recode(as.numeric(a2004$V043088),"c(8,9)=NA")
	a2004$lc_nader  <- car::recode(as.numeric(a2004$V043089),"c(8,9)=NA")
	a2004$lc_dem    <- car::recode(as.numeric(a2004$V043090),"c(8,9)=NA")
	a2004$lc_rep    <- car::recode(as.numeric(a2004$V043091),"c(8,9)=NA")
	
	# Some people think the government should provide fewer services even in areas such as health and
	a2004$serv_bush  <- car::recode(as.numeric(a2004$V043138),"c(8,9)=NA")
	a2004$serv_kerry <- car::recode(as.numeric(a2004$V043139),"c(8,9)=NA")
	a2004$serv_dem   <- car::recode(as.numeric(a2004$V043140),"c(8,9)=NA")
	a2004$serv_rep   <- car::recode(as.numeric(a2004$V043141),"c(8,9)=NA")
	
	# Some people feel the government in Washington should see to it that every person has a job and a
	a2004$job_bush   <- car::recode(as.numeric(a2004$V043154),"c(8,9)=NA")
	a2004$job_kerry  <- car::recode(as.numeric(a2004$V043155),"c(8,9)=NA")
	a2004$job_dem    <- car::recode(as.numeric(a2004$V043156),"c(8,9)=NA")
	a2004$job_rep    <- car::recode(as.numeric(a2004$V043157),"c(8,9)=NA")
	
	# Some people feel that the government in Washington should make every effort to improve the social
	a2004$blk_bush   <- car::recode(as.numeric(a2004$V043160),"c(8,9)=NA")
	a2004$blk_kerry  <- car::recode(as.numeric(a2004$V043161),"c(8,9)=NA")
	a2004$blk_dem    <- car::recode(as.numeric(a2004$V043162),"c(8,9)=NA")
	a2004$blk_rep    <- car::recode(as.numeric(a2004$V043163),"c(8,9)=NA")

	# Internet
	a2004$internet <- as.numeric(a2004$V045155)==1


#
# IMPUTE MISSING DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
		
	# Creating 5 sets than 25
	# All of the ordinal variables are going to be taken to be continuous (previous version was sub-optimal) 
	ad04   <- with(a2004, data.frame(V040001, V043299a, internet, insurance, jobs, services, ss, women, abortion, gayrights, demrep, libcon, south, educ, pk, gender, fthermin,fthermout, interest, 
									 lc_bush, lc_kerry, lc_nader, lc_dem, lc_rep, serv_bush, serv_kerry, serv_dem, serv_rep, job_bush, job_kerry, job_dem, job_rep, 
									 blk_bush, blk_kerry, blk_dem, blk_rep))
			
	aout04 <- amelia(ad04,noms=c("V043299a","south","gender","demrep", "internet"), idvars="V040001", m=5)
	
	# Some testing; good
	overimpute(aout04, var = "insurance")

	# Race variable
	aout04$imputations <- lapply(aout04$imputations,  function(x) { x[,'race'] <- car::recode(x$V043299a,"5='White';7:8=NA;1:4='Other';6='Other'"); return(x)})
	
	# Education
	aout04$imputations <- lapply(aout04$imputations, function(x) { x[,'education'] <- car::recode(x$educ,"3:4='High school';5:6='Some college';7:9='College or Higher';else='Less'"); return(x)})
	aout04$imputations <- lapply(aout04$imputations, transform, education = relevel(a2004$education,ref="Less"))
	
#
# DEFINE TWO FACTOR SEM
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Good. May want to do it via OpenMx (>> sem)
	library(sem)
	model.sem <- specifyModel()
	SW -> insurance, lam1, NA
	SW -> jobs,      lam2, NA
	SW -> services,  lam3, NA
	SW -> ss,        lam4, NA
	CU -> women,     lam6, NA
	CU -> abortion,  lam7, NA
	CU -> gayrights, lam8, NA
	SW <-> SW,       NA, 1
	CU <-> CU,       NA, 1
	CU <-> SW,       eps3, NA
	insurance <-> insurance, d1, NA
	jobs <-> jobs,           d2, NA
	services <-> services,   d3, NA
	ss <-> ss,               d4, NA
	women <-> women,         d7, NA
	abortion <-> abortion,   d8, NA
	gayrights <-> gayrights, d9, NA

#
# APPLY SEM TO EACH IMPUTED DATASET
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	a04   <- lapply(aout04$imputations, function(x) with(x,cor(data.frame(insurance,jobs,services,ss,women,abortion, gayrights))))
	sem04 <- lapply(a04,function(x)sem(model.sem,x,nrow(aout04$imputations[[1]])))

# EXTRACT FSCORES FROM EACH IMPUTED DATASET

# No. of imputed datasets
q <- 5

d04f <- list()
d04d <- list()
for(i in 1:q){
  d04f[[i]] <- fscores(sem04[[i]],aout04$imputations[[i]],scale=T,center=F)
  d04d[[i]] <- data.frame(d04f[[i]],aout04$imputations[[i]])
}


CU01 <- lapply(d04d,function(x) zero1(x$CU))
SW01 <- lapply(d04d,function(x) zero1(x$SW))


for(i in 1:q){
  d04d[[i]] <- data.frame(d04d[[i]],CU01=CU01[[i]])
  d04d[[i]] <- data.frame(d04d[[i]],SW01=SW01[[i]])
  
}

for(i in 1:q){
  d04d[[i]] <- data.frame(d04d[[i]],CU01r =abs(d04d[[i]]$CU01-1))
  d04d[[i]] <- data.frame(d04d[[i]],SW01r =abs(d04d[[i]]$SW01-1))	
}



#######LM FOR Democrats
library(mitools)
	d04dd     <- lapply(d04d,function(x)subset(x,demrep<4))
	imlist04d <- imputationList(d04dd)
	im04outdd <- with(imlist04d,lm(zero1(abs(fthermin-fthermout))~CU01r+SW01r+internet*interest+(demrep==1)+gender+race+south+education))
	
	# In and Out Separated
	im04out1dd <- with(imlist04d,lm(zero1(fthermin)~CU01r+SW01r+interest+(demrep==1)+gender+race+south+education))
	im04out2dd <- with(imlist04d,lm(zero1(fthermout)~CU01r+SW01r+interest+(demrep==1)+gender+race+south+education))
	
	betas     <- MIextract(im04outdd,fun=coef)
	vars      <- MIextract(im04outdd, fun=vcov)
	dems      <- as.data.frame(summary(MIcombine(betas,vars)))

write.csv(dems,file="polar/results/dem04coefs.csv")

library(arm)
rs04d <- mean(unlist(lapply(im04outdd,function(x)summary(x)$adj.r.squared)))

#######LM FOR REPUBLICANS
d04dr 	  <- lapply(d04d,function(x)subset(x,demrep>4))
imlist04r <- imputationList(d04dr)
im04outdr <- with(imlist04r,lm(zero1(abs(fthermin-fthermout))~CU01+SW01+(demrep==7)+interest*internet+gender+race+south+education))
betas     <- MIextract(im04outdr,fun=coef)
vars      <- MIextract(im04outdr, fun=vcov)
reps      <- as.data.frame(summary(MIcombine(betas,vars)))
write.csv(reps,file="polar/results/rep04coefs.csv")

display(im04outdr[[1]])

rs04r <- mean(unlist(lapply(im04outdr,function(x)summary(x)$adj.r.squared)))

#
# 1988 NES  ~~
# repeat exercise for 1988 dataset
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load data
	a1988 <- foreign::read.spss("data/nes88/anes1988.POR", to.data.frame=T)

# Econ.
	a1988$insurance <- car::recode(as.numeric(a1988$V880318),"8:1000=NA")
	a1988$jobs <- car::recode(as.numeric(a1988$V880323),"8:1000=NA")
	a1988$services <- abs(car::recode(as.numeric(a1988$V880302),"8:1000=NA")-8)
	a1988$ss <- car::recode(as.numeric(a1988$V880348),"1=1; 2=2;4=3;3=3;else=NA")

# Moral
	a1988$women <- car::recode(as.numeric(a1988$V880387),"8:1000=NA")
	a1988$abortion <- abs(car::recode(as.numeric(a1988$V880395),"5:7=NA")-5)
	a1988$gayrights <- car::recode(as.numeric(a1988$V880853),"6=NA")

# Background
	a1988$demrep <- car::recode(as.numeric(a1988$V880274),"8:10=NA")
	#a1988$libcon <- car::recode(as.numeric(a2004$V043085),"8:10=NA")
	a1988$education <- factor(car::recode(as.numeric(a1988$V880422),"3:4='High school';5:6='Some college';7:9='College or Higher';else='Less'"))
	a1988$education <- relevel(a1988$education,ref="Less")
	
	a1988$race <- car::recode(as.numeric(a1988$V880412),"1='White';else='Other'")
	
	a1988$region <- a1988$V880008
	a1988$south <- car::recode(as.numeric(a1988$V880008),"3=1;else=0")
	a1988$gender <- a1988$V880413
	a1988$interest <- zero1(car::recode(as.numeric(a1988$V880097),"3=1;2=2;1=3;else=NA"))

# Party Therm. Ratings
	a1988$fthermdem <- car::recode(a1988$V880164,"101:1000=NA")
	a1988$fthermrep <- car::recode(a1988$V880165,"101:1000=NA")
	
	a1988$fthermout <- NA 
	a1988$fthermout[which(as.numeric(a1988$demrep)<4)] <- a1988$fthermrep[which(as.numeric(a1988$demrep)<4)]
	a1988$fthermout[which(as.numeric(a1988$demrep)>4)] <- a1988$fthermdem[which(as.numeric(a1988$demrep)>4)]
	
	a1988$fthermin <- NA 
	a1988$fthermin[which(as.numeric(a1988$demrep)>4)] <- a1988$fthermrep[which(as.numeric(a1988$demrep)>4)]
	a1988$fthermin[which(as.numeric(a1988$demrep)<4)] <- a1988$fthermdem[which(as.numeric(a1988$demrep)<4)]

	a1988 <- subset(a1988,is.na(demrep)==F)

#
# IMPUTE MISSING DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
	library(Amelia)
	ad04 <- with(a1988, data.frame(insurance, jobs, services, ss, women, abortion, demrep, race, south,education, gender, gayrights,fthermin,fthermout,interest))
	aout04 <- amelia(ad04,noms=c("race","south","gender","demrep"), ords=c("insurance","jobs","services","ss","women","abortion","education","gayrights"),m=25)

#
# DEFINE TWO FACTOR SEM
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

	library(sem)
	
	model.sem <- specifyModel()
	SW -> insurance, lam1, NA
	SW -> jobs, lam2, NA
	SW -> services, lam3, NA
	SW -> ss, lam4, NA
	CU -> women, lam6, NA
	CU -> abortion, lam7, NA
	CU -> gayrights, lam8, NA
	SW <-> SW, NA, 1
	CU <-> CU, NA, 1
	CU <-> SW, eps3, NA
	insurance <-> insurance, d1, NA
	jobs <-> jobs, d2, NA
	services <-> services, d3, NA
	ss <-> ss, d4, NA
	women <-> women, d7, NA
	abortion <-> abortion, d8, NA
	gayrights <-> gayrights, d9, NA


a04  <- lapply(aout04$imputations,function(x) with(x,cor(data.frame(insurance,jobs,services,ss,women,abortion, gayrights))))

sem04 <- lapply(a04,function(x)sem(model.sem,x,nrow(aout04$imputations[[1]])))

d04f <- list()
for(i in 1:25){
  d04f[[i]] <- fscores(sem04[[i]],aout04$imputations[[i]],scale=T,center=F)
}


cor(d04f[[1]])

d04d <- list()
for(i in 1:25){
  d04d[[i]] <- data.frame(d04f[[i]],aout04$imputations[[i]])
}

CU01 <- lapply(d04d,function(x) zero1(x$CU))
SW01 <- lapply(d04d,function(x) zero1(x$SW))


for(i in 1:25){
  d04d[[i]] <- data.frame(d04d[[i]],CU01=CU01[[i]])
  d04d[[i]] <- data.frame(d04d[[i]],SW01=SW01[[i]])
  
}

for(i in 1:25){
  d04d[[i]] <- data.frame(d04d[[i]],CU01r =abs(d04d[[i]]$CU01-1))
  d04d[[i]] <- data.frame(d04d[[i]],SW01r =abs(d04d[[i]]$SW01-1))	
}


d04db <-	lapply(d04d,transform, function(x) car::recode(x$demrep,"1:3='Democrat';5:7='Republican';else=NA"))

for(i in 1:25){
  d04d[[i]] <- data.frame(d04d[[i]],CU01b = ifelse(d04d[[i]]$demrep=='Democrat',d04d[[i]]$CU01r, d04d[[i]]$CU01))
  d04d[[i]] <- data.frame(d04d[[i]],SW01b = ifelse(d04d[[i]]$demrep=='Democrat',d04d[[i]]$SW01r, d04d[[i]]$SW01))
  
}


library(mitools)
	d04dd <- lapply(d04d,function(x)subset(x,demrep<4))
	imlist04d <- imputationList(d04dd)
	# Remark (GS): 9/3/2012: Not sure why absolute has been taken for in and out therm - relationship should be directional
	im04outdd <- with(imlist04d,lm(zero1(abs(fthermin-fthermout))~CU01r+SW01r+interest+(demrep==1)+gender+race+south+education))
	
	# In and Out separated
	im04out1dd <- with(imlist04d,lm(zero1((fthermin))~CU01r+SW01r+interest+(demrep==1)+gender+race+south+education))
	im04out2dd <- with(imlist04d,lm(zero1((fthermout))~CU01r+SW01r+interest+(demrep==1)+gender+race+south+education))
	
	betas<-MIextract(im04outdd,fun=coef)
	vars<-MIextract(im04outdd, fun=vcov)
	dems <- as.data.frame(summary(MIcombine(betas,vars)))
	write.csv(dems,file="dem88coefs.csv")
	ds88d <- mean(unlist(lapply(im04outdd,function(x)summary(x)$adj.r.squared)))

library(arm)
	d04dr <- lapply(d04d,function(x)subset(x,demrep>4))
	imlist04r <- imputationList(d04dr)
	im04outdr <- with(imlist04r,lm(zero1(abs(fthermin-fthermout))~CU01+SW01+(demrep==7)+interest+gender+race+south+education))
	betas<-MIextract(im04outdr,fun=coef)
	vars<-MIextract(im04outdr, fun=vcov)
	reps <- as.data.frame(summary(MIcombine(betas,vars)))
	write.csv(reps,file="rep88coefs.csv")
	ds88r <- mean(unlist(lapply(im04outdr,function(x)summary(x)$adj.r.squared)))
