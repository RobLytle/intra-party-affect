##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##   													##
##      Polarization
##		Brady/Rivers Dataset (Yougov/Economist/Hoover)
##		Last Edited: 2/17/11 		
##   	Gaurav Sood										##
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

# Set Working dir.
	#setwd("C:/Users/gsood/Dropbox/")

# Sourcing Common Functions
	#source("func/func.R")

# Load data
	## Rivers data
	if(FALSE){
		rivers <- spss("replication-data/rivers/merged_dataset.sav")
		save(rivers, file="replication-data/rivers/merged_dataset.rdata")
	}
	
	load("replication-data/rivers/merged_dataset.rdata")

## Mnemonic Variable Names 
	names(rivers)[12:21] <- c("r.generous","r.honest","r.patriotic","r.mean","r.hypo","r.intelligent","r.selfish","r.openmind","r.closemind","r.none")
	names(rivers)[22:31] <- c("d.generous","d.honest","d.patriotic","d.mean","d.hypo","d.intelligent","d.selfish","d.openmind","d.closemind","d.none")
	names(rivers)[7] 	 <- "ukpartyid"
	names(rivers)[10]    <- "marry.r" 
	names(rivers)[11]	 <- "marry.d"
	names(rivers)[102]	 <- "marry.rel"

## Party ID
	## R, D, C, L
	rivers$strpid <- car::recode(rivers$pid7, "c(1,7)=1; c(2,6)=.5; c(3,5)=0")
	rivers$rd 	  <- car::recode(rivers$pid7, "c(5,6,7)=1; c(1,2,3)=0; else=NA")
	rivers$cl 	  <- car::recode(rivers$ukpartyid, "1=1; 2=0; else=NA")
	rivers$rcdl   <- out(rivers$rd, rivers$cl, !is.na(rivers$rd), !is.na(rivers$cl))
	#ifelse((!is.na(rivers$rd) & rivers$rd) | (!is.na(rivers$cl) & rivers$cl), 1, 0)

	rivers$pid3r  	  <- car::recode(rivers$pid7, "c(5,6,7)='Republican'; c(1,2,3)='Democrat'; else=NA") 
	rivers$ukpartyidr <- car::recode(rivers$ukpartyid, "1='Conservative'; 2='Labour'; else=NA") 
	rivers$partid 	  <- ifelse(rivers$source==2, rivers$ukpartyidr, rivers$pid3r)

## Traits
	rivers$r.generous	<- 1*(rivers$r.generous==1)
	rivers$r.honest		<- 1*(rivers$r.honest==1)
	rivers$r.patriotic	<- 1*(rivers$r.patriotic==1)
	rivers$r.intelligent<- 1*(rivers$r.intelligent==1)
	rivers$r.openmind	<- 1*(rivers$r.openmind==1)

	rivers$d.generous    <- 1*(rivers$d.generous==1)
	rivers$d.honest		 <- 1*(rivers$d.honest==1)
	rivers$d.patriotic 	 <- 1*(rivers$d.patriotic==1)
	rivers$d.intelligent <- 1*(rivers$d.intelligent==1)
	rivers$d.openmind 	 <- 1*(rivers$d.openmind==1)
	rivers$r.none		 <- 1*(rivers$r.none==1)
	
	rivers$r.mean	 	<- 1*(rivers$r.mean==1)
	rivers$r.hypo 	 	<- 1*(rivers$r.hypo==1)
	rivers$r.selfish 	<- 1*(rivers$r.selfish==1)
	rivers$r.closemind 	<- 1*(rivers$r.closemind==1)
	
	rivers$d.mean		<- 1*(rivers$d.mean==1)
	rivers$d.hypo		<- 1*(rivers$d.hypo==1)
	rivers$d.selfish	<- 1*(rivers$d.selfish==1)
	rivers$d.closemind	<- 1*(rivers$d.closemind==1)
	rivers$d.none		<- 1*(rivers$d.none==1)

	rivers$pos.r 		<- with(rivers, rowSums(cbind(r.generous, r.honest, r.patriotic, r.intelligent, r.openmind)==1))
	rivers$pos.d 		<- with(rivers, rowSums(cbind(d.generous, d.honest, d.patriotic, d.intelligent, d.openmind)==1))
	rivers$neg.r 		<- with(rivers, rowSums(cbind(r.mean, r.hypo, r.selfish, r.closemind)==1))
	rivers$neg.d 		<- with(rivers, rowSums(cbind(d.mean, d.hypo, d.selfish, d.closemind)==1))
	rivers$pos.neg.r 	<- rivers$pos.r - rivers$neg.r
	rivers$pos.neg.d 	<- rivers$pos.d - rivers$neg.d

	# Order is maintained as above (for tables)
	rivers$in.generous		<- with(rivers, out(r.generous, d.generous,	!is.na(rcdl) & rcdl==1, !is.na(rcdl) & rcdl==0))
	rivers$in.honest		<- with(rivers, out(r.honest,	d.honest,	!is.na(rcdl) & rcdl==1, !is.na(rcdl) & rcdl==0))
	rivers$in.patriotic		<- with(rivers, out(r.patriotic,d.patriotic, !is.na(rcdl) & rcdl==1, !is.na(rcdl) & rcdl==0))
	rivers$in.mean     		<- with(rivers, out(r.mean, d.mean, !is.na(rcdl) & rcdl==1, !is.na(rcdl) & rcdl==0))
	rivers$in.hypo     		<- with(rivers, out(r.hypo, d.hypo, !is.na(rcdl) & rcdl==1, !is.na(rcdl) & rcdl==0))
	rivers$in.intelligent	<- with(rivers, out(r.intelligent, d.intelligent, !is.na(rcdl) & rcdl==1, !is.na(rcdl) & rcdl==0))
	rivers$in.selfish     	<- with(rivers, out(r.selfish, d.selfish, !is.na(rcdl) & rcdl==1, !is.na(rcdl) & rcdl==0))
	rivers$in.openmind     	<- with(rivers, out(r.openmind, d.openmind, !is.na(rcdl) & rcdl==1, !is.na(rcdl) & rcdl==0))
	rivers$in.closemind     <- with(rivers, out(r.closemind, d.closemind, !is.na(rcdl) & rcdl==1, !is.na(rcdl) & rcdl==0))
	rivers$in.none     		<- with(rivers, out(r.none, d.none, !is.na(rcdl) & rcdl==1, !is.na(rcdl) & rcdl==0))
	
	rivers$out.generous		<- with(rivers, out(r.generous, d.generous, !is.na(rcdl) & rcdl==0, !is.na(rcdl) & rcdl==1))
	rivers$out.honest		<- with(rivers, out(r.honest, d.honest, !is.na(rcdl) & rcdl==0, !is.na(rcdl) & rcdl==1))
	rivers$out.patriotic	<- with(rivers, out(r.patriotic, d.patriotic, !is.na(rcdl) & rcdl==0, !is.na(rcdl) & rcdl==1))
	rivers$out.mean     	<- with(rivers, out(r.mean, d.mean, nonab(rcdl==0), nonab(rcdl==1)))
	rivers$out.hypo     	<- with(rivers, out(r.hypo, d.hypo, !is.na(rcdl) & rcdl==0, !is.na(rcdl) & rcdl==1))
	rivers$out.intelligent	<- with(rivers, out(r.intelligent, d.intelligent, !is.na(rcdl) & rcdl==0, !is.na(rcdl) & rcdl==1))
	rivers$out.selfish     	<- with(rivers, out(r.selfish, d.selfish, !is.na(rcdl) & rcdl==0, !is.na(rcdl) & rcdl==1))
	rivers$out.openmind     <- with(rivers, out(r.openmind, d.openmind, !is.na(rcdl) & rcdl==0, !is.na(rcdl) & rcdl==1))
	rivers$out.closemind    <- with(rivers, out(r.closemind, d.closemind, !is.na(rcdl) & rcdl==0, !is.na(rcdl) & rcdl==1))
	rivers$out.none     	<- with(rivers, out(r.none, d.none, !is.na(rcdl) & rcdl==0, !is.na(rcdl) & rcdl==1))
	
	rivers$pos.in 		<- with(rivers, rowSums(cbind(in.generous, in.honest, in.patriotic, in.intelligent, in.openmind)==1))
	rivers$pos.out 		<- with(rivers, rowSums(cbind(out.generous, out.honest, out.patriotic, out.intelligent, out.openmind)==1))
	rivers$neg.in 		<- with(rivers, rowSums(cbind(in.mean, in.hypo, in.selfish, in.closemind)==1))
	rivers$neg.out 		<- with(rivers, rowSums(cbind(out.mean, out.hypo, out.selfish, out.closemind)==1))
	rivers$pos.neg.out  <- rivers$pos.out - rivers$neg.out
	rivers$pos.neg.in   <- rivers$pos.in - rivers$neg.in
	rivers$net.out      <- rivers$pos.in - rivers$neg.in + rivers$neg.out - rivers$pos.out

# Marriage
	# 1 Very Upset, 2 Somewhat, 3 Not at all, 4 = Not Sure
	rivers$marry.out <- ifelse((!is.na(rivers$pid3) & rivers$pid3==1) | (!is.na(rivers$ukpartyid) & rivers$ukpartyid==2), rivers$marry.r, rivers$marry.d)
	#rivers$marry.out <- ifelse((!is.na(rivers$pid3) & !is.na(rivers$ukpartyid)) & (rivers$pid3==2 | rivers$ukpartyid==1), rivers$marry.d, rivers$marry.out)
	rivers$marry.out[!(rivers$pid3 %in% c(1,2) | rivers$ukpartyid %in% c(1,2) )] <- NA
	
	rivers$marry.outr <- car::recode(rivers$marry.out, "1=0; 2=.5; 3=1; else=NA")
	rivers$marry.relr <- car::recode(rivers$marry.rel, "1=0; 2=.5; 3=1; else=NA")
	rivers$marry.outr <- car::recode(rivers$marry.out, "1=0; 2=.5; 3=1; else=NA")
	rivers$marry.relr <- car::recode(rivers$marry.rel, "1=0; 2=.5; 3=1; else=NA")

# US and UK
	rivers$src <- ifelse(rivers$source==2, "UK", "US")

# Save File
	save(rivers, file="replication-data/rivers/rivers.rdata")


# Some Subsets
	# By Country
	r.uk <- subset(rivers, rivers$source==2)
	r.us <- subset(rivers, rivers$source==1)
	
	# By Party
	rep <- subset(rivers, rivers$pid3==2) 		   ##rivers$pid3==2 (Republican)
	dem <- subset(rivers, rivers$pid3==1)  		   ##rivers$pid3==1 (Democrat)
	con <- subset(rivers, rivers$ukpartyid==1) 	   ##rivers$ukpartyid==1 (Conservatives)
	lab <- subset(rivers, rivers$ukpartyid==2)     ##rivers$ukpartyid==2 (Labor)
	
