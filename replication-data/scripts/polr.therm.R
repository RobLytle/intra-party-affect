##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--++
##      Polarization
##      NES + 10 NatThermometer of Parties, Lib./Con.
## 		MinMax Regret
##		Last Edited: 4.25.12  	         
##   	Gaurav Sood							    	 ## 
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--++

# Set Working dir.
	setwd(basedir)

## Load Functions
	source("func/func.R")
	source("func/chai.R")
	source("func/match.R")
	source("func/reg.R")
	source("coldstorage/polar/auxscripts/polr.func.R")
	

## LOAD DATA 
	load("coldstorage/polar/data/polar.Rdata")
	load("coldstorage/polar/data/gssp.Rdata")
	load("coldstorage/polar/data/us10r.rdata") #[Don't use therm ratings from 10 nation]

## Function to generate weighted means by Dem/Rep/Lib/Con by Year
##
lmcap <- function(data, grp1, grp2, grp3, x, y, wts) 
{	
	#tester: data <- polar; grp1 <- "surveyyr"; x <- "th.l";  y <- "th.c"; wts <- "vcf0009a"; i <- unique(data[,grp1])[1]
	#tester: x <- "libtemp";  y <- "contemp"; wts <- "wtssall"
	res <- matrix(nrow=25, ncol=8) 
	j <- 1
	for(i in unique(data[,grp1]))	#polar$surveyyr; gssp$year				# For each survey year
	{
		yr <- subset(data, subset= data[,grp1]==i, select=c(x,y, wts, grp2, grp3))		# Subset of an year
		if((sum(abs(yr[,x]), na.rm=T) > 0) & (sum(abs(yr[,grp2]), na.rm=T) > 0)) 		# If var. is there for the year
			{
				year.d <- subset(yr, subset = yr[,grp2]==1)
				year.r <- subset(yr, subset = yr[,grp3]==1)
			
				res[j,(1:2)] <- c(i, summary(lm(year.d[,1] ~ 1, weight=year.d[,3]))$coef[1,1])  #dem. rate dem.
				res[j,3] 	 <- summary(lm(year.d[,2] ~ 1, weight=year.d[,3]))$coef[1,1] 	    #dem. rate rep.
				res[j,4]     <- summary(lm(I(year.d[,1] - year.d[,2]) ~ 1, weight=year.d[,3]))$coef[1,1] #dem. (dem. - rep.)
				res[j,5]     <- summary(lm(year.r[,2] ~ 1, weight=year.r[,3]))$coef[1,1] 
				res[j,6]     <- summary(lm(year.r[,1] ~ 1, weight=year.r[,3]))$coef[1,1] 
				res[j,7]     <- summary(lm(I(year.r[,2] - year.r[,1]) ~ 1, weight=year.r[,3]))$coef[1,1] 
				res[j,8]     <- res[j,4] + res[j,7]
				j <- j + 1	
		}
	}
	colnames(res) <- c("year", "dem.dem", "dem.rep", "dem.rd", "rep.rep", "rep.dem", "rep.rd", "dem.rd-rep.rd")
	res
}

## Recoding Thermometer scores, dealing with 50
##***********************************************
	polar$thd  <- ifelse(polar$th.dem==50, NA, polar$th.dem)
	polar$thdp <- ifelse(polar$th.dem.p==50, NA, polar$th.dem.p)
	
	# Party and partisans
	th 	 <- lmcap(polar, "surveyyr", "dem", "rep", "th.dem",	"th.rep",	 "vcf0009a")  
	th.p <- lmcap(polar, "surveyyr", "dem", "rep", "th.dem.p",  "th.rep.p",  "vcf0009a")
	
	dem10 <- with(us10[!is.na(us10$rd) & us10$rd=="dem",], wt.chai(d.therm, r.therm, weight))
	rep10 <- with(us10[!is.na(us10$rd) & us10$rd=="rep",], wt.chai(r.therm, d.therm, weight))
	th.p[15,1:4] <- c(2010, as.numeric(dem10[1:2]), as.numeric(clean(dem10[3]))/100)
	th.p[15,5:7] <- c(as.numeric(rep10[1:2]), as.numeric(clean(rep10[3]))/100)
	th.p[15,8] <- th.p[15,4] + th.p[15,7]
	write.csv(rbind(th[1:10,], th.p[1:15,]), file="coldstorage/polar/res/therm.party.csv")
	
	## Correlation between scores of parties and partisans
	## High correlation will mean either semantic issues, or that people misattribute party polar
	with(subset(polar, polar$surveyyr %in% c(1980, 1982)), cor(in.rd, in.rdp, use="na.or.complete"))

## In/Out Group			##
##**********************##
	ingrp1  <- wtgrp(polar,"surveyyr", "in.rd", "vcf0009a")
	ingrp2  <- wtgrp(polar,"surveyyr", "in.rdp", "vcf0009a")
	outgrp1 <- wtgrp(polar,"surveyyr", "out.rd", "vcf0009a")
	outgrp2 <- wtgrp(polar,"surveyyr", "out.rdp", "vcf0009a")
	net1    <- plyr::ddply(polar,"surveyyr",function(X) data.frame(wmn=weighted.mean(X$in.rd - X$out.rd, X$vcf0009a, na.rm=T)))
	net2    <- plyr::ddply(polar,"surveyyr",function(X) data.frame(wmn=weighted.mean(X$in.rdp - X$out.rdp, X$vcf0009a, na.rm=T)))
	inoutrd <- as.data.frame(cbind(rbind(ingrp1[8:17,], ingrp2[15:29,]), rbind(outgrp1[8:17,], outgrp2[15:29,]), rbind(net1[8:17,], net2[15:29,])))
	rownames(inoutrd) <- 1:25
	inoutrd$source <- ifelse(rownames(inoutrd) %in% 1:10, "demrep", "party")
	finrd <- inoutrd[!rownames(inoutrd) %in% c("8","23"),c(1,2,4,6,7)]
	net3    <- plyr::ddply(polar,"surveyyr",function(X) data.frame(wmn=weighted.mean(X$in.race - X$out.race, X$vcf0009a, na.rm=T)))
	net4    <- plyr::ddply(polar,"surveyyr",function(X) data.frame(wmn=weighted.mean(X$in.rel -  X$out.rel, X$vcf0009a, na.rm=T)))
	
	finrd2 <- merge(finrd, net3, all.x=T, all.y=F, by="surveyyr")
	finrd3 <- merge(finrd2, net4, all.x=T, all.y=F, by="surveyyr")
	colnames(finrd3) <- c("year", "in", "out", "inout", "source","race","rel")

	
	#!!!!!!! Taking into account NAs for 1980 and 1982
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	polar1 <- subset(polar, !is.na(in.rd)  & !is.na(out.rd))
	polar2 <- subset(polar, !is.na(in.rdp) & !is.na(out.rdp))	
			
	ingrp1  <- wtgrp(polar1, "surveyyr", "in.rd", "vcf0009a")
	ingrp2  <- wtgrp(polar2, "surveyyr", "in.rdp", "vcf0009a")
	outgrp1 <- wtgrp(polar1, "surveyyr", "out.rd", "vcf0009a")
	outgrp2 <- wtgrp(polar2, "surveyyr", "out.rdp", "vcf0009a")
	net1    <- plyr::ddply(polar, "surveyyr", function(X) data.frame(wmn=weighted.mean(X$in.rd - X$out.rd, X$vcf0009a, na.rm=T)))
	net2    <- plyr::ddply(polar, "surveyyr", function(X) data.frame(wmn=weighted.mean(X$in.rdp - X$out.rdp, X$vcf0009a, na.rm=T)))
	inoutrd <- as.data.frame(cbind(rbind(ingrp1[8:17,], ingrp2[15:29,]), rbind(outgrp1[8:17,], outgrp2[15:29,]), rbind(net1[8:17,], net2[15:29,])))
	rownames(inoutrd) <- 1:25
	inoutrd$source <- ifelse(rownames(inoutrd) %in% 1:10, "demrep", "party")
	finrd <- inoutrd[!rownames(inoutrd) %in% c("8","23"),c(1,2,4,6,7)]
	net3    <- plyr::ddply(polar,"surveyyr",function(X) data.frame(wmn=weighted.mean(X$in.race - X$out.race, X$vcf0009a, na.rm=T)))
	net4    <- plyr::ddply(polar,"surveyyr",function(X) data.frame(wmn=weighted.mean(X$in.rel -  X$out.rel, X$vcf0009a, na.rm=T)))
	
	finrd2 <- merge(finrd, net3, all.x=T, all.y=F, by="surveyyr")
	finrd3 <- merge(finrd2, net4, all.x=T, all.y=F, by="surveyyr")
	colnames(finrd3) <- c("year", "in", "out", "inout", "source","race","rel")
	
	
	
	write.csv(finrd3, file="coldstorage/polar/res/inout.therm.party.csv")

## Lib Con # Only Post 1970	 ##
##***************************##
	l.th		 <- lmcap(polar, "surveyyr", "dem", "rep", "th.l", "th.c", "vcf0009a") 	 # by party
	ll.th		 <- lmcap(polar, "surveyyr", "lib", "con", "th.l", "th.c", "vcf0009a") 	 # This is for lib/con division
	gss.l.th.rd	 <- lmcap(gssp,   "year", "dem", "rep", "libtemp", "contemp", "wtssall") # by party
	gss.l.th	 <- lmcap(gssp,   "year", "lib", "con", "libtemp", "contemp", "wtssall") # by ideology
	
	fin 		 <- as.data.frame(rbind(l.th[1:22,], ll.th[1:18, ], gss.l.th.rd[1:3,], gss.l.th[1:3,]))
	fin$source   <- NA
	fin$source[1:21]  <- "nes.rd"
	fin$source[23:39] <- "nes.cl"
	fin$source[41:43] <- "gss.rd"
	fin$source[44:46] <- "gss.cl"
	write.csv(fin, file="coldstorage/polar/res/therm.libcon.csv")

## Sorted Partisans		##
## *********************##
polar$libdem <- ifelse(!is.na(polar$lib) & !is.na(polar$dem) & polar$lib==1 & polar$dem==1, 1, 0)
polar$libdem[is.na(polar$lib) | is.na(polar$dem)] <- NA

polar$repcon <- ifelse(!is.na(polar$con) & !is.na(polar$rep) & polar$con==1 & polar$rep==1, 1, 0)
polar$repcon[is.na(polar$con) | is.na(polar$rep)] <- NA

polar$nlibdem <- ifelse(!is.na(polar$lib) & !is.na(polar$dem) & polar$lib!=1 & polar$dem==1, 1, 0)
#polar$nlibdem[is.na(polar$lib) | is.na(polar$dem)] <- NA

polar$nrepcon <- ifelse(!is.na(polar$con) & !is.na(polar$rep) & polar$con!=1 & polar$rep==1, 1, 0)
#polar$nrepcon[is.na(polar$con) | is.na(polar$rep)] <- NA


sortrd		 <- lmcap(polar, "surveyyr", "libdem", "repcon", "th.dem",  "th.rep", "vcf0009a") 
sortrdp		 <- lmcap(polar, "surveyyr", "nlibdem", "nrepcon", "th.dem.p",  "th.rep.p", "vcf0009a") # Unsorted
sortlc		 <- lmcap(polar, "surveyyr", "libdem", "repcon", "th.l", "th.c", "vcf0009a") # This is for "sorted" lib/con division
sortall 	 <- rbind(sortrd[1:6,], sortrdp[1:15,], sortlc[1:17,])
write.csv(sortall, file="coldstorage/polar/res/sort.therm.libcon.csv")

## *************************************************##
## Putting 2008 Therm. Scores in Perspective		##
## *************************************************##

load("coldstorage/polar/data/nes08p.rdata")
	# Catholic
	weighted.mean(nes08$v085064cr[!is.na(nes08$cathprot) & nes08$cathprot==0], w=nes08$weight[!is.na(nes08$cathprot) & nes08$cathprot==0], na.rm=T) 
	# Welfare
	weighted.mean(nes08$v085064pr[!is.na(nes08$rep) & nes08$rep==1], w=nes08$weight[!is.na(nes08$rep) & nes08$rep==1], na.rm=T) 
	#[1] 49.901
	# Gay
	weighted.mean(nes08$v085064ur[!is.na(nes08$rep) & nes08$rep==1], w=nes08$weight[!is.na(nes08$rep) & nes08$rep==1], na.rm=T) 
	#[1] 42.294
	# Big Business
	weighted.mean(nes08$v085064nr[!is.na(nes08$dem) & nes08$dem==1], w=nes08$weight[!is.na(nes08$dem) & nes08$dem==1], na.rm=T) 
	#[1] 50.707
	# Republican Party Therm
	sd(car::recode(nes08$v083044b[!is.na(nes08$dem) & nes08$dem==1], "c(-6,-9,-8)=NA"), na.rm=T)

## *************************************************##
## Min Max Regret Analysis							##
## *************************************************##

load("coldstorage/polar/data/nes84p.rdata")
load("coldstorage/polar/data/nes92p.rdata")

	# Dems. hated Rs more as a result of election than -> 
	# D's decline in affect towards R > R's decline in affect towards D
	summary(lm(I(nes84$post.out.therm - nes84$pre.out.therm) ~ nes84$rd))
	summary(lm(I((nes84$post.in.therm - nes84$post.out.therm) - (nes84$pre.in.therm - nes84$pre.out.therm)) ~ nes84$rd))
	
	# Decline over election
	summary(lm(I(nes84$post.d - nes84$pre.d) ~  nes84$rd))
	summary(lm(I(nes84$post.r - nes84$pre.r) ~  nes84$rd))
	
	# Dems. hated Rs more as a result of election than -> 
	# D's decline in affect towards R > R's decline in affect towards D
	summary(lm(I(nes92$post.out.therm - nes92$pre.out.therm) ~ nes92$rd))
	summary(lm(I((nes92$post.in.therm - nes92$post.out.therm) - (nes92$pre.in.therm - nes92$pre.out.therm)) ~ nes92$rd))
	
	# Decline over election
	summary(lm(I(nes92$post.d - nes92$pre.d) ~  nes92$rd))
	summary(lm(I(nes92$post.r - nes92$pre.r) ~  nes92$rd))

##************************************************##
## PLOTTING										  ##
## Fig 1a: Thermometer Ratings by Party - pro/con ##
##************************************************##

	a <- data.frame(year=rep(th.p[1:14,1],4), mes=c(th.p[1:14,5],th.p[1:14,6],th.p[1:14,2],th.p[1:14,3]), 
			pid=c(rep("Republican - In Party",14), rep("Republican - Out Party",14), rep("Democrat - In Party",14), 
					rep("Democrat - Out Party",14)))
	a$pid <- factor(a$pid, levels=c("Republican - In Party","Republican - Out Party", "Democrat - In Party",
					"Democrat - Out Party"))
	col <- c("red", "orange", "blue", "#a3a3c3")
	d <- paste("b =", lmc2(th.p[,3], th.p[,1]))
	
	# Color
	qplot(a$year, a$mes, colour=a$pid, geom=c('point', 'smooth'), method="loess", se=F, span=1,  asp=.75, alpha = I(.4), ylim=c(20,80), 
					xlim=c(1975,2010)) + 
					scale_colour_manual(values=col) + 
					labs(list(x = "Year", y = "Mean Thermometer Rating of Partisans", size=17, colour="")) + 
					theme_bw() + 
					theme(legend.position = c(.15, .15), legend.justification = "centre") + 
					theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) + 
					annotate("text", label=paste("b =", lmc2(th.p[,3], th.p[,1])), x=2006, y=32, size=4, color="#a3a3c3") 
	#geom_text(aes(x=2003, y=25, label=paste("b =", lmc2(th.p[,3], th.p[,1])), colour=factor(a$pid)[4], hjust = 0.7, vjust = 1))
	
	ggsave("polar/fig/therm.rd.p.color.png",dpi=400, width=7, scale=1)
	ggsave("polar/fig/therm.rd.p.color.tiff",dpi=400, width=7, scale=1)
	
	# Black and White
	qplot(a$year, a$mes, shape=a$pid, asp=.75, alpha = I(.4), ylim=c(20,80), xlim=c(1975,2010)) + 
			geom_smooth(aes(a$year, a$mes, colour=a$pid, linetype=a$pid), method = "loess", se=F, span=1) + 
			theme_bw() + 
			scale_colour_manual(values=c("#aaaaaa","#bbbbbb","#555555","black"), name  ="", breaks=unique(a$pid), labels=unique(a$pid)) +
			scale_linetype(name  ="", breaks=unique(a$pid), labels=unique(a$pid)) +
			scale_shape(name="", breaks=unique(a$pid), labels=unique(a$pid)) + 
			labs(list(x = "Year", y = "Mean Thermometer Rating of Partisans", size=17)) + 
			opts(axis.text.x = theme_text(size = 12), axis.text.y = theme_text(size = 12)) + 
			opts(legend.position = c(.15, .15))
	
	ggsave("polar/fig/therm.rd.p.bw.png", dpi=400, width=7, scale=1)
	ggsave("polar/fig/therm.rd.p.bw.tiff",dpi=400, width=7, scale=1)
	
	
	# Presentation Mode
	col <- c("#b33333", "#c35555", "#7777c3", "#a3a3c3")
	
	a <- data.frame(year=rep(th.p[1:14,1],2), 
					mes=c(rowMeans(cbind(th.p[1:14,5],th.p[1:14,2])), rowMeans(cbind(th.p[1:14,6],th.p[1:14,3]))), 
					pid=c(rep("In Party",14), rep("Out Party",14)))
	
	a$pid <- factor(a$pid, levels=c("In Party","Out Party"))
	col <- c("#000000", "#7777c3")
	#d <- paste("b =", lmc2(th.p[,3], th.p[,1]))
	
	qplot(a$year, a$mes, 
					colour=a$pid, 
					geom=c('smooth'), size =I(1), method="loess", se=F, span=1,  asp=.75, alpha = I(.6), 
					ylim=c(20,80), 
					xlim=c(1975,2010)) + 
			scale_colour_manual(values=col) + 
			labs(list(x = "Year", y = "Mean Thermometer Rating of Partisans", size=17, colour="")) + 
			theme_bw() + 
			theme(axis.text.x = element_text(size = 12), 
					axis.text.y = element_text(size = 12),
					axis.title.x = element_text(colour = "white"),
					axis.title.y = element_text(colour = "white"),
					panel.background=element_rect(fill="black", colour="black"), 
					plot.background=element_rect(fill="black", colour="black"),
					panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
					panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
					axis.text = element_text(colour = "white"),
					axis.ticks =element_line(colour = "gray30"),
					legend.position = "none",
					legend.title = element_text(colour="white", size=10, face="bold"),
					legend.text = element_text(colour="white", size=10, face="bold"),
					legend.position = c(.17, .15), 
					legend.justification = "centre",
					legend.key = element_blank(),
					legend.background = element_rect(fill = "transparent", ,colour=NA, size=.5)) +
			annotate("text", label=c("In Party"),  x=2005, y=68, size=4, color=c("#000000"))  +
			annotate("text", label=c("Out Party"), x=2005, y=32, size=4, color=c("#a3a3c3")) 
	
	ggsave("polarcamp/present/fig/trendsp1.png",dpi=400, width=7, scale=1)

	col <- c("#cccccc", "#000000")
	qplot(a$year, a$mes, 
					colour=a$pid, 
					geom=c('smooth'), size =I(1), method="loess", se=F, span=1,  asp=.75, alpha = I(.6), 
					ylim=c(20,80), 
					xlim=c(1975,2010)) + 
			scale_colour_manual(values=col) + 
			labs(list(x = "Year", y = "Mean Thermometer Rating of Partisans", size=17, colour="")) + 
			theme_bw() + 
			theme(axis.text.x = element_text(size = 12), 
					axis.text.y = element_text(size = 12),
					axis.title.x = element_text(colour = "white"),
					axis.title.y = element_text(colour = "white"),
					panel.background=element_rect(fill="black", colour="black"), 
					plot.background=element_rect(fill="black", colour="black"),
					panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
					panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
					axis.text = element_text(colour = "white"),
					axis.ticks =element_line(colour = "gray30"),
					legend.position = "none",
					legend.title = element_text(colour="white", size=10, face="bold"),
					legend.text = element_text(colour="white", size=10, face="bold"),
					legend.position = c(.17, .15), 
					legend.justification = "centre",
					legend.key = element_blank(),
					legend.background = element_rect(fill = "transparent", colour=NA, size=.5)) +
			annotate("text", label=c("In Party"),  x=2005, y=68, size=4, color=c("#a3a3c3"))  +
			annotate("text", label=c("Out Party"), x=2005, y=32, size=4, color=c("#000000")) 
	
	ggsave("polarcamp/present/fig/trendsp2.png",dpi=400, width=7, scale=1)
	
	col <- c("#cccccc", "#7777c3")
	qplot(a$year, a$mes, 
					colour=a$pid, 
					geom=c('smooth'), size =I(1), method="loess", se=F, span=1,  asp=.75, alpha = I(.6), 
					ylim=c(20,80), 
					xlim=c(1975,2010)) + 
					scale_colour_manual(values=col) + 
					labs(list(x = "Year", y = "Mean Thermometer Rating of Partisans", size=17, colour="")) + 
					theme_bw() + 
					theme(axis.text.x = element_text(size = 12), 
						axis.text.y = element_text(size = 12),
						axis.title.x = element_text(colour = "white"),
						axis.title.y = element_text(colour = "white"),
						panel.background=element_rect(fill="black", colour="black"), 
						plot.background=element_rect(fill="black", colour="black"),
						panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
						panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
						axis.text = element_text(colour = "white"),
						axis.ticks =element_line(colour = "gray30"),
						legend.position = "none",
						legend.title = element_text(colour="white", size=10, face="bold"),
						legend.text = element_text(colour="white", size=10, face="bold"),
						legend.position = c(.17, .15), 
						legend.justification = "centre",
						legend.key = element_blank(),
						legend.background = element_rect(fill = "transparent", colour=NA, size=.5)) +
						annotate("text", label=c("In Party"),  x=2005, y=68, size=4, color=c("#a3a3c3"))  +
						annotate("text", label=c("Out Party"), x=2005, y=32, size=4, color=c("#a3a3c3")) 

		ggsave("polarcamp/present/fig/trendsp.png",dpi=400, width=7, scale=1)
	
## Fig 1b: Average In-Out Party Comparison to Rel/Race 								##
##**********************************************************************************##
	png("polar/fig/therm.diff.inout.comp.png", res='150', width=1650, height=1200)
		par(oma=c(.9,0,0,0), mar=c(4.5,5.7,3.2,2.1))
		plot(finrd[11:25,c(1,4)], col='white', lwd=3, lty=3, cex=1, type='p', pch=21, xlim=c(1960, 2010), ylim=c(3,38), 
				axes=FALSE, ann=FALSE)
		lines(loess.smooth(finrd3[11:25,1], finrd3[11:25,4]), lwd=3, col='#777777')
		lines(loess.smooth(finrd3[11:25,1], finrd3[11:25,c(6)]), lwd=3, lty=3, col='#7777cc')
		lines(finrd3[11:25,1], finrd3[11:25,c(7)], lwd=3, lty=3, col='#cc7777')
		lines(loess.smooth(finrd3[11:25,1], finrd3[11:25,c(7)]), lwd=3, lty=3, col='#cc7777')
		axis(1, at=c(finrd[,1]), lab=as.character(finrd[,1]), cex=1.1)
		axis(2, las=1, cex=1.1)
		box()
		title(xlab=list("Years", cex=1.25))
		title(ylab=list("Difference between in-group \n and out-group thermometer ratings" , cex=1.25))
		legend(1994, 20, col=c("#555555", "#555555", "#7777cc", "#cc7777"), pch=c(-1,-1), lwd=3, cex=1.1, lty=c(1,2,3,3), 
				c("Partisans", "Party", "Race", "Religion"))
	dev.off()
		
	png("polar/fig/therm.diff.inout.comp.color.png", res='150', width=1650, height=1200)
		par(oma=c(.9,0,0,0), mar=c(4.5,5.7,3.2,2.1))
		plot(finrd[1:10,c(1,4)], col='white', lwd=3, lty=3, cex=1, type='p', pch=21, xlim=c(1964, 2008), ylim=c(3,38), 
				axes=FALSE, ann=FALSE)
		lines(loess.smooth(finrd[1:10,c(1)], finrd[1:10,c(4)]), lwd=3, col='#777777')
		lines(loess.smooth(finrd[11:25,c(1)], finrd[11:25,c(4)]), lwd=3, lty=2, col='#777777')
		lines(loess.smooth(finrd3[,1], finrd3[,c(6)]), lwd=3, lty=3, col='#7777cc')
		lines(loess.smooth(finrd3[,1], finrd3[,c(7)]), lwd=3, lty=3, col='#cc7777')
		axis(1, at=c(finrd[,1]), lab=as.character(finrd[,1]), cex=1.1)
		axis(2, las=1, cex=1.1)
		box()
		title(xlab=list("Years", cex=1.25))
		title(ylab=list("Difference between in-group \n and out-group thermometer ratings" , cex=1.25))
		legend(1994, 20, col=c("#555555", "#555555", "#7777cc", "#cc7777"), pch=c(-1,-1), lwd=3, cex=1.1, lty=c(1,2,3,3), 
				c("Democrats/Republicans", "Democratic/Republican Party", "Race (Whites/Blacks)", 
						"Religion (Catholic/Protestant)"))
	dev.off()
	
	# Present
	png("in.n.out/present/figs/thermDiffInoutComp.png", res='150', width=1650, height=1200)
		par(oma=c(.9,0,0,0), mar=c(4.5,5.7,3.2,2.1), bg = "black", bg="black", fg="white", col.lab="#cccccc", col.main="#eeeeee", col.axis="#dddddd")
		plot(finrd[1:10,c(1,4)], col='black', lwd=3, lty=3, cex=1, type='p', pch=21, xlim=c(1964, 2008), ylim=c(3,38), 
				axes=FALSE, ann=FALSE)
		lines(loess.smooth(finrd[1:10,c(1)], finrd[1:10,c(4)]), lwd=3, col='#777777')
		lines(loess.smooth(finrd[11:25,c(1)], finrd[11:25,c(4)]), lwd=3, lty=2, col='#777777')
		lines(loess.smooth(finrd3[,1], finrd3[,c(6)]), lwd=3, lty=3, col='#7777cc')
		lines(loess.smooth(finrd3[,1], finrd3[,c(7)]), lwd=3, lty=3, col='#cc7777')
		axis(1, at=c(finrd[,1]), lab=as.character(finrd[,1]), cex=1.1)
		axis(2, las=1, cex=1.1)
		box()
		title(xlab=list("Years", cex=1.25))
		title(ylab=list("Difference between in-group \n and out-group thermometer ratings" , cex=1.25))
		legend(1994, 20, col=c("#555555", "#555555", "#7777cc", "#cc7777"), pch=c(-1,-1), lwd=3, cex=1.1, lty=c(1,2,3,3), 
				c("Democrats/Republicans", "Democratic/Republican Party", "Race (Whites/Blacks)", 
						"Religion (Catholic/Protestant)"))
	dev.off()
	
	good <- complete.cases(finrd3[,1], finrd3[,c(7)])
    scatter.smooth(finrd3[good,1], finrd3[good,7])
	require(spline)
	yy <-predict(interpSpline(finrd3[good,1], finrd3[good,7]))
	
	# Black and White with points 
	png("polar/fig/therm.diff.inout.comp.bw.png", res='150', width=1650, height=1200)
		par(oma=c(.9,0,0,0), mar=c(4.5,5.7,3.2,2.1))
		plot(finrd[1:10,c(1,4)], col='white', lwd=3, lty=3, cex=1, type='p', pch=21, xlim=c(1964, 2008), ylim=c(3,38), 
				axes=FALSE, ann=FALSE)
		points(finrd[1:10,c(1)], finrd[1:10,c(4)], col='#777777', pch=20)
		lines(loess.smooth(finrd[1:10,c(1)], finrd[1:10,c(4)]), lwd=3, col='#777777')
		points(finrd[11:25,c(1)], finrd[11:25,c(4)], col='#777777', pch=20)
		lines(loess.smooth(finrd[11:25,c(1)], finrd[11:25,c(4)]), lwd=3, lty=2, col='#777777')
		points(finrd3[,1], finrd3[,c(6)], col='#aaaaaa', pch=3)
		lines(loess.smooth(finrd3[,1], finrd3[,c(6)]), lwd=3, lty='dotted', col='#aaaaaa')
		lines(loess.smooth(finrd3[,1], finrd3[,c(7)]), lwd=3, lty=3, col='black')
		points(finrd3[good,1], finrd3[good,7], type="o", lwd=2, cex=1.2, pch="")
		axis(1, at=c(finrd[,1]), lab=as.character(finrd[,1]), cex=1.1)
		axis(2, las=1, cex=1.1)
		box()
		title(xlab=list("Years", cex=1.25))
		title(ylab=list("Difference between in-group \n and out-group thermometer ratings" , cex=1.25))
		legend(1964, 38, col=c("#777777", "#777777", "#aaaaaa", "#000000"), pch=c(-1,-1), lwd=3, cex=1.1, lty=c(1,2,4,1), 
				c("Democrats/Republicans", "Democratic/Republican Party", "Race (Whites/Blacks)", 
						"Religion (Catholic/Protestant)"))
	dev.off()
	
	# Black and white without points
	tiff("polar/fig/therm.diff.inout.comp.bw.tiff", res='300', width=3250, height=2700)
	#png("polar/fig/therm.diff.inout.comp.bw.png", res='150', width=1650, height=1200)
		par(oma=c(.9,0,0,0), mar=c(4.5,5.7,3.2,2.1))
		plot(finrd[1:10,c(1,4)], col='white', lwd=3, lty=3, cex=1, type='p', pch=21, xlim=c(1964, 2008), ylim=c(3,38), 
				axes=FALSE, ann=FALSE)	
		lines(loess.smooth(finrd[1:10,c(1)], finrd[1:10,c(4)]), lwd=3, col='#777777')
		lines(loess.smooth(finrd[11:25,c(1)], finrd[11:25,c(4)]), lwd=3, lty=2, col='#777777')
		lines(loess.smooth(finrd3[,1], finrd3[,c(6)]), lwd=3, lty='dotted', col='#aaaaaa')
		#lines(loess.smooth(finrd3[,1], finrd3[,c(7)]), lwd=3, lty=3, col='black')
		lines(yy, lwd=2)
		#points(finrd3[good,1], finrd3[good,7], type="o", lwd=2, cex=1.2, pch="")
		axis(1, at=c(finrd[,1]), lab=as.character(finrd[,1]), cex=1.1)
		axis(2, las=1, cex=1.1)
		box()
		title(xlab=list("Years", cex=1.25))
		title(ylab=list("Difference between in-group \n and out-group thermometer ratings" , cex=1.25))
		legend(1964, 38, col=c("#777777", "#777777", "#aaaaaa", "#000000"), pch=c(-1,-1), lwd=3, cex=1.1, lty=c(1,2,3,1), 
				c("Democrats/Republicans", "Democratic/Republican Party", "Race (Whites/Blacks)", 
						"Religion (Catholic/Protestant)"))
	dev.off()
	
	# Color without points
	tiff("polar/fig/therm.diff.inout.comp.color.tiff", res='300', width=3250, height=2700)
	#png("polar/fig/therm.diff.inout.comp.bw.png", res='150', width=1650, height=1200)
		par(oma=c(.9,0,0,0), mar=c(4.5,5.7,3.2,2.1))
		plot(finrd[1:10,c(1,4)], col='white', lwd=3, lty=3, cex=1, type='p', pch=21, xlim=c(1964, 2008), ylim=c(3,38), 
				axes=FALSE, ann=FALSE)	
		lines(loess.smooth(finrd[1:10,c(1)], finrd[1:10,c(4)]), lwd=3, col='#777777')
		lines(loess.smooth(finrd[11:25,c(1)], finrd[11:25,c(4)]), lwd=3, lty=2, col='#777777')
		lines(loess.smooth(finrd3[,1], finrd3[,c(6)]), lwd=3, lty='dotted', col='#7777cc')
		#lines(loess.smooth(finrd3[,1], finrd3[,c(7)]), lwd=3, lty=3, col='black')
		lines(yy, lwd=2, col='#cc7777')
		#points(finrd3[good,1], finrd3[good,7], type="o", lwd=2, cex=1.2, pch="")
		axis(1, at=c(finrd[,1]), lab=as.character(finrd[,1]), cex=1.1)
		axis(2, las=1, cex=1.1)
		box()
		title(xlab=list("Years", cex=1.25))
		title(ylab=list("Difference between in-group \n and out-group thermometer ratings" , cex=1.25))
		legend(1964, 38, col=c("#777777", "#777777", "#7777cc", "#cc7777"), pch=c(-1,-1), lwd=3, cex=1.1, lty=c(1,2,3,1), 
				c("Democrats/Republicans", "Democratic/Republican Party", "Race (Whites/Blacks)", 
						"Religion (Catholic/Protestant)"))
	dev.off()


## Fig 1c: Thermometer Ratings of Ideological Groups by Members of Ideological Groups	##
##**************************************************************************************##
	a <- data.frame(year=rep(ll.th[1:17,1],4), mes=c(ll.th[1:17,5],ll.th[1:17,6],ll.th[1:17,2],ll.th[1:17,3]), 
			pid=c(rep("Conservative - In Group",17), rep("Conservative - Out Group",17), rep("Liberal - In Group",17), 
					rep("Liberal - Out Group",17)))
	a$pid <- factor(a$pid, levels=c("Conservative - In Group","Conservative - Out Group", "Liberal - In Group",
					"Liberal - Out Group"))
	col <- c("red", "orange", "blue", "#a3a3c3")
	
	
	qplot(a$year, a$mes, colour=a$pid, geom=c('point', 'smooth'),  method = "loess", se=F, span=1,  asp=.75, alpha = I(.4), ylim=c(20,80), 
					xlim=c(1970,2010)) + scale_colour_manual(values=col)  + 
					labs(list(x = "Year", y = "Mean Thermometer Rating of Ideological Groups", size=17, colour="")) + 
					theme_bw() + opts(legend.position = c(.2, .2)) + 
					opts(axis.text.x = theme_text(size = 12), axis.text.y = theme_text(size = 12)) 
			
	ggsave("polar/fig/therm.lc.lc.color.png",  dpi=400, width=7, scale=1)
	ggsave("polar/fig/therm.lc.lc.color.tiff", dpi=400, width=7, scale=1)
			
	qplot(a$year, a$mes, shape=a$pid, asp=.75, alpha = I(.4), ylim=c(20,80), xlim=c(1970,2010)) + 
			geom_smooth(aes(a$year, a$mes, colour=a$pid, linetype=a$pid), method = "loess", se=F, span=1) + 
			theme_bw() + 
			scale_colour_manual(values=c("#aaaaaa","#bbbbbb","#555555","black"), name  ="", breaks=unique(a$pid), labels=unique(a$pid)) +
			scale_linetype(name  ="", breaks=unique(a$pid), labels=unique(a$pid)) +
			scale_shape(name="", breaks=unique(a$pid), labels=unique(a$pid)) + 
			labs(list(x = "Year", y = "Mean Thermometer Rating of Ideological Groups", size=17)) + 
			opts(axis.text.x = theme_text(size = 12), axis.text.y = theme_text(size = 12)) + opts(legend.position = c(.15, .15))
	
	
	ggsave("polar/fig/therm.lc.lc.bw.png",  dpi=400, width=7, scale=1)
	ggsave("polar/fig/therm.lc.lc.bw.tiff", dpi=400, width=7, scale=1)
	
	
## Fig 1d: Thermometer Ratings of Ideological Groups by Sorted Members of Ideological Groups   ##
##*********************************************************************************************##

# *******************
# Activists
# ******************
netty   <- plyr::ddply(polar,"surveyyr",function(X) 
			data.frame(
					wmn=weighted.mean(X$in.rdp[!is.na(X$participation) & X$participation > .4] - X$out.rdp[!is.na(X$participation) & X$participation > .4], X$vcf0009a[!is.na(X$participation) & X$participation > .4], na.rm=T), 
					wmn2=weighted.mean(X$in.rdp[!is.na(X$participation) & X$participation < .6] - X$out.rdp[!is.na(X$participation) & X$participation < .6], X$vcf0009a[!is.na(X$participation) & X$participation < .6], na.rm=T)))
netty$diff <- netty[,2] - netty[,3]
plot(netty[,1], netty[,4], type="b")
plot(netty[,1], netty[,2], type="b", ylim=c(20,65))
points(netty[,1], netty[,3], type="b", col="red")

