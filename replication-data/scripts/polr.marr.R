##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--++
##      Polarization
##      Marriage
##		Last Edited: 4.25.12  	         
##   	Gaurav Sood							   ## 
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--++

# Set Working dir.
	setwd(basedir)

## LOAD DATA 
	load("coldstorage/polar/data/rivers.rdata")
	load("coldstorage/polar/data/verba.rdata")
	load("coldstorage/polar/data/us10r.rdata")
	load("coldstorage/polar/data/uk10.rdata")

## Load Functions
	source("func/func.R")
	source("coldstorage/polar/auxscripts/polr.func.R")

# Some subsets
	# Rivers
	rep <- subset(rivers, rivers$pid3==2) 		   ##rivers$pid3==2 (Republican)
	dem <- subset(rivers, rivers$pid3==1)  		   ##rivers$pid3==1 (Democrat)
	con <- subset(rivers, rivers$ukpartyid==1) 	   ##rivers$ukpartyid==1 (Conservatives)
	lab <- subset(rivers, rivers$ukpartyid==2)     ##rivers$ukpartyid==2 (Labor)

	r.us <- subset(rivers, rivers$source==1 & rivers$pid3 %in% c(1,2)) 
	r.uk <- subset(rivers, rivers$source==2 & rivers$ukpartyid %in% c(1,2)) 
	
	# Verba
	v.rep <- subset(verba, tolower(verba$v2)=="united states" &   verba$pid5 > 3)
	v.dem <- subset(verba, tolower(verba$v2)=="united states" &   verba$pid5 < 3)
	v.con <- subset(verba, tolower(verba$v2)=="united kingdom" &  verba$pid5 > 3)
	v.lab <- subset(verba, tolower(verba$v2)=="united kingdom" &  verba$pid5 < 3)
	
	v.us <- subset(verba, tolower(verba$v2)=="united states"   & verba$pid5 !=3)
	v.uk <- subset(verba, tolower(verba$v2)=="united kingdom"  & verba$pid5 !=3)
	
	# Ten Nation
	t.rep <- subset(us10, us10$rd=='rep')
	t.dem <- subset(us10, us10$rd=='dem')
	t.con <- subset(uk10, uk10$cl=='con')
	t.lab <- subset(uk10, uk10$cl=='lab')
	
	t.us <- subset(us10, !is.na(us10$rd))
	t.uk <- subset(uk10, !is.na(uk10$cl))

## ************************************** ##
## Tab 4: Marriage	to Others		      ##
## ************************************** ##

	tab4 <- data.frame(var=1:17, rep=NA, dem=NA, rd.diff=NA, con=NA, lab=NA, cl.diff=NA, us.out=NA, uk.out=NA, us.uk.out=NA)
	tab4[1:17,1] <- c("Verba", "Pleased", "Indifferent", "Displeased",
					  "Rivers", "Not at all Upset", "Not Sure", "Somewhat Upset", "Very Upset", "Net Upset",
					  "10 Nat", "Very Happy", "Somewhat happy", "Neither happy nor unhappy", "Somewhat Unhappy",
					  "Very unhappy", "Net Proportion Unhappy")
	# Verba
	tab4[2:4,2:4]  <- prop.all(v.rep$marry.d, v.dem$marry.r)[c(3,2,1),]
	tab4[2:4,5:7]  <- prop.all(v.con$marry.d, v.lab$marry.r)[c(3,2,1),]
	tab4[2:4,8:10] <- prop.all(v.us$marry.out, v.uk$marry.out)[c(3,2,1),]
	
	# Rivers
	tab4[6:9,2:4]  <- prop.all(rep$marry.d, dem$marry.r, rep$weight, dem$weight)[c(3,4,2,1),]
	tab4[6:9,5:7]  <- prop.all(con$marry.d, lab$marry.r, con$weight, lab$weight)[c(3,4,2,1),]
	tab4[10,2:4]   <- prop.all(rep$marry.d < 3, dem$marry.r < 3, rep$weight, dem$weight)[2,]
	tab4[10,5:7]   <- prop.all(con$marry.d < 3, lab$marry.r < 3, con$weight, lab$weight)[2,]
	tab4[6:9,8:10] <- prop.all(r.us$marry.out, r.uk$marry.out, r.us$weight, r.uk$weight)[c(3,4,2,1),] 
	tab4[10,8:10]  <- prop.all(r.us$marry.out < 3, r.uk$marry.out < 3, r.us$weight, r.uk$weight)[2,] 
	
	# Ten Nation
	tab4[12:16,2:4]  <- prop.all(t.rep$marry.d, t.dem$marry.r, t.rep$weight, t.dem$weight)[c(5,4,3,2,1),]
	tab4[17,2:4]     <- prop.all(t.rep$marry.d < 3, t.dem$marry.r < 3, t.rep$weight, t.dem$weight)[2,]
	tab4[12:16,5:7]  <- prop.all(t.con$marry.d, t.lab$marry.r, t.con$weightuk, t.lab$weightuk)[c(5,4,3,2,1),]
	tab4[17,5:7]     <- prop.all(t.con$marry.d < 3, t.lab$marry.r < 3, t.con$weightuk, t.lab$weightuk)[2,]
	tab4[12:16,8:10] <- prop.all(t.us$marry.out, t.uk$marry.out, t.us$weight, t.uk$weightuk)[c(5,4,3,2,1),]
	tab4[17,8:10]    <- prop.all(t.us$marry.out < 3, t.uk$marry.out < 3, t.us$weight, t.uk$weightuk)[2,] 
	
	write.csv(tab4, file="polar/res/tab4.marr.csv")

## *************************************************** ##
## Tab 5: Comparison to Marriage to Other Religion	   ##
## *************************************************** ##

	tab5 <- data.frame(var=1:5, us.party=NA, us.rel=NA, us.diff=NA, uk.party=NA, uk.rel=NA, uk.diff=NA, diff.of.diff=NA)
	tab5[1:4,2:4] <- prop.all(r.us$marry.out, r.us$marry.rel, r.us$weight, r.us$weight)[c(3,4,2,1),]
	tab5[1:4,5:7] <- prop.all(r.uk$marry.out, r.uk$marry.rel, r.uk$weight, r.uk$weight)[c(3,4,2,1),]
	tab5[5,2:4]   <- prop.all(r.us$marry.out < 3, r.us$marry.rel < 3, r.us$weight, r.us$weight)[2,]
	tab5[5,5:7]   <- prop.all(r.uk$marry.out < 3, r.uk$marry.rel < 3, r.uk$weight, r.uk$weight)[2,]
	write.csv(tab5, file="polar/res/tab5.rel.marr.csv")

# Proportion who thought out.party worse than out.rel
	prop.all(r.us$marry.outr < r.us$marry.relr, r.us$marry.relr < r.us$marry.outr, r.us$weight, r.us$weight)
	prop.all(r.uk$marry.outr < r.uk$marry.relr, r.uk$marry.relr < r.uk$marry.outr, r.uk$weight, r.uk$weight)
	prop.all(r.us$marry.outr < r.us$marry.relr, r.uk$marry.outr < r.uk$marry.relr, r.us$weight, r.uk$weight)

## Verba/Rivers/10nat Plot			 
##***********************************##

## FOR OLD STYLE PLOT, SEE Polr.Marr.Misc.

	prop.err <- function(p, n){
		sqrt((p*(1-p)/n))
	}

	library(ggplot2)
	rs  <- array(c(.05, .27, .49, 1960, 2009, 2010), dim=c(3,2))
	ds  <- array(c(.04, .20, .32, 1960, 2009, 2010), dim=c(3,2))
	net <- rbind(rs, ds)
	net <- as.data.frame(net)
	colnames(net) <- c("mean", "Year")
	net$pid <- c(rep("Republicans",3), rep("Democrats", 3))
	net$n   <- c(261, 286, 256, 408, 370, 369)
	net$err <- prop.err(net$mean, net$n)
	net$Year <- as.factor(net$Year)
	net$Year <- rep(c("1960 - Proportion displeased", "2008 - Proportion somewhat or very upset", 
					"2010 - Proportion somewhat or very unhappy" ), 2)

# Split by Party (within year comparisons are clear)

	a <- ggplot(net, aes(x = pid, y = mean, fill = Year)) + xlab("") + 
			ylab("Proportion upset, dipleased, or unhappy\n if progeny married someone from another party") +
			scale_fill_discrete("") # remove legend title
			
	dodge <- position_dodge(width=0.9)
	a + geom_bar(stat = "identity", position="dodge") +	
			geom_linerange(aes(ymax=net[,1]+1.96*net$err, ymin=net[,1]-1.96*net$err), position=dodge)+
			theme_bw() + 
			opts(legend.position = c(.37, .85), legend.justification = "centre") 
	ggsave("polar/fig/verba.rivers.10nat2.png", dpi=300, width=7)
	
	# Presentation mode
	a <- ggplot(net, aes(x = pid, y = mean, fill = Year)) + xlab("") + 
			ylab("Proportion upset, dipleased, or unhappy\n if progeny married someone from another party") +
			scale_fill_discrete("") # remove legend title
	
	dodge <- position_dodge(width=0.9)
	a + geom_bar(stat = "identity", position="dodge") +	
			geom_linerange(aes(ymax=net[,1]+1.96*net$err, ymin=net[,1]-1.96*net$err), col="white", position=dodge)+
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
					legend.title = element_text(colour="white", size=10, face="bold"),
					legend.text = element_text(colour="white", size=10, face="bold"),
					legend.position = c(.37, .85), 
					legend.justification = "centre",
					legend.key = element_blank(),
					legend.background = element_rect(fill = "transparent", ,colour=NA, size=.5)) 
	ggsave("polarcamp/present/fig/marr.png", dpi=300, width=7)

	
	
	net$Year <- rep(c("1960 \n Proportion displeased", "2008 \n Proportion somewhat \n or very upset", 
					"2010 \n Proportion somewhat \n or very unhappy" ), 2)
	
	net$pid <- as.factor(net$pid)
	net$pid <- factor(net$pid,levels(net$pid)[c(2,1)])
	
	# Split by Year (across party comparisons are clear)
	a <- ggplot(net, aes(x = Year, y = mean, fill = pid)) + 
			xlab("") + 
			ylab("Proportion upset, dipleased, or unhappy\n if progeny married someone from another party") +
			scale_fill_discrete("") # remove legend title
	
	a + geom_bar(stat = "identity", position="dodge") + 
				geom_linerange(aes(ymax=net[,1]+1.96*net$err, ymin=net[,1]-1.96*net$err), position=dodge) +
				theme_bw() + 
				opts(legend.position = c(.25, .85), legend.justification = "centre") 
	ggsave("polar/fig/verba.rivers.10nat3.png", dpi=300, width=7)
	

# Joint US and UK and Party
# 2 Panel, Requested by Shanto
# ********************************************
	dodge <- position_dodge(width=0.9)
	rs  <- array(c(.05, .27, .49, 1960, 2009, 2010), dim=c(3,2))
	ds  <- array(c(.04, .20, .32, 1960, 2009, 2010), dim=c(3,2))
	net <- rbind(rs, ds)
	net <- as.data.frame(net)
	colnames(net) <- c("mean", "Year")
	net$pid <- c(rep("Republicans",3), rep("Democrats", 3))
	net$n   <- c(261, 286, 256, 408, 370, 369)
	net$err <- prop.err(net$mean, net$n)
	net$Year <- as.factor(net$Year)
	net$Year <- rep(c("1960 \n Proportion displeased", "2008 \n Proportion somewhat \n or very upset", 
					"2010 \n Proportion somewhat \n or very unhappy" ), 2)
	net$Country <- "U.S."
	
	cs  <- array(c(.12, .10, .22, 1960, 2009, 2010), dim=c(3,2))
	ls  <- array(c(.03, .19, .24, 1960, 2009, 2010), dim=c(3,2))
	
	net2 <- rbind(cs, ls)
	net2 <- as.data.frame(net2)
	colnames(net2) <- c("mean", "Year")
	net2$pid <- c(rep("Conservatives",3), rep("Labour", 3))
	net2$n   <- c(261, 286, 256, 408, 370, 369)
	net2$err <- prop.err(net2$mean, net2$n)
	net2$Year <- as.factor(net2$Year)
	net2$Year <- rep(c("1960 \n Proportion displeased", "2008 \n Proportion somewhat \n or very upset", 
					"2010 \n Proportion somewhat \n or very unhappy" ), 2)
	
	net2$Country <- "U.K."
	
	a <- ggplot(net2, aes(x = Year, y = mean, fill = pid)) + 
			xlab("") + 
			ylab("Proportion upset, dipleased, or unhappy\n if progeny married someone from another party") +
			scale_fill_discrete("") # remove legend title
	
	a + geom_bar(stat = "identity", position="dodge") + 
			geom_linerange(aes(ymax=net2[,1]+1.96*net2$err, ymin=net2[,1]-1.96*net2$err), position=dodge)+
			theme_bw() + 
			opts(legend.position = c(.25, .85), legend.justification = "centre") 
			
	net3 <- rbind(net, net2)
	net3$pid    <- factor(net3$pid, levels=c("Republicans", "Democrats", "Conservatives", "Labour"))
	net3$Country <- factor(net3$Country, levels =c("U.S.", "U.K."))
	
	# Not very elegant for works for this case: 
	# to get zeros out before decimal places

	fmt <- function(){
		f <- function(x) { a <- substr(as.character(sprintf("%1.1f", x)), 2,3); a[match(".0", a)] <- "0"; a }
			
			#substr(as.character(x), 2,3)
		f
	}
	
	a <- ggplot(net3, aes(x = net3$Year, y = net3$mean, fill = pid, shade=TRUE)) + facet_grid(Country ~ .) +
			xlab("")  + scale_y_continuous(labels=fmt())  +
			ylab("Proportion upset, dipleased, or unhappy\n if progeny married someone from another party") +
			scale_fill_discrete("") # remove legend title
	
	a + geom_bar(stat = "identity", position="dodge")  + 
			geom_linerange(aes(ymax=net3[,1]+1.96*net2$err, ymin=net3[,1]-1.96*net3$err), position=dodge) +
			theme_bw() + opts(legend.position = c(.25, .85), legend.justification = "right") 
	
	ggsave("polar/fig/verba.rivers.joint.color.png",  dpi=400, width=7)
	ggsave("polar/fig/verba.rivers.joint.color.tiff", dpi=400, width=7)
	
	# Present
	,
	a + geom_bar(stat = "identity", position="dodge")  + 
			geom_linerange(aes(ymax=net3[,1]+1.96*net2$err, ymin=net3[,1]-1.96*net3$err), color="white", position=dodge) +
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
					axis.line = element_line(colour = "gray30"),
					legend.title = element_text(colour="white", size=10, face="bold"),
					legend.text = element_text(colour="white", size=10, face="bold"),
					legend.position = c(.17, .85), 
					legend.justification = "centre",
					legend.key = element_blank(),
					legend.background = element_rect(fill = "transparent", ,colour=NA, size=.5),
					legend.position = c(.05, .85), legend.justification = "right") 
	ggsave("in.n.out/present/figs/verba.rivers.joint.color.png",  dpi=400, width=7)
	
	
	a + geom_bar(stat = "identity", position="dodge")  + 
			geom_linerange(aes(ymax=net3[,1]+1.96*net2$err, ymin=net3[,1]-1.96*net3$err), position=dodge) +
			theme_bw() + opts(legend.position = c(.25, .85), legend.justification = "right") + 
			scale_fill_grey(name="")
	
	ggsave("polar/fig/verba.rivers.joint.bw.png",  dpi=400, width=7)
	ggsave("polar/fig/verba.rivers.joint.bw.tiff", dpi=400, width=7)
	
	# Alternate b/w plot (at Yph's Request)
	#png("polar/fig/verba.rivers.joint.bw2.png", res='150', width=1650, height=1200)
	tiff(filename = "polar/fig/verba.rivers.joint.bw2.tif", res='150', width=1650, height=1200)
	par(oma=c(.9,0,0,0), mar=c(4.5,5.7,3.2,2.1))
	barplot(xtabs(net3$mean ~ net3$pid + net3$Year), beside=T, angle=c(0, 0, 45, 0), density=c(100, 100, 20, 0), 
			col=c("#000000", "#bbbbbb", "#333333", "#ffffff"), ylim=c(0, .55), axes=F, axisnames=F, cex.axis=.7)
	axis(1, at=c(3, 8, 13), lab=unique(net3$Year), cex=.5, padj=.6)
	axis(2, at=seq(0,.6, .1))
	legend(.5, .55, bty='n', unique(net3$pid), angle=c(0, 0, 45, 0), density=c(100, 100, 20, 0),  fill = c("#000000", "#bbbbbb", "#333333", "#ffffff"))
	box()
	title(ylab=list("Proportion upset, dipleased, or unhappy\n if progeny married someone from another party" , cex=1.2))
	dev.off()
	
## Mix US and UK - Avg. Over R and D (Verba/Rivers/10nat Plot)			 
##************************************************************##

	p60 <- array(c(.04, .07, "U.S.", "U.K."), dim=c(2,2))
	p09 <- array(c(.24, .15, "U.S.", "U.K."), dim=c(2,2))
	p10 <- array(c(.39, .24, "U.S.", "U.K."), dim=c(2,2))
	net <- rbind(p60, p09, p10)
	net <- as.data.frame(net)
	names(net) <- c("Mean", "Country")
	net$n   <- c(669, 759, 656, 817, 625, 616)
	net$err <- prop.err(as.numeric(as.character(net$Mean)), as.numeric(as.character(net$n)))
	net$Year <- c("1960", "1960", "2008", "2008", "2010", "2010")
	net$Mean <- as.numeric(as.character(net$Mean))
	net$Country <- factor(net$Country,levels(net$Country)[c(2,1)])
	net$Year <- c(rep("1960 \n Proportion upset",2), rep("2008 \n Proportion somewhat \n or very upset", 2),
					rep("2010 \n Proportion somewhat \n or very unhappy" , 2))
	
	a <- ggplot(net, aes(x = Year, y = Mean, fill = Country)) + 
			xlab("") + 
			ylab("Proportion upset, dipleased, or unhappy\n if progeny married someone from another party") +
			scale_fill_discrete("") # remove legend title
	
	a + geom_bar(stat = "identity", position="dodge") + 
			geom_linerange(aes(ymax=net[,1]+1.96*net$err, ymin=net[,1]-1.96*net$err), position=dodge) +
			theme_bw() + 
			opts(legend.position = c(.25, .85), legend.justification = "centre") 
	ggsave("polar/fig/usuk.marr.10nat2.png", dpi=300, width=7)

## Proportion Happier Marrying someone from another religion in US and UK
##*************************************************************************##

	prop.all(r.us$marry.outr < r.us$marry.relr, r.uk$marry.outr < r.uk$marry.relr, r.us$weight, r.uk$weight)
	
	rs <- array(c(.12, .08, 1, 2), dim=c(2,1))
	
png("polar/fig/rivers.rel.marr.png", width=2850, height=2450, res='300')
	par(oma=c(.9,0,0,0), mar=c(4,5.5,3,2))
	mp <- barplot(rs, beside=T, axisnames = FALSE, ylim=c(0,.15), density=c(100,60), 
			col=c("#eebbbb", "#eebbbb"), axes=F)
	#main="Proportion upset or displeased if son or daughter married\n someone from another party in 1960 and 2010")
	axis(2, at=seq(0, .55, .05),labels=kros(seq(0, .55, .05)))
	mtext("Proportion more upset if progeny married \n someone from another party than another religion", side=2, 
			cex=1.2, cex.lab=1, line=3)
	# Add the individual bar labels
	mtext(1, at = mp, text = c("US", "UK"), line = 0, cex = 1)
	box()
	shortname <- "Source: 2009: YouGov/Polimetrix"
	mtext(shortname, cex=.95, line=0, side=SOUTH<-1, adj=.1, outer=TRUE)
dev.off()
