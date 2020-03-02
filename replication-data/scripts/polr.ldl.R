##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--++
##      Polarization
##      NES Like-Dislike of Parties, text, and 0-10
##		Last Edited: 3/08/11  	         
##   	Gaurav Sood							    	 ## 
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--++

# Set Working dir.
	setwd("C:/Users/gsood/Dropbox/")

## Load Functions
	source("func/func.R")
	source("func/chai.R")

## LOAD DATA 
	load("polar/data/polar.Rdata")

## Function to generate weighted means by Dem/Rep by Year
lmcap <- function(data, grp, x,y,wts) 
{	
	#tester: x <- "th.l";  y <- "th.c"; wts <- "vcf0009a"
	#tester: x <- "libtemp";  y <- "contemp"; wts <- "wtssall"
	res <- matrix(nrow=25, ncol=8) 
	j <- 1
	for(i in unique(data[,grp]))	#polar$surveyyr; gssp$year				# For each survey year
	{
		yr <- subset(polar, (data[,grp]==i))						# Subset of an year
		#yr <- subset(gssp, gssp$year==i)
		var  <- eval(parse(text=paste("yr$",x, sep="")))
		if((sum(abs(var), na.rm=T) > 0)) 		# If var. is there for the year
		#& (sum(abs(year$con), na.rm=T) > 0) 							# for lib/con subsets; as libcon only 1972 on
		{
				year.d <- subset(yr, (yr$dem==1), select=c(x, y, wts))
				year.r <- subset(yr, (yr$rep==1), select=c(x, y, wts))
				#year.d <- subset(yr, (yr$lib==1), select=c(x, y, wts))
				#year.r <- subset(yr, (yr$con==1), select=c(x, y, wts))
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

# Like Dislike
####################
like    <- lmcap("dem.gl",  "rep.gl", "vcf0009a") 
dlike   <- lmcap("dem.gdl", "rep.gdl", "vcf0009a")

aff    <- lmcap("vcf0316", "vcf0320", "vcf0009a") 
n.aff  <- lmcap("vcf0322", "vcf0322", "vcf0009a")
nuff <- as.data.frame(n.aff)
nuff$t <- n.aff[,2] + abs(n.aff[,4])
n.aff2 <- lmcap("naff", "naff", "vcf0009a") #All with negative affect coded -1, positive +1, same 0

## *************************************************##
## Fig 3a: Like/Dislike Party; 0 to 10 scale		##
## *************************************************##
## Only 1996, 2004, 2008; Post; part of CSES

load("polar/data/nes96p.rdata")
load("polar/data/nes04p.rdata")
load("polar/data/nes08p.rdata")

res <- data.frame(yr=NA, dd=NA, dr=NA, d.dr=NA, rr=NA, rd=NA, r.rd=NA, in.like=NA, out.like=NA, inout=NA) 
res[1,] <- c(1996, with(subset(nes96p, nes96p$dem==1), wt.all(d.like, r.like, weight, weight)), with(subset(nes96p, nes96p$rep==1), wt.all(r.like, d.like, weight, weight)), with(nes96p[!is.na(nes96p$rd),], wt.all(in.like, out.like, weight, weight)))
res[2,] <- c(2004, with(subset(nes04p, nes04p$dem==1), wt.all(d.like, r.like, weight, weight)), with(subset(nes04p, nes04p$rep==1), wt.all(r.like, d.like, weight, weight)), with(nes04p[!is.na(nes04p$rd),], wt.all(in.like, out.like, weight, weight)))
res[3,] <- c(2008, with(subset(nes08p, nes08p$dem==1), wt.all(d.like, r.like, weight, weight)), with(subset(nes08p, nes08p$rep==1), wt.all(r.like, d.like, weight, weight)), with(nes08p[!is.na(nes08p$rd),], wt.all(in.like, out.like, weight, weight)))

write.csv(res, file="polar/res/like.parties.csv")

res2 <- data.frame(year=c(1996, 2004, 2008), mean=NA)
res2[1,2] <- weighted.mean(nes96p$diff.like[!is.na(nes96p$rd)], nes96p$weight[!is.na(nes96p$rd)], na.rm=T)
res2[2,2] <- weighted.mean(nes04p$diff.like[!is.na(nes04p$rd)], nes04p$weight[!is.na(nes04p$rd)],na.rm=T)
res2[3,2] <- weighted.mean(nes08p$diff.like[!is.na(nes08p$rd)], nes08p$weight[!is.na(nes08p$rd)],na.rm=T)

png("polar/fig/like1to10.png", res=200, width=1400, height=1250)
par(oma=c(.9,0,0,0), mar=c(5.5,5.2,3.2,2.1))
plot(res2, type="b",  col='#55555577', cex=1.1, lwd=2, ylim=c(.35, .55), axes=F, xlab="Year", 
		ylab="Like Own Party Minus Like Out Party")
points(res[,c(1,6)], pch=16, col= '#5555bb77', type="b")
points(res[,c(1,7)], pch=16, col= '#bb555577', type="b")
axis(1, at=c(1996, 2004, 2008), lab=c(1996, 2004, 2008), cex=1.1)
axis(2, at=c(.35, .40, .45, .50, .55), lab=c(".35", ".40", ".45", ".50", ".55"), las=1, cex=1.1)
box()
legend(1996, .55, col=c("#5555bb77", "#bb555577", "#55555577"), pch=21, lwd=2, cex=1, 
		c("Democrats", "Republicans", "Combined"))
shortname <- "Source: ANES. Measure: 'Strongly Dislike to Strongly Like' Party. Sample and measure limited to Republicans and Democrats. \nTime:"
mtext(paste(shortname, " ", format(Sys.time(), "%Y-%m-%d %H:%M")), cex=0.65, line=0, side=SOUTH<-1, adj=0, outer=TRUE)
dev.off()

## Fig 3b: Own Party Positive; 0 to 10 scale		##
## *********************************************##
# Message - People still like their parties

png("polar/fig/pos.party.1to10.png", res=200, width=1400, height=1250)
par(oma=c(.9,0,0,0), mar=c(5.5,5.2,3.2,2.1))
plot(res[,c(1,2)], type="b",  col='white', cex=1.1, lwd=2, ylim=c(.70, .87), axes=F, xlab="Year", 
		ylab="Like Own Party")
points(res[,c(1,2)], pch=16, col= '#5555bbdd', type="b")
points(res[,c(1,4)], pch=16, col= '#bb5555dd', type="b")
axis(1, at=c(1996, 2004, 2008), lab=c(1996, 2004, 2008), cex=1.1)
axis(2, at=seq(from=.70, to=.85, by=.05), lab=c(".70", ".75", ".80", ".85"), las=1, cex=1.1)
box()
legend(1996, .87, col=c("#5555bbdd", "#bb5555dd"), pch=21, lwd=2, cex=1, 
		c("Democrats", "Republicans"))
shortname <- "Source: ANES. Measure: 'Strongly Dislike to Strongly Like' Party. Sample and measure limited to Republicans and Democrats. \nTime:"
mtext(paste(shortname, " ", format(Sys.time(), "%Y-%m-%d %H:%M")), cex=0.65, line=0, side=SOUTH<-1, adj=0, outer=TRUE)
dev.off()

## *********************************************##
## Like Dislike Measures						##
## Fig 4a: Net Affect by Party 					##
## *********************************************##

png("polar/fig/l.dl.png", res=200, width=1400, height=1200)
par(oma=c(.9,0,0,0), mar=c(4.5,5.2,3.2,2.1))
plot(nuff[,c(1,2)], main=list("Net Affect towards Parties", cex=1.1), 
		xlab="Year", ylab="Net Affect towards Parties:\n (D Likes + R Dislikes) - (D Dislikes + R Likes)", 
		 type="p", ylim=c(-3,2.7), xlim=(range(n.aff[,1], na.rm=T)), pch=21, col="#5555cc")
lines(loess.smooth(n.aff[,1], abs(n.aff[,2])), lwd=3, col='#5555cc55')
points(n.aff[,1], n.aff[,5], type="p", pch=21, cex=1, col="#cc5555")
lines(loess.smooth(n.aff[,1], n.aff[,5]), lwd=3, col='#cc555555')
legend(1951, 0, c("Democrats", "Republicans"), fill=c("#5555cc", "#cc5555"), cex=.9)
dev.off()

## Fig 4b: Diff. in Net Affect across parties   ##
## *********************************************##
png("polar/fig/diff.l.dl.png", res=200, width=1400, height=1200)
par(oma=c(.9,0,0,0), mar=c(4.5,6.2,3.2,2.1))
scatter.smooth(nuff[,1], nuff[,9], main=list("Difference of net affect towards parties\n between Democrats and Republicans", cex=1.1), 
		xlab="Year", ylab="Difference between Democrats and Republicans\n in Net Affect towards Parties :\n (D Likes + R Dislikes) - (D Dislikes + R Likes)", 
		type="p", ylim=c(.8, 2.5), xlim=(range(n.aff[,1], na.rm=T)), pch=21, col="#555555")
dev.off()

# summary(glm(polar$dem.dl ~ polar$surveyyr*polar$rd + polar$polint + polar$presyear + polar$int_elections, family="poisson"))

