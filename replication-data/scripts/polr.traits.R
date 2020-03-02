##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--++
##      Polarization
##      Trait Ratings
##		Last Edited: 5/22/11 	         
##   	Gaurav Sood							    	 ## 
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--++

# Set Working dir.
#	setwd("C:/Users/gsood/Dropbox/")

## Load Functions
	source("replication-data/scripts/func.R")
	source("replication-data/scriptspolr.func.R")
	
## LOAD DATA 
	load("polar/data/rivers.rdata")
	load("polar/data/verba.rdata")
	vars <- read.csv("polar/data/vars.csv", header=T)

## Initializing some more data
	vars.1 <- c("Generous","Honest","Patriotic","Mean","Hypocritical","Intelligent","Selfish",
			"Open-minded","Close-minded","None", "Mean No. Positives", "Mean No. Negatives", "Mean No. Pos.- Neg.", 
			"Marry", "Marry Oth. Rlgn.")

# Some subsets
	# Rivers
		rep <- subset(rivers, rivers$pid3==2) 		   ##rivers$pid3==2 (Republican)
		dem <- subset(rivers, rivers$pid3==1)  		   ##rivers$pid3==1 (Democrat)
		con <- subset(rivers, rivers$ukpartyid==1) 	   ##rivers$ukpartyid==1 (Conservatives)
		lab <- subset(rivers, rivers$ukpartyid==2)     ##rivers$ukpartyid==2 (Labor)
		
		r.us <- subset(rivers, rivers$source==1 & rivers$pid3 %in% c(1,2)) 
		r.uk <- subset(rivers, rivers$source==2 & rivers$ukpartyid %in% c(1,2)) 
	
	# Verba
		v.rep <- subset(verba, tolower(verba$v2)=="united states" &  verba$pid5 > 3)
		v.dem <- subset(verba, tolower(verba$v2)=="united states" &  verba$pid5 < 3)
		v.con <- subset(verba, tolower(verba$v2)=="united kingdom" & verba$pid5 > 3)
		v.lab <- subset(verba, tolower(verba$v2)=="united kingdom" &  verba$pid5 < 3)
		
		v.us <- subset(verba, tolower(verba$v2)=="united states"  &  verba$pid5 !=3)
		v.uk <- subset(verba, tolower(verba$v2)=="united kingdom"  & verba$pid5 !=3)

## *****************************  ##
## Tab 1: Trait Ratings			  ##
## ****************************** ##

res <- data.frame(var=1:23, rr=NA, rd=NA, rrd=NA, dd=NA, dr=NA, ddr=NA, cc=NA, cl=NA, ccl=NA, ll=NA, lc=NA, 
		llc=NA, us.in=NA, us.out=NA, us.in.out=NA, uk.in=NA, uk.out=NA, uk.in.out=NA, us.uk.in=NA, us.uk.out=NA, us.uk.in.out=NA)
colnames(res) <- c("Variables","R rate R", "R rate D","R R.min.D", "D rate D","D rate R", "D D.min.R", "C rate C", 
		"C rate L", "C C.min.L", "L rate L", "L rate C", "L L.min.C", "US In","US Out", "US (In - Out)", "UK In",
		"UK Out", "UK In - Out", "US - UK In", "US - UK Out", "US - UK (In - Out)")

for(j in c(1:10)){
	res[j,1] <- vars.1[j]
	r <- ifelse(j < 11, j + 11, 0)
	d <- ifelse(j < 11, j + 21, 0)
	i <- ifelse(j < 11, j + 300, 0)
	o <- ifelse(j < 11, j + 310, j+1)
	
	res[j,2:4]   <- prop.all(rep[,r], rep[,d], rep$weight, rep$weight)[2,]
	res[j,5:7]   <- prop.all(dem[,d], dem[,r], dem$weight, dem$weight)[2,]
	res[j,8:10]  <- prop.all(con[,r], con[,d], con$weight, con$weight)[2,]
	res[j,11:13] <- prop.all(lab[,d], lab[,r], lab$weight, lab$weight)[2,]
	res[j,14:16] <- prop.all(r.us[,i], r.us[,o], r.us$weight, r.us$weight)[2,]
	res[j,17:19] <- prop.all(r.uk[,i], r.uk[,o], r.uk$weight, r.uk$weight)[2,]
	res[j, 20]   <- prop.all(r.us[,i], r.uk[,i], r.us$weight, r.uk$weight)[1,3]
	res[j, 21]   <- prop.all(r.us[,o], r.uk[,o], r.us$weight, r.uk$weight)[1,3]
	lm7       	 <- summary(lm(I(c(r.us[,i] - r.us[,o], r.uk[,i] - r.uk[,o]))~ c(rep(1, nrow(r.us)), rep(0, nrow(r.uk))), weight=c(r.us$weight, r.uk$weight)))
	res[j,22] 	 <- paste(kros(lm7$coeff[2,1]), stars(lm7$coeff[2,4]), sep="")
	#b    <- boot2(c(rep[,r] - rep[,d]), c(con[,r] - con[,d]), 
	#		rep$weight[!is.na(rep[,r]) & !is.na(rep[,d])], con$weight[!is.na(con[,r]) & !is.na(con[,d])])
}

k <- 11
for(j in c(11,12,13)){
	res[j,1] <- vars.1[j]
	
	r <- ifelse(j < 14, k + 284, 0)
	d <- ifelse(j < 14, k + 285,  0)
	i <- ifelse(j < 14, k + 310, 0)
	o <- ifelse(j < 14, k + 311, j+1)
	k <- k +2
	res[j,2:4]   <- wt.all(rep[,r], rep[,d], rep$weight, rep$weight)
	res[j,5:7]   <- wt.all(dem[,d], dem[,r], dem$weight, dem$weight)
	res[j,8:10]  <- wt.all(con[,r], con[,d], con$weight, con$weight)
	res[j,11:13] <- wt.all(lab[,d], lab[,r], lab$weight, lab$weight)
	res[j,14:16] <- wt.all(r.us[,i], r.us[,o], r.us$weight, r.us$weight)
	res[j,17:19] <- wt.all(r.uk[,i], r.uk[,o], r.uk$weight, r.uk$weight)
	res[j, 20]   <- wt.all(r.us[,i], r.uk[,i], r.us$weight, r.uk$weight)[3]
	res[j, 21]   <- wt.all(r.us[,o], r.uk[,o], r.us$weight, r.uk$weight)[3]
	lm7       	 <- summary(lm(I(c(r.us[,i] - r.us[,o], r.uk[,i] - r.uk[,o]))~ 
							c(rep(1, nrow(r.us)), rep(0, nrow(r.uk))), weight=c(r.us$weight, r.uk$weight)))
	res[j,22] 	 <- paste(kros(lm7$coeff[2,1]), stars(lm7$coeff[2,4]), sep="")
}
		
## Verba Traits
#################
res[14:21,1] <- c("Verba", "Intelligent","Welfare of Humanity", "Independence", "Selfish","Ignorant","Betrayers","Fascist")

vary <- c("r.intelligent", "d.intelligent", "in.intelligent", "out.intelligent", 
		"r.welfare",	 "d.welfare", 	"in.welfare", "out.welfare", 
		"r.independence","d.independence",	"in.independence", "out.independence", 
		"r.selfish",	 "d.selfish",		"in.selfish",		"out.selfish", 
		"r.ignorant",	 "d.ignorant",		"in.ignorant",		"out.ignorant",
		"r.betrayer",	 "d.betrayer",		"in.betrayer",		"out.betrayer", 
		"r.fascist",	 "d.fascist",		"in.fascist", 		"out.fascist")
j <- 15
for(i in seq(1,28,4)){
	res[j,2:4]   <- prop.all(v.rep[,vary[i]], 	v.rep[,vary[i+1]],	v.rep$weight, v.rep$weight)[2,]
	res[j,5:7]   <- prop.all(v.dem[,vary[i+1]], v.dem[,vary[i]], 	v.dem$weight, v.dem$weight)[2,]
	res[j,8:10]  <- prop.all(v.con[,vary[i]], 	v.con[,vary[i+1]],	v.con$weight, v.con$weight)[2,]
	res[j,11:13] <- prop.all(v.lab[,vary[i+1]], v.lab[,vary[i]],	v.lab$weight, v.lab$weight)[2,]
	res[j,14:16] <- prop.all(v.us[,vary[i+2]], 	v.us[,vary[i+3]], 	v.us$weight, v.us$weight)[2,]
	res[j,17:19] <- prop.all(v.uk[,vary[i+2]], 	v.uk[,vary[i+3]], 	v.uk$weight, v.uk$weight)[2,]
	res[j, 20]   <- prop.all(v.us[,vary[i+2]], 	v.uk[,vary[i+2]], 	v.us$weight, v.uk$weight)[1,3]
	res[j, 21]   <- prop.all(v.us[,vary[i+3]], 	v.uk[,vary[i+3]], 	v.us$weight, v.uk$weight)[1,3]
	lm7       <- summary(lm(I(c(v.us[,vary[i+2]] - v.us[,vary[i+3]], v.uk[,vary[i+2]] - v.uk[,vary[i+3]]))~ 
							c(rep(1, nrow(v.us)), rep(0, nrow(v.uk))), weight=c(v.us$weight, v.uk$weight)))
	res[j,22] <- paste(kros(lm7$coeff[2,1]), stars(lm7$coeff[2,4]), sep="")
	j <- j + 1
}

for(j in c(22,23)){
	res[22:23,1] <- c("Positive", "Negative")
	r <- ifelse(j ==22, "r.pos", "r.neg")
	d <- ifelse(j ==22, "d.pos", "d.neg")
	i <- ifelse(j ==22, "in.pos", "in.neg")
	o <- ifelse(j ==22, "out.pos","out.neg")
	res[j,2:4]   <- wt.all(v.rep[,r], v.rep[,d], v.rep$weight, v.rep$weight)
	res[j,5:7]   <- wt.all(v.dem[,d], v.dem[,r], v.dem$weight, v.dem$weight)
	res[j,8:10]  <- wt.all(v.con[,r], v.con[,d], v.con$weight, v.con$weight)
	res[j,11:13] <- wt.all(v.lab[,d], v.lab[,r], v.lab$weight, v.lab$weight)
	res[j,14:16] <- wt.all(v.us[,i], v.us[,o], v.us$weight, v.us$weight)
	res[j,17:19] <- wt.all(v.uk[,i], v.uk[,o], v.uk$weight, v.uk$weight)
	res[j, 20]   <- wt.all(v.us[,i], v.uk[,i], v.us$weight, v.uk$weight)[3]
	res[j, 21]   <- wt.all(v.us[,o], v.uk[,o], v.us$weight, v.uk$weight)[3]
	lm7       	 <- summary(lm(I(c(v.us[,i] - v.us[,o], v.uk[,i] - v.uk[,o]))~ 
							c(rep(1, nrow(v.us)), rep(0, nrow(v.uk))), weight=c(v.us$weight, v.uk$weight)))
	res[j,22] 	 <- paste(kros(lm7$coeff[2,1]), stars(lm7$coeff[2,4]), sep="")
}

# Reorder Rows
res[1:13,] <- res[c("1", "2", "3", "6", "8", "4", "5", "7", "9", "10", "11", "12", "13"),]
write.csv(res, file="polar/res/tab1.trait.csv", row.names=F)

## **********************************************  ##
## Tab 2: Strength of Party ID/Trait Ratings	   ##
## ****************************** ***************  ##
tab2 <- data.frame(var=1:13, rr=NA, rd=NA, rrd=NA, dd=NA, dr=NA, ddr=NA)
colnames(tab2) <- c("Variables","R rate R", "R rate D","R R.min.D", "D rate D","D rate R", "D D.min.R")
vars.1 <- c("Generous","Honest","Patriotic","Mean","Hypocritical","Intelligent","Selfish",
		"Open-minded","Close-minded","None", "Mean No. Positives", "Mean No. Negatives", "Mean No. Pos.- Neg.")
#strong R, sum(rivers$pid7==7, na.rm=T), 5,6
#strong D, 1, and then 2,3
for(i in 1:10){
	tab2[i,1] <- vars.1[i]
	tab2[i,2] <- lmc2(rep[,i+11]==1, rep$strpid==1, rep$weight)
	tab2[i,3] <- lmc2(rep[,i+21]==1, rep$strpid==1, rep$weight)
	tab2[i,4] <- lmc2((rep[,i+11]==1) - rep[,i+21]==1, rep$strpid==1, rep$weight)
	tab2[i,5] <- lmc2(dem[,i+21]==1, dem$strpid==1, dem$weight)
	tab2[i,6] <- lmc2(dem[,i+11]==1, dem$strpid==1, dem$weight)
	tab2[i,7] <- lmc2((dem[,i+21]==1) - dem[,i+11]==1, dem$strpid==1, dem$weight)
}
j <- 11
for(i in seq(295,300,2)){
	tab2[j,1] <- vars.1[j]
	tab2[j,2] <- lmc2(rep[,i], rep$strpid==1, rep$weight)
	tab2[j,3] <- lmc2(rep[,(i+1)], rep$strpid==1, rep$weight)
	tab2[j,4] <- lmc2(rep[,i] - rep[,(i+1)], rep$strpid==1, rep$weight)
	tab2[j,5] <- lmc2(dem[,(i+1)], dem$strpid==1, dem$weight)
	tab2[j,6] <- lmc2(dem[,i], dem$strpid==1, dem$weight)
	tab2[j,7] <- lmc2(dem[,i] - dem[,(i+1)] , dem$strpid==1, dem$weight)
	j <- j +1
}
tab2[1:13,] <- tab2[c("1", "2", "3", "6", "8", "4", "5", "7", "9", "10", "11", "12", "13"),]
write.csv(tab2, file="polar/res/tab2.strpid.traits.csv")

## Latent Trait Model
###########################

library(ltm)
# r good, d bad
dm <- with(rivers, data.frame(cbind(r.generous, r.honest, r.patriotic, r.intelligent, 
						r.openmind, d.mean, d.hypo, d.selfish, d.closemind, d.generous, d.honest, d.patriotic, 
						d.intelligent, d.openmind, r.mean, r.hypo, r.selfish, r.closemind), partid))
b <- lapply(dm[,1:18], as.numeric)
trait <- matrix(unlist(b), nrow=dim(dm)[1], ncol=length(b), byrow=FALSE)
dimnames(trait) <- list(NULL,names(dm[,1:18]))

table(dm[,1],trait[,1])
lapply(dm[,1:18],table,exclude=NULL)

library(polycor)
m <- dim(trait)[2]
r <- diag(rep(1,m))

for(i in 1:m){
	for(j in 1:m){
		if(i<j){
			r[i,j] <- polychor(tapply(rivers$weight, list(trait[,i],trait[,j]),sum), ML=TRUE)
			r[j,i] <- r[i,j]
			}
		}
}

dimnames(r) <- list(names(dm),names(dm))
round(r,2)

v <- eigen(r)$values
v

rmse <- function(object){
	lambda <- unclass(object$loadings)
	psi <- diag(object$uniquenesses)
	e <- r - (lambda%*%t(lambda) + psi)
	elt <- matrixcalc::lower.triangle(e)
	diag(elt) <- NA
	elt[elt==0] <- NA
	sqrt(mean(elt^2,na.rm=TRUE))
}
f1 <- factanal(covmat=r,n.obs=dim(trait)[1],factors=1)
f1

f2 <- factanal(covmat=r,n.obs=dim(trait)[1],factors=2)
f2

cosmoRate <- unlist(lapply(apply(trait,2,wtab),function(x)x["1","%"]))
cosmoRate[order(cosmoRate)]
cosmoRateIncome <- apply(trait,2, 
		function(x){
			wtab(x,rivers$partid,margin=1)["1",]
			})
wtab(trait[,1], rivers$partid, margin=1)



## Strength of Party ID Plot
##***************************************

plotData <- data.frame(y=as.vector(cosmoRateIncome),
			inc=rep(dimnames(cosmoRateIncome)[[1]],
			dim(cosmoRateIncome)[2]),
		j=rep(dimnames(cosmoRateIncome)[[2]],
		each=dim(cosmoRateIncome)[1]))

plotData$inc <- factor(plotData$inc,
				levels=dimnames(cosmoRateIncome)[[1]],
				labels=dimnames(cosmoRateIncome)[[1]])

plotData$j <- factor(plotData$j,
				levels=dimnames(cosmoRateIncome)[[2]],
				labels=dimnames(cosmoRateIncome)[[2]])

stripFunction <- function(which.given, which.panel, factor.levels,...){
	theOne <- match(factor.levels[which.panel[which.given]],
	names(cosmoRate))
	panel.rect(0,0,1,1,col="#ffe5cc",border=1)
	panel.text(x=.5,y=.5,
	cex=.8,
	col="black",
	paste(factor.levels[which.panel[which.given]],
	": Overall Rate ",
	format(cosmoRate[theOne],
	nsmall=1),
	"%",
	sep=""))
	}

library(lattice)
#pdf(file="incidenceByIncome.pdf")
xyplot(y ~ inc | j,
data=plotData,
layout=c(2,4),
panel=function(x,y,...){
panel.abline(h=c(20,40,60,80),col="blue",alpha=.33,lwd=.15)
panel.abline(v=1:7,col="blue",lwd=.15,alpha=.33)
panel.points(x,y,pch=16,col="black",cex=1,...)
},
xlab="Income",
ylab="",
index.cond=list(order(cosmoRate)),
strip=stripFunction,
scales=list(x=list(alternating=3,cex=.6)),
subset=!(inc %in% c("Skipped","Not Asked")))

dev.off()








# good out, bad in =0; bad out, good in=1

dm <- with(subset(rivers, !is.na(rivers$partid)), data.frame(cbind(out.generous, out.honest, out.patriotic, out.intelligent, out.openmind,
					  					in.mean, in.hypo, in.selfish, in.closemind)==0, 
							  cbind(in.generous, in.honest, in.patriotic, in.intelligent, in.openmind, 
										out.mean, out.hypo, out.selfish, out.closemind)==1, partid))

descript(dm[, 1:18])

# Mean
dm$simple <- rowMeans(dm[,1:18])

fit <- ltm(dm[,1:18] ~ z1 + z2)
fsc <- factor.scores(fit)

a <- as.data.frame(data.matrix(dm[,1:18]))
dm$join <- do.call(paste, c(a, sep = ""))

newdf <- fsc$score.dat
newdf$join <- do.call(paste, c(newdf[,1:18], sep = ""))
c <- merge(dm, newdf, by='join', all.x=T, all.y=T)
d <- c[order(c$z1),]

cor(c$z1, c$simple, use="na.or.complete")

### Plotting
#####################
pdf("polar/fig/irt.dist.trait.pdf")
par(oma=c(.9,0,0,0), mar=c(4.5,5.2,3.2,2.1))
plot(seq(1, length(d$z1))~ d$z1, col="white", ylim=c(0,370), xlab="Latent Affect", ylab="", yaxt='n', 
		main="Affective Polarization?: Affect towards Partisans, 
				as measured by trait ratings")
points(seq(1, sum(d$partid==1)) ~ d$z1[d$partid==1], col="light blue")
points(seq(1, sum(d$partid==2)) ~ d$z1[d$partid==2], col="pink")
points(seq(1, sum(d$pid3 > 2))  ~ d$z1[d$partid > 2], col="grey")
legend(-2, 370, col=c("pink", "light blue", "grey"), c("Republicans", "Democrats", "Independents"), pch=1, cex = .75)
shortname <- "Source: YouGov/Polimetrix. Measure: 18 Trait Ratings of Republicans and Democrats. Time:"
mtext(paste(shortname, " ", format(Sys.time(), "%Y-%m-%d %H:%M")), cex=0.65, line=0, side=SOUTH<-1, adj=0, outer=TRUE)
dev.off()

pdf("polar/fig/irt.box.trait.pdf",width=6.5,height=5)
par(oma=c(.9,0,0,0), mar=c(4.5,5.2,3.2,2.1))
boxplot(d$z1[!is.na(d$partid)] ~ (d$partid)[!is.na(d$partid)], col=c("light blue", "pink"), 
			main="Affective Polarization?: Affect towards Partisans, as measured by trait ratings,\n by Partisan self-identification",
			xlab="Party ID", ylab="Latent Affect", border=c("#000077", "#770000"))
shortname <- "Source: YouGov/Polimetrix. Measure: 18 Trait Ratings of Republicans and Democrats. Time:"
mtext(paste(shortname, " ", format(Sys.time(), "%Y-%m-%d %H:%M")), cex=0.65, line=0, side=SOUTH<-1, adj=0, outer=TRUE)
dev.off()

pdf("polar/fig/irt.density.trait.pdf")
par(oma=c(.9,0,0,0), mar=c(4.5,5.2,3.2,2.1))
plot(density(c$z1[!is.na(c$pid) & c$pid=="Republicans"]), col="red", ylim=c(0,.85), xlim=c(-2.5,2.5),
		main="Affective Polarization?: Affect towards Partisans, as measured \n by trait ratings, by Partisan self-identification", xlab="Latent Affect")
lines(density(c$z1[!is.na(c$pid) & c$pid=="Democrats"]), col="blue")
lines(density(d$z1[is.na(c$pid)]), col="black")
legend(-2.3, .8, col=c("red", "blue", "black"), c("Republicans", "Democrats", "Independents"), pch=1, cex = .75)
shortname <- "Source: YouGov/Polimetrix. Measure: 18 Trait Ratings of Republicans and Democrats. Time:"
mtext(paste(shortname, " ", format(Sys.time(), "%Y-%m-%d %H:%M")), cex=0.65, line=0, side=SOUTH<-1, adj=0, outer=TRUE)
dev.off()

pdf("polar/fig/icc.trait.pdf",width=5.5,height=5)
plot(fit3)
dev.off()

# Using all data						##
##########################################

dm <- with(rivers,      data.frame(cbind(r.generous, r.honest, r.patriotic, r.intelligent, 
						r.openmind, d.mean, d.hypo, d.selfish, d.closemind)==0, cbind(d.generous, d.honest, d.patriotic, 
						d.intelligent, d.openmind, r.mean, r.hypo, r.selfish, r.closemind)==1, pid3, ukpartyid))

# Mean
dm$simple <- rowMeans(dm[,1:18])

fit3 <- ltm(dm[,1:18] ~ z1)
fsc <- factor.scores(fit3)

dm$join <- do.call(paste, c(dm[,1:18], sep = ""))

newdf <- fsc$score.dat
newdf$join <- do.call(paste, c(newdf[,1:18], sep = ""))
c <- merge(dm, newdf, by='join', all.x=T, all.y=T)

d <- c[order(c$z1),]
cor(c$z1, c$simple)

c$ukpartyid <- car::recode(c$ukpartyid, "1 = 'Conservative'; 2='Labour'; c(3,4,5,6,7)='Other'")
c$pid 		<- car::recode(c$pid, "1 = 'Democrat'; 2='Republican'; c(3,4,5) ='Other'")

c$cid <- NA
c$cid[!is.na(c$pid)] <- c$pid[!is.na(c$pid)]
c$cid[is.na(c$pid)]  <- c$ukpartyid[is.na(c$pid)]

c$cid <- car::recode(c$cid, "'Other'=NA")
c$cid <- factor(c$cid, c('Conservative', 'Labour', 'Democrat', 'Republican'))

pdf("polar/fig/icc.trait.c.pdf",width=5.5,height=5)
plot(fit3)
dev.off()

#pdf("polar/fig/irt.box.trait.c.pdf",width=6.5,height=5)
png("polar/fig/irt.box.trait.c.png",width=650,height=500)
par(oma=c(.9,0,0,0), mar=c(4.5,5.2,3.2,2.1))
boxplot(c$z1[!is.na(c$cid)] ~ c$cid[!is.na(c$cid)], col=c("#2e4174", "red", "#0040ff", "pink"),
		xlab="Party ID", ylab="Latent Affect", ylim=c(-2.5, 2.5))
#title("Affective Polarization?: Affect towards Partisans\n by Partisan self-identification", cex=.75)
shortname <- "Source: YouGov/Polimetrix. Measure: 18 Trait Ratings of Republicans and Democrats. Time:"
mtext(paste(shortname, " ", format(Sys.time(), "%Y-%m-%d %H:%M")), cex=0.65, line=0, side=SOUTH<-1, adj=0, outer=TRUE)
dev.off()

pdf("polar/fig/irt.density.trait.c.pdf")
par(oma=c(.9,0,0,0), mar=c(4.5,5.2,3.2,2.1))
plot(density(c$z1[!is.na(c$cid) & c$cid=="Republican"]), col="pink", ylim=c(0,.75), xlim=c(-2.5,2.5),
		main="Affective Polarization?: Affect towards Partisans, as measured \n by trait ratings, by Partisan self-identification", xlab="Latent Affect")
lines(density(c$z1[!is.na(c$cid) & c$cid=="Democrat"]), col="blue")
lines(density(d$z1[!is.na(c$cid) & c$cid=="Labour"]), col="red")
lines(density(d$z1[!is.na(c$cid) & c$cid=="Conservative"]), col="#2e4174")
legend(-2.3, .75, col=c("pink", "blue", "red", "#2e4174"), c("Republicans", "Democrats", "Labour", "Conservatives"), pch=1, cex = .75)
shortname <- "Source: YouGov/Polimetrix. Measure: 18 Trait Ratings of Republicans and Democrats. Time:"
mtext(paste(shortname, " ", format(Sys.time(), "%Y-%m-%d %H:%M")), cex=0.65, line=0, side=SOUTH<-1, adj=0, outer=TRUE)
dev.off()


# Linking two data-sets with common items
###################################################
dat1 <- with(rivers[!is.na(rivers$partid),], data.frame(cbind(r.generous, r.honest, r.patriotic, r.intelligent, 
						r.openmind, d.mean, d.hypo, d.selfish, d.closemind)==0, cbind(d.generous, d.honest, 
						d.patriotic, d.intelligent, d.openmind, r.mean, r.hypo, r.selfish, r.closemind)==1,
						partid, src))
dat2 <- with(verba[!is.na(verba$partid),], data.frame(cbind(r.intelligent, r.welfare, r.independence, d.selfish, 
						d.betrayer, d.fascist, d.ignorant)==0, cbind(d.intelligent, d.welfare, d.independence, 
						r.selfish, r.fascist, r.ignorant, r.betrayer)==1, partid, src))


#fit2 <- ltm(dat2[,1:14] ~ z1 +)

# combine in one data-set by
lisForms <- list(as.data.frame(dat1[,1:18]), as.data.frame(dat2[,1:14]))
a <- testEquatingData(lisForms)
b <- as.data.frame(a)
b$pid <- as.factor(c(as.character(dat1$partid), paste("1960", as.character(dat2$partid))))
b$cid <- c(rep(1, nrow(dat1)), rep(0, nrow(dat2)))
b$src <- as.factor(c(dat1$src, as.character(dat2$src)))
b$join <- do.call(paste, c(b[,1:28], sep = ""))

#fit <- rasch(a, constraint = cbind(ncol(a) + 1, 1))
fit2 <- ltm(a ~ z1 +z2)
summary(fit2)

fsc <- factor.scores(fit2)
newdf <- fsc$score.dat

g <- newdf[,c(names(b[1:28]), "Obs", "Exp",  "z1", "se.z1", "z2", "se.z2")]
g$join <- do.call(paste, c(newdf[,1:28], sep = ""))

c <- merge(b, g, by='join', all.x=T, all.y=T)

c$ppid <- factor(c$pid,levels=c("1960 Conservative", "1960 Labour",  "1960 Republican", "1960 Democrat", "Conservative", "Labour",  "Republican", "Democrat"))
png("polar/fig/joint.6010.irt.png", width=2850, height=2450, res='300')
boxplot(c$z1 ~ c$ppid, col=c("#2e417455", "#cc000099", "#0040ee55", "#FF6EC755"),
		ylim=c(-2.5, 2.5), axisnames = FALSE, axes=F)
#main="Proportion upset or displeased if son or daughter married\n someone from another party in 1960 and 2010")
axis(2, at=seq(-2.5, 2.5,.5),labels=kros(seq(-2.5, 2.5, .5)))
mtext("Net Affect towards Democrats", side=2, cex=1.2, cex.lab=1, line=3)
# Add the individual bar labels
mtext(1, at = c(1,2,3,4,5,6,7,8), text = c("Conservative", "Labour", "Republican", "Democrat", "Conservative", "Labour", "Republican", "Democrat"), line = 0, cex = 1)
mtext(1, at = c(2.5,6.5), text = c("1960", "2010"), line = 2, cex=1)
box()
shortname <- "Source: 2009: YouGov/Polimetrix; 2010: YouGov/Polimetrix; 1960: Almond and Verba."
mtext(shortname, cex=.95, line=0, side=SOUTH<-1, adj=.1, outer=TRUE)
dev.off()

boxplot(c$z2 ~ c$pid, col=c("#2e4174", "red", "#0040ff", "pink"),
		xlab="Party ID", ylab="Latent Affect", ylim=c(-2.5, 2.5))

plot(c$z1)

plot(density(c$z1[!is.na(c$pid) & c$cid==0 & c$pid==3]), col="pink", ylim=c(0,.75), xlim=c(-2.5,2.5),
		main="Affective Polarization?: Affect towards Partisans, as measured \n by trait ratings, by Partisan self-identification", xlab="Latent Affect")
lines(density(c$z1[!is.na(c$pid) & c$cid==0 & c$pid==1]), col="blue")
lines(density(c$z1[!is.na(c$pid) & c$cid==0 & c$pid==2]), col="red")
lines(density(c$z1[!is.na(c$pid) & c$cid==0 & c$pid==4]), col="#2e4174")

library(plink)
common <- matrix(c(c(18, 28, 17, 27), 222:225), 4, 2)

pars <- as.irt.pars(x = list(x.D, x.E, x.F),
		common = list(common.DE, common.EF), cat = list(cat.D, cat.E, cat.F),
		poly.mod = list(poly.mod.D, poly.mod.E, poly.mod.F))