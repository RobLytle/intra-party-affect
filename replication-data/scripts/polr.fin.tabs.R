##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--++
##      Polarization
##		Last Edited: 1/27/11  	         
##   	Gaurav Sood							    	 ## 
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--++

# Set Working dir.
	setwd(basedir)

## Load Functions
	source("func/func.R")
	source("func/reg.R")
	
## LOAD DATA 
	load("polar/data/rivers.rdata")
	load("polar/data/verba.rdata")
	vars <- read.csv("polar/data/vars.csv", header=T)

## Some more functions
	# lmc2(iv,dv,wt=NULL): 2nd coefficients plus the stars from a regression
	
# Weighted Mean of two variables
	wt.mean <- function(x, y, wt){
		c(round(weighted.mean(x,wt, na.rm=T),2), round(weighted.mean(y,wt, na.rm=T),2))
	}
	
	prop <- function(x,y){
		diff <- mean(x, na.rm=T) - mean(y, na.rm=T)
		grp  <- c(rep(1, length(x)), rep(0, length(y)))
		paste(kros(diff), stars(fisher.test(table(c(x,y), grp))$p.val), sep="")
	}
	
	wt.prop <- function(x, wtx=NA, y, wty=NA){
		# x <- rep$marry.d ==1;wtx <- rep$weight;y <- dem$marry.r==1;wty <- dem$weight
		diff <- weighted.mean(x, wtx, na.rm=T) - weighted.mean(y, wty, na.rm=T)
		grp  <- c(rep(1, length(x)), rep(0, length(y)))
		wt   <- c(wtx, wty)
		paste(kros(diff), stars(chisq.test(round(xtabs(wt ~ c(x,y) + grp)))$p.val), sep="")
	}

## Initializing some more data
##

	vars.1 <- c("Generous","Honest","Patriotic","Mean","Hypocritical","Intelligent","Selfish",
			"Open-minded","Close-minded","None", "Mean No. Positives", "Mean No. Negatives", "Mean No. Pos.- Neg.", 
			"Marry", "Marry Oth. Rlgn.")
	
	# Some subsets
	rep <- subset(rivers, rivers$pid3==2) 		   ##rivers$pid3==2 (Republican)
	dem <- subset(rivers, rivers$pid3==1)  		   ##rivers$pid3==1 (Democrat)
	con <- subset(rivers, rivers$ukpartyid==1) 	   ##rivers$ukpartyid==1 (Conservatives)
	lab <- subset(rivers, rivers$ukpartyid==2)     ##rivers$ukpartyid==2 (Labor)
	
	v.rep <- subset(verba, tolower(verba$v2)=="united states" & verba$pid5 > 3)
	v.dem <- subset(verba, tolower(verba$v2)=="united states" & verba$pid5 < 3)
	v.con <- subset(verba, tolower(verba$v2)=="united kingdom" & verba$pid5 > 3)
	v.lab <- subset(verba, tolower(verba$v2)=="united kingdom" &  verba$pid5 < 3)
	
	t.rep <- subset(us10, us10$rd=='rep')
	t.dem <- subset(us10, us10$rd=='dem')

## *****************************  ##
## Tab 1: Trait Ratings			  ##
## ****************************** ##

res <- data.frame(var=1:20, rr=NA, rd=NA, rrd=NA, dd=NA, dr=NA, ddr=NA, cc=NA, cl=NA, ccl=NA, ll=NA, lc=NA, 
		llc=NA, rr.cc=NA, rd.cl=NA, rd.cl.p=NA, dd.ll=NA, dr.lc=NA, dr.lcp=NA)
colnames(res) <- c("Variables","R rate R", "R rate D","R R.min.D", "D rate D","D rate R", "D D.min.R", "C rate C", 
		"C rate L", "C C.min.L", "L rate L", "L rate C", "L L.min.C", "RR-CC","RD - CL", "RminD - CminL", "DD-LL","DR-LC", "DminR - LminC")

j <- 1
for(i in c(1:10, seq(291,295,2))){
	res[j,1] <- vars.1[j]
	r <- ifelse(i <  11, i + 11, i)
	d <- ifelse(i < 11,  i + 21, i+1)
	res[j,2:3] <- wt.mean(rep[,r], rep[,d], rep$weight)
	lm1 <- summary(lm(I(rep[,r] - rep[,d]) ~ 1, weight=rep$weight))
	res[j,4] <- paste(kros(lm1$coeff[1,1]), stars(lm1$coeff[1,4]), sep="")
	res[j,5:6] <- wt.mean(dem[,d],dem[,r], dem$weight)
	lm2 <- summary(lm(I(dem[,d] - dem[,r]) ~ 1, weight=dem$weight))
	res[j,7] <- paste(kros(lm2$coeff[1,1]), stars(lm2$coeff[1,4]), sep="")
	res[j,8:9] <- wt.mean(con[,r],con[,d], con$weight)
	lm3 <- summary(lm(I(con[,r] - con[,d]) ~ 1, weight=con$weight))
	res[j,10] <- paste(kros(lm3$coeff[1,1]), stars(lm3$coeff[1,4]), sep="")
	res[j,11:12] <- wt.mean(lab[,d], lab[,r], lab$weight)
	lm4 <- summary(lm(I(lab[,d] - lab[,r]) ~ 1, weight=lab$weight))
	res[j,13] <- paste(kros(lm4$coeff[1,1]), stars(lm4$coeff[1,4]), sep="")
	
	res[j,14] <- wt.prop(rep[,r], rep$weight, con[,r], con$weight)
	res[j,15] <- wt.prop(rep[,d], rep$weight, con[,d], con$weight)
	
	lm7 <- summary(lm(I(c(rep[,r] - rep[,d], con[,r] - con[,d])) ~ c(rep(1,nrow(rep)), rep(0, nrow(con))), weight=c(rep$weight, con$weight)))
	res[j,16] <- paste(kros(lm7$coeff[2,1]), stars(lm7$coeff[2,4]), sep="")
	
	res[j,17] <- wt.prop(dem[,d], dem$weight, lab[,d], lab$weight)
	res[j,18] <- wt.prop(dem[,r], dem$weight, lab[,r], lab$weight)
	lm5 <- summary(lm(I(c(dem[,d] - dem[,r], lab[,d] - lab[,r])) ~ c(rep(1,nrow(dem)), rep(0, nrow(lab))), weight=c(dem$weight, lab$weight)))
	res[j,19] <- paste(kros(lm5$coeff[2,1]), stars(lm5$coeff[2,4]), sep="")
	j <- j + 1
}

## Verba Traits
#################
res[14:20,1] <- c("Verba", "Intelligent","Welfare of Humanity","Selfish","Ignorant","Betrayers","Fascist")

vary <- c("r.intelligent", "d.intelligent", "r.welfare", "d.welfare", "r.selfish", "d.selfish", "r.ignorant", 
		"d.ignorant", "r.betrayer", "d.betrayer", "r.fascist","d.fascist")
j <- 15
for(i in seq(1,11,2)){
	res[j,2:3] <- wt.mean(v.rep[,vary[i]], v.rep[,vary[i+1]], v.rep$weight)
	lm1 <- summary(lm(I(v.rep[,vary[i]] - v.rep[,vary[i+1]]) ~ 1, weight=v.rep$weight))
	res[j,4] <- paste(kros(lm1$coeff[1,1]), stars(lm1$coeff[1,4]), sep="")
	res[j,5:6] <- wt.mean(v.dem[,vary[i+1]], v.dem[,vary[i]], v.dem$weight)
	lm2 <- summary(lm(I(v.dem[,vary[i+1]] - v.dem[,vary[i]]) ~ 1, weight=v.dem$weight))
	res[j,7] <- paste(kros(lm2$coeff[1,1]), stars(lm2$coeff[1,4]), sep="")
	res[j,8:9] <- wt.mean(v.con[,vary[i]],v.con[,vary[i+1]],v.con$weight)
	lm3 <- summary(lm(I(v.con[,vary[i]] - v.con[,vary[i+1]]) ~ 1, weight=v.con$weight))
	res[j,10] <- paste(kros(lm3$coeff[1,1]), stars(lm3$coeff[1,4]), sep="")
	res[j,11:12] <- wt.mean(v.lab[,vary[i+1]], v.lab[,vary[i]], v.lab$weight)
	lm4 <- summary(lm(I(v.lab[,vary[i+1]] - v.lab[,vary[i]]) ~ 1, weight=v.lab$weight))
	res[j,13] <- paste(kros(lm4$coeff[1,1]), stars(lm4$coeff[1,4]), sep="")
	
	res[j,14] <- wt.prop(v.rep[,vary[i]], v.rep$weight, v.con[,vary[i]], v.con$weight)
	res[j,15] <- wt.prop(v.rep[,vary[i+1]], v.rep$weight, v.con[,vary[i+1]], v.con$weight)
	
	lm7 <- summary(lm(I(c(v.rep[,vary[i]] - v.rep[,vary[i+1]], v.con[,vary[i]] - v.con[,vary[i+1]])) ~ c(rep(1,nrow(v.rep)), rep(0, nrow(v.con))), weight=c(v.rep$weight, v.con$weight)))
	res[j,16] <- paste(kros(lm7$coeff[2,1]), stars(lm7$coeff[2,4]), sep="")
	
	res[j,17] <- wt.prop(v.dem[,vary[i+1]], v.dem$weight, v.lab[,vary[i+1]], v.lab$weight)
	res[j,18] <- wt.prop(v.dem[,vary[i]],  v.dem$weight, v.lab[,vary[i]], v.lab$weight)
	lm5 <- summary(lm(I(c(v.dem[,vary[i+1]] - v.dem[,vary[i]], v.lab[,vary[i+1]] - v.lab[,vary[i]])) ~ c(rep(1,nrow(v.dem)), rep(0, nrow(v.lab))), weight=c(v.dem$weight, v.lab$weight)))
	res[j,19] <- paste(kros(lm5$coeff[2,1]), stars(lm5$coeff[2,4]), sep="")
	j <- j + 1
}
# Reorder Rows
res[1:13,] <- res[c("1", "2", "3", "6", "8", "4", "5", "7", "9", "10", "11", "12", "13"),]
write.csv(res, file="polar/res/tab1.csv")

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
	tab2[i,4] <- as.numeric(gsub('[^[:alnum:]]', "", tab2[i,2]))/100 - as.numeric(gsub('[^[:alnum:]]', "", tab2[i,3]))/100
	tab2[i,5] <- lmc2(dem[,i+21]==1, dem$strpid==1, dem$weight)
	tab2[i,6] <- lmc2(dem[,i+11]==1, dem$strpid==1, dem$weight)
	tab2[i,7] <- as.numeric(gsub('[^[:alnum:]]', "", tab2[i,5]))/100 - as.numeric(gsub('[^[:alnum:]]', "", tab2[i,6]))/100
}
j <- 11
for(i in seq(291,295,2)){
	tab2[j,1] <- vars.1[j]
	tab2[j,2] <- lmc2(rep[,i], rep$strpid==1, rep$weight)
	tab2[j,3] <- lmc2(rep[,(i+1)], rep$strpid==1, rep$weight)
	tab2[j,4] <- as.numeric(gsub('[^[:alnum:]]', "", tab2[j,2]))/100 - as.numeric(gsub('[^[:alnum:]]', "", tab2[j,3]))/100
	tab2[j,5] <- lmc2(dem[,(i+1)], dem$strpid==1, dem$weight)
	tab2[j,6] <- lmc2(dem[,i], dem$strpid==1, dem$weight)
	tab2[j,7] <- as.numeric(gsub('[^[:alnum:]]', "", tab2[j,5]))/100 - as.numeric(gsub('[^[:alnum:]]', "", tab2[j,6]))/100
	j <- j +1
}

write.csv(tab2, file="polar/res/tab2.csv")

## *****************************  ##
## Tab 4: Marriage			  ##
## ****************************** ##

tab4 <- data.frame(var=1:8, rep=NA, dem=NA, rd.diff=NA, con=NA, lab=NA, cl.diff=NA, rc=NA, dl=NA)
colnames(tab4) <- c("Variables","Rep.", "Dems.", "Diff", "Cons.","Lab.", "Diff", "RC", "DL")
vars2 <- c("Not at all Upset", "Somewhat Upset", "Very Upset", "Net Upset", "Verba", "Pleased", "Indifferent", "Displeased")

tab4[1:8,1] <- vars2
tab4[1:3,2] <- rev(kros(prop.table(xtabs(rep$weight ~ rep$marry.d)))[1:3])
tab4[4,2]   <- kros(prop.table(xtabs(rep$weight ~ rep$marry.d < 3)))[2]
tab4[1:3,3] <- rev(kros(prop.table(xtabs(dem$weight ~ dem$marry.r)))[1:3])
tab4[4,3]   <- kros(prop.table(xtabs(dem$weight ~ dem$marry.r < 3)))[2]
tab4[1:3,5] <- rev(kros(prop.table(xtabs(con$weight ~ con$marry.d)))[1:3])
tab4[4,5]   <- kros(prop.table(xtabs(con$weight ~ con$marry.d < 3)))[2]
tab4[1:3,6] <- rev(kros(prop.table(xtabs(lab$weight ~ lab$marry.r)))[1:3])
tab4[4,6]   <- kros(prop.table(xtabs(lab$weight ~ lab$marry.r < 3)))[2]

tab4[1,4] <- wt.prop(rep$marry.d ==3, rep$weight, dem$marry.r==3, dem$weight)
tab4[1,7] <- wt.prop(con$marry.d ==3, con$weight, lab$marry.r==3, lab$weight)
tab4[1,8] <- wt.prop(rep$marry.d ==3, rep$weight, con$marry.d==3, con$weight)
tab4[1,9] <- wt.prop(dem$marry.r ==3, dem$weight, lab$marry.r==3, lab$weight)

tab4[2,4] <- wt.prop(rep$marry.d ==1, rep$weight, dem$marry.r==1, dem$weight)
tab4[2,7] <- wt.prop(con$marry.d ==1, con$weight, lab$marry.r==1, lab$weight)
tab4[2,8] <- wt.prop(rep$marry.d ==1, rep$weight, con$marry.d==1, con$weight)
tab4[2,9] <- wt.prop(dem$marry.r ==1, dem$weight, lab$marry.r==1, lab$weight)

tab4[3,4] <- wt.prop(rep$marry.d ==2, rep$weight, dem$marry.r==2, dem$weight)
tab4[3,7] <- wt.prop(con$marry.d ==2, con$weight, lab$marry.r==2, lab$weight)
tab4[3,8] <- wt.prop(rep$marry.d ==2, rep$weight, con$marry.d==2, con$weight)
tab4[3,9] <- wt.prop(dem$marry.r ==2, dem$weight, lab$marry.r==2, lab$weight)

tab4[4,4] <- wt.prop(rep$marry.d < 3, rep$weight, dem$marry.r < 3, dem$weight)
tab4[4,7] <- wt.prop(con$marry.d < 3, con$weight, lab$marry.r < 3, lab$weight)
tab4[4,8] <- wt.prop(rep$marry.d < 3, rep$weight, con$marry.d < 3, con$weight)
tab4[4,9] <- wt.prop(dem$marry.r < 3, dem$weight, lab$marry.r < 3, lab$weight)

tab4[6:8,2]  <- rev(kros(prop.table(table(v.rep$marry.d))))
tab4[6:8,3]  <- rev(kros(prop.table(table(v.dem$marry.r))))
tab4[6:8,5]  <- rev(kros(prop.table(table(v.con$marry.d))))
tab4[6:8,6]  <- rev(kros(prop.table(table(v.lab$marry.r))))

tab4[6,4] <- prop(v.rep$marry.d ==1, v.dem$marry.r==1)
tab4[6,7] <- prop(v.con$marry.d ==1, v.lab$marry.r==1)
tab4[6,8] <- prop(v.rep$marry.d ==1, v.con$marry.d==1)
tab4[6,9] <- prop(v.dem$marry.r ==1, v.lab$marry.r==1)

tab4[7,4] <- prop(v.rep$marry.d ==.5, v.dem$marry.r==.5)
tab4[7,7] <- prop(v.con$marry.d ==.5, v.lab$marry.r==.5)
tab4[7,8] <- prop(v.rep$marry.d ==.5, v.con$marry.d==.5)
tab4[7,9] <- prop(v.dem$marry.r ==.5, v.lab$marry.r==.5)

tab4[8,4] <- prop(v.rep$marry.d ==0, v.dem$marry.r==0)
tab4[8,7] <- prop(v.con$marry.d ==0, v.lab$marry.r==0)
tab4[8,8] <- prop(v.rep$marry.d ==0, v.con$marry.d==0)
tab4[8,9] <- prop(v.dem$marry.r ==0, v.lab$marry.r==0)

write.csv(tab4, file="polar/res/tab4.csv")

## *****************************  ##
## Tab 5: Trait Ratings			  ##
## ****************************** ##

res3 <- data.frame(var=1:3, us=NA, uk=NA)
colnames(res3) <- c("Variables","US", "UK")
vars3 <- c("Very Upset", "Somewhat Upset", "Net Upset")

i <- 1
res3[i,1] <- vars3[i]
res3[i,2] <- with(us, weighted.mean(marry.rel ==1, weight, na.rm=T))
res3[i,3] <- with(uk, weighted.mean(marry.rel ==1, weight, na.rm=T))
i <- i + 1
res3[i,1] <- vars3[i]
res3[i,2] <- with(us, weighted.mean(marry.rel ==2, weight, na.rm=T))
res3[i,3] <- with(uk, weighted.mean(marry.rel ==2, weight, na.rm=T))
i <- i + 1
res3[i,1] <- vars3[i]
res3[i,2] <- with(us, weighted.mean(marry.rel < 3, weight, na.rm=T))
res3[i,3] <- with(uk, weighted.mean(marry.rel < 3, weight, na.rm=T))



## Latent Trait Model
###########################
library(ltm)
# r good, d bad
dm <- with(rivers[!is.na(rivers$pid3),], data.frame(cbind(r.generous, r.honest, r.patriotic, r.intelligent, 
						r.openmind, d.mean, d.hypo, d.selfish, d.closemind)==2, cbind(d.generous, d.honest, d.patriotic, 
						d.intelligent, d.openmind, r.mean, r.hypo, r.selfish, r.closemind)==1, pid3))
descript(dm[, 1:18])

# Mean
dm$simple <- rowMeans(dm[,1:18])

fit <- ltm(dm[,1:18] ~ z1)
fsc <- factor.scores(fit)

dm$join <- do.call(paste, c(dm[,1:18], sep = ""))

newdf <- fsc$score.dat
newdf$join <- do.call(paste, c(newdf[,1:18], sep = ""))
c <- merge(dm, newdf, by='join', all.x=T, all.y=T)
d <- c[order(c$z1),]

cor(c$z1, c$simple)

c$pid <- NA
c$pid[c$pid3==2] <- "Republicans"
c$pid[c$pid3==1] <- "Democrats"

### Plotting
#####################
pdf("polar/fig/irt.dist.trait.pdf")
par(oma=c(.9,0,0,0), mar=c(4.5,5.2,3.2,2.1))
plot(seq(1, length(d$z1))~ d$z1, col="white", ylim=c(0,370), xlab="Latent Affect", ylab="", yaxt='n', 
		main="Affective Polarization?: Affect towards Partisans, 
				as measured by trait ratings")
points(seq(1, sum(d$pid3==1)) ~ d$z1[d$pid3==1], col="light blue")
points(seq(1, sum(d$pid3==2)) ~ d$z1[d$pid3==2], col="pink")
points(seq(1, sum(d$pid3> 2)) ~ d$z1[d$pid3 > 2], col="grey")
legend(-2, 370, col=c("pink", "light blue", "grey"), c("Republicans", "Democrats", "Independents"), pch=1, cex = .75)
shortname <- "Source: YouGov/Polimetrix. Measure: 18 Trait Ratings of Republicans and Democrats. Time:"
mtext(paste(shortname, " ", format(Sys.time(), "%Y-%m-%d %H:%M")), cex=0.65, line=0, side=SOUTH<-1, adj=0, outer=TRUE)
dev.off()

pdf("polar/fig/irt.box.trait.pdf",width=6.5,height=5)
par(oma=c(.9,0,0,0), mar=c(4.5,5.2,3.2,2.1))
boxplot(d$z1[!is.na(c$pid)] ~ (c$pid)[!is.na(c$pid)], col=c("light blue", "pink"), 
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

## Fig 2 
## Verba/Rivers comparison
rv <- tab4[c(4,8),]
rv$year <- c(1959, 2010, 2010)
rs <- array(c(.05, .27, .49, 1960, 2010, 2010), dim=c(3,1))
ds <- array(c(.04, .20, .32, 1960, 2010, 2010), dim=c(3,1))
net <- rbind(rs, ds)

png("polar/fig/verba.rivers.10nat.png", width=2850, height=2450, res='300')
par(oma=c(.9,0,0,0), mar=c(4,5.5,3,2))
mp <- barplot(net, beside=T, axisnames = FALSE, ylim=c(0,.55), density=c(100,100,30,100,100,30), 
		col=c("#eebbbb", "#eebbbb", "#eebbbb", "#ccccff", "#ccccff", "#ccccff"), axes=F)
#main="Proportion upset or displeased if son or daughter married\n someone from another party in 1960 and 2010")
axis(2, at=seq(0, .55, .05),labels=kros(seq(0, .55, .05)))
mtext("Proportion upset or dipleased\n if son or daughter married someone from another party", side=2, 
		cex=1.2, cex.lab=1, line=3)
# Add the individual bar labels
mtext(1, at = mp, text = c("1960", "2010 YouGov", "2010 Survey 2"), line = 0, cex = 1)
mtext(1, at = c(2.5,5.5), text = c("Republicans", "Democrats"), line = 2, cex=1)
legend(4.2, .53, cex=.95, density=c(100,100,30), col="#555555", c("1960 - Proportion 'displeased'", "2010 - Proportion 'somewhat' or 'very' upset", 
				"2010 - Proportion 'somewhat' or 'very' unhappy"))
box()
shortname <- "Source: 2010: YouGov/Polimetrix; 2010: YouGov/Survey 2; 1960: Almond/Verba."
mtext(shortname, cex=.95, line=0, side=SOUTH<-1, adj=.1, outer=TRUE)
dev.off()

##########################################
# Using all data						##
##########################################

dm <- with(rivers,data.frame(cbind(r.generous, r.honest, r.patriotic, r.intelligent, 
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
c$pid <- car::recode(c$pid, "1 = 'Democrat'; 2='Republican'; c(3,4,5) ='Other'")

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
title("Affective Polarization?: Affect towards Partisans\n by Partisan self-identification", cex=.75)
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

