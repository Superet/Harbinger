library(xlsx)
library(ggplot2)
library(data.table)
library(gridExtra)
library(reshape2)
library(scales)

my.wd <- "~/Documents/Research/Harbinger/processed data"
iri.wd <- "~/Documents/Research/Data/IRI/kelloggirir"
setwd(my.wd)
plot.wd <- "~/Desktop"
ww <- 6.5
ar <- .6

my.wd <- "//tsclient/Resear1/Harbinger/processed data"
iri.wd <- "E:/Users/Projects/Project_Marketing_IRI/kelloggirir"
setwd(my.wd)

source("../Exercise/mosaic.R")
tmp.csv <- paste(my.wd,"/tmp_result_4.csv",sep="")

# Read data #
mycut	<-   4		# The cutoff year that defines product failure
upc	<- read.csv(paste(my.wd,"/new_product.csv",sep=""),header=T,as.is=T,comment.char = '',quote="",blank.lines.skip = T)
hh 	<- read.csv(paste(my.wd,"/new_product_hh_class_cutoff",mycut,".csv",sep=""),header=T,as.is=T,comment.char = '',quote="",blank.lines.skip = T)
baselab	<- "Harbinger"

# If we only consider the first and last quantile group 
hh_save <- hh
hh		<- subset(hh_save, cls_grp %in% c(1,4))
hh$grp 	<- factor(hh$grp, levels=c("Normal","Harbinger"), labels=c("1st Qt","4th Qt"))
baselab	<- "4th Qt"

#############
# Functions #
#############

# Permutation test of demographic difference
grp.permutation <- function(grp,x,numsim,base.grp=1,drop=NULL){
	out <- rep(NA,numsim)
	if(!is.null(drop)){
		grp <- grp[!drop]
		x	<- x[!drop]
	}
	for(i in 1:numsim){
		grp1 <- sample(grp==base.grp)
		grp2 <- !grp1
		out[i] <- mean(x[grp2],na.rm=T) - mean(x[grp1],na.rm=T)
	}
	out
}

mean.qt <- function(x){
	out <- c(mean(x), quantile(x, c(.025, .975)) )
	names(out) <- c("y", "ymin", "ymax")
	out
}

######################################
### Harbinger demographic profiles ###
######################################
# ------------------------------------------------------------------------------------# 
# Format demographic variables

### Read demographic data ###
# demo <- read.delim(file=paste(iri.wd,"/DEMO.txt",sep=""), header=F,blank.lines.skip = T, sep="|", row.names=NULL,comment.char = '', quote = '')
# sel <- colMeans(is.na(demo))!=1
# demo <- demo[,sel]
# colnames(demo) <- c("PANID", "FAMSIZE", "INCOME", "RACE", "CHILDREN", "FMLE_AGE", "FMLE_EDU", "FMLE_HRS", "FMLE_OCC", 
# 					"MALE_AGE", "MALE_EDU", "MALE_HRS", "MALE_OCC", "M_STATUS", "RENTOWN", "NUM_CATS", "NUM_DOGS",
# 					"REGION", "MKT", "PROJ09", "PROJ08", "PROJ07", "PROJ06", "ZIPCODE")


#----------------------------------------- # 
# Plot the demographic composition # 

selcol <- c("FAMSIZE","INCOME","CHILDREN","FMLE_AGE","MALE_AGE","FMLE_EDU","MALE_EDU","FMLE_HRS","MALE_HRS","M_STATUS")
par(mfrow=c(2,5))
for(i in 1:length(selcol)){
	tmp <- table(hh[,"grp"],hh[,selcol[i]])
	mosaic.plot(tmp,cex=.65,print=F,main=selcol[i])
}

plots <- list(NULL)
for(i in 1:length(selcol)){
	plots[[i]] <- makeplot_mosaic(hh,x="grp",y=selcol[i])
}

pdf("~/Desktop/graph_mosaic_harbinger.pdf",width=15,height=15)
do.call(grid.arrange,c(plots,list(ncol=3)))
dev.off()

#-------------------------------------------------------------------#
# Permutation test: if two groups differ in any demographic variables 
selcol <- c("FAMSIZE","INCOME","CHILDREN","FMLE_AGE","MALE_AGE","FMLE_EDU","MALE_EDU","FMLE_HRS","MALE_HRS","M_STATUS")
numsim <- 999
ggtmp <- matrix(NA,numsim,length(selcol))
ggtmp1 <- rep(NA,length(selcol))
names(ggtmp1) <- selcol
sel <- hh$grp == baselab
for(i in 1:length(selcol)){
	if(selcol[i] %in% c("FMLE_AGE","MALE_AGE","FMLE_EDU","MALE_EDU","FMLE_HRS","MALE_HRS")){
		drop <- hh[,selcol[i]] %in% c("NO FEMALE HEAD OF HOUSEHOLD", "VALUE NOT AVAILABLE", "NO MALE HEAD OF HOUSEHOLD")
	}else{
		drop <- NULL
	}
	ggtmp[,i] <- grp.permutation(hh$grp,as.numeric(hh[,selcol[i]]),numsim=numsim,base.grp=setdiff(unique(hh$grp),baselab), drop=drop)
	ggtmp1[i] <- mean(as.numeric(hh[sel,selcol[i]]),na.rm=T) - mean(as.numeric(hh[!sel,selcol[i]]),na.rm=T)
	print(i)
}
colnames(ggtmp) <- selcol
tmp <- colMeans(ggtmp >= rep(1,numsim) %*% t(ggtmp1))
tmp <- cbind(ggtmp1,tmp)
colnames(tmp) <- c("Harbinger - Normal","p.value")

sel 	<- tmp[,"p.value"] < .05
selcol1 <- rownames(tmp)[sel]


######################
# Hypotehsis testing #
######################
# Plot the behavior distribution 
selcol <- c("pct_niche","niche_cat_share","pct_coupon","pct_priceoff","num_chain","prim_share","adoption_all","adoption_success","adoption_fail",
			"all_num_brand","HHI","ENTR","repeat_freq")
ggtmp <- melt(hh[,c("PANID","grp",selcol)],id.var=c("PANID","grp"))

ggplot(ggtmp,aes(value,col=grp)) + geom_density() + 
		facet_wrap(~variable, scales="free")
ggplot(ggtmp,aes(x=grp,y=value)) + stat_summary(fun.data = mean.qt, geom="pointrange", shape=1) +
		facet_wrap(~variable, scales="free")

#---------------------------------# 
# Simple t-test 
selcol <- c("pct_niche","niche_cat_share","pct_coupon","pct_priceoff","num_chain","prim_share","adoption_all","adoption_success","adoption_fail",
			"all_num_brand","HHI","ENTR","repeat_freq")
sel <- hh$grp==baselab			# Select harbingers
tmp.tab <- data.frame()
for(i in 1:length(selcol)){
	tmp <- t.test(as.numeric(hh[!sel,selcol[i]]),as.numeric(hh[sel,selcol[i]]))
	tmp.tab <- rbind(tmp.tab,data.frame(Variable=selcol[i],Normal=tmp$estimate[1],Harbinger=tmp$estimate[2],
										Harbinger_Normal=tmp$estimate[2]-tmp$estimate[1],p.value=tmp$p.value))
}
row.names(tmp.tab) <- NULL
selcol	<- sapply(1:ncol(tmp.tab), function(x) is.numeric(tmp.tab[,x]))
cat("T test results:\n"); cbind(tmp.tab[,!selcol], round(tmp.tab[,selcol], 4))

f <- file(tmp.csv,"w")
write.csv(tmp.tab,f)
close(f)

#-----------------------------------#
# Permuation test 
numsim <- 9999
selcol <- c("pct_niche","niche_cat_share","pct_coupon","pct_priceoff","num_chain","prim_share","adoption_all","adoption_success","adoption_fail",
			"all_num_brand","HHI","ENTR","repeat_freq")
ggtmp <- matrix(NA,numsim,length(selcol))
ggtmp1 <- rep(NA,length(selcol))
names(ggtmp1) <- selcol
sel <- hh$grp == baselab
for(i in 1:length(selcol)){
	ggtmp[,i] <- grp.permutation(hh$grp,hh[,selcol[i]],numsim=numsim,base.grp=setdiff(unique(hh$grp),baselab))
	ggtmp1[i] <- mean(hh[sel,selcol[i]],na.rm=T) - mean(hh[!sel,selcol[i]],na.rm=T)
	print(i)
}
colnames(ggtmp) <- selcol
tmp <- colMeans(ggtmp >= rep(1,numsim) %*% t(ggtmp1))
tmp <- cbind(ggtmp1,tmp)
colnames(tmp) <- c("Harbinger - Normal","p.value")
tmp.tab	<- cbind(tmp.tab, Permutation.p=tmp[,"p.value"])
selcol	<- sapply(1:ncol(tmp.tab), function(x) is.numeric(tmp.tab[,x]))
cat("Testing difference:\n"); cbind(tmp.tab[,!selcol], round(tmp.tab[,selcol], 4))

f <- file(tmp.csv,"at")
writeLines("--------------------------------------",f)
writeLines("Permutation test",f)
write.csv(tmp,f)
close(f)

# Bar plot of the permutation 
ggtmp2 <- melt(ggtmp)
names(ggtmp2) <- c("sim","variable","dif")
ggtmp2$variable <- factor(ggtmp2$variable)

#----------------------------------------# 
# Bar chart by hypothesis
ggtmp2 <- data.frame()	
sel <- hh$grp == baselab	
for(i in 1:length(selcol)){
	ggtmp2 <- rbind(ggtmp2, data.frame(var = selcol[i], Harbinger = mean(hh[sel,selcol[i]], na.rm=T), 
				Normal = mean(hh[!sel,selcol[i]], na.rm=T), p=tmp[i,"p.value"]))
}
ggtmp2$Variable <- c("Affinity of niche products", "Percent of total dollars \nspent on niche products", 
					"Proportion of items purchased\n with coupon per trip", "Proportion of items purchased\n at price reduction per trip", 
					"Number of chains\n visited per week", "Expenditure share\n at the primary store", 
					"Average adoption lag\n of all new products", "Average adoption lag\n of winning new products", "Average adoption lag\n of losing new products", 
					"Number of brand\n per category", "HHI of brand quantity\n per category", "Entropy of brand quantity\n per category", 
					"Frequency of repeated purchase\n of the same brand")
ggtmp2$Hypothesis <- c(rep("H1",2), rep("H2",4), rep("H3/4",3), rep("H5", 4))	
tmp.hyp <- sort(unique(ggtmp2$Hypothesis))
ggtmp3 <- melt(ggtmp2, id.var=c("Hypothesis","var","Variable","p"))
# ggtmp3$variable <- factor(ggtmp3$variable, levels=rev(levels(ggtmp3$variable)), ordered=TRUE)
ggtmp3$unit <- 1

plots <- list(NULL)
for(i in 1:length(tmp.hyp)){
	plots[[i]] <- ggplot(subset(ggtmp3, Hypothesis == tmp.hyp[i]), aes(x=Variable, y = value, fill=variable)) + 
					geom_bar(stat="identity", position="dodge") + 
					coord_flip() + 
					scale_fill_discrete(name="", guide = guide_legend(reverse=TRUE)) + 
					xlab("")
}
