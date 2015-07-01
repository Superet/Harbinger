# Read in data:
set.seed(1111)
library(glmnet)
library(doParallel)
library(foreach)
library(caret)
library(Matrix)
library(gamlr)
library(pROC)
library(scales)

setwd("~/Documents/Research/Harbinger/Exercise/machine_learning")

my.savefile 	<- "~/Desktop/fit_oos.csv"
f3			<- file(my.savefile, "w")
writeLines("Table of out-of-sample model fitness\n", f3)
close(f3)

myfiles 	<- list.files()
sel			<- grep(".rdata",myfiles)
myfiles		<- myfiles[sel]

for(kkk in 1:length(myfiles)){
	load(myfiles[kkk])
	
	# Out-of-sample prediction 
	e1			<- phat.oos - y.out
	tab.oos		<- data.frame(data=seldata, category=selcat, modelidx)
	tab.oos$cor	<- apply(phat.oos, 2, function(x) cor(x, y.out))	
	tab.oos$RMSE	<- apply(e1, 2, function(x) sqrt(mean(x^2)))
	tab.oos$MAD  <- apply(e1, 2, function(x) median(abs(x)))
	tab.oos$ROC 	<- tab.oos$sensitivity.mean <- tab.oos$specificity.mean <- NA
	
	###############################
	# Compute baseline prediction #
	###############################
	num_grp	<- 4
	af		<- colSums(y*x)/colSums(x)
	# hist(af, main = "Histogram of affinity index from the classification set")
	sum(is.na(af))
	quantile(af, na.rm=T, c(.25, .5, .75))
	grp 	<- as.character(cut(af, c(-.5, quantile(af, na.rm=T, c(1:num_grp/num_grp))), include.lowest=T, label=paste("grp",1:num_grp,sep="") ))
	grp[is.na(grp)] <- "grp0"
	grp		<- as.factor(grp)

	# Find column index for each group
	tmp		<- levels(grp)
	grp_idx <- lapply(tmp, function(x) which(grp==x))
	x.out.grp <- sapply(grp_idx, function(sel) rowSums(x.out[,sel]))
	colnames(x.out.grp) <- tmp

	# Predict product failure
	lmg		<- glm(y.out ~ x.out.grp, family="binomial")
	summary(lmg)
	p.base 	<- predict(lmg, newdata = data.frame(x.out.grp), type = "response")
	p.baselm <- predict(lm(y.out ~ x.out.grp))	
	
	# Plot Roc 
	myalpha <- .6
	tmp		<- roc(y.out, p.baselm)
	modelidx$shrinkage <- factor(modelidx$shrinkage, levels=c("ridge","elastic.net","lasso","gamma.lasso"))
	pdf(paste("~/Desktop/graph_auc_",seldata,"_",selcat,".pdf",sep=""),width=7, height=6)
	print(plot(tmp, main = paste("OOS ROC for gaussian models with ",seldata, "_", selcat, sep=""),col="grey30", lwd=1))
	legend("bottomright", legend=c("Baseline", paste(rep(c("no cov","covariates"), each=4), modelidx[1:8, "shrinkage"], sep="-")),
	           col=c("grey30", rep(1:4, 2)), lty= c(1,rep(1:2, each=4)), lwd = c(1, rep(2,8)))	
	for(i in 1:(ncol(phat.oos)/2)){
		tmp3	<- roc(y.out, phat.oos[,i])
		tab.oos[i,"ROC"]	<- tmp3$auc
		tab.oos[i,"sensitivity.mean"]	<- mean(tmp3$sensitivities, na.rm=T)
		tab.oos[i,"specificity.mean"]	<- mean(tmp3$specificities, na.mr=T)
		print(plot(tmp3,add=TRUE, col=alpha(as.numeric(modelidx[modelidx$index==i, "shrinkage"]), myalpha), 
					lty = as.numeric(modelidx[modelidx$index==i, "covariates"])), lwd=2)
	}
	
	tmp		<- roc(y.out, p.base)
	print(plot(tmp, main = paste("OOS ROC for logit models with ",seldata, "_", selcat, sep=""),col="grey30", lwd=1))
	legend("bottomright", legend=c("Baseline", paste(rep(c("no cov","covariates"), each=4), modelidx[9:16, "shrinkage"], sep="-")),
	           col=c("grey30", rep(1:4, 2)), lty= c(1,rep(1:2, each=4)), lwd = c(1, rep(2,8)))	
	for(i in 9:ncol(phat.oos)){
		tmp3	<- roc(y.out, phat.oos[,i])
		tab.oos[i,"ROC"]	<- tmp3$auc
		tab.oos[i,"sensitivity.mean"]	<- mean(tmp3$sensitivities, na.rm=T)
		tab.oos[i,"specificity.mean"]	<- mean(tmp3$specificities, na.mr=T)
		print(plot(tmp3,add=TRUE, col=alpha(as.numeric(modelidx[modelidx$index==i, "shrinkage"]), myalpha), 
					lty = as.numeric(modelidx[modelidx$index==i, "covariates"])), lwd=2)
	}
	dev.off()
	
	f3			<- file(my.savefile, "at")
	write.csv(tab.oos, f3)
	writeLines("\n", f3)
	close(f3)
	
	rm(list = setdiff(ls(), c("kkk","myfiles","my.savefile")))
}


