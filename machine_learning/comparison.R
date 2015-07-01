# Compare models 
set.seed(1111)
library(glmnet)
library(doParallel)
library(foreach)
library(caret)
library(Matrix)
library(gamlr)
library(pROC)
library(reshape2)
library(ggplot2)

setwd("~/Documents/Research/Harbinger/Exercise/machine_learning")
selm	<- c("trial_all", "usage_all")

load(paste("rs_",selm[1],".rdata",sep=""))

# Out-of-sample prediction 
e1			<- phat.oos - y.out
tab.oos		<- data.frame(data=seldata, category=selcat, modelidx)
tab.oos$cor	<- apply(phat.oos, 2, function(x) cor(x, y.out))	
tab.oos$RMSE	<- apply(e1, 2, function(x) sqrt(mean(x^2)))
tab.oos$MAD  <- apply(e1, 2, function(x) median(abs(x)))
tab.oos$ROC 	<- tab.oos$sensitivity.mean <- tab.oos$specificity.mean <- NA

for(i in 1:ncol(phat)){
	tmp3	<- roc(y.out, phat.oos[,i])
	tab.oos[i,"ROC"]	<- tmp3$auc
	tab.oos[i,"sensitivity.mean"]	<- mean(tmp3$sensitivities, na.rm=T)
	tab.oos[i,"specificity.mean"]	<- mean(tmp3$specificities, na.rm=T)
}

######################################
# Covariates within a data selection #
######################################
sel		<- 1:n.c + n.hh
betac	<- sapply(mc, function(x) coef(x, select="min")[sel])
lbetac	<- sapply(lmc, function(x) coef(x, select="min")[sel])
rownames(betac) <- rownames(lbetac) <- colnames(z)
rs		<- colSums(z)/n.hh
cat("Rescaled coefficients for gaussian models:\n"); round(betac*rs, 2)
cat("Rescaled coefficients for logit models:\n"); round(lbetac*rs, 2)

# Out of sample predcition comparison
sel 	<- tab.oos$covariates == "no"
selcol 	<- c("cor", "RMSE","MAD","specificity.mean","sensitivity.mean", "ROC")
quartz()
par(mfrow = c(3, 2))
for(i in 1:length(selcol)){
	tmpr	<- range(tab.oos[,selcol[i]])
	plot(tab.oos[sel,selcol[i]], tab.oos[!sel,selcol[i]], xlim=tmpr, ylim=tmpr, xlab="No covariates", ylab="Covariates", main=selcol[i]); 
	abline(a = 0, b=1, col="red", lty = 2)
}	

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
oos.base<- c(cor = cor(p.base, y.out), RMSE = sqrt(mean((p.base - y.out)^2 )), MAD = median(abs(p.base - y.out)))
tmp		<- roc(y.out, p.base)
oos.base<- c(oos.base, sensitivity.mean=mean(tmp$sensitivities), specificity.mean=mean(tmp$specificities), ROC = tmp$auc)
cat("Model fitness for the baseline model:\n"); print(oos.base)

#-------------------------------------# 
# Add another dimension to segment households
tmp  	<- colSums(x)
quartz()
hist(tmp, breaks=100)
quantile(tmp, c(.25, .5, .75))
grp.new <- as.character(cut(tmp, c(-.5, quantile(tmp, na.rm=T, c(1:num_grp/num_grp))), include.lowest=T, label=paste("grp",1:num_grp,sep="") ))
grp.new[is.na(grp.new)] <- "grp0"
grp.new	<- as.factor(grp.new)

# Combine the two groups 
table(grp, grp.new)
dim_grp		<- c("ratio", "volume")
grp_name1	<- c("grp0*grp1", paste(rep(setdiff(levels(grp), "grp0"), each=num_grp), rep(levels(grp.new), num_grp), sep="_") )
tmp			<- paste(grp, grp.new, sep="_")
grp_idx1 	<- lapply(grp_name1, function(x) which(tmp==x))
x.out.grp1 	<- sapply(grp_idx1, function(sel) rowSums(x.out[,sel]))
colnames(x.out.grp1) <- grp_name1

# Predict product failure
summary(x.out.grp1)
lmg1		<- glm(y.out ~ x.out.grp1[,-1], family="binomial")
summary(lmg1)
p.base1 	<- predict(lmg1, newdata = data.frame(x.out.grp1), type = "response")
oos.base1<- c(cor = cor(p.base1, y.out), RMSE = sqrt(mean((p.base1 - y.out)^2 )), MAD = median(abs(p.base1 - y.out)))
tmp		<- roc(y.out, p.base1)
oos.base1<- c(oos.base1, sensitivity.mean=mean(tmp$sensitivities), specificity.mean=mean(tmp$specificities), ROC = tmp$auc)
cat("Model fitness for the base1line model:\n"); print(oos.base1)

#####################
# Compare the model #
#####################
# List oosf model summary 
ls.sgnb	<- vector("list", length(selm)); names(ls.sgnb) <- selm
ls.oos	<- vector("list", length(selm)); names(ls.oos) <- selm
ls.x	<- vector("list", length(selm)); names(ls.x) <- selm

ls.sgnb[[selm[1]]] 	<- tab.sgnb
ls.oos[[selm[1]]]	<- tab.oos
ls.x[[selm[1]]]		<- x

load(paste("rs_",selm[2],".rdata",sep=""))
# Out-of-sample prediction 
e1			<- phat.oos - y.out
tab.oos		<- data.frame(data=seldata, category=selcat, modelidx)
tab.oos$cor	<- apply(phat.oos, 2, function(x) cor(x, y.out))	
tab.oos$RMSE	<- apply(e1, 2, function(x) sqrt(mean(x^2)))
tab.oos$MAD  <- apply(e1, 2, function(x) median(abs(x)))
tab.oos$ROC 	<- tab.oos$sensitivity.mean <- tab.oos$specificity.mean <- NA

for(i in 1:ncol(phat)){
	tmp3	<- roc(y.out, phat.oos[,i])
	tab.oos[i,"ROC"]	<- tmp3$auc
	tab.oos[i,"sensitivity.mean"]	<- mean(tmp3$sensitivities, na.rm=T)
	tab.oos[i,"specificity.mean"]	<- mean(tmp3$specificities, na.rm=T)
}

ls.sgnb[[selm[2]]] 	<- tab.sgnb
ls.oos[[selm[2]]]	<- tab.oos
ls.x[[selm[2]]]		<- x

# Sign of coefficients 
tab.sgnb	<- cbind(modelidx, matrix(NA, nrow(modelidx), length(selm), dimnames=list(NULL, selm)) )
for(i in 1:length(selm)){
	nhh		<- ncol(ls.x[[i]])
	n.zero 	<- sum(colSums(ls.x[[i]])==0)
	tab.sgnb[,selm[i]] <- 1 - (ls.sgnb[[i]][,"X0"] - n.zero)/(nhh - n.zero)
}
sel 		<- sapply(1:ncol(tab.sgnb), function(i) is.numeric(tab.sgnb[,i]))
cat("The proportion of households with non-zero coefficients:\n"); print(cbind(tab.sgnb[,!sel], round(tab.sgnb[,sel], 4))); cat("\n")

# Out of sample prediction 
selcol 	<- c("cor", "RMSE","MAD","specificity.mean","sensitivity.mean", "ROC")
quartz()
par(mfrow = c(3, 2))
for(i in 1:length(selcol)){
	tmpr	<- range(c(ls.oos[[1]][,selcol[i]], ls.oos[[2]][,selcol[i]], oos.base[selcol[i]]), na.rm=T)
	plot(ls.oos[[1]][,selcol[i]], ls.oos[[2]][,selcol[i]], xlim=tmpr, ylim=tmpr, xlab=selm[1], ylab=selm[2], main=selcol[i]); 
	abline(v=oos.base[selcol[i]], col="blue")
	abline(a = 0, b=1, lty = 2)
}	



