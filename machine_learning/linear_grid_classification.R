# Read in data:
set.seed(1111)
library(doParallel)
library(foreach)
library(caret)
library(Matrix)
library(pROC)
library(ggplot2)

# setwd("~/Documents/Research/Harbinger/processed data/machine_learning_data")
setwd("/kellogg/users/marketing/2661703/Harbinger")
options(error = quote({dump.frames(to.file = TRUE)}))

# Set parameters to determin the model version
seldata		<- "trial"
cls_met		<- "affinity"
# clsv_met	<- "n_fail"
make_plot	<- FALSE

args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args)>0){
    for(i in 1:length(args)){
      eval(parse(text=args[[i]]))
    }
}

outfile 	<- paste("classification_line_",clsv_met,".rdata", sep="")

#############
# Functions #
#############
class_fn <- function(hh_dat, fml, plot=FALSE){
	out	<- hh_dat
	out$score	<- with(hh_dat, eval(parse(text=fml)))
	out$grp		<- ifelse(out$score<=0, 1, 2)
	if(any(table(out$grp) <= 1) | length(table(out$grp)) == 1){
		return(NULL)
	}
	if(plot){
		print(ggplot(out, aes(affinity, n_fail, col=factor(grp))) + geom_point(position = 'jitter') )
	}
	return(out)
}

class2_fn <- function(hh_dat, fml1, fml2, plot=FALSE){
	out	<- hh_dat
	score1		<- with(hh_dat, eval(parse(text=fml1)))
	score2		<- with(hh_dat, eval(parse(text=fml2)))
	grp1		<- ifelse(score1<=0, 1, 2)
	grp2		<- ifelse(score2<=0, 1, 2)
	out$grp		<- 2 * (grp1 - 1) + grp2
	if(any(table(out$grp) <= 1) | length(table(out$grp)) < 3){
		return(NULL)
	}
	if(plot){
		print(ggplot(out, aes(affinity, n_fail, col=factor(grp))) + geom_point(position = 'jitter') )
	}
	return(out)
}

pred_fn <- function(y.out, x.out, grp, stde=FALSE){
	tmp			<- sort(unique(grp))
	grp_idx 	<- lapply(tmp, function(x) which(grp==x))
	x.out.grp	<- sapply(grp_idx, function(sel) rowSums(x.out[,sel]))
	colnames(x.out.grp) <- tmp
	lmg			<- glm(y.out ~ x.out.grp, family="binomial")
	
	# Compute model fitness
	phat		<- predict(lmg, newdata = data.frame(x.out.grp), type = "response")
	eps			<- y.out - phat
	auc			<- roc(y.out, phat)$auc
	rmse		<- sqrt(mean(eps^2))
	fitness		<- matrix(c(rmse, auc, lmg$aic), nrow=1)
	colnames(fitness) <- c("RMSE","AUC","AIC")
	if(stde){
		return(list(coef = coef(lmg), estimate = summary(lmg)$coefficients, fitness = fitness))
	}else{
		return(list(coef = coef(lmg), fitness = fitness))
	}	
}

pred_wrapper <- function(hh_dat, fml1, y.out, x.out, fml2 = NULL, plot=FALSE){
	if(is.null(fml2)){
		mygrp		<- class_fn(hh_dat, fml1, plot)
	}else{
		mygrp		<- class2_fn(hh_dat, fml1, fml2, plot)
	}
	
	if(is.null(mygrp)){
		return(NULL)
	}else{
		mypred		<- pred_fn(y.out, x.out, mygrp$grp)
		# out 		<- matrix(c(mypred$coef, mypred$fitness), nrow=1)
		# colnames(out) 	<- c(names(mypred$coef), colnames(mypred$fitness))
		out			<- mypred$fitness
		return(out)
	}
}

##################
# Construct data #
##################
load(paste("harbinger_", seldata, ".rdata",sep=""))

# Convert category variables into numeric 
category 	<- sort(unique(dat1$V3))
category_idx<- 1:length(category)
names(category_idx) <- category
tmp			<- dat1$V3
dat1$V3		<- NA
dat1$V3		<- category_idx[as.character(tmp)]

dat2$y 	<- rnorm(nrow(dat2))
y.col	<- 4						# The column number of failure variable
x.st	<- 6						# The first column number of household purchase data

# Consider only the first p households for convenience:
# p <- 250
# dat1 <- dat1[,1:(p+x.st-1)]
# dat2 <- dat2[1:p,]

# Get in and out of sample data indices:
idxi <- sample(1:nrow(dat1), round(0.80*nrow(dat1)))
idxo <- setdiff(1:nrow(dat1), idxi)
ni <- length(idxi)
no <- length(idxo)

# Create y, x, xx matrices:
y <- dat1[idxi,y.col]
# x <- as.matrix(dat1[idxi,-(1:(x.st-1))])
x <- dat1.x[idxi,]
dat2$num_ratio <- with(dat2, num_new_upc/num_all_upc)
sum(is.na(dat2))
dat2[is.na(dat2)] <- 0 

# z <- model.matrix(lm(y~FAMSIZE+INCOME+RACE+CHILD+M_STATUS+REGION+RENTOWN+month_exp+num_trip+num_ratio -1, data=dat2))
z <- model.matrix(lm(y~FAMSIZE+INCOME+RACE+CHILD+M_STATUS+REGION+RENTOWN+month_exp+num_trip+num_ratio, data=dat2))[,-1]
n.hh <- ncol(x)
xx <- cbind(x, x%*%z/n.hh)
n.c <- ncol(xx) - n.hh

y.out <- dat1[idxo,y.col]
# x.out <- as.matrix(dat1[idxo,-(1:(x.st-1))])
x.out	<- dat1.x[idxo,]
xx.out <- cbind(x.out, x.out%*%z/n.hh)

# Clear unnecessary items:
colnames(x) <- colnames(xx) <- colnames(x.out) <- colnames(xx.out) <- NULL
# dat1	<- Matrix(as.matrix(dat1), sparse = TRUE)
dat1	<- NULL

# Convert matrix to sparse matrix
# x		<- Matrix(x, sparse=TRUE)
xx		<- Matrix(xx, sparse=TRUE)
# x.out	<- Matrix(x.out, sparse=TRUE)
xx.out	<- Matrix(xx.out, sparse=TRUE)

# Check how sparse the model matrix is. 
cat("Number of products in classification =", length(y),"\n")
cat("Failure rate =", mean(y),"\n")
cat("Summary of number of products an individual purchased:\n"); 
print(summary(colSums(x))); cat("\n")
cat("Summary of relative ratio of individual purchase to the total number of products\n")
print(summary(colMeans(x))); cat("\n")

# Construct housheold data 
hh_dat 	<- data.frame(n_fail = colSums(y*x), n_new = colSums(x))
hh_dat$affinity <- with(hh_dat, n_fail/n_new)

if(make_plot){
	ggplot(hh_dat, aes(affinity, n_fail)) + geom_point(position="jitter")
	ggplot(hh_dat, aes(affinity, n_new)) + geom_point(position="jitter")
}

#######################################
# Grid search for classification line #
#######################################
# Baseline
tmp 	<- median(hh_dat$affinity, na.rm=T)
myfml	<- paste("affinity - ", tmp, sep="")
p.base	<- pred_wrapper(hh_dat, myfml, y.out, x.out)

# n_fail ~ affinity 
# Line: n_fail - slope*affinity - intercept 
range(hh_dat[,clsv_met])
metv.min	<- 0 
metv.max	<- quantile(hh_dat[,clsv_met], .95, na.rm=T)
metv.out	<- 10
inter.step 	<- 1
slope.step	<- 2
inter.seq 	<- seq(metv.min - metv.out, metv.max + metv.out, inter.step)
med.af		<- median(hh_dat[,cls_met], na.rm=T)

# Register parallel computing
mycore 	<- 3
cl		<- makeCluster(mycore, type = "FORK")
registerDoParallel(cl)
cat("Register", mycore, "core parallel computing. \n")

fitout <- foreach(i = 1:length(inter.seq), .combine='rbind') %dopar% {
	fit.out 	<- data.frame(NULL)
	slope.range <- c(metv.min - inter.seq[i], (metv.min - inter.seq[i]) / med.af, 
					 metv.max - inter.seq[i], (metv.max - inter.seq[i]) / med.af)
    slope.seq 	<- seq(min(slope.range), max(slope.range), slope.step)
	for(j in 1:length(slope.seq)){
		myfml	<- paste(clsv_met, ifelse(sign(slope.seq[j]), "-", "+"), abs(slope.seq[j]), "*", cls_met, 
								   ifelse(sign(inter.seq[i]), "-", "+"), abs(inter.seq[i]))
		tmp		<- pred_wrapper(hh_dat, myfml, y.out, x.out)
		if(!is.null(tmp)){
			fit.out	<- rbind(fit.out, cbind(intercept = inter.seq[i], slope = slope.seq[j], tmp))
		}
	}
	fit.out
}

# Try vertical lines for classificaiton
vinter.step	<- .05
vinter.seq 	<- seq(min(hh_dat[,cls_met], na.rm=T), max(hh_dat[,cls_met], na.rm=T), by = vinter.step)
for(j in 1:length(vinter.seq)){
	myfml	<- paste(cls_met, "-", vinter.seq[j], sep="")
	tmp		<- pred_wrapper(hh_dat, myfml, y.out, x.out)
	if(!is.null(tmp)){
		fitout	<- rbind(fitout, cbind(intercept = vinter.seq[j], slope = Inf, tmp))
	}
}

stopCluster(cl)

# Find the line with the greatest AUC
cat("Fitness of baseline model:\n"); print(p.base); cat("\n")
cat("Maximium AUC =", max(fitout$AUC, na.rm=T))
sel 	<- which(fitout$AUC == max(fitout$AUC, na.rm=T) )
cat("The coresponding lines are:\n")
head(fitout[sel,1:2])

# Keep the formular of line 1
if(length(sel) > 1){
	sel <- sel[1]
}
line1.fml	<- ifelse(fitout[sel,"slope"]==Inf, paste(cls_met, "-", fitout[sel,"intercept"], sep=""), 
				paste(clsv_met, ifelse(sign(fitout[sel,"slope"]), "-", "+"), abs(fitout[sel,"slope"]), "*", cls_met, 
										   ifelse(sign(fitout[sel,"intercept"]), "-", "+"), abs(fitout[sel,"intercept"]))	)

#########################################
# Segment the households with two lines # 
#########################################
# Register parallel computing
mycore 	<- 3
cl		<- makeCluster(mycore, type = "FORK")
registerDoParallel(cl)
cat("Register", mycore, "core parallel computing. \n")

fitout2 <- foreach(i = 1:length(inter.seq), .combine='rbind') %dopar% {
	fit.out2 	<- data.frame(NULL)
	slope.range <- c(metv.min - inter.seq[i], (metv.min - inter.seq[i]) / med.af, 
					 metv.max - inter.seq[i], (metv.max - inter.seq[i]) / med.af)
    slope.seq 	<- seq(min(slope.range), max(slope.range), slope.step)
	for(j in 1:length(slope.seq)){
		myfml	<- paste(clsv_met, ifelse(sign(slope.seq[j]), "-", "+"), abs(slope.seq[j]), "*", cls_met, 
								   ifelse(sign(inter.seq[i]), "-", "+"), abs(inter.seq[i]))
		tmp		<- pred_wrapper(hh_dat, line1.fml, y.out, x.out, myfml)
		if(!is.null(tmp)){
			fit.out2	<- rbind(fit.out2, cbind(intercept = inter.seq[i], slope = slope.seq[j], tmp))
		}
	}
	fit.out2
}

# Try vertical lines for classificaiton
vinter.step	<- .05
vinter.seq 	<- seq(min(hh_dat[,cls_met], na.rm=T), max(hh_dat[,cls_met], na.rm=T), by = vinter.step)
for(j in 1:length(vinter.seq)){
	myfml	<- paste(cls_met, "-", vinter.seq[j], sep="")
	tmp		<- pred_wrapper(hh_dat, line1.fml, y.out, x.out, myfml)
	if(!is.null(tmp)){
		fitout2	<- rbind(fitout2, cbind(intercept = vinter.seq[j], slope = Inf, tmp))
	}
}

stopCluster(cl)

# Find the line with the greatest AUC
cat("Fitness of baseline model:\n"); print(p.base); cat("\n")
cat("Maximium AUC =", max(fitout2$AUC, na.rm=T))
sel 	<- which(fitout2$AUC == max(fitout2$AUC, na.rm=T) )
cat("The coresponding lines are:\n")
head(fitout2[sel,1:2])

# Keep the formular of line 1
if(length(sel) > 1){
	sel <- sel[1]
}
line2.fml	<- ifelse(fitout2[sel,"slope"]==Inf, paste(cls_met, "-", fitout2[sel,"intercept"], sep=""), 
				paste(clsv_met, ifelse(sign(fitout2[sel,"slope"]), "-", "+"), abs(fitout2[sel,"slope"]), "*", cls_met, 
										   ifelse(sign(fitout2[sel,"intercept"]), "-", "+"), abs(fitout2[sel,"intercept"]))	)
cat("Line 1 is", line1.fml,"\n")
cat("Line 2 is", line2.fml,"\n")

# Plot the classfication results
if(make_plot){
	tmp <- class2_fn(hh_dat, line1.fml, line2.fml, plot=TRUE)
}

save.image(file = outfile)

cat("This program is done. \n")




