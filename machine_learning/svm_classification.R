# Read in data:
set.seed(1111)
library(glmnet)
library(doParallel)
library(foreach)
library(caret)
library(Matrix)
library(gamlr)
library(pROC)
library(kernlab)
library(e1071)

# setwd("~/Documents/Research/Harbinger/processed data/machine_learning_data")
source('~/Documents/Research/Harbinger/Exercise/outreg function.R')
setwd("/kellogg/users/marketing/2661703/Harbinger")
options(error = quote({dump.frames(to.file = TRUE)}))

# Set parameters to determin the model version
seldata		<- "trial"
cls_met		<- "affinity"
clsv_met	<- "n_fail"
make_plot	<- TRUE
plot.wd		<- getwd()
out.file	<- "harbinger_svm.rdata"

args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args)>0){
    for(i in 1:length(args)){
      eval(parse(text=args[[i]]))
    }
}

#############
# Functions #
#############
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

fitness_fn <- function(y.out, phat){
	eps			<- y.out - phat
	auc			<- roc(y.out, phat)$auc
	rmse		<- sqrt(mean(eps^2))
	fitness		<- matrix(c(rmse, auc), nrow=1)
	colnames(fitness) <- c("RMSE","AUC")
	return(fitness)
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
x	<- dat1.x[idxi,]
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

##############################
# Two-segment classification # 
##############################
cat("-----------------------------------------------\n")
cat("Now fit two-segment classification.\n")

# ---------------------------------------------------# 
# Fit gaussian linear model
# Register parallel computing
mycore 	<- 2
cl		<- makeCluster(mycore, type = "FORK")
registerDoParallel(cl)
cat("Register", mycore, "core parallel computing. \n")

prc		<- proc.time()
mynfold	<- 10	
cv0		<- cv.glmnet(x=x, y=y, family="gaussian", alpha=0, standardize=FALSE, intercept=TRUE, nfolds=mynfold, parallel=TRUE)
use.time<- proc.time() - prc
cat("\n (Parallel) glmnet gaussian cross validation finishes using", use.time[3]/60,"min.\n")

m0 		<- glmnet(x=x, y=y, family="gaussian", alpha=0, standardize=FALSE, intercept=TRUE, lambda=cv0$lambda.min)
grp_datr<- hh_dat
grp_datr$beta <- coef(m0)[-1]

# Explore relationship between household affinity 
cat("cor(beta, affinity) =", cor(grp_datr$beta, grp_datr$affinity))
cat("cor(beta, n_fail) =", cor(grp_datr$beta, grp_datr$n_fail))
cat("cor(beta, n_new) =", cor(grp_datr$beta, grp_datr$n_new))

# ---------------------------------------------------# 
# Find SVM for segment classifier
grp_datr$harbinger 	<- ifelse(grp_datr$beta > 0, 1, 0)
grp_datr$harbinger	<- factor(grp_datr$harbinger)
my_C	<- 1000000
prc		<- proc.time()
myfml	<- list(as.formula(harbinger ~ n_fail + affinity), as.formula(harbinger ~ n_new + affinity))
myfit	<- foreach(i = 1:length(myfml)) %dopar%{
	ksvm(myfml[[i]], data = grp_datr, type = "C-svc", kernel = "rbfdot", C = my_C, prob.model = F)
}
fit1.rk		<- myfit[[1]]
fit2.rk		<- myfit[[2]]
use.time<- proc.time() - prc
cat("KSVM finishes with", use.time[3]/60, "min.\n")
print(fit1.rk)
print(fit2.rk)

# Use svm 
prc		<- proc.time()
myfit	<- foreach(i = 1:length(myfml)) %dopar%{
	svm(myfml[[i]], data = grp_datr, type ="C-classification", cost = my_C)
}
fit1.r		<- myfit[[1]]
fit2.r		<- myfit[[2]]
use.time<- proc.time() - prc
cat("SVM finishes with", use.time[3]/60, "min.\n")
print(fit1.r)
print(fit2.r)

if(make_plot){
	pdf(paste(plot.wd,"/graph_seg2.pdf",sep=""), width = 6.5, height = 6.5)
	print(ggplot(grp_datr, aes(affinity, n_fail, col=harbinger)) + geom_point(position = "jitter") ) 
	print(ggplot(grp_datr, aes(affinity, n_new, col=harbinger)) + geom_point(position = "jitter") ) 
	# print(plot(fit1.rk, data=grp_datr[, c(1,3)]))
	# print(plot(fit2.rk, data=grp_datr[, c(2,3)]))
	print(plot(fit1.r, grp_datr, n_fail ~ affinity))
	print(plot(fit2.r, grp_datr, n_new ~ affinity))
	dev.off()
}

################################
# Three segment classification # 
################################
# ---------------------------------------------------# 
# Fit gaussian linear model
cat("-----------------------------------------------\n")
cat("Now fit three-segment classification.\n")

prc		<- proc.time()
mynfold	<- 10	
cve		<- cv.glmnet(x=x, y=y, family="gaussian", alpha=0.5, standardize=FALSE, intercept=TRUE, nfolds=mynfold, parallel=TRUE)
use.time<- proc.time() - prc
cat("\n (Parallel) glmnet gaussian cross validation finishes using", use.time[3]/60,"min.\n")

mel		<- glmnet(x=x, y=y, family="gaussian", alpha=0.5, standardize=FALSE, intercept=TRUE, lambda=cve$lambda.min)
grp_dat_el <- hh_dat
grp_dat_el$beta <- coef(mel)[-1]

# Explore relationship between household affinity 
cat("Table of coefficient signs from gaussian model (elastic net):\n"); print(table(sign(grp_dat_el$beta))); cat("\n")
cat("cor(beta, affinity) =", cor(grp_dat_el$beta, grp_dat_el$affinity))
cat("cor(beta, n_fail) =", cor(grp_dat_el$beta, grp_dat_el$n_fail))
cat("cor(beta, n_new) =", cor(grp_dat_el$beta, grp_dat_el$n_new))

grp_dat_el$harbinger <- sign(grp_dat_el$beta)
grp_dat_el$harbinger <- factor(grp_dat_el$harbinger)

# ---------------------------------------------------# 
# Find SVM for segment classifier
my_C		<- 1000000
# prc			<- proc.time()
# fit1.elk	<- ksvm(harbinger ~ n_fail + affinity, data = grp_dat_el, type = "spoc-svc", kernel = "rbfdot", C = my_C, prob.model = F)
# fit2.elk	<- ksvm(harbinger ~ n_new + affinity, data = grp_dat_el, type = "spoc-svc", kernel = "rbfdot", C = my_C, prob.model = F)
# use.time<- proc.time() - prc
# cat("KSVM finishes with", use.time[3]/60, "min.\n")
# print(fit1.elk)
# print(fit2.elk)

# Use svm 
prc		<- proc.time()
myfit	<- foreach(i = 1:length(myfml)) %dopar%{
	svm(myfml[[i]], data = grp_dat_el, type ="C-classification", cost = my_C)
}
fit1.el		<- myfit[[1]]
fit2.el		<- myfit[[2]]
# fit1.el 	<- svm(harbinger ~ n_fail + affinity , data = grp_dat_el, type ="C-classification", cost = my_C)
# fit2.el 	<- svm(harbinger ~ n_new + affinity, data = grp_dat_el, type = "C-classification", cost = my_C)
use.time<- proc.time() - prc
cat("SVM finishes with", use.time[3]/60, "min.\n")
print(fit1.el)
print(fit2.el)

if(make_plot){
	pdf(paste(plot.wd,"/graph_seg3.pdf",sep=""), width = 6.5, height = 6.5)
	print(ggplot(grp_dat_el, aes(affinity, n_fail, col=harbinger)) + geom_point(position = "jitter") ) 
	print(ggplot(grp_dat_el, aes(affinity, n_new, col=harbinger)) + geom_point(position = "jitter") ) 
	# print(plot(fit1.elk, data=grp_dat_el[, c(1,3)]))
	# print(plot(fit2.elk, data=grp_dat_el[, c(2,3)]))
	print(plot(fit1.el, grp_dat_el, n_fail ~ affinity, dataSymbol=NA, svSymbol = NA))
	print(plot(fit2.el, grp_dat_el, n_new ~ affinity, dataSymbol=NA, svSymbol = NA))
	dev.off()
}

stopCluster(cl)

##############
# Comparison # 
##############
# Baseline model: 4 groups classified by quantile of affinity index 
quantile(hh_dat$affinity, c(0:4)/4, na.rm=T)
grp.base	<- as.numeric(cut(hh_dat$affinity, quantile(hh_dat$affinity, c(0:4)/4, na.rm=T), include.lowest = T, labels=1:4))
grp.base[is.na(grp.base)] <- 0
fit.base	<- pred_fn(y.out, x.out, grp.base, TRUE)

# 2-segment model fit 
phat.r	<- predict(m0, newx = x.out, type = "response")
fit.seg2<- pred_fn(y.out, x.out, grp_datr$harbinger, TRUE)

# 3-segment model fit
phat.el	<- predict(mel, newx = x.out, type = "response")
fit.seg3<- pred_fn(y.out, x.out, grp_dat_el$harbinger, TRUE)

# Coefficients: 
cat("Coefficient estimates from the baseline model:\n"); print(fit.base$estimate); cat("\n")
cat("Coefficient estimates from the 2-segment model(using sign(beta) from ridge regression):\n"); print(fit.seg2$estimate); cat("\n")
cat("Coefficient estimates from the 3-segment model(using sign(beta) from elastic net regression):\n"); print(fit.seg3$estimate); cat("\n")

# Model fitness
sel <- c("RMSE", "AUC")
tmp	<- rbind(baseline = fit.base$fitness[,sel], class_2seg = fit.seg2$fitness[,sel], class_3seg = fit.seg3$fitness[,sel], 
			ridge = fitness_fn(y.out, phat.r), el = fitness_fn(y.out, phat.el))
rownames(tmp)	<- c("baseline", "class_2seg","class_3seg", "ridge","elastic net")
cat("Out-of-sample prediction:\n"); print(tmp); cat("\n")

# Linear regression of coefficients
mydata <- data.frame(grp_datr, dat2[,c("FAMSIZE","INCOME","CHILD", "month_exp","num_trip")])
tmp1 	<- lm(beta ~ n_new + affinity + log(month_exp) + log(num_trip)+ FAMSIZE + INCOME + CHILD , data = mydata)
tmp2 	<- lm(abs(beta) ~ n_new + affinity + log(month_exp) + log(num_trip)+ FAMSIZE + INCOME + CHILD , data = mydata)
model_list	<- list(beta = summary(tmp1)$coefficients, abs_beta = summary(tmp2)$coefficients)
tmp.tab	<- model_outreg(model_list, p.given = TRUE, digit = 5, head.name = c("Estimate", "Std. Error", "Pr(>|t|)"))


save.image(file = out.file)
cat("This program is done.\n")
