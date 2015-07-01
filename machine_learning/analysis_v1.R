# Versions: 
# data: 		new product trial vs. new product usage
# category: 	all vs. individual category
# family:		linear vs. logit
# covariates:	no vs. yes
# shrinkage: ridge, elastic net, lasso, and gamma lasso
# tuning parameter: selected via cv

# Read in data:
set.seed(1111)
library(glmnet)
library(doParallel)
library(foreach)
library(caret)
library(Matrix)
library(gamlr)
library(pROC)

# setwd("~/Documents/Research/Harbinger/processed data/machine_learning_data")
# setwd("/home/brgordon/ccv103/Harbinger")
setwd("/kellogg/users/marketing/2661703/Harbinger")
options(error = quote({dump.frames(to.file = TRUE)}))

# Set parameters to determin the model version
# seldata	<- "trial"
# selcat	<- "all"
args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args)>0){
    for(i in 1:length(args)){
      eval(parse(text=args[[i]]))
    }
}

outfile <- paste("rs_", seldata,"_", selcat,".txt",sep="")
sfile	<- paste("rs_", seldata,"_", selcat,".rdata",sep="")
# sink(outfile)

##################
# Construct data #
##################
load(paste("harbinger_", seldata, "06.rdata",sep=""))
if(selcat!="all"){
	allcat	<- c("Bakery","Dairy","Deli","Edible","Frozen","General Merch","HBC", "Non-Edible")
	sel		<- which(allcat == selcat)
	dat1	<- subset(dat1, V2==sel)
}

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
x <- as.matrix(dat1[idxi,-(1:(x.st-1))])
dat2$num_ratio <- with(dat2, num_new_upc/num_all_upc)
sum(is.na(dat2))
dat2[is.na(dat2)] <- 0 

# z <- model.matrix(lm(y~FAMSIZE+INCOME+RACE+CHILD+M_STATUS+REGION+RENTOWN+month_exp+num_trip+num_ratio -1, data=dat2))
z <- model.matrix(lm(y~FAMSIZE+INCOME+RACE+CHILD+M_STATUS+REGION+RENTOWN+month_exp+num_trip+num_ratio, data=dat2))[,-1]
n.hh <- ncol(x)
xx <- cbind(x, x%*%z/n.hh)
if(selcat=="all"){
	tmp <- model.matrix(V4 ~ factor(V2) , data=dat1[idxi,])[,-1]
	xx	<- cbind(xx, tmp)
}
n.c <- ncol(xx) - n.hh

y.out <- dat1[idxo,y.col]
x.out <- as.matrix(dat1[idxo,-(1:(x.st-1))])
xx.out <- cbind(x.out, x.out%*%z/n.hh)
if(selcat=="all"){
	tmp <- model.matrix(V4 ~ factor(V2) , data=dat1[idxo,])[,-1]
	xx.out	<- cbind(xx.out, tmp)
}

# Clear unnecessary items:
colnames(x) <- colnames(xx) <- colnames(x.out) <- colnames(xx.out) <- NULL
dat1	<- Matrix(as.matrix(dat1), sparse = TRUE)

# Convert matrix to sparse matrix
x		<- Matrix(x, sparse=TRUE)
xx		<- Matrix(xx, sparse=TRUE)
x.out	<- Matrix(x.out, sparse=TRUE)
xx.out	<- Matrix(xx.out, sparse=TRUE)

# Check how sparse the model matrix is. 
cat("Number of products in classification =", length(y),"\n")
cat("Failure rate =", mean(y),"\n")
cat("Summary of number of products an individual purchased:\n"); 
print(summary(colSums(x))); cat("\n")
cat("Summary of relative ratio of individual purchase to the total number of products\n")
print(summary(colMeans(x))); cat("\n")

#############
# Fit model #
#############
# Register parallel computing
mycore 	<- 4
cl		<- makeCluster(mycore, type = "FORK")
registerDoParallel(cl)
cat("Register", mycore, "core parallel computing. \n")

# Set parameters for model 
mod			<- c("gaussian", "logit")
cova		<- c("no", "yes")
shrk		<- c("ridge", "elastic.net", "lasso", "gamma.lasso")
myalpha 	<- c(0, .5, 1)					# alpha parameter in glmnet
mygamma 	<- seq(.1, 2, .1)				# gamma parameter in gamlr
mynfold 	<- 10
pf 			<- rep(1:0, c(n.hh,n.c))		# Penalty factor in glmnet
pfree 		<- 1:n.c + n.hh					# Free parameters in gamlr

# ---------------------------------------------------# 
# Fit gaussian linear model 
cat("\n--------------------------------------------------------\n")
cat("Fit gaussian linear models.\n")

prc	<- proc.time()
cv0	<- foreach(a=myalpha) %do% {
	cv.glmnet(x=x, y=y, family="gaussian", alpha=a, standardize=FALSE, intercept=TRUE, nfolds=mynfold, parallel=TRUE)
}
cvc <- foreach(a = myalpha) %do% {
	cv.glmnet(x=xx, y=y, family="gaussian", alpha=a, standardize=FALSE, intercept=TRUE, nfolds=mynfold, penalty.factor=pf, parallel=TRUE)
}
use.time <- proc.time() - prc
cat("\n (Parallel) glmnet gaussian cross validation finishes using", use.time[3]/60,"min.\n")

prc <- proc.time()
m0 	<- foreach(i = 1:length(myalpha)) %dopar% {
	glmnet(x=x, y=y, family="gaussian", alpha=myalpha[i], standardize=FALSE, intercept=TRUE, lambda=cv0[[i]]$lambda.min)
} 
mc 	<- foreach(i = 1:length(myalpha)) %dopar% {
	glmnet(x=xx, y=y, family="gaussian", alpha=myalpha[i], standardize=FALSE, intercept=TRUE, lambda=cvc[[i]]$lambda.min, penalty.factor=pf)
}
names(m0) <- names(mc) <- shrk[1:3]
use.time <- proc.time() - prc
cat("\n (Parallel) glmnet finishes using", use.time[3]/60,"min.\n")

# Run gamma lasso 
prc	<- proc.time()
cvg0	<- foreach(g = mygamma) %do% {
	cv.gamlr(x=x, y=y, family="gaussian", gamma = g, standardize=FALSE, intercept=TRUE, nfolds=mynfold, parallel=TRUE)
}
cvgc <- foreach(g = mygamma) %do% {
	cv.gamlr(x=xx, y=y, family="gaussian", gamma = g, free=pfree, standardize=FALSE, intercept=TRUE, nfolds=mynfold, parallel=TRUE)
}
use.time <- proc.time() - prc
names(cvg0)	<- names(cvgc) <- paste("gamma_", mygamma, sep="")
cat("\n (Parallel) gamlr cross validation finishes using", use.time[3]/60,"min.\n")

sel 			<- which.min(sapply(cvg0, function(x) min(x$cvm)))
m0[[shrk[4]]]	<- cvg0[[sel]]
sel 			<- which.min(sapply(cvgc, function(x) min(x$cvm)))
mc[[shrk[4]]]	<- cvgc[[sel]]

# ---------------------------------------------------# 
# Fit logit model 
cat("\n--------------------------------------------------------\n")
cat("Fit logit models.\n")

prc	<- proc.time()
lcv0	<- foreach(a=myalpha) %do% {
	cv.glmnet(x=x, y=y, family="binomial", alpha=a, standardize=FALSE, intercept=TRUE, nfolds=mynfold, parallel=TRUE)
}
lcvc <- foreach(a = myalpha) %do% {
	cv.glmnet(x=xx, y=y, family="binomial", alpha=a, standardize=FALSE, intercept=TRUE, nfolds=mynfold, penalty.factor=pf, parallel=TRUE)
}
use.time <- proc.time() - prc
cat("\n (Parallel) glmnet binomial cross validation finishes using", use.time[3]/60,"min.\n")

prc <- proc.time()
lm0 	<- foreach(i = 1:length(myalpha)) %dopar% {
	glmnet(x=x, y=y, family="binomial", alpha=myalpha[i], standardize=FALSE, intercept=TRUE, lambda=lcv0[[i]]$lambda.min)
} 
lmc 	<- foreach(i = 1:length(myalpha)) %dopar% {
	glmnet(x=xx, y=y, family="binomial", alpha=myalpha[i], standardize=FALSE, intercept=TRUE, lambda=lcvc[[i]]$lambda.min, penalty.factor=pf)
}
names(lm0) <- names(lmc) <- shrk[1:3]
use.time <- proc.time() - prc
cat("\n (Parallel) glmnet finishes using", use.time[3]/60,"min.\n")

# Run gamma lasso 
prc	<- proc.time()
lcvg0	<- foreach(g = mygamma) %do% {
	cv.gamlr(x=x, y=y, family="binomial", gamma = g, standardize=FALSE, intercept=TRUE, nfolds=mynfold, parallel=TRUE)
}
lcvgc <- foreach(g = mygamma) %do% {
	cv.gamlr(x=xx, y=y, family="binomial", gamma = g, free=pfree, standardize=FALSE, intercept=TRUE, nfolds=mynfold, parallel=TRUE)
}
use.time <- proc.time() - prc
names(lcvg0)	<- names(lcvgc) <- paste("gamma_", mygamma, sep="")
cat("\n (Parallel) gamlr cross validation finishes using", use.time[3]/60,"min.\n")

sel 			<- which.min(sapply(lcvg0, function(x) min(x$cvm)))
lm0[[shrk[4]]]	<- lcvg0[[sel]]
sel 			<- which.min(sapply(lcvgc, function(x) min(x$cvm)))
lmc[[shrk[4]]]	<- lcvgc[[sel]]

##################################
# Compare linear vs. logit model #
##################################
cat("\n--------------------------------------------------------\n")
cat("Compare gaussian vs. logit models.\n")

b0	<- sapply(m0, function(x) as.vector(coef(x, select="min"))[-1])
bc	<- sapply(mc, function(x) as.vector(coef(x, select="min"))[2:(n.hh+1)])
lb0	<- sapply(lm0, function(x) as.vector(coef(x, select="min"))[-1])
lbc	<- sapply(lmc, function(x) as.vector(coef(x, select="min"))[2:(n.hh+1)])

tmp1	<- lapply(1:length(shrk), function(i) table(sign(b0[,i]), sign(lb0[,i])))
tmp2	<- lapply(1:length(shrk), function(i) table(sign(bc[,i]), sign(lbc[,i])))
cat("Coefficient signs from the two models w/o covariates (lambda=lambda.min):\n"); print(tmp1);cat("\n")
cat("Coefficient signs from the two models with covariates (lambda=lambda.min):\n"); print(tmp2);cat("\n")

tmp 	<- cbind(sapply(1:length(shrk), function(i) cor(b0[,i], lb0[,i], use="na.or.complete")), 
				 sapply(1:length(shrk), function(i) cor(bc[,i], lbc[,i], use="na.or.complete")) )
dimnames(tmp) <- list(shrk, covariates=cova)
cat("Correlation of coefficients from two models:\n"); print(tmp)

tmp		<- matrix(NA, length(shrk), length(cova), dimnames = list(shrk, covariates=cova))
for(i in 1:length(shrk)){
	tmp[i,1] <- cor(as.vector(predict(m0[[i]], x)), as.vector(predict(lm0[[i]], x, type="response")))
	tmp[i,2] <- cor(as.vector(predict(mc[[i]], xx)), as.vector(predict(lmc[[i]], xx, type="response")))
}
cat("Correlation of predicted values from two models:\n"); print(tmp)

############################
# Model fitness comparison # 
############################
cat("\n--------------------------------------------------------\n")
cat("Compute model fitness.\n")

matrix_expand <- function(mat, vec){
	n1	<- nrow(mat)
	n2	<- length(vec)
	sel	<- rep(1:n1, each=n2)
	mat_new <- cbind(mat[sel,], rep(vec, n1))
	return(mat_new)
}

modelidx		<- matrix_expand(as.matrix(mod, ncol=1), cova)
modelidx		<- matrix_expand(modelidx, shrk)
modelidx		<- data.frame(modelidx, index = 1:nrow(modelidx))
names(modelidx) <- c("model","covariates","shrinkage","index")
modnames		<- c("m0","mc","lm0","lmc")

# Sign of beta coefficients 
tmp1 	<- t(apply(b0, 2, function(x) table(factor(sign(x), levels=c(-1,0, 1)))))
tmp2 	<- t(apply(bc, 2, function(x) table(factor(sign(x), levels=c(-1,0, 1)))))
tmp3 	<- t(apply(lb0, 2, function(x) table(factor(sign(x), levels=c(-1,0, 1)))))
tmp4 	<- t(apply(lbc, 2, function(x) table(factor(sign(x), levels=c(-1,0, 1)))))
rownames(tmp1) <- rownames(tmp2) <- rownames(tmp3) <- rownames(tmp4) <- NULL
tab.sgnb<- data.frame(data=seldata, category=selcat, modelidx, rbind(tmp1, tmp2, tmp3, tmp4))
cat("The sign of beta:\n"); print(tab.sgnb); cat("\n")

# In-sample prediction 
tmp		<- c("x", "xx", "x", "xx")
tmp1	<- c("link", "link", "response", "response")
phat	<- matrix(NA, nrow(x), nrow(modelidx))
for(i in 1:length(modnames)){
	sel			<- 1:length(shrk) + length(shrk)*(i-1)
	tmp2		<- predict(get(modnames[i]), get(tmp[i]), select="min", type=tmp1[i])
	phat[,sel] 	<- sapply(tmp2, function(x) as.vector(x))
}
e1			<- phat - y
tab.in		<- data.frame(data=seldata, category=selcat, modelidx)
tab.in$cor	<- apply(phat, 2, function(x) cor(x, y))	
tab.in$RMSE	<- apply(e1, 2, function(x) sqrt(mean(x^2)))
tab.in$MAD  <- apply(e1, 2, function(x) median(abs(x)))
tab.in$ROC 	<- tab.in$sensitivity.mean <- tab.in$specificity.mean <- NA
for(i in 1:ncol(phat)){
	tmp3	<- roc(y, phat[,i])
	tab.in[i,"ROC"]	<- tmp3$auc
	tab.in[i,"sensitivity.mean"]	<- mean(tmp3$sensitivities, na.rm=T)
	tab.in[i,"specificity.mean"]	<- mean(tmp3$specificities, na.mr=T)
}
sel 		<- sapply(1:ncol(tab.in), function(i) is.numeric(tab.in[,i]))
cat("In sample prediction:\n"); print(cbind(tab.in[,!sel], round(tab.in[,sel], 4))); cat("\n")

# Out-of-sample prediction 
tmp		<- c("x.out", "xx.out", "x.out", "xx.out")
tmp1	<- c("link", "link", "response", "response")
phat.oos	<- matrix(NA, nrow(x.out), nrow(modelidx))
for(i in 1:length(modnames)){
	sel			<- 1:length(shrk) + length(shrk)*(i-1)
	tmp2		<- predict(get(modnames[i]), get(tmp[i]), select="min",type=tmp1[i])
	phat.oos[,sel] 	<- sapply(tmp2, function(x) as.vector(x))
}
e1			<- phat.oos - y.out
tab.oos		<- data.frame(data=seldata, category=selcat, modelidx)
tab.oos$cor	<- apply(phat.oos, 2, function(x) cor(x, y.out))	
tab.oos$RMSE	<- apply(e1, 2, function(x) sqrt(mean(x^2)))
tab.oos$MAD  <- apply(e1, 2, function(x) median(abs(x)))
tab.oos$ROC 	<- tab.oos$sensitivity.mean <- tab.oos$specificity.mean <- NA

pdf("graph_oos_roc.pdf",width=5, height=5)
for(i in 1:ncol(phat)){
	tmp3	<- roc(y.out, phat.oos[,i])
	tab.oos[i,"ROC"]	<- tmp3$auc
	tab.oos[i,"sensitivity.mean"]	<- mean(tmp3$sensitivities, na.rm=T)
	tab.oos[i,"specificity.mean"]	<- mean(tmp3$specificities, na.mr=T)
	print(plot(tmp3))
}
dev.off()
sel 		<- sapply(1:ncol(tab.oos), function(i) is.numeric(tab.oos[,i]))
cat("Out of sample prediction:\n"); print(cbind(tab.oos[,!sel], round(tab.oos[,sel], 4))); cat("\n")


# Export the tables to excels 
excf_name1	<- "table_beta.csv"
excf_name2 	<- "fit_in.csv"
excf_name3 	<- "fit_oos.csv"
if(!excf_name1 %in% list.files()){
	f1			<- file(excf_name1, "w")
	f2			<- file(excf_name2, "w")
	f3			<- file(excf_name3, "w")
	writeLines("Table of beta sign\n", f1)
	writeLines("Table of in-sample model fitness\n", f2)
	writeLines("Table of out-of-sample model fitness\n", f3)
	close(f1)
	close(f2)
	close(f3)
}

f1			<- file(excf_name1, "at")
f2			<- file(excf_name2, "at")
f3			<- file(excf_name3, "at")
write.csv(tab.sgnb, f1)
writeLines("\n", f1)
write.csv(tab.in, f2)
writeLines("\n", f2)
write.csv(tab.oos, f3)
writeLines("\n", f3)
close(f1)
close(f2)
close(f3)

# sink()
stopCluster(cl)
save.image(sfile)
cat("This program is done. \n")

