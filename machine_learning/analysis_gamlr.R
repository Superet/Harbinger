# Read in data:
set.seed(1111)
library(glmnet)
library(doParallel)
library(foreach)
library(caret)
library(Matrix)
library(gamlr)

# setwd("~/Dropbox/Dropbox Research/Blake_Chaoqun/Harbingers")
# setwd("/home/brgordon/ccv103/Harbinger")
setwd("/kellogg/users/marketing/2661703/Harbinger")
options(error = quote({dump.frames(to.file = TRUE)}))
outfile <- "harbinger_gamlr_output.txt"
sink(outfile)

load("harbinger06.rdata")
dat2$y <- rnorm(nrow(dat2))

# Consider only the first p households for convenience:
# p <- 250
# dat1 <- dat1[,1:(p+3)]
# dat2 <- dat2[1:p,]

# Get in and out of sample data indices:
idxi <- sample(1:nrow(dat1), round(0.80*nrow(dat1)))
idxo <- setdiff(1:nrow(dat1), idxi)
ni <- length(idxi)
no <- length(idxo)

# Create y, x, xx matrices:
y <- dat1[idxi,2]
x <- as.matrix(dat1[idxi,-(1:3)])
z <- model.matrix(lm(y~FAMSIZE+INCOME+RACE+CHILD+M_STATUS+REGION+RENTOWN-1, data=dat2))
n.hh <- ncol(x)
xx <- cbind(x, x%*%z/n.hh)
n.c <- ncol(xx) - n.hh

y.out <- dat1[idxo,2]
x.out <- as.matrix(dat1[idxo,-(1:3)])
xx.out <- cbind(x.out, x.out%*%z/n.hh)

# Clear unnecessary items:
colnames(x) <- colnames(xx) <- colnames(x.out) <- colnames(xx.out) <- NULL
dat1	<- Matrix(as.matrix(dat1), sparse = TRUE)

# Convert matrix to sparse matrix
x		<- Matrix(x, sparse=TRUE)
xx		<- Matrix(xx, sparse=TRUE)
x.out	<- Matrix(x.out, sparse=TRUE)
xx.out	<- Matrix(xx.out, sparse=TRUE)

# Register parallel computing
mycore 	<- 4
cl		<- makeCluster(mycore, type = "FORK")
registerDoParallel(cl)
cat("Register", mycore, "core parallel computing. \n")

mygamma <- seq(0, 20, by=2)
mylambdav <- c("lambda.min", "lambda.1se")
pf 	<- rep(1:0, c(n.hh,n.c))
pfree <- 1:n.c + n.hh

prc	<- proc.time()
cv0	<- foreach(g = mygamma) %do% {
	cv.gamlr(x=x, y=y, family="gaussian", gamma = g, standardize=FALSE, intercept=TRUE, nfolds=10, parallel=TRUE)
}
cvc <- foreach(g = mygamma) %do% {
	cv.gamlr(x=xx, y=y, family="gaussian", gamma = g, free=pfree, standardize=FALSE, intercept=TRUE, nfolds=10, parallel=TRUE)
}
use.time <- proc.time() - prc
names(cv0)	<- names(cvc) <- paste("gamma_", mygamma, sep="")
cat("\n (Parallel) Cross validation finishes using", use.time[3]/60,"min.\n")

# Check the cofficient signs
tmp	<- sapply(cv0, function(x) sign(coef(x, select="min")[-1]))
cat("Table of coefficient signs from the model w/o covariates (lambda=lambda.min):\n")
print(apply(tmp, 2, table))

tmp	<- sapply(cv0, function(x) sign(coef(x,, select="1se")[-1]))
cat("Table of coefficient signs from the model w/o covariates (lambda=lambda.1se):\n")
print(apply(tmp, 2, table))

tmp	<- sapply(cvc, function(x) sign(coef(x, select="min")[-1]))
cat("Table of coefficient signs from the model wtih covariates (lambda=lambda.min):\n")
print(apply(tmp, 2, table))

tmp	<- sapply(cvc, function(x) sign(coef(x,select="1se")[-1]))
cat("Table of coefficient signs from the model with covariates (lambda=lambda.1se):\n")
print(apply(tmp, 2, table))

# Compute the correlation between y and yhat 
tmp	<- sapply(cv0, function(m) cor(as.vector(predict(m, newdata=x, select="min")), y))
cat("Correlation of predicted values from models w/o covariates and y (lambda=lambda.min):\n"); print(tmp)

tmp	<- sapply(cv0, function(m) cor(as.vector(predict(m, newdata=x, select="1se")), y))
cat("Correlation of predicted values from models w/o covariates and y (lambda=lambda.1se):\n"); print(tmp)

tmp	<- sapply(cvc, function(m) cor(as.vector(predict(m, newdata=xx, select="min")), y))
cat("Correlation of predicted values from models wtih covariates and y (lambda=lambda.min):\n"); print(tmp)

tmp	<- sapply(cvc, function(m) cor(as.vector(predict(m, newdata=xx, select="1se")), y))
cat("Correlation of predicted values from models with covariates and y (lambda=lambda.1se):\n"); print(tmp)

#######################################
# Fit binomial (i.e, logistic) models #
#######################################
cat("\n--------------------------------------------------------\n")
cat("Fit logit models.\n")
prc	<- proc.time()
lcv0	<- foreach(g = mygamma) %do% {
	cv.gamlr(x=x, y=y, family="binomial", gamma = g, standardize=FALSE, intercept=TRUE, nfolds=10, parallel=TRUE)
}
lcvc <- foreach(g = mygamma) %do% {
	cv.gamlr(x=xx, y=y, family="binomial", gamma = g, free=pfree, standardize=FALSE, intercept=TRUE, nfolds=10, parallel=TRUE)
}
use.time <- proc.time() - prc
names(lcv0)	<- names(lcvc) <- paste("gamma_", mygamma, sep="")
cat("\n (Parallel) Cross validation of logit models finishes using", use.time[3]/60,"min.\n")

# Check the cofficient signs
tmp	<- sapply(lcv0, function(x) sign(coef(x, select="min")[-1]))
cat("Table of coefficient signs from the model w/o covariates (lambda=lambda.min):\n")
print(apply(tmp, 2, table))

tmp	<- sapply(lcv0, function(x) sign(coef(x, select="1se")[-1]))
cat("Table of coefficient signs from the model w/o covariates (lambda=lambda.1se):\n")
print(apply(tmp, 2, table))

tmp	<- sapply(lcvc, function(x) sign(coef(x, select="min")[-1]))
cat("Table of coefficient signs from the model wtih covariates (lambda=lambda.min):\n")
print(apply(tmp, 2, table))

tmp	<- sapply(lcvc, function(x) sign(coef(x, select="1se")[-1]))
cat("Table of coefficient signs from the model with covariates (lambda=lambda.1se):\n")
print(apply(tmp, 2, table))

# Compute the correlation between y and yhat 
tmp	<- sapply(lcv0, function(m) cor(as.vector(predict(m, newdata=x, select="min", type="response")), y))
cat("Correlation of predicted values from models w/o covariates and y (lambda=lambda.min):\n"); print(tmp)

tmp	<- sapply(lcv0, function(m) cor(as.vector(predict(m, newdata=x, select="1se", type="response")), y))
cat("Correlation of predicted values from models w/o covariates and y (lambda=lambda.1se):\n"); print(tmp)

tmp	<- sapply(lcvc, function(m) cor(as.vector(predict(m, newdata=xx, select="min", type="response")), y))
cat("Correlation of predicted values from models wtih covariates and y (lambda=lambda.min):\n"); print(tmp)

tmp	<- sapply(lcvc, function(m) cor(as.vector(predict(m, newdata=xx, select="1se", type="response")), y))
cat("Correlation of predicted values from models with covariates and y (lambda=lambda.1se):\n"); print(tmp)

##########################################
# Compare logit model and gaussian model #
##########################################
cat("\n--------------------------------------------------------\n")
cat("Compare gaussian models to logit models.\n")

tmp1 <- lapply(1:length(mygamma), function(i) table(gaussian=sign(coef(cv0[[i]], select="min")[-1]), 
													logit=sign(coef(lcv0[[i]], select="min")[-1])))
tmp2 <- lapply(1:length(mygamma), function(i) table(gaussian=sign(coef(cv0[[i]], select="1se")[-1]), 
													logit=sign(coef(lcv0[[i]], select="1se")[-1])))
tmp3 <- lapply(1:length(mygamma), function(i) table(gaussian=sign(coef(cvc[[i]], select="min")[-1]), 
													logit=sign(coef(lcvc[[i]], select="min")[-1])))
tmp4 <- lapply(1:length(mygamma), function(i) table(gaussian=sign(coef(cvc[[i]], select="1se")[-1]), 
													logit=sign(coef(lcvc[[i]], select="1se")[-1])))
names(tmp1) <- names(tmp2) <- names(tmp3) <- names(tmp4) <- paste("gamma_", mygamma, sep="")
cat("Coefficient signs from the two models w/o covariates (lambda=lambda.min):\n"); print(tmp1);cat("\n")
cat("Coefficient signs from the two models w/o covariates (lambda=lambda.1se):\n"); print(tmp2);cat("\n")
cat("Coefficient signs from the two models with covariates (lambda=lambda.min):\n"); print(tmp3);cat("\n")
cat("Coefficient signs from the two models with covariates (lambda=lambda.1se):\n"); print(tmp4);cat("\n")

tmp	<- NULL
for(i in 1:length(mygamma)){
	tmp1 <- cor(coef(cv0[[i]], select="min")[-1], coef(lcv0[[i]], select="min")[-1]) 
	tmp2 <- cor(coef(cv0[[i]], select="min")[-1], coef(lcv0[[i]], select="1se")[-1]) 
	tmp3 <- cor(coef(cvc[[i]], select="min")[-1], coef(lcvc[[i]], select="min")[-1]) 
	tmp4 <- cor(coef(cvc[[i]], select="min")[-1], coef(lcvc[[i]], select="1se")[-1])
	tmp  <- rbind(tmp, c(tmp1, tmp2, tmp3, tmp4))
}
colnames(tmp) <- c("No cov_lambda.min", "No cov_lambda.1se", "Cov_lambda.min", "Cov_lambda.1se")
cat("Correlation of coefficients from two models:\n"); print(tmp)

tmp	<- NULL
for(i in 1:length(mygamma)){
	tmp1 <- cor(as.vector(predict(cv0[[i]], newdata=x, select="min")), 
				as.vector(predict(lcv0[[i]], newdata=x, select="min", type="response")) )
	tmp2 <- cor(as.vector(predict(cv0[[i]], newdata=x, select="1se")), 
				as.vector(predict(lcv0[[i]], newdata=x, select="1se", type="response")) )

	tmp3 <- cor(as.vector(predict(cvc[[i]], newdata=xx, select="min")), 
				as.vector(predict(lcvc[[i]], newdata=xx, select="min", type="response")) )
	tmp4 <- cor(as.vector(predict(cvc[[i]], newdata=xx, select="1se")), 
				as.vector(predict(lcvc[[i]], newdata=xx, select="1se", type="response")) )
	tmp  <- rbind(tmp, c(tmp1, tmp2, tmp3, tmp4))
}
colnames(tmp) <- c("No cov_lambda.min", "No cov_lambda.1se", "Cov_lambda.min", "Cov_lambda.1se")
cat("Correlation of the predicted values from two models:\n"); print(tmp)

############################
# Out of sample prediction #
############################
cat("\n--------------------------------------------------------\n")
cat("Out of sample prediction.\n")

matrix_expand <- function(mat, vec){
	n1	<- nrow(mat)
	n2	<- length(vec)
	sel	<- rep(1:n1, n2)
	mat_new <- cbind(mat[sel,], rep(vec, each=n1))
	return(mat_new)
}

mod			<- c("gaussian", "logit")
cova		<- c("no", "demo")
modelidx		<- matrix_expand(as.matrix(mylambdav, ncol=1), mygamma)
modelidx		<- matrix_expand(modelidx, cova)
modelidx		<- matrix_expand(modelidx, mod)
modelidx		<- data.frame(modelidx, index = 1:nrow(modelidx))
names(modelidx) <- c("lambda","gamma","covariates","model","index")

# Gausian linear prediction and logit probability prediction
phat		<- matrix(NA, nrow(x.out), nrow(modelidx))
tmp			<- c("min","1se")
for(i in 1:length(mygamma)){
	for(j in 1:length(mylambdav)){
		sel <- modelidx$gamma == mygamma[i] & modelidx$lambda == mylambdav[j]
		sel	<- modelidx[sel,"index"]
		phat[,sel[1]] 	<- as.vector(predict(cv0[[i]], newdata = x.out, select =tmp[j]))
		phat[,sel[2]] 	<- as.vector(predict(cvc[[i]], newdata = xx.out, select =tmp[j]))
		phat[,sel[3]] 	<- as.vector(predict(lcv0[[i]], newdata = x.out, type = "response", select =tmp[j]))
		phat[,sel[4]] 	<- as.vector(predict(lcvc[[i]], newdata = xx.out, type = "response", select =tmp[j]))
	}
}
e1		<- phat - y.out
tmp.tab	<- modelidx 
tmp.tab$RMSE  <- apply(e1, 2, function(x) sqrt(mean(x^2)))
tmp.tab$MED  <- apply(e1, 2, function(x) median(abs(x)))
cat("Model fitness:\n"); print(tmp.tab)


sink()
stopCluster(cl)
save.image("harbinger_gamlr_results.rdata")
cat("This program is done. \n")



