# Read in data:
set.seed(1111)
library(glmnet)
library(doParallel)
library(foreach)
library(caret)
library(Matrix)

# setwd("~/Dropbox/Dropbox Research/Blake_Chaoqun/Harbingers")
# setwd("/home/brgordon/ccv103/Harbinger")
setwd("/kellogg/users/marketing/2661703/Harbinger")
outfile <- "harbinger_output.txt"
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

# Cross validate lasso and ridge (gaussian):
# We should wrap this function so as to do multiple 10-fold cross-validation and also do binomial if feasible
myalpha <- c(0, .5, 1)
mylambdav <- c("lambda.min", "lambda.1se")
prc	<- proc.time()
pf 	<- rep(1:0, c(n.hh,n.c))
cv0	<- foreach(a=myalpha) %do% {
	cv.glmnet(x=x, y=y, family="gaussian", alpha=a, standardize=FALSE, intercept=TRUE, nfolds=10, parallel=TRUE)
}
cvc <- foreach(a = myalpha) %do% {
	cv.glmnet(x=xx, y=y, family="gaussian", alpha=a, standardize=FALSE, intercept=TRUE, nfolds=10, penalty.factor=pf, parallel=TRUE)
}
use.time <- proc.time() - prc
cat("\n (Parallel) Cross validation finishes using", use.time[3]/60,"min.\n")

prc <- proc.time()
m0 	<- foreach(i = 1:length(myalpha)) %dopar% {
	foreach(l = c(cv0[[i]]$lambda.min, cv0[[i]]$lambda.1se)) %do% {
		glmnet(x=x, y=y, family="gaussian", alpha=myalpha[i], standardize=FALSE, intercept=TRUE, lambda=l)
	}
} 
mc 	<- foreach(i = 1:length(myalpha)) %dopar% {
	foreach(l = c(cvc[[i]]$lambda.min, cvc[[i]]$lambda.1se)) %do% {
		glmnet(x=xx, y=y, family="gaussian", alpha=myalpha[i], standardize=FALSE, intercept=TRUE, lambda=l, penalty.factor=pf)
	}
}
names(m0) <- names(mc)	<- paste("alpha_", myalpha, sep="")
for(i in 1:length(myalpha)){
	names(m0[[i]]) <- c("lambda.min","lambda.1se")
	names(mc[[i]]) <- c("lambda.min","lambda.1se")
}
use.time <- proc.time() - prc
cat("\n (Parallel) glmnet finishes using", use.time[3]/60,"min.\n")

tmp	<- lapply(m0, function(x) sapply(x, function(z) as.vector(sign(z$beta))) )
cat("Table of coefficient signs from the model w/o covariates:\n")
print(lapply(tmp, function(x) apply(x, 2, table)))

tmp	<- lapply(mc, function(x) sapply(x, function(z) as.vector(sign(z$beta[1:n.hh]))) )
cat("Table of coefficient signs from the model with covariates:\n")
print(lapply(tmp, function(x) apply(x, 2, table)))

prc		<- proc.time()
tmp1	<- foreach(m = 1:length(m0)) %dopar% {
	foreach(j = 1:length(m0[[1]])) %do% {
		cor(predict(m0[[i]][[j]], newx= x), y)
	}	
}
tmp1	<- unlist(tmp1)
names(tmp1) <- paste(rep(names(m0), each=length(mylambdav)), rep(mylambdav, length(m0)), sep="_")
use.time 	<- proc.time() - prc
cat("Correlation of predicted values from models w/o covariates and y (using", use.time[3]/60,"min):\n")
print(tmp1)

prc		<- proc.time()
tmp1	<- foreach(m = 1:length(mc)) %dopar% {
	foreach(j = 1:length(mc[[1]])) %do% {
		cor(predict(mc[[i]][[j]], newx= xx), y)
	}	
}
tmp1	<- unlist(tmp1)
names(tmp1) <- paste(rep(names(m0), each=length(mylambdav)), rep(mylambdav, length(m0)), sep="_")
use.time 	<- proc.time() - prc
cat("Correlation of predicted values from models with covariates and y (using", use.time[3]/60,"min):\n")
print(tmp1)

# Fit binomial (i.e, logistic) models:
myalpha <- c(0, .5, 1)
mylambdav <- c("lambda.min", "lambda.1se")
prc	<- proc.time()
pf 	<- rep(1:0, c(n.hh,n.c))
lcv0	<- foreach(a=myalpha) %do% {
	cv.glmnet(x=x, y=y, family="binomial", alpha=a, standardize=FALSE, intercept=TRUE, nfolds=10, parallel=TRUE)
}
lcvc <- foreach(a = myalpha) %do% {
	cv.glmnet(x=xx, y=y, family="binomial", alpha=a, standardize=FALSE, intercept=TRUE, nfolds=10, penalty.factor=pf, parallel=TRUE)
}
use.time <- proc.time() - prc
cat("\n (Parallel) Cross validation finishes using", use.time[3]/60,"min.\n")

prc <- proc.time()
lm0 	<- foreach(i = 1:length(myalpha)) %dopar% {
	foreach(l = c(lcv0[[i]]$lambda.min, lcv0[[i]]$lambda.1se)) %do% {
		glmnet(x=x, y=y, family="binomial", alpha=myalpha[i], standardize=FALSE, intercept=TRUE, lambda=l)
	}
} 
lmc 	<- foreach(i = 1:length(myalpha)) %dopar% {
	foreach(l = c(lcvc[[i]]$lambda.min, lcvc[[i]]$lambda.1se)) %do% {
		glmnet(x=xx, y=y, family="binomial", alpha=myalpha[i], standardize=FALSE, intercept=TRUE, lambda=l, penalty.factor=pf)
	}
}
names(lm0) <- names(lmc)	<- paste("alpha_", myalpha, sep="")
for(i in 1:length(myalpha)){
	names(lm0[[i]]) <- names(lmc[[i]]) <- mylambdav
}
use.time <- proc.time() - prc
cat("\n (Parallel) glmnet finishes using", use.time[3]/60,"min.\n")

tmp	<- lapply(lm0, function(x) sapply(x, function(z) as.vector(sign(z$beta))) )
cat("Table of coefficient signs from the model w/o covariates:\n")
print(lapply(tmp, function(x) apply(x, 2, table)))

tmp	<- lapply(lmc, function(x) sapply(x, function(z) as.vector(sign(z$beta[1:n.hh]))) )
cat("Table of coefficient signs from the model with covariates:\n")
print(lapply(tmp, function(x) apply(x, 2, table)))

prc		<- proc.time()
tmp1	<- foreach(m = 1:length(lm0)) %dopar% {
	foreach(j = 1:length(lm0[[1]])) %do% {
		cor(predict(lm0[[i]][[j]], newx= x, type="response"), y)
	}	
}
tmp1	<- unlist(tmp1)
names(tmp1) <- paste(rep(names(lm0), each=length(mylambdav)), rep(mylambdav, length(lm0)), sep="_")
use.time 	<- proc.time() - prc
cat("Correlation of predicted values from models w/o covariates and y (using", use.time[3]/60,"min):\n")
print(tmp1)

prc		<- proc.time()
tmp1	<- foreach(m = 1:length(lmc)) %dopar% {
	foreach(j = 1:length(lmc[[1]])) %do% {
		cor(predict(lmc[[i]][[j]], newx= xx, type="response"), y)
	}	
}
tmp1	<- unlist(tmp1)
names(tmp1) <- paste(rep(names(lm0), each=length(mylambdav)), rep(mylambdav, length(lm0)), sep="_")
use.time 	<- proc.time() - prc
cat("Correlation of predicted values from models with covariates and y (using", use.time[3]/60,"min):\n")
print(tmp1)

################################
# Compare gaussian to binomial #
################################
prc		<- proc.time()
tmp1	<- foreach(i = 1:length(myalpha)) %dopar% {
	foreach(j = 1:length(mylambdav)) %dopar% {
		cor(predict(m0[[i]][[j]],newx=x), predict(lm0[[i]][[j]],newx=x,type="response"))
	}
}
tmp1	<- unlist(tmp1)
names(tmp1) <- paste(rep(names(lm0), each=length(mylambdav)), rep(mylambdav, length(lm0)), sep="_")
use.time 	<- proc.time() - prc
cat("Comparing gaussian to binomial-correlation of predicted values from two models w/o covariates (using", use.time[3]/60,"min):\n")
print(tmp1)

prc		<- proc.time()
tmp1	<- foreach(i = 1:length(myalpha)) %dopar% {
	foreach(j = 1:length(mylambdav)) %dopar% {
		cor(predict(mc[[i]][[j]],newx=xx), predict(lmc[[i]][[j]],newx=xx,type="response"))
	}
}
tmp1	<- unlist(tmp1)
names(tmp1) <- paste(rep(names(lm0), each=length(mylambdav)), rep(mylambdav, length(lm0)), sep="_")
use.time 	<- proc.time() - prc
cat("Comparing gaussian to binomial-correlation of predicted values from two models with covariates (using", use.time[3]/60,"min):\n")
print(tmp1)

tmp1	<- NULL
for(i in 1:length(myalpha)){
	for(j in 1:length(mylambdav)){
		cat("Models w/o covariates, alpha=", myalpha[i], ", lambda=", mylambdav[j])
		print( table( as.vector(sign(m0[[i]][[j]]$beta)), as.vector(sign(lm0[[i]][[j]]$beta)) ) )
		tmp1 	<- c(tmp1, cor( as.vector(m0[[i]][[j]]$beta), as.vector(lm0[[i]][[j]]$beta) ) )
	}
}
names(tmp1) <- paste(rep(names(lm0), each=length(mylambdav)), rep(mylambdav, length(lm0)), sep="_")
cat("Correlation of coefficients from two models w/o covariates:")
print(tmp1)

tmp1	<- NULL
for(i in 1:length(myalpha)){
	for(j in 1:length(mylambdav)){
		cat("Models with covariates, alpha=", myalpha[i], ", lambda=", mylambdav[j])
		print( table( as.vector(sign(mc[[i]][[j]]$beta)), as.vector(sign(lmc[[i]][[j]]$beta)) ) )
		tmp1 	<- c(tmp1, cor( as.vector(mc[[i]][[j]]$beta), as.vector(lmc[[i]][[j]]$beta) ) )
	}
}
names(tmp1) <- paste(rep(names(lmc), each=length(mylambdav)), rep(mylambdav, length(lm0)), sep="_")
cat("Correlation of coefficients from two models with covariates:")
print(tmp1)

##############
# Prediction #
##############
matrix_expand <- function(mat, vec){
	n1	<- nrow(mat)
	n2	<- length(vec)
	sel	<- rep(1:n1, n2)
	mat_new <- cbind(mat[sel,], rep(vec, each=n1))
	return(mat_new)
}

mod			<- c("gaussian", "logit")
cova		<- c("no", "demo")
modelidx		<- matrix_expand(as.matrix(mod, ncol=1), cova)
modelidx		<- matrix_expand(modelidx, myalpha)
modelidx		<- matrix_expand(modelidx, mylambdav)
modelidx		<- data.frame(modelidx, index = 1:nrow(modelidx))
names(modelidx) <- c("model","covariates","alpha","lambda","index")

#----------------------------------------------------------------# 
# In-sample prediction 
cat("In sample prediction. \n")
# Gausian linear prediction and logit probability prediction
phat		<- matrix(NA, nrow(x), nrow(modelidx))
for(i in 1:length(myalpha)){
	for(j in 1:length(mylambdav)){
		sel <- modelidx$alpha == myalpha[i] & modelidx$lambda == mylambdav[j]
		sel	<- modelidx[sel,"index"]
		phat[,sel[1]] 	<- predict(m0[[i]][[j]], newx = x)
		phat[,sel[2]] 	<- predict(lm0[[i]][[j]], newx = x, type="response")
		phat[,sel[3]] 	<- predict(mc[[i]][[j]], newx = xx)
		phat[,sel[4]] 	<- predict(lmc[[i]][[j]], newx = xx, type="response")
	}
}
e1		<- phat - y
i.pred	<- modelidx 
i.pred$RMSE  <- apply(e1, 2, function(x) sqrt(mean(x^2)))
i.pred$MED  <- apply(e1, 2, function(x) median(abs(x)))
i.pred 

# Logit classification prediction 
yhat	<- matrix(NA, nrow(x), nrow(modelidx)/2)
for(i in 1:length(myalpha)){
	for(j in 1:length(mylambdav)){
		sel <- modelidx$alpha == myalpha[i] & modelidx$lambda == mylambdav[j] & modelidx$model == "logit"
		sel	<- modelidx[sel,"index"]/2
		yhat[,sel[1]] 	<- predict(lm0[[i]][[j]], newx = x, type="class")
		yhat[,sel[2]] 	<- predict(lmc[[i]][[j]], newx = xx, type="class")
	}
}
i.pred1 <- subset(modelidx, model=="logit")
i.pred1$specificity <- i.pred1$sensitivity <- NA
for(i in 1:nrow(i.pred1)){
	tmp		<- table(as.numeric(yhat[,1]), y)
	tmp1 	<- confusionMatrix(tmp)
	i.pred1[i,c("specificity", "sensitivity")] <- tmp1$byClass[c("Specificity","Sensitivity")]
}
i.pred1

#----------------------------------------------------------------# 
# Out-of-scample prediction
cat("Out of sample prediction.\n ")

# Gausian linear prediction and logit probability prediction
phat		<- matrix(NA, nrow(x.out), nrow(modelidx))
for(i in 1:length(myalpha)){
	for(j in 1:length(mylambdav)){
		sel <- modelidx$alpha == myalpha[i] & modelidx$lambda == mylambdav[j]
		sel	<- modelidx[sel,"index"]
		phat[,sel[1]] 	<- predict(m0[[i]][[j]], newx = x.out)
		phat[,sel[2]] 	<- predict(lm0[[i]][[j]], newx = x.out, type="response")
		phat[,sel[3]] 	<- predict(mc[[i]][[j]], newx = xx.out)
		phat[,sel[4]] 	<- predict(lmc[[i]][[j]], newx = xx.out, type="response")
	}
}
e1		<- phat - y.out
o.pred	<- modelidx 
o.pred$RMSE  <- apply(e1, 2, function(x) sqrt(mean(x^2)))
o.pred$MED  <- apply(e1, 2, function(x) median(abs(x)))
o.pred 

# Logit classification prediction 
yhat	<- matrix(NA, nrow(x.out), nrow(modelidx)/2)
for(i in 1:length(myalpha)){
	for(j in 1:length(mylambdav)){
		sel <- modelidx$alpha == myalpha[i] & modelidx$lambda == mylambdav[j] & modelidx$model == "logit"
		sel	<- modelidx[sel,"index"]/2
		yhat[,sel[1]] 	<- predict(lm0[[i]][[j]], newx = x.out, type="class")
		yhat[,sel[2]] 	<- predict(lmc[[i]][[j]], newx = xx.out, type="class")
	}
}
o.pred1 <- subset(modelidx, model=="logit")
o.pred1$specificity <- o.pred1$sensitivity <- NA
for(i in 1:nrow(o.pred1)){
	tmp		<- table(as.numeric(yhat[,1]), y.out)
	tmp1 	<- confusionMatrix(tmp)
	o.pred1[i,c("specificity", "sensitivity")] <- tmp1$byClass[c("Specificity","Sensitivity")]
}
o.pred1

#----------------------------------------------------------------# 
# Eric's model 
num_grp	<- 4
af		<- colSums(y*x)/colSums(x)
hist(af, main = "Histogram of affinity index from the classification set")
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
yhat 	<- predict(lmg, newdata = data.frame(x.out.grp), type = "response")
e1		<- yhat - y.out
cat("Eric's logit model fitness:\n")
cat("RMSE=", sqrt(mean(e1^2)), ", MED=", median(abs(e1)))


# Check coefficient size for demographic variables 
demo.beta0	<- demo.betal	<- matrix(NA, n.c, length(myalpha)*length(mylambdav), 
							dimnames=list(colnames(z), paste("alpha_", rep(myalpha, each=length(mylambdav)),"__", rep(mylambdav, length(myalpha)), sep="")))
sel			<- 1:n.c + n.hh
for(i in 1:length(myalpha)){
	for(j in 1:length(mylambdav)){
		idx		<- (i-1)*length(mylambdav) + j
		demo.beta0[,idx] <- mc[[i]][[j]]$beta[sel]
		demo.betal[,idx] <- lmc[[i]][[j]]$beta[sel]
	}
}
rs	<- colSums(z)/n.hh
cat("Rescaled coefficients for gaussian models:\n"); round(demo.beta0*rs, 2)
cat("Rescaled coefficients for logit models:\n"); round(demo.beta0*rs, 2)

sink()
stopCluster(cl)
save.image("harbinger_glmnet_results.rdata")
cat("This program is done. \n")



