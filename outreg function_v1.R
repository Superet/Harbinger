model_outreg <- function(model.list,p.given=FALSE,varname=NULL,varlabs=NULL,digits=2,modelname=NULL,latex.superscript=F,
						head.name=c("estimate","std. error","pr(> t)")){
# Return result table wit p-value level indication star
# =======================================================
# model.list 		...	A list of models, each element has "Estimate" and "Std.Error"
# p.given			...	A logical indicator, TRUE if the model list includes p-values, 
#						 if FALSE then the function computes z-value and two-sided p-value
# varname			...	A vector of variable characters
# varlabs			...	A vector of variable 
# digits			...	rounding number
# modelname			...	A vector of model names
# latex.superscript	... Logical values, if T, it returns Latex style. 
	nm <- length(model.list)
	if(is.null(modelname)){
		if(is.null(names(model.list))){
			modelname <- paste("Model",1:nm,sep="")
		}else{
			modelname <- names(model.list)
		}
	}
	for(i in 1:nm){
		colnames(model.list[[i]]) <- tolower(colnames(model.list[[i]]))
	}
	head.name	<- tolower(head.name)
	if(p.given){
		out.list <- lapply(1:nm,function(i) 
					outreg(model.list[[i]][,head.name[1]],model.list[[i]][,head.name[2]],pval=model.list[[i]][,head.name[3]],
						   varname,varlabs,digits,modelname=modelname[i],latex.superscript) )
	}else{
		out.list <- lapply(1:nm,function(i) 
					outreg(model.list[[i]][,head.name[1]],model.list[[i]][,head.name[2]],pval=NULL,
						   varname,varlabs,digits,modelname=modelname[i],latex.superscript) )
	}
	
	check.var	<- sapply(out.list, function(x) identical(x$Variable, out.list[[1]]$Variable))
	if(all(check.var)){
		out <- do.call(cbind,out.list)
		if(nm>1){
			sel <- names(out)=="Variable" & 1:ncol(out)>1
			out <- out[,!sel]
		}
	}else{
		for(i in 1:nm){
			sel		<- seq(1, nrow(out.list[[i]]), 2)
			out.list[[i]]$Variable 	<- as.character(out.list[[i]]$Variable)
			out.list[[i]][(sel+1), "Variable"] <- paste(out.list[[i]][sel,"Variable"], ".se", sep="")
			if(i == 1) { out <- out.list[[1]]}
			if(i > 1){
				out	<- merge(out, out.list[[i]], by="Variable", all=T)	
			}
		}
		sel 	<- seq(1,nrow(out),2)
		tmp 	<- sort(apply(out[sel,], 1, function(x) sum(is.na(x))))
		ord		<- as.numeric(names(tmp))
		ord 	<- c(rbind(ord, ord+1))
		out		<- out[ord,]
		out[(sel+1),"Variable"] <- ""
	}
	out
}


outreg <- function(estimate,se,pval=NULL,varname=NULL,varlabs=NULL,digits=2,modelname=NULL,latex.superscript=F){
# Return result table with p-value level indication star
# NOTE: the p-value is computed by assuming normal distribution, not t distribution	 
#==================================================================================
# estimate			...	A vector of model estimate
# se				... A vector of standard error
# varname			...	A vector of variable characters
# varlabs			...	A vector of variable 
# digits			...	rounding number
# modelname			...	A character of model name
# latex.superscript	... Logical values, if T, it returns Latex style.
	
	
	n <- length(estimate)
	if(is.null(varname)){
		if(is.null(names(estimate))){
			varname <- paste("X",1:n,sep="")
		}else{
			varname <- names(estimate)
		}
	}else{
		estimate <- estimate[varname]
		se <- se[varname]
		n <- length(estimate)
	}
	if(is.null(varlabs)){
		varlabs <- varname
	}
	if(is.null(pval)){
		zval <- estimate/se
		pval <- (1-pnorm(abs(zval)))*2
	}
	# else{
	# 	if(any(!is.null(varname))){
	# 		pval <- pval[varname]
	# 	}
	# }
		
	sig.lev <- c(.01,.05,.1)
	sig.lab <- c("***","**","*","")
	names(sig.lab) <- c(1:3,0)
	tmp	<- sapply(1:3,function(x) x*(pval<sig.lev[x]))
	sig <- apply(tmp,1,function(x) ifelse(all(x==0), 0, min(x[x>0])))
	
	out <- rep(NA,2*n)
	out[seq(1,2*n,2)] <- paste(formatC(estimate,digits=digits,format="f"),sig.lab[as.character(sig)],sep="")
	out[seq(2,2*n,2)] <- paste("(",formatC(se,digits=digits,format="f"),")",sep="")
	if(latex.superscript){
		sel <- which(sig.lab[as.character(sig)]!="")
		out[(sel*2-1)] <- paste("$",formatC(estimate[sel],digits=digits,format="f"),"^{",sig.lab[as.character(sig)][sel],"}$",sep="")
	}
	out <- data.frame(Variable=rep(NA,2*n),out)
	colnames(out) <- c("Variable",modelname)
	out[seq(1,2*n,2),"Variable"] <- as.character(varname)
	out$Variable <- factor(out$Variable,levels=varname,labels=varlabs)
	out
}