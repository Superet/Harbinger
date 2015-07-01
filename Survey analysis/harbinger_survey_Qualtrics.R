library(reshape2)
library(ggplot2)
library(data.table)
library(GGally)
library(psych)
library(xlsx)
library(gridExtra)
library(lme4)

setwd("~/Documents/Research/Harbinger/processed data/survey data")
source("../../Exercise/outreg function.R")

write2csv	<- FALSE
sub_pool	<- "Qualtrics"
if(write2csv){
	outfile 	<- paste("~/Desktop/harbinger_survey_result_",sub_pool,".csv", sep="")
	f			<- file(outfile, "w")
	writeLines("-------------------------------\n", f)
	writeLines(paste("Results from ",sub_pool,"\n", sep=""), f)
	close(f)
}

cls_met		<- "intention_affinity" 							# The classification metrics 
past_met	<- "past_rp_affinity"								# The past behavior measurement
clsv_met	<- "num_pastall"

#############
# Functions # 
#############
# ggplot mosaic plots 
makeplot_mosaic <- function(data, x, y, xlab=NULL,...){
	# xvar <- deparse(substitute(x))
	# yvar <- deparse(substitute(y))
	# mydata <- data[c(xvar, yvar)];
	xvar <- x
	yvar <- y
	mydata <- data[,c(xvar,yvar)]
	dim1 <- unique(data[,xvar])[1]
	mytable <- table(mydata, useNA="no");
	widths <- c(0, cumsum(apply(mytable, 1, sum)))/sum(mytable);
	heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))});

	alldata <- data.frame();
	allnames <- data.frame();
	for(i in 1:nrow(mytable)){
		for(j in 1:ncol(mytable)){
			alldata <- rbind(alldata, c(widths[i], widths[i+1], heights[j, i], heights[j+1, i]));
		}
	}
	colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")

	alldata[[xvar]] <- rep(dimnames(mytable)[[1]],rep(ncol(mytable), nrow(mytable)));
	alldata[[yvar]] <- rep(dimnames(mytable)[[2]],nrow(mytable));
	alldata$text_grp <- alldata[[xvar]]
	alldata$xtext <- with(alldata,xmin + (xmax-xmin)/2)
	alldata$ytext <- with(alldata,ymin + (ymax-ymin)/2)
	alldata$text <- as.character(alldata[[yvar]])
	alldata$text <- ifelse(alldata$text_grp == dim1, alldata$text, "")
	if(!is.null(xlab)){
		alldata$xlab <- xlab[as.numeric(alldata[[xvar]])]
	}else{
		alldata$xlab <- alldata[[xvar]]
	}

	ggplot(alldata, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) + 
		geom_rect(color="black", aes_string(fill=yvar)) +
		xlab(paste(xvar, "(proportion)")) + ylab(paste(yvar, "(proportion)")) + 
		scale_fill_brewer(palette = "Set3") + 
		geom_text(aes(x = xtext, y = ytext,label = text ),size=3) + 
		geom_text(aes(x = xtext, y = 1.05,label=xlab),size=4) +
		theme_bw() + labs(fill = NULL) + 
		theme(legend.position = "none",panel.grid.major = element_line(colour = NA),panel.grid.minor = element_line(colour = NA))
}


##########################
# Read and organize data #
##########################
myfile1		<- "harbinger_Qualtrics_code.csv"
myfile2		<- "harbinger_Qualtrics_text.csv"
orig_code	<- read.csv(myfile1, stringsAsFactors=FALSE)		
orig_text	<- read.csv(myfile2, stringsAsFactors=FALSE)
prod		<- read.xlsx("harbinger_survey_product.xlsx", sheetIndex=1)
prod_rate	<- read.xlsx("harbinger_survey_product.xlsx", sheetIndex=2)

qs_num		<- list(past_buy1	= paste("Q2.",rep(2:9, each=3),".", 1, "_", 1:3, sep=""), 
					intention1	= paste("Q2.",rep(2:9, each=3),".", 2, "_", 1:3, sep=""), 
					past_buy2 	= paste("Q3.",rep(2:9, each=3), "_", 1:3, sep=""), 
					intention2	= paste("Q3.",rep(11:18, each=3), "_", 1:3, sep=""), 
					choice 		= paste("Q4.",rep(1:2, each = 6), "_", 1:6, sep=""), 
					rating		= paste("Q4.", 4:7,"_1", sep=""), 
					scale 		= list(opinion = paste("Q5.1_",1:4, sep=""), 
					 				   variety = paste("Q5.2_", 1:3, sep=""), 
									   innovativeness = paste("Q5.2_",4:5, sep=""), 
									   search  = paste("Q5.3_", 1:4, sep=""), 
									   uniqueness = paste("Q5.4_", c(1,2,4,5), sep="")), 
					store		= c("Q1.4","Q1.5"),				
					attention 	= paste("Q5.6_", 1:21, sep=""), 
					demo		= paste("Q6.", 1:8, sep=""))

# Combine text and code format data
# Text data: product selection and demograhics
sel			<- c(qs_num$choice, qs_num$rating, qs_num$demo)
# orig_text[1,sel]
orig_dat	<- merge(orig_text[,c("V1",sel)], orig_code[,setdiff(names(orig_code), sel)], by="V1")
sel 		<- orig_dat$V1 =="ResponseID"
orig_dat  	<- rbind(orig_dat[sel,], orig_dat[-sel,])

# Strip the question data 
tmp 	<- orig_dat[1,]
selcol 	<- grep("Q",names(orig_dat))
tmp1	<- strsplit(names(tmp)[selcol],"_")
qs	 	<- data.frame(head = as.character(names(tmp)[selcol]), Q_num=sapply(tmp1, function(x) x[1]), 
					Q_value = as.numeric(sapply(tmp1, function(x) x[2])), Question = as.character(tmp[selcol]))
qs$head	<- as.character(qs$head)				

# Check screener 
summary(as.numeric(orig_text[-1,"Age"]))
table(orig_text[-1,"State"])
table(orig_text[-1,"GroShop"])

#--------------------------------------------# 
# Drop unuseful data 
# Drop my preview data 
mydat	<- orig_dat[-1,]
selcol 	<- grep("Q",names(orig_dat))
selcol1	<- setdiff(1:ncol(orig_dat), selcol) 
names(mydat)[selcol1] <- orig_dat[1,selcol1] 
sel 	<- names(mydat) == "Display Order: Block Randomizer FL_20"
names(mydat)[sel] <- "blk_random"
mydat	<- subset(mydat, Status==0)
dim(mydat)

# Check IP address
length(unique(mydat$IPAddress))
sel		<- duplicated(mydat$IPAddress)
sum(sel)
sum(is.na(mydat$ResponseID))
mydat	<- subset(mydat, !is.na(ResponseID))

# Check attention
sel		<- qs_num$attention	# The observation id in qs data that shows the right answer to attention check 
tmp 	<- as.matrix(mydat[,sel])
tmp1	<- apply(tmp[,-ncol(tmp)], 1, function(x) sum(x=="1"))
tmp2	<- 1*(tmp[,ncol(tmp)]=="1")
identical(tmp2, 1*(tmp1==0))
table(tmp2)

# Check finish time 
mydat$use_time <- as.numeric(as.POSIXlt(mydat$EndDate) - as.POSIXlt(mydat$StartDate) )
hist(as.numeric(mydat$use_time), breaks=50)
cat("Subjects' finishing time by block randomization:\n")
print(by(mydat$use_time, mydat$blk_random, summary))

# Drop subjects who failed attention check or use too little time
min_time <- 5			# The minimun completion time for effective response
sum(mydat$use_time < min_time)
mydat	<- subset(mydat, use_time >= min_time)
mydat	<- mydat[tmp2==1, ]
dim(mydat)

# Drop attention check questions
selcol	<- setdiff(names(mydat), qs_num$attention)
mydat	<- mydat[,selcol]

# -------------------------------------------# 
# Recode soome questions. 
# Convert numeric variables 
sel			<- c(unlist(qs_num[c("past_buy1","intention1", "past_buy2","intention2","rating","scale")]), "What is your age?")
for(i in 1:length(sel)){
	mydat[,sel[i]] <- as.numeric(mydat[,sel[i]])
}
summary(mydat[,sel])
sum(is.na(mydat$ResponseID))
mydat	<- subset(mydat, !is.na(ResponseID))

qs_num$store
qs[qs$head %in% qs_num$store, ]
sel		<- qs_num$store
sel1	<- which(names(mydat) %in% sel)
names(mydat)[sel1]	<- c("Costco", "Target")
sel		<- which(names(mydat) == "What is your age?")
names(mydat)[sel]	<- "Agen"


# -------------------------------------------# 
# Factor demongrahphic variables 
lapply(qs_num$demo, function(x) unique(mydat[,x]))
demo_name		<- c("Age", "Marrital", "Income", "Education", "Children", "Famsize", "Employment", "Occupation")
mydat$Age		<- factor(mydat[,qs_num$demo[1]], levels=c("18 - 24", "25 - 34","35 - 44", "45 - 54","55 - 64","65 and over"))
mydat$Marrital	<- factor(mydat[,qs_num$demo[2]], levels=c("Single","Married","Divorced"))
mydat$Income	<- factor(mydat[,qs_num$demo[3]], levels=c("Under $5,000","$5,000 - $9,999","$10,000 - $14,999","$15,000 - $19,999",
															"$20,000 - $24,999","$25,000 - $34,999","$35,000 - $49,999","$50,000 - $74,999",
															"$75,000 - $99,999","$100,000 -$149,999","$150,000 and over" ))
mydat$Education	<- factor(mydat[,qs_num$demo[4]], levels=c("Some high school","High school graduate","Some college","College graduate","Postgraduate/professional"))
mydat$Children	<- factor(mydat[,qs_num$demo[5]], levels=c("None","One","Two","Three") ) 
mydat$Famsize	<- factor(mydat[,qs_num$demo[6]], levels=c("One","Two","Three","Four","Five or more") )
mydat$Employment<- factor(mydat[,qs_num$demo[7]], levels=c("Do not work outside home","Work outside home part time","Work outside home full time"))
mydat$Occupation<- factor(mydat[,qs_num$demo[8]])

# Collapse some categorical variables 
tmpn 			<- 3
quantile(as.numeric(mydat$Income), c(0:tmpn)/tmpn)
mydat$Incomeg	<- cut(as.numeric(mydat$Income), quantile(as.numeric(mydat$Income), c(0:tmpn)/tmpn), include.lowest = T, 
						labels = c("L","M","H"))

##############
# Compute DV #
##############
#------------------------------------------------------------------------# 
# Summarize subjects' product selection 
# Match questions and product data
sel		<- qs$head %in% unlist(qs_num[c("past_buy1","intention1","past_buy2","intention2","choice")])
tmp		<- qs[sel,]
tmp1	<- sapply(strsplit(as.character(tmp$Question),"-"), function(x) x[length(x)])
tmp$Product <- gsub("<br>","",tmp1) 
tmp$Product	<- gsub("\\s+"," ", tmp$Product)
tmp		<- merge(tmp[,setdiff(names(tmp),"Question")], prod, by="Product", all.x=T)

# Subjects' response 
sel		<- mydat$blk_random == "calibration_1"
tmp1	<- melt(mydat[sel,c("ResponseID","blk_random", unlist(qs_num[c("past_buy1","intention1","choice")]))], 
				id.var=c("ResponseID","blk_random"))
tmp2	<- melt(mydat[!sel,c("ResponseID", "blk_random", unlist(qs_num[c("past_buy2","intention2","choice")]))], 
				id.var=c("ResponseID","blk_random"))
tmp1	<- rbind(tmp1, tmp2)
resp_long	<- merge(tmp1, tmp, by.x="variable",by.y="head",all.x=T)
tmp		<- melt(qs_num[c("past_buy1","intention1","past_buy2","intention2","choice")])
resp_long	<- merge(resp_long, tmp, by.x="variable", by.y="value", all.x=T)
resp_long	<- data.table(resp_long)
setkeyv(resp_long, c("ResponseID","variable"))
resp_long$L1 <- gsub("[0-9]", "", resp_long$L1)

#------------------------------------------------------------------------# 
# Compute DV
# Compute subjects' tendency of purchasing failure products: past purchase and intention to purchase
my_scale <- 6
resp1	<- resp_long[L1 != "choice",]
resp1$value	<- as.numeric(resp1$value)
resp1	<- resp1[,value1 := ifelse(Fail==1, value, my_scale-value)]
resp1	<- resp1[,list(num_pastall = sum((L1=="past_buy")*(value>1), na.rm=T), num_pastfail = sum((L1=="past_buy")*(value*Fail>1), na.rm=T), 
 					   num_past_rp2 = sum((L1=="past_buy")*(value-1), na.rm=T), num_past_failrp2=sum((L1=="past_buy")*((value-1)*Fail), na.rm=T),
					   sum_intention = sum((L1=="intention")*value, na.rm=T), 
					   num_intentionall = sum((L1=="intention") & value > .5*my_scale), 
					   num_intentionfail = sum((L1=="intention") & value > .5*my_scale & Fail == 1), 
					   intention_fail = sum((L1=="intention")*value*Fail, na.rm=T)/sum(L1=="intention" & Fail==1), 
					   intention_success = sum((L1=="intention")*value*(1-Fail), na.rm=T)/sum(L1=="intention" & Fail==0),
					   intention_affinity = sum((L1=="intention")*value*Fail, na.rm=T)/sum((L1=="intention")*value, na.rm=T)
					), 
					by=list(blk_random, ResponseID)]
resp1	<- resp1[,':='(past_affinity = num_pastfail/num_pastall, past_rp_affinity = num_past_failrp2/num_past_rp2, 
						intention_dif=intention_fail - intention_success)]

# Compute subjects choice of hold-out products
resp2	<- resp_long[L1=="choice",]
resp2	<- resp2[,list(num_choice_all=sum(value!=""), num_choice_fail=sum(value!="" & Fail==1)), by=list(ResponseID)]
resp2	<- resp2[,choice_affinity := num_choice_fail/num_choice_all]

# Collect all the metrics and check correlation 
resp	<- merge(data.frame(resp1), data.frame(resp2), by="ResponseID")
dv_name	<- c("num_pastfail","past_affinity","past_rp_affinity","intention_fail","intention_success","intention_dif","intention_affinity","choice_affinity")
ggpairs(resp[,dv_name])

#------------------------------------------------------------------------# 
# Subset data with consistent DV
# Check if measure of past purchase is sparse. 
ggtmp	<- melt(resp[,c("blk_random","num_pastall","num_pastfail","num_past_rp2","num_past_failrp2")], id.vars = "blk_random")
ggplot(ggtmp, aes(value)) + geom_histogram(aes(y=..density..)) + geom_density() + 
		facet_wrap(~variable, scales="free")

# Check number of choices vary by randomization
ggplot(ggtmp, aes(value,fill=blk_random, alpha=.5)) + geom_histogram(aes(y=..density..), position="identity") + geom_density() + 
		facet_wrap(~variable, scales="free")
by(resp$num_pastall, resp$blk_random, summary)
by(resp$num_pastfail, resp$blk_random, summary)
by(resp$num_past_rp2, resp$blk_random, summary)

sel <- resp$blk_random == "calibration_1"	
tmp	<- c("num_pastall","num_pastfail","num_past_rp2","num_past_failrp2")
for(i in 1:length(tmp)){
	cat("------------------------------------------------------------\n")
	cat("T-test of difference of", tmp[i], "between calibration 1 and 2:\n")
	print(t.test(resp[sel,tmp[i]], resp[!sel,tmp[i]]))
}

# Drop outlier subjects 
outlier1<- 30
outlier2<- 40
sum(resp$num_pastall> outlier1)
sum(resp$num_past_rp2 > outlier2)
sel		<- resp[resp$num_pastall>outlier1 | resp$num_past_rp2 > outlier2, "ResponseID"]
resp	<- subset(resp, !ResponseID %in% sel)
mydat	<- subset(mydat, !ResponseID %in% sel)
dim(mydat)

# Internal vadility: use choice_affinity as prediciton variable
ggtmp	<- data.frame(x = resp[,cls_met], y = resp$choice_affinity)
ggplot(ggtmp, aes(x,y)) + geom_point() + 
		geom_smooth()
		
# Validity regression: logit model 
tmp		<- subset(resp_long, L1=="choice")
tmp$y	<- ifelse(tmp$value=="", 0, 1)
tmp		<- merge(tmp, resp, by="ResponseID", all.x=T)
tmp		<- merge(tmp, mydat[,c("ResponseID","Target")], by="ResponseID", all.x=T)
selcol	<- c("num_pastall","num_pastfail","past_affinity","past_rp_affinity","num_intentionfail", "intention_dif","intention_affinity")
tmp_fit	<- vector("list", length(selcol))
tmp_list<- vector("list", length(selcol))
for(i in 1:length(selcol)){
	myfml		<- as.formula(paste("y~",selcol[i],"+ Fail*", selcol[i], sep="" ))
	cat("---------------------------------------------------\n")
	print(summary(tmp1 <- glm(myfml, data=tmp, family = "binomial")))
	tmp_fit[[i]] 	<- tmp1
	tmp_list[[i]]	<- summary(tmp1)$coefficients
	rownames(tmp_list[[i]]) <- gsub(selcol[i], "cali_var",rownames(tmp_list[[i]]))
}
tmp.tab		<- model_outreg(tmp_list, p.given = T, modelname = selcol,head.name = c("Estimate","Std. Error","Pr(>|z|)"))
for(i in 1:ncol(tmp.tab)) {tmp.tab[,i] <- as.character(tmp.tab[,i])}
tmp.aic		<- sapply(tmp_fit, function(x) x$aic)
tmp.tab		<- rbind(tmp.tab, c("AIC", tmp.aic))
cat("Choice model:\n"); print(tmp.tab); cat("\n")

# Validity regression: logit model controling for individual and product effects
tmp_fit	<- vector("list", length(selcol))
tmp_list<- vector("list", length(selcol))
for(i in 1:length(selcol)){
	myfml		<- as.formula(paste("y~",selcol[i],"+ Fail*",selcol[i],"+ (1|ResponseID) + (1|variable)", sep="" ))
	cat("---------------------------------------------------\n")
	print(summary(tmp1 <- glmer(myfml, data=tmp, family="binomial")))
	tmp_fit[[i]] 	<- tmp1
	tmp_list[[i]]	<- summary(tmp1)$coefficients
	rownames(tmp_list[[i]]) <- gsub(selcol[i], "cali_var",rownames(tmp_list[[i]]))
}
tmp.tab1	<- model_outreg(tmp_list, p.given = T, modelname = selcol,head.name = c("Estimate","Std. Error","Pr(>|z|)"))
for(i in 1:ncol(tmp.tab1)) {tmp.tab1[,i] <- as.character(tmp.tab1[,i])}
tmp.aic		<- sapply(tmp_fit, function(x) summary(x)$AICtab["AIC"])
tmp.tab1	<- rbind(tmp.tab1, c("AIC", tmp.aic))
cat("Choice model controling for individual and product effects:\n"); print(tmp.tab1); cat("\n")

# Add shopping stores to the regressions
tmp_fit	<- vector("list", length(selcol))
tmp_list<- vector("list", length(selcol))
for(i in 1:length(selcol)){
	myfml		<- as.formula(paste("y~",selcol[i],"+ Fail*", selcol[i], "+Target + Target:Fail",sep="" ))
	cat("---------------------------------------------------\n")
	print(summary(tmp1 <- glm(myfml, data=tmp, family = "binomial")))
	tmp_fit[[i]] 	<- tmp1
	tmp_list[[i]]	<- summary(tmp1)$coefficients
	rownames(tmp_list[[i]]) <- gsub(selcol[i], "cali_var",rownames(tmp_list[[i]]))
}
tmp.tab3		<- model_outreg(tmp_list, p.given = T, modelname = selcol,head.name = c("Estimate","Std. Error","Pr(>|z|)"))
for(i in 1:ncol(tmp.tab3)) {tmp.tab3[,i] <- as.character(tmp.tab3[,i])}
tmp.aic		<- sapply(tmp_fit, function(x) x$aic)
tmp.tab3		<- rbind(tmp.tab3, c("AIC", tmp.aic))
cat("Choice model:\n"); print(tmp.tab3); cat("\n")

# Export the regression 
if(write2csv){
	f		<- file(outfile, "at")
	writeLines("Internal validity -- choice model:", f)
	write.csv(tmp.tab, f, row.names=F)
	writeLines("Internal validity -- controling for individual and product effects:", f)
	write.csv(tmp.tab1, f, row.names=F)
	writeLines("Internal validity -- choice model (add shopping stores):", f)
	write.csv(tmp.tab3, f, row.names=F)
	writeLines("\n")
	close(f)
}

##############################################################
# Check measurement validity and compuate personality scores # 
##############################################################
selcol	<- unlist(qs_num$scale)
cor(mydat[,selcol])
ev		<- eigen(cor(mydat[,selcol], use="complete.obs"))$values
nf		<- sum(ev>=1)
scree(mydat[,selcol])
fa(mydat[,selcol], nfactors=5, rotate = "varimax", fm="ml")

# Compute aggregate score for each scales 
Nobs	<- nrow(mydat)
myscale	<- 5
scale_name	<- names(qs_num$scale)
qs[qs$head %in% unlist(qs_num$scale),]
weight		<- list(opinion		= c(1, 1, 1, -1), 
					variety		= c(-1, -1, 1), 
					innovativeness = c(1, -1),
					search		= rep(1, 4), 
					uniqueness	= rep(1, 4))

for(i in 1:length(qs_num$scale)){
	tmp_name	<- names(qs_num$scale)[i]
	tmp			<- (myscale + 1) *rep(1, Nobs) %*% t(weight[[i]]==-1) + 
					as.matrix(mydat[, qs_num$scale[[i]]]) * (rep(1, Nobs) %*% t(weight[[i]]))
	mydat[,tmp_name] <- rowMeans(tmp)
}
summary(mydat[,scale_name])

# Combine innovativeness and variety seeking together
sel		<- unlist(qs_num$scale[c("variety","innovativeness")])
tmp1	<- unlist(weight[c("variety","innovativeness")])
tmp		<- (myscale + 1) *rep(1, Nobs) %*% t(tmp1 == -1) + 
				as.matrix(mydat[, sel]) * (rep(1, Nobs) %*% t(tmp1))
mydat$exploration	<- rowMeans(tmp)				

# Preference rating 
qs[qs$head %in% qs_num$rating,]
print(prod_rate)
prod_rate$order <- c(1,0,3,0,4,2,0,0)
prod_rate		<- subset(prod_rate, order>0)
prod_rate$variable <- qs_num$rating[prod_rate$order]
tmp		<- melt(mydat[,c("ResponseID", qs_num$rating)], id.var="ResponseID")
tmp1	<- data.table(merge(tmp, prod_rate, by="variable"))
tmp1	<- tmp1[,minority := ifelse(Fail==1, value, 6 - value)]
tmp1	<- tmp1[,list(minority_pref = mean(minority)), by=list(ResponseID)]
hist(tmp1$minority_pref)
mydat	<- merge(mydat, data.frame(tmp1), by="ResponseID")

quartz()
ggpairs(mydat[,c(scale_name,"minority_pref","exploration")])

# Recheck factor analysis
selcol	<- c(unlist(qs_num$scale), qs_num$rating)
ev		<- eigen(cor(mydat[,selcol], use="complete.obs"))$values
nf		<- sum(ev>=1); nf
scree(mydat[,selcol])
fa(mydat[,selcol], nfactors=5, rotate = "varimax", fm="ml")

# Drop the individual qestions.
sel 	<- setdiff(names(qs_num), c("attention", "store"))
selcol	<- setdiff(names(mydat), unlist(qs_num[sel]))
mydat	<- mydat[,selcol]
dim(mydat)

if(write2csv){
	f		<- file(outfile, "at")
	writeLines("\nCorrelation between scales",f)
	write.csv(cor(mydat[,scale_name]), f)
	close(f)
}

###########################################
# Demongraphic and Personality difference # 
###########################################
#------------------------------------------------------------------------# 
# Search for a good classification combo
# num_group	<- 2
# tmp1 	<- c("num_pastfail","num_pastall","num_past_failrp2","num_past_rp2")
# cls_mat	<- cbind(tmp1, rep(c("past_affinity","past_rp_affinity"), each=2))
# cls_mat	<- rbind(cls_mat, cbind(tmp1, "intention_affinity"))
# cls_mat	<- rbind(cls_mat, cbind(c("num_intentionfail","num_intentionall"), "intention_affinity"))
# sel.subl<- list(c('p1v1','p2v2','p1v2','p2v1'), c('p1v1','p2v2','p1v2','p2v1'), c('p2v2','p1v2'))
# sel.norml <- list(c('p1v1','p2v1'), c('p1v1', 'p1v2'), 'p1v2')
# 
# # Plot the distribution of personality scales and repeat purchases
# selcol	<- c(scale_name,"exploration","minority_pref", "num_pastall", "num_pastfail","num_past_rp2")
# tmplab	<- c(scale_name, "exploration","Minority preference", "Sum of past purchases", "Sum of purchased failured", "Sum of purchase counts")
# 
# ggtmp	<- data.frame()
# for(i in 1:nrow(cls_mat)){
# 	clsv_met<- cls_mat[i,1]
# 	cls_met	<- cls_mat[i,2]
# 	
# 	cat("# ------------------------------------------#\n")
# 	tmpdat	<- merge(mydat, resp, by="ResponseID", all.x=T)
# 	tmp1	<- cut(tmpdat[,cls_met], quantile(tmpdat[,cls_met], c(0:num_group)/num_group, na.rm=T), labels=paste("p",1:num_group,sep=""), include.lowest = TRUE)
# 	tmp2	<- cut(tmpdat[,clsv_met], quantile(tmpdat[,clsv_met], c(0:num_group)/num_group, na.rm=T), labels=paste("v",1:num_group,sep=""), include.lowest = TRUE)
# 	print(table(tmp1, tmp2, dnn = list(cls_met = c("p1","p2"), clsv_met = c("v1", "v2"))))
# 	tmpdat$grp	<- factor(paste(tmp1, tmp2, sep=""), levels=c("p1v1","p2v1","p1v2","p2v2"))
# 	cat("Missing values:", sum(is.na(tmpdat$grp)),"\n")
# 	
# 	for(j in 1:length(sel.subl)){
# 		sel.sub		<- sel.subl[[j]]
# 		sel.norm	<- sel.norml[[j]]
# 		
# 		tmpdat.sub	<- subset(tmpdat, grp %in% sel.sub)
# 		tmpdat.sub$group	<- ifelse(tmpdat.sub$grp %in% sel.norm, "Normal","Harbinger")
# 		# T-test personality scales 
# 		sel		<- tmpdat.sub$group == "Harbinger"
# 		tmp.tab <- data.frame()
# 		for(i in 1:length(selcol)){
# 			tmp <- t.test(as.numeric(tmpdat.sub[!sel,selcol[i]]),as.numeric(tmpdat.sub[sel,selcol[i]]))
# 			tmp.tab <- rbind(tmp.tab,data.frame(Variable=selcol[i],Normal=tmp$estimate[1],Harbinger=tmp$estimate[2],
# 												Harbinger_Normal=tmp$estimate[2]-tmp$estimate[1],p.value=tmp$p.value))
# 		}
# 		row.names(tmp.tab) <- NULL
# 		tmp.tab$Variable	<- factor(tmp.tab$Variable, levels=selcol, labels= tmplab)
# 		sel	<- sapply(1:ncol(tmp.tab), function(x) is.numeric(tmp.tab[,x]))
# 		cat("T test results:\n"); cbind(tmp.tab[,!sel], round(tmp.tab[,sel], 4))
# 		
# 		# f	<- file(outfile, "at")
# 		tmp	<- ifelse(j==1, "volume", ifelse(j==2, "percentage", "persentage conditional on large volume"))
# 		# writeLines(paste("Using the metrics of", clsv_met, "and", cls_met, ", we segment subjects based on dimention of", tmp, ", T-test:"), f)
# 		# write.csv(tmp.tab, f, row.names=F)
# 		# close(f)
# 		ggtmp		<- rbind(ggtmp, data.frame(tmp.tab, volume = clsv_met, percentage = cls_met, subset= ifelse(j<3, "all","high volume"), crit = tmp))
# 	}
# }
# 
# sel	<- ggtmp$p.value <= .05 & ggtmp$crit == "percentage"
# sum(sel)
# ggtmp[sel,]

#------------------------------------------------------------------------# 
# Subject classification
# Classify subjects into serveral groups
num_group	<- 2
hist(resp[,cls_met], breaks=50); abline(v = quantile(resp[,cls_met], c(0:num_group)/num_group, na.rm=T), col="red")
quantile(resp[,cls_met], c(0:num_group)/num_group, na.rm=T) 
hist(resp[,clsv_met], breaks=50); abline(v = quantile(resp[,clsv_met], c(0:num_group)/num_group, na.rm=T), col="red")
quantile(resp[,clsv_met], c(0:num_group)/num_group, na.rm=T) 

mydat	<- merge(mydat, resp, by="ResponseID", all.x=T)
tmp1	<- cut(mydat[,cls_met], quantile(mydat[,cls_met], c(0:num_group)/num_group, na.rm=T), labels=paste("p",1:num_group,sep=""), include.lowest = TRUE)
tmp2	<- cut(mydat[,clsv_met], quantile(mydat[,clsv_met], c(0:num_group)/num_group, na.rm=T), labels=paste("v",1:num_group,sep=""), include.lowest = TRUE)
table(tmp1, tmp2)
mydat$grp	<- factor(paste(tmp1, tmp2, sep=""), levels=c("p1v1","p2v1","p1v2","p2v2"))
sum(is.na(mydat$grp))

ggplot(mydat, aes(choice_affinity, col=grp)) + geom_density()

# ------------------------------------------#
# Compare group 
# First plot demongrahpic mosiac
sel			<- c("p1v2","p2v2")
sel.norm	<- "p1v2"
mydat.sub	<- subset(mydat, grp %in% sel)
mydat.sub$group	<- ifelse(mydat.sub$grp %in% sel.norm, "Normal","Harbinger")
plots		<- list(NULL)
for(i in 1:length(qs_num$demo)){
	plots[[i]] <- makeplot_mosaic(mydat.sub,x="group",y=demo_name[i])
}
do.call(grid.arrange,c(plots,list(ncol=3)))

# Plot the distribution of personality scales and repeat purchases
selcol	<- c(scale_name,"exploration","minority_pref", "num_pastall", "num_pastfail","num_past_rp2","num_past_failrp2","Agen")
tmplab	<- c(scale_name, "exploration", "Minority preference", "Sum of past purchases", "Sum of purchased flops", 
			"Sum of purchase counts", "Sum of purchase counts of flops", "Age")
ggtmp	<- melt(mydat.sub[,c("group","ResponseID",selcol)], id.var=c("group","ResponseID"))
ggtmp$variable <- factor(ggtmp$variable, levels=selcol, labels=tmplab)
quartz()
ggplot(ggtmp, aes(group, value)) + geom_boxplot() + 
		facet_wrap(~variable, scales="free") + 
		labs(title = paste("Box plot of individual measures from ", sub_pool, sep=""))

# T-test personality scales 
sel		<- mydat.sub$group == "Harbinger"
tmp.tab <- data.frame()
for(i in 1:length(selcol)){
	tmp <- t.test(as.numeric(mydat.sub[!sel,selcol[i]]),as.numeric(mydat.sub[sel,selcol[i]]))
	tmp.tab <- rbind(tmp.tab,data.frame(Variable=selcol[i],Normal=tmp$estimate[1],Harbinger=tmp$estimate[2],
										Harbinger_Normal=tmp$estimate[2]-tmp$estimate[1],p.value=tmp$p.value))
}
row.names(tmp.tab) <- NULL
tmp.tab$Variable	<- factor(tmp.tab$Variable, levels=selcol, labels= tmplab)
sel	<- sapply(1:ncol(tmp.tab), function(x) is.numeric(tmp.tab[,x]))
cat("T test results:\n"); cbind(tmp.tab[,!sel], round(tmp.tab[,sel], 4))

if(write2csv){
	f	<- file(outfile, "at")
	writeLines(paste("\n T test comparing groups using", cls_met), f)
	write.csv(tmp.tab, f, row.names=F)
	close(f)
}

##########################
# Prediction of affinity # 
##########################
#---------------------------#
# Check distribution of affinity across high-low scales 
ggtmp	<- data.frame(NULL)
for(i in 1:length(scale_name)){
	tmp		<- median(mydat[,scale_name[i]])
	ggtmp	<- rbind(ggtmp, data.frame(scale=scale_name[i], level = ifelse(mydat[,scale_name[i]]>=tmp, "High","Low"), 
						affinity = mydat[,cls_met]))
}
ggplot(ggtmp, aes(affinity, alpha=.3)) + geom_histogram(aes(y=..density.., fill=level), position="identity") + 
		geom_density(aes(col=level)) + 
		facet_wrap(~scale) + 
		guides(alpha = FALSE) + 
		labs(title =  paste("Distribution of ",cls_met, " by low-high scale",sep=""))

#-------------------------------#
# Put the covariates one by one
cor(mydat[,scale_name])
for(i in 1:length(scale_name)){
	myfml	<- as.formula(paste(cls_met, " ~ ",scale_name[i], sep=""))
	print("-------------------------------")
	print(summary(lm(myfml, data= mydat)))
}

# Check if minority preference is explained by personality measurement
summary(lm(minority_pref ~  search + variety + innovativeness + opinion + uniqueness, data=mydat))

myfml <- as.formula(paste(cls_met, "~", paste(scale_name, collapse="+"),sep=""))
summary(myfit <- lm(myfml, data=mydat))
summary(update(myfit, . ~ .+ minority_pref))
summary(update(myfit, . ~ . - variety - innovativeness + exploration))

# Add shopping destination to the regression 
summary(update(myfit, . ~ .+ Costco))
summary(update(myfit, . ~ .+ Target))

#--------------------------------------------#
# Use personality traits to predict behavior # 
dv1		<- c("past_affinity","past_rp_affinity", "intention_dif", "intention_affinity", "choice_affinity",
			"num_pastfail", "num_past_failrp2", "num_choice_fail",  
			 "num_pastall", "num_past_rp2" , "num_choice_all")
tmp		<- paste(scale_name, collapse = "+")			
myfml	<- lapply(dv1, function(x) as.formula(paste(x, "~", tmp, sep="")))
fit_list <- list(NULL)
for(i in 1:length(dv1)){
	fit_list[[i]] <- lm(myfml[[i]], data=mydat)
}

model_list	<- lapply(fit_list, function(x) summary(x)$coefficient)
names(model_list) <- dv1
tmp.tab		<- model_outreg(model_list, p.give=TRUE, digits = 3, head.name=c("Estimate","Std. Error","Pr(>|t|)"))
print(tmp.tab)

# Use exploration to replace variety + innovativeness, control for demographics
tmp		<- paste(setdiff(scale_name, c("variety","innovativeness")), collapse = "+")			
myfml	<- lapply(dv1, function(x) as.formula(paste(x, "~", tmp, "+ exploration + Target + Incomeg + Agen",sep="")))
fit_list <- list(NULL)
for(i in 1:length(dv1)){
	fit_list[[i]] <- lm(myfml[[i]], data=mydat)
}

model_list	<- lapply(fit_list, function(x) summary(x)$coefficient)
names(model_list) <- dv1
tmp.tab1	<- model_outreg(model_list, p.give=TRUE, digits = 3, head.name=c("Estimate","Std. Error","Pr(>|t|)"))
print(tmp.tab1)

# Focus only on intention affinity
tmpiv	<- c(scale_name, paste(rep(c("opinion", "innovativeness+search+uniqueness"), 3), 
							   rep(c("Target","Target+Incomeg", "Target+Incomeg+Agen"), each=2), sep="+"))
myfml	<- lapply(tmpiv, function(x) as.formula(paste("intention_affinity", "~", x,sep="")))
fit_list <- list(NULL)
for(i in 1:length(myfml)){
	fit_list[[i]] <- lm(myfml[[i]], data=mydat)
}

model_list	<- lapply(fit_list, function(x) summary(x)$coefficient)
# names(model_list) <- dv1
tmp.tab2	<- model_outreg(model_list, p.give=TRUE, digits = 3, head.name=c("Estimate","Std. Error","Pr(>|t|)"))
print(tmp.tab2)


if(write2csv){
	f	<- file(outfile, "at")
	writeLines("\n Behavior prediction with personality:", f)
	write.csv(tmp.tab, f)
	writeLines("\n Behavior prediction with personality, including shopping places:", f)
	write.csv(tmp.tab1, f)
	writeLines("\n Prediction of intention_affinity with personality:", f)
	write.csv(tmp.tab2, f)
	close(f)
}






