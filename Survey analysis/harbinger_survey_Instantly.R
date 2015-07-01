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

sub_pool 	<- "Turk"
outfile 	<- paste("~/Desktop/harbinger_survey_result_",sub_pool,".csv", sep="")
f			<- file(outfile, "w")
writeLines("-------------------------------\n", f)
writeLines(paste("Results from ",sub_pool,"\n", sep=""), f)
close(f)

cls_met		<- "intention_affinity" 							# The classification metrics 
past_met	<- "past_rp_affinity"								# The past behavior measurement

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
myfile1		<- "harbinger_Instantly_code.csv"
myfile2		<- "harbinger_Instantly_text.csv"
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
					 				   variaty = paste("Q5.2_", 1:3, sep=""), 
									   innovativeness = paste("Q5.2_",4:5, sep=""), 
									   search  = paste("Q5.3_", 1:4, sep=""), 
									   uniqueness = paste("Q5.4_", c(1,2,4,5), sep="")), 
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

# Drop screen outer and people who did not finish
table(mydat$blk_random)
mydat	<- subset(mydat, blk_random !="")
table(mydat$Finished)
mydat 	<- subset(mydat, Finished==1)

# Check IP address
length(unique(mydat$IPAddress))
sel		<- duplicated(mydat$IPAddress)
sum(sel)
mydat	<- mydat[!sel,]

# Check attention
sel		<- qs_num$attention	# The observation id in qs data that shows the right answer to attention check 
tmp 	<- as.matrix(mydat[,sel])
tmp1	<- apply(tmp[,-ncol(tmp)], 1, function(x) sum(x=="1"))
tmp2	<- 1*(tmp[,ncol(tmp)]=="1")
identical(tmp2, 1*(tmp1==0))
table(tmp2)
by(tmp2, mydat$blk_random, table)

# Check finish time 
mydat$use_time <- as.numeric(as.POSIXlt(mydat$EndDate) - as.POSIXlt(mydat$StartDate) )
hist(as.numeric(mydat$use_time), breaks=50)
by(mydat$use_time, tmp2, summary)
ggtmp	<- data.frame(attention=tmp2, use_time = mydat$use_time)
ggplot(ggtmp, aes(use_time, fill=factor(attention), alpha=.3)) + geom_histogram(position="identity", binwidth=1) + xlim(c(0, 30))

# Drop subjects who failed attention check or use too little time
min_time <- 4			# The minimun completion time for effective response
mydat	<- subset(mydat, use_time >= min_time)
mydat	<- mydat[tmp2==1, ]
dim(mydat)

# Drop attention check questions
selcol	<- setdiff(names(mydat), qs_num$attention)
mydat	<- mydat[,selcol]

# Drop the responses that did not complete personality scales 
sel		<- unlist(qs_num$scale)
sel1	<- apply(mydat[,sel], 1, function(x) any(is.na(x)))
mydat	<- mydat[!sel1,]

# -------------------------------------------# 
# Recode soome questions. 
# Convert numeric variables 
sel			<- unlist(qs_num[c("past_buy1","intention1", "past_buy2","intention2","rating","scale")])
for(i in 1:length(sel)){
	mydat[,sel[i]] <- as.numeric(mydat[,sel[i]])
}
summary(mydat[,sel])

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

#####################
# Classify subjects #
#####################
#------------------------------------------------------------------------# 
# Summarize subjects' product selection 
# Match questions and product data
sel		<- qs$head %in% unlist(qs_num[c("past_buy1","intention1","choice")])
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
dv_name	<- c("past_affinity","past_rp_affinity","intention_fail","intention_success","intention_dif","intention_affinity","choice_affinity")
ggpairs(resp[,dv_name])


#------------------------------------------------------------------------# 
# Subset data with consistent DV
# Check if measure of past purchase is sparse. 
ggtmp	<- melt(resp[,c("num_pastall","num_pastfail","num_past_rp2","num_past_failrp2")])
ggplot(ggtmp, aes(value)) + geom_histogram(aes(y=..density..)) + geom_density() + 
		facet_wrap(~variable, scales="free")

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
		
# Validity regression
tmp		<- subset(resp_long, L1=="choice")
tmp$y	<- ifelse(tmp$value=="", 0, 1)
tmp		<- merge(tmp, resp, by="ResponseID", all.x=T)
myfml	<- as.formula(paste("y~",cls_met,"+ Fail*",cls_met, sep="" ))
summary(tmp1 <- glm(myfml, data=tmp, family = "binomial"))
myfml	<- as.formula(paste("y~",past_met,"+ Fail*",past_met, sep="" ))
summary(tmp2 <- glm(myfml, data=tmp, family = "binomial"))
tmp11	<- summary(tmp1)$coefficients
rownames(tmp11) <- gsub(cls_met, "cali_var",rownames(tmp11))
tmp21	<- summary(tmp2)$coefficients
rownames(tmp21) <- gsub(past_met, "cali_var",rownames(tmp21))
tmp_list<- list(tmp11, tmp21)
tmp.tab	<- model_outreg(tmp_list, p.given = T, modelname = c(cls_met,past_met),head.name = c("Estimate","Std. Error","Pr(>|z|)"))
for(i in 1:ncol(tmp.tab)) {tmp.tab[,i] <- as.character(tmp.tab[,i])}
tmp.tab	<- rbind(tmp.tab, c("AIC", tmp1$aic, tmp2$aic))

f		<- file(outfile, "at")
writeLines("Internal validity:", f)
write.csv(tmp.tab, f, row.names=F)
close(f)

myfml	<- as.formula(paste("y~",cls_met,"+ Fail*",cls_met,"+ (1|ResponseID) + (1|variable)", sep="" ))
m <- glmer(myfml, data=tmp, family="binomial")
summary(m)

#------------------------------------------------------------------------# 
# Subject classification
# Classify subjects into serveral groups
num_group	<- 2
hist(resp[,cls_met], breaks=50); abline(v = quantile(resp[,cls_met], c(0:num_group)/num_group, na.rm=T), col="red")
quantile(resp[,cls_met], c(0:num_group)/num_group, na.rm=T) 
mydat	<- merge(mydat, resp, by="ResponseID", all.x=T)

mydat$grp	<- cut(mydat[,cls_met], quantile(mydat[,cls_met], c(0:num_group)/num_group, na.rm=T), labels=1:num_group, include.lowest = TRUE)
table(mydat$grp)
mydat	<- subset(mydat, !is.na(grp))

ggplot(mydat, aes(choice_affinity, col=grp)) + geom_density()



