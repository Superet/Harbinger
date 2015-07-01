library(reshape2)
library(ggplot2)
library(data.table)
library(GGally)
library(psych)
library(xlsx)
library(gridExtra)
library(lme4)
library(pwr)

setwd("~/Documents/Research/Harbinger/processed data/survey data")
source("../../Exercise/outreg function.R")
sub_pool 	<- "Turk"
outfile 	<- paste("~/Desktop/harbinger_survey_result_",sub_pool,".csv", sep="")
f			<- file(outfile, "w")
writeLines("-------------------------------\n", f)
writeLines(paste("Results from ",sub_pool,"\n", sep=""), f)
close(f)

cls_met		<- "intention_affinity" 							# The classification metrics 
past_met	<- "past_affinity"								# The past behavior measurement

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
#-------------------------------------------------------------------#
if(sub_pool == "elab"){
	# Set the right version/batch of responses: harbinger_eLab
	myfile1		<- "harbinger_survey_eLab_batch1_code.csv"
	myfile2		<- "harbinger_survey_eLab_batch1_text.csv"

	# Set question numbers -- double check it.
	qs_num		<- list(past_buy= paste("Q2.",rep(2:13, each=3), "_", rep(1:3, 12), sep=""), 
						intention= paste("Q2.", rep(15:26, each=3), "_", rep(1:3, 12), sep=""),
						choice	= paste("Q2.", rep(28:33, each=3), "_", rep(1:3, 6), sep=""),
						rating	= paste("Q2.", 34:41, "_", 1, sep=""), 
						scale	= list(	search = paste("Q3.1_", c(2,5,6,7), sep=""), 
										exploration = paste("Q3.2_", 1:4, sep=""), 
										opinion = paste("Q3.3_", c(1,3,5,6), sep=""), 
										uniqueness = paste("Q3.4_", c(2,3,6,10,12), sep="")), 
						attention = "Q3.6_21", 
						demo	= paste("Q4.", 1:8, sep=""))
}else{
	# Set the right version/batch of responses: harbinger_Turk_v1
	myfile1		<- "harbinger_survey_Turk_v1_code.csv"
	myfile2		<- "harbinger_survey_Turk_v1_text.csv"

	# Set question numbers -- double check it.
	qs_num		<- list(past_buy= paste("Q2.",rep(2:13, each=3), "_", rep(1:3, 12), sep=""), 
						intention= paste("Q2.", rep(15:26, each=3), "_", rep(1:3, 12), sep=""),
						choice	= paste("Q2.", rep(28:33, each=3), "_", rep(1:3, 6), sep=""),
						rating	= paste("Q2.", 34:41, "_", 1, sep=""), 
						scale	= list(	search = paste("Q3.1_", 1:4, sep=""), 
										exploration = paste("Q3.2_", 1:4, sep=""),
										opinion = paste("Q3.3_", 1:4, sep=""), 
										uniqueness = paste("Q3.4_", 1:5, sep="")), 
						attention = "Q3.6_21", 
						demo	= paste("Q4.", 1:8, sep=""))		
	
}


#-------------------------------------------------------------------#
# Read data in
orig_code	<- read.csv(myfile1, stringsAsFactors=FALSE)		
orig_text	<- read.csv(myfile2, stringsAsFactors=FALSE)
prod		<- read.xlsx("harbinger_survey_product.xlsx", sheetIndex=1)
prod_rate	<- read.xlsx("harbinger_survey_product.xlsx", sheetIndex=2)

# Combine text and code format data
# Text data: product selection and demograhics
sel			<- c(qs_num$intention,qs_num$choice, qs_num$rating, qs_num$demo)
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
mydat	<- subset(mydat, Status==0)
dim(mydat)

# Drop the people who did not finish. 
mydat	<- subset(mydat, Finished==1)
dim(mydat)

# Check IP address
length(unique(mydat$IPAddress))
sel		<- duplicated(mydat$IPAddress)
sum(sel)
mydat	<- mydat[!sel,]

# Check attention
sel		<- qs_num$attention	# The observation id in qs data that shows the right answer to attention check 
table(mydat[,sel])
mydat	<- subset(mydat, mydat[,sel]=="1")

# Check finish time 
mydat$use_time <- as.numeric(as.POSIXlt(mydat$EndDate) - as.POSIXlt(mydat$StartDate) )
hist(as.numeric(mydat$use_time), breaks=50)
min_time <- 4			# The minimun completion time for effective response
mydat	<- subset(mydat, use_time >= min_time)
dim(mydat)


# -------------------------------------------# 
# Recode soome questions. 
sel		<- sapply(qs_num[c("intention","choice","rating")], function(x) unlist(x)[1])
lapply(sel,function(x) unique(mydat[,x]))
for(i in 1:length(qs_num$intention)){
	mydat[,qs_num$intention[i]] <- factor(mydat[,qs_num$intention[i]], 
									levels=c("Very Unlikely","Unlikely","Somewhat Unlikely","Undecided","Somewhat Likely","Likely","Very Likely"))
	mydat[,qs_num$intention[i]]	<- as.numeric(mydat[,qs_num$intention[i]])
}

for(i in 1:length(qs_num$rating)){
	mydat[,qs_num$rating[i]]	<- as.numeric(mydat[,qs_num$rating[i]]) - 14
}

# Convert numeric variables 
sel			<- unlist(qs_num[c("past_buy","rating","scale","attention")])
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
sel		<- qs$head %in% unlist(qs_num[c("past_buy","intention","choice")])
tmp		<- qs[sel,]
tmp1	<- sapply(strsplit(as.character(tmp$Question),"-"), function(x) x[2])
tmp$Product <- gsub("<br>","",tmp1) 
tmp$Product	<- gsub("\\s+"," ", tmp$Product)
tmp		<- merge(tmp[,setdiff(names(tmp),"Question")], prod, by="Product", all.x=T)

# Subjects' response 
tmp1	<- melt(mydat[,c("ResponseID", unlist(qs_num[c("past_buy","intention","choice")]))], id.var="ResponseID")
resp_long	<- merge(tmp1, tmp, by.x="variable",by.y="head",all.x=T)
tmp		<- melt(qs_num[c("past_buy","intention","choice")])
resp_long	<- merge(resp_long, tmp, by.x="variable", by.y="value", all.x=T)
resp_long	<- data.table(resp_long)
setkeyv(resp_long, c("ResponseID","variable"))

#------------------------------------------------------------------------# 
# Compute DV
# Compute subjects' tendency of purchasing failure products: past purchase and intention to purchase
my_scale <- 8
resp1	<- resp_long[L1 != "choice",]
resp1$value	<- as.numeric(resp1$value)
resp1	<- resp1[,value1 := ifelse(Fail==1, value, my_scale-value)]
resp1	<- resp1[,list(num_pastall = sum((L1=="past_buy")*(value>1), na.rm=T), num_pastfail = sum((L1=="past_buy")*(value*Fail>1), na.rm=T), 
 					   num_past_rp2 = sum((L1=="past_buy")*(value-1), na.rm=T), num_past_failrp2=sum((L1=="past_buy")*((value-1)*Fail), na.rm=T),
					   intention_fail = sum((L1=="intention")*value*Fail, na.rm=T)/sum(L1=="intention" & Fail==1), 
					   intention_success = sum((L1=="intention")*value*(1-Fail), na.rm=T)/sum(L1=="intention" & Fail==0),
					   intention_affinity = sum((L1=="intention")*value*Fail, na.rm=T)/sum((L1=="intention")*value, na.rm=T)
					), 
					by=list(ResponseID)]
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

# # Normalize DV, and compute the consistency between DV (small standard deviation)
# tmp 	<- c(1,1,7,7,7,1)
# tmp1	<- as.matrix(resp[,dv_name])/(rep(1, nrow(resp)) %*% t(tmp))
# tmp1	<- apply(tmp1, 1, sd)
# hist(tmp1)
# quantile(tmp1, .9, na.rm=T)
# sel		<- resp[tmp1 <= quantile(tmp1, .9, na.rm=T), "ResponseID"]
# resp	<- subset(resp, ResponseID %in% sel)
# mydat	<- subset(mydat, ResponseID %in% sel)
# dim(mydat)

#------------------------------------------------------------------------# 
# Subject classification
# Classify subjects into serveral groups
num_group	<- 4
hist(resp[,cls_met], breaks=50); abline(v = quantile(resp[,cls_met], c(0:num_group)/num_group, na.rm=T), col="red")
quantile(resp[,cls_met], c(0:num_group)/num_group, na.rm=T) 
mydat	<- merge(mydat, resp, by="ResponseID", all.x=T)
mydat$grp	<- cut(mydat[,cls_met], quantile(mydat[,cls_met], c(0:num_group)/num_group, na.rm=T), labels=1:num_group, include.lowest = TRUE)
table(mydat$grp)
mydat	<- subset(mydat, !is.na(grp))

ggplot(mydat, aes(choice_affinity, col=grp)) + geom_density()

#------------------------------------------------------------------------# 
# Proportion and sample size 
# To archieve proportion of past purchases
tmp		<- resp$num_pastall/36
zstar	<- qnorm(.975)
tmp_p	<- .3
tmp_e	<- sd(tmp)
n		<- zstar^2*tmp_p*(1-tmp_p)/tmp_e^2 

# T-test of affinity index between groups
sel		<- as.numeric(resp$grp) <= num_group/2
t.test(resp[sel,"past_affinity"], resp[!sel,"past_affinity"])
pwr.t.test(d = 0.15, sig.level=.05, power= 0.8, type ="two.sample", alternative = "greater")

##############################
# Check measurement validity #
##############################
selcol	<- unlist(qs_num$scale)
cor(mydat[,selcol])
ev		<- eigen(cor(mydat[,selcol], use="complete.obs"))$values
nf		<- sum(ev>=1)
scree(mydat[,selcol])
fa(mydat[,selcol], nfactors=4, rotate = "varimax", fm="ml")

###########################################
# Demongraphic and Personality difference # 
###########################################
# Compute aggregate score for each scales 
Nobs	<- nrow(mydat)
myscale	<- 5
scale_name	<- c(names(qs_num$scale), "variety","innovativeness")
weight		<- list(search		= rep(1, 4),
					exploration	= c(-1, -1, 1, 1), 
					opinion		= c(1, 1, 1, -1), 
					uniqueness	= rep(1, 5), 
					variety 	= c(-1, -1, 1), 
					innovativeness = 1)

for(i in 1:length(qs_num$scale)){
	tmp_name	<- names(qs_num$scale)[i]
	tmp			<- (myscale + 1) *rep(1, Nobs) %*% t(weight[[i]]==-1) + 
					as.matrix(mydat[, qs_num$scale[[i]]]) * (rep(1, Nobs) %*% t(weight[[i]]))
	mydat[,tmp_name] <- rowMeans(tmp)
}
sel			<- qs_num$scale$exploration[1:3]
tmp			<- (myscale + 1) *rep(1, Nobs) %*% t(weight[[5]]==-1) + 
				as.matrix(mydat[, sel]) * (rep(1, Nobs) %*% t(weight[[5]]))
mydat$variety <- rowMeans(tmp)	
sel			<- qs_num$scale$exploration[4]
mydat$innovativeness <- mydat[,sel]
summary(mydat[,scale_name])

# Preference rating 
qs[qs$head %in% qs_num$rating,]
prod_rate$order <- c(1,2,4,6,5,3,7,8)
prod_rate$variable <- qs_num$rating[prod_rate$order]
tmp		<- melt(mydat[,c("ResponseID", qs_num$rating)], id.var="ResponseID")
tmp1	<- data.table(merge(tmp, prod_rate, by="variable"))
tmp1	<- tmp1[,minority := ifelse(Fail==1, value, 8 - value)]
tmp1	<- tmp1[,list(minority_pref = mean(minority)), by=list(ResponseID)]
hist(tmp1$minority_pref)
mydat	<- merge(mydat, data.frame(tmp1), by="ResponseID")

quartz()
ggpairs(mydat[,c(scale_name,"minority_pref")])

# Recheck factor analysis
selcol	<- c(unlist(qs_num$scale), qs_num$rating)
ev		<- eigen(cor(mydat[,selcol], use="complete.obs"))$values
nf		<- sum(ev>=1); nf
scree(mydat[,selcol])
fa(mydat[,selcol], nfactors=5, rotate = "varimax", fm="ml")

# ------------------------------------------#
# Compare group 12 vs. group 34
# First plot demongrahpic mosiac
mydat$group	<- ifelse(mydat$grp %in% seq(1,num_group/2), "Normal","Harbinger")
plots		<- list(NULL)
for(i in 1:length(qs_num$demo)){
	plots[[i]] <- makeplot_mosaic(mydat,x="group",y=demo_name[i])
}
do.call(grid.arrange,c(plots,list(ncol=3)))

# Chi-square testing demongraphics
chip	<- rep(NA, length(demo_name))
names(chip) <- demo_name
for(i in 1:length(demo_name)){
	tmp	<- table(mydat$group, mydat[,demo_name[i]])
	chip[i] <- chisq.test(tmp)$p.value
}
cat("P value of chi-square test: \n"); print(chip)

# Plot the distribution of personality scales and repeat purchases
sel		<- qs_num$scale$exploration
qs[qs$head %in% sel,]
sel		<- qs_num$scale$exploration[2]
selcol	<- c(scale_name,"minority_pref", sel,"num_past_failrp2","num_past_rp2")
ggtmp	<- melt(mydat[,c("group","ResponseID",selcol)], id.var=c("group","ResponseID"))
ggtmp$variable <- factor(ggtmp$variable, levels=selcol, 
				labels=c(scale_name, "Minority preference","Switch question","Sum of repeat purchases\n in the past",
						"Sum of repeast purchases of \n flops bought in the past"))
quartz()
ggplot(ggtmp, aes(group, value)) + geom_boxplot() + 
		facet_wrap(~variable, scales="free") + 
		labs(title = paste("Box plot of individual measures from ", sub_pool, sep=""))

# T-test personality scales 
sel		<- mydat$group == "Harbinger"
selcol	<- c(scale_name,"minority_pref", qs_num$scale$exploration[2],"num_past_failrp2","num_past_rp2")
tmp.tab <- data.frame()
for(i in 1:length(selcol)){
	tmp <- t.test(as.numeric(mydat[!sel,selcol[i]]),as.numeric(mydat[sel,selcol[i]]))
	tmp.tab <- rbind(tmp.tab,data.frame(Variable=selcol[i],Normal=tmp$estimate[1],Harbinger=tmp$estimate[2],
										Harbinger_Normal=tmp$estimate[2]-tmp$estimate[1],p.value=tmp$p.value))
}
row.names(tmp.tab) <- NULL
tmp.tab$Variable	<- factor(tmp.tab$Variable, levels=selcol, labels= c(scale_name, "minority preference","Switch question",
							"Sum of repeat purchases in the past","Sum of repeast purchases of flops bought in the past"))
sel	<- sapply(1:ncol(tmp.tab), function(x) is.numeric(tmp.tab[,x]))
cat("T test results:\n"); cbind(tmp.tab[,!sel], round(tmp.tab[,sel], 4))

f	<- file(outfile, "at")
writeLines(paste("\n T test comparing group 12 vs. 34 using", cls_met), f)
write.csv(tmp.tab, f, row.names=F)
close(f)

# ------------------------------------------#
# Compare group 1 vs. group 4
mydat1		<- subset(mydat, grp %in% c(1,num_group))
dim(mydat1)

plots		<- list(NULL)
for(i in 1:length(qs_num$demo)){
	plots[[i]] <- makeplot_mosaic(mydat1,x="group",y=demo_name[i])
}
do.call(grid.arrange,c(plots,list(ncol=3)))

# Chi-square testing demongraphics
chip	<- rep(NA, length(demo_name))
names(chip) <- demo_name
for(i in 1:length(demo_name)){
	tmp	<- table(mydat1$group, mydat1[,demo_name[i]])
	chip[i] <- chisq.test(tmp)$p.value
}
cat("P value of chi-square test: \n"); print(chip)

# Plot the distribution of personality scales and repeat purchases
sel		<- qs_num$scale$exploration
qs[qs$head %in% sel,]
sel		<- qs_num$scale$exploration[2]
selcol	<- c(scale_name,"minority_pref", sel,"num_past_failrp2","num_past_rp2")
ggtmp	<- melt(mydat1[,c("group","ResponseID",selcol)], id.var=c("group","ResponseID"))
ggtmp$variable <- factor(ggtmp$variable, levels=selcol, 
				labels=c(scale_name, "Minority preference","Switch question","Sum of repeat purchases\n in the past",
						"Sum of repeast purchases of \n flops bought in the past"))
quartz()
ggplot(ggtmp, aes(group, value)) + geom_boxplot() + 
		facet_wrap(~variable, scales="free") + 
		labs(title = paste("Box plot of individual measures from ", sub_pool, sep=""))

# T-test personality scales 
sel		<- mydat1$group == "Harbinger"
selcol	<- c(scale_name,"minority_pref", qs_num$scale$exploration[2],"num_past_failrp2","num_past_rp2")
tmp.tab <- data.frame()
for(i in 1:length(selcol)){
	tmp <- t.test(as.numeric(mydat1[!sel,selcol[i]]),as.numeric(mydat1[sel,selcol[i]]))
	tmp.tab <- rbind(tmp.tab,data.frame(Variable=selcol[i],Normal=tmp$estimate[1],Harbinger=tmp$estimate[2],
										Harbinger_Normal=tmp$estimate[2]-tmp$estimate[1],p.value=tmp$p.value))
}
row.names(tmp.tab) <- NULL
tmp.tab$Variable	<- factor(tmp.tab$Variable, levels=selcol, labels= c(scale_name, "minority preference","Switch question",
							"Sum of repeat purchases in the past","Sum of repeast purchases of flops bought in the past"))
sel	<- sapply(1:ncol(tmp.tab), function(x) is.numeric(tmp.tab[,x]))
cat("T test results:\n"); cbind(tmp.tab[,!sel], round(tmp.tab[,sel], 4))

f	<- file(outfile, "at")
writeLines(paste("\n T test comparing group 1 vs. 4 using", cls_met), f)
write.csv(tmp.tab, f, row.names=F)
close(f)

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
summary(lm(minority_pref ~  search + exploration + opinion + uniqueness, data=mydat))

myfml <- as.formula(paste(cls_met, "~ search + exploration + opinion + uniqueness + minority_pref +",qs_num$scale$exploration[2],sep=""))
summary(myfit <- lm(myfml, data=mydat))
summary(update(myfit, . ~. -exploration - minority_pref))


#--------------------------------------------#
# Use personality traits to predict behavior # 
dv1		<- c("past_affinity","past_rp_affinity","num_past_failrp2", "num_past_rp2" ,"intention_affinity", 
			"choice_affinity", "num_pastall","num_choice_all")
myfml	<- lapply(dv1, function(x) as.formula(paste(x, "~ search + variety + innovativeness + opinion + uniqueness ",sep="")))
as.formula(paste(cls_met, "~ search + exploration + opinion + uniqueness + minority_pref",sep=""))
fit_list <- list(NULL)
for(i in 1:length(dv1)){
	fit_list[[i]] <- lm(myfml[[i]], data=mydat)
	cat("-------------------------------------")
	print(summary(fit_list[[i]]))
}

model_list	<- lapply(fit_list, function(x) summary(x)$coefficient)
names(model_list) <- dv1
tmp.tab		<- model_outreg(model_list, p.give=TRUE, digits = 3, head.name=c("Estimate","Std. Error","Pr(>|t|)"))

# Focus only on intention affinity
tmpiv	<- scale_name
myfml	<- lapply(tmpiv, function(x) as.formula(paste("intention_affinity", "~", x,sep="")))
fit_list <- list(NULL)
for(i in 1:length(myfml)){
	fit_list[[i]] <- lm(myfml[[i]], data=mydat)
}

model_list	<- lapply(fit_list, function(x) summary(x)$coefficient)
# names(model_list) <- dv1
tmp.tab2	<- model_outreg(model_list, p.give=TRUE, digits = 3, head.name=c("Estimate","Std. Error","Pr(>|t|)"))
print(tmp.tab2)


f	<- file(outfile, "at")
writeLines("\n Behavior prediction with personality:", f)
write.csv(tmp.tab, f)
writeLines("\n Prediction of intention_affinity with personality:", f)
write.csv(tmp.tab2, f)
close(f)

