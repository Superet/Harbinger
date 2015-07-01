library(reshape2)
library(ggplot2)
library(data.table)
library(GGally)
library(psych)
library(xlsx)
library(gridExtra)

setwd("~/Documents/Research/Harbinger/processed data/survey data")
source("../../Exercise/outreg function.R")

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
orig_code	<- read.csv("harbinger_survey_Turk_code.csv", stringsAsFactors=FALSE)		
orig_text	<- read.csv("harbinger_survey_Turk_text.csv", stringsAsFactors=FALSE)
prod		<- read.xlsx("harbinger_survey_product.xlsx", sheetIndex=1)
prod_rate	<- read.xlsx("harbinger_survey_product.xlsx", sheetIndex=2)

# Set question numbers -- double check it.
qs_num		<- list(product = paste("Q2.", rep(2:19, each=3), "_", rep(1:3, 18), sep=""),
					rating	= paste("Q2.", 20:27, "_", 1, sep=""), 
					scale	= list(	search = paste("Q3.1_", c(2,5,6,7), sep=""), 
									exploration = paste("Q3.2_", 1:4, sep=""), 
									opinion = paste("Q3.3_", c(1,3,5,6), sep=""), 
									uniqueness = paste("Q3.4_", c(2,3,6,10,12), sep="")), 
					attention = "Q3.6_21", 
					demo	= paste("Q4.", 1:8, sep=""))		

# Combine text and code format data
# Text data: product selection and demograhics
sel			<- c(qs_num$product, qs_num$demo); orig_text[1,sel]
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

# Convert numeric variables 
sel			<- unlist(qs_num[c("rating","scale","attention")])
for(i in 1:length(sel)){
	mydat[,sel[i]] <- as.numeric(mydat[,sel[i]])
}

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
tmp		<- melt(mydat[,c("ResponseID", qs_num$product)], id.var="ResponseID")
tmp$value	<- gsub("<br>","",tmp$value)
tmp$value	<- gsub("\\s+"," ",tmp$value)
unique(tmp$value)[!unique(tmp$value) %in% prod$Product]
resp	<- merge(tmp, prod, by.x="value",by.y="Product", all.x=T)
resp	<- data.table(resp)
resp	<- resp[,list(num_prod = sum(value!=""), num_fail=sum(Fail, na.rm=T)), by=list(ResponseID)]
resp	<- resp[,affinity:=num_fail/num_prod]

par(mfrow = c(2,1))
hist(resp$num_prod)
hist(resp$affinity)

# Classify subjects into serveral groups
num_group	<- 4
quantile(resp$affinity, c(0:num_group)/num_group)
resp$grp	<- cut(resp$affinity, quantile(resp$affinity, c(0:num_group)/num_group), labels=1:4)
table(resp$grp)
resp		<- data.frame(resp)

# Merge to the full data
mydat	<- merge(mydat, resp, by="ResponseID")
dim(mydat)

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
scale_name	<- names(qs_num$scale)
weight		<- list(search		= rep(1, 4),
					exploration	= c(-1, -1, 1, 1), 
					opinion		= c(1, 1, 1, -1), 
					uniqueness	= rep(1, 5))

for(i in 1:length(qs_num$scale)){
	tmp_name	<- names(qs_num$scale)[i]
	tmp			<- (myscale + 1) *rep(1, Nobs) %*% t(weight[[i]]==-1) + 
					as.matrix(mydat[, qs_num$scale[[i]]]) * (rep(1, Nobs) %*% t(weight[[i]]))
	mydat[,tmp_name] <- rowMeans(tmp)
}
summary(mydat[,scale_name])
ggpairs(mydat[,scale_name])

# Preference rating 
qs[qs$head %in% qs_num$rating,]
prod_rate$order <- c(1,2,4,6,5,3,7,8)
prod_rate$variable <- qs_num$rating[prod_rate$order]
tmp		<- melt(mydat[,c("ResponseID", qs_num$rating)], id.var="ResponseID")
tmp1	<- data.table(merge(tmp, prod_rate, by="variable"))
tmp1	<- tmp1[,minority := ifelse(Fail==1, value-14, 22 - value)]
tmp1	<- tmp1[,list(minority_pref = mean(minority)), by=list(ResponseID)]
hist(tmp1$minority_pref)
mydat	<- merge(mydat, data.frame(tmp1), by="ResponseID")

# ------------------------------------------#
# Compare group 12 vs. group 34
# First plot demongrahpic mosiac
mydat$group	<- ifelse(mydat$grp %in% c(1,2), "Normal","Harbinger")
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

# Plot the distribution of personality scales 
ggtmp	<- melt(mydat[,c("group","ResponseID", scale_name)], id.var=c("group","ResponseID"))
ggplot(ggtmp, aes(group, value)) + geom_boxplot() + 
		facet_wrap(~variable) + 
		labs(title = "Box plot of personality scales")

# T-test personality scales 
sel		<- mydat$grp %in% c(3,4)
tmp.tab <- data.frame()
for(i in 1:length(scale_name)){
	tmp <- t.test(as.numeric(mydat[!sel,scale_name[i]]),as.numeric(mydat[sel,scale_name[i]]))
	tmp.tab <- rbind(tmp.tab,data.frame(Variable=scale_name[i],Normal=tmp$estimate[1],Harbinger=tmp$estimate[2],
										Harbinger_Normal=tmp$estimate[2]-tmp$estimate[1],p.value=tmp$p.value))
}
row.names(tmp.tab) <- NULL
sel	<- sapply(1:ncol(tmp.tab), function(x) is.numeric(tmp.tab[,x]))
cat("T test results:\n"); cbind(tmp.tab[,!sel], round(tmp.tab[,sel], 4))

# Test minority preference 
tmp		<- t.test(as.numeric(mydat[!sel,"minority_pref"]),as.numeric(mydat[sel,"minority_pref"]))
tmp.tab <- rbind(tmp.tab,data.frame(Variable="minority_pref",Normal=tmp$estimate[1],Harbinger=tmp$estimate[2],
									Harbinger_Normal=tmp$estimate[2]-tmp$estimate[1],p.value=tmp$p.value))

# ------------------------------------------#
# Compare group 1 vs. group 4
mydat1		<- subset(mydat, grp %in% c(1,4))
dim(mydat1)

plots	<- list(NULL)
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

# Plot the distribution of personality scales 
ggtmp	<- melt(mydat1[,c("group","ResponseID", scale_name)], id.var=c("group","ResponseID"))
ggplot(ggtmp, aes(group, value)) + geom_boxplot() + 
		facet_wrap(~variable) + 
		labs(title = "Box plot of personality scales")

# T-test personality scales 
sel		<- mydat1$grp %in% c(3,4)
tmp.tab <- data.frame()
for(i in 1:length(scale_name)){
	tmp <- t.test(as.numeric(mydat1[!sel,scale_name[i]]),as.numeric(mydat1[sel,scale_name[i]]))
	tmp.tab <- rbind(tmp.tab,data.frame(Variable=scale_name[i],Normal=tmp$estimate[1],Harbinger=tmp$estimate[2],
										Harbinger_Normal=tmp$estimate[2]-tmp$estimate[1],p.value=tmp$p.value))
}

# Test minority preference 
tmp		<- t.test(as.numeric(mydat1[!sel,"minority_pref"]),as.numeric(mydat1[sel,"minority_pref"]))
tmp.tab <- rbind(tmp.tab,data.frame(Variable="minority_pref",Normal=tmp$estimate[1],Harbinger=tmp$estimate[2],
									Harbinger_Normal=tmp$estimate[2]-tmp$estimate[1],p.value=tmp$p.value))
row.names(tmp.tab) <- NULL
sel	<- sapply(1:ncol(tmp.tab), function(x) is.numeric(tmp.tab[,x]))
cat("T test results:\n"); cbind(tmp.tab[,!sel], round(tmp.tab[,sel], 4))

##########################
# Prediction of affinity # 
##########################
#---------------------------#
# Check distribution of affinity across high-low scales 
ggtmp	<- data.frame(NULL)
for(i in 1:length(scale_name)){
	tmp		<- median(mydat[,scale_name[i]])
	ggtmp	<- rbind(ggtmp, data.frame(scale=scale_name[i], level = ifelse(mydat[,scale_name[i]]>=tmp, "High","Low"), 
						affinity = mydat$affinity))
}
ggplot(ggtmp, aes(affinity, alpha=.3)) + geom_histogram(aes(fill=level), position="identity") + 
		geom_density(aes(col=level)) + 
		facet_wrap(~scale) + 
		guides(alpha = FALSE) + 
		labs(title = "Distribution of affinity index by low-high scale")

#---------------------------#
myfit	<- lm(affinity ~ search + exploration + opinion + uniqueness + minority_pref, data = mydat)
summary(myfit)

# Put the covariates one by one
cor(mydat[,scale_name])
for(i in 1:length(scale_name)){
	myfml	<- as.formula(paste("affinity ~ ",scale_name[i], sep=""))
	print("-------------------------------")
	print(summary(lm(myfml, data= mydat)))
}

# Check if minority preference is explained by personality measurement
summary(lm(minority_pref ~  search + exploration + opinion + uniqueness, data=mydat))

# Check the absolute number of adoptions. 
summary(lm(num_prod ~  search + exploration + opinion + uniqueness, data=mydat))

model_list <- list(NULL)
model_list[[1]] <- summary(myfit)$coefficients
model_list[[2]] <- summary(lm(num_prod ~  search + exploration + opinion + uniqueness + minority_pref, data=mydat))$coefficients
model_list[[3]] <- summary(lm(num_fail ~  search + exploration + opinion + uniqueness + minority_pref, data=mydat))$coefficients
names(model_list) <- c("affinity","num_prod","num_fail")

tmp.tab		<- model_outreg(model_list, p.give=TRUE, digits = 3, head.name=c("Estimate","Std. Error","Pr(>|t|)"))


