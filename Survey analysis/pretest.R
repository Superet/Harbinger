library(reshape2)
library(ggplot2)
library(data.table)
library(GGally)
library(psych)

setwd("~/Documents/Research/Harbinger/processed data/survey data")
orig_code	<- read.csv("harbinger_pretest_1_code.csv", stringsAsFactors=FALSE)		
orig_text	<- read.csv("harbinger_pretest_1_text.csv", stringsAsFactors=FALSE)
id_dat		<- read.csv("harbinger_pretest_1_batch_results.csv")
prod		<- read.csv("harbinger_survey_product.csv")

prod_Q		<- 2:36				# The question numbers that ask about product purchases
scale_Q		<- 37:40			# The question numbers that measure personality 
scale_name	<- c("Search", "Exploration","Opinion leadership","Uniqueness")

# Combine text and code format data
sel			<- c(13:79, 131:138); orig_text[1,sel]
orig_dat	<- merge(orig_text[,c(1,sel)], orig_code[,setdiff(1:ncol(orig_code), sel)], by="V1")
sel 		<- orig_dat$V1 =="ResponseID"
orig_dat  	<- rbind(orig_dat[sel,], orig_dat[-sel,])

# # First time run to obtain the product names 
# sel <- 13:70		# The colume number that are product selection questions. 
# tmp	<- strsplit(as.character(orig_text[1,sel]), "-")
# tmp	<- sapply(tmp, function(x) x[2])
# prod	<- sort(unique(tmp[!is.na(tmp)]))
# sel	<- 72:79		# The colume number that are product selection question (Block 3)
# tmp	<- as.vector(as.matrix(orig_text[-1,sel]))
# unique(tmp)
# prod	<- sort(unique(c(prod, tmp)))
# 
# write.csv(prod, "~/Desktop/harbinger_survey_product.csv", row.names=F)

#####################
# Drop unusful data #
#####################
# Strip the question data 
tmp 	<- orig_dat[1,]
selcol 	<- grep("Q",names(orig_dat))
tmp1	<- strsplit(names(tmp)[selcol],"_")
qs	 	<- data.frame(head = as.character(names(tmp)[selcol]), Q_num=sapply(tmp1, function(x) x[1]), 
					Q_value = as.numeric(sapply(tmp1, function(x) x[2])), Question = as.character(tmp[selcol]))
qs$head	<- as.character(qs$head)				

# Drop my preview data 
mydat	<- orig_dat[-1,]
selcol1	<- setdiff(1:ncol(orig_dat), selcol) 
names(mydat)[selcol1] <- orig_dat[1,selcol1] 
mydat	<- subset(mydat, Status==0)
dim(mydat)

# Drop the people who did not finish. 
mydat	<- subset(mydat, Finished==1)
dim(mydat)

dim(id_dat)
length(unique(id_dat$Answer.surveycode))
mydat	<- merge(mydat, id_dat[,c("Answer.surveycode","WorkerId")], by.x="mTurkCode", by.y="Answer.surveycode")
dim(mydat)

# # Numeric the response to questions 
# selcol	<- as.character(qs[,"head"])
# for(i in 1:length(selcol)){
# 	mydat[,selcol[i]] <- as.numeric(mydat[,selcol[i]])
# }

# Check attention
sel		<- "Q75_21"	# The observation id in qs data that shows the right answer to attention check 
table(mydat[,sel])
mydat	<- subset(mydat, mydat[,sel]==1)

# Check block randomization. 
table(mydat[,"Display Order: Block Randomizer FL_9"])
sel <- names(mydat) == "Display Order: Block Randomizer FL_9"
names(mydat)[sel] <- "Block"

###########################
# Check response variable #
###########################
# Link product fail information
sel	<- qs$Q_num %in% paste("Q", prod_Q, sep="")
qs[sel,"head"]	
tmp	<- melt(mydat[,c("ResponseID","Block",qs[sel,"head"])], id.var=c("ResponseID","Block"))	
tmp	<- merge(tmp, prod, by.x="value",by.y="Values", all.x=T)
resp<- data.table(tmp)
resp<- resp[,list(num_prod = sum(!is.na(product)), num_fail=sum(fail, na.rm=T)), by=list(ResponseID,Block)]
resp<- resp[,affinity:=num_fail/num_prod]

# Compute DV: affinity/number of products
by(resp$num_prod, resp$Block, summary)
by(resp$num_fail, resp$Block, summary)
by(resp$affinity, resp$Block, summary)
ggplot(resp, aes(affinity, fill=Block, alpha=.5)) + geom_histogram(position="identity")

############################
# Check measurement scales #
############################
# Change factor variables to numeric variables
sel	<- qs$Q_num %in% paste("Q",scale_Q, sep="")
sel	<- qs[sel,"head"]
for(i in 1:length(sel)){
	mydat[,sel[i]] <- as.numeric(as.character(mydat[,sel[i]]))
}


# Check the correlation of questions within each scales 
for(i in 1:length(scale_Q)){
	sel	<- qs$Q_num == paste("Q", scale_Q[i],sep="")
	cat("\n------------------------------------------------\n")
	print(qs[sel,"Question"])
	sel1	<- qs[sel,"head"]
	print(ggpairs(mydat[,sel1], title=scale_name[i]))
	quartz()
}

# Factor analysis of each scales
fa_list	<- vector("list", length(scale_Q))
names(fa_list) <- scale_name
for(i in 1:length(scale_Q)){
	sel	<- qs$Q_num == paste("Q", scale_Q[i],sep="")
	cat("\n------------------------------------------------\n")
	print(qs[sel,"Question"])
	sel1	<- qs[sel,"head"]
	ev		<- eigen(cor(mydat[,sel1], use="complete.obs"))$values
	nf		<- sum(ev>=1)
	scree(mydat[,sel1])
	fa_list[[i]]	<- fa(mydat[,sel1], nfactors=nf, rotate = "varimax", fm="ml")
	print(fa_list[[i]])
}

# Factor analysis with common number of factors 
nf 	<- 1
fa_list	<- vector("list", length(scale_Q))
names(fa_list) <- scale_name
for(i in 1:length(scale_Q)){
	sel	<- qs$Q_num == paste("Q", scale_Q[i],sep="")
	cat("\n------------------------------------------------\n")
	print(qs[sel,"Question"])
	sel1	<- qs[sel,"head"]
	fa_list[[i]]	<- fa(mydat[,sel1], nfactors=nf, rotate = "varimax", fm="ml")
	print(fa_list[[i]])
}

# Correlation of factor scores between scales 
tmp	<- lapply(fa_list, function(x) x$scores)
tmp1 <- do.call(cbind, tmp)
colnames(tmp1) <- sapply(1:length(scale_Q), function(i) paste(names(fa_list)[i], colnames(fa_list[[i]]$scores),sep="_"))
cor(tmp1, use="complete.obs")

################
# Predictivity #
################
# Scales selection 
resp	<- data.frame(resp)
mydat2	<- data.frame(ResponseID = mydat$ResponseID, tmp1)
mydat2	<- merge(mydat2,resp[,c("ResponseID","affinity")], by="ResponseID", all.x=T)

fit		<- lm(affinity ~ . - ResponseID, data=mydat2)

# Variable seleciton 
