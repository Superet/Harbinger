# This file generates summary statistics 

library(xlsx)
library(ggplot2)
library(data.table)
library(gridExtra)
library(reshape2)
library(scales)

# Set working directory
my.wd <- "~/Documents/Research/Harbinger/processed data"
iri.wd <- "~/Documents/Research/Data/IRI/kelloggirir"
setwd(my.wd)

# Set plotting parameters and 
plot.wd 	<- "/Users/chaoqunchen/Desktop"
ww 			<- 6.5
ar 			<- .6
make_plot 	<- FALSE

# Initiate exporting xlsx 
tab.file<- paste(plot.wd, "/harbinger_6_sumstat.xlsx",sep="")
mywb	<- createWorkbook()
sht1	<- createSheet(mywb, "SumStats")

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

###############################
# Read data and organize data # 
###############################
# Read data #
mycut	<-   4		# The cutoff year that defines product failure
new_prod<- read.delim(paste(my.wd,"/new_full.csv",sep=""),sep = ",", header=T, comment.char = "",quote="",blank.lines.skip = T)
hh 		<- read.csv(paste(my.wd,"/new_product_hh_class_cutoff",mycut,".csv",sep=""),header=T,as.is=T,comment.char = '',quote="",blank.lines.skip = T)

# Keep primary UPC 
new_prim<- subset(new_prod, prim == 1)

# Convert date variables 
new_prod$DFM <- as.Date(as.character(new_prod$DFM),format="%Y-%m-%d")
new_prod$DLM <- as.Date(as.character(new_prod$DLM),format="%Y-%m-%d")
new_prod$year<- year(new_prod$DFM)

# Check the date: if it is consitent with IRI definition
tmp <- (new_prod$WFM-400)*7+31900
tmp <- as.Date(tmp,origin="1899-12-30")
identical(tmp,new_prod$DFM)

# Read trip and demo code
demo.code <- read.xlsx(paste(iri.wd,"/Academic Household File.xls",sep=""),sheetName="Demo Code")
demo.code <- demo.code[-c(1,2),]
colnames(demo.code) <- c("VARIABLE","CODE","DEMO_DSC")
sel <- apply(demo.code,1,function(x) all(is.na(x)|x==""))
demo.code <- demo.code[!sel,]

trip.code <- read.xlsx(paste(iri.wd,"/Academic Household File.xls",sep=""),sheetName="Trip Code")
sel <- colMeans(is.na(trip.code))!=1
trip.code <- trip.code[-c(1,2),sel]
colnames(trip.code) <- c("VARIABLE","CODE","TRIP_DSC")
sel <- apply(trip.code,1,function(x) all(is.na(x)|x==""))
trip.code <- trip.code[!sel,]

# Convert demographics codes
tmp <- unique(demo.code$VARIABLE) 
tmp <- as.character(tmp[tmp %in% names(hh)])
for(i in 1:length(tmp)){
	sel <- demo.code$VARIABLE == tmp[i]
	tmp1 <- gsub("^\\s+|\\s+$","",as.character(demo.code[sel,"DEMO_DSC"]))
	# demo[,as.character(tmp[i])] <- factor(demo[,as.character(tmp[i])],levels=demo.code[sel,"CODE"],labels=tmp1)
	hh[,as.character(tmp[i])] <- factor(hh[,as.character(tmp[i])],levels=demo.code[sel,"CODE"],labels=tmp1)
}

#######################################
# Summary statistics for new products # 
#######################################
# Table of number of (primary) new product introduction by year
tmp.tab	<- data.table(subset(new_prod, prim==1))
tmp.tab	<- tmp.tab[,list(num_upc = length(unique(UPC)), num_brand = length(unique(BRAND))), by=list(year)]
tmp.tab	<- rbind(as.matrix(tmp.tab), colSums(tmp.tab))
tmp.tab[5,1] <- NA
cat("Table of number of new product introduction by year:\n"); print(tmp.tab); cat("\n")
addDataFrame(x=tmp.tab, sheet=sht1)

# Histogram of new product life and definition of new product failure
my.thresh <- 52*mycut
if(make_plot){
	pdf(paste(plot.wd,"/graph_product_life.pdf",sep=""), width=ww, height=ww*ar)
	print( ggplot(new_prim, aes(x=LIFE/4)) + geom_histogram(aes(y=..density..), binwidth=1) + 
			geom_vline(x = my.thresh/4, linetype=2) + 
			xlab("Product life (month)") 
		)	
	dev.off()
}

#####################################
# Summary statistics for households # 
#####################################
# Histogram of affinity index 
tmp	<- quantile(hh$affinity, c(1:3)/4, na.rm=T)
if(make_plot){
	pdf(paste(plot.wd,"/graph_affinity_hist.pdf",sep=""), width = ww, height=ww*ar)
	print(ggplot(hh, aes(affinity)) + geom_histogram(aes(y=..density..), binwidth=.01) + 
				geom_vline(xintercept = tmp, linetype = 2) + 
				labs(x = "Affinity")
		)
	dev.off()	
}

# Plot all demographic composition
num_compare <- 2					# 2 -- compare group 1 vs 4; 4 -- compare group 12 vs. 34;
selcol <- c("FAMSIZE","INCOME","CHILDREN","FMLE_AGE","MALE_AGE","FMLE_EDU","MALE_EDU","FMLE_HRS","MALE_HRS","M_STATUS")
if(num_compare == 2){
	ggtmp	<- subset(hh, cls_grp %in% c(1,4))
	tmpf	<- paste(plot.wd, "/graph_mosaic_harbinger2.pdf", sep="")
}else{
	ggmp	<- subset(hh, cls_grp > 0)
	tmpf	<- paste(plot.wd, "/graph_mosaic_harbinger4.pdf", sep="")
}
plots <- list(NULL)
for(i in 1:length(selcol)){
	plots[[i]] <- makeplot_mosaic(ggtmp ,x="grp",y=selcol[i])
}

if(make_plot){
	pdf(tmpf,width=15,height=15)
	do.call(grid.arrange,c(plots,list(ncol=3)))
	dev.off()
}

# Test if a demongraphic is significantly correlated with affinity -- Regression test
for(i in 1:length(selcol)){
	myfml 	<- as.formula(paste("affinity ~ ", selcol[i], sep=""))
	cat("----------------------------------------------------------\n")
	print(summary(lm(myfml, data = hh)))
}

# Make mosaic plots for the subset of the demogrpahic variables that turn out to be significant
selcol1	<- c("FAMSIZE","INCOME","FMLE_EDU")
selcol2 <- c("FamilySize", "Income", "Education")
lapply(1:length(selcol1), function(i) levels(hh[,selcol1[i]]))
ggtmp 	<- data.frame(Group = ggtmp$grp, FamilySize = as.numeric(ggtmp$FAMSIZE), Income = as.numeric(ggtmp$INCOME), Education = as.numeric(ggtmp$FMLE_EDU))
ggtmp$FamilySize	<- ifelse(ggtmp$FamilySize >= 6, 6, ggtmp$FamilySize)
ggtmp$FamilySize 	<- factor(ggtmp$FamilySize, levels=1:6, labels=c("Single","Two people","Three people","Four people", "Five people","Six people or plus"))
ggtmp$Income		<- ifelse(ggtmp$Income <= 3, 3, ggtmp$Income)
ggtmp$Income		<- ifelse(ggtmp$Income == 13, NA, ggtmp$Income)
ggtmp$Income		<- factor(ggtmp$Income, 3:12, labels=c("$0 ~ $14,999", "15,000 ~ $19,999", "$20,000 ~ $24,999", "$25,000 ~ $34,999", 
						"$35,000 ~ $44,999","$45,000 ~ $54,999", "$55,000 ~ $64,999","$65,000 ~ $74,999","$75,000 ~ $99,999","$100,000 and above"))
ggtmp$Education		<- ifelse(ggtmp$Education<=3, 3, ggtmp$Education)
ggtmp$Education		<- ifelse(ggtmp$Education>=9, NA, ggtmp$Education)
ggtmp$Education		<- factor(ggtmp$Education, 3:8, labels=c("Some high school or less","Graduated high school","Technical school",
						"Some college","Graduated from college","Post graduate work"))

plots <- list(NULL)
for(i in 1:length(selcol1)){
	sel <- !is.na(ggtmp[,selcol2[i]])
	plots[[i]] <- makeplot_mosaic(ggtmp[sel,],x="Group",y=selcol2[i])
}

if(make_plot){
	pdf(paste(plot.wd,"/graph_demo_mosiac.pdf",sep=""), width=ww, height=ww)
	for(i in 1:length(plots)){
		print(plots[[i]])
	}
	dev.off()
}

########################
# Revenue contribution #
########################
rev		<- read.csv("chain_revenue_contribution.csv")

ggtmp 	<- melt(rev[,c("CHAIN_NAME","rev_contr1","rev_contr4")], id.var="CHAIN_NAME")
ggtmp$Group	<- ifelse(ggtmp$variable=="rev_contr1", "1st Quantile","4th Quantile")
tmp		<- unique(as.character(rev$CHAIN_NAME))
tmp		<- tmp[order(rev$rev_contr1)]
ggtmp$CHAIN_NAME	<- factor(ggtmp$CHAIN_NAME, levels=tmp)

if(make_plot){
	pdf(paste(plot.wd,"/graph_contribution.pdf",sep=""), width=6.5, height=5)
	print(ggplot(ggtmp, aes(x = CHAIN_NAME, y=value, fill=Group)) + geom_bar(stat="identity", position="dodge") + 
			coord_flip() + 
			labs(x = "Chain", y="Revenue contribution") + 
			scale_fill_grey() + 
			scale_y_continuous(labels= percent) + 
			theme_bw()
			)
	dev.off()
}

# Save excel workbook 
saveWorkbook(mywb, tab.file)

