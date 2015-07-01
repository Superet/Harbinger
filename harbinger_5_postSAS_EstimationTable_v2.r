library(xlsx)
library(ggplot2)
library(data.table)
library(gridExtra)
library(reshape2)
library(r2excel)

setwd("~/Documents/Research/Harbinger/processed data")
my.wd <- "~/Documents/Research/Harbinger/processed data"
iri.wd <- "~/Documents/Research/Data/IRI/kelloggirir"
source("../Exercise/outreg function.R")

# Initiate exporting xlsx 
plot.wd <- "/Users/chaoqunchen/Desktop"
tab.file<- paste(plot.wd, "/harbinger_5_postSAS.xlsx",sep="")
mywb	<- createWorkbook()
sht1	<- createSheet(mywb, "MainLogit")
sht2	<- createSheet(mywb, "CrossCategory")
sht3	<- createSheet(mywb, "Testing")

###################################
# Main logit and Robustness check #
###################################
mycut	<- 4
est 	<- read.csv(paste("result_2_regression_cutoff",mycut, ".csv",sep=""))
est		<- subset(est, !model %in% c("Hazard","Cat-cluster","Baseline"))
sel		<- est[est$model=="Affinity","Variable"]
est		<- subset(est, Variable %in% sel)
est$ProbChiSq <- as.character(est$ProbChiSq)
sel		<- est$Probt!=''
est[sel,"ProbChiSq"] <- as.character(est[sel,"Probt"])
est$pvalue	<- as.numeric(gsub("<","", est$ProbChiSq))

# The results if we segment 4 groups
sel		<- est$model %in% c("Affinity22", "Affinity32", "Affinityl22", "Affinityl32")
tmp		<- split(est[!sel,], as.character(est[!sel,"model"]))
tmp1	<- model_outreg(model.list=tmp, p.given=TRUE, varlab=as.character(tmp[[1]]$Variable), digits=4, head.name=c("Estimate", "StdErr","pvalue"))
tmp1	<- data.frame(Cutoff = mycut, Num_group = 4, tmp1)
# tmp		<- split(est[sel,], as.character(est[sel,"model"]))
# tmp2	<- model_outreg(model.list=tmp, p.given=TRUE, varlab=as.character(tmp[[1]]$Variable), digits=4, head.name=c("Estimate", "StdErr","pvalue"))
# tmp2	<- data.frame(Cutoff = mycut, Num_group = 2, tmp2)

xlsx.addHeader(mywb, sht1, value = "Main logit regression", level = 2, underline = 1)
xlsx.addLineBreak(sht1, 1)
xlsx.addTable(mywb, sht1, tmp1, row.names = FALSE)

########################################
# Cross category analysis organization #
########################################
num_group	<- 4
filename	<- paste("result_3_cross_grp",num_group, ".csv",sep="")
cross_est	<- read.csv(filename)
dpt			<- unique(cross_est$DEPARTMENT_Class)

# Change the the p-value from factor to numeric
cross_est$pvalue <- as.numeric(gsub("<","",as.character(cross_est$ProbChiSq)))
for(i in 1:length(dpt)){
	tmp		<- subset(cross_est, DEPARTMENT_Class==dpt[i])
	sel		<- duplicated(tmp[,1:4])
	tmp		<- tmp[!sel,]
	tmp1	<- split(tmp, tmp$DEPARTMENT_Test)
	tmp2	<- model_outreg(model.list=tmp1, p.given=TRUE, varlabs=as.character(tmp1[[1]]$Variable), digits=4, head.name=c("Estimate", "StdErr","pvalue"))
	tmp2	<- cbind(Class_category = dpt[i], tmp2)
	xlsx.addHeader(mywb, sht2, value = paste("Classification department ", dpt[i], sep=""), level = 2, underline = 1)
	xlsx.addLineBreak(sht2, 1)
	xlsx.addTable(mywb, sht2, tmp2, row.names = FALSE)
	xlsx.addLineBreak(sht2, 1)
}

#######################################
# Regression of behavioral difference # 
#######################################
mycut		<- 4
filename	<- paste("result_4_sas_reg_cutoff",mycut, ".csv", sep="")
res_reg		<- read.csv(filename, stringsAsFactors=FALSE)

# --------------------------------------------------------------- #
# Regressions of group dummy as independent variable
res_reg1 	<- subset(res_reg, data_level != "HH" & IV == "group")
res_reg1$Parameter <- gsub(" ","", res_reg1$Parameter)
unique(res_reg1$Parameter)

# Regression at the category level
tmp_type	<- unique(as.character(res_reg1$data_level)); tmp_type
sel			<- res_reg1$data_level == tmp_type[1]
tmp_model	<- split(res_reg1[sel,], res_reg1[sel,"Dependent"])
tmp_model	<- lapply(tmp_model, function(x) subset(x, Parameter!="num_purchase"))
tmp.tab		<- model_outreg(tmp_model, p.given=TRUE, head=c("Estimate","StdErr","Probt","Parameter"), digits = 4)
tmp.tab
xlsx.addHeader(mywb, sht3, value = "Regression at the category level:", level = 2, underline = 1)
xlsx.addTable(mywb, sht3, tmp.tab, row.names = FALSE)
xlsx.addLineBreak(sht3, 1)

# Regression at the UPC level 
sel			<- res_reg1$data_level %in% tmp_type[c(2,3)]
tmp_model	<- split(res_reg1[sel,], res_reg1[sel,"Dependent"])
tmp.tab		<- model_outreg(tmp_model, p.given=TRUE, head=c("Estimate","StdErr","Probt", "Parameter"), digits = 4)
tmp.tab$Variable <- gsub("\\s+"," ", tmp.tab$Variable)
tmp.tab
xlsx.addHeader(mywb, sht3, value = "Regression at the UPC level:", level = 2, underline = 1)
xlsx.addTable(mywb, sht3, tmp.tab, row.names = FALSE)
xlsx.addLineBreak(sht3, 1)

# Regression at the HH-weekly level 
# Regression at the UPC level 
sel			<- res_reg1$data_level %in% tmp_type[4]
tmp_model	<- split(res_reg1[sel,], res_reg1[sel,"Dependent"])
tmp.tab		<- model_outreg(tmp_model, p.given=TRUE, head=c("Estimate","StdErr","Probt", "Parameter"), digits = 4)
tmp.tab
xlsx.addHeader(mywb, sht3, value = "Regression at the HH-weekly level:", level = 2, underline = 1)
xlsx.addTable(mywb, sht3, tmp.tab, row.names = FALSE)
xlsx.addLineBreak(sht3, 1)

# --------------------------------------------------------------- #
# Regressions of affinity index as independent variable
res_reg2 			<- subset(res_reg, data_level != "HH" & IV == "affinity")
res_reg2$Parameter 	<- gsub(" ","", res_reg2$Parameter)
unique(res_reg2$Parameter)
res_reg2			<- subset(res_reg2, Parameter == "affinity")
xlsx.addHeader(mywb, sht3, value = "Coefficients of affinity in the regression test.", level = 2, underline = 1)
xlsx.addTable(mywb, sht3, tmp.tab, row.names = FALSE)
xlsx.addLineBreak(sht3, 1)

######################
# Hypothesis testing #
######################
mycut		<- 4
filename	<- paste("result_4_sas_test_cutoff",mycut, ".csv", sep="")
res_test	<- read.csv(filename, stringsAsFactors=FALSE)
tmp.csv		<- "~/Desktop/restult_4_test.csv"

selcol 		<- c("X_VAR_","tValue","Probt","P2_DATA","P2_MED","lab")
sel			<- sapply(1:length(selcol), function(x) is.numeric(res_test[, x]))
res_test[,selcol]

xlsx.addHeader(mywb, sht3, value = "T-test", level = 2, underline = 1)
xlsx.addTable(mywb, sht3, res_test[,selcol], row.names = FALSE)
xlsx.addLineBreak(sht3, 1)

saveWorkbook(mywb, tab.file)

