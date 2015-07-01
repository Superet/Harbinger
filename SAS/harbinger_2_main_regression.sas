libname orig 	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Orig";
libname prod 	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Prod";
libname hh 		"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\HH";
libname trip	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Trip";
%let dirname= E:\Users\Projects\Project_Marketing_IRI\kelloggirir;
options compress=yes reuse=yes;

/* Use the previous product data; 
Two output data: 
1. logit_out: point estimates and standard errors of model; 
2. logit_outfit: model fitness. 
*/

proc sql noprint; 
select unique(fail_cutoff) into: fail_cutoff from PROD.new_prim; 
quit;
%put &fail_cutoff;
%let reg_outfile = %sysfunc(catx(%str(), E:\Users\ccv103\Desktop\result_2_SAS_reg_cutoff,&fail_cutoff,.csv));
%put &reg_outfile; 
%let regfit_outfile = %sysfunc(catx(%str(), E:\Users\ccv103\Desktop\result_2_SAS_regfit_cutoff,&fail_cutoff,.csv));
%put &regfit_outfile; 

********************;
* Manin regression *;
********************;
%ClassFn(HH_outname=hh_new_cls,trip_data=TRIP.trip_det,prod_data=PROD.new_full,class_window=52,num_group=4,repeat=);
%TestFn(outname=new_prod_test,HH_outname=hh_new_cls,trip_prod=trip_prod,test_prod_data=PROD.new_prim,test_window=52,num_group=4);

* Save the main classification; 
data HH.hh_new_cls; set hh_new_cls; fail_cutoff = &fail_cutoff; run;

* Check correlation between affinity index; 
proc corr data=hh_new_cls;
var affinity affinity2 affinity3 affinityl affinityl2 affinityl3;
run;

* Plot histogram of affinity; 
proc univariate data=hh_new_cls; 
	var affinity affinity2 affinity3 affinityl affinityl2 affinityl3;
	histogram;
run;

data new_prod_test;
	set new_prod_test;
	logP = log(PRICE);
run;

* Check missing sales;
proc sql;
	select count(UPC) as N_nosale from new_prod_test where total_sale=0;
	select Nmiss(logP) as Miss_price from new_prod_test;
quit;


*** Simple logistic regression with no correction of se. ***;
ods trace on;
ods output ParameterEstimates=out_data1;
ods output FitStatistics = outfit_data1;
ods output ROCAssociation = out_roc1;
proc logistic data=new_prod_test descending;
	model success = total_sale;
	roc; 
run;
ods trace off;

proc logistic data=new_prod_test descending;
	model success = grp1 grp2 grp3 grp4 grp0;
	roc; 
	ods output ParameterEstimates=out_data2 FitStatistics = outfit_data2 ROCAssociation = out_roc2; 		
	output out = pred0 p=p;
run;

* Collect parameter estimates;
data out_data1; 	set out_data1; model = 'Baseline'; 
data outfit_data1; 	set outfit_data1; model = 'Baseline'; run;
data out_roc1; 		set out_roc1(where = (ROCModel = "Model")); model = "Baseline"; 
data out_data2; 	set out_data2; model = 'Affinity'; 
data outfit_data2; 	set outfit_data2; model = 'Affinity'; run;
data out_roc2; 		set out_roc2(where = (ROCModel = "Model")); model = "Affinity"; run;
data logit_out; 
	length model $ 30;
	set out_data1 out_data2; 
data logit_outfit; 
	length model $ 30;
	set outfit_data1 outfit_data2; 
data logit_roc; 
	length model $ 30; 
	set out_roc1 out_roc2; 
run;
proc sort data = logit_out; by model; run;
proc sort data = logit_outfit; by model; run; 
proc sort data = logit_roc; by model; run; 

proc datasets noprint nodetails; delete _doctmp: ; run; 

* ROC curve comparison;
ods graphics on;
proc logistic data=new_prod_test plots = roc(id=prob);
	model success(event='1') = grp1 grp2 grp3 grp4 grp0 total_sale /nofit;
	roc 'Baseline' total_sale;
	roc 'Separate sale' grp1 grp2 grp3 grp4 grp0;
	roccontrast reference('Baseline') / estimate e;
run;
ods graphics off;

* Marginal effects; 
data out_data2;
	set out_data2 end = eof;
	if eof and Variable = 'Intercept' then delete; 
run;

proc transpose data=out_data2 out=tlog(rename = (grp0 = tgrp0 grp1=tgrp1 grp2=tgrp2 grp3=tgrp3 grp4=tgrp4)); 
	var estimate;
	id variable; 
run;

data pred0;
	if _n_ = 1 then set tlog; 
	set pred0;
	ME_grp1 = p*(1-p)*tgrp1;
	ME_grp4 = p*(1-p)*tgrp4;
run;

proc means data=pred0;
	var ME_grp1 ME_grp4; 
run;

* Marginal effects of 10% sales increase; 
data tmp;
	set new_prod_test;
	grp4 = grp4*1.1; 
run;

proc logistic data=new_prod_test descending noprint;
	model success = grp1 grp2 grp3 grp4 grp0;
	score data=tmp out = pred1; 
run;
proc sort data = pred0; by UPC; 
proc sort data = pred1; by UPC; 
run; 

data tmp;
	merge pred0 pred1(keep = UPC p_1);
	by UPC;
	pchange = p_1 - p; 
proc sort; by success; 
run;
proc means data = tmp;
	*by success; 
	var pchange; 
run;

proc datasets noprint; delete pred0 pred1 tlog out_data1 out_data2 outfit_data1 outfit_data2 out_roc1 out_roc2 _doctmp: ; run;

********************;
* Robustness check *;
********************;
/*
1. Cat-clustering: Main regression with category-clustering standard error; 
2. Control:	Main regression with product control; (NOTE that less observations b/c missing price)
3. RanEf: category-specific random effect.
4. Hazard: model product life.
5. Affinity2: Household classification using repeat (2) purchases;
6. Affinity3: Household classification using repeat (3) purchases;
7. DropCandy: Drop candy category;  
8. HalfWindow: Limit to the classification window to 6-12 months after introduction; 
9. TwoCriteria: Use number of purchased new products and affinity to segment consumers; 
*/

*** Category-clustered model ***;
* Base model ;
proc genmod data=new_prod_test descending;
	class CATEGORY ;
	model success = total_sale/link=logit dist=binomial;
	repeated subject=CATEGORY / type = cs;
	store out1;
run;
ods output ParameterEstimates = out_data1;
proc plm source = out1;
	show parameters;
run;

* Separate sales by group model ;
proc genmod data=new_prod_test descending;
	class CATEGORY ;
	model success = grp1 grp2 grp3 grp4 grp0/link=logit dist=binomial;
	repeated subject=CATEGORY / type = cs;
	store out1;
run;
ods output ParameterEstimates = out_data2;
proc plm source = out1;
	show parameters;
run;

* Collect parameter estimates;
data out_data2; 	length model $ 30; set out_data2(rename=(Parameter=Variable)); model = 'Cat-cluster'; run;
proc sort data=logit_out; by model; run;
data logit_out;  merge logit_out out_data2; by model; run;

proc datasets noprint; delte out_data2; run; 

*-------------------------------------------------------------;
* Robustness check * ;
* Add product control to the main regression; 
proc logistic data=new_prod_test descending;
	class PRVT_LAB; 
	model success = grp1 grp2 grp3 grp4 grp0 PRVT_LAB logP promotion pct_AVC;
	roc; 
	ods output ParameterEstimates=out_data2 FitStatistics = outfit_data2 ROCAssociation = out_roc2; 
run;

* Collect parameter estimates;
data out_data2; 	length model $ 30; set out_data2; model = 'Control'; 
data outfit_data2; 	length model $ 30; set outfit_data2; model = 'Control'; 
data out_roc2; 		length model $ 30; set out_roc2(where = (ROCModel = "Model")); model = "Control"; 
run;
data logit_out; merge logit_out out_data2; by model;
proc sort; by model; 
run;
data logit_outfit; merge logit_outfit outfit_data2; by model;
proc sort; by model;
run;
data logit_roc; merge logit_roc out_roc2;  by model;
proc sort; by model;
run;

proc datasets noprint; delete out_data2 outfit_data2 out_roc2 _doctmp:; run;

*-------------------------------------------------------------;
* Robustness check * ;
* Category random effect ; 
proc glimmix data=new_prod_test;
	class CATEGORY;
	model success(event='1') = grp1 grp2 grp3 grp4 grp0 / link=logit dist=binary solution;
	random intercept / sub = CATEGORY; 
	ods output ParameterEstimates = out_data2 FitStatistics=outfit_data2; 
	output out = glmmout pred=xbeta pred(ilink)=predprob; 
run;

proc logistic data=glmmout descending; 
	model success = predprob;
	roc; 
	ods output ROCAssociation = out_roc2;
run;

* Collect parameter estimates;
data out_data2; 	length model $ 30;set out_data2(rename=(Effect=Variable)); model = 'RanEf'; 
data outfit_data2; 	length model $ 30;set outfit_data2(rename=(Descr=Criterion Value=InterceptAndCovariates)); model = 'RanEf'; run;
data out_roc2; 		length model $ 30; set out_roc2(where = (ROCModel = "Model")); model = "RanEf"; 
run;
data logit_out; merge logit_out out_data2; by model;
proc sort; by model; 
data logit_outfit; merge logit_outfit outfit_data2; by model;
proc sort; by model;
data logit_roc; merge logit_roc out_roc2; by model;
proc sort; by model; 
run;

proc datasets noprint; delete out_data2 outfit_data2 out_rox2 _doctmp: ; run;

*-------------------------------------------------------------;
* Robustness check * ;
* Model product life; 
ods graphics on; 
proc phreg data=new_prod_test plots=survival;
	model LIFE = grp1 grp2 grp3 grp4 grp0;
	ods output ParameterEstimates = out_data2 FitStatistics=outfit_data2; 
run;
ods graphics off; 

* Collect parameter estimates;
data out_data2; 	length model $ 30;set out_data2(rename=(Parameter=Variable)); model = 'Hazard'; 
data outfit_data2; 	length model $ 30;set outfit_data2(rename=(WithoutCovariates=InterceptOnly WithCovariates=InterceptAndCovariates)); model = 'Hazard'; run;
proc sort data=logit_out; by model; 
proc sort data=logit_outfit; by model; run;
data logit_out; 
	merge logit_out out_data2; by model;
proc sort; by model; 
data logit_outfit; 
	merge logit_outfit outfit_data2; by model;
proc sort; by model; run;
proc datasets noprint; delete out_data2 outfit_data2; run;

*-------------------------------------------------------------;
* Robustness check * ;
* Use repeat (2) purchase as household grouping criteria; 
%ClassFn(HH_outname=hh_new_cls,trip_data=TRIP.trip_det,prod_data=PROD.new_full,class_window=52,num_group=4,repeat=2);
%TestFn(outname=new_prod_test,HH_outname=hh_new_cls,trip_prod=trip_prod,test_prod_data=PROD.new_prim,test_window=52,num_group=4);

data new_prod_test;
	set new_prod_test;
	logP = log(PRICE);
run;

* Check missing sales;
proc sql;
	select count(UPC) 
	from new_prod_test where total_sale=0;
quit;

** Simple logistic regression with no correction of se. ***;
proc logistic data=new_prod_test descending;
	model success = grp1 grp2 grp3 grp4 grp0;
	roc; 
	ods output ParameterEstimates=out_data2 FitStatistics = outfit_data2 ROCAssociation = out_roc2; 	
run;

* Collect parameter estimates;
data out_data2; 	length model $ 30; set out_data2; model = 'Affinity2'; 
data outfit_data2; 	length model $ 30; set outfit_data2; model = 'Affinity2'; run;
data out_roc2; 		length model $ 30; set out_roc2(where = (ROCModel = "Model")); model = "Affinity2"; 

data logit_out; merge logit_out out_data2; by model; 
proc sort; by model;
data logit_outfit; merge logit_outfit outfit_data2; by model; 
proc sort; by model;
data logit_roc; merge logit_roc out_roc2; by model;
proc sort; by model;
run;

proc datasets noprint; delete out_data2 outfit_data2 out_roc2 _doctmp: ; run;

*-------------------------------------------------------------;
* Robustness check * ;
* Use repeat (3) purchase as household grouping criteria;
%ClassFn(HH_outname=hh_new_cls,trip_data=TRIP.trip_det,prod_data=PROD.new_full,class_window=52,num_group=4,repeat=3);
%TestFn(outname=new_prod_test,HH_outname=hh_new_cls,trip_prod=trip_prod,test_prod_data=PROD.new_prim,test_window=52,num_group=4);

data new_prod_test;
	set new_prod_test;
	logP = log(PRICE);
run;

* Check missing sales;
proc sql;
	select count(UPC) 
	from new_prod_test where total_sale=0;
quit;

** Simple logistic regression with no correction of se. ***;
proc logistic data=new_prod_test descending;
	model success = grp1 grp2 grp3 grp4 grp0;
	roc; 
	ods output ParameterEstimates=out_data2 FitStatistics = outfit_data2 ROCAssociation = out_roc2; 	
run;

* Collect parameter estimates;
data out_data2; length model $ 30; set out_data2; model = 'Affinity3'; run;
data outfit_data2; 	length model $ 30; set outfit_data2; model = 'Affinity3'; run;
data out_roc2; 		length model $ 30; set out_roc2(where = (ROCModel = "Model")); model = "Affinity3"; 
data logit_out; merge logit_out out_data2; by model; 
proc sort; by model;
data logit_outfit; merge logit_outfit outfit_data2; by model; 
proc sort; by model;
data logit_roc; merge logit_roc out_roc2; by model;
proc sort; by model;
run;

proc datasets noprint; delete out_data2 outfit_data2 out_roc2 _doctmp: ; run;

*-------------------------------------------------------------;
* Robustness check * ;
* Use affinity weighted by lifetime as grouping criteria;
%ClassFn(HH_outname=hh_new_cls,trip_data=TRIP.trip_det,prod_data=PROD.new_full,class_window=52,num_group=4,repeat=l);
%TestFn(outname=new_prod_test,HH_outname=hh_new_cls,trip_prod=trip_prod,test_prod_data=PROD.new_prim,test_window=52,num_group=4);

data new_prod_test;
	set new_prod_test;
	logP = log(PRICE);
run;

* Check missing sales;
proc sql;
	select count(UPC) 
	from new_prod_test where total_sale=0;
quit;

** Simple logistic regression with no correction of se. ***;
proc logistic data=new_prod_test descending;
	model success = grp1 grp2 grp3 grp4 grp0;
	roc; 
	ods output ParameterEstimates=out_data2 FitStatistics = outfit_data2 ROCAssociation = out_roc2; 	
run;

* Collect parameter estimates;
data out_data2; length model $ 30; set out_data2; model = 'Affinityl'; run;
data outfit_data2; 	length model $ 30; set outfit_data2; model = 'Affinityl'; run;
data out_roc2; 		length model $ 30; set out_roc2(where = (ROCModel = "Model")); model = 'Affinityl'; 

data logit_out; merge logit_out out_data2; by model; 
proc sort; by model;
data logit_outfit; merge logit_outfit outfit_data2; by model; 
proc sort; by model;
data logit_roc; merge logit_roc out_roc2; by model;
proc sort; by model;
run;

proc datasets noprint; delete out_data2 outfit_data2 out_roc2 _doctmp: ; run;

*-------------------------------------------------------------;
* Robustness check * ;
* Use repeat (2) purchase weighted by lifetime as household grouping criteria;
%ClassFn(HH_outname=hh_new_cls,trip_data=TRIP.trip_det,prod_data=PROD.new_full,class_window=52,num_group=4,repeat=l2);
%TestFn(outname=new_prod_test,HH_outname=hh_new_cls,trip_prod=trip_prod,test_prod_data=PROD.new_prim,test_window=52,num_group=4);

data new_prod_test;
	set new_prod_test;
	logP = log(PRICE);
run;

* Check missing sales;
proc sql;
	select count(UPC) 
	from new_prod_test where total_sale=0;
quit;

** Simple logistic regression with no correction of se. ***;
proc logistic data=new_prod_test descending;
	model success = grp1 grp2 grp3 grp4 grp0;
	roc; 
	ods output ParameterEstimates=out_data2 FitStatistics = outfit_data2 ROCAssociation = out_roc2; 	
run;

* Collect parameter estimates;
data out_data2; length model $ 30; set out_data2; model = 'Affinityl2'; run;
data outfit_data2; 	length model $ 30; set outfit_data2; model = 'Affinityl2'; run;
data out_roc2; 		length model $ 30; set out_roc2(where = (ROCModel = "Model")); model = "Affinityl2"; 
data logit_out; merge logit_out out_data2; by model; 
proc sort; by model;
data logit_outfit; merge logit_outfit outfit_data2; by model; 
proc sort; by model;
data logit_roc; merge logit_roc out_roc2; by model;
proc sort; by model;
run;

proc datasets noprint; delete out_data2 outfit_data2 out_roc2 _doctmp: ; run;

*-------------------------------------------------------------;
* Robustness check * ;
* Use repeat (3) purchase weighted by lifetime as household grouping criteria;
%ClassFn(HH_outname=hh_new_cls,trip_data=TRIP.trip_det,prod_data=PROD.new_full,class_window=52,num_group=4,repeat=l3);
%TestFn(outname=new_prod_test,HH_outname=hh_new_cls,trip_prod=trip_prod,test_prod_data=PROD.new_prim,test_window=52,num_group=4);

data new_prod_test;
	set new_prod_test;
	logP = log(PRICE);
run;

* Check missing sales;
proc sql;
	select count(UPC) 
	from new_prod_test where total_sale=0;
quit;

** Simple logistic regression with no correction of se. ***;
proc logistic data=new_prod_test descending;
	model success = grp1 grp2 grp3 grp4 grp0;
	roc; 
	ods output ParameterEstimates=out_data2 FitStatistics = outfit_data2 ROCAssociation = out_roc2; 	
run;

* Collect parameter estimates;
data out_data2; length model $ 30; set out_data2; model = 'Affinityl3'; run;
data outfit_data2; 	length model $ 30; set outfit_data2; model = 'Affinityl3'; run;
data out_roc2; 		length model $ 30; set out_roc2(where = (ROCModel = "Model")); model = "Affinityl3"; 
data logit_out; merge logit_out out_data2; by model; 
proc sort; by model;
data logit_outfit; merge logit_outfit outfit_data2; by model; 
proc sort; by model;
data logit_roc; merge logit_roc out_roc2; by model;
proc sort; by model;
run;

proc datasets noprint; delete out_data2 outfit_data2 out_roc2 _doctmp: ; run;

*-------------------------------------------------------------;
* Robustness check * ;
* Drop entire candy category;
data new_prim; set PROD.new_prim; if CATEGORY ^= 'CATEGORY - TOTAL NON-CHOCOLATE CANDY'; run;
data new_full; set PROD.new_full; if CATEGORY ^= 'CATEGORY - TOTAL NON-CHOCOLATE CANDY'; run;

%ClassFn(HH_outname=hh_new_cls,trip_data=TRIP.trip_det,prod_data=new_full,class_window=52,num_group=4,repeat=);
%TestFn(outname=new_prod_test,HH_outname=hh_new_cls,trip_prod=trip_prod,test_prod_data=new_prim,test_window=52,num_group=4);

** Simple logistic regression with no correction of se. ***;
proc logistic data=new_prod_test descending;
	model success = grp1 grp2 grp3 grp4 grp0;
	roc; 
	ods output ParameterEstimates=out_data2 FitStatistics = outfit_data2 ROCAssociation = out_roc2; 	
run;

* Collect parameter estimates;
data out_data2; length model $ 30; set out_data2; model = 'DropCandy'; run;
data outfit_data2; 	length model $ 30; set outfit_data2; model = 'DropCandy'; run;
data out_roc2; 		length model $ 30; set out_roc2(where = (ROCModel = "Model")); model = "DropCandy"; 
data logit_out; merge logit_out out_data2; by model; 
proc sort; by model;
data logit_outfit; merge logit_outfit outfit_data2; by model; 
proc sort; by model;
data logit_roc; merge logit_roc out_roc2; by model;
proc sort; by model;
run;

proc datasets noprint; delete new_prim new_full out_data2 outfit_data2 out_roc2 _doctmp: ; run;

*-------------------------------------------------------------;
* Robustness check * ;
* Limit the classification window to 6-12 months after product introduction; 
%ClassFn(HH_outname=hh_new_cls,trip_data=TRIP.trip_det,prod_data=PROD.new_full,class_window=52,num_group=4,repeat=, class_window_low = 26);
%TestFn(outname=new_prod_test,HH_outname=hh_new_cls,trip_prod=trip_prod,test_prod_data=PROD.new_prim,test_window=52,num_group=4, test_window_low = 26);

* Simple logistic regression with no correction of se. ***;
proc logistic data=new_prod_test descending;
	model success = grp1 grp2 grp3 grp4 grp0;
	roc;
	ods output ParameterEstimates=out_data2 FitStatistics = outfit_data2 ROCAssociation = out_roc2; 	
run;

* Collect parameter estimates;
data out_data2; length model $ 30; set out_data2; model = 'HalfWindow'; run;
data outfit_data2; 	length model $ 30; set outfit_data2; model = 'HalfWindow'; run;
data out_roc2; 		length model $ 30; set out_roc2(where = (ROCModel = "Model")); model = "HalfWindow"; 
data logit_out; merge logit_out out_data2; by model; 
proc sort; by model;
data logit_outfit; merge logit_outfit outfit_data2; by model; 
proc sort; by model;
data logit_roc; merge logit_roc out_roc2; by model;
proc sort; by model;
run;

proc datasets noprint; delete out_data2 outfit_data2 out_roc2 _doctmp: ; run;

*-------------------------------------------------------------;
* Robustness check * ;
* Use number of purchased new products and affinity to segment consumers; 
* First segment 2 groups using affinity; 
%ClassFn(HH_outname=hh1,trip_data=TRIP.trip_det,prod_data=PROD.new_full,class_window=52,num_group=2,repeat=);
* Then segment another 2 groups num_allnew; 
proc univariate data=hh1 noprint; 
	var num_allnew; 
	output out=tmp1 pctlpts = 50 to 100 by 50 pctlpre = num_P;
run;
proc sql noprint; select num_P50 into:group_thresh from tmp1; quit; 
%put &group_thresh; 

data hh_new_cls; 
	set hh1(rename = (cls_grp = grp_aff)); 
	grp_num = 0; 
	if num_allnew > 0 and num_allnew <= &group_thresh then grp_num = 1; 
	if num_allnew > &group_thresh then grp_num = 2; 
	cls_grp = 0; 
	if grp_num = 1 then do; 
		if grp_aff <= 1 then cls_grp = 1; 
		if grp_aff = 2 then cls_grp = 2; 
	end; 
	else if grp_num = 2 then do; 
		if grp_aff <= 1 then cls_grp = 3; 
		if grp_aff = 2 then cls_grp = 4; 
	end; 
run;
proc freq data= hh_new_cls; tables grp_num grp_aff cls_grp; run;

%TestFn(outname=new_prod_test,HH_outname=hh_new_cls,trip_prod=trip_prod,test_prod_data=PROD.new_prim,test_window=52,num_group=4);

* Simple logistic regression with no correction of se. ***;
proc logistic data=new_prod_test descending;
	model success = grp1 grp2 grp3 grp4 grp0;
	roc; 
	ods output ParameterEstimates=out_data2 FitStatistics = outfit_data2 ROCAssociation = out_roc2; 
run;

* Collect parameter estimates;
data out_data2; length model $ 30; set out_data2; model = 'TwoCriteria'; run;
data outfit_data2; 	length model $ 30; set outfit_data2; model = 'TwoCriteria'; run;
data out_roc2; 		length model $ 30; set out_roc2(where = (ROCModel = "Model")); model = "TwoCriteria"; 
data logit_out; merge logit_out out_data2; by model; 
proc sort; by model;
data logit_outfit; merge logit_outfit outfit_data2; by model; 
proc sort; by model;
data logit_roc; merge logit_roc out_roc2; by model;
proc sort; by model;
run;

proc datasets noprint; delete out_data2 outfit_data2 out_roc2 _doctmp:; run;

* Combine together fitness and roc;
proc transpose data = logit_outfit(where = (Equals='')) out=tmp(drop = _NAME_ _label_); 
	by model;
	id criterion; 
	var InterceptAndCovariates; 
run;

proc sql noprint; 
	create table logit_fit(drop = old_model) as 
	select *
	from tmp(keep = model AIC SC _2_Log_L) as A full join logit_roc(rename = (model = old_model ) drop=ROCModel) as B
	on A.model = B.old_model 
	order by model; 
quit; 

* Drop the parameters from the models with intercept only; 
data logit_out;
	set logit_out; 
	by model; 
	if last.model and variable = 'Intercept' then delete; 
run;


* Delete datasets; 
proc datasets noprint; delete hh_new_cls tmp tmp1 tmp2 glmmout hh1 tmp_trip new_prim new_full; run;

******************;
* Export results *;
******************;
PROC EXPORT DATA= logit_out
            OUTFILE= "&reg_outfile" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA= logit_fit
            OUTFILE= "&regfit_outfile" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;


