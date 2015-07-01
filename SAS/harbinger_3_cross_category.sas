/* Use the previous product data; 
Two output data: 
1. logit_out: point estimates and standard errors of model; 
2. logit_outfit: model fitness. 
*/

libname orig 	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Orig";
libname prod 	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Prod";
libname hh 		"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\HH";
libname trip	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Trip";
%let dirname= E:\Users\Projects\Project_Marketing_IRI\kelloggirir;
options compress=yes reuse=yes;

proc sql noprint; 
select unique(fail_cutoff) into: fail_cutoff from PROD.new_prim; 
quit;
%put &fail_cutoff;
%let reg_outfile = %sysfunc(catx(%str(), E:\Users\ccv103\Desktop\result_3_SAS_crosscat_cutoff,&fail_cutoff,.csv));
%put &reg_outfile; 

************;
* Function *;
************;
* Test the harbinger section by product department;
%macro DptmtClssTest(dptmt_data,outname,HH_outname,trip_data,prod_data,test_prod_data,class_window,test_window,num_group,repeat=);
	proc sql noprint;
		select count(department_group) into: num_dpt
		from &dptmt_data;
	quit;

	%do i=1 %to &num_dpt;
	* Take classification category;
		proc sql noprint; 
			select DEPARTMENT into:dpt_cls separated by ' ' from &dptmt_data where DEPARTMENT_GROUP = &i; 
			select DEPARTMENT_GROUP_DSC into:dpt_cls_dsc from &dptmt_data where DEPARTMENT_GROUP=&i;
		quit;
		
		proc sql noprint;
			create table prod_data1 as 
			select *
			from &prod_data
			where DEPARTMENT in (&dpt_cls);
		quit;
		
		%let HH_outname1 = &HH_outname&i;
		%ClassFn(HH_outname=&HH_outname1,trip_data=&trip_data,prod_data=prod_data1,class_window=&class_window,num_group=&num_group,repeat=&repeat);
		data &HH_outname1;
			set &HH_outname1;
			DEPARTMENT_Class = "&dpt_cls_dsc";
		run;
	%end;
	
	%do j=1 %to &num_dpt;
		%let trip_prod1 = trip_prod&j;
		%let test_prod_data1 = test_prod_data&j;
		proc sql noprint;
			select DEPARTMENT into:dpt_test from &dptmt_data where DEPARTMENT_GROUP = &j; 
			select DEPARTMENT_GROUP_DSC into:dpt_test_dsc from &dptmt_data where DEPARTMENT_GROUP=&j;
			
			create table prod_data1 as
			select *
			from &prod_data 
			where DEPARTMENT in (&dpt_test);

			create table &trip_prod1 as
			select A.*,B.PRIM_UPC,B.fail,B.UPC_fail,B.WFM,B.class,B.test,B.PRIM_WFM
			from &trip_data as A inner join prod_data1 as B
			on A.UPC = B.UPC
			order by PANID,PRIM_UPC;
			
			create table &test_prod_data1 as
			select *
			from &test_prod_data
			where DEPARTMENT in (&dpt_test);
		quit;
	%end;
%mend DptmtClssTest;

%macro DptmtLoop(dptmt_data,outname,HH_outname,trip_data,prod_data,test_prod_data,class_window,test_window,num_group,repeat=);
	* Initiate the dataset to store regresssion results;
	data logit_out;
		length DEPARTMENT_Class $30 DEPARTMENT_Test $30 model $30 
			   Variable $15 DF 8 Estimate 8 StdErr 8 WaldChiSq 8 ProbChiSq 8; 
		stop;
	data logit_outfit; 
		length DEPARTMENT_Class $30 DEPARTMENT_Test $30 model $30 Criterion $9 InterceptOnly 8 InterceptAndCovariates 8 ;
		stop;
	data logit_res;
		length DEPARTMENT_Class $30 DEPARTMENT_Test $30 model $30 OrderedValue 8 Outcome $8 Count 8; 
		stop;
	run;

	proc sql noprint;
		select count(department_group) into: num_dpt
		from &dptmt_data;
	quit;
	
	%do i=1 %to &num_dpt;
		%let HH_outname1 = &HH_outname&i;
		proc sql noprint; 
			select DEPARTMENT into:dpt_cls from &dptmt_data where DEPARTMENT_GROUP = &i; 
			select DEPARTMENT_GROUP_DSC into:dpt_cls_dsc from &dptmt_data where DEPARTMENT_GROUP=&i;
		quit;
		
		%do j=1 %to &num_dpt;
			* Create test product data;
			proc sql noprint;
				select DEPARTMENT into:dpt_test from &dptmt_data where DEPARTMENT_GROUP = &j; 
				select DEPARTMENT_GROUP_DSC into:dpt_test_dsc from &dptmt_data where DEPARTMENT_GROUP=&j;
			quit;
			%let outname1 = prod_test_reg;
			%let trip_prod1	= trip_prod&j;
			%let test_prod_data1 = test_prod_data&j;
			
			%TestFn(outname=&outname1, HH_outname=&HH_outname1,trip_prod=&trip_prod1,
			test_prod_data=&test_prod_data1,test_window=&test_window,num_group=&num_group);
						
			*** Simple logistic regression ***;
			ods trace on;
			ods output ParameterEstimates=out_data1;
			ods output FitStatistics = outfit_data1;
			ods output ResponseProfile = outres_data1;
			proc logistic data=&outname1 descending;
				model success = total_sale;
			run;
			ods trace off;

			ods trace on;
			ods output ParameterEstimates=out_data2;
			ods output FitStatistics = outfit_data2;
			ods output ResponseProfile = outres_data2;
			proc logistic data=&outname1 descending;
				model success = grp0 - grp&num_group;
			run;
			ods trace off;
		
			* Collect parameter estimates;
			data out_data1; 	
				length DEPARTMENT_Class $30 DEPARTMENT_Test $30 model $30 Variable $15 DF 8 Estimate 8 StdErr 8 WaldChiSq 8 ProbChiSq 8;
				set out_data1; model = 'Baseline'; DEPARTMENT_Class = "&dpt_cls_dsc"; DEPARTMENT_Test = "&dpt_test_dsc";
			data outfit_data1; 	
				length DEPARTMENT_Class $30 DEPARTMENT_Test $30 model $30 Criterion $9 InterceptOnly 8 InterceptAndCovariates 8 ;
				set outfit_data1; model = 'Baseline'; DEPARTMENT_Class = "&dpt_cls_dsc"; DEPARTMENT_Test = "&dpt_test_dsc";
			data outres_data1;	
				length DEPARTMENT_Class $30 DEPARTMENT_Test $30 model $30 OrderedValue 8 Outcome $8 Count 8; 
				set outres_data1;  model = 'Baseline'; DEPARTMENT_Class = "&dpt_cls_dsc"; DEPARTMENT_Test = "&dpt_test_dsc";
			
			data out_data2; 	
				length DEPARTMENT_Class $30 DEPARTMENT_Test $30 model $30 Variable $15 DF 8 Estimate 8 StdErr 8 WaldChiSq 8 ProbChiSq 8;
				set out_data2; model = 'Affinity'; DEPARTMENT_Class = "&dpt_cls_dsc"; DEPARTMENT_Test = "&dpt_test_dsc";
			data outfit_data2; 	
				length DEPARTMENT_Class $30 DEPARTMENT_Test $30 model $30 Criterion $9 InterceptOnly 8 InterceptAndCovariates 8 ;
				set outfit_data2; model = 'Affinity'; DEPARTMENT_Class = "&dpt_cls_dsc"; DEPARTMENT_Test = "&dpt_test_dsc";
			data outres_data2;	
				length DEPARTMENT_Class $30 DEPARTMENT_Test $30 model $30 OrderedValue 8 Outcome $8 Count 8; 
				set outres_data2; model = 'Affinity'; DEPARTMENT_Class = "&dpt_cls_dsc"; DEPARTMENT_Test = "&dpt_test_dsc";
			run;
		
			data logit_out;
				set logit_out out_data1 out_data2; run;
			data logit_outfit; 
				set logit_outfit outfit_data1 outfit_data2; 
			data logit_res;
				set logit_res outres_data1 outres_data2; 
			run;
	
			proc datasets noprint; delete out_data1 out_data2 outfit_data1 outfit_data2 outres_data1 outres_data2; run;

			* ROC curve comparison;
			%let basetitl = %trim(&dpt_cls_dsc) %trim(&dpt_test_dsc) Base;
			%let modtitl = %trim(&dpt_cls_dsc) %trim(&dpt_test_dsc) Separate;		
			ods graphics on;
			proc logistic data=&outname1 plots(only) = roc(id=prob);
				model success(event='1') = grp0 - grp&num_group total_sale /nofit;
				roc "&basetitl" total_sale;
				roc "&modtitl" grp0 - grp&num_group;
				roccontrast reference("&basetitl") / estimate e;
			run;
			ods graphics off;	
		%end;
	%end;
%mend DptmtLoop;

****************************;
* Run cross-category model *;
****************************;
* Department data ;
data dpt_data;
	set ORIG.tripcode;
	where TRIP_CODE = 'DEPARTMENT';
	drop TRIP_CODE;
	rename CODE = DEPARTMENT TRIP_DSC = DEPARTMENT_DSC;
	DEPARTMENT_GROUP = CODE;
	DEPARTMENT_GROUP_DSC = TRIP_DSC;
	If CODE <= 3 then do;
		DEPARTMENT_GROUP = 1;
		DEPARTMENT_GROUP_DSC = 'BDD';
	end;
	else do;
		DEPARTMENT_GROUP = DEPARTMENT_GROUP - 2;
	end; 
run;

%DptmtClssTest(dptmt_data=dpt_data,outname=prod_test,HH_outname=HH_new_dpt,trip_data=TRIP.trip_det,
		prod_data=PROD.new_full,test_prod_data=PROD.new_prim,class_window=52,test_window=52,num_group=4,repeat=);
%DptmtLoop(dptmt_data=dpt_data,outname=prod_test,HH_outname=HH_new_dpt,trip_data=TRIP.trip_det,
		prod_data=PROD.new_full,test_prod_data=PROD.new_prim,class_window=52,test_window=52,num_group=4,repeat=);

/*data HH.result_3_cross_grp4; set logit_out;
data HH.result_3_corssfit_grp4; set logt_res;
run;*/

proc datasets noprint; delete trip_prod1-trip_prod8 test_prod test_prod_data1-test_prod_data8; run;

***************************************************;
* Correlation of affinity index cross departments *;
***************************************************;
data tmp;
	set	hh_new_dpt1(keep = DEPARTMENT PANID affinity)
		hh_new_dpt2(keep = DEPARTMENT PANID affinity)
		hh_new_dpt3(keep = DEPARTMENT PANID affinity)
		hh_new_dpt4(keep = DEPARTMENT PANID affinity)
		hh_new_dpt5(keep = DEPARTMENT PANID affinity)
		hh_new_dpt6(keep = DEPARTMENT PANID affinity)
		hh_new_dpt7(keep = DEPARTMENT PANID affinity)
		hh_new_dpt8(keep = DEPARTMENT PANID affinity);
run;
		
proc sort data=tmp; by PANID DEPARTMENT;run;
proc transpose data=tmp out=hh_affinity;
	by PANID;
	id DEPARTMENT;
	var affinity;
run;

ods graphics on;
proc corr data=hh_affinity plots(MAXPOINTS= NONE)=matrix(histogram NVAR=ALL);
	var Dairy--Bakery;
run;
ods graphics off;

ods graphics on;
proc corr data=hh_affinity nomiss plots(MAXPOINTS= NONE)=matrix(histogram NVAR=ALL);
	var Dairy--Bakery;
run;
ods graphics off;


*********************;
* Export the output *;
*********************;
PROC EXPORT DATA= logit_out
            OUTFILE= "&reg_outfile" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;


