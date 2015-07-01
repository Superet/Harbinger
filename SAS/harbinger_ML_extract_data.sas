libname orig 	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Orig";
libname prod 	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Prod";
libname hh 		"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\HH";
libname trip	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Trip";
%let dirname= E:\Users\Projects\Project_Marketing_IRI\kelloggirir;

options compress=yes reuse=yes;

* Set parameters;
%let selyear = 2006; 
%let class_window = 52; 
*%let nsample = 10000; 

* Select hosueholds ;
*** Compute the staying length of each year;
proc sql noprint; 
	create table tmp as 
	select PANID,Year, max(DATE)-min(DATE) as stay
	from ORIG.trip_sum
	group by PANID,Year;
quit;

proc transpose data=tmp out=hh prefix=stay;
	by PANID;
	id YEAR;
run;

* Selection criterion: if a householse showed up in 2006(2008), then we need him to stay in 2007(2009) as well;
data PAN;
	merge hh ORIG.demo;
	by PANID;
	drop=0;
/*	classification = stay2006;
	test = stay2007;*/
	classification = max(stay2006, stay2008); 
	test = max(stay2007, stay2009); 
	if (classification<10 | test<10) then drop=1;
	array demovar{14} INCOME FAMSIZE RACE CHILDREN FMLE_AGE FMLE_EDU FMLE_HRS FMLE_OCC 
					MALE_AGE MALE_EDU MALE_HRS MALE_OCC M_STATUS RENTOWN;
	do i = 1 to 14;
		if demovar[i] = 99 then drop = 1;
	end;
proc freq; tables drop;
run;

data PAN;
	set PAN;
	where drop=0;
	keep PANID;
run;

/*proc surveyselect data = PAN sampsize = &nsample out= pan1;
	id PANID; 
run;*/

* Extract demographic data for select panelists; 
proc sql noprint;
	create table mydemo as 
	select PANID, FAMSIZE, INCOME, RACE, CHILDREN, M_STATUS, REGION, RENTOWN, FMLE_AGE, FMLE_EDU, FMLE_HRS, FMLE_OCC, 
			MALE_AGE, MALE_EDU, MALE_HRS, MALE_OCC
	from ORIG.demo
	where PANID in (select PANID from PAN)
	order by PANID;
quit;

proc freq data=mydemo;
	table FAMSIZE INCOME RACE CHILDREN M_STATUS REGION RENTOWN;
run;

* Collapse some demographic varaibles;
data mydemo; 
	set mydemo; 
	if FAMSIZE >= 4 then FAMSIZE = 4; 
	CHILD = 0; 
	if CHILDREN ^= 8 then CHILD=1; 
run;

*** Format demographics ***;
data democode; 
	set ORIG.democode end = eof; 
	if eof then do; 
	output; 
	variable = 'CHILD'; start = 0; label = "NO CHILD"; output;
	variable = 'CHILD'; start = 1; label = "HAVE CHILDREN";
	end; 
	output; 
run;
data democode; 
	set democode; 
	if variable = "FAMSIZE" and start = 4 then label = "THREE+";
	if variable = "FAMSIZE" and start > 4 then delete;
run;

data demo_fmt(keep = fmtname type start label);
	retain type 'N';
	set democode end=lastrec;
	fmtname = variable;
	output;
	/*if lastrec then do;
		hlo ='O'; 
		label = 'Missing';
		output;
	end;*/
run;

proc format library=WORK cntlin=demo_fmt; run;

options  fmtsearch=(WORK);
data mydemo;
	set mydemo;
	format FAMSIZE FAMSIZE.;
	format INCOME INCOME.;
	format RACE RACE.;
	format CHILDREN CHILDREN.;
	format FMLE_AGE FMLE_AGE.;
	format FMLE_EDU FMLE_EDU.;
	format FMLE_HRS FMLE_HRS.;
	format FMLE_OCC FMLE_OCC.;
	format MALE_AGE MALE_AGE.;
	format MALE_EDU MALE_EDU.;
	format MALE_HRS MALE_HRS.;
	format MALE_OCC MALE_OCC.;
	format M_STATUS M_STATUS.;
	format RENTOWN RENTOWN.;
	format REGION REGION.;
	format CHILD CHILD.; 
run;

*-----------------------------------------------------*;
* Construct the data of households purchasing products;
proc sql noprint; 
	create table tmp as 
	select *
	from TRIP.trip_det 
	where PANID in (select PANID from PAN);
	
	create table trip_prod as
	select A.*,B.PRIM_UPC,B.fail,B.UPC_fail,B.WFM,B.class,B.test, B.PRIM_LIFE, B.PRIM_WFM, B.DEPARTMENT, B.CATEGORY
	from tmp as A inner join PROD.new_full as B
	on A.UPC = B.UPC
	order by PANID,PRIM_UPC;
	
	create table prod_hh as 
	select PANID, PRIM_UPC, DEPARTMENT, CATEGORY, fail, mean(PRIM_LIFE) as LIFE, count(distinct DATE) as num_purchase, 
		ifn(count(distinct DATE)>0, 1, 0) as purchase_ind
	from (select * from trip_prod where WEEK <= PRIM_WFM + &class_window)
	group by PANID, DEPARTMENT, CATEGORY, PRIM_UPC, fail
	order by PRIM_UPC, PANID;
	
	* Subset the data; 
	create table prod_hh_sub as 
	select *
	from prod_hh;
/*	where PRIM_UPC in (select PRIM_UPC from PROD.new_full where year in &selyear);*/
quit;

* Transpose the data from long to wide: purchase incidence; 
proc transpose data=prod_hh_sub out = prod_hh_wide prefix= HH; 
	by PRIM_UPC DEPARTMENT CATEGORY fail LIFE; 
	id PANID;
	var purchase_ind;
run;
/*
* Transpose the data from long to wide: purchase quantity; 
proc transpose data=prod_hh_sub out = prod_hh_wide prefix= HH; 
	by PRIM_UPC DEPARTMENT CATEGORY fail LIFE; 
	id PANID;
	var num_purchase;
run;
*/
data prod_hh_wide; 
	set prod_hh_wide(drop=_NAME_); 
	array pur{*} HH:;
	do i=1 to dim(pur);
		if pur[i] = . then pur[i]=0; 
	end; 
	drop i;
run;

*-----------------------------------------------------*;
* Compute household behavior; 
proc sql noprint;
	* Compute monthly expenditure and shopping frequency; 
	create table tmp as 
	select PANID, year, month, sum(BSKTDOL) as DOL, count(DATE) as num_trip, count(distinct DATE) as num_day
	from (select *, month(DATE) as month from ORIG.trip_sum where PANID in (select PANID from PAN) ) 
	group by PANID, year, month; 
	
	* Compute average monthly expenditure; 
	create table tmp1 as
	select PANID, mean(DOL) as month_exp, mean(num_trip) as num_trip, mean(num_day) as num_day
	from tmp 
	group by PANID; 
	
	* Compute the overall number of UPCs and number of new products; 
	create table tmp as 
	select *, year(DATE) as year
	from TRIP.trip_det 
	where PANID in (select PANID from PAN);
	
	create table trip_prod_full as
	select A.*,B.PRIM_UPC,B.fail,B.UPC_fail,B.WFM,B.class,B.test, B.PRIM_LIFE, B.PRIM_WFM, B.CATEGORY
	from tmp as A left join PROD.new_full as B
	on A.UPC = B.UPC; 
	
	create table tmp as 
	select PANID, year, count(distinct UPC) as num_all_upc, count(distinct PRIM_UPC) as num_new_upc,
						sum(DOL) as exp_all_upc, sum(DOL*PRIM_UPC) as exp_new_upc, 
						max(DATE) - min(DATE) as stay
	from trip_prod_full 
	group by PANID, year; 
	
	create table tmp2 as 
	select PANID, sum(num_all_upc*stay)/sum(stay) as num_all_upc, sum(num_new_upc*stay)/sum(stay) as num_new_upc, 
				sum(exp_all_upc*stay)/sum(stay) as exp_all_upc, sum(exp_new_upc*stay)/sum(stay) as exp_new_upc
	from tmp 
	group by PANID;
	
	* Merge the two sets together; 
	create table my_hh_bhv(drop=PANID_old) as 
	select *
	from tmp1 as A full join tmp2(rename=(PANID = PANID_old)) as B
	on A.PANID = B.PANID_old; 
	
	* Merge the behavioral metrics and demographics; 
	create table tmp(drop=PANID_old) as 
	select *
	from mydemo as A full join my_hh_bhv(rename=(PANID = PANID_old)) as B
	on A.PANID = B.PANID_old
	order by PANID; 
quit; 
data mydemo; set tmp; run;

*-----------------------------------------------------*;
* Link the column name with PANID; 
%let coln1 = 5; 
data tmp; 
	set prod_hh_wide; 
	length var $20.; 
	array def(*) _numeric_; 
	do i = 1 to dim(def);
		var = vname(def[i]);
		output;
	end;
	stop;
	keep var; 
run;

data tmp;
	set tmp(firstobs = &coln1);
	PANID_col = input(substr(var, 3), 12.);
	col = &coln1 + _N_; 
proc contents; 
run;

proc sql noprint; 
	create table tmp1 as 
	select *
	from tmp as A left join mydemo as B
	on A.PANID_col = B.PANID
	order by col;
run;
data mydemo; 
	set tmp1(drop=PANID_col);
run;
	
******************;
* Export the data*;
******************;
PROC EXPORT DATA= mydemo
            OUTFILE= "E:\Users\ccv103\Desktop\hh_demo.csv"
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

DATA _NULL_;
   SET prod_hh_wide;
   FILE "E:\Users\ccv103\Desktop\prod_hh_wide.csv" DSD DLM=',' LRECL=1000000 ;
   PUT (_ALL_) (:) ;
RUN;


