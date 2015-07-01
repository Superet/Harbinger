%let dirname= E:\Users\ccv103\Documents\Research\IRI_academic;

*************;
* Read data *; 
*************;
*----------------------------------------;
*** Macro functions of importing data ***; 
%macro read_sales(cat_data,outname);
	proc sql noprint;
		select count(n) into :num_cat from cat_data;
	quit;
	
	* Read sales by category including all years;
	%do i=1 %to &num_cat;
		proc sql noprint; 
			select category into :cat from &cat_data where n=&i;
		quit;
		
		%let attrpath=%sysfunc(catx(%str(),E:\Users\ccv103\Documents\Research\IRI_academic\parsed stub files\prod_,&cat,.xls));
		%put &attrpath;
		PROC IMPORT OUT= tmp_attr&i 
		            DATAFILE= "&attrpath"
		            DBMS=EXCELCS REPLACE;
		     *SHEET="Sheet1$"; 
			 *GUESSINGROWS=32767;
		RUN;
		
		data tmp_attr&i;
			set tmp_attr&i;
			Category = L1;
			Vendor = L4;
			Brand = L5;
			keep UPC SY GE VEND ITEM Category Vendor Brand VOL_EQ;
		run;
		
		data tmp&i(drop=year fext);
			set tmp_name;
			filepath = cats("&dirname.","\Year",year,"\External\","&cat.","\","&cat.",fext);
			infile dummy filevar = filepath length=reclen end = done;
		    do while(not done) ;
		       input IRI_KEY  WEEK SY GE VEND ITEM UNITS DOLLARS F $47-51 D PR ;
			   output ; 
			end ;
		run;
	%end;
	
	* Merge all the data;
	%let tmpend=%sysfunc(catx(%str(),tmp,&num_cat));
	%let tmpend1=%sysfunc(catx(%str(),tmp_attr,&num_cat));
	%put &tmpend;
	data &outname;
		set tmp1-&tmpend;
	run;
	data attr;
		set tmp_attr1-&tmpend1;
	run;
	
	proc datasets noprint;delete tmp1-&tmpend tmp_attr1-&tmpend1;run;
%mend read_sales;

*--------------------------------------------;
*** Read product first and last scan date ***; 
proc import out=upc
	datafile="E:\Users\Projects\Project_Marketing_IRI\kelloggirir\cleanattr.txt"
	dbms=dlm replace;
	delimiter='|';
	getnames=no;
	guessingrows=5000;
	datarow=2;
run;

data sub_upc;
	set upc(keep= var1-var7 var233 var58 var270 var316);
	rename var233 = PROD_TYPE
		   var58  = CALORIE
		   var270 = SERV_SIZE
		   var316 = STORE_LOC;
	SYS = input(var1,2.);
	GEN = input(var2,2.);
	VEN = input(var5,5.);   
	ITE = input(var6,5.);
	WFM = input(var3,6.);
	WLM = input(var4,6.);
	KEYCAT = input(var7,6.);
	format SYS z2. 
		   GEN z2. 
		   VEN z5.
		   ITE z5.;
	UPC = input(cats(put(sys,z2.),put(gen,z2.),put(ven,z5.),put(ite,z5.)),14.);
	drop var1-var7;
	DFM = (WFM - 400)*7 + 31900 - 21916;
	DLM = (WLM - 400)*7 + 31900 - 21916;
	format DFM YYMMDD10. DLM YYMMDD10.;
	LIFE = WLM - WFM;
	Year = year(DFM);
/*	UPC_num = input(UPC,14.);*/
proc sort; by upc; 
run;
proc contents data=sub_upc; run;

*--------------------------;
*** Read UPC dictionary ***;
PROC IMPORT OUT= WORK.dict 
            DATAFILE= "E:\Users\Projects\Project_Marketing_IRI\kelloggirir\DICT.txt" 
            DBMS=dlm REPLACE;
	 delimiter='|';
     GETNAMES=NO;
     DATAROW=1; 
RUN;

data dict;
	set dict(drop=VAR10);
	rename VAR1=UPC VAR2=UPCDSC VAR3=CATEGORY VAR4=TYPE VAR5=VENDOR VAR6=BRAND VAR7=DEPARTMENT VAR8=VOL_EQ VAR9=UNIT_MEASURE;
proc sort; by UPC;
proc contents;
run;

*----------------------------------;
*** Read store level sales data ***;
* Read in categories;
filename dirlist pipe "dir /B &dirname\Year1\External";
data cat_data;
	length category $30;
	infile dirlist length=reclen;
	input category $varying30. reclen;
	n=_n_;
	*call symput ('num_files',_n_);
run;
proc print data=cat_data;run;

data tmp_name;
	input year fext $30.;
	cards;
	1	_groc_1114_1165
	2	_groc_1166_1217
	3	_groc_1218_1269
	4	_groc_1270_1321
	5	_groc_1322_1373
	6	_groc_1374_1426
	7	_groc_1427_1478
	8	_groc_1479_1530
	9	_groc_1531_1582
	10	_groc_1583_1634
	11	_groc_1635_1686
	;
run;

* For testing;
data cat_data;
	set cat_data(obs=6);
run;

/*data tmp_name;
	set tmp_name(obs=1);
run;*/

%read_sales(cat_data,sales_full);
proc contents data=sales_full;
proc contents data=attr;
run;

* Create UPC in sales data and correct UNITS;
data sales_full;
	set sales_full;
	where ITEM^=.;
	format SY z2. 
	   GE z2. 
	   VEND z5.
	   ITEM z5.;
	UPC = input(cats(put(sy,z2.),put(ge,z2.),put(vend,z5.),put(item,z5.)),14.);
	if UNITS<1 then UNITS=1;
/*proc sort; by UPC;*/
run;

data attr;
	set attr(rename=(UPC=UPC_char));
	SYS = input(sy,2.);
	GEN = input(GE,2.);
	VEN = input(VEND,5.);
	ITE = input(ITEM,5.);
	format SYS z2. 
	   GEN z2. 
	   VEN z5.
	   ITE z5.;
	UPC = input(cats(put(sys,z2.),put(gen,z2.),put(ven,z5.),put(ite,z5.)),14.);
run;
proc sort data=attr nodupkey;
	by UPC;
run;

proc datasets noprint; delete tmp_name; run;

******************************;
* Construct new product data *;
******************************;
* Find intersected UPC;
proc sql noprint;
	create table tmp as
	select *
	from sub_upc
	where Year>=2002 & Year<=2010;
	
	create table tmp_sale as 
	select UPC, min(WEEK) as WFM_SALE, max(WEEK) as WLM_SALE
	from sales_full
	where UPC in (select UPC from tmp)
	group by UPC;

	create table new_prod as 
	select A.*,B.WFM_SALE,B.WLM_SALE
	from tmp as A inner join tmp_sale as B
	on A.UPC=B.UPC
	order by UPC;
quit;

* Drop the UPC that differ in first scan weeks more than 3 months;
data new_prod;
	set new_prod;
	WFM_dif = WFM_SALE - WFM;
	WLM_dif = WLM_SALE - WLM;
proc means; var WFM_dif WLM_dif;
proc univariate noprint; histogram WFM_dif WLM_dif;
run;

%let max_dif=12;
data new_prod;
	set new_prod;
	where WFM_dif <= &max_dif;
run;

* Construct primary UPC for each category, type, vendor, and brand;
proc sql noprint;
	create table tmp as
	select A.*,B.UPCDSC,B.CATEGORY,B.TYPE,B.VENDOR,B.BRAND,B.DEPARTMENT,B.VOL_EQ
	from new_prod as A inner join dict as B
	on A.UPC=B.UPC
	order by CATEGORY,PROD_TYPE,VEN,BRAND,WFM;
quit;

data new_prod;
	set tmp;
	id = cats(CATEGORY,'*',PROD_TYPE,'*',VEN,'*',BRAND);
	by CATEGORY PROD_TYPE VEN BRAND;
	ranking=0;
	if first.BRAND then prim=1;	
run;

* Check product life and then define product failure;
proc univariate data=new_prod noprint; histogram LIFE;
proc means data=new_prod N MEAN STD MEDIAN MIN P10 P25 P75 P90 MAX; var LIFE;
run;

data prim_new;
	retain UPC UPCDSC WFM WLM DFM DLM LIFE KEYCAT CATEGORY TYPE VENDOR BRAND DEPARTMENT PROD_TYPE VOL_EQ;
	set new_prod;
	where prim=1;
	fail	= 1*(LIFE<208);
	success = 1 - fail;
	PRVT_LAB = 1*(BRAND='PRIVATE LABEL');
proc freq; tables fail;
run;

* Merge to have all the variant products;
proc sql noprint;
	create table full_new(drop=id) as
	select A.*, B.UPC as PRIM_UPC,B.fail,B.success,B.WFM as PRIM_WFM,B.DFM as PRIM_DFM,B.PRVT_LAB,
		   B.WFM_SALE as PRIM_WFM_SALE,B.WLM_SALE as PRIM_WLM_SALE
	from new_prod as A inner join prim_new as B
	on A.id=B.id
	order by PRIM_UPC,UPC;
quit;

data full_new;
	retain PRIM_UPC UPC UPCDSC WFM WLM DFM DLM Year LIFE fail success PRIM_WFM PRIM_DFM PRIM_WFM_SALE PRIM_WLM_SALE KEYCAT CATEGORY TYPE 
			VENDOR BRAND DEPARTMENT PROD_TYPE VOL_EQ UNIT_MEASURE;
	set full_new;
proc contents;
run;

***************************************;
* Compute covariates for new products *;
***************************************;
proc sql noprint;
	create table sales_full1 as
	select A.*,B.CATEGORY,B.VOL_EQ
	from sales_full as A left join dict as B
	on A.UPC = B.UPC;
quit;
proc sql noprint;
	* Aggregate sales data across stores;
	create table sales_agg as
	select UPC,WEEK,sum(UNITS) as UNITS, sum(DOLLARS) as DOLLARS, mean(PR) as PR, mean(PR*DOLLARS)/sum(DOLLARS) as PR_WT
	from sales_full
	group by UPC,WEEK
	order by UPC, WEEK;
	
	* Merge sales with new product data;
	create table tmp as
	select A.*, B.WFM, B.WLM,B.LIFE,B.FAIL,B.CATEGORY, B.TYPE,B.VENDOR,B.BRAND,B.VOL_EQ
	from sales_agg as A inner join prim_new as B
	on A.UPC=B.UPC
	order by UPC,WEEK;
proc sql noprint;	
	* Category sales;
	create table sales_aggcat as 
	select CATEGORY,WEEK, sum(UNITS) as UNITS_CAT, sum(DOLLARS) as DOLLARS_CAT 
	from sales_full1 
	group by CATEGORY, WEEK
	order by CATEGORY, WEEK;
	
	* Merge with new product sales data;
	create table sales_new as
	select A.*,B.UNITS_CAT,B.DOLLARS_CAT
	from tmp as A full join sales_aggcat as B
	on (A.CATEGORY = B.CATEGORY) & (A.WEEK = B.WEEK)
	order by UPC,WEEK;
quit;

data sales_new;
	set sales_new;
	PRICE = DOLLARS/UNITS;
	FM = 1*(WEEK <= WFM + 4);
	FQ = 1*(WEEK <= WFM + 12);
run;

* Compute regular price and initial sales, initial promotions;
proc univariate data=sales_new noprint;
	class UPC;
	var PRICE;
	output out=tmp MODE=PRICE_mode MEAN=PRICE_mean ;
run;
data tmp;
	set tmp;
	PRICE = PRICE_mode;
	if PRICE_mode=. then PRICE = PRICE_mean;
	drop PRICE_mode PRICE_mean;
run;

proc sql noprint;
	create table tmp1 as
	select 	CATEGORY, UPC,LIFE,FAIL,
			sum(DOLLARS*FM) as DOL_FM, sum(DOLLARS*FQ) as DOL_FQ, sum(UNITS*FM) as UNITS_FM, sum(UNITS*FQ) as UNITS_FQ,
		   	sum(PR*FM)/N(FM=1) as PR_FM, sum(PR_WT*FM)/N(FM=1) as PR_WT_FM,
		   	sum(PR*FQ)/N(FQ=1) as PR_FQ, sum(PR_WT*FQ)/N(FQ=1) as PR_WT_FQ,
			sum(DOLLARS*FM)/sum(DOLLARS_CAT*FM) as MKTSR_FM, sum(DOLLARS*FQ)/sum(DOLLARS_CAT*FQ) as MKTSR_FQ
	from sales_new
	group by CATEGORY,UPC,LIFE,FAIL
	order by UPC;

	create table new_test as
	select A.*,B.*
	from tmp as A inner join tmp1 as B
	on A.UPC = B.UPC;
quit;

data new_test;
	set new_test;
	logLife		= log(LIFE);
	logP 		= log(PRICE);
	logUnits_FM	= log(UNITS_FM);
	logUnits_FQ	= log(UNITS_FQ);
	logMKTSR_FM	= log(MKTSR_FM);
	logMKTSR_FQ	= log(MKTSR_FQ);
run;

******************;
* Model analysis *;
******************;
proc contents data=new_test; run;
proc means data=new_test; var fail;run;

proc reg data=new_test;
	model LIFE = MKTSR_FM logP PR_WT_FM;
run;

proc reg data=new_test;
	model LIFE = MKTSR_FQ logP PR_WT_FQ;
run;

* Restrict to failed products;
data new_test_fail;
	set new_test;
	if fail=1;
run;

proc reg data=new_test_fail;
	model logLIFE = LogMKTSR_FQ logP PR_WT_FQ;
run;













