%let dirname= E:\Users\ccv103\Documents\Research\IRI_academic;

*************;
* Read data *; 
*************;
*----------------------------------------;
*** Macro functions of importing data ***; 
%macro read_sales(cat_data,outname);
	proc sql noprint;
		select count(n) into :num_cat from &cat_data;
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
		            DBMS=xls REPLACE;
		     *SHEET="Sheet1$"; 
			 GUESSINGROWS=5000;
		RUN;
		
		data tmp_attr&i;
			set tmp_attr&i;
			Category = L1;
			Vendor = put(L4,30.);
			Brand = put(L5,30.);
			keep UPC SY GE VEND ITEM Category Vendor Brand VOL_EQ PRODUCT_TYPE;
		run;
		
		/*data tmp&i(drop=year fext);
			set tmp_name;
			filepath = cats("&dirname.","\Year",year,"\External\","&cat.","\","&cat.",fext);
			infile dummy filevar = filepath length=reclen end = done;
		    do while(not done) ;
		       input IRI_KEY  WEEK SY GE VEND ITEM UNITS DOLLARS F $47-51 D PR ;
			   output ; 
			end ;
		run;*/
	%end;
	
	* Merge all the data;
	%let tmpend=%sysfunc(catx(%str(),tmp,&num_cat));
	%let tmpend1=%sysfunc(catx(%str(),tmp_attr,&num_cat));
	%put &tmpend;
	/*data &outname;
		set tmp1-&tmpend;
	run;*/
	data attr;
		set tmp_attr1-&tmpend1;
	run;
	
	/*proc datasets noprint;delete tmp1-&tmpend tmp_attr1-&tmpend1;run;*/
%mend read_sales;


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
	set cat_data(obs=5);
run;

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

* Define unique product;
proc sort data=attr; by CATEGORY PRODUCT_TYPE BRAND;run;
data attr;	
	set attr;
	by CATEGORY PRODUCT_TYPE BRAND;
	if first.BRAND then
		PROD_ID + 1;
run;

/*proc datasets noprint; delete tmp_name; run;*/

*----------------------;
*** Read chain data ***;
* Read IRI week;
PROC IMPORT OUT= WORK.IRI_week 
            DATAFILE= "E:\Users\ccv103\Documents\Research\IRI_academic\demos trips external 1_11 may13\IRI week translation.xls" 
            DBMS=EXCELCS REPLACE;
     SHEET="Sheet1$"; 
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

data iri_week;
	set iri_week(drop=Year Calendar_date IRI_week1);
	year = year(Calendar_week_ending_on);
	rename iri_week = WEEK;
proc sort; by year week;run;
data iri_week;
	set iri_week;
	retain first_week;
	by year;
	if first.year then first_week=WEEK; output;
run;

* Unify chain names over year;
PROC IMPORT OUT = WORK.store_xref
			DATAFILE = "E:\Users\ccv103\Documents\Research\IRI_academic\demos trips external 1_11 may13\masked_chain_xref1_11.csv"
			DBMS = CSV REPLACE;
			GUESSINGROWS=5000;
			GETNAMES = Yes;
RUN;
	
data store_xref;
	set store_xref;
	* Note: chain reference error;
	if my_chain=73 then Year7=.;
	if my_chain>73 then Year7=Year7-1;
	nopres = cmiss(of Year1-Year11);
	my_chain = _n_;
run;

proc freq data=store_xref; tables nopres;run;
* Focus on the chains that participate the full years;
data store_xref(drop = nopres);
	set store_xref;
	if nopres=0;
run;

proc transpose data=store_xref out=xref_long(rename=(_NAME_=year));
	by my_chain;
run;
data xref_long;
	set xref_long;
	if COL1 ^=.;
	MskdName = cats("Chain",COL1);
run;

* Read dilvery store data;
data tmp;
	set xref_long(keep=year);	
proc sort nodup;by year; run;

data store2mkt;
	set tmp;
	filepath = cats("&dirname.","\",year,"\External\beer\Delivery_Stores");
	infile dummy filevar = filepath length=reclen firstobs=2 end = done;
    do while(not done) ;
       input IRI_KEY OU $9-10 MARKET_NAME $21-45 OPEN Clsd MskdName $;
	   output ; 
	end ;
run;
proc sort data = store2mkt nodup ; 
    by IRI_KEY OPEN ;
run ;

* Match the unified chain name with IRI_KEY;
proc sql noprint;
	create table tmp as
	select A.*,B.my_chain
	from store2mkt as A inner join xref_long as B
	on (A.MskdName=B.MskdName) and (A.year=B.year)
	order by IRI_KEY;
quit;

data store;
	set tmp(rename=(year=year_char));
	if MskdName ^= 'NONE';
	year = 2000+input(substr(year_char,5),2.);
	drop year_char;
run;
proc sort data=store nodup; by IRI_KEY year open; run;

* Assign match week to each IRI_KEY;
data year_week;
	set iri_week(keep=year first_week);
proc sort nodup; by year first_week;run;

proc sql noprint;
	create table store_week as
	select A.*,B.first_week 
	from store as A left join year_week as B
	on A.year=B.year
	order by IRI_KEY,year,open;
quit;

data store_week;
	set store_week;
	by IRI_KEY year;
	match_week = OPEN;
	if first.year then match_week = first_week;
run;

*--------------------------------------;
*** Match chain names to sales data ***;
* A separate data with only swtiching week;
data tmp_store;
	set store_week;
	if first_week ^= match_week;
run;

* Assign matching weeks to sales data;
proc sql noprint;
	create table tmp as
	select A.*,B.year,B.first_week
	from sales_full as A left join iri_week as B
	on A.WEEK=B.WEEK;
	
	create table tmp1 as
	select A.*,B.match_week as switch_week
	from tmp as A left join tmp_store as B
	on (A.IRI_KEY=B.IRI_KEY) and (A.year=B.year);
quit;

data tmp1;
	set tmp1;
	match_week = first_week;
	if (switch_week^=. & WEEK >= switch_week) then match_week = switch_week;
run;

proc sql noprint;
	create table sales_chain(drop=first_week switch_week match_week) as 
	select A.*,B.my_chain,B.market_name
	from tmp1 as A inner join store_week as B
	on (A.IRI_KEY=B.IRI_KEY) and (A.match_week = B.match_week)
	order by my_chain,WEEK;
quit;

proc datasets noprint; delete tmp tmp1 tmp_store store; run;

****************************;
* First and last scan week *;
****************************;
*----------------------------------------------------*;
*** Identify the product life over all the markets ***;
proc sql noprint;
	create table tmp as
	select A.*,B.PROD_ID
	from sales_full as A inner join attr as B
	on A.UPC=B.UPC
	order by prod_id;
	
	create table tmp1 as
	select prod_id, WEEK,sum(DOLLARS) as DOLLARS, sum(UNITS) as UNITS
	from tmp
	group by prod_id,WEEK;
	
	create table sales_prod as
	select prod_id,WEEK,DOLLARS,UNITS,sum(DOLLARS) as tot_dol
	from tmp1
	group by prod_id
	order by prod_id, WEEK;
quit;

* Compute cumulative dollars each week;
data sales_prod;
	set sales_prod;
	by prod_id;
	if first.prod_id then cum_dol=0;
	cum_dol + DOLLARS;
	ind_95 = . ;
	if cum_dol >= .95*tot_dol then ind_95=1;
run;
	
* Identify the first and last (approximately) scan week;
* Choose new products introducted in 2002-2007;
proc sql noprint;
	create table tmp as 
	select prod_id, min(WEEK) as WFM, min(WEEK*ind_95) as WLM
	from sales_prod
	group by prod_id;
	
	create table prod_all_mkt as
	select prod_id, WFM, WLM, (WLM-WFM) as life
	from tmp where (WFM>=1167) and (WFM<=1478);
quit;

proc univariate data=prod_all_mkt noprint; histogram life; 
proc means data=prod_all_mkt; var life; run;
	
*----------------------------------------------*;
*** Identify the product life in each chain ***;
proc sql noprint;
	create table tmp as 
	select A.*,B.PROD_ID
	from sales_chain as A inner join attr as B
	on A.UPC=B.UPC;
	
	create table tmp1 as 
	select *
	from tmp 
	where PROD_ID in (select PROD_ID from prod_all_mkt);
	
	create table tmp2 as
	select prod_id,my_chain,market_name,WEEK,sum(DOLLARS) as DOLLARS, sum(UNITS) as UNITS
	from tmp1 
	group by PROD_ID,market_name,my_chain,WEEK
	order by PROD_ID,market_name,my_chain,WEEK;
	
	create table sales_newprod_chain as
	select prod_id,my_chain,market_name,WEEK,DOLLARS,UNITS,sum(DOLLARS) as tot_dol
	from tmp2
	group by PROD_ID,market_name,my_chain
	order by PROD_ID,market_name,my_chain,WEEK;
quit;

* Compute cumulative dollars each week in each chain;
data sales_newprod_chain;
	set sales_newprod_chain;
	by prod_id market_name my_chain;
	if first.my_chain then cum_dol=0;
	cum_dol + DOLLARS;
	ind_95 = . ;
	if cum_dol >= .95*tot_dol then ind_95=1;
run;	

* Identify the first and last (approximately) scan week in each chain;
proc sql noprint;
	create table main_newprod_chain as 
	select prod_id,my_chain,market_name,min(WEEK) as WFM, min(WEEK*ind_95) as WLM
	from sales_newprod_chain
	group by prod_id,my_chain,market_name
	order by PROD_ID,market_name,my_chain;
quit;

data main_newprod_chain;
	set main_newprod_chain;
	life = WLM - WFM;
run;

proc univariate data=main_newprod_chain noprint; 
	class prod_id market_name;
	var life;
	output out=tmp N=nobs range=life_range std=life_std;
run;

*********************************************;
* Regressions of life against initial sales *;
*********************************************;
proc sql noprint;
	create table tmp as
	select A.*,B.WFM,B.life
	from sales_newprod_chain as A inner join main_newprod_chain as B
	on (A.prod_id=B.prod_id) and (A.my_chain=B.my_chain) and (A.market_name=B.market_name)
	order by prod_id,my_chain,market_name,week;
quit;

data tmp;
	set tmp;
	period = .;
	if WEEK<=WFM + 8 then period= 1;
	if ((WEEK>WFM+8) & (WEEK<=WFM+16) ) then period = 2;
	if ((WEEK>WFM+16) & (WEEK<=WFM+24) )then period = 3;
run;

proc sql noprint;
	create table tmp1 as
	select prod_id,my_chain,market_name,life,period,sum(DOLLARS) as DOLLARS,sum(UNITS) as UNITS
	from tmp
	group by prod_id,my_chain,market_name,life,period;
quit;
	
proc transpose data=tmp1 out=reg_data prefix=dol;
	by prod_id my_chain market_name life;
	id period;
	var UNITS;
run;

data reg_data;
	set reg_data(drop=_NAME_);
	log_dol1=log(dol1);
	log_dol2=log(dol2);
	prod_mkt = cats(prod_id,"-",market_name);
run;

proc glm data=reg_data;
	class prod_id my_chain;
	model life = log_dol1 log_dol2 prod_id my_chain / solution;
run;

proc glm data=reg_data;
	absorb prod_mkt;
	class my_chain;
	model life = log_dol1 log_dol2 my_chain/ solution;
run;	
