libname orig 	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Orig";
libname prod 	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Prod";
libname hh 		"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\HH";
libname trip	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Trip";
%let dirname= E:\Users\Projects\Project_Marketing_IRI\kelloggirir;
options compress=yes reuse=yes;

* Set parameters;
%let fail_threshold = 208;
%let popthresh = .0002;

*************;
* Read data *; 
*************;
* A function of reading data;
%macro readfile(dirlist,outname);
	proc sql noprint;
		select count(fname) into :num_files
		from &dirlist;
	quit;

	%do j=1 %to &num_files;
		proc sql noprint;
		select fname into :fname
		from &dirlist
		where n=&j;
		quit;
	
		%let fpath=&dirname\&fname;
		%put &fpath;
		proc import out=tmp&j
			datafile="&fpath"
			dbms=dlm replace;
			delimiter='|';
			getnames=no;
			datarow=1;
		run;
	%end;

	%let tmpend=%sysfunc(catx(%str(),tmp,&num_files));
	%put &tmpend;
	data &outname;
		set tmp1-&tmpend;
	run;

	* Delete separate files;
	proc datasets noprint; delete tmp1-&tmpend;run;	
%mend readfile;		

*** Read demographics ***;
PROC IMPORT OUT= ORIG.demo 
            DATAFILE= "E:\Users\Projects\Project_Marketing_IRI\kelloggirir\DEMO.txt" 
            DBMS=dlm REPLACE;
	 delimiter='|';
     GETNAMES=NO;
     DATAROW=1; 
RUN;

data ORIG.demo;
	set ORIG.demo(drop=VAR25);
	rename VAR1=PANID VAR2=FAMSIZE VAR3=INCOME VAR4=RACE VAR5=CHILDREN
	 	VAR6=FMLE_AGE VAR7=FMLE_EDU VAR8=FMLE_HRS VAR9=FMLE_OCC	VAR10=MALE_AGE 
		VAR11=MALE_EDU VAR12=MALE_HRS VAR13=MALE_OCC VAR14=M_STATUS VAR15=RENTOWN 
		VAR16=NUM_CATS VAR17=NUM_DOGS VAR18=REGION VAR19=MKT VAR20=PROJ09 VAR21=PROJ08 
		VAR22=PROJ07 VAR23=PROJ06 VAR24=ZIPCODE;
run;
	
*** Read demo code ***; 
PROC IMPORT OUT=ORIG.democode
            DATAFILE="E:\Users\Projects\Project_Marketing_IRI\kelloggirir\Academic Household File.xls"
            DBMS=EXCELCS REPLACE;
   SHEET='Demo Code';
RUN;

data ORIG.democode;
	set ORIG.democode (firstobs=3 rename=(DEMO_CODE=VARIABLE F2=START F3=LABEL));
	if START=. then delete;
run;

*** Read trip code;
PROC IMPORT OUT=ORIG.tripcode
            DATAFILE="E:\Users\Projects\Project_Marketing_IRI\kelloggirir\Academic Household File.xls"
            DBMS=EXCELCS REPLACE;
   SHEET='Trip Code';
RUN;

data ORIG.tripcode;
	set ORIG.tripcode(firstobs=3 drop=F4-F7);
	if F2=. then delete;
	rename F2=CODE F3=TRIP_DSC;
run;

*** Read UPC ***;
PROC IMPORT OUT= ORIG.dict 
            DATAFILE= "E:\Users\Projects\Project_Marketing_IRI\kelloggirir\DICT.txt" 
            DBMS=dlm REPLACE;
	 delimiter='|';
     GETNAMES=NO;
     DATAROW=1; 
RUN;

data ORIG.dict;
	set ORIG.dict(drop=VAR10);
	rename VAR1=UPC VAR2=UPCDSC VAR3=CATEGORY VAR4=TYPE VAR5=VENDOR VAR6=BRAND VAR7=DEPARTMENT VAR8=VOL_EQ VAR9=UNIT_MEASURE;
proc sort; by UPC;
run;
proc contents data=ORIG.dict;run;

*** Read trip summary data ***;
filename dirlist pipe "dir /B &dirname\Trip_summary*.txt";
data dirlist;
	length fname $30;
	infile dirlist length=reclen;
	input fname $varying30. reclen;
	n=_n_;
	*call symput ('num_files',_n_);
run;
proc print data=dirlist;run;

%readfile(dirlist,trip_sum);

data tmp;
	set trip_sum(drop=VAR9 rename=(VAR1=PANID VAR2=CHAIN VAR3=DATE VAR4=TRIP_ID VAR5=WEEK VAR6=BSKTDOL VAR7=TRIP_MISSION VAR8=TRIP_TYPE));
	DATE = INPUT(PUT(DATE,8.),YYMMDD8.);
	FORMAT DATE YYMMDD10.;
run;

* Put together the multiple trips in the same store on the same day;
proc sql noprint;
	create table trip_sum as
	select PANID, CHAIN, DATE, sum(BSKTDOL) as BSKTDOL, year(DATE) as YEAR
	from tmp
	group by PANID,CHAIN,DATE
	order by PANID,YEAR,DATE,CHAIN;
quit;

* Check any systematic errors of basket dollars of trips;
proc freq data=trip_sum;
	where BSKTDOL<0.05;
	tables bsktdol;
run;

* Drop trips with basket size equal to .01; 
data ORIG.trip_sum; set trip_sum; if BSKTDOL>.01; run;
proc datasets library=work noprint; delete trip_sum tmp;run;

*--------------------------------------------------------------------------------------------------------*;
* Household selection *;
*** Compute the staying length of each year;
/*
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
	set hh;
	drop=0;
	classification = max(stay2006,stay2008);
	test = max(stay2007,stay2009);
	if (classification<10 | test<10) then drop=1;
proc freq; tables drop;
run;

data HH.PAN;
	set PAN;
	where drop=0;
	keep PANID;
run;

proc datasets noprint;delete tmp hh pan;run;
*/

data HH.PAN; 
	set demo(keep=PANID);
run;

*--------------------------------------------------------------------------------------------------------*;
*** Read trip detail data ***;
filename dirlist pipe "dir /B &dirname\Trip_detail*.txt";
data dirlist;
	length fname $30;
	infile dirlist length=reclen;
	input fname $varying30. reclen;
	n=_n_;
	*call symput ('num_files',_n_);
run;
proc print data=dirlist;run;

%readfile(dirlist,trip_det_full);

data trip_det_full;
	set trip_det_full(drop=VAR13 rename=(VAR1=PANID VAR2=CHAIN VAR3=DATE VAR4=TRIP_ID VAR5=WEEK 
				VAR6=UPC VAR7=DOL VAR8=UNITS VAR9=COUPON VAR10=DISPLAY VAR11=FEATUR VAR12=PRICEOFF));
	DATE = INPUT(PUT(DATE,8.),YYMMDD8.);
	FORMAT DATE YYMMDD10.;
run;

* Restrict the trip detail to the selected panelists and collapse the TRIP_ID; 
proc sql noprint;
	create table TRIP.trip_det as
	select PANID,DATE,CHAIN,WEEK,UPC,sum(DOL) as DOL,sum(UNITS) as UNITS,1*(sum(COUPON)>0) as COUPON, 
		   1*(mean(PRICEOFF)>0) as PRICEOFF
	from trip_det_full
	where PANID in (select PANID from HH.PAN)
	group by PANID,DATE,CHAIN,UPC
	order by PANID,DATE,CHAIN;	
quit;

proc datasets noprint; delete dirlist trip_det_full; run;
	
*** Read new product attriubtes ***;
proc import out=upc
	datafile="E:\Users\Projects\Project_Marketing_IRI\kelloggirir\cleanattr.txt"
	dbms=dlm replace;
	delimiter='|';
	getnames=no;
	guessingrows=5000;
	datarow=2;
run;

data upc;
	set upc(keep= var1-var7 var233 var264 var270 var316);
	rename var233 = PROD_TYPE
		   var264 = SEASONAL
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
	UPC = input( cats(put(sys,z2.),put(gen,z2.),put(ven,z5.),put(ite,z5.)),14.);
	drop var1-var7;
	DFM = (WFM - 400)*7 + 31900 - 21916;
	DLM = (WLM - 400)*7 + 31900 - 21916;
	format DFM YYMMDD10. DLM YYMMDD10.;
	LIFE = WLM - WFM;
	Year = year(DFM);
proc sort; by UPC; 
run;
proc contents data=upc; run;

* Create the upc data with unique product id;
proc sql noprint;
	create table PROD.upc as 
	select *,B.UPCDSC,B.CATEGORY,B.TYPE,B.VENDOR,B.BRAND,B.DEPARTMENT,B.VOL_EQ
	from upc as A inner join ORIG.dict as B
	on A.UPC = B.UPC
	order by CATEGORY,PROD_TYPE,VEN,BRAND,WFM;
quit;


proc datasets library=work noprint;delete upc;run;

*********************************;
* Construct new product profiles*;
*********************************;
data PROD.upc;
	set PROD.upc;
	by CATEGORY PROD_TYPE VEN BRAND;
	if first.BRAND then do;
		PROD_ID + 1;
		prim=1;
	end;
run;

* Restrict attention to the non-seasonal new products introduced from 2006 to 2009, define failure products;
proc freq data=PROD.upc noprint; tables SEASONAL/out=tmp; run;
data tmp;
	set tmp;
	where 	SEASONAL ^= '' 				& SEASONAL ^= 'ALL OCCASION' & 
			SEASONAL ^= 'ALL SEASONS' 	& SEASONAL ^= 'ANNIVERSARY' &
			SEASONAL ^= 'ASSORTED' 		& SEASONAL ^= 'BABY SHOWER' &
			SEASONAL ^= 'BIRTHDAY'		& SEASONAL ^= 'CELEBRATION' & 
			SEASONAL ^= 'EVERY DAY'		& SEASONAL ^= 'NON SEASONAL' &
			SEASONAL ^= 'NOT SEASONAL'	& SEASONAL ^= 'PARTY' & 
			SEASONAL ^= 'PICNIC';
run;
	
proc sql noprint;
	create table new_prim as 
	select *
	from PROD.upc 
	where (prim=1) & (Year>=2006) & (Year<=2009) & 
		(SEASONAL not in (select SEASONAL from tmp))
	order by UPC;
quit;
		
proc univariate data=new_prim noprint; histogram LIFE;
proc means N MEAN STD MEDIAN MIN P10 P25 P75 P90 MAX; var LIFE;
run;

data new_prim;
	retain UPC UPCDSC WFM WLM DFM DLM LIFE KEYCAT CATEGORY TYPE VENDOR BRAND DEPARTMENT PROD_TYPE SERV_SIZE STORE_LOC;
	set new_prim;
	fail	= 1*(LIFE < &fail_threshold);
	success = 1 - fail;
	class 	= 1*(year(DFM)=2006 | year(DFM)=2008);
	test	= 1*(year(DFM)=2007 | year(DFM)=2009);
	UPC_fail=fail*input(UPC,14.);
	if fail=0 then UPC_fail=.;
	PRVT_LAB = 1*(BRAND='PRIVATE LABEL');
proc freq; tables fail;
run;

*** Compute the price paid for each product ***; 
proc sql noprint;
	create table tmp_trip as
	select A.*, DOL/UNITS as PRICE, 1*(COUPON + PRICEOFF>0) as promotion, B.prod_id, B.prim
	from TRIP.trip_det as A inner join (select * from PROD.upc where year(DFM)>=2006 and year(DFM)<=2009)  as B
	on A.upc = B.upc
	order by prod_id, prim, UPC;
quit;	

* Use average price paid as price variable for each product;
proc univariate data=tmp_trip noprint;
	by prod_id prim; 
	class UPC; 
	var PRICE promotion;
	output out=tmp1 mean=price_mean prom_mean mode=price_mode prom_mode;
run;

* Compute the average price within a prod_id; 
proc sql noprint;
	create table tmp2 as 
	select *, mean(price_mean/VOL_EQ) as price_avg, mean(prom_mean) as prom_avg
	from (select A.*, B.VOL_EQ from tmp1 as A left join ORIG.dict as B on A.upc=B.upc)
	group by prod_id
	order by prod_id, prim desc, upc; 
quit;

* Price for a given product: (1) median price of prim UPC, (2) average price within the product id; 
data tmp1;
	set tmp2;
	PRICE = price_mode;
	if price_mode=. then PRICE=price_mean;
	promotion = prom_mean; 
	by prod_id;
	if first.prod_id then do;
		if PRICE=. then PRICE = price_avg * VOL_EQ; 
		if promotion =. then promotion = prom_avg; 
	end;
	if (not first.prod_id) & prim ^= 1 then delete; 
	drop price_mode price_mean prom_mean prom_mode;
run;

* Check if the data contains unique prod_id; 
proc sort data = tmp1 nodupkeys out = tmp_check; by prod_id; run;

* Merge in price and promotion to the product data; 
proc sql noprint;
	create table tmp3 as
	select A.*,B.PRICE, B.promotion
	from new_prim as A left join tmp1 as B
	on A.prod_id = B.prod_id;
quit;
proc means data=tmp3;var PRICE promotion;run;

* NOTE: 1844 products are not observed in trip_det, check; 
proc sql; 
	select count(*) from tmp_trip where prod_id in (select prod_id from tmp3 where price = .);
quit; 

data new_prim; 
	set tmp3;
	where price ^= .;  
	fail_cutoff = &fail_threshold/52;
run;
proc datasets noprint; delete tmp tmp1 tmp2 tmp3; run;

*** Compute % AVC ***;
proc sql noprint;
	* Compute AVC of each chain; 
	create table tmp_chain as 
	select CHAIN, AVC, AVC/sum(AVC) as pct_AVC
	from (select CHAIN, sum(BSKTDOL) as AVC from ORIG.trip_sum group by CHAIN); 
	
	* Link product introduction date to the trip data; 
	create table tmp2 as 
	select A.*, B.WFM
	from tmp_trip as A left join PROD.upc as B
	on A.upc = B. upc; 
	
	* Get unique product*chain during the first year after introduction; 
	create table tmp3 as 
	select distinct * 
	from (select prod_id, CHAIN from tmp2 ); 

	* Sum %AVC of all the chains that sell a product; 
	create table tmp1 as 
	select prod_id, sum(pct_AVC) as pct_AVC
	from (select A.*, B.pct_AVC	from tmp3 as A left join tmp_chain as B	on A.CHAIN = B.CHAIN)
	group by prod_id
	order by prod_id; 
quit;

* Merge in new_prim; 
proc sql noprint; 
	create table tmp4 as 
	select A.*, B.pct_AVC
	from new_prim as A left join tmp1 as B
	on A.prod_id = B.prod_id; 
quit; 
proc means data = tmp4; var pct_AVC; run; 

data PROD.new_prim; 
	set tmp4; 
run;
proc datasets noprint; delete tmp tmp1 tmp2 tmp3 tmp4; run;

* Merge to have all the variant products;
proc sql noprint;
	create table new_full as
	select A.*, B.UPC as PRIM_UPC,B.fail,B.success,B.WFM as PRIM_WFM,B.DFM as PRIM_DFM,B.LIFE as PRIM_LIFE,
		   B.class,B.test,B.PRVT_LAB,B.PRICE, B.promotion as prim_promotion, B.pct_AVC
	from PROD.upc as A inner join PROD.new_prim as B
	on A.prod_id=B.prod_id
	where (year(A.DFM)>=2006) and (year(A.DFM)<=2009)
	order by PRIM_UPC,UPC;
quit;

data PROD.new_full;
	retain PRIM_UPC UPC UPCDSC WFM WLM DFM DLM Year LIFE fail success PRIM_WFM PRIM_DFM PRIM_LIFE class test KEYCAT CATEGORY TYPE 
			VENDOR BRAND DEPARTMENT PROD_TYPE SERV_SIZE STORE_LOC VOL_EQ UNIT_MEASURE PRICE PRIM_PROMOTION PCT_AVC;
	set new_full;
	UPC_fail=fail*input(PRIM_UPC,14.);
	if fail=0 then UPC_fail=.;
	fail_cutoff = &fail_threshold/52;
proc contents;
run;

* Check number of new products in each category;
proc freq data=PROD.new_prim noprint;
	tables CATEGORY /out = tmp;
	tables CATEGORY*fail/out= tmp1;
run;
proc sort data=tmp; by descending COUNT; run;
proc sort data=tmp1; by descending fail descending COUNT; run;
			
proc datasets library=work kill noprint;run;

* Export the new product data;
* Conver the format of UPC to long, otherwise R reads in scientific notation of numbers; 
data prod_expt;
 	format UPC 15.; 
	set PROD.new_prim(keep = UPC UPCDSC PROD_ID DEPARTMENT CATEGORY BRAND DFM DLM WFM WLM PRICE PRVT_LAB VENDOR LIFE fail success test fail_cutoff);
run;

data prod_expt1; 
	format UPC 15.; 
	set PROD.new_full(keep = UPC UPCDSC PROD_ID prim DEPARTMENT CATEGORY BRAND DFM DLM WFM WLM PRICE PRVT_LAB VENDOR LIFE fail success test fail_cutoff);
run;

PROC EXPORT DATA= prod_expt
            OUTFILE= "\\tsclient\Resear1\Harbinger\processed data\new_prim.csv" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA= prod_expt1
            OUTFILE= "\\tsclient\Resear1\Harbinger\processed data\new_full.csv" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;



