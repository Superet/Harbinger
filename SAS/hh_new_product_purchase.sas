libname orig 	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Orig";
libname prod 	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Prod";
libname hh 		"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\HH";
libname trip	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Trip";
%let dirname= E:\Users\Projects\Project_Marketing_IRI\kelloggirir;

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
/*proc sql noprint;
	create table PROD.upc as 
	select *,B.UPCDSC,B.CATEGORY,B.TYPE,B.VENDOR,B.BRAND,B.DEPARTMENT,B.VOL_EQ
	from upc as A inner join ORIG.dict as B
	on A.UPC = B.UPC
	order by CATEGORY,PROD_TYPE,VEN,BRAND,LIFE descending;
quit;*/

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
	create table tmp as
	select *, DOL/UNITS as PRICE
	from TRIP.trip_det 
	where UPC in (select UPC from new_prim)
	order by UPC;
quit;	

* Use average price paid as price variable for each product;
proc univariate data=tmp noprint;
	class UPC;
	var PRICE;
	output out=tmp1 mean=price_mean mode=price_mode;
run;

data tmp1;
	set tmp1;
	PRICE = price_mode;
	if price_mode=. then PRICE=price_mean;
	drop price_mode price_mean;
run;

proc sql noprint;
	create table tmp2 as
	select A.*,B.PRICE
	from new_prim as A left join tmp1 as B
	on A.UPC = B.UPC
	order by UPC;
quit;
proc means data=tmp2;var PRICE;run;

data PROD.new_prim; set tmp2;run;

* Merge to have all the variant products;
proc sql noprint;
	create table new_full as
	select A.*, B.UPC as PRIM_UPC,B.fail,B.success,B.WFM as PRIM_WFM,B.DFM as PRIM_DFM,
		   B.class,B.test,B.PRVT_LAB,B.PRICE
	from PROD.upc as A inner join PROD.new_prim as B
	on A.prod_id=B.prod_id
	where (year(A.DFM)>=2006) and (year(A.DFM)<=2009)
	order by PRIM_UPC,UPC;
quit;

data PROD.new_full;
	retain PRIM_UPC UPC UPCDSC WFM WLM DFM DLM Year LIFE fail success PRIM_WFM PRIM_DFM class test KEYCAT CATEGORY TYPE 
			VENDOR BRAND DEPARTMENT PROD_TYPE SERV_SIZE STORE_LOC VOL_EQ UNIT_MEASURE;
	set new_full;
	UPC_fail=fail*input(PRIM_UPC,14.);
	if fail=0 then UPC_fail=.;
proc contents;
run;

* Check number of new products in each category;
proc freq data=PROD.new_prim noprint;
	tables CATEGORY /out = tmp;
	tables CATEGORY*fail/out= tmp1;
run;
proc sort data=tmp; by descending COUNT; run;
proc sort data=tmp1; by descending fail descending COUNT; run;

/** Drop candy category to avoid seasonal products;
data PROD.new_prim; set PROD.new_prim; if CATEGORY ^= 'CATEGORY - TOTAL NON-CHOCOLATE CANDY'; run;
data PROD.new_full; set PROD.new_full; if CATEGORY ^= 'CATEGORY - TOTAL NON-CHOCOLATE CANDY'; run;
*/			
			
proc datasets library=work kill noprint;run;

******************************************************;
* Construct household profiles: new product purchase *;
******************************************************;libname orig 	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Orig";
libname prod 	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Prod";
libname hh 		"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\HH";
libname trip	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Trip";
%let dirname= E:\Users\Projects\Project_Marketing_IRI\kelloggirir;

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
/*proc sql noprint;
	create table PROD.upc as 
	select *,B.UPCDSC,B.CATEGORY,B.TYPE,B.VENDOR,B.BRAND,B.DEPARTMENT,B.VOL_EQ
	from upc as A inner join ORIG.dict as B
	on A.UPC = B.UPC
	order by CATEGORY,PROD_TYPE,VEN,BRAND,LIFE descending;
quit;*/

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
	create table tmp as
	select *, DOL/UNITS as PRICE
	from TRIP.trip_det 
	where UPC in (select UPC from new_prim)
	order by UPC;
quit;	

* Use average price paid as price variable for each product;
proc univariate data=tmp noprint;
	class UPC;
	var PRICE;
	output out=tmp1 mean=price_mean mode=price_mode;
run;

data tmp1;
	set tmp1;
	PRICE = price_mode;
	if price_mode=. then PRICE=price_mean;
	drop price_mode price_mean;
run;

proc sql noprint;
	create table tmp2 as
	select A.*,B.PRICE
	from new_prim as A left join tmp1 as B
	on A.UPC = B.UPC
	order by UPC;
quit;
proc means data=tmp2;var PRICE;run;

data PROD.new_prim; set tmp2;run;

* Merge to have all the variant products;
proc sql noprint;
	create table new_full as
	select A.*, B.UPC as PRIM_UPC,B.fail,B.success,B.WFM as PRIM_WFM,B.DFM as PRIM_DFM,
		   B.class,B.test,B.PRVT_LAB,B.PRICE
	from PROD.upc as A inner join PROD.new_prim as B
	on A.prod_id=B.prod_id
	where (year(A.DFM)>=2006) and (year(A.DFM)<=2009)
	order by PRIM_UPC,UPC;
quit;

data PROD.new_full;
	retain PRIM_UPC UPC UPCDSC WFM WLM DFM DLM Year LIFE fail success PRIM_WFM PRIM_DFM class test KEYCAT CATEGORY TYPE 
			VENDOR BRAND DEPARTMENT PROD_TYPE SERV_SIZE STORE_LOC VOL_EQ UNIT_MEASURE;
	set new_full;
	UPC_fail=fail*input(PRIM_UPC,14.);
	if fail=0 then UPC_fail=.;
proc contents;
run;

* Check number of new products in each category;
proc freq data=PROD.new_prim noprint;
	tables CATEGORY /out = tmp;
	tables CATEGORY*fail/out= tmp1;
run;
proc sort data=tmp; by descending COUNT; run;
proc sort data=tmp1; by descending fail descending COUNT; run;

/** Drop candy category to avoid seasonal products;
data PROD.new_prim; set PROD.new_prim; if CATEGORY ^= 'CATEGORY - TOTAL NON-CHOCOLATE CANDY'; run;
data PROD.new_full; set PROD.new_full; if CATEGORY ^= 'CATEGORY - TOTAL NON-CHOCOLATE CANDY'; run;
*/			
			
proc datasets library=work kill noprint;run;

******************************************************;
* Construct household profiles: new product purchase *;
******************************************************;
*** Classfiy customers based their new product purchase in 2006/2008;
%ClassFn(HH_outname=hh_new_cls,trip_data=TRIP.trip_det,prod_data=PROD.new_full,class_window=52,num_group=4,repeat=);
%TestFn(outname=new_prod_test,HH_outname=hh_new_cls,trip_prod=trip_prod,test_prod_data=PROD.new_prim,test_window=52,num_group=4);

data HH.hh_new_cls; set hh_new_cls; run;

data new_prod_test;
	set new_prod_test;
	logP = log(PRICE);
run;

* Check missing sales;
proc sql;
	select count(UPC) 
	from new_prod_test where total_sale=0;
quit;

/**** Category-clustered model ***;
* Base model ;
proc genmod data=new_prod_test descending;
	class CATEGORY ;
	model success = total_sale/link=logit;
	repeated subject=CATEGORY / type = cs;
	store out1;
	output out=tmp1 xbeta=xbeta pred = predprob;
run;
ods output ParameterEstimates = out_data1;
proc plm source = out1;
	show parameters;
run;

* Separate sales by group model ;
proc genmod data=new_prod_test descending;
	class CATEGORY ;
	model success = grp1 grp2 grp3 grp4 grp0/link=logit;
	repeated subject=CATEGORY / type = cs;
	store out2;
	output out=tmp2 xbeta=xbeta pred = predprob;
run;
ods output ParameterEstimates = out_data2;
proc plm source = out2;
	show parameters;
run;

* Collect parameter estimates;
data out_data1; set out_data1; model = 'Baseline'; run;
data out_data2; set out_data2; model = 'Affinity'; run;
data logit_out; set out_data1 out_data2; run;

* ROC curve; 
ods graphics on;
proc logistic data=tmp2 plots(only) = roc;
	model success(event = '1') = total_sale;
	ods select roccurve;
	roccontrast;
run;*/

*** Simple logistic regression with no correction of se. ***;
ods trace on;
ods output ParameterEstimates=out_data1;
ods output FitStatistics = outfit_data1;
proc logistic data=new_prod_test descending;
	model success = total_sale PRVT_LAB ;
run;
ods trace off;

ods trace on;
ods output ParameterEstimates=out_data2;
ods output FitStatistics = outfit_data2;
proc logistic data=new_prod_test descending;
	model success = grp1 grp2 grp3 grp4 grp0;
run;
ods trace off;

* Collect parameter estimates;
data out_data1; 	set out_data1; model = 'Baseline'; 
data outfit_data1; 	set outfit_data1; model = 'Baseline'; run;
data out_data2; 	set out_data2; model = 'Affinity'; 
data outfit_data2; 	set outfit_data2; model = 'Affinity'; run;
data logit_out; 
	length model $ 30;
	set out_data1 out_data2; 
data logit_outfit; 
	length model $ 30;
	set outfit_data1 outfit_data2; run;
proc datasets noprint; delete out_data1 out_data2 outfit_data1 outfit_data2; run;

* ROC curve comparison;
ods graphics on;
proc logistic data=new_prod_test plots = roc(id=prob);
	model success(event='1') = grp1 grp2 grp3 grp4 grp0 total_sale /nofit;
	roc 'Baseline' total_sale;
	roc 'Separate sale' grp1 grp2 grp3 grp4 grp0;
	roccontrast reference('Baseline') / estimate e;
run;
ods graphics off;

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
ods trace on;
ods output ParameterEstimates=out_data2;
ods output FitStatistics = outfit_data2;
proc logistic data=new_prod_test descending;
	model success = grp1 grp2 grp3 grp4 grp0;
run;
ods trace off;

* Collect parameter estimates;
data out_data2; 	set out_data2; model = 'Affinity2'; 
data outfit_data2; 	set outfit_data2; model = 'Affinity2'; run;
data logit_out; set logit_out out_data2; 
data logit_outfit; set logit_outfit outfit_data2; run;

proc datasets noprint; delete out_data2 outfit_data2; run;

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
ods trace on;
ods output ParameterEstimates=out_data2;
ods output FitStatistics = outfit_data2;
proc logistic data=new_prod_test descending;
	model success = grp1 grp2 grp3 grp4 grp0;
run;
ods trace off;

* Collect parameter estimates;
data out_data2; set out_data2; model = 'Affinity3'; run;
data outfit_data2; 	set outfit_data2; model = 'Affinity3'; run;
data logit_out; set logit_out out_data2; run;
data logit_outfit; set logit_outfit outfit_data2; run;

proc datasets noprint; delete out_data2 outfit_data2; run;

*-------------------------------------------------------------;
* Robustness check * ;
* Drop entire candy category;
data new_prim; set PROD.new_prim; if CATEGORY ^= 'CATEGORY - TOTAL NON-CHOCOLATE CANDY'; run;
data new_full; set PROD.new_full; if CATEGORY ^= 'CATEGORY - TOTAL NON-CHOCOLATE CANDY'; run;

%ClassFn(HH_outname=hh_new_cls,trip_data=TRIP.trip_det,prod_data=new_full,class_window=52,num_group=4,repeat=);
%TestFn(outname=new_prod_test,HH_outname=hh_new_cls,trip_prod=trip_prod,test_prod_data=new_prim,test_window=52,num_group=4);

** Simple logistic regression with no correction of se. ***;
ods trace on;
ods output ParameterEstimates=out_data2;
ods output FitStatistics = outfit_data2;
proc logistic data=new_prod_test descending;
	model success = grp1 grp2 grp3 grp4 grp0;
run;
ods trace off;

* Collect parameter estimates;
data out_data2; set out_data2; model = 'DropCandy'; run;
data outfit_data2; 	set outfit_data2; model = 'DropCandy'; run;
data logit_out; set logit_out out_data2; run;
data logit_outfit; set logit_outfit outfit_data2; run;

proc datasets noprint; delete out_data2 outfit_data2; run;

proc datasets noprint; delete hh_new_cls tmp tmp1 tmp2 trip_new_cls trip_new_test tmp_trip new_prim new_full; run;

***********************************************;
* Check the difference of groups of consumers *; 
***********************************************;
/*
1. Whether harbingers experiment more new products: number of new products;
2. Demographic difference: FAMSIZE,INCOME,CHILDREN,M_STATUS,AGE,HRS,EDU
3. Shopping behavior per trip: BSKTDOL, inter_purchase, num_cat, num_upc, pct_coupon, pct_priceoff, num_brands per category
							   wallet share (BDD, Edible, Frozen, General merchandise, HBC, Nonedible, Nonscan);
4. Do they have bad tastes? Test purchase of niche products: num_niche, pct_niche;
5. Are harbingers early adopters and followers? Examine the adoption lag: 
		adoption days since introduction of all new products, failed products, sucessful products;
6. Between store search per week: number of chains, expenditure at the primary chain;
7. Variety seeking difference for a given category, number of brands purchased, repeat purchase frequence, entropy, and HHI;
*/

data HH.HH_new_cls;
	set HH.HH_new_cls;
	grp = 'Harbinger';
	if cls_grp <= 2 then grp = 'Normal';
run;

* Experimental difference: number of new product purchase; 
proc ttest data=HH.HH_new_cls;
	class grp;
	var num_allnew;
run;

*-------------------------------------------------------------;
* Demongraphic difference;
proc sql noprint;
	create table hh_new_cls as
	select A.*,B.INCOME,B.FAMSIZE,B.CHILDREN,B.M_STATUS,B.FMLE_AGE,B.MALE_AGE,B.FMLE_HRS,B.MALE_HRS,B.FMLE_EDU,B.MALE_EDU
	from HH.HH_new_cls as A left join ORIG.demo as B
	on A.PANID=B.PANID
	order by PANID;
quit;
	
data HH.hh_new_cls; set hh_new_cls; run;
	
*-------------------------------------------------------------;
* Purchase behavior per trip of all the products;
proc sql noprint;	
	* Merge with UPC data;
	create table tmp1 as 
	select A.*,B.CATEGORY,B.BRAND,B.DEPARTMENT
	from TRIP.trip_det as A left join ORIG.dict as B
	on A.UPC = B.UPC;

	* Compute per trip information;
	create table tmp2 as
	select	PANID,DATE,CHAIN,
			count(distinct UPC) as num_upc,count(distinct CATEGORY) as num_cat,mean(COUPON) as pct_coupon, mean(PRICEOFF) as pct_priceoff,
			sum(DOL*1*(DEPARTMENT<4)) as DOL_BDD,sum(DOL*(1*(DEPARTMENT=4))) as DOL_EDIBLE,sum(DOL*(1*(DEPARTMENT=5))) as DOL_FROZEN,
			sum(DOL*(1*(DEPARTMENT=6))) as DOL_GENERALM,sum(DOL*(1*(DEPARTMENT=7))) as DOL_HBC,sum(DOL*(1*(DEPARTMENT=8))) as DOL_NONEDIBLE
	from tmp1
	group by PANID,DATE,CHAIN;

	* Count categories with multiple brands purchase;
	create table tmp as
	select 	PANID,DATE,CHAIN,CATEGORY,count(distinct BRAND) as num_brand
	from tmp1 
	group by PANID,DATE,CHAIN,CATEGORY;

	create table tmp3 as 
	select PANID,DATE,CHAIN,mean(num_brand) as num_brand_prcat
	from tmp
	group by PANID,DATE,CHAIN;

	* Merge the two datasets above;
	create table tmp as 
	select A.*,B.num_brand_prcat
	from tmp2 as A full join tmp3 as B
	on (A.PANID=B.PANID) & (A.DATE=B.DATE) & (A.CHAIN=B.CHAIN);
	
	* Merge with the trip summary data;
	create table tmp_trip as
	select A.*, B.BSKTDOL
	from tmp as A full join ORIG.trip_sum as B
	on (A.PANID=B.PANID) & (A.DATE=B.DATE) & (A.CHAIN=B.CHAIN)
	order by PANID,DATE,CHAIN;
quit;

proc datasets noprint; delete tmp tmp1 tmp2 tmp3;run;

* Compute interpurchase days;
data tmp_trip;
	set tmp_trip;
	if DOL_BDD=. 		then DOL_BDD=0;
	if DOL_EDIBLE=. 	then DOL_EDIBLE=0;
	if DOL_FROZEN=. 	then DOL_FROZEN=0;
	if DOL_GENERALM=. 	then DOL_GENERALM=0;
	if DOL_HBC=. 		then DOL_HBC=0;
	if DOL_NONEDIBLE=. 	then DOL_NONEDIBLE=0;
	DOL_NONSCAN = BSKTDOL-DOL_BDD-DOL_EDIBLE-DOL_FROZEN-DOL_GENERALM-DOL_HBC-DOL_NONEDIBLE;
	PCT_BDD			= DOL_BDD/BSKTDOL;
	PCT_EDIBLE		= DOL_EDIBLE/BSKTDOL;
	PCT_FROZEN		= DOL_FROZEN/BSKTDOL;
	PCT_GENERALM	= DOL_GENERALM/BSKTDOL;
	PCT_HBC			= DOL_HBC/BSKTDOL;
	PCT_NONEDIBLE	= DOL_NONEDIBLE/BSKTDOL;
	PCT_NONSCAN		= DOL_NONSCAN/BSKTDOL;	
	inter_purchase 	= dif(DATE);
	by PANID;
	if first.PANID then inter_purchase=.;
run;

* Summarize purchase behavior for each PANID;
proc means data=tmp_trip noprint ;
	class PANID;
	var BSKTDOL inter_purchase num_upc num_cat pct_coupon pct_priceoff num_brand_prcat 
		PCT_BDD PCT_EDIBLE PCT_FROZEN PCT_GENERALM PCT_HBC PCT_NONEDIBLE PCT_NONSCAN;
	output out=tmp MEAN=;
run;

proc sql noprint;	
	create table hh_new_cls(drop=PANID_old _TYPE_) as 
	select *
	from HH.hh_new_cls as A left join tmp(rename=(PANID=PANID_old _FREQ_=num_trip)) as B
	on A.PANID = B.PANID_old;
quit;

data HH.hh_new_cls; set hh_new_cls; run;
proc datasets noprint; delete tmp tmp1 tmp_trip; run;

*-------------------------------------------------------------;
* Do harbingers buy more niche products? ;

* The trip records of existing products;
proc sql noprint;
	create table tmp_trip as 
	select *,B.prod_id
	from TRIP.trip_det as A left join PROD.upc as B
	on A.UPC = B.UPC
	where A.UPC not in (select UPC from PROD.new_full);

	create table prod_pop as
	select prod_id,count(distinct PANID) as num_phh, sum(UNITS) as UNITS
	from tmp_trip
	group by prod_id;
	
	select count(distinct PANID), sum(UNITS) into :numpan, :totalunits from tmp_trip;
quit;

* Compute the popularity of products;
data PROD.exist_pop;
	set prod_pop;
	if (num_phh>0) and (UNITS^=.);
	total_pan 	= &numpan;
	total_units = &totalunits;
	pop 		= num_phh/total_pan;
	pop_units 	= units/total_units;
run;
proc univariate data=PROD.exist_pop noprint; histogram pop pop_units; run;
proc means data=PROD.exist_pop N MEAN STD MEDIAN MIN P1 P5 P10 P25 P75 P90 MAX; 
var pop pop_units; run;

* Define niche products;
data PROD.exist_pop;
	set PROD.exist_pop;
	niche = .;
	if pop < &popthresh then niche=1;
proc freq; tables niche;
run;

* Compute the number of niche products for each household;
proc sql noprint;
	create table tmp as
	select A.*,B.niche, B.niche*A.prod_id as niche_id
	from tmp_trip as A full join PROD.exist_pop as B
	on A.prod_id = B.prod_id;
	
	create table tmp1 as 
	select PANID,count(distinct niche_id) as num_niche,count(distinct niche_id)/count(distinct prod_id) as pct_niche
	from tmp
	group by PANID;
	
	create table hh_new_cls as
	select *, B.num_niche,B.pct_niche
	from HH.hh_new_cls as A left join tmp1 as B
	on A.PANID = B.PANID;
quit;

data HH.hh_new_cls; set hh_new_cls; run;
proc datasets noprint; delete tmp tmp1 tmp_trip prod_pop; run;

*-------------------------------------------------------------;
* Do harbingers buy more niche category? ;
* Compute categroy sales contribution;
proc sql noprint;
	create table tmp_trip as 
	select *,B.CATEGORY
	from TRIP.trip_det as A left join PROD.upc as B
	on A.UPC = B.UPC
	where A.UPC not in (select UPC from PROD.new_full);

	create table tmp as 
	select CATEGORY, sum(DOL) as DOL
	from tmp_trip 
	group by CATEGORY 
	order by CATEGORY;
	
	create table cat_pop as 
	select *, sum(DOL) as total_dol, dol/sum(dol) as contr
	from tmp
	order by contr descending;
quit;

proc means data=cat_pop N MEAN STD MEDIAN MIN P1 P5 P10 P25 P75 P90 MAX; 
	var contr; 
	output out = tmp P25=;
run;
proc univariate data=cat_pop noprint; histogram contr; run;

data tmp;
	set tmp; 
	call symput('cat_pop_thresh', contr);
run;

data cat_pop;
	set cat_pop;
 	niche = 0;
	if contr <&cat_pop_thresh then niche = 1;
run;

* Compute consumers expenditure share of each category;
proc sql noprint;
	create table tmp as
	select PANID, CATEGORY, sum(DOL) as dol
	from tmp_trip 
	group by PANID, CATEGORY;

	create table tmp1 as 
	select A.*, B.niche
	from tmp as A inner join cat_pop as B
	on A.CATEGORY = B.CATEGORY;

	create table tmp2 as 
	select PANID, sum(dol*niche)/sum(dol) as niche_cat_share
	from tmp1 
	group by PANID;
	
	create table hh_new_cls as
	select A.*, B.niche_cat_share
	from HH.HH_new_cls as A left join tmp2 as B
	on A.PANID = B.PANID;
quit;

data HH.hh_new_cls; set hh_new_cls; run;

proc datasets noprint; delete cat_pop tmp tmp1 tmp2; run;

*-------------------------------------------------------------;
* Are harbingers early adopters? ;
proc sql noprint;
	create table tmp_trip as
	select A.*,B.PRIM_UPC,B.WFM,B.DFM,B.fail,B.success
	from TRIP.trip_det as A right join PROD.new_full as B
	on A.UPC = B.UPC;
	
	create table tmp as
	select PANID,PRIM_UPC,fail, success,min(DATE - DFM) as adoption
	from tmp_trip
	where PANID is not missing
	group by PANID,PRIM_UPC,fail,success
	order by PANID,PRIM_UPC;

	* Average over products for each households;
	create table tmp1 as
	select PANID, mean(adoption) as adoption_all, sum(adoption*fail)/sum(fail) as adoption_fail,
		   sum(adoption*success)/sum(success) as adoption_success
	from tmp
	group by PANID;
	
	* Merge it with the household profile data;
	create table hh_new_cls as
	select A.*,B.adoption_all,B.adoption_fail,B.adoption_success
	from HH.hh_new_cls as A left join tmp1 as B
	on A.PANID = B.PANID
	order by PANID;
quit;

data HH.hh_new_cls; set hh_new_cls; run;
proc datasets noprint; delete tmp_trip tmp tmp1; run;

*-------------------------------------------------------------;
* Do harbingers search less ? ;
* Number of stores visited per week, and expenditure share in the primary store;
data trip_sum;
	set ORIG.trip_sum;
	WEEK = cats(YEAR, '-',put(week(DATE),2.));
run;

proc sql noprint;
	create table tmp1 as
	select PANID,CHAIN,sum(BSKTDOL) as DOL
	from ORIG.trip_sum
	group by PANID,CHAIN
	order by PANID,DOL;
quit;

* Find primary store;
data tmp1;
	set tmp1;
	by PANID DOL;
	rank = 0;
	if first.PANID then DOL_ALL = 0;
	DOL_ALL + DOL;
	if last.PANID then rank=1;
run;

proc sql noprint;
	create table tmp as 
	select A.*, B.rank
	from trip_sum as A full join tmp1 as B
	on (A.PANID=B.PANID) and (A.CHAIN=B.CHAIN);
	
	create table tmp1 as 
	select PANID, count(distinct CHAIN) as num_chain ,sum(BSKTDOL*rank)/sum(BSKTDOL) as prim_share 
	from tmp 
	group by PANID,WEEK;
	
	create table tmp as 
	select PANID, mean(num_chain) as num_chain, mean(prim_share) as prim_share
	from tmp1
	group by PANID;
quit;

* Merge to household profile;
proc sql noprint;
	create table hh_new_cls as
	select A.*,B.num_chain,B.prim_share
	from HH.hh_new_cls as A left join tmp as B
	on A.PANID=B.PANID
	order by PANID;
quit;

data HH.hh_new_cls; set hh_new_cls; run;
proc datasets noprint; delete tmp tmp1 trip_sum; run;	

*-------------------------------------------------------------;
*Variety seeking difference for a given category;  
*number of brands purchased, repeat purchase frequence, entropy, and HHI;
proc sql noprint;
	create table tmp as 
	select UPC,CATEGORY,BRAND,DEPARTMENT,VOL_EQ, count(distinct BRAND) as proliferation
	from ORIG.dict
	group by CATEGORY;

	create table trip_det_upc as 
	select A.*,B.CATEGORY,B.BRAND,B.DEPARTMENT,B.VOL_EQ,B.proliferation
	from TRIP.trip_det as A left join tmp as B
	on A.UPC = B.UPC
	order by PANID,CATEGORY,DATE;
quit;

data trip_det_upc;
	set trip_det_upc;
	last_brand = lag1(brand);
	repeat_purch = 0;
	by PANID CATEGORY;
	if first.CATEGORY then do; 
		last_brand = .;
		repeat_purch = .;
	end;
	if brand = last_brand then repeat_purch = 1;
run;

proc sql noprint;	
	* Compute brand quantity share;
	create table tmp as
	select PANID, CATEGORY,BRAND, sum(UNITS*VOL_EQ) as quantity
	from trip_det_upc 
	group by PANID,CATEGORY,BRAND
	order by PANID,CATEGORY;
	
	create table tmp1 as 
	select PANID, CATEGORY, BRAND, quantity/sum(quantity) as share_qnt
	from tmp
	group by PANID, CATEGORY;
	
	create table tmp2 as
	select PANID,CATEGORY,sum(share_qnt**2) as HHI, sum(-share_qnt*log(share_qnt)) as ENT
	from tmp1 
	group by PANID, CATEGORY;

	* Compute brand repeat purchase;
	create table tmp1 as 
	select PANID, CATEGORY, proliferation, count(distinct BRAND) as all_num_brand, 
			count(DATE) as num_purchase, sum(repeat_purch) as repeat_purch, 
			sum(repeat_purch)/count(DATE) as repeat_freq,
			min(proliferation, count(DATE)) as N_star
	from trip_det_upc
	group by PANID,CATEGORY, proliferation;

	* Merge the two; 
	create table tmp as
	select A.*, B.HHI, B.ENT, -num_purchase/N_star*log(1/N_star) as ENT_max, 
		   B.ENT/( -num_purchase/N_star*log(1/N_star)) as ENTR
	from tmp1 as A inner join tmp2 as B
	on A.PANID = B.PANID and A.CATEGORY = B.CATEGORY
	where num_purchase >= 5
	order by PANID, CATEGORY;
	
	* Merge with households group classification;
	create table variety_test as 
	select A.*, B.grp, B.cls_grp 
	from tmp as A inner join HH.hh_new_cls as B
	on A.PANID = B.PANID
	order by CATEGORY, PANID;
quit;

* Regression to test;
proc sort data=variety_test; by CATEGORY PANID; run;

proc glm data=variety_test; 
	absorb CATEGORY;
	class grp;
	model all_num_brand=grp num_purchase / solution;
run;
proc glm data=variety_test; 
	absorb CATEGORY;
	class grp;
	model HHI=grp num_purchase/ solution;
run;
proc glm data=variety_test; 
	absorb CATEGORY;
	class grp;
	model ENTR=grp / solution;
run;
proc glm data=variety_test; 
	absorb CATEGORY;
	class grp;
	model repeat_freq=grp / solution;
run;

* Average over catgory and merge to household profile;
proc means data=variety_test noprint;
	class PANID;
	var all_num_brand HHI ENTR repeat_freq;
	output out = tmp MEDIAN=;
run;

proc sql noprint;
	create table hh_new_cls as
	select A.*,B.all_num_brand, B.HHI, B.ENTR, B.repeat_freq
	from HH.hh_new_cls as A left join tmp as B
	on A.PANID=B.PANID
	order by PANID;
quit;

data HH.hh_new_cls; set hh_new_cls; run;
proc datasets noprint; delete tmp tmp1 tmp2; run;

*-------------------------------------------------------------;
* T test the difference ;
data HH.hh_new_cls;
	retain PANID cls_grp grp num_fail num_allnew affinity 
		   num_trip BSKTDOL inter_purchase num_cat num_upc num_brand_prcat pct_coupon pct_priceoff
		   PCT_BDD PCT_EDIBLE PCT_FROZEN PCT_GENERALM PCT_HBC PCT_NONEDIBLE PCT_NONSCAN 
		   num_niche pct_niche niche_cat_share adoption_all adoption_fail adoption_success
		   num_chain prim_share all_num_brand HHI ENTR repeat_freq
		   ;
	set HH.hh_new_cls;
	label 	num_allnew		= 'Number of new products purchased in classification period'
			num_fail		= 'Number of failed new producuts purchased in classification period'
			affinity		= 'Affinity=num_fail/num_allnew'
			cls_grp			= 'Classfication group'
			grp				= 'Harbinger or normal'
			num_trip		= 'Number of total trips'
			BSKTDOL			= 'Basket dollars per trip'
			inter_purchase 	= 'Interpurchase days'
			num_cat			= 'Number of category purchased per trip'
			num_upc			= 'Number of UPC purchased per trip'
			num_brand_prcat = 'Number of brands per category per trip'
			pct_coupon		= 'Percentage of items with coupon'
			pct_priceoff	= 'Percentage of items with price-off'
			PCT_BDD			= 'Wallet share of Bread/Deli/Dairy'
			PCT_EDIBLE		= 'Wallet share of Edible'
			PCT_FROZEN		= 'Wallet share of Frozen'
			PCT_GENERALM	= 'Wallet share of Gerneral merchandise'
			PCT_HBC			= 'Wallet share of HBC'
			PCT_NONEDIBLE	= 'Wallet share of Nonedible'
			PCT_NONSCAN		= 'Wallet share of Nonscannable'
			num_niche		= 'Number of niche products purchased'
			pct_niche		= 'Percent of niche products out of all products purchased'
			niche_cat_share	= 'Expenditure share of niche products'
			adoption_all	= 'Average adoption lag of all new products'
			adoption_fail	= 'Average adoption lag of failed new products'
			adoption_success= 'Average adoption lag of successful new products'
			num_chain		= 'Number of chains visited per week'
			prim_share		= 'Expenditure share at the primary store'
			all_num_brand	= 'Median number of brands in a category'
			HHI				= 'Median brand quantity HHI in a category'
			ENTR			= 'Median entropy of brand quantity in a category'
			repeat_freq		= 'Median repeated brand with last purchase in a category'
			;
run;

proc ttest data=HH.hh_new_cls;
	class grp;
	var num_allnew BSKTDOL inter_purchase num_upc num_cat pct_coupon pct_priceoff num_brand_prcat 
		PCT_BDD PCT_EDIBLE PCT_FROZEN PCT_GENERALM PCT_HBC PCT_NONEDIBLE PCT_NONSCAN 
		num_allnew num_niche pct_niche
		adoption_all adoption_fail adoption_success num_chain prim_share;
	ods output Ttests=pan_test;
run;

*-------------------------------------------------------------;
* Revnue contribution of each group;
proc sql noprint;
	create table tmp as
	select PANID, CHAIN, sum(BSKTDOL) as BSKTDOL
	from ORIG.trip_sum
	group by PANID, CHAIN
	order by PANID;
	
	create table tmp1 as 
	select A.*, B.cls_grp, B.grp
	from tmp as A left join HH.HH_new_cls as B
	on A.PANID = B.PANID;
quit;

data tmp1;
	set tmp1;
	if cls_grp = . then cls_grp=0;
run;

proc sql noprint;
	create table tmp2 as 
	select CHAIN, cls_grp, sum(BSKTDOL) as BSKTDOL
	from tmp1 
	group by CHAIN,cls_grp;
	
	create table tmp as 
	select *, sum(BSKTDOL) as revenue, BSKTDOL/sum(BSKTDOL) as rev_contr
	from tmp2 
	group by CHAIN
	order by CHAIN, cls_grp;
quit;

proc transpose data=tmp out=chain_rev_contr prefix = rev_contr;
	by CHAIN;
	id cls_grp;
	var rev_contr;
run;
	
data chain_rev_contr;
	set chain_rev_contr;
	Harbinger_contr = rev_contr3 + rev_contr4;
	Normal_contr	= rev_contr1 + rev_contr2;
	diff 			= Harbinger_contr - Normal_contr;
run;
proc means data=chain_rev_contr; var diff; run; 
proc ttest data=chain_rev_contr; paired Harbinger_contr*Normal_contr; run;

data tmp; 
	set ORIG.tripcode; 
	if TRIP_CODE = "CHAIN"; 
	rename CODE = CHAIN TRIP_DSC = CHAIN_NAME;
	drop TRIP_CODE;
proc sort; by chain;
run;

data chain_rev_contr;
	merge tmp chain_rev_contr;
	by CHAIN;
run;

* Plot a few chain;
data tmp;
	set chain_rev_contr(drop=_name_);
	where CHAIN_NAME in ('Aldi' 'Dominicks' 'Jewel/Osco' 'Kroger' 'Publix Super Market'
						'Whole Foods' "Trader Joe's Supermarket" "7   Eleven" 
						'Dollar General' 'Costco' 'Dollar General' 'Wal*Mart Super Center' 'Target Trad.');
run;

proc sort data=tmp; by chain_name;run;
proc transpose data=tmp(keep= chain_name rev_contr0-rev_contr4) out=tmp1;
	by chain_name;
run;


proc gchart data=tmp1;
	hbar chain_name/discreate sumvar=col1 subgroup=_name_;
run;
		
*-------------------------------------------------------------;
* Export to excel data;
PROC EXPORT DATA=HH.HH_new_cls
            OUTFILE= "\\tsclient\Resear1\Store switching\processed data\new_product_HH_class.csv" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;	

* Export the new product data;
PROC EXPORT DATA= PROD.new_full
            OUTFILE= "\\tsclient\Resear1\Store switching\processed data\new_product.csv" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

* Export new product failure test;
PROC EXPORT DATA= new_prod_test
            OUTFILE= "\\tsclient\Resear1\Store switching\processed data\new_product_test.csv" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA= fail_logit
            OUTFILE= "\\tsclient\Resear1\Store switching\processed data\new_product_logit_results.csv" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

* Export the t-test results of shopping behavior;
PROC EXPORT DATA= pan_test
            OUTFILE= "\\tsclient\Resear1\Store switching\processed data\new_product_behavior_dif.csv" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

proc datasets library=ORIG kill noprint; run;
proc datasets library=TRIP kill noprint; run;





