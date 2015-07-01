/*
Input: consumer segments computed from SAS file 2 (main); 
Output: 
1. Household profile (key=PANID): demographics, classification index, behavior metrics; 
2. Regression parameter estimates: testing regression of variety seeking and repeat purchase; 
3. Testing results; 
*/
* Set output options; 
libname orig 	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Orig";
libname prod 	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Prod";
libname hh 		"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\HH";
libname trip	"E:\Users\ccv103\Documents\Research\Store switching\SAS_temp\Trip";
%let dirname= E:\Users\Projects\Project_Marketing_IRI\kelloggirir;

options compress=yes reuse=yes;

%let popthresh = .0002;
proc sql noprint; 
select unique(fail_cutoff) into: fail_cutoff from PROD.new_prim; 
quit;
%put &fail_cutoff;

%let HH_outfile = %sysfunc(catx(%str(), E:\Users\ccv103\Desktop\result_4_SAS_hh_class_cutoff,&fail_cutoff,.csv));
%let reg_outfile = %sysfunc(catx(%str(), E:\Users\ccv103\Desktop\result_4_SAS_reg_cutoff,&fail_cutoff,.csv));
%let test_outfile = %sysfunc(catx(%str(), E:\Users\ccv103\Desktop\result_4_SAS_test_cutoff,&fail_cutoff,.csv));
%put &HH_outfile; 
%put &reg_outfile; 
%put &test_outfile; 

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
8. Repeat purchase. 
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

************************************************************;
* Demongraphic difference;
proc sql noprint;
	create table hh_new_cls as
	select A.*,B.INCOME,B.FAMSIZE,B.CHILDREN,B.M_STATUS,B.FMLE_AGE,B.MALE_AGE,B.FMLE_HRS,B.MALE_HRS,B.FMLE_EDU,B.MALE_EDU
	from HH.HH_new_cls as A left join ORIG.demo as B
	on A.PANID=B.PANID
	order by PANID;
quit;
	
data HH.hh_new_cls; set hh_new_cls; run;
	
************************************************************;
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
proc datasets noprint; delete tmp tmp1 tmp_trip _doctmp:; run;

************************************************************;
* Do harbingers buy more niche products? ;

* The trip records of existing products;
proc sql noprint;
	create table tmp_trip as 
	select A.*,B.prod_id,B.CATEGORY,B.BRAND
	from TRIP.trip_det as A left join PROD.upc as B
	on A.UPC = B.UPC
	where A.UPC not in (select UPC from PROD.new_full);

	create table prod_pop as
	select CATEGORY, BRAND, prod_id,count(distinct PANID) as num_phh, sum(UNITS) as UNITS, sum(DOL) as dol
	from tmp_trip
	group by CATEGORY, BRAND, prod_id
	order by CATEGORY, BRAND, prod_id;
	
	select count(distinct PANID), sum(UNITS) into :numpan, :totalunits from tmp_trip;
quit;

* Compute the popularity of products;
data exist_pop;
	set prod_pop;
	if (num_phh>0) and (UNITS^=.);
	total_pan 	= &numpan;
	total_units = &totalunits;
	pop 		= num_phh/total_pan;
	pop_units 	= units/total_units;
run;
proc univariate data=exist_pop noprint; var pop pop_units; histogram; run;
proc means data=exist_pop N MEAN STD MEDIAN MIN P1 P5 P10 P25 P75 P90 MAX; 
var pop pop_units; run;

* Rank the popularity within each category; 
proc rank data = prod_pop out = tmp_rank descending; 
	by CATEGORY; 
	VAR UNITS; 
	ranks units_rank; 
run;

* Define niche products;
data exist_pop;
	set exist_pop; 
	by CATEGORY BRAND prod_id; 
	niche = .;
	if pop < &popthresh then niche=1;
proc freq; tables niche;
run;

* Merge rank data;
proc sql noprint; 
	create table PROD.exist_pop as 
	select A.*, B.units_rank
	from exist_pop as A full join tmp_rank as B
	on A.prod_id = B.prod_id
	order by CATEGORY, BRAND, prod_id;
quit;

* Compute the number of niche products for each household;
proc sql noprint;
	create table tmp as
	select A.*,B.niche, B.niche*A.prod_id as niche_id, B.units_rank
	from tmp_trip as A full join PROD.exist_pop as B
	on A.prod_id = B.prod_id;
	
	create table tmp1 as 
	select PANID,count(distinct niche_id) as num_niche,count(distinct niche_id)/count(distinct prod_id) as pct_niche, 
			mean(units_rank) as units_rank
	from tmp
	group by PANID;
	
	create table hh_new_cls as
	select A.*, B.num_niche,B.pct_niche,B.units_rank
	from HH.hh_new_cls as A left join tmp1 as B
	on A.PANID = B.PANID;
	
	create table tmp1 as 
	select PANID, CATEGORY, mean(units_rank) as units_rank, sum(units_rank*UNITS)/sum(UNITS) as wt_units_rank
	from tmp
	group by PANID, CATEGORY; 
	
	create table mydata as 
	select A.*, B.cls_grp, B.grp, B.affinity
	from tmp1 as A left join hh_new_cls as B
	on A.PANID = B.PANID
	order by CATEGORY, cls_grp descending; 
quit;

data HH.hh_new_cls; set hh_new_cls; run;

*--------------------------------*; 
* Run regression of units ranks; 
proc glm data = mydata(where = (cls_grp>0)) order = data; 
	absorb CATEGORY; 
	class cls_grp; 
	model units_rank = cls_grp / solution; 
	title 'Regression of units_rank = group at hh-category';
	ods output ParameterEstimates = tmp; 
run; 
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30. ; set tmp; data_level = "HH-CATEGORY"; IV = "group"; run;
data myreg; set tmp; proc sort; by IV Dependent; run;

proc glm data = mydata(where = (cls_grp>0)) order = data; 
	absorb CATEGORY; 
	model units_rank = affinity / solution;
	title 'Regression of units_rank = affinity at hh-category';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-CATEGORY"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc datasets noprint; delete tmp tmp1 tmp_trip prod_pop mydata exist_pop tmp_rank prod_pop; run;

************************************************************;
* Do harbingers buy more niche category? ;
* Compute categroy sales contribution;
proc sql noprint;
	create table tmp_trip as 
	select A.*,B.CATEGORY
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

************************************************************;
* Are harbinger buy lower price tier products? ; 

* The trip records of existing products;
proc sql noprint;
	create table tmp_trip as 
	select A.*,B.prod_id,B.CATEGORY,B.TYPE, B.BRAND, B.vol_eq
	from TRIP.trip_det as A left join PROD.upc as B
	on A.UPC = B.UPC
	where A.UPC not in (select UPC from PROD.new_full);

	create table prod_price as
	select CATEGORY, BRAND, TYPE, prod_id, sum(UNITS) as UNITS, sum(DOL) as dol, sum(DOL)/sum(UNITS*vol_eq) as unit_price
	from tmp_trip
	group by CATEGORY, TYPE, BRAND, prod_id
	order by CATEGORY, TYPE, BRAND, prod_id;
quit;

* Rank price tier within category;
proc rank data = prod_price(where=(prod_id^=.)) out = tmp_rank descending ties = low; 
	by CATEGORY TYPE; 
	VAR unit_price; 
	ranks price_tier; 
run;

* Merge rank data;
proc sql noprint; 
	create table tmp as 
	select A.*, B.price_tier
	from prod_price as A inner join tmp_rank as B
	on A.prod_id = B.prod_id
	order by CATEGORY, TYPE, BRAND, prod_id;
quit;
data prod_price; set tmp; run; 

* Compute the the price tier within category for each household; 
proc sql noprint; 
	* Merge price rank data with household data; 
	create table tmp as
	select A.*, B.price_tier
	from tmp_trip as A full join prod_price as B
	on A.prod_id = B.prod_id;
	
	* Take average price tier within category; 
	create table tmp1 as 
	select PANID, CATEGORY, TYPE, mean(price_tier) as price_tier, sum(price_tier*UNITS*vol_eq)/sum(UNITS*vol_eq) as wt_price_tier
	from tmp
	group by PANID, CATEGORY, TYPE; 
	
	* Merge in household classification;
	create table mydata as
	select A.*, B.cls_grp, B.grp, B.affinity, cats(CATEGORY, '*', TYPE) as subcat
	from tmp1 as A left join hh_new_cls as B
	on A.PANID = B.PANID
	order by subcat, cls_grp descending;
	
	* Further aggregate at household level; 
	create table tmp1 as 
	select PANID, mean(price_tier) as price_tier
	from tmp 
	group by PANID;
	
	* Add the new metric to the household data; 
	create table hh_new_cls as 
	select A.*, B.price_tier
	from HH.hh_new_cls as A left join tmp1 as B
	on A.PANID = B.PANID;
quit; 

data HH.hh_new_cls; set hh_new_cls; run;

*--------------------------------*; 
* Run regression of price tier; 
proc glm data = mydata(where = (cls_grp>0)) order = data; 
	absorb subcat; 
	class cls_grp; 
	model price_tier = cls_grp / solution; 
	title 'Regression of price_tier = group at hh-category';
	ods output ParameterEstimates = tmp; 
run; 
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30. ; set tmp; data_level = "HH-CATEGORY"; IV = "group"; run;
data myreg; merge myreg tmp; by IV Dependent; proc sort; by IV Dependent; run;

proc glm data = mydata(where = (cls_grp>0)) order = data; 
	absorb subcat; 
	model price_tier = affinity / solution;
	title 'Regression of price_tier = affinity at hh-category';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-CATEGORY"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc datasets noprint; delete tmp_trip prod_price tmp_rank mydata tmp tmp1; run; 

************************************************************;
* Do harbingers shop more often in less poplar chains? ; 
* Compute revenue share of each chain within market; 
proc sql noprint; 
	* Append geographic market to trip summary; 
	create table tmp_trip as 
	select A.*, B.MKT
	from ORIG.trip_sum as A left join ORIG.demo as B
	on A.PANID = B.PANID;
	
	* Sum up sales of chains by market; 
	create table chain_rev as 
	select MKT, CHAIN, sum(BSKTDOL) as rev
	from tmp_trip 
	group by MKT, CHAIN
	order by MKT; 
quit; 

* Rank popularity of chains; 
proc rank data = chain_rev out = tmp_rank descending ties = low; 
	by MKT; 
	VAR rev; 
	ranks popl; 
run;

* Compute the the price tier within category for each household; 
proc sql noprint; 
	* Merge chain rank data with household data; 
	create table tmp as 
	select A.*, B.popl
	from tmp_trip as A left join tmp_rank as B
	on A.MKT = B.MKT and A.CHAIN = B.CHAIN; 
	
	* Compute expenditure share at each chain for every household; 
	create table tmp1 as 
	select MKT, PANID, CHAIN, popl, sum(BSKTDOL) as DOL
	from tmp 
	group by MKT, PANID, CHAIN, popl; 
	
	create table tmp2 as 
	select MKT, PANID, CHAIN, popl, DOL/sum(DOL) as share
	from tmp1 
	group by MKT, PANID; 
	
	create table mydata as
	select A.*, B.cls_grp, B.grp, B.affinity
	from tmp2 as A left join hh_new_cls as B
	on A.PANID = B.PANID
	order by CHAIN, cls_grp descending;
	
	* Compute the popularity ranks weighted by expenditure share for each HH; 
	create table tmp2 as 
	select PANID, sum(popl * share) as share
	from mydata
	group by PANID; 
	
	* Merge in the household data; 
	create table hh_new_cls as 
	select A.*, B.share as chain_poplarity
	from HH.hh_new_cls as A left join tmp2 as B
	on A.PANID = B.PANID;
quit; 

data HH.hh_new_cls; set hh_new_cls; run;

*--------------------------------*; 
* Run regression of expenditure share at chain; 
* share ~ grp * rank; 
proc glm data = mydata(where = (cls_grp>0)) order = data; 
	absorb CHAIN; 
	class cls_grp MKT; 
	model share = cls_grp popl cls_grp*popl/ solution; 
	title 'Regression of expenditure share = group at hh-chain';
	ods output ParameterEstimates = tmp; 
run; 
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30. ; set tmp; data_level = "HH-CHAIN"; IV = "group"; run;
data myreg; set tmp; proc sort; by IV Dependent; run;

proc glm data = mydata(where = (cls_grp>0)) order = data; 
	absorb CHAIN; 
	model share = affinity popl affinity*popl / solution;
	title 'Regression of expenditure share = affinity at hh-chain';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-CHAIN"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc datasets noprint; delete tmp_trip prod_price tmp_rank mydata tmp tmp1; run; 

************************************************************;
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

* Append category variable to construct regression data; 
proc sql noprint;
	create table tmp1 as
	select A.*, B.DEPARTMENT, B.CATEGORY, B.PRICE
	from tmp as A left join PROD.new_prim as B
	on A.PRIM_UPC = B.UPC; 
	
	create table mydata as 
	select A.*, B.cls_grp, B.grp, B.affinity
	from tmp1 as A left join hh_new_cls as B
	on A.PANID = B.PANID
	order by CATEGORY, cls_grp descending, fail descending; 
quit; 

data HH.hh_new_cls; set hh_new_cls; run;

*-----------------------------------------------------------;
* Regression to test if harbingers had adopt new products early;
proc glm data=mydata(where=(cls_grp>0)) order = data;
	absorb CATEGORY;
	class cls_grp;
	model adoption = cls_grp /solution;
	title 'Regression of adoption lag = group at hh-UPC';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-PRIMUPC"; IV = "group"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc glm data=mydata(where=(cls_grp>0)) order = data;
	absorb CATEGORY;
	class cls_grp fail;
	model adoption = cls_grp fail fail*cls_grp/solution;
	title 'Regression of adoption lag = group at hh-UPC';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-PRIMUPC-int"; IV = "group"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc glm data=mydata(where=(cls_grp>0)) order = data;
	absorb CATEGORY;
	class cls_grp ;
	model adoption = affinity/solution;
	title 'Regression of adoption lag = affinity at hh-UPC';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-PRIMUPC"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc glm data=mydata(where=(cls_grp>0)) order = data;
	absorb CATEGORY;
	class cls_grp fail;
	model adoption = affinity fail fail*affinity/solution;
	title 'Regression of adoption lag = affinity at hh-UPC';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-PRIMUPC-int"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc datasets noprint; delete tmp_trip tmp tmp1 mydata; run;

************************************************************;
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
	select PANID, WEEK, count(distinct CHAIN) as num_chain ,sum(BSKTDOL*rank)/sum(BSKTDOL) as prim_share 
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

*-----------------------------------------------------------;
* Regression of shopping behavior at weekly level; 
data tmp; 
	set trip.trip_det; 
	Year = year(Date);
	month = cats(year, "-", put(month(DATE), z2.)); 
	myWEEK = cats(YEAR, '-',put(week(DATE),2.));
run;

proc sql noprint; 
	* Aggregate trip leve data to weekly level; 
	create table tmp2 as 
	select PANID, myWEEK as WEEK, mean(COUPON) as pct_coupon, mean(PRICEOFF) as pct_priceoff
	from tmp
	group by PANID, WEEK; 
	
	* Merge price search with store search; 
	create table tmp3 as 
	select A.*, B.pct_coupon, B.pct_priceoff
	from tmp1 as A left join tmp2 as B
	on A.PANID = B.PANID and A.WEEK = B.WEEK;
	
	* Merge data with households segments; 
	create table mydata_week as 
	select A.*, B.cls_grp, B.grp, B.affinity
	from tmp3 as A left join hh_new_cls as B
	on A.PANID = B.PANID
	order by WEEK, cls_grp descending; 
quit; 

* Regression with groups dummy; 
proc glm data = mydata_week(where=(cls_grp>0)) order = data; 
	absorb WEEK; 
	class cls_grp;
	model  pct_coupon = cls_grp /solution;
	title 'Regression of coupon percentage = group at hh-week';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-WEEK"; IV = "group"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc glm data = mydata_week(where=(cls_grp>0)) order = data; 
	absorb WEEK; 
	class cls_grp;
	model  pct_priceoff = cls_grp /solution;
	title 'Regression of price off percentage = group at hh-week';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-WEEK"; IV = "group"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc glm data = mydata_week(where=(cls_grp>0)) order = data; 
	absorb WEEK; 
	class cls_grp;
	model num_chain = cls_grp /solution;
	title 'Regression of num of visited chains = group at hh-week';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-WEEK"; IV = "group"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc glm data = mydata_week(where=(cls_grp>0)) order = data; 
	absorb WEEK; 
	class cls_grp;
	model prim_share = cls_grp /solution;
		title 'Regression of expenditure share at primary store = group at hh-week';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-WEEK"; IV = "group"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

*-------------------------------------*; 
* Regression with affinity index as IV; 
proc glm data = mydata_week(where=(cls_grp>0)) order = data; 
	absorb WEEK; 
	model  pct_coupon = affinity /solution;
	title 'Regression of coupon percentage = affinity at hh-week';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-WEEK"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc glm data = mydata_week(where=(cls_grp>0)) order = data; 
	absorb WEEK; 
	model  pct_priceoff = affinity /solution;
	title 'Regression of price off percentage = affinity at hh-week';	
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-WEEK"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc glm data = mydata_week(where=(cls_grp>0)) order = data; 
	absorb WEEK; 
	model num_chain = affinity /solution;
	title 'Regression of number of visited chains = affinity at hh-week';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-WEEK"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc glm data = mydata_week(where=(cls_grp>0)) order = data; 
	absorb WEEK; 
	model prim_share = affinity /solution;
	title 'Regression of expenditure share at primary store = affinity at hh-week';	
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-WEEK"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc datasets noprint; delete tmp tmp1 tmp2 tmp3 trip_sum; run;	

************************************************************;
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
	select A.*, B.grp, B.affinity, B.cls_grp 
	from tmp as A inner join HH.hh_new_cls as B
	on A.PANID = B.PANID
	order by CATEGORY, PANID;
quit;

*--------------------------------*; 
* Regression to test: group dummy; 
proc sort data=variety_test; by CATEGORY descending cls_grp; run;

proc glm data=variety_test(where = (cls_grp>0)) order=data; 
	absorb CATEGORY;
	class cls_grp;
	model all_num_brand=cls_grp num_purchase / solution;
	title 'Regression of number of brands = group at hh-category';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-CATEGORY"; IV = "group"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc glm data=variety_test(where = (cls_grp>0)) order=data; 
	absorb CATEGORY;
	class cls_grp;
	model HHI=cls_grp num_purchase/ solution;
	title 'Regression of HHI = group at hh-category';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-CATEGORY"; IV = "group"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc glm data=variety_test(where = (cls_grp>0)) order=data; 
	absorb CATEGORY;
	class cls_grp;
	model ENTR=cls_grp / solution;
	title 'Regression of quantitye entropy = group at hh-category';	
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-CATEGORY"; IV = "group"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc glm data=variety_test(where = (cls_grp>0)) order=data; 
	absorb CATEGORY;
	class cls_grp;
	model repeat_freq=cls_grp / solution;
	title 'Regression of tendency of purchasing the same brand as the last = group at hh-category';	
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-CATEGORY"; IV = "group"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

*--------------------------------------------*; 
* Regression to test with affinity index as IV; 
proc glm data=variety_test(where = (cls_grp>0)) order=data; 
	absorb CATEGORY;
	class cls_grp;
	model all_num_brand= affinity num_purchase / solution;
	title 'Regression of number of brands = affinity at hh-category';		
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-CATEGORY"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc glm data=variety_test(where = (cls_grp>0)) order=data; 
	absorb CATEGORY;
	class cls_grp;
	model HHI= affinity num_purchase/ solution;
	title 'Regression of HHI = affinity at hh-category';		
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-CATEGORY"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc glm data=variety_test(where = (cls_grp>0)) order=data; 
	absorb CATEGORY;
	class cls_grp;
	model ENTR= affinity / solution;
	title 'Regression of entropy = affinity at hh-category';		
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-CATEGORY"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

proc glm data=variety_test(where = (cls_grp>0)) order=data; 
	absorb CATEGORY;
	class cls_grp;
	model repeat_freq= affinity / solution;
	title 'Regression of tendency of purchasing the same brand as the last = affinity at hh-category';	
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-CATEGORY"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

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

************************************************************;
* Repeat purchases; 
%let class_window = 52;
proc sql noprint;
	create table trip_prod as
	select A.*,B.PRIM_UPC,B.fail,B.UPC_fail,B.WFM,B.class,B.test, B.PRIM_LIFE, B.PRIM_WFM, B.DEPARTMENT, B.CATEGORY, B.PRICE, 
			ifn(WEEK <= B.PRIM_WFM + &class_window, DATE, .) as within_window
	from ORIG.trip_det as A inner join PROD.new_full as B
	on A.UPC = B.UPC
	order by PANID,PRIM_UPC;
	
	* Count new product purchase;
	create table tmp1 as 
	select CATEGORY, PANID, PRIM_UPC,fail, class, mean(PRIM_LIFE) as LIFE, mean(PRICE) as PRICE, count(distinct DATE) as num_purchase, 
			count(distinct within_window) as num_purchase_window
	from trip_prod
	group by CATEGORY, PANID, PRIM_UPC, fail, class;
	
	create table mydata as 
	select A.*, B.cls_grp, B.affinity
	from tmp1 as A left join HH.hh_new_cls as B
	on A.PANID = B.PANID
	order by CATEGORY, cls_grp descending, fail descending; 
quit;

* First check if the repeat purchase behavior is consistent in classification and test products; 
proc glm data=mydata(where = (cls_grp ^= 0)) order=data;
	absorb CATEGORY;
	class cls_grp class;
	model num_purchase = cls_grp class cls_grp*class PRICE/solution;
run;

* Regression to test if harbingers had less repeat purchases;
proc glm data=mydata(where = (cls_grp ^= 0)) order = data;
	absorb CATEGORY;
	class cls_grp ;
	model num_purchase = cls_grp PRICE/solution;
	ods output ParameterEstimates = tmp; 
run;

proc glm data=mydata(where = (cls_grp ^= 0)) order = data;
	absorb CATEGORY;
	class cls_grp fail;
	model num_purchase = cls_grp fail fail*cls_grp PRICE/solution;
	title 'Regression of number of purchase incidence = group*fail at hh-UPC';		
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-PRIMUPC"; IV = "group"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

* Use number of purchases within one year after introduction as DV; 
proc glm data=mydata(where = (cls_grp ^= 0)) order = data;
	absorb CATEGORY;
	class cls_grp fail;
	model num_purchase_window = cls_grp fail fail*cls_grp PRICE/solution;
	title 'Regression of number of purchase incidence within one-year after introduction = group*fail at hh-UPC';		
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-PRIMUPC"; IV = "group"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

* Conditional on purchases (num_purchase > 0); 
proc glm data=mydata(where=(num_purchase_window > 0 and cls_grp^= 0 )) order = data;
	absorb CATEGORY;
	class cls_grp fail;
	model num_purchase_window = cls_grp fail fail*cls_grp PRICE/solution;
	title 'Regression of (positive) number purchase incidence within one-year after introduction = group*fail at hh-UPC'; 
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-PRIMUPC-pos"; IV = "group"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

* Regression with affinity index as IV; 
proc glm data=mydata(where = (cls_grp ^= 0)) order = data;
	absorb CATEGORY;
	class  fail;
	model num_purchase = affinity fail fail*affinity PRICE/solution;
	title 'Regression of number of purchase incidence = affinity * fail at hh-UPC'; 
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-PRIMUPC"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

* Use number of purchases within one year after introduction as DV; 
proc glm data=mydata(where = (cls_grp ^= 0)) order = data;
	absorb CATEGORY;
	class  fail;
	model num_purchase_window = affinity fail fail*affinity PRICE/solution;
	title 'Regression of number of purchase incidence withhin one-year after intro = affinity * fail at hh-UPC'; 
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-PRIMUPC"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

* Conditional on purchases (num_purchase > 0); 
proc glm data=mydata(where=(num_purchase_window > 0 and cls_grp^= 0 )) order = data;
	absorb CATEGORY;
	class  fail;
	model num_purchase_window = affinity fail fail*affinity PRICE/solution;
	title 'Regression of (positive) number of purchase incidence within one year = affinity*fail at hh-UPC';
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH-PRIMUPC-pos"; IV = "affinity"; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

* Average over catgory and merge to household profile;
proc means data=mydata(where = (num_purchase > 0)) noprint;
	class PANID;
	var num_purchase num_purchase_window;
	output out = tmp MEAN=;
run;

proc sql noprint;
	create table hh_new_cls as
	select A.*,B.num_purchase, B.num_purchase_window
	from HH.hh_new_cls as A left join tmp as B
	on A.PANID=B.PANID
	order by PANID;
quit;

data HH.hh_new_cls; set hh_new_cls; run;
proc datasets noprint; delete tmp tmp1 tmp2; run;

*-------------------------------------------------------------;
* T test the difference ;
proc contents data = HH.hh_new_cls; run;
data HH.hh_new_cls;
	retain PANID cls_grp grp num_fail num_allnew affinity 
		   num_trip BSKTDOL inter_purchase num_cat num_upc num_brand_prcat pct_coupon pct_priceoff
		   PCT_BDD PCT_EDIBLE PCT_FROZEN PCT_GENERALM PCT_HBC PCT_NONEDIBLE PCT_NONSCAN 
		   num_niche pct_niche niche_cat_share units_rank price_tier chain_popularity
		   adoption_all adoption_fail adoption_success
		   num_chain prim_share all_num_brand HHI ENTR repeat_freq
		   ;
	set HH.hh_new_cls;
	label 	fail_cutoff		= 'The cutoff used to define failure'
			affinity		= 'Affinity index'
			affinity2		= 'Affinity index of repeat (2)'
			affinity3		= 'Affinity index of repeat (3)'
			affinity		= 'Affinity index weighted by inverse lifetime'
			affinity2		= 'Affinity index of repeat (2) weighted by inverse lifetime'
			affinity3		= 'Affinity index of repeat (3) weighted by inverse lifetime'
			num_allnew		= 'Number of new products purchased in classification period'
			num_allnew2		= 'Number of new products with 2+ purchases'
			num_allnew3		= 'Number of new products with 3+ purchases'
			num_fail		= 'Number of failed new producuts purchased in classification period'
			num_fail2		= 'Number of flops purchased with 2+ purchases in classification period'
			num_fail3		= 'Number of flops purchased with 3+ purchases in classification period'
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
			units_rank		= 'Popularity ranks of products purchased'
			niche_cat_share	= 'Expenditure share of niche products'
			price_tier		= 'Average price tier'
			chain_popularity= 'Chain popularity weighted by expenditure share'
			adoption_all	= 'Average adoption lag of all new products'
			adoption_fail	= 'Average adoption lag of failed new products'
			adoption_success= 'Average adoption lag of successful new products'
			num_chain		= 'Number of chains visited per week'
			prim_share		= 'Expenditure share at the primary store'
			all_num_brand	= 'Median number of brands in a category'
			HHI				= 'Median brand quantity HHI in a category'
			ENTR			= 'Median entropy of brand quantity in a category'
			repeat_freq		= 'Frequency of purchasing the same brand as last time'
			num_purchase	= 'Average number of repeat purchases'
			num_purchase_window = 'Average number of repeat purchases within first year'
			;
run;
proc contents data=HH.hh_new_cls; run;

proc ttest data=HH.hh_new_cls plots=none;
	class grp;
	var num_allnew BSKTDOL inter_purchase num_upc num_cat pct_coupon pct_priceoff num_brand_prcat 
		PCT_BDD PCT_EDIBLE PCT_FROZEN PCT_GENERALM PCT_HBC PCT_NONEDIBLE PCT_NONSCAN 
		num_niche pct_niche
		adoption_all adoption_fail adoption_success num_chain prim_share
		all_num_brand HHI ENTR repeat_freq num_purchase num_purchase_window;
	ods output Ttests=tmp1 Statistics=tmp2;
run;

data pan_test;
	merge tmp1 tmp2;
	by variable; 
run;

* Regression of affinity against variety and adoption lag; 
proc reg data = hh_new_cls; 
	model affinity = adoption_all HHI/stb; 
	ods output ParameterEstimates = tmp; 
run;
data tmp; length IV $30. Dependent $30. data_level $30. Parameter $30.; set tmp; data_level = "HH"; IV = ""; run;
data myreg; merge myreg tmp; by IV Dependent; 
proc sort; by IV Dependent; run ;

* Permutation test; 
proc npar1way data=hh_new_cls edf median noprint; 
	class grp;
	var num_allnew BSKTDOL inter_purchase num_upc num_cat pct_coupon pct_priceoff num_brand_prcat 
		PCT_BDD PCT_EDIBLE PCT_FROZEN PCT_GENERALM PCT_HBC PCT_NONEDIBLE PCT_NONSCAN 
		 num_niche pct_niche
		adoption_all adoption_fail adoption_success num_chain prim_share
		all_num_brand HHI ENTR repeat_freq num_purchase num_purchase_window;
	exact scores = data/mc n = 9999; 
	output out = my_perm_test; 
run;

data my_perm_test;
	retain _VAR_ _DATA_ P2_DATA PL_DATA PR_DATA P_CHDATA _CHDATA_ _MED_ P2_MED PL_MED PR_MED _KS_ P_KSA _D_;
	set my_perm_test(keep = _VAR_ _DATA_ P2_DATA PL_DATA PR_DATA P_CHDATA _CHDATA_ _MED_ P2_MED PL_MED PR_MED _KS_ P_KSA _D_);
proc contents; 
run;

proc sql noprint; 
	create table tmp(drop = Variable) as 
	select *
	from my_perm_test as A full join (select * from pan_test where Variances="Unequal") as B
	on A._VAR_ = B.variable 
	order by _VAR_;	
quit;
proc sort data=tmp nodupkeys out = test_res; by _VAR_; run;

* Extract the label of variables; 
data tmp; 
	set HH.hh_new_cls; 
	length lab $300. _VAR_ $20.; 
	array def(*) _numeric_; 
	do i = 1 to dim(def);
		_VAR_ = vname(def[i]);
		call label(def[i], lab);
		output;
	end;
	stop;
	keep _VAR_ lab; 
proc sort; by _VAR_; run;
run;

proc sql noprint;
	create table tmp1 as 
	select A.*, B.lab
	from test_res as A left join tmp as B
	on A._VAR_ = B._VAR_
	order by _VAR_;
quit;
data test_res; set tmp1; run;

************************************;
* Revnue contribution of each group*;
************************************;
proc sql noprint;
	create table tmp as
	select PANID, CHAIN, sum(BSKTDOL) as BSKTDOL
	from ORIG.trip_sum
	group by PANID, CHAIN
	order by PANID;
	
	create table tmp1 as 
	select A.*, B.cls_grp, B.grp, B.affinity
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
		
***********************;
* Export to excel data*;
***********************;
PROC EXPORT DATA=HH.HH_new_cls
            OUTFILE= "&HH_outfile" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;	

* Export regression estimates; 
proc sort data=myreg; by data_level Dependent; run;
PROC EXPORT DATA= myreg
            OUTFILE= "&reg_outfile" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

* Export results of T-test and permuation test; 
PROC EXPORT DATA= test_res
            OUTFILE= "&test_outfile" 
            DBMS=csv REPLACE;
     PUTNAMES=YES;
RUN;

/*
proc datasets library=ORIG kill noprint; run;
proc datasets library=TRIP kill noprint; run;
*/
