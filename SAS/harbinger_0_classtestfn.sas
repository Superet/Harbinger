%macro ClassFn(HH_outname,trip_data,prod_data,class_window,num_group,repeat=,class_window_low = 0);
	* Merge trip and product data;
	proc sql noprint;
		create table trip_prod as
		select A.*,B.PRIM_UPC,B.fail,B.UPC_fail,B.WFM,B.class,B.test, B.PRIM_LIFE, B.PRIM_WFM
		from &trip_data as A inner join &prod_data as B
		on A.UPC = B.UPC
		order by PANID,PRIM_UPC;
	quit; 
	
	* Subset classification ;	
	%if &class_window_low = 0 %then %do; 
		proc sql noprint; 
			create table tmp as 
			select * from trip_prod where (class=1) and (WEEK <= PRIM_WFM + &class_window); 
		quit; 
	%end; 
	%else %do; 
		proc sql noprint; 
			create table tmp as 
			select * from trip_prod where (class=1) and (WEEK <= PRIM_WFM + &class_window) and (WEEK >= PRIM_WFM + &class_window_low); 
		quit;	
	%end; 

	proc sql noprint;
		* Count new product purchase;
		create table tmp1 as 
		select PANID, PRIM_UPC,fail, mean(PRIM_LIFE) as LIFE, count(distinct DATE) as num_purchase
		from tmp 
		group by PANID, PRIM_UPC, fail;
		
		create table &HH_outname as
		select PANID, count(distinct PRIM_UPC) as num_allnew, sum(fail) as num_fail, 
					  sum(1*(num_purchase>=2)) as num_allnew2, sum(fail*(num_purchase>=2)) as num_fail2,
					  sum(1*(num_purchase>=3)) as num_allnew3, sum(fail*(num_purchase>=3)) as num_fail3, 
					  sum(1/LIFE) as inv_life, sum(fail/LIFE) as fail_life, 
					  sum(1*(num_purchase>=2)/LIFE) as inv_life2, sum(fail*(num_purchase>=2)/LIFE) as fail_life2,
					  sum(1*(num_purchase>=3)/LIFE) as inv_life3, sum(fail*(num_purchase>=3)/LIFE) as fail_life3
		from tmp1
		group by PANID;
	quit;
	
	data &HH_outname;
		set &HH_outname;
		affinity  = .;
		affinity2 = .;
		affinity3 = .; 		
		if num_allnew >=2 then affinity 	= num_fail/num_allnew;
		if num_allnew2 >0 then affinity2	= num_fail2/num_allnew2;
		if num_allnew3 >0 then affinity3	= num_fail3/num_allnew3;
		if num_allnew >=2 then affinityl	= fail_life/inv_life; 
		if num_allnew2 >=0 then affinityl2	= fail_life2/inv_life2; 
		if num_allnew3 >=0 then affinityl3	= fail_life3/inv_life3; 
	run;
	
	* Restrict to the households with at least two new product purchases;
	%let pct_step = %eval(100/&num_group);
	proc univariate data=&HH_outname noprint; 
		var affinity&repeat; 
		output out=tmp1 pctlpts = &pct_step to 100 by &pct_step pctlpre = aff_P;
	run;
	data tmp1; set tmp1; aff_P100 = 1; run; 
	proc transpose data=tmp1 out=tmp2; run;
	proc sql noprint; select COL1 into:group_thresh separated by ' ' from tmp2; quit;
	%put &group_thresh.;
	
	* Segment households;
	%let fvar = affinity&repeat;
	data &HH_outname;
		set &HH_outname;
		cls_grp = 0;
		if &fvar>=0 & &fvar<=scan("&group_thresh",1,' ') then cls_grp = 1;		
		do i = 2 to &num_group;
			if &fvar>scan("&group_thresh",i-1,' ') & &fvar<=scan("&group_thresh",i,' ') then 
			cls_grp = i;
		end;
		drop i;
	run;
	proc datasets noprint; delete tmp tmp1 tmp2; run;
%mend ClassFn;

%macro TestFn(outname, HH_outname,trip_prod,test_prod_data,test_window,num_group,test_window_low = 0);
	* Merge the households classification to purchase data; 
	%if &test_window_low = 0 %then %do; 
		proc sql noprint;
			create table tmp as 
			select A.*,B.cls_grp
			from &trip_prod as A left join &HH_outname as B
			on A.PANID = B.PANID
			where (test= 1) and (WEEK <= PRIM_WFM + &test_window);
		quit;
	%end; 
	%else %do; 
		proc sql noprint;
			create table tmp as 
			select A.*,B.cls_grp
			from &trip_prod as A left join &HH_outname as B
			on A.PANID = B.PANID
			where (test= 1) and (WEEK <= PRIM_WFM + &test_window) and (WEEK>= PRIM_WFM + &test_window_low);
		quit;
	%end; 
		
	data tmp;
		set tmp;
		if cls_grp = . then cls_grp = 0;
	run;
	
	* Compute sales contribution by group;
	proc sql noprint;
		create table tmp1 as
		select PRIM_UPC,cls_grp,sum(DOL) as sales
		from tmp
		group by PRIM_UPC, cls_grp;
	quit;
	
	* Reshape the long data to wide data; 
	proc transpose data=tmp1 out=tmp2 prefix=grp;
		by PRIM_UPC; id cls_grp;
	run;
	data tmp2;
		set tmp2;
		total_sale = 0;
		array grp{*} grp0-grp&num_group;
		do i = 1 to dim(grp);
			if grp[i] = . then grp[i] = 0;
			total_sale = total_sale + grp[i];
		end;
		drop i _NAME_;
	run;
	
	* Combine the product data and the group sales data; 
	proc sql noprint;
		create table &outname(drop=old_UPC) as
		select *
		from &test_prod_data as A inner join tmp2(rename=(PRIM_UPC=old_UPC)) as B
		on A.UPC=B.old_UPC
		where test=1;
	quit;	
	proc datasets noprint; delete tmp tmp1 tmp2; run;
%mend TestFn;
