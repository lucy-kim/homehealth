* areg2gen.ado ---- written by Mitchell Petersen -- March 2006 --- modified by Dan Taylor --- March 2009
* Program calculates two-way clustered standard errors for OLS
*  as described by Thompson in "A Simple Formula for Standard Errors that Cluster by Both Firm and Time" and
*      	and Cameron, Gelbach, and Miller, 2006, "Robust Inference with Multi-way Clustering"

#delimit ;
program define areg2gen, eclass sortpreserve byable(recall);               
	syntax [varlist] [in] [if], absorb(varname) fcluster(varname) tcluster(varname);
	tokenize `varlist';
	local depv `"`1'"';
* ---------------------------------------------------------------- ;
* ----- Regression Clustering by First Variable (e.g. Firm) ------ ;
* ---------------------------------------------------------------- ;
	quietly areg `varlist' `in' `if', absorb(`absorb') robust cluster(`fcluster');
	matrix vcf = e(V);
	local nfcluster=e(N_clust);
* ---------------------------------------------------------------- ;
* ----- Regression Clustering by Second Variable (e.g. Time) ----- ;
* ---------------------------------------------------------------- ;
	quietly areg `varlist' `in' `if', absorb(`absorb') robust cluster(`tcluster');
	matrix vct = e(V);
	local ntcluster=e(N_clust);
* ---------------------------------------------------------------- ;
* ---------------  Regression with No Clustering  ---------------- ;
* ---------------------------------------------------------------- ;

	   	capture confirm string variable `fcluster';
			if !_rc {;
				gen bc1 = `fcluster'; /* string variable */
		                };
		   	   else {;
		   		gen bc1 = string(`fcluster'); /* numeric */
		   		};
		capture confirm string variable `tcluster';
			if !_rc {;
				gen bc2 = `tcluster'; /* string variable */
		                };
		   	   else {;
		   		gen bc2 = string(`tcluster'); /* numeric */
		   		};
	   	gen bc3 = bc1 + "_" + bc2;
	   	quietly areg `varlist' `in' `if', absorb(`absorb') robust cluster(bc3);
		drop bc1 bc2 bc3;
	local nparm = e(df_m)+1;
	matrix coef = e(b);
	matrix vcb = e(V);
	matrix vc = vcf+vct-vcb;
	


* ---------------------------------------------------------------- ;
* ----------------- Print out Regression Results ----------------- ;
* ---------------------------------------------------------------- ;
	tokenize `varlist';  		/* this puts varlist in to the macros `1' `2' etc */
	macro shift;			/* drops first arguement (dep var) and shifts the rest up one */
	
	dis " ";
	dis "Absorption regression with two-way clustered SEs"
		_column (52) "Number of obs = " %7.0f e(N);
	dis _column(52) "F(" %3.0f e(df_m) "," %6.0f e(df_r) ") =" %8.2f e(F);
	dis _column(52) "Prob > F      ="  %8.4f 1-F(e(df_m),e(df_r),e(F));
	dis "Number of clusters (`fcluster') = " _column(31) %5.0f $_nfcluster
        	_column(52) "R-squared     =" %8.4f e(r2);
        dis "Number of clusters (`tcluster') = " _column(31) %5.0f $_ntcluster
		_column(52) "Root MSE      =" %8.4f e(rmse);

	dis " ";
	dis _column(10) "    Beta" _column(24) "  Std Error" 
	    _column(38) "   T-stat" _column(52) "  p-value";
	dis _column(10) " -----------" _column(24) " ------------" 
	    _column(38)	" -----------" _column(52) " -----------";
	
	local i = 1;
	* loop as long as macro `1' is not empty;
	while "`1'" ~= "" {;
		dis "`1'" _column(10) %9.5f coef[1,`i']
			  _column(24) %9.5f sqrt(vc[`i',`i'])
			  _column(38) %9.4f coef[1,`i']/sqrt(vc[`i',`i'])
			  _column(52) %9.4f 2*ttail(e(N)-1,abs(coef[1,`i']/sqrt(vc[`i',`i'])));
		macro shift;
		local i = `i' + 1;
		};
		dis "Constant"
			  _column(10) %9.5f coef[1,`nparm']
			  _column(24) %9.5f sqrt(vc[`nparm',`nparm'])
			  _column(38) %9.4f coef[1,`nparm']/sqrt(vc[`nparm',`nparm'])
			  _column(52) %9.4f 2*ttail(e(N)-1,abs(coef[1,`nparm']/sqrt(vc[`nparm',`nparm'])));
	dis " ";
	dis "     SE clustered by " "`fcluster'" " and " "`tcluster'";
	dis " ";

* ---------------------------------------------------------------- ;
* ----------------- upload Regression Results into e()------------ ;
* ---------------------------------------------------------------- ;

* save statistics from the last regression (clustered by fcluster+tcluster);
* scalars;
	scalar e_N=e(N);
	scalar e_df_m = e(df_m);
	scalar e_df_r = e(df_r);
	scalar e_F = e(F);
	scalar e_r2 = e(r2);
	scalar e_rmse = e(rmse);
	scalar e_mss = e(mss);
	scalar e_rss = e(rss);
	scalar e_r2_a = e(r2_a);
	scalar e_ll = e(ll);
	scalar e_ll_0 = e(ll_0);

* prepare matrices to upload into e();
	ereturn clear;
	tempname b V;
	matrix `b' = coef;
	matrix `V' = vc;

* post the resuls in e();
	ereturn post `b' `V';
	ereturn scalar N = e_N;
	ereturn scalar df_m = e_df_m;
	ereturn scalar df_r = e_df_r;
 	ereturn scalar F= e_F;
	ereturn scalar r2= e_r2;
	ereturn scalar rmse = e_rmse;
	ereturn scalar mss = e_mss;
	ereturn scalar rss = e_rss;
	ereturn scalar r2_a = e_r2_a;
	ereturn scalar ll = e_ll;
	ereturn scalar ll_0 = e_ll_0;
	ereturn local title "Absorption regression with two-way clustered SEs";
	ereturn local method "2-dimension clustered SEs";
	ereturn local depvar "`depv'";
	ereturn local cmd "cluster2";
* end of uploading;


end;