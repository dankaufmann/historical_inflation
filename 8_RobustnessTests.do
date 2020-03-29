* ------------------------------------------------------------------------
* Daniel Kaufmann, "Is Deflation Costly After All? The Perils of Erroneous 
*   Historical Classifications," Journal of Applied Econometrics, forthcoming
* ------------------------------------------------------------------------
*
* Computes the robustness tests
*
* ------------------------------------------------------------------------
* Daniel Kaufmann, 2020, daniel.kaufmann@unine.ch
* ------------------------------------------------------------------------
version 15
capture log close
clear
clear matrix
program drop _all
clear mata
cls
set more off
pause off

* Adjust this path to where you stored the files
global path = "C:\Users\kaufmannd\switchdrive\Research\TrendInflation\submissions\JAE\HistoricalInflation_Replication"

* Set up paths for storing results
global respath = "$path\Results\"
global datpath = "$path\Data\"
global figpath = "$path\Results\"
global funcpath = "$path\Functions\"
sysdir set PLUS $path\Functions\ado\plus
cd $path


*-------------------------------------------------------------------------------
* Import data and do settings
*-------------------------------------------------------------------------------
import excel $datpath/DataForStataUS.xlsx, first
duplicates report date
isid date
tsset date, yearly

* Dummy variables
gen cpi_dum = 0
replace cpi_dum = 1 if cpi<0
replace cpi_dum = . if cpi == .
gen prx_dum = 0
replace prx_dum = 1 if proxy<0
replace prx_dum = . if proxy == .

gen bank_dum = bank
gen share_dum = 0
replace share_dum = 1 if share<0
replace share_dum = . if share==.
gen mon_dum = 0
replace mon_dum = 1 if m2<5
replace mon_dum = . if m2==.


gen prx2_dum = 0
replace prx2_dum = 1 if cpif<0
replace prx2_dum = . if cpif== .

* For robustness different threshold and persistence
gen cpi_dum3 = 0
replace cpi_dum3 = 1 if cpi<-3
replace cpi_dum3 = . if cpi == .
gen prx_dum3 = 0
replace prx_dum3 = 1 if proxy<-3
replace prx_dum3 = . if proxy == .

gen cpi_dum1 = 0
replace cpi_dum1 = 1 if cpi<1
replace cpi_dum1 = . if cpi == .
gen prx_dum1 = 0
replace prx_dum1 = 1 if proxy<1
replace prx_dum1 = . if proxy == .

gen cpi_dump = 0
replace cpi_dump = 1 if cpi_dum[_n-1]==1 & cpi_dum[_n] == 1
replace cpi_dump = 1 if cpi_dum[_n]==1 & cpi_dum[_n+1] == 1
gen prx_dump = 0
replace prx_dump = 1 if prx_dum[_n-1]==1 & prx_dum[_n] == 1
replace prx_dump = 1 if prx_dum[_n]==1 & prx_dum[_n+1] == 1


/*Settings*/
	gen yt 			= iprod
	gen xt			= cpi_dum
	gen zt 			= prx_dum
	gen qt 			= .
	gen gt 			= prx2_dum
	gen dSample 	= 0
	replace dSample = 1 if date <= 1899 & date >= 1800
	local maxIter 	= 500
	local maxTry  	= 3
	local fixE00 	= 0.15
	local fixN11 	= 0.15
/*Settings end*/

*-------------------------------------------------------------------------------
* Main Table IPROD, with controls (Bounds and GMM)
*-------------------------------------------------------------------------------
* Country and depVar label for saving coefficients
* Note: Set qt = 0 (scalar) for estimation without covariates
local cty = ""
eststo clear
disp("Main results United States")
foreach coVar in "bank_dum" "share_dum" {
	replace yt = iprod
	replace qt = `coVar'

* OLS CPI
	*EstimateBinaryOLS yt xt qt 0 dSample "CPI" "`coVar'" "`cty'"

	* Black et al. CPI and proxy
	*EstimateBinaryBlack yt xt zt qt 0 dSample "CPI, proxy" "`coVar'" "`cty'"
			
	* GMM conditional independence
	EstimateBinaryIndep yt xt zt qt dSample `maxIter' `maxTry' "CPI, proxy" "`coVar'" "`cty'"
							
	* IV CPI and proxy
	*EstimateBinaryIV yt xt zt qt 0 dSample "CPI, proxy" "`coVar'" "`cty'"

	* GMM fixed misclassification
	EstimateBinaryFixed yt xt zt qt dSample `fixE00' `fixN11' `maxIter' `maxTry' "CPI, proxy" "`coVar'" "`cty'"
}


* Do estimates of bounds with all covariates
* ------------ Black et al, unconstrained --------------
* Generate deflation indicators
gen m00 = (1-xt)*(1-zt)
gen m10 = xt*(1-zt)
gen m01 = (1-xt)*zt
gen m11 = xt*zt

ivreg2 yt xt share_dum bank_dum mon_dum if dSample == 1, robust bw(2)
eststo mod1usall: nlcom (a: _b[_cons]) ///
					  (b: _b[xt]) ///
					  (ab: _b[_cons]+_b[xt]) ///
					  (d2: _b[share_dum]) (d1: _b[bank_dum]) (d3: _b[mon_dum]), post

estadd scalar Converge = 1
estadd local Bound "Upper"
estadd local Method "OLS"
estadd local Indicator "CPI, proxy"
estadd local DepVar ""

ivreg2 yt m11 share_dum bank_dum mon_dum m10 m01 if dSample == 1, robust bw(2)
eststo mod2usall: nlcom (a: _b[_cons]) ///
					  (b: _b[m11]) ///
					  (ab: _b[_cons]+_b[m11]) ///
					  (d2: _b[share_dum]) (d1: _b[bank_dum])(d3: _b[mon_dum]), post
estadd scalar Converge = 1
estadd local Bound "Upper"
estadd local Method "OLS"
estadd local Indicator "CPI, proxy"
estadd local DepVar ""

ivreg2 yt m11 share_dum bank_dum  m10 m01 if dSample == 1, robust bw(2)
eststo mod20usall: nlcom (a: _b[_cons]) ///
					  (b: _b[m11]) ///
					  (ab: _b[_cons]+_b[m11]) ///
					  (d2: _b[share_dum]) (d1: _b[bank_dum]), post
estadd scalar Converge = 1
estadd local Bound "Upper"
estadd local Method "OLS"
estadd local Indicator "CPI, proxy"
estadd local DepVar ""

drop m0* m1*


esttab mod2* mod3* mod5* , keep(a* b* d*) drop(bias* ab*) noomitted ///
		title("United States, 1800-1899, IPROD, with controls") ///
		stats(N Bound Method Indicator sOverident pOverident pDiff nTries Converge maxIter , fmt(0 0 0 0 2 3 2 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "Model parameters" biasXa "Bias estimates", nolabel) ///
		rename(abank_dum a ashare_dum a amon_dum a bbank_dum b bshare_dum b bmon_dum b share_dum d2 bank_dum d1 dbank_dum d1 dshare_dum d2) ///
		coeflabels(pOverident "\emph{p}-value \emph{J}-test" a "Growth inflation"  b "Shortfall deflation" d3 "M2 slowdown" d1 "Banking crisis" d2 "Stock price decline"  dshare_dum "Stock price decline" dmon_dum "Money growth decline") 		 

esttab mod2* mod3* mod5*  using $respath/Tab_D1.txt, replace keep(a* b* d*) drop(bias* ab*) mlabels(none) collabels(none) nomtitles nolegend nonumbers label noomitted ///
		stats(N Bound Method Indicator sOverident pOverident pDiff nTries Converge maxIter , fmt(0 0 0 0 2 3 2 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "Model parameters" biasXa "Bias estimates", nolabel) ///
		rename(abank_dum a ashare_dum a amon_dum a bbank_dum b bshare_dum b bmon_dum b share_dum d2 bank_dum d1 dbank_dum d1 dshare_dum d2) ///
		coeflabels(pOverident "\emph{p}-value \emph{J}-test" a "Growth inflation" b "Shortfall deflation" d3 "M2 slowdown" d1 "Banking crisis" d2 "Stock price decline" dbank_dum "Banking crisis" dshare_dum "Stock price decline" dmon_dum "Money growth decline") 
		
*-------------------------------------------------------------------------------
* Robustness Table 
*-------------------------------------------------------------------------------
* Country and depVar label for saving coefficients
* Note: Set qt = 0 (scalar) for estimation without covariates
local cty = ""
local depName = ""
eststo clear

* Severe deflations
replace yt = iprod
replace xt = cpi_dum3
replace zt = prx_dum3 

* OLS CPI
EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depName'" "`cty'"

* Black et al. CPI and proxy
EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"
		
* GMM conditional independence
EstimateBinaryIndep yt xt zt 0 dSample `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"
						
* IV CPI and proxy
EstimateBinaryIV yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"

* GMM fixed misclassification
EstimateBinaryFixed yt xt zt 0 dSample `fixE00' `fixN11' `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"

esttab mod1* mod2* mod3* mod4* mod5*,  keep(a b)  noomitted ///
		title("United States, 1800-1899, IPROD, no controls") ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "Model parameters" biasXa "Bias estimates", nolabel) ///
		coeflabels(a "\$\alpha = E[y|\pi>0]$" b "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" ab "\$\alpha+\beta = E[y|\pi<0]$"  pd "\$P[\pi<0]$" ///
		biasXa "\$plim\ \hat\alpha-\alpha$" biasXb "\$plim\ \hat\beta-\beta$" biasXab "\$plim\ \hat\alpha+\hat\beta-\alpha-\beta$") 		 

esttab mod1* mod2* mod3* mod4* mod5* using $respath/Tab_D3A.txt, replace keep(a b)  mlabels(none) collabels(none) nomtitles nolegend nonumbers label noomitted ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "\textbf{Model parameters:}" biasXa "\textbf{Bias estimates:}", nolabel) ///
		coeflabels(a "\$\alpha = E[y|\pi>0]$" b "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" ab "\$\alpha+\beta = E[y|\pi<0]$"  pd "\$P[\pi<0]$" ///
		biasXa "\$plim\ \hat\alpha-\alpha$" biasXb "\$plim\ \hat\beta-\beta$" biasXab "\$plim\ \hat\alpha+\hat\beta-\alpha-\beta$") 		 

* Persistent deflation
replace yt = iprod
replace xt = cpi_dump
replace zt = prx_dump 

* OLS CPI
EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depName'" "`cty'"

* Black et al. CPI and proxy
EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"
		
* GMM conditional independence
EstimateBinaryIndep yt xt zt 0 dSample `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"
						
* IV CPI and proxy
EstimateBinaryIV yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"

* GMM fixed misclassification
EstimateBinaryFixed yt xt zt 0 dSample `fixE00' `fixN11' `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"

esttab mod1* mod2* mod3* mod4* mod5*,  keep(a b)  noomitted ///
		title("United States, 1800-1899, IPROD, no controls") ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "Model parameters" biasXa "Bias estimates", nolabel) ///
		coeflabels(a "\$\alpha = E[y|\pi>0]$" b "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" ab "\$\alpha+\beta = E[y|\pi<0]$"  pd "\$P[\pi<0]$" ///
		biasXa "\$plim\ \hat\alpha-\alpha$" biasXb "\$plim\ \hat\beta-\beta$" biasXab "\$plim\ \hat\alpha+\hat\beta-\alpha-\beta$") 		 

esttab mod1* mod2* mod3* mod4* mod5* using $respath/Tab_D3B.txt, replace keep(a b)  mlabels(none) collabels(none) nomtitles nolegend nonumbers label noomitted ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "\textbf{Model parameters:}" biasXa "\textbf{Bias estimates:}", nolabel) ///
		coeflabels(a "\$\alpha = E[y|\pi>0]$" b "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" ab "\$\alpha+\beta = E[y|\pi<0]$"  pd "\$P[\pi<0]$" ///
		biasXa "\$plim\ \hat\alpha-\alpha$" biasXb "\$plim\ \hat\beta-\beta$" biasXab "\$plim\ \hat\alpha+\hat\beta-\alpha-\beta$") 		 

replace xt 			= cpi_dum
replace zt 			= prx_dum

*-------------------------------------------------------------------------------
* Robustness Table Three indicators
*-------------------------------------------------------------------------------
** Preliminary check whether cells have enough observations
gen dSample2 = (gt<.)
eststo clear
foreach depVar in "iprod" {

	* ----------- OLS, CPI--------------
	ivreg2 `depVar' xt if dSample2 == 1, robust bw(2)
	scalar hasConverged = 1
	eststo: nlcom  (a: _b[_cons]) (b: _b[xt]) , post
	estadd scalar Converge = hasConverged
	estadd local Bound "Upper"
	estadd local Method "OLS"
	estadd local Indicator "CPI"
	
	* ----------- OLS, Proxy--------------
	ivreg2 `depVar' zt if dSample2 == 1, robust bw(2)
	scalar hasConverged = 1
	eststo: nlcom  (a: _b[_cons]) (b: _b[zt]) , post
	estadd scalar Converge = hasConverged
	estadd local Bound "Upper"
	estadd local Method "OLS"
	estadd local Indicator "Proxy"
	
	* ----------- OLS, Falkner --------------
	ivreg2 `depVar' gt if dSample2 == 1, robust bw(2)
	scalar hasConverged = 1
	eststo: nlcom  (a: _b[_cons]) (b: _b[gt]) , post
	estadd scalar Converge = hasConverged
	estadd local Bound "Upper"
	estadd local Method "OLS"
	estadd local Indicator "Falkner"
	
	* ------------ Black et al, unconstrained --------------
	* Generate deflation indicators
	gen m00 = (1-xt)*(1-zt)
	gen m10 = xt*(1-zt)
	gen m01 = (1-xt)*zt
	gen m11 = xt*zt
	
	ivreg2 `depVar' m11 m01 m10 if dSample2 == 1, robust bw(2)
	eststo: nlcom  (a: _b[_cons]) (b: _b[m11]), post
	estadd scalar Converge = 1
	estadd local Bound "Upper"	
	estadd local Method "OLS"
	estadd local Assumption "Independence"
	estadd local Indicator "CPI, Proxy"
	
	* ------------ Black et al, unconstrained --------------
	* Generate deflation indicators
	drop m0* m1*
	gen m00 = (1-xt)*(1-gt)
	gen m10 = xt*(1-gt)
	gen m01 = (1-xt)*gt
	gen m11 = xt*gt	
	
	ivreg2 `depVar' m11 m01 m10 if dSample2 == 1, robust bw(2)
	eststo: nlcom  (a: _b[_cons]) (b: _b[m11]), post
	estadd scalar Converge = 1
	estadd local Bound "Upper"
	estadd local Method "OLS"
	estadd local Assumption "Independence"
	estadd local Indicator "CPI, Falkner"
	
	* ------------ Black et al, unconstrained --------------
	* Generate deflation indicators
	drop m0* m1*
	gen m00 = (1-zt)*(1-gt)
	gen m10 = zt*(1-gt)
	gen m01 = (1-zt)*gt
	gen m11 = zt*gt	
	
	ivreg2 `depVar' m11 m01 m10 if dSample2 == 1, robust bw(2)
	eststo: nlcom  (a: _b[_cons]) (b: _b[m11]), post
	estadd scalar Converge = 1
	estadd local Bound "Upper"
	estadd local Method "OLS"
	estadd local Assumption "Independence"
	estadd local Indicator "Proxy, Falkner"

	
	* ------------ Black et al, unconstrained --------------
	* Generate deflation indicators
	drop m*
	gen m000 = (1-xt)*(1-zt)*(1-gt)
	gen m100 = xt*(1-zt)*(1-gt)
	gen m010 = (1-xt)*zt*(1-gt)
	gen m110 = xt*zt*(1-gt)
	gen m001 = (1-xt)*(1-zt)*gt
	gen m101 = xt*(1-zt)*gt
	gen m011 = (1-xt)*zt*gt
	gen m111 = xt*zt*gt
	ivreg2 `depVar' m111 m100 m010 m110 m001 m101 m011 if dSample2 == 1, robust bw(2)
	eststo: nlcom  (a: _b[_cons]) (b: _b[m111]) , post
	estadd scalar Converge = 1
	estadd local Bound "Upper"
	estadd local Method "OLS"
	estadd local Assumption "Independence"
	estadd local Indicator "All"
	
	disp "`depVar'"

	local aInit = _b[a]
	local bInit = _b[b]

	esttab, noomitted  title("Test")  ///
	stats(r2 N Indicator Bound Meth , fmt(2 0 0 0 0 2 2 2)) ///
	se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01)

	esttab using $respath/Tab_D2.txt, replace nolegend nonumbers label noomitted ///
	stats(r2 N Indicator Bound Meth ManyTries Converge pDiff pOverident  , fmt(2 0 0 0 0 2 2 2)) ///
	se(2)  b(2) star(* 0.10 ** 0.05 *** 0.01) ///
	coeflabels(a "Growth inflation" b "Shortfall deflation") 		 
}

		
*-------------------------------------------------------------------------------
* Robustness Table GDP per capita, no controls
*-------------------------------------------------------------------------------
* Country and depVar label for saving coefficients
* Note: Set qt = 0 (scalar) for estimation without covariates
local cty = ""
local depName = ""
eststo clear
disp("Main results United States")
foreach depVar in "rgdp" {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depName'" "`cty'"

	* Black et al. CPI and proxy
	EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"
			
	* GMM conditional independence
	EstimateBinaryIndep yt xt zt 0 dSample `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"
							
	* IV CPI and proxy
	EstimateBinaryIV yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"

	* GMM fixed misclassification
	EstimateBinaryFixed yt xt zt 0 dSample `fixE00' `fixN11' `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"
}


esttab mod1* mod2* mod3* mod4* mod5*, keep(a b ab pd biasX*) noomitted ///
		title("United States, 1800-1899, IPROD, no controls") ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "Model parameters" biasXa "Bias estimates", nolabel) ///
		coeflabels(a "\$\alpha = E[y|\pi>0]$" b "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" ab "\$\alpha+\beta = E[y|\pi<0]$"  pd "\$P[\pi<0]$" ///
		biasXa "\$plim\ \hat\alpha-\alpha$" biasXb "\$plim\ \hat\beta-\beta$" biasXab "\$plim\ \hat\alpha+\hat\beta-\alpha-\beta$") 		 

esttab mod1* mod2* mod3* mod4* mod5* using $respath/Tab_C3.txt, replace keep(a b ab pd biasX*)  mlabels(none) collabels(none) nomtitles nolegend nonumbers label noomitted ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "\textbf{Model parameters:}" biasXa "\textbf{Bias estimates:}", nolabel) ///
		coeflabels(a "\$\alpha = E[y|\pi>0]$" b "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" ab "\$\alpha+\beta = E[y|\pi<0]$"  pd "\$P[\pi<0]$" ///
		biasXa "\$plim\ \hat\alpha-\alpha$" biasXb "\$plim\ \hat\beta-\beta$" biasXab "\$plim\ \hat\alpha+\hat\beta-\alpha-\beta$") 		 

		

*-------------------------------------------------------------------------------
* 1870-1899 (Gold Standard) IPROD, no controls
*-------------------------------------------------------------------------------
eststo clear
replace dSample = 0 if date < 1870
eststo clear
disp("1870-1899 United States")
foreach depVar in "iprod" {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depName'" "`cty'"

	* Black et al. CPI and proxy
	EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"
			
	* GMM conditional independence
	EstimateBinaryIndep yt xt zt 0 dSample `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"
							
	* IV CPI and proxy
	EstimateBinaryIV yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"

	* GMM fixed misclassification
	EstimateBinaryFixed yt xt zt 0 dSample `fixE00' `fixN11' `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"
}

* Set back to original value		
replace dSample = 1 if date < 1870 & date >= 1800

esttab mod1* mod2* mod3* mod4* mod5*, keep(a b ab pd biasX*) noomitted        ///
		title("United States, 1870-1899, GDP, no controls") ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "Model parameters" biasXa "Bias estimates", nolabel) ///
		coeflabels(a "\$\alpha = E[y|\pi>0]$" b "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" ab "\$\alpha+\beta = E[y|\pi<0]$"  pd "\$P[\pi<0]$" ///
		biasXa "\$plim\ \hat\alpha-\alpha$" biasXb "\$plim\ \hat\beta-\beta$" biasXab "\$plim\ \hat\alpha+\hat\beta-\alpha-\beta$") 		 

esttab mod1* mod2* mod3* mod4* mod5* using $respath/Tab_C2.txt,  replace keep(a b ab pd biasX*)  mlabels(none) collabels(none) nomtitles nolegend nonumbers label noomitted ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "\textbf{Model parameters:}" biasXa "\textbf{Bias estimates:}", nolabel) ///
		coeflabels(a "\$\alpha = E[y|\pi>0]$" b "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" ab "\$\alpha+\beta = E[y|\pi<0]$"  pd "\$P[\pi<0]$" ///
		biasXa "\$plim\ \hat\alpha-\alpha$" biasXb "\$plim\ \hat\beta-\beta$" biasXab "\$plim\ \hat\alpha+\hat\beta-\alpha-\beta$") 		 

*-------------------------------------------------------------------------------
* 1800-1869 (Before Gold Standard) IPROD, no controls
*-------------------------------------------------------------------------------
eststo clear
replace dSample = 0 if date > 1870
eststo clear
disp("1800-1869 United States")
foreach depVar in "iprod" {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depName'" "`cty'"

	* Black et al. CPI and proxy
	EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"
			
	* GMM conditional independence
	EstimateBinaryIndep yt xt zt 0 dSample `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"
							
	* IV CPI and proxy
	EstimateBinaryIV yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"

	* GMM fixed misclassification
	EstimateBinaryFixed yt xt zt 0 dSample `fixE00' `fixN11' `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"
}

* Set back to original value		
replace dSample = 1 if date > 1870 & date <= 1899

esttab mod1* mod2* mod3* mod4* mod5*, keep(a b ab pd biasX*) noomitted ///
		title("United States, 1800-1869, GDP, no controls") ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "Model parameters" biasXa "Bias estimates", nolabel) ///
		coeflabels(a "\$\alpha = E[y|\pi>0]$" b "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" ab "\$\alpha+\beta = E[y|\pi<0]$"  pd "\$P[\pi<0]$" ///
		biasXa "\$plim\ \hat\alpha-\alpha$" biasXb "\$plim\ \hat\beta-\beta$" biasXab "\$plim\ \hat\alpha+\hat\beta-\alpha-\beta$") 		 

esttab mod1* mod2* mod3* mod4* mod5* using $respath/Tab_C1.txt,  replace keep(a b ab pd biasX*)  mlabels(none) collabels(none) nomtitles nolegend nonumbers label noomitted ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "\textbf{Model parameters:}" biasXa "\textbf{Bias estimates:}", nolabel) ///
		coeflabels(a "\$\alpha = E[y|\pi>0]$" b "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" ab "\$\alpha+\beta = E[y|\pi<0]$"  pd "\$P[\pi<0]$" ///
		biasXa "\$plim\ \hat\alpha-\alpha$" biasXb "\$plim\ \hat\beta-\beta$" biasXab "\$plim\ \hat\alpha+\hat\beta-\alpha-\beta$") 		 
	 
	 
*-------------------------------------------------------------------------------
* Main Table IPROD, with lagged dependent
*-------------------------------------------------------------------------------
* Country and depVar label for saving coefficients
* Note: Set qt = 0 (scalar) for estimation without covariates
local cty = ""
eststo clear
disp("Main results United States")
foreach depVar in "iprod"  {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 1 dSample "CPI" "`depName'" "`cty'"

	* Black et al. CPI and proxy
	EstimateBinaryBlack yt xt zt 0 1 dSample "CPI, proxy" "`depName'" "`cty'"
						
	* IV CPI and proxy
	EstimateBinaryIV yt xt zt 0 1 dSample "CPI, proxy" "`depName'" "`cty'"

}

esttab mod1* mod2* mod4* , keep(b* l*) noomitted ///
		title("United States, 1800-1899, IPROD, no controls") ///
		stats(N Bound Method Indicator sOverident pOverident pDiff nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "Model parameters" biasXa "Bias estimates", nolabel) ///
		rename(bbank_dum b bshare_dum b bmon_dum b) ///
		coeflabels(pOverident "\emph{p}-value \emph{J}-test" b "Shortfall deflation" dbank_dum "Banking crisis" dshare_dum "Stock price decline" dmon_dum "Money growth decline") 		 

esttab mod1* mod2* mod4* using $respath/Tab_D4.txt, replace keep(b* l*) mlabels(none) collabels(none) nomtitles nolegend nonumbers label noomitted ///
		stats(N Bound Method Indicator sOverident pOverident pDiff nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "Model parameters" biasXa "Bias estimates", nolabel) ///
		rename(bbank_dum b bshare_dum b bmon_dum b) ///
		coeflabels(pOverident "\emph{p}-value \emph{J}-test" l "Lagged dep. variable" b "Shortfall deflation" dbank_dum "Banking crisis" dshare_dum "Stock price decline" dmon_dum "Money growth decline") 	
		
