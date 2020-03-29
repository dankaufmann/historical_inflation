* ------------------------------------------------------------------------
* Daniel Kaufmann, "Is Deflation Costly After All? The Perils of Erroneous 
*   Historical Classifications," Journal of Applied Econometrics, forthcoming
* ------------------------------------------------------------------------
*
* Computes the results with US data.
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
replace mon_dum = 1 if m2<3.5
replace mon_dum = . if m2==.

/*Settings*/
	gen xt 			= cpi_dum
	gen zt 			= prx_dum
	gen qt 			= .
	gen dSample 	= 0
	replace dSample = 1 if date <= 1899 & date >= 1800
	local maxIter 	= 1000
	local maxTry  	= 4
	local fixE00 	= 0.15
	local fixN11 	= 0.15
/*Settings end*/


*-------------------------------------------------------------------------------
* Main Table IPROD, no controls
*-------------------------------------------------------------------------------
* Country and depVar label for saving coefficients
* Note: Set qt = 0 (scalar) for estimation without covariates
local cty = ""
local depName = ""
gen yt = 0
eststo clear
disp("Main results United States")
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


esttab mod1* mod2* mod3* mod4* mod5*, keep(a b ab pd biasX*) noomitted ///
		title("United States, 1800-1899, IPROD, no controls") ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "Model parameters" biasXa "Bias estimates", nolabel) ///
		coeflabels(a "\$\alpha = E[y|\pi>0]$" b "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" ab "\$\alpha+\beta = E[y|\pi<0]$"  pd "\$P[\pi<0]$" ///
		biasXa "\$plim\ \hat\alpha-\alpha$" biasXb "\$plim\ \hat\beta-\beta$" biasXab "\$plim\ \hat\alpha+\hat\beta-\alpha-\beta$") 		 

esttab mod1* mod2* mod3* mod4* mod5* using $respath/Tab_2.txt, replace keep(a b ab pd biasX*)  mlabels(none) collabels(none) nomtitles nolegend nonumbers label noomitted ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(a "\textbf{Model parameters:}" biasXa "\textbf{Bias estimates:}", nolabel) ///
		coeflabels(a "\$\alpha = E[y|\pi>0]$" b "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" ab "\$\alpha+\beta = E[y|\pi<0]$"  pd "\$P[\pi<0]$" ///
		biasXa "\$plim\ \hat\alpha-\alpha$" biasXb "\$plim\ \hat\beta-\beta$" biasXab "\$plim\ \hat\alpha+\hat\beta-\alpha-\beta$") 		 
