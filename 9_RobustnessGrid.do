* ------------------------------------------------------------------------
* Daniel Kaufmann, "Is Deflation Costly After All? The Perils of Erroneous 
*   Historical Classifications," Journal of Applied Econometrics, forthcoming
* ------------------------------------------------------------------------
*
* Computes the robustness tests Figure D.1
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
	gen yt			= iprod
	gen xt 			= cpi_dum
	gen zt 			= prx_dum
	gen qt 			= .
	gen dSample 	= 0
	replace dSample = 1 if date <= 1899 & date >= 1800
	local maxIter 	= 1000
	local maxTry  	= 5
/*Settings end*/


*-------------------------------------------------------------------------------
* Main Table IPROD, no controls
*-------------------------------------------------------------------------------
* Country and depVar label for saving coefficients
* Note: Set qt = 0 (scalar) for estimation without covariates
local cty = ""
local depName = ""
eststo clear
disp("Main results United States")
* OLS CPI
EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depName'" "`cty'"
local olsBeta = _b[b]
local olsAlpha = _b[a]
	
local i = 1
forvalues fixE00 = 0.0(0.05)0.15 {
			forvalues fixN11 = 0.0(0.05)0.15 {
				
			* GMM fixed misclassification
			EstimateBinaryFixed yt xt zt 0 dSample `fixE00' `fixN11' `maxIter' `maxTry' "CPI, proxy" "" "g`i'"
			local i = `i'+1
	}
}

coefplot (mod1*, lcolor(navy) mlcolor(navy) keep(b*) drop(bias*) ciopts(recast(rcap))) (mod5*, ///
		keep(b*) drop(bias*) ciopts(recast(rcap))  ///
		xline(0, lpattern(solid) lcolor(black)) legend(off) ///
		xline(`olsBeta', lcolor(navy) lpattern(dash)) ///
		mfcolor(white)  mlcolor(cranberry)), ///
		grid(between glcolor(white) glpattern(dot)) nolabels ///
		coeflabels(b = " " bg* =" ") ///
		xtitle("{&beta} (in pp)") ///
		graphregion(color(white)) bgcolor(white)  plotregion(lcolor(black)) 
		graph export $figpath/Fig_D1A.pdf, as(pdf) replace  fontface("Helvetica")
		*fontface("Times New Roman")

coefplot (mod5*, ///
		keep(biasXb*) ciopts(recast(rcap)) ///
		xline(0, lpattern(solid) lcolor(black)) legend(off) ///
		mfcolor(white) mlcolor(navy) color(navy) lcolor(navy)), ///
		grid(between glcolor(white) glpattern(dot)) nolabels ///
		coeflabels(bias* =" ") ///
		xtitle("Bias {&beta} (in pp)") ///
		graphregion(color(white)) bgcolor(white)  plotregion(lcolor(black)) 
		graph export $figpath/Fig_D1B.pdf, as(pdf) replace fontface("Helvetica")
		*fontface("Times New Roman")

		 
