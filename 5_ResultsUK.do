* ------------------------------------------------------------------------
* Daniel Kaufmann, "Is Deflation Costly After All? The Perils of Erroneous 
*   Historical Classifications," Journal of Applied Econometrics, forthcoming
* ------------------------------------------------------------------------
*
* Computes the results with UK data.
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
* UK Results
*-------------------------------------------------------------------------------
import excel $datpath/DataForStataUK.xlsx, first clear
duplicates report date
isid date
tsset date, yearly

* Dummy variables
gen cpi_dum = 0
replace cpi_dum = 1 if cpi<0
replace cpi_dum = . if cpi==.
gen prx_dum = 0
replace prx_dum = 1 if proxy<0
replace prx_dum = . if prx==.
gen wpi_dum = 0
replace wpi_dum = 1 if wpi<0
replace wpi_dum = . if wpi==.
gen def_dum = 0
replace def_dum = 1 if cdefl<0
replace def_dum = . if cdefl==.
gen ppi_dum = 0
replace ppi_dum = 1 if ppi<0
replace ppi_dum = . if ppi==.

/*Settings*/
	gen xt 			= cpi_dum
	gen zt 			= wpi_dum
	gen dSample 	= 0
	replace dSample = 1 if date <= 1899 & date >= 1830
	local fixE00 	= 0.15
	local fixN11 	= 0.15
	local maxTry	= 5
	local maxIter	= 200
/*Settings end*/

* Country label for saving coefficients
local cty = "uk"
  
gen yt = 0 
disp("Main results United Kingdom")
foreach depVar in "gdp" "cons" "inv" "prod" "urate"  {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depVar'" "`cty'"

	* Black et al. CPI and proxy
	EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depVar'" "`cty'"
			
	* GMM conditional independence
	EstimateBinaryIndep yt xt zt 0 dSample `maxIter' `maxTry' "CPI, proxy" "`depVar'" "`cty'"
							
	* IV CPI and proxy
	EstimateBinaryIV yt xt zt 0 0 dSample "CPI, proxy" "`depVar'" "`cty'" "`saveB'"

	* GMM fixed misclassification
	EstimateBinaryFixed yt xt zt 0 dSample `fixE00' `fixN11' `maxIter' `maxTry' "CPI, proxy" "`depVar'" "`cty'"
}


* Do table for UK, Norway, unemployment rate and consumption growth (because we have good data)
eststo all1: appendmodels mod1produk mod1urateuk mod1consuk mod1invuk
eststo all2: appendmodels mod2produk mod2urateuk mod2consuk mod2invuk
eststo all3: appendmodels mod3produk mod3urateuk mod3consuk mod3invuk
eststo all4: appendmodels mod4produk mod4urateuk mod4consuk mod4invuk
eststo all5: appendmodels mod5produk mod5urateuk mod5consuk mod5invuk

* Export Table on beta
esttab all*, keep(b*) drop(bias*) noomitted ///
		title("UK Results") ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(burateuk "United Kingdom (1830-1899):" bgdpno "Norway (1840-1899):", nolabel)	
		
esttab all* using $respath/Tab_3.txt, replace keep(b*) drop(bias*) mlabels(none) collabels(none) nomtitles nolegend nonumbers label noomitted ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(bproduk "\textbf{United Kingdom (1830-1899):}" bconsno "\textbf{Norway (1840-1899):}" ///
		bgdpse "\textbf{Sweden (1800-1899):}" bgdpfi "\textbf{Finland (1860-1899):}" bgdp2ch "\textbf{Switzerland (1851-1899):}" bgdpau "\textbf{Austria (1863-1899):}" ///
		bgdppt "\textbf{Portugal (1865-1899):}" bgdp1es "\textbf{Spain (1851-1899):}" brgdpag "\textbf{Argentina (1810-1899):}", nolabel)	///
		coeflabels(brgdpus "GDP growth" bexpuk "Export growth"  bimpuk "Import growth"  bconsuk "Consumption growth"  binvuk "Investment growth"  bgdpuk "GDP growth" bconsuk "Consumption growth" ///
		bgdpno "GDP growth" bproduk "Industrial production growth"  binvno "Investment growth"  bconsno "Consumption growth" bgdpau "GDP growth"  ///
		bgdpse "GDP growth"  bgdpfi "GDP growth" bgdp2ch "GDP growth" bgdppt "GDP growth" bgdp1es "GDP growth"  burateuk "Unemployment rate") 		 
