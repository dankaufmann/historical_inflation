* ------------------------------------------------------------------------
* Daniel Kaufmann, "Is Deflation Costly After All? The Perils of Erroneous 
*   Historical Classifications," Journal of Applied Econometrics, forthcoming
* ------------------------------------------------------------------------
*
* Computes the results with simulated data
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
* 1) Do US results
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

/*Settings*/
	gen yt 			= 0
	gen xt 			= cpi_dum
	gen zt 			= prx_dum
	gen qt 			= .
	gen dSample 	= 0
	replace dSample = 1 if date <= 1899 & date >= 1800
	local maxIter 	= 500
	local maxTry  	= 8
	local fixE00 	= .15
	local fixN11	= .15
/*Settings end*/


*-------------------------------------------------------------------------------
* Main Table error-ridden GDP, no controls
*-------------------------------------------------------------------------------
eststo clear
disp("Main results United States")
foreach depVar in "rgdp" "rgdpcpi" "rgdpprox" {
	replace yt = `depVar'

	* OLS CPI
	if("`depVar'"=="rgdpprox"){
		EstimateBinaryOLS yt zt 0 0 dSample "CPI" "`depVar'" "`cty'"
	}
	else{
		EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depVar'" "`cty'"
	}

	* Black et al. CPI and proxy
	EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depVar'" "`cty'"
			
	* GMM conditional independence
	EstimateBinaryIndep2 yt xt zt 0 dSample `maxIter' `maxTry' "CPI, proxy" "`depVar'" "`cty'"
							
	* IV CPI and proxy
	EstimateBinaryIV yt xt zt 0 0 dSample "CPI, proxy" "`depVar'" "`cty'"

	* GMM fixed misclassification
	EstimateBinaryFixed yt xt zt 0 dSample `fixE00' `fixN11' `maxIter' `maxTry' "CPI, proxy" "`depVar'" "`cty'"
}
 
*-------------------------------------------------------------------------------
* 2) Do simulation results Import data and do settings
*-------------------------------------------------------------------------------
clear
set obs 1000
set seed 98034
gen date = _n
tsset date, yearly

* Generate CPI and nominal GDP with properties in modern data (see RomerizeBLS.R)
* The only difference is, I set the mean inflation rate to 0
gen cpi 	 = 2.8*rnormal()
gen cpi_dum  = 0
replace cpi_dum = 1 if cpi<0

scalar beta  = -1
scalar alpha = 3
gen gdp 	 = alpha+beta*cpi_dum + 2*rnormal()
gen ngdp 	 = gdp+cpi

sum gdp
sum ngdp

* Error-ridden CPI (worst case, probably relevant for Switzerland but not US)
* Error-ridden GDP based on the wrong deflator. Using an error-ridden CPI
* Three cases. We get two indicators that are independent. We also get three measures
* of GDP growth to assess, whether independent measurement error matters
gen errx = -0.3+3*rnormal()
gen errz = -0.3+3*rnormal()
gen errg = -0.3+3*rnormal()

gen cpix = cpi+errx
gen cpiz = cpi+errz
gen cpig = cpi+errg

gen gdpx = ngdp-cpix
gen gdpz = ngdp-cpiz
gen gdpg = gdp+errg

gen cpix_dum  = 0
replace cpix_dum = 1 if cpix<0
gen cpiz_dum  = 0
replace cpiz_dum = 1 if cpiz<0
gen cpig_dum  = 0
replace cpig_dum = 1 if cpig<0

gen E00 = ((1-cpix_dum)*(1-cpiz_dum) == 1)
sum E00 if cpi_dum == 1
gen N11 = (cpix_dum*cpiz_dum == 1)
sum N11 if cpi_dum == 0

/*Settings*/
	gen xt 			= cpix_dum
	gen zt 			= cpiz_dum
	gen dSample 	= 1
	local maxIter 	= 200
	local maxTry  	= 5
	local fixE00 	= .1
	local fixN11	= .1
/*Settings end*/

*-------------------------------------------------------------------------------
* Simulate results with well-measured GDP
*-------------------------------------------------------------------------------
* Country and depVar label for saving coefficients
local cty = ""
local depName = "well"
gen yt = 0
foreach depVar in "gdp" {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depName'" "`cty'"

	* Black et al. CPI and proxy
	EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"
			
	* GMM conditional independence
	EstimateBinaryIndep2 yt xt zt 0 dSample `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"
							
	* IV CPI and proxy
	EstimateBinaryIV yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"

	* GMM fixed misclassification
	EstimateBinaryFixed yt xt zt 0 dSample `fixE00' `fixN11' `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"
}


*-------------------------------------------------------------------------------
* Simulate results with error-ridden GDP (but independent measurement error)
*-------------------------------------------------------------------------------
local depName = "err1"
* Country and depVar label for saving coefficients
foreach depVar in "gdpg" {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depName'" "`cty'"

	* Black et al. CPI and proxy
	EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"
			
	* GMM conditional independence
	EstimateBinaryIndep2 yt xt zt 0 dSample `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"
							
	* IV CPI and proxy
	EstimateBinaryIV yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"

	* GMM fixed misclassification
	EstimateBinaryFixed yt xt zt 0 dSample `fixE00' `fixN11' `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"
}


*-------------------------------------------------------------------------------
* Simulate results with error-ridden GDP (but correlated measurement error)
*-------------------------------------------------------------------------------
* Country and depVar label for saving coefficients
local depName = "err2"
foreach depVar in "gdpx" {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depName'" "`cty'"

	* Black et al. CPI and proxy
	EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"
			
	* GMM conditional independence
	EstimateBinaryIndep2 yt xt zt 0 dSample `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"
							
	* IV CPI and proxy
	EstimateBinaryIV yt xt zt 0 0 dSample "CPI, proxy" "`depName'" "`cty'"

	* GMM fixed misclassification
	EstimateBinaryFixed yt xt zt 0 dSample `fixE00' `fixN11' `maxIter' `maxTry' "CPI, proxy" "`depName'" "`cty'"
}

* Collect results and construct table
eststo all1: appendmodels mod1rgdpcpi mod1rgdpprox mod1well mod1err1 mod1err2
eststo all2: appendmodels mod2rgdpcpi mod2rgdpprox mod2well mod2err1 mod2err2
eststo all3: appendmodels mod3rgdpcpi mod3rgdpprox mod3well mod3err1 mod3err2
eststo all4: appendmodels mod4rgdpcpi mod4rgdpprox mod4well mod4err1 mod4err2
eststo all5: appendmodels mod5rgdpcpi mod5rgdpprox mod5well mod5err1 mod5err2

* Export Table on coefficients
esttab all*, keep(a* b*) drop(bias* ab*) noomitted ///
		title("Dependent variable bias, Alpha, Beta, no controls") ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(argdp "U.S. real GDP:" argdpcpi "U.S. NGDP/CPI:"  argdpprox "U.S. NGDP/proxy:"  ///
		awell "Simulation:" aerr1 "Simulation real GDP/i.i.d. error:" aerr2 "Simulation NGDP/proxy:", nolabel)	
		
esttab all* using $respath/Tab_4.txt, replace keep(a* b*) drop(bias* ab*) mlabels(none) collabels(none) nomtitles nolegend nonumbers label noomitted ///
		stats(N Bound Method Indicator sOverident pOverident nTries Converge maxIter , fmt(0 0 0 0 2 3 0 0 0)) ///
		se(2) r2(2) b(2) star(* 0.10 ** 0.05 *** 0.01) ///
		refcat(argdp "\textbf{U.S. real GDP:}" argdpcpi "\textbf{U.S. NGDP/CPI:}"  argdpprox "\textbf{U.S. NGDP/proxy:}"  ///
		awell "\textbf{Simulation:}" aerr1 "\textbf{Simulation GDP+i.i.d. error:}" aerr2 "\textbf{Simulation NGDP/proxy:}", nolabel)	///
		coeflabels(argdp "\$\alpha = E[y|\pi>0]$" argdpcpi "\$\alpha = E[y|\pi>0]$" argdpprox "\$\alpha = E[y|\pi>0]$"  awell "\$\alpha = E[y|\pi>0]$" aerr1 "\$\alpha = E[y|\pi>0]$" aerr2 "\$\alpha = E[y|\pi>0]$" ///
				   brgdp "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" brgdpcpi "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" brgdpprox "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" bwell "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" berr1 "\$\beta = E[y|\pi<0]-E[y|\pi>0]$" berr2 "\$\beta = E[y|\pi<0]-E[y|\pi>0]$") 			 
		
		
