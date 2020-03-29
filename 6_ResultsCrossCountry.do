* ------------------------------------------------------------------------
* Daniel Kaufmann, "Is Deflation Costly After All? The Perils of Erroneous 
*   Historical Classifications," Journal of Applied Econometrics, forthcoming
* ------------------------------------------------------------------------
*
* Computes the results with for cross-country data
*
* ------------------------------------------------------------------------
* Daniel Kaufmann, 2020, daniel.kaufmann@unine.ch
* ------------------------------------------------------------------------
version 12
capture log close
clear
clear matrix
program drop _all
clear mata
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
* US Results
*-------------------------------------------------------------------------------
import excel $datpath/DataForStataUS.xlsx, first
duplicates report date
isid date
tsset date, yearly

* Matrix saving the volatility of CPI inflation
mat Vol = J(6,11,.) 
matrix rownames Vol = "CPI" "RGDP" "BASE" "BLACK" "BASE2" "BLACK2"
matrix colnames Vol = "us" "uk" "no" "fi" "ch" "es" "pt" "se" "au" "hu" "cl"
gen ctyNames = ""

local EndSample = 1899

* Dummy variables
gen cpi_dum = 0
replace cpi_dum = 1 if cpi<0
replace cpi_dum = . if cpi==.
gen prx_dum = 0
replace prx_dum = 1 if proxy<0
replace prx_dum = . if prx==.

/*Settings*/
	gen xt 			= cpi_dum
	gen zt 			= prx_dum
	gen dSample 	= 0
	replace dSample = 1 if date <= `EndSample' & date >= 1800
	local maxIter 	= 200

/*Settings end*/

* Country label for saving coefficients
local cty = "us"

gen yt = 0
* "gcons" "urate"  "cons" 
disp("Main results United States")
foreach depVar in "rgdp"  {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depVar'" "`cty'"

	* Black et al. CPI and proxy
	EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depVar'" "`cty'"
			
}

* Save baseline, coefficient, and volatility
sum cpi if dSample
mat Vol[1, colnumb(Vol,"`cty'")] = r(sd)
sum rgdp if dSample
mat Vol[2, colnumb(Vol,"`cty'")] = r(sd)
estimates restore mod1rgdp`cty'
mat Vol[3, colnumb(Vol,"`cty'")] = _b[brgdp`cty']
mat Vol[5, colnumb(Vol,"`cty'")] = _b[argdp`cty']+_b[brgdp`cty']
estimates restore mod2rgdp`cty'
mat Vol[4, colnumb(Vol,"`cty'")] = _b[brgdp`cty']
mat Vol[6, colnumb(Vol,"`cty'")] = _b[argdp`cty']+_b[brgdp`cty']

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
	replace dSample = 1 if date <= `EndSample' & date >= 1830
/*Settings end*/

* Country label for saving coefficients
local cty = "uk"
  
gen yt = 0 
disp("Main results United Kingdom")
foreach depVar in "gdp"  {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depVar'" "`cty'"

	* Black et al. CPI and proxy
	EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depVar'" "`cty'"
			
}

* Save baseline, coefficient, and volatility
sum cpi if dSample
mat Vol[1, colnumb(Vol,"`cty'")] = r(sd)
sum gdp if dSample
mat Vol[2, colnumb(Vol,"`cty'")] = r(sd)
estimates restore mod1gdp`cty'
mat Vol[3, colnumb(Vol,"`cty'")] = _b[bgdp`cty']
mat Vol[5, colnumb(Vol,"`cty'")] = _b[agdp`cty']+_b[bgdp`cty']
estimates restore mod2gdp`cty'
mat Vol[4, colnumb(Vol,"`cty'")] = _b[bgdp`cty']
mat Vol[6, colnumb(Vol,"`cty'")] = _b[agdp`cty']+_b[bgdp`cty']

*-------------------------------------------------------------------------------
* Norway results
*-------------------------------------------------------------------------------
import excel $datpath/DataForStataNorway.xlsx, first clear
duplicates report date
isid date
tsset date, yearly

* Dummy variables
gen cpi_dum = 0
replace cpi_dum = 1 if cpi<0
replace cpi_dum = . if cpi==.
gen wpi_dum = 0
replace wpi_dum = 1 if wpi<0
replace wpi_dum = . if wpi==.
gen prx_dum = 0
replace prx_dum = 1 if proxy<0
replace prx_dum = . if prx==.
gen ppi_dum = 0
replace ppi_dum = 1 if ppi<0
replace ppi_dum = . if ppi==.

/*Settings*/
	gen xt 			= cpi_dum
	gen zt 			= prx_dum
	gen dSample 	= 0
	replace dSample = 1 if date <= `EndSample' & date >= 1831

	/*Settings end*/

* Country label for saving coefficients
local cty = "no"
  
disp("Main results Norway")
gen yt = 0
* inv gpdpc  "gdp"
foreach depVar in "gdp"  {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depVar'" "`cty'"

	* Black et al. CPI and proxy
	EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depVar'" "`cty'"
}


* Save baseline, coefficient, and volatility
sum cpi if dSample
mat Vol[1, colnumb(Vol,"`cty'")] = r(sd)
sum gdp if dSample
mat Vol[2, colnumb(Vol,"`cty'")] = r(sd)
estimates restore mod1gdp`cty'
mat Vol[3, colnumb(Vol,"`cty'")] = _b[bgdp`cty']
mat Vol[5, colnumb(Vol,"`cty'")] = _b[agdp`cty']+_b[bgdp`cty']
estimates restore mod2gdp`cty'
mat Vol[4, colnumb(Vol,"`cty'")] = _b[bgdp`cty']
mat Vol[6, colnumb(Vol,"`cty'")] = _b[agdp`cty']+_b[bgdp`cty']

*-------------------------------------------------------------------------------
* Sweden results
*-------------------------------------------------------------------------------
import excel $datpath/DataForStataSweden.xlsx, first clear
duplicates report date
isid date
tsset date, yearly

* Dummy variables
gen cpi_dum = 0
replace cpi_dum = 1 if cpi<0
replace cpi_dum = . if cpi==.

/*Settings*/
	gen xt 			= cpi_dum
	gen dSample 	= 0
	replace dSample = 1 if date <= `EndSample' & date >= 1800
	
/*Settings end*/

* Country label for saving coefficients
local cty = "se"
  
disp("Main results Sweden")
gen yt = 0
foreach depVar in "gdp" {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depVar'" "`cty'"

}

* Save baseline, coefficient, and volatility
sum cpi if dSample
mat Vol[1, colnumb(Vol,"`cty'")] = r(sd)
sum gdp if dSample
mat Vol[2, colnumb(Vol,"`cty'")] = r(sd)
estimates restore mod1gdp`cty'
mat Vol[3, colnumb(Vol,"`cty'")] = _b[bgdp`cty']
mat Vol[5, colnumb(Vol,"`cty'")] = _b[agdp`cty']+_b[bgdp`cty']


*-------------------------------------------------------------------------------
* Finland results
*-------------------------------------------------------------------------------
import excel $datpath/DataForStataFinland.xlsx, first clear
duplicates report date
isid date
tsset date, yearly

* Dummy variables
gen cpi_dum = 0
replace cpi_dum = 1 if coli<0
replace cpi_dum = . if coli==.
gen wpi_dum = 0
replace wpi_dum = 1 if wpi<0
replace wpi_dum = . if wpi==.
gen defl_dum = 0
replace defl_dum = 1 if gdefl<0
replace defl_dum = . if gdefl==.

/*Settings*/
	gen xt 			= cpi_dum
	gen zt 			= wpi_dum
	gen dSample 	= 0
	replace dSample = 1 if date <= `EndSample' & date >= 1860
	
/*Settings end*/

* Country label for saving coefficients
local cty = "fi"
  
disp("Main results Finland")
gen yt = 0
foreach depVar in "gdp"  {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depVar'" "`cty'"

	* Black et al. CPI and proxy
	EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depVar'" "`cty'"

}

* Save baseline, coefficient, and volatility
sum coli if dSample
mat Vol[1, colnumb(Vol,"`cty'")] = r(sd)
sum gdp if dSample
mat Vol[2, colnumb(Vol,"`cty'")] = r(sd)
estimates restore mod1gdp`cty'
mat Vol[3, colnumb(Vol,"`cty'")] = _b[bgdp`cty']
mat Vol[5, colnumb(Vol,"`cty'")] = _b[agdp`cty']+_b[bgdp`cty']
estimates restore mod2gdp`cty'
mat Vol[4, colnumb(Vol,"`cty'")] = _b[bgdp`cty']
mat Vol[6, colnumb(Vol,"`cty'")] = _b[agdp`cty']+_b[bgdp`cty']

*-------------------------------------------------------------------------------
* Switzerland results
*-------------------------------------------------------------------------------
import excel $datpath/DataForStataSwitzerland.xlsx, first clear
duplicates report date
isid date
tsset date, yearly

* Dummy variables
gen cpi_dum = 0
replace cpi_dum = 1 if cpi<0
replace cpi_dum = . if cpi==.
gen prx_dum = 0
replace prx_dum = 1 if proxy<0
replace prx_dum = . if proxy==.

/*Settings*/
	gen xt 			= cpi_dum
	gen zt 			= prx_dum
	gen dSample 	= 0
	replace dSample = 1 if date <= `EndSample' & date >= 1851
	
/*Settings end*/

* Country label for saving coefficients
local cty = "ch"
  
disp("Main results Switzerland")
gen yt = 0
foreach depVar in "gdp2"  {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depVar'" "`cty'"

	* Black et al. CPI and proxy
	EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depVar'" "`cty'"
			
}

* Save baseline, coefficient, and volatility
sum cpi if dSample
mat Vol[1, colnumb(Vol,"`cty'")] = r(sd)
sum gdp2 if dSample
mat Vol[2, colnumb(Vol,"`cty'")] = r(sd)
estimates restore mod1gdp2`cty'
mat Vol[3, colnumb(Vol,"`cty'")] = _b[bgdp2`cty']
mat Vol[5, colnumb(Vol,"`cty'")] = _b[agdp2`cty']+_b[bgdp2`cty']
estimates restore mod2gdp2`cty'
mat Vol[4, colnumb(Vol,"`cty'")] = _b[bgdp2`cty']
mat Vol[6, colnumb(Vol,"`cty'")] = _b[agdp2`cty']+_b[bgdp2`cty']

*-------------------------------------------------------------------------------
* Austria results
*-------------------------------------------------------------------------------
import excel $datpath/DataForStataAustria.xlsx, first clear
duplicates report date
isid date
tsset date, yearly

* Dummy variables
gen cpi_dum = 0
replace cpi_dum = 1 if cpi<0
replace cpi_dum = . if cpi==.

/*Settings*/
	gen xt 			= cpi_dum
	gen dSample 	= 0
	replace dSample = 1 if date <= `EndSample' & date >= 1851
	
/*Settings end*/

* Country label for saving coefficients
local cty = "au"
 
disp("Main results Austria")
gen yt = 0
foreach depVar in "gdp"  {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depVar'" "`cty'"

}

* Save baseline, coefficient, and volatility
sum cpi if dSample
mat Vol[1, colnumb(Vol,"`cty'")] = r(sd)
sum gdp if dSample
mat Vol[2, colnumb(Vol,"`cty'")] = r(sd)
estimates restore mod1gdp`cty'
mat Vol[3, colnumb(Vol,"`cty'")] = _b[bgdp`cty']
mat Vol[5, colnumb(Vol,"`cty'")] = _b[agdp`cty']+_b[bgdp`cty']

*-------------------------------------------------------------------------------
* Portugal results
*-------------------------------------------------------------------------------
import excel $datpath/DataForStataPortugal.xlsx, first clear
duplicates report date
isid date
tsset date, yearly

* Dummy variables
gen gdefl_dum = 0
replace gdefl_dum = 1 if gdefl<0
replace gdefl_dum = . if gdefl==.
gen cpi_dum = 0
replace cpi_dum = 1 if cpi<0
replace cpi_dum = . if cpi==.

/*Settings*/
	gen xt 			= cpi_dum
	gen zt 			= gdefl_dum
	gen dSample 	= 0
	replace dSample = 1 if date <= `EndSample' & date >= 1865
	
/*Settings end*/

* Country label for saving coefficients
local cty = "pt"
  
disp("Main results Portugal")
gen yt = 0
foreach depVar in "gdp" {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depVar'" "`cty'"

}

* Save baseline, coefficient, and volatility
sum cpi if dSample
mat Vol[1, colnumb(Vol,"`cty'")] = r(sd)
sum gdp if dSample
mat Vol[2, colnumb(Vol,"`cty'")] = r(sd)
estimates restore mod1gdp`cty'
mat Vol[3, colnumb(Vol,"`cty'")] = _b[bgdp`cty']
mat Vol[5, colnumb(Vol,"`cty'")] = _b[agdp`cty']+_b[bgdp`cty']

*-------------------------------------------------------------------------------
* Spain results
*-------------------------------------------------------------------------------
import excel $datpath/DataForStataSpain.xlsx, first clear
duplicates report date
isid date
tsset date, yearly

* Dummy variables
gen cpi1_dum = 0
replace cpi1_dum = 1 if cpi1<0
replace cpi1_dum = . if cpi1==.
gen cpi2_dum = 0
replace cpi2_dum = 1 if cpi2<0
replace cpi2_dum = . if cpi2==.
gen cpi3_dum = 0
replace cpi3_dum = 1 if cpi3<0
replace cpi3_dum = . if cpi3==.
gen cpi4_dum = 0
replace cpi4_dum = 1 if cpi4<0
replace cpi4_dum = . if cpi4==.

/*Settings*/
	gen xt 			= cpi1_dum
	gen zt 			= cpi2_dum
	gen dSample 	= 0
	replace dSample = 1 if date <= `EndSample' & date >= 1850
	
/*Settings end*/

* Country label for saving coefficients
local cty = "es"
  
disp("Main results Spain")
gen yt = 0
foreach depVar in "gdp" {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depVar'" "`cty'"

	* Black et al. CPI and proxy
	EstimateBinaryBlack yt xt zt 0 0 dSample "CPI, proxy" "`depVar'" "`cty'"
			
}

* Save baseline, coefficient, and volatility
sum cpi1 if dSample
mat Vol[1, colnumb(Vol,"`cty'")] = r(sd)
sum gdp if dSample
mat Vol[2, colnumb(Vol,"`cty'")] = r(sd)
estimates restore mod1gdp`cty'
mat Vol[3, colnumb(Vol,"`cty'")] = _b[bgdp`cty']
mat Vol[5, colnumb(Vol,"`cty'")] = _b[agdp`cty']+_b[bgdp`cty']
estimates restore mod2gdp`cty'
mat Vol[4, colnumb(Vol,"`cty'")] = _b[bgdp`cty']
mat Vol[6, colnumb(Vol,"`cty'")] = _b[agdp`cty']+_b[bgdp`cty']

*-------------------------------------------------------------------------------
* Hungary results
*-------------------------------------------------------------------------------
import excel $datpath/DataForStataHungary.xlsx, first clear
duplicates report date
isid date
tsset date, yearly

* Dummy variables
gen gdefl_dum = 0
replace gdefl_dum = 1 if gdefl<0
replace gdefl_dum = . if gdefl==.

/*Settings*/
	gen xt 			= gdefl_dum
	gen dSample 	= 0
	replace dSample = 1 if date <= `EndSample' & date >= 1851
	
/*Settings end*/

* Country label for saving coefficients
local cty = "hu"
 
disp("Main results Hungary")
gen yt = 0
foreach depVar in "gdp" {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depVar'" "`cty'"

}

* Save baseline, coefficient, and volatility
sum gdefl if dSample
mat Vol[1, colnumb(Vol,"`cty'")] = r(sd)
sum gdp if dSample
mat Vol[2, colnumb(Vol,"`cty'")] = r(sd)
estimates restore mod1gdp`cty'
mat Vol[3, colnumb(Vol,"`cty'")] = _b[bgdp`cty']
mat Vol[5, colnumb(Vol,"`cty'")] = _b[agdp`cty']+_b[bgdp`cty']


*-------------------------------------------------------------------------------
* Chile results
*-------------------------------------------------------------------------------
import excel $datpath/DataForStataChile.xlsx, first clear
duplicates report date
isid date
tsset date, yearly

* Dummy variables
gen cpi_dum = 0
replace cpi_dum = 1 if cpi<0
replace cpi_dum = . if cpi==.

/*Settings*/
	gen xt 			= cpi_dum
	gen dSample 	= 0
	replace dSample = 1 if date <= `EndSample' & date >= 1830
	
/*Settings end*/

* Country label for saving coefficients
local cty = "cl"
 
disp("Main results Chile")
gen yt = 0
foreach depVar in "gdp"  {
	replace yt = `depVar'

	* OLS CPI
	EstimateBinaryOLS yt xt 0 0 dSample "CPI" "`depVar'" "`cty'"

}

* Save baseline, coefficient, and volatility
sum cpi if dSample
mat Vol[1, colnumb(Vol,"`cty'")] = r(sd)
sum gdp if dSample
mat Vol[2, colnumb(Vol,"`cty'")] = r(sd)
estimates restore mod1gdp`cty'
mat Vol[3, colnumb(Vol,"`cty'")] = _b[bgdp`cty']
mat Vol[5, colnumb(Vol,"`cty'")] = _b[agdp`cty']+_b[bgdp`cty']

*-------------------------------------------------------------------------------
* Do graphs and output tables
*-------------------------------------------------------------------------------
clear
mat Vol = Vol'

* Standardize to U.K. se and square
forval i = 1/11 {
	mat Vol[`i', 1] = (Vol[`i', 1]/Vol[2, 1])^2
	mat Vol[`i', 2] = (Vol[`i', 2]/Vol[2, 2])^2
}

* Standardize to volatility of U.K. inflation
svmat Vol
rename Vol1 volcpi
rename Vol2 volrgd
rename Vol3 base
rename Vol4 black
rename Vol5 base2
rename Vol6 black2

*matrix colnames Vol = "us" "uk" "no" "fi" "ch" "es" "pt" "se" "au" "hu"
gen ctyNames = ""
replace ctyNames = "us" if _n == 1
replace ctyNames = "uk" if _n == 2
replace ctyNames = "no" if _n == 3
replace ctyNames = "fi" if _n == 4
replace ctyNames = "ch" if _n == 5
replace ctyNames = "es" if _n == 6
replace ctyNames = "pt" if _n == 7
replace ctyNames = "se" if _n == 8
replace ctyNames = "au" if _n == 9
replace ctyNames = "hu" if _n == 10
replace ctyNames = "cl" if _n == 11

replace ctyNames = upper(ctyNames)
grstyle init
grstyle set plain
grstyle background white
grstyle plotregion white
grstyle plotregion_line black

eststo GDP1: appendmodels mod1rgdpus mod1gdpuk mod1gdpno mod1gdpfi mod1gdp2ch mod1gdpes mod1gdppt mod1gdpse mod1gdpau mod1gdphu mod1gdpcl
coefplot GDP1, lev(95) ciopts(recast(rcap)) xline(0, lpattern(solid) lcolor(black)) ///
		 keep(a*) drop(ab*) bylabel("{&alpha}") rename(argdpus=US agdpuk=UK agdpno=NO agdpfi=FI agdp2ch=CH agdpes=ES agdppt=PT agdpse=SE agdpau=AU agdphu=HU  agdpcl=CL) ///
				leg(off) plotregion(lcolor(black)) graphregion(color(white))  || ///
         GDP1, ciopts(recast(rcap)) xline(0, lpattern(solid) lcolor(black))  ///
		  keep(b*)  bylabel("{&beta}")  rename(brgdpus=US bgdpuk=UK bgdpno=NO bgdpfi=FI bgdp2ch=CH bgdpes=ES bgdppt=PT bgdpse=SE bgdpau=AU bgdphu=HU bgdpcl=CL) ///
		 graphregion(color(white)) leg(off) grid(none)
graph export $figpath/Fig_3a.pdf, replace fontface("Helvetica") 

eststo GDP2: appendmodels mod2rgdpus mod2gdpuk mod2gdpno mod2gdpfi mod2gdp2ch mod2gdpes   
coefplot GDP2, lev(95)  ciopts(recast(rcap)) xline(0, lpattern(solid) lcolor(black))  ///
		 keep(a*) drop(ab*) bylabel("{&alpha}") rename(argdpus=US agdpuk=UK agdpno=NO agdpfi=FI agdp2ch=CH agdpes=ES agdppt=PT agdpse=SE agdpau=AU agdphu=HU agdpcl=CL) ///
				leg(off) plotregion(lcolor(black)) graphregion(color(white))  || ///
         GDP2,lev(95)  ciopts(recast(rcap)) xline(0, lpattern(solid) lcolor(black))  ///
		 keep(b*)  bylabel("{&beta}") rename(brgdpus=US bgdpuk=UK bgdpno=NO bgdpfi=FI bgdp2ch=CH bgdpes=ES bgdppt=PT bgdpse=SE bgdpau=AU bgdphu=HU bgdpcl=CL)  ///
				grid(none) 
graph export $figpath/Fig_C2a.pdf, replace fontface("Helvetica")

eststo GDP1: appendmodels mod1rgdpus mod1gdpuk mod1gdpno mod1gdpfi mod1gdp2ch mod1gdpes mod1gdppt mod1gdpau mod1gdphu mod1gdpcl
coefplot GDP1, lev(95) ciopts(recast(rcap)) xline(0, lpattern(solid) lcolor(black)) ///
		 keep(a*) drop(ab*) bylabel("{&alpha}") rename(argdpus=US agdpuk=UK agdpno=NO agdpfi=FI agdp2ch=CH agdpes=ES agdppt=PT agdpse=SE agdpau=AU agdphu=HU  agdpcl=CL) ///
				leg(off) plotregion(lcolor(black)) graphregion(color(white))  || ///
         GDP1, ciopts(recast(rcap)) xline(0, lpattern(solid) lcolor(black))  ///
		  keep(b*)  bylabel("{&beta}")  rename(brgdpus=US bgdpuk=UK bgdpno=NO bgdpfi=FI bgdp2ch=CH bgdpes=ES bgdppt=PT bgdpse=SE bgdpau=AU bgdphu=HU bgdpcl=CL) ///
		 graphregion(color(white)) leg(off) grid(none)
graph export $figpath/Fig_C3a.pdf, replace fontface("Helvetica") 


* Add these coefficients to plots instead of confidence intervals
ivreg2 volcpi volrgd, robust
local coeff : di %3.2f  _b[volrgd]
local t : di %3.2f  _b[volrgd]/_se[volrgd]
twoway (lfit volcpi volrgd, text(80 3 "Slope (t-stat) = `coeff' (`t')", place(e))) (scatter volcpi volrgd, mcolor(black) mlabel(ctyNames) mlabcolor(black)) ///
       ,xlabel(, nogrid) ylabel(, nogrid)  ///
	   	plotregion(lcolor(black)) graphregion(color(white))   ///
        xtitle("Variance inflation relative to U.K.") ///
	   ytitle("Variance real GDP growth relative to U.K.") ///
	   leg(off)  
graph export $figpath/Fig_3b.pdf, replace fontface("Helvetica")
 
ivreg2 base volcpi , robust
local coeff : di %3.2f  _b[volcpi]
local t : di %3.2f  _b[volcpi]/_se[volcpi]
twoway (lfit base volcpi, text(6 3 "Slope (t-stat) = `coeff' (`t')", place(e))) (scatter base volcpi, mcolor(black) mlabel(ctyNames) mlabcolor(black)) ///
       , xlabel(, nogrid) ylabel(, nogrid) ///
	   plotregion(lcolor(black)) graphregion(color(white))  ///
        xtitle("Variance inflation relative to U.K.") ///
	   ytitle("{&beta} (in pp)") ///
	   graphregion(fcolor(white)) leg(off)  plotregion(lcolor(black)) 
graph export $figpath/Fig_3c.pdf, replace fontface("Helvetica")
	 
ivreg2 base  volrgd , robust
local coeff : di %3.2f  _b[volrgd]
local t : di %3.2f  _b[volrgd]/_se[volrgd]
twoway (lfit base volrgd, text(6 3 "Slope (t-stat) = `coeff' (`t')", place(e))) (scatter base volrgd, mcolor(black) mlabel(ctyNames) mlabcolor(black)) ///
       , xlabel(, nogrid) ylabel(, nogrid) ///
	   plotregion(lcolor(black)) graphregion(color(white))  ///
        xtitle("Variance real GDP growth relative to U.K.") ///
	   ytitle("{&beta} (in pp)") ///
	   graphregion(fcolor(white)) leg(off) plotregion(lcolor(black)) 
graph export $figpath/Fig_3d.pdf, replace fontface("Helvetica")

ivreg2 black  volcpi , robust
local coeff : di %3.2f  _b[volcpi]
local t : di %3.2f  _b[volcpi]/_se[volcpi]
twoway (lfit black volcpi, text(4 3 "Slope (t-stat) = `coeff' (`t')", place(e))) (scatter black volcpi, mcolor(black) mlabel(ctyNames) mlabcolor(black)) ///
       , xlabel(, nogrid) ylabel(, nogrid) ///
	   plotregion(lcolor(black)) graphregion(color(white))   ///
        xtitle("Variance inflation relative to U.K.") ///
	   ytitle("{&beta} (in pp)") ///
	   graphregion(fcolor(white)) leg(off) plotregion(lcolor(black)) 
graph export $figpath/Fig_C2c.pdf, replace fontface("Helvetica")
	 
ivreg2 black  volrgd , robust
local coeff : di %3.2f  _b[volrgd]
local t : di %3.2f  _b[volrgd]/_se[volrgd]
twoway (lfit black volrgd, text(4 3 "Slope (t-stat) = `coeff' (`t')", place(e))) (scatter black volrgd, mcolor(black) mlabel(ctyNames) mlabcolor(black)) ///
       , xlabel(, nogrid) ylabel(, nogrid) ///
	   plotregion(lcolor(black)) graphregion(color(white))   ///
        xtitle("Variance real GDP growth relative to U.K.") ///
	   ytitle("{&beta} (in pp)") ///
	   graphregion(fcolor(white)) leg(off) plotregion(lcolor(black)) 
graph export $figpath/Fig_C2d.pdf, replace fontface("Helvetica")

* Add these coefficients to plots instead of confidence intervals
ivreg2 volcpi volrgd if black < ., robust
local coeff : di %3.2f  _b[volrgd]
local t : di %3.2f  _b[volrgd]/_se[volrgd]
twoway (lfit volcpi volrgd  if black < ., text(60 3 "Slope (t-stat) = `coeff' (`t')", place(e))) (scatter volcpi volrgd if black < ., mcolor(black) mlabel(ctyNames) mlabcolor(black)) ///
       ,xlabel(, nogrid) ylabel(, nogrid)  ///
	   	plotregion(lcolor(black)) graphregion(color(white))   ///
        xtitle("Variance inflation relative to U.K.") ///
	   ytitle("Variance real GDP growth relative to U.K.") ///
	   leg(off)  
graph export $figpath/Fig_C2b.pdf, replace fontface("Helvetica")


* Do without Sweden
* Add these coefficients to plots instead of confidence intervals
ivreg2 volcpi volrgd if ctyNames != "SE", robust
local coeff : di %3.2f  _b[volrgd]
local t : di %3.2f  _b[volrgd]/_se[volrgd]
twoway (lfit volcpi volrgd if ctyNames != "SE", text(80 3 "Slope (t-stat) = `coeff' (`t')", place(e))) (scatter volcpi volrgd if ctyNames != "SE", mcolor(black) mlabel(ctyNames) mlabcolor(black)) ///
       ,xlabel(, nogrid) ylabel(, nogrid)  ///
	   	plotregion(lcolor(black)) graphregion(color(white))   ///
        xtitle("Variance inflation relative to U.K.") ///
	   ytitle("Variance real GDP growth relative to U.K.") ///
	   leg(off)  
graph export $figpath/Fig_C3b.pdf, replace fontface("Helvetica")
 
ivreg2 base volcpi if ctyNames != "SE" , robust
local coeff : di %3.2f  _b[volcpi]
local t : di %3.2f  _b[volcpi]/_se[volcpi]
twoway (lfit base volcpi if ctyNames != "SE", text(6 3 "Slope (t-stat) = `coeff' (`t')", place(e))) (scatter base volcpi if ctyNames != "SE", mcolor(black) mlabel(ctyNames) mlabcolor(black)) ///
       , xlabel(, nogrid) ylabel(, nogrid) ///
	   plotregion(lcolor(black)) graphregion(color(white))  ///
        xtitle("Variance inflation relative to U.K.") ///
	   ytitle("{&beta} (in pp)") ///
	   graphregion(fcolor(white)) leg(off)  plotregion(lcolor(black)) 
graph export $figpath/Fig_C3c.pdf, replace fontface("Helvetica")
	 
ivreg2 base  volrgd if ctyNames != "SE" , robust
local coeff : di %3.2f  _b[volrgd]
local t : di %3.2f  _b[volrgd]/_se[volrgd]
twoway (lfit base volrgd if ctyNames != "SE", text(6 3 "Slope (t-stat) = `coeff' (`t')", place(e))) (scatter base volrgd if ctyNames != "SE", mcolor(black) mlabel(ctyNames) mlabcolor(black)) ///
       , xlabel(, nogrid) ylabel(, nogrid) ///
	   plotregion(lcolor(black)) graphregion(color(white))  ///
        xtitle("Variance real GDP growth relative to U.K.") ///
	   ytitle("{&beta} (in pp)") ///
	   graphregion(fcolor(white)) leg(off) plotregion(lcolor(black)) 
graph export $figpath/Fig_C3d.pdf, replace fontface("Helvetica")
 


