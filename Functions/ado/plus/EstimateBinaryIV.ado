program EstimateBinaryIV
args yt xt zt qt lag dSample indicator depName cty saveB

version 15
quietly{

* If we don't use covariates
if (`qt' == 0) {

	* ----------- IV, CPI--------------
	ivreg2 yt (xt = zt) if dSample == 1, robust bw(2)
	scalar hasConverged = 1

	eststo mod4`depName'`cty':  nlcom  (a`depName'`cty': _b[_cons]) ///
									   (b`depName'`cty': _b[xt]) ///
									   (ab`depName'`cty': _b[_cons]+_b[xt]), post
	estadd scalar Converge = hasConverged
	estadd local Bound "Lower"
	estadd local Method "IV"
	estadd local Indicator "`indicator'"
	estadd local DepVar "`depName'"
}
else{

	* ----------- IV, CPI--------------
	ivreg2 yt qt (xt = zt) if dSample == 1, robust bw(2)
	scalar hasConverged = 1

	eststo mod4`depName'`cty':  nlcom  (a`depName'`cty': _b[_cons]) ///
									   (b`depName'`cty': _b[xt]) ///
									   (ab`depName'`cty': _b[_cons]+_b[xt]) ///
									   (d`depName'`cty': _b[qt]), post
	estadd scalar Converge = hasConverged
	estadd local Bound "Lower"
	estadd local Method "IV"
	estadd local Indicator "`indicator'"
	estadd local DepVar "`depName'"
	test b`depName'`cty' = d`depName'`cty'
	estadd scalar pDiff = r(p)
}
if (`lag' == 1){

	gen lag = L1.yt
	
	* ----------- IV, CPI--------------
	ivreg2 yt lag (xt = zt) if dSample == 1, robust bw(2)
	scalar hasConverged = 1

	eststo mod4`depName'`cty':  nlcom  (a`depName'`cty': _b[_cons]/(1-_b[lag])) ///
									   (b`depName'`cty': _b[xt]/(1-_b[lag])) ///
									   (ab`depName'`cty': (_b[_cons]+_b[xt])/(1-_b[lag])) ///
									   (l`depName'`cty': _b[lag]), post
	estadd scalar Converge = hasConverged
	estadd local Bound "Lower"
	estadd local Method "IV"
	estadd local Indicator "`indicator'"
	estadd local DepVar "`depName'"
	
	drop lag
}
}

disp("IV for `depName' - `indicator' - done")

end
