program EstimateBinaryOLS
args yt xt qt lag dSample indicator depName cty saveB 

version 15

quietly{

* If we don't use covariates
if (`qt' == 0) {
	* ----------- OLS, CPI--------------
	ivreg2 yt xt if dSample == 1, robust bw(2)
	scalar hasConverged = 1

	eststo mod1`depName'`cty': nlcom  (a`depName'`cty': _b[_cons]) ///
									  (b`depName'`cty': _b[xt]) ///
									  (ab`depName'`cty': _b[_cons]+_b[xt]), post


	estadd scalar Converge = hasConverged
	estadd local Bound "Upper"
	estadd local Method "OLS"
	estadd local Indicator "`indicator'"
	estadd local DepVar "`depName'"
}
else{
	* ----------- OLS, CPI--------------
	ivreg2 yt xt qt if dSample == 1, robust bw(2)
	scalar hasConverged = 1

	eststo mod1`depName'`cty'cov: nlcom  (a`depName'`cty': _b[_cons]) ///
									  (b`depName'`cty': _b[xt]) ///
									  (ab`depName'`cty': _b[_cons]+_b[xt]) ///
									  (d`depName'`cty': _b[qt]), post

	estadd scalar Converge = hasConverged
	estadd local Bound "Upper"
	estadd local Method "OLS"
	estadd local Indicator "`indicator'"
	estadd local DepVar "`depName'"
	test b`depName'`cty' = d`depName'`cty'
	estadd scalar pDiff = r(p)
}
if (`lag' == 1) {
	gen lag = L1.yt
	
	* ----------- OLS, CPI--------------
	ivreg2 yt xt lag if dSample == 1, robust bw(2)
	scalar hasConverged = 1

	eststo mod1`depName'`cty':  nlcom  (a`depName'`cty': _b[_cons]/(1-_b[lag])) ///
									   (b`depName'`cty': _b[xt]/(1-_b[lag])) ///
									   (ab`depName'`cty': (_b[_cons]+_b[xt])/(1-_b[lag])) ///
									   (l`depName'`cty': _b[lag]), post

	estadd scalar Converge = hasConverged
	estadd local Bound "Upper"
	estadd local Method "OLS"
	estadd local Indicator "`indicator'"
	estadd local DepVar "`depName'"
	
	drop lag
}
}
disp("OLS for `depName' - `indicator' - done")
	
	
end
