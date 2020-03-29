program EstimateBinaryBlack
args yt xt zt qt lag dSample indicator depName cty saveB

version 15

quietly{

* If we don't use covariates
if (`qt' == 0) {

	* ------------ Black et al, unconstrained --------------
	* Generate deflation indicators
	gen m00 = (1-xt)*(1-zt)
	gen m10 = xt*(1-zt)
	gen m01 = (1-xt)*zt
	gen m11 = xt*zt

	ivreg2 yt m11 m01 m10 if dSample == 1, robust bw(2)

	eststo mod2`depName'`cty': nlcom  (a`depName'`cty': _b[_cons]) ///
									  (b`depName'`cty': _b[m11]) ///
									  (ab`depName'`cty': _b[_cons]+_b[m11]) ///
									  (m01`depName'`cty': _b[m01]) (m10`depName'`cty': _b[m10]), post

	drop m0* m1*

	estadd scalar Converge = 1
	estadd local Bound "Upper"
	estadd local Method "OLS"
	estadd local Indicator "`indicator'"
	estadd local DepVar "`depName'"
}
else{

	* ------------ Black et al, unconstrained --------------
	* Generate deflation indicators
	gen m00 = (1-xt)*(1-zt)
	gen m10 = xt*(1-zt)
	gen m01 = (1-xt)*zt
	gen m11 = xt*zt

	ivreg2 yt m11 m01 m10 qt if dSample == 1, robust bw(2)

	eststo mod2`depName'`cty': nlcom  (a`depName'`cty': _b[_cons]) ///
									  (b`depName'`cty': _b[m11]) ///
									  (ab`depName'`cty': _b[_cons]+_b[m11]) ///
									  (d`depName'`cty': _b[qt]) ///
									  (m01`depName'`cty': _b[m01]) (m10`depName'`cty': _b[m10]), post

	drop m0* m1*

	estadd scalar Converge = 1
	estadd local Bound "Upper"
	estadd local Method "OLS"
	estadd local Indicator "`indicator'"
	estadd local DepVar "`depName'"
	test b`depName'`cty' = d`depName'`cty'
	estadd scalar pDiff = r(p)
}

if(`lag' == 1){
	gen lag = L1.yt
	
	* ------------ Black et al, unconstrained --------------
	* Generate deflation indicators
	gen m00 = (1-xt)*(1-zt)
	gen m10 = xt*(1-zt)
	gen m01 = (1-xt)*zt
	gen m11 = xt*zt
		
	ivreg2 yt m11 m01 m10 lag if dSample == 1, robust bw(2)

	eststo mod2`depName'`cty': nlcom  (a`depName'`cty': _b[_cons]/(1-_b[lag])) ///
									  (b`depName'`cty': _b[m11]/(1-_b[lag])) ///
									  (ab`depName'`cty': (_b[_cons]+_b[m11])/(1-_b[lag])) ///
									  (l`depName'`cty': _b[lag]) ///
									  (m01`depName'`cty': _b[m01]) (m10`depName'`cty': _b[m10]), post

	drop m0* m1*

	estadd scalar Converge = 1
	estadd local Bound "Upper"
	estadd local Method "OLS"
	estadd local Indicator "`indicator'"
	estadd local DepVar "`depName'"
	
	drop lag

}
}

disp("Black for `depName' - `indicator' - done")
		

end
