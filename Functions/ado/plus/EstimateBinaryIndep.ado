program EstimateBinaryIndep
args yt xt zt qt dSample maxIter maxTry indicator depName cty saveB

version 15
quietly {


* If we don't use covariates
if (`qt' == 0) {

	* Generate deflation indicators
	gen m00 = (1-xt)*(1-zt)
	gen m10 = xt*(1-zt)
	gen m01 = (1-xt)*zt
	gen m11 = xt*zt
	
	* Get OLS starting values
	ivreg2 yt m11 m01 m10 if dSample == 1, robust bw(2)
		
	* ------------ GMM, conditional independence --------------
	local aInit   = _b[_cons]
	local bInit   = _b[m11]
	local exInit = runiform(0, .8)
	local nxInit = runiform(0, .8)
	local ezInit = runiform(0, .8)
	local nzInit = runiform(0, .8)
	local pdInit = runiform(0, .8)

	scalar hasConverged = 0
	local currTry = 0
	while (hasConverged == 0 & `currTry' < `maxTry ') {

		if `currTry'>1 {
				noisily{
				disp("Try no: `currTry'")
			}

			* Use a random change if no convergence
			local aInit  = runiform(-6, 6)
			local bInit  = runiform(-6, 6)
			local exInit = runiform(0, .8)
			local nxInit = runiform(0, .8)
			local ezInit = runiform(0, .8)
			local nzInit = runiform(0, .8)
			local pdInit = runiform(.4, .6)
		}

		gmm (m00 - ({ex}*{ez}*{pd}+(1-{nx})*(1-{nz})*(1-{pd})))					///
			(m10 - ((1-{ex})*{ez}*{pd}+{nx}*(1-{nz})*(1-{pd})))					///
			(m01 - ({ex}*(1-{ez})*{pd}+(1-{nx})*{nz}*(1-{pd})))					///
			(yt*m00 - m00*({a} + {b}*{pd}*{ex}*{ez}/({ex}*{ez}*{pd}+(1-{nx})*(1-{nz})*(1-{pd}))))		///
			(yt*m10 - m10*({a} + {b}*{pd}*(1-{ex})*{ez}/((1-{ex})*{ez}*{pd}+{nx}*(1-{nz})*(1-{pd}))))	///
			(yt*m01 - m01*({a} + {b}*{pd}*{ex}*(1-{ez})/({ex}*(1-{ez})*{pd}+(1-{nx})*{nz}*(1-{pd}))))	///
			(yt*m11 - m11*({a} + {b}*{pd}*(1-{ex})*(1-{ez})/((1-{ex})*(1-{ez})*{pd}+{nx}*{nz}*(1-{pd})))) ///	
			if dSample == 1 ///
			,instruments(4:m00, noconst) ///
			instruments(5:m10, noconst) instruments(6:m01, noconst) instruments(7:m11, noconst) ///
			winitial(unadjusted, independent) ///
			twostep  vce(hac nwest 2) 	///
			from(a `aInit' b `bInit' ex `exInit' nx `nxInit' pd `pdInit' ez `ezInit' nz `nzInit')  ///
			conv_maxiter(`maxIter') 
		scalar hasConverged = e(converged)
		local currTry = `currTry' + 1
	}
	estat overid
	scalar pOverident = chi2tail(e(J_df), e(J))
	scalar sOverident = e(J)
	scalar nTries = `currTry'

	if(hasConverged == 0){
		matrix b = e(b)
		matrix V = e(V)
		
		local nEl = colsof(b) 
		foreach i of numlist 1/`nEl'{
			matrix b[1,`i'] = 0
			matrix V[`i', `i'] = 10000
			
		}
		ereturn post b V
		
		eststo mod3`depName'`cty'
	}

	if(hasConverged == 1){
		eststo mod3`depName'`cty': nlcom  (a`depName'`cty': _b[a:_cons]) ///
										  (b`depName'`cty': _b[b:_cons]) ///
										  (ab`depName'`cty': _b[a:_cons]+_b[b:_cons]) ///
										  (pd`depName'`cty': _b[pd:_cons]) ///
		(biasXa`depName'`cty':   _b[b:_cons]*(_b[ex:_cons])*(_b[pd:_cons])/(_b[ex:_cons]*(_b[pd:_cons])+(1-_b[nx:_cons])*(1-_b[pd:_cons]))) ///
		(biasXb`depName'`cty':  -_b[b:_cons]*(_b[nx:_cons]*(1-_b[pd:_cons]))/((1-_b[ex:_cons])*_b[pd:_cons]+_b[nx:_cons]*(1-_b[pd:_cons]))+_b[ex:_cons]*(_b[pd:_cons])/(_b[ex:_cons]*(_b[pd:_cons])+(1-_b[nx:_cons])*(1-_b[pd:_cons]))) ///
		(biasXab`depName'`cty': -_b[b:_cons]*(_b[nx:_cons]*(1-_b[pd:_cons]))/((1-_b[ex:_cons])*_b[pd:_cons]+_b[nx:_cons]*(1-_b[pd:_cons]))) ///
							(ex: _b[ex:_cons]) (nx: _b[nx:_cons]) (ez: _b[ex:_cons]) (nz: _b[nx:_cons]) /// 
						   , post
	}					   
	if(hasConverged == 0){
	eststo mod3`depName'`cty': nlcom (a`depName'`cty': _b[a:_cons]) ///
									 (b`depName'`cty': _b[b:_cons]) ///
									 (ab`depName'`cty': _b[a:_cons]+_b[b:_cons])  ///
									 (pd`depName'`cty': _b[pd:_cons]), post
	}	

	* Add stats GMM diagnostics
	estadd local Bound "Point"
	estadd local Method "GMM"
	estadd local Indicator "`indicator'"
	estadd local DepVar "`depName'"

	estadd scalar nTries = nTries
	estadd scalar Converge   = hasConverged
	estadd scalar sOverident = sOverident
	estadd scalar pOverident = pOverident

	estadd scalar maxIter = `maxIter'

}
else{

	* Get OLS starting values
	ivreg2 yt xt qt if dSample == 1, robust bw(2)

	* ------------ GMM, conditional independence --------------
	local aInit   = _b[_cons]
	local bInit   = _b[xt]
	local dInit   = _b[qt]
	local eqInit = runiform(0, .8)
	local nqInit = runiform(0, .8)
	local exInit = runiform(0, .8)
	local nxInit = runiform(0, .8)
	local ezInit = runiform(0, .8)
	local nzInit = runiform(0, .8)
	local pdInit = runiform(0, .8)
	
	gen m000 = (1-xt)*(1-zt)*(1-qt)
	gen m100 = xt*(1-zt)*(1-qt)
	gen m010 = (1-xt)*zt*(1-qt)
	gen m110 = xt*zt*(1-qt)
	gen m001 = (1-xt)*(1-zt)*qt
	gen m101 = xt*(1-zt)*qt
	gen m011 = (1-xt)*zt*qt
	gen m111 = xt*zt*qt

	scalar hasConverged = 0
	local currTry = 0
	while (hasConverged == 0 & `currTry' < `maxTry ') {

		if `currTry'>1 {
				noisily{
				disp("Try no: `currTry'")
			}

			* Use a random change if no convergence
			local aInit  = runiform(-6, 6)
			local bInit  = runiform(-6, 6)
			local dInit  = runiform(-6, 6)
			local eqInit = runiform(0, .8)
			local nqInit = runiform(0, .8)
			local exInit = runiform(0, .8)
			local nxInit = runiform(0, .8)
			local ezInit = runiform(0, .8)
			local nzInit = runiform(0, .8)
			local pdInit = runiform(.4, .6)
				
		}

		gmm (m000 - ({ex}*{ez}*{eq}*{pd}+(1-{nx})*(1-{nz})*(1-{nq})*(1-{pd})))	///
				(m100 - ((1-{ex})*{ez}*{eq}*{pd}+{nx}*(1-{nz})*(1-{nq})*(1-{pd})))	///
				(m010 - ({ex}*(1-{ez})*{eq}*{pd}+(1-{nx})*{nz}*(1-{nq})*(1-{pd})))	///
				(m110 - ((1-{ex})*(1-{ez})*{eq}*{pd}+{nx}*{nz}*(1-{nq})*(1-{pd})))	///
				(m001 - ({ex}*{ez}*(1-{eq})*{pd}+(1-{nx})*(1-{nz})*{nq}*(1-{pd})))	///
				(m011 - ({ex}*(1-{ez})*(1-{eq})*{pd}+(1-{nx})*{nz}*{nq}*(1-{pd})))	///
				(m000*yt - m000*({a} + {b}*{pd}*{ex}*{ez}*{eq}/({ex}*{ez}*{eq}*{pd}+(1-{nx})*(1-{nz})*(1-{nq})*(1-{pd}))))		///
				(m100*yt - m100*({a} + {b}*{pd}*(1-{ex})*{ez}*{eq}/((1-{ex})*{ez}*{eq}*{pd}+{nx}*(1-{nz})*(1-{nq})*(1-{pd}))))	///
				(m010*yt - m010*({a} + {b}*{pd}*{ex}*(1-{ez})*{eq}/({ex}*(1-{ez})*{eq}*{pd}+(1-{nx})*{nz}*(1-{nq})*(1-{pd}))))	///
				(m110*yt - m110*({a} + {b}*{pd}*(1-{ex})*(1-{ez})*{eq}/((1-{ex})*(1-{ez})*{eq}*{pd}+{nx}*{nz}*(1-{nq})*(1-{pd})))) ///	
				(m001*yt - m001*({a} + {b}*{pd}*{ex}*{ez}*(1-{eq})/({ex}*{ez}*(1-{eq})*{pd}+(1-{nx})*(1-{nz})*{nq}*(1-{pd})) + {d}))		///
				(m011*yt - m011*({a} + {b}*{pd}*{ex}*(1-{ez})*(1-{eq})/({ex}*(1-{ez})*(1-{eq})*{pd}+(1-{nx})*{nz}*{nq}*(1-{pd})) + {d}))	///
				(m111*yt - m111*({a} + {b}*{pd}*(1-{ex})*(1-{ez})*(1-{eq})/((1-{ex})*(1-{eq})*(1-{ez})*{pd}+{nx}*{nz}*{nq}*(1-{pd})) + {d})) ///	
				if dSample == 1  ///
				,instruments(7:m000, noconst) instruments(8:m100, noconst) instruments(9:m010, noconst) ///
				instruments(10:m110, noconst) instruments(11:m001, noconst) ///
				instruments(12:m011, noconst) instruments(13:m111, noconst) ///
				winitial(unadjusted, independent) ///
				twostep  vce(hac nwest 2) 	///
				from(a `aInit' b `bInit' d `dInit' ex `exInit' nx `nxInit' pd `pdInit' ez `ezInit' nz `nzInit'   eq `eqInit' nq `nqInit') ///
				conv_maxiter(`maxIter')
			scalar hasConverged = e(converged)
			local currTry = `currTry' + 1
	}
	estat overid
	scalar pOverident = chi2tail(e(J_df), e(J))
	scalar sOverident = e(J)
	scalar nTries = `currTry'
	
	if(hasConverged == 0){
		matrix b = e(b)
		matrix V = e(V)
		
		local nEl = colsof(b) 
		foreach i of numlist 1/`nEl'{
			matrix b[1,`i'] = 0
			matrix V[`i', `i'] = 10000
			
		}
		ereturn post b V
		
		eststo mod3`depName'`cty'
	}

	if(hasConverged == 1){
		eststo mod3`depName'`cty': nlcom  (a`depName'`cty': _b[a:_cons]) ///
										  (b`depName'`cty': _b[b:_cons]) ///
										  (ab`depName'`cty': _b[a:_cons]+_b[b:_cons]) ///
										  (d`depName'`cty': _b[d:_cons]) ///
										  (pd`depName'`cty': _b[pd:_cons]) ///
		(biasXa`depName'`cty':   _b[b:_cons]*(_b[ex:_cons])*(_b[pd:_cons])/(_b[ex:_cons]*(_b[pd:_cons])+(1-_b[nx:_cons])*(1-_b[pd:_cons]))) ///
		(biasXb`depName'`cty':  -_b[b:_cons]*(_b[nx:_cons]*(1-_b[pd:_cons]))/((1-_b[ex:_cons])*_b[pd:_cons]+_b[nx:_cons]*(1-_b[pd:_cons]))+_b[ex:_cons]*(_b[pd:_cons])/(_b[ex:_cons]*(_b[pd:_cons])+(1-_b[nx:_cons])*(1-_b[pd:_cons]))) ///
		(biasXab`depName'`cty': -_b[b:_cons]*(_b[nx:_cons]*(1-_b[pd:_cons]))/((1-_b[ex:_cons])*_b[pd:_cons]+_b[nx:_cons]*(1-_b[pd:_cons]))) ///
							(ex: _b[ex:_cons]) (nx: _b[nx:_cons]) (ez: _b[ex:_cons]) (nz: _b[nx:_cons]) /// 
						   , post
	}					   
	if(hasConverged == 0){
	eststo mod3`depName'`cty': nlcom (a`depName'`cty': _b[a:_cons]) ///
									 (b`depName'`cty': _b[b:_cons]) ///
									 (ab`depName'`cty': _b[a:_cons]+_b[b:_cons])  ///
									 (d`depName'`cty': _b[d:_cons])  ///
									 (pd`depName'`cty': _b[pd:_cons]), post
	}	

	* Add stats GMM diagnostics
	estadd local Bound "Point"
	estadd local Method "GMM"
	estadd local Indicator "`indicator'"
	estadd local DepVar "`depName'"

	estadd scalar nTries = nTries
	estadd scalar Converge   = hasConverged
	estadd scalar sOverident = sOverident
	estadd scalar pOverident = pOverident

	estadd scalar maxIter = `maxIter'
	
	test b`depName'`cty' = d`depName'`cty'
	estadd scalar pDiff = r(p)
}
	
drop m0* m1*
			
}

disp("Cond. independence for `depName' - `indicator' - done")

end
