program EstimateBinaryFixed
args yt xt zt qt dSample fixE00 fixN11 maxIter maxTry indicator depName cty saveB

version 15
quietly{


* If we don't use covariates
if (`qt' == 0) {
	
	
	* ------------ GMM, constrained misclassification--------------
	* Generate deflation indicators
	gen m00 = (1-xt)*(1-zt)
	gen m10 = xt*(1-zt)
	gen m01 = (1-xt)*zt
	gen m11 = xt*zt

	local count = 1
	local currMin = 1000000
	local currMax = -1000000
			
	* Get OLS starting values
	ivreg2 yt xt if dSample == 1, robust bw(2)
	
	local aInit   = _b[_cons]
	local bInit   = _b[xt]
	local e00Init = runiform(0,.8)
	local e10Init = runiform(0,.8)
	local e01Init = runiform(0,.8)
	local e11Init = runiform(0,.8)
	local n00Init = runiform(0,.8)
	local n10Init = runiform(0,.8)
	local n01Init = runiform(0,.8)
	local n11Init = runiform(0,.8)
	local pdInit = runiform(.4,.6)

	scalar hasConverged = 0
	local currTry = 0

	while (hasConverged == 0 & `currTry' < `maxTry') {

		if `currTry'>1 {
			noisily{
				disp("Try no: `currTry'")
			}
			* Use a random change if no convergence
			local aInit   = runiform(-6, 6)
			local bInit   = runiform(-6, 6)
			local e00Init = runiform(0,.8)
			local e10Init = runiform(0,.8)
			local e01Init = runiform(0,.8)
			local e11Init = runiform(0,.8)
			local n00Init = runiform(0,.8)
			local n10Init = runiform(0,.8)
			local n01Init = runiform(0,.8)
			local n11Init = runiform(0,.8)
			local pdInit = runiform(.4,.6)
		}
					
		* Estimation
		gmm (m00 - (`fixE00'*{pd}+{n00}*(1-{pd})))					///
			(m10 - ({e10}*{pd}+{n10}*(1-{pd})))					///
			(m01 - ((1-`fixE00'-{e10}-{e11})*{pd}+(1-{n00}-{n10}-`fixN11')*(1-{pd}))) ///
			(yt*m00 - m00*({a} + {b}*{pd}*`fixE00'/(`fixE00'*{pd}+{n00}*(1-{pd}))))		///
			(yt*m10 - m10*({a} + {b}*{pd}*{e10}/({e10}*{pd}+{n10}*(1-{pd}))))	///
			(yt*m01 - m01*({a} + {b}*{pd}*(1-`fixE00'-{e10}-{e11})/((1-`fixE00'-{e10}-{e11})*{pd}+(1-{n00}-{n10}-`fixN11')*(1-{pd}))))	///
			(yt*m11 - m11*({a} + {b}*{pd}*{e11}/({e11}*{pd}+`fixN11'*(1-{pd}))))		///
			if dSample == 1 ///
			,instruments(4:m00, noconst) ///
			instruments(5:m10, noconst) instruments(6:m01, noconst) instruments(7:m11, noconst) ///
			winitial(unadjusted, independent) ///
			twostep  vce(hac nwest 2) 	///
			from(a `aInit' b `bInit' e11 `e11Init' e10 `e10Init' n10 `n10Init' n00 `n00Init' pd `pdInit' )  ///
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
		
		eststo mod5`depName'`cty'
	}

	if(hasConverged == 1){
	eststo mod5`depName'`cty': nlcom (a`depName'`cty': _b[a:_cons]) ///
									 (b`depName'`cty': _b[b:_cons]) ///
									 (ab`depName'`cty': _b[a:_cons]+_b[b:_cons])  ///
									 (pd`depName'`cty': _b[pd:_cons]) ///
		(biasXa`depName'`cty':  _b[b:_cons]*(`fixE00'+(1-`fixE00'-_b[e10:_cons]-_b[e11:_cons]))*_b[pd:_cons]/(`fixE00'+(1-`fixE00'-_b[e10:_cons]-_b[e11:_cons])*_b[pd:_cons]+(_b[n00:_cons]+(1-_b[n00:_cons]-_b[n10:_cons]-`fixN11'))*(1-_b[pd:_cons]))) ///
		(biasXb`depName'`cty': -_b[b:_cons]*(`fixE00'+(1-`fixE00'-_b[e10:_cons]-_b[e11:_cons]))*_b[pd:_cons]/(`fixE00'+(1-`fixE00'-_b[e10:_cons]-_b[e11:_cons])*_b[pd:_cons]+(_b[n00:_cons]+(1-_b[n00:_cons]-_b[n10:_cons]-`fixN11'))*(1-_b[pd:_cons])) ///
							   +(_b[n10:_cons]+`fixN11')*(1-_b[pd:_cons])/((_b[e10:_cons]+_b[e11:_cons])*_b[pd:_cons]+(_b[n10:_cons]+`fixN11')*(1-_b[pd:_cons]))) ///		
		(biasXab`depName'`cty':  -_b[b:_cons]*(_b[n10:_cons]+`fixN11')*(1-_b[pd:_cons])/((_b[e10:_cons]+_b[e11:_cons])*_b[pd:_cons]+(_b[n10:_cons]+`fixN11')*(1-_b[pd:_cons]))) ///
								(e00: `fixE00') (e01: (1-`fixE00'-_b[e10:_cons]-_b[e11:_cons])) (e10: _b[e10:_cons]) (e11: _b[e11:_cons]) ///
							(n00: _b[n00:_cons]) (n01: (1-_b[n00:_cons]-_b[n10:_cons]-`fixN11')) (n10: _b[n10:_cons]) (n11: `fixN11') ///
						   , post
	}
	if(hasConverged == 0){
	eststo mod5`depName'`cty': nlcom (a`depName'`cty': _b[a:_cons]) ///
									 (b`depName'`cty': _b[b:_cons]) ///
									 (ab`depName'`cty': _b[a:_cons]+_b[b:_cons])  ///
									 (pd`depName'`cty': _b[pd:_cons]), post
	}

	* Add stats GMM diagnostics
	estadd local Bound "Point"
	estadd local Method "GMM"
	estadd local Indicator "`indicator'"
	estadd local DepVar "`depName'"

	estadd scalar nTries 	 = nTries
	estadd scalar Converge   = hasConverged
	estadd scalar sOverident = sOverident
	estadd scalar pOverident = pOverident

	estadd scalar maxIter = `maxIter'
}
else{

	* ------------ GMM, constrained misclassification--------------
	* Generate deflation indicators
	gen m000 = (1-xt)*(1-zt)*(1-qt)
	gen m100 = xt*(1-zt)*(1-qt)
	gen m010 = (1-xt)*zt*(1-qt)
	gen m110 = xt*zt*(1-qt)
	gen m001 = (1-xt)*(1-zt)*qt
	gen m101 = xt*(1-zt)*qt
	gen m011 = (1-xt)*zt*qt
	gen m111 = xt*zt*qt

	local count = 1
	local currMin = 1000000
	local currMax = -1000000
				
	* Get OLS starting values
	ivreg2 yt xt qt if dSample == 1, robust bw(2)
	
	local aInit   = _b[_cons]
	local bInit   = _b[xt]
	local dInit   = _b[qt]
	local e00Init = runiform(0,.8)
	local e10Init = runiform(0,.8)
	local e01Init = runiform(0,.8)
	local e11Init = runiform(0,.8)
	local n00Init = runiform(0,.8)
	local n10Init = runiform(0,.8)
	local n01Init = runiform(0,.8)
	local n11Init = runiform(0,.8)
	local eqInit = runiform(0,.8)
	local nqInit = runiform(0,.8)
	local pdInit = runiform(.4,.6)

	scalar hasConverged = 0
	local currTry = 0

	while (hasConverged == 0 & `currTry' < `maxTry') {

		if `currTry'>1 {
			noisily{
				disp("Try no: `currTry'")
			}
			* Use a random change if no convergence
			local aInit   = runiform(-6,6)
			local bInit   = runiform(-6,6)
			local dInit   = runiform(-6,6)
			local e00Init = runiform(0,.8)
			local e10Init = runiform(0,.8)
			local e01Init = runiform(0,.8)
			local e11Init = runiform(0,.8)
			local n00Init = runiform(0,.8)
			local n10Init = runiform(0,.8)
			local n01Init = runiform(0,.8)
			local n11Init = runiform(0,.8)
			local eqInit = runiform(0,.8)
			local nqInit = runiform(0,.8)
			local pdInit = runiform(.4,.6)

		}
					
		* Estimation
		gmm (m000 - (`fixE00'*{eq}*{pd}+{n00}*(1-{nq})*(1-{pd})))	///
				(m100 - ({e10}*{eq}*{pd}+{n10}*(1-{nq})*(1-{pd})))	///
				(m010 - ((1-`fixE00'-{e10}-{e11})*{eq}*{pd}+(1-{n00}-{n10}-`fixN11')*(1-{nq})*(1-{pd})))	///
				(m110 - ({e11}*{eq}*{pd}+`fixN11'*(1-{nq})*(1-{pd})))	///
				(m001 - (`fixE00'*(1-{eq})*{pd}+{n00}*{nq}*(1-{pd})))	///
				(m011 - ((1-`fixE00'-{e10}-{e11})*(1-{eq})*{pd}+(1-{n00}-{n10}-`fixN11')*{nq}*(1-{pd})))	///
				(m000*yt - m000*({a} + {b}*{pd}*`fixE00'*{eq}/(`fixE00'*{eq}*{pd}+{n00}*(1-{nq})*(1-{pd}))))		///
				(m100*yt - m100*({a} + {b}*{pd}*{e10}*{eq}/({e10}*{eq}*{pd}+{n10}*(1-{nq})*(1-{pd}))))	///
				(m010*yt - m010*({a} + {b}*{pd}*(1-`fixE00'-{e10}-{e11})*{eq}/((1-`fixE00'-{e10}-{e11})*{eq}*{pd}+(1-`fixN11'-{n00}-{n10})*(1-{nq})*(1-{pd}))))	///
				(m110*yt - m110*({a} + {b}*{pd}*{e11}*{eq}/({e11}*{eq}*{pd}+`fixN11'*(1-{nq})*(1-{pd})))) ///	
				(m001*yt - m001*({a} + {b}*{pd}*`fixE00'*(1-{eq})/(`fixE00'*(1-{eq})*{pd}+{n00}*{nq}*(1-{pd})) + {d}))		///
				(m011*yt - m011*({a} + {b}*{pd}*(1-`fixE00'-{e10}-{e11})*(1-{eq})/((1-`fixE00'-{e10}-{e11})*(1-{eq})*{pd}+(1-`fixN11'-{n00}-{n10})*{nq}*(1-{pd})) + {d}))	///
				(m111*yt - m111*({a} + {b}*{pd}*{e11}*(1-{eq})/({e11}*(1-{eq})*{pd}+`fixN11'*{nq}*(1-{pd})) + {d})) ///	
				if dSample == 1  ///
				,instruments(7:m000, noconst) instruments(8:m100, noconst) instruments(9:m010, noconst) ///
				instruments(10:m110, noconst) instruments(11:m001, noconst) ///
				instruments(12:m011, noconst) instruments(13:m111, noconst) ///
				winitial(unadjusted, independent) ///
				twostep  vce(hac nwest 2) 	///
				from(a `aInit' b `bInit' d `dInit' e11 `e11Init' e10 `e10Init' n10 `n10Init' n00 `n00Init' pd `pdInit' eq `eqInit' nq `nqInit') ///
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
		
		eststo mod5`depName'`cty'
	}

	if(hasConverged == 1){
	eststo mod5`depName'`cty': nlcom (a`depName'`cty': _b[a:_cons]) ///
									 (b`depName'`cty': _b[b:_cons]) ///
									 (ab`depName'`cty': _b[a:_cons]+_b[b:_cons])  ///
									 (d`depName'`cty': _b[d:_cons]) ///
									 (pd`depName'`cty': _b[pd:_cons]) ///
		(biasXa`depName'`cty':  _b[b:_cons]*(`fixE00'+(1-`fixE00'-_b[e10:_cons]-_b[e11:_cons]))*_b[pd:_cons]/(`fixE00'+(1-`fixE00'-_b[e10:_cons]-_b[e11:_cons])*_b[pd:_cons]+(_b[n00:_cons]+(1-_b[n00:_cons]-_b[n10:_cons]-`fixN11'))*(1-_b[pd:_cons]))) ///
		(biasXb`depName'`cty': -_b[b:_cons]*(`fixE00'+(1-`fixE00'-_b[e10:_cons]-_b[e11:_cons]))*_b[pd:_cons]/(`fixE00'+(1-`fixE00'-_b[e10:_cons]-_b[e11:_cons])*_b[pd:_cons]+(_b[n00:_cons]+(1-_b[n00:_cons]-_b[n10:_cons]-`fixN11'))*(1-_b[pd:_cons])) ///
							   +(_b[n10:_cons]+`fixN11')*(1-_b[pd:_cons])/((_b[e10:_cons]+_b[e11:_cons])*_b[pd:_cons]+(_b[n10:_cons]+`fixN11')*(1-_b[pd:_cons]))) ///		
		(biasXab`depName'`cty':  -_b[b:_cons]*(_b[n10:_cons]+`fixN11')*(1-_b[pd:_cons])/((_b[e10:_cons]+_b[e11:_cons])*_b[pd:_cons]+(_b[n10:_cons]+`fixN11')*(1-_b[pd:_cons]))) ///
								(e00: `fixE00') (e01: (1-`fixE00'-_b[e10:_cons]-_b[e11:_cons])) (e10: _b[e10:_cons]) (e11: _b[e11:_cons]) ///
							(n00: _b[n00:_cons]) (n01: (1-_b[n00:_cons]-_b[n10:_cons]-`fixN11')) (n10: _b[n10:_cons]) (n11: `fixN11') ///
						   , post
	}
	if(hasConverged == 0){
	eststo mod5`depName'`cty': nlcom (a`depName'`cty': _b[a:_cons]) ///
									 (b`depName'`cty': _b[b:_cons]) ///
									 (ab`depName'`cty': _b[a:_cons]+_b[b:_cons])  ///
									 (d`depName'`cty': _b[d:_cons]) ///
									 (pd`depName'`cty': _b[pd:_cons]), post
	}

	* Add stats GMM diagnostics
	estadd local Bound "Point"
	estadd local Method "GMM"
	estadd local Indicator "`indicator'"
	estadd local DepVar "`depName'"

	estadd scalar nTries 	 = nTries
	estadd scalar Converge   = hasConverged
	estadd scalar sOverident = sOverident
	estadd scalar pOverident = pOverident

	estadd scalar maxIter = `maxIter'
	
	test b`depName'`cty' = d`depName'`cty'
	estadd scalar pDiff = r(p)
}
drop m0* m1*

}	

disp("Fixed misclass. for `depName' - `indicator' - done")

end
