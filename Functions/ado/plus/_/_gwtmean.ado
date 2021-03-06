/*
_gwtmean.ado  6-5-2001

David Kantor, Institute for Policy Studies
Johns Hopkins University

This is an egen weighted mean function, based on c:\stata\ado\_\_gmean.ado
Actually, it is just a very minor tweaking of _gmean.

In fact this could be considered an enhancement to _gmean; just rename this
file to _gmean and adjust the program define statement.

This is used with egen as follows:

 egen newvar = wtmean(exp), by(byvar) weight(wgt_expr)

This allows a weight option.  egen does not allow a weight in the usual
manner ([weight= wgtvar]), so we us an option.  If the weight option is
omitted, then the action and results are identical to egen mean
(_gmean.ado).  (If the weight expression is a non-zero constant, then, too,
the results are identical to egen mean.)

The weight macro captures the weight.  If it is non-empty, then
it is changed to include parentheses and the * operator.  The inclusion
of parentheses actually allows an expression -- not just a variable.
*/

*! version 1.0.0  6-5-2001
program define _gwtmean
	version 3.0
	local varlist "req new max(1)"
	local exp "req nopre"
	local if "opt"
	local in "opt"
	local options "by(string) weight(string)"
	parse "`*'"
	tempvar touse 
	if "`weight'" ~= "" {
		local weight "* (`weight')"
	}
	quietly {
		gen byte `touse'=1 `if' `in'
		sort `touse' `by'
		by `touse' `by': replace `varlist' = /*
			*/ sum((`exp')`weight')/sum(((`exp')!=.)`weight') if `touse'==1
		by `touse' `by': replace `varlist' = `varlist'[_N]
	}
end
