*  genfwd.ado
*  Generate a forward rate curve from a yield curve of spot rates
*
*  Timothy J. Schmidt
*  June 8, 2019
*
*  Syntax: genfwd ycfwd [if] [in], spotrate(varname numeric) tenor(varname numeric) nperiods(integer)

cap program drop genfwd
program define genfwd
	version 10

	syntax newvarlist(max=1) [if] [in], SPOTrate(varname numeric) Tenor(varname numeric) Nperiods(integer)

	tokenize "`varlist'", parse( ,)

	local yfwd "`1'"

	qui {
		cap keep `if' `in'

		sort `tenor'

		tempvar i tperiods
		gen `i' = _n
		gen `tperiods' = `nperiods' + `i'
		gen `yfwd' = ((((1 + `spotrate'[`tperiods']/200)^`tperiods') / ((1 + `spotrate'[`nperiods']/200)^`nperiods'))^(1/`i') - 1) * 200
	}
end
