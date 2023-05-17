*  splinert.ado
*  Fit a cubic spline to a curve defined by user-specified data points
*
*  Timothy J. Schmidt
*  June 23, 2019
*
*  Syntax: splinert spline [if] [in], x(varname) y(varname) fn(filename) inc(real)

cap program drop splinert
program define splinert
	version 10

	syntax newvarlist(max=1) [if] [in], X(varname numeric) Y(varname numeric) Inc(real) FN(string)

	tokenize "`varlist'", parse( ,)

	local spline "`1'"

	qui {
		local numObs = _N
		preserve

		sort `x'
		cap keep `if' `in'

		tempvar A b z

		/*  Determine the dimension of matrix needed, based on qualified observations  */

		local npts = _N
		local dim = (_N-1) * 4

		/*  Define the matrices needed to hold the equation parameters  */

		mat `A' = J(`dim', `dim', 0)
		mat `b' = J(`dim', 1, 0)

		/*  Loop through the matrix to fill it  */

		/*  Set the entries that map to the end point equations (npts-1 pairs of them)  */

		local r = 1
		local c = 1
		local i = 1

		mat `A'[`r',`c'] = 1
		mat `A'[`r',`c'+1] = `x'[`i']
		mat `A'[`r',`c'+2] = `x'[`i']^2
		mat `A'[`r',`c'+3] = `x'[`i']^3

		local i = `i' + 1
		local r = `r' + 1
		while (`r' < ((`npts'-1)*2)) {
			mat `A'[`r',`c'] = 1
			mat `A'[`r',`c'+1] = `x'[`i']
			mat `A'[`r',`c'+2] = `x'[`i']^2
			mat `A'[`r',`c'+3] = `x'[`i']^3

			local c = `c' + 4

			mat `A'[`r'+1,`c'] = 1
			mat `A'[`r'+1,`c'+1] = `x'[`i']
			mat `A'[`r'+1,`c'+2] = `x'[`i']^2
			mat `A'[`r'+1,`c'+3] = `x'[`i']^3

			local i = `i' + 1
			local r = `r' + 2
		}

		mat `A'[`r',`c'] = 1
		mat `A'[`r',`c'+1] = `x'[`i']
		mat `A'[`r',`c'+2] = `x'[`i']^2
		mat `A'[`r',`c'+3] = `x'[`i']^3

		/*  Set the entries that map to the interior point equations (first derivs)  */

		local R = `r' + `npts'-2
		local r = `r' + 1
		local c = 2
		local i = 2
		while (`r' <= `R') {
			mat `A'[`r',`c'] = 1
			mat `A'[`r',`c'+1] = 2 * `x'[`i']
			mat `A'[`r',`c'+2] = 3 * `x'[`i']^2

			local c = `c' + 4

			mat `A'[`r',`c'] = -1
			mat `A'[`r',`c'+1] = -2 * `x'[`i']
			mat `A'[`r',`c'+2] = -3 * `x'[`i']^2

			local i = `i' + 1
			local r = `r' + 1
		}

		/*  Set the entries that map to the interior point equations (second derivs)  */

		local R = `r' + `npts'-3
		local c = 3
		local i = 2
		while (`r' <= `R') {
			mat `A'[`r',`c'] = 2
			mat `A'[`r',`c'+1] = 6 * `x'[`i']

			local c = `c' + 4

			mat `A'[`r',`c'] = -2
			mat `A'[`r',`c'+1] = -6 * `x'[`i']

			local i = `i' + 1
			local r = `r' + 1
		}

		/*  Set the end point equations (second derivs)  */

		local c = 3
		mat `A'[`r',`c'] = 2
		mat `A'[`r',`c'+1] = 6 * `x'[1]

		local c = `dim' - 1
		local r = `r' + 1

		mat `A'[`r',`c'] = 2
		mat `A'[`r',`c'+1] = 6 * `x'[_N]

		/*  Fill the b matrix  */

		mat `b'[1,1] = `y'[1]
		local i = 2
		local R = `dim'/2
		local r = 2
		while (`r' < `R') {
			mat `b'[`r',1] = `y'[`i']
			mat `b'[`r'+1,1] = `y'[`i']

			local i = `i' + 1
			local r = `r' + 2
		}
		mat `b'[`r',1] = `y'[_N]

		/*  Generate the spline from the matrices  */

		mat `z' = inv(`A') * `b'
		local n = round((`x'[_N] - `x'[1] + 1) * 1/`inc', 1)
		set obs `n'
		tempvar xvar
		gen `xvar' = `x'[1] in 1
		replace `xvar' = `xvar'[_n-1] + `inc' in 2/l
		gen `spline' = `y'[1] in 1

		local i = 1
		local r = 1
		while (`r' <= `dim') {
			replace `spline' = `z'[`r',1] + `z'[`r'+1,1] * `xvar' + `z'[`r'+2,1] * `xvar'^2 + `z'[`r'+3,1] * `xvar'^3 if `xvar' > `x'[`i'] & `xvar' < `x'[`i'+1]
			local i = `i' + 1
			local r = `r' + 4
		}

		local i = 1
		while (`i' <= `numObs') {
			replace `spline' = `y'[`i'] if `xvar' == `x'[`i']
			local i = `i' + 1
		}

		/*  Save the spline and restore the user's dataset  */

		drop if `spline'==.
		keep `xvar' `spline'
		ren `xvar' `x'
		sort `x'
		save `fn', replace
		noi di "spline saved in `fn'.dta"

		restore
	}

end
