*  genspot.ado
*  Bootstrap a spot rate curve from a yield curve of market interest rates
*  under a no-arbitrage assumption
*
*  Timothy J. Schmidt
*  March 12, 2019
*
*  Syntax: genspot yspot [if] [in], principal(principal amt)
*		tenor(tenor variable) price(price variable) coupon(coupon variable)
*		ytm(yield to maturity variable) freq(payments per annum) fn(filename)

program define genspot
	version 10

	syntax newvarlist(max=2) [if] [in], PRINcipal(real) Tenor(varname numeric) ///
		Coupon(varname numeric) Ytm(varname numeric) PRICe(varname numeric) ///
		FReq(integer) FN(string)

	tokenize "`varlist'", parse( ,)

	local yspot "`1'"

	qui {
		preserve

		cap keep `if' `in'
		sort `tenor'

		/*  Report an error if there aren't at least two zero coupon bonds to start  */

		if (`coupon'[1]!= 0 | `coupon'[2]!=0) {
			noi di in red "at least two zero coupon bonds required to bootstrap spot rates"
			restore
			exit
		}

		/*  Generate a cubic spline curve from the coupon rates  */

		tempfile fnspline
		tempvar yld
		local incr = 1/`freq'
		splinert `yld', x(`tenor') y(`ytm') inc(`incr') fn(`fnspline')

		/*  Determine the number of observations based on the frequency  */

		local nobs = `tenor'[_N] * `freq'
		set obs `nobs'

		sort `tenor'
		merge `tenor' using `fnspline'
		sort `tenor'
		drop if `tenor'==.
		local yvn = "`ytm'"
		drop `ytm' _merge
		ren `yld' `yvn'

		/*  If a bond has a zero coupon, it is a spot rate  */

		gen `yspot' = `yvn'/(100 * `freq') if `coupon'==0

		tempvar t
		gen `t' = `incr' in 1
		replace `t' = `t'[_n-1] + `incr' in 2/l

		tempvar df dcf cdcf cf
		gen `df' = .
		gen `dcf' = .
		gen `cdcf' = .
		gen `cf' = .

		/*  Find the remaining spot rates recursively  */

		local i = 1
		while (`i' <= _N) {
		/*  Skip the zero coupon bonds  */
			if (`coupon'[`i'] != 0) {

		/*  Calculate the cash flow from the coupon rate on the bond  */
				replace `cf' = `yvn'[`i']/`freq' in 1/`i'
				replace `cf' = `cf' + `principal' in `i'

		/*  Calculate DCF using spot rates from shorter tenors  */
				replace `df' = (1/(1+`yspot'))^(`t'*`freq')
				replace `dcf' = `cf' * `df'
				cap drop `cdcf'
				egen `cdcf' = sum(`dcf')
				replace `yspot' = ((`cf'/(100 - `cdcf'[`i'-1]))^(1/(`t'*`freq'))-1) in `i'
			}

			local i = `i' + 1
		}

		/*  Annualize the spot rate  */

		replace `yspot' = `yspot' * `freq' * 100

		/*  Save the curve and restore the user's dataset  */

		keep `t' `yspot'
		ren `t' `tenor'
		order `tenor' `yspot'
		sort `tenor'
		save `fn', replace
		noi di "spot curve saved in `fn'.dta"

		restore
	}
end
