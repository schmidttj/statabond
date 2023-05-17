*  pricebond.ado
*  Finds the price per $100 face value of a bond that pays periodic interest
*
*  Timothy J. Schmidt
*  June 9, 2019
*
*  Syntax: pricebond yspot [if] [in], coupon(annual coupon rate) freq(coupons per year)
*		principal(principal amt of one bond) tenor(bond tenor variable)

program define pricebond, rclass
	version 10

	syntax varlist(max=1) [if] [in], Tenor(varname numeric) Coupon(real) Freq(int) PRINcipal(real)

	tokenize "`varlist'"

	local spotrate = "`1'"

	qui {
		cap keep `if' `in'

		tempvar dcf cf px

		/*  Sort the spot rates by tenor (shortest first)  */

		sort `tenor'

		/*  Calculate the cash flow from the coupon rate on the bond  */
		gen `cf' = (`coupon'/(`freq' * 100))
		replace `cf' = `cf' + `principal' in l

		gen `dcf' = `cf' / ((1 + `spotrate'/(`freq' * 200))^(_n))
		egen `px' = sum(`dcf')

		return scalar price = `px'[1]
	}

end
