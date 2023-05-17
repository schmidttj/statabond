clear
pause on
use yc061507
describe
list
pause
line yield tenor
pause
genspot rspot, principal(100) tenor(tenor) price(price) coupon(coupon) ytm(yield) freq(2) fn(rspots)
use rspots
describe
pause
line rspot tenor
pause
genfwd rfwd, spotrate(rspot) tenor(tenor) nperiods(6)
describe
line rfwd rspot tenor

clear
use yc061419
describe
list
pause
line yield tenor
pause
genspot rspot in 4/l, principal(100) tenor(tenor) price(price) coupon(coupon) ytm(yield) freq(2) fn(rspots)
use rspots
describe
pause
line rspot tenor
pause
genfwd rfwd, spotrate(rspot) tenor(tenor) nperiods(6)
describe
line rfwd rspot tenor
pause
pricebond rspot, principal(100) tenor(tenor) coupon(3.5) freq(2)
di r(price)
