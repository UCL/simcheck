/* 
Figure for simcheck paper, point 8
simcheck08figure.do
IW 24/1/2023 & 4/5/2023
IW 31/8/2023 - makes high quality graphic
IW 6/9/2023 - makes high quality graphic
*/

// Preliminaries
ssc install sencode
local opts by(method, row(1) note("") imargin(l=10)) jitter(1) ///
	xtitle(,size(large)) ytitle(,size(large)) xlabel(,labsize(vlarge)) /// 
	ylabel(,labsize(vlarge)) subtitle(,size(vlarge)) xsize(9) ///
	xtitle("") ytitle("") 
set scheme mrc
// draw graph of R results
use "../R/simcheck07_Rresults.dta", clear
sencode method, replace
scatter se est, name(R, replace) ylabel(0 5000, angle(90)) subtitle("") fysize(48) `opts'

// draw graph of Stata results
use simcheck07_postfile, clear
sencode method, replace
scatter se b, name(Stata, replace) ylabel(0 3 6, angle(90)) fysize(52) `opts'

// draw graph of combined results
graph combine Stata R, col(1) b1title("Point estimate") ///
	l1title("Standard error estimate" " " "R                           Stata") ///
	xsize(9) ysize(5) saving(simcheck08figure,replace) imargin(l=-10) scale(1.2)
graph export Figure2.jpg, width(3600) replace
