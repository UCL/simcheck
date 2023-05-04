/*
STEP 8. LOOK FOR OUTLIERS.
simcheck08.do
IW 18/01/2023
*/

// Preliminaries
ssc install sencode


// Scatterplot the estimates and standard errors
use simcheck07_postfile, clear
sencode method, gen(methnum)
label var b "Point estimate"
label var se "Standard error estimate"
scatter se b, by(methnum, row(1) note("")) jitter(1) name(simcheck08, replace) ///
	xtitle(,size(large)) ytitle(,size(large)) xlabel(,labsize(large)) /// 
	ylabel(,labsize(large)) subtitle(,size(vlarge)) xsize(9) ysize(5)


