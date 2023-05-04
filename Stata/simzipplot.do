/*
simzipplot.do
Illustrative zip plots for point 13
IW 18/4/2023 & 4/5/2023
*/

cd "N:\Home\Analysis\Simulations\simcheck\Stata code"

prog drop _all

// CREATE 3 HYPOTHETICAL SETS OF SIMULATION STUDY RESULTS
use simcheck99_postfile, clear
sencode dgm, replace
keep if method=="Full"
keep if dgm==1
drop method N
expand 3
sort dgm rep
by dgm rep: gen type=_n
replace b=b-0.5 if type==2
replace se=se*0.8 if type==3

// CREATE A ZIPPLOT

* install siman, if required
net install siman, from(https://raw.githubusercontent.com/UCL/siman/master/)

* comp0ute performance measures
siman setup, true(0) est(b) se(se) df(df) method(type) rep(rep) dgm(dgm)
siman analyse

* draw the graph
siman zipplot, bygr(row(1)) ///
	coveropt(lcol(blue)) noncoveropt(lcol(red)) ///
	xtitle(,size(large)) ytitle(,size(large)) ///
	xlabel(,labsize(large)) ylabel(,labsize(large)) ///
	legend(size(large)) subtitle(,size(huge)) ///
	xsize(9) ysize(5) 
graph rename zipplot, replace
graph export figure3.jpg

* now use graph editor to correct by-groups to "Correct", "Bias", "Low SE estimate"
