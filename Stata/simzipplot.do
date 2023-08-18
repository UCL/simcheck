/*
simzipplot.do
Illustrative zip plots for point 13
IW 18/4/2023 & 4/5/2023
Revised 18/8/2023 taking advantage of siman zipplot v1.8.5
Run in same folder as simcheck99_postfile.dta
*/

prog drop _all

// CREATE 3 HYPOTHETICAL SETS OF SIMULATION STUDY RESULTS
use simcheck99_postfile, clear
sencode dgm, replace
keep if method=="Full"
keep if dgm==1
drop method N dgm
expand 3
sort rep
by rep: gen type=_n
replace b=b-0.5 if type==2
replace se=se*0.8 if type==3
label def type 1 "Correct coverage" 2 "Negative bias" 3 "Low standard error"
label val type type

// CREATE A ZIPPLOT
* install siman, if required
net install siman, from(https://raw.githubusercontent.com/UCL/siman/master/) replace
which siman_zipplot

* compute performance measures
siman setup, true(0) est(b) se(se) df(df) method(type) rep(rep)
siman analyse
set scheme mrc
* draw the graph
siman zipplot, bygr(row(1) note("")) ///
	coveropt(lcol(blue)) noncoveropt(lcol(red)) ///
	xtitle(,size(large)) ytitle(,size(large)) ///
	xlabel(,labsize(large)) ylabel(,labsize(large)) ///
	legend(size(large)) subtitle(,size(huge)) ///
	xsize(9) ysize(5) subtitle(,just(center)) name(zipplot, replace)
graph export Figure3.jpg, replace

