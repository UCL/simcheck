/*
A COMPLETE SUCCESSFUL SIMULATION STUDY
simcheck99.do
IW 18/01/2023
*/

//Preliminaries
prog drop _all
set seed 576819506
capture postclose simcheck99
capture postclose rngstates99


// Program to generate the data
program define gendata
	version 14
	syntax, obs(int) logite(string) logitd(string) pmiss(string)
	clear
	set obs `obs'
	drawnorm Ctrue
	gen E = runiform()<invlogit(`logite')
	gen D = runiform()<invlogit(`logitd')
	gen Cobs = Ctrue if runiform()>=`pmiss'

end


// Program to analyse the data
program define anadata
	version 14
	syntax, [rep(int 0) post(string) dgm(string)]

	* Method 1: full data before data deletion
	capture noisily logit D E Ctrue
	if _rc==0 & !mi("`post'") post `post' ("`dgm'") (`rep') ("Full") ///
		(_b[E]) (_se[E]) (e(N)) (.)
	if _rc>0 & !mi("`post'") post `post' ("`dgm'") (`rep') ("Full") ///
		(.) (.) (.) (.)

	* Method 2: CCA
	capture noisily logit D E Cobs 
	if _rc==0 & !mi("`post'") post `post' ("`dgm'") (`rep') ("CCA") ///
		(_b[E]) (_se[E]) (e(N)) (.)
	if _rc>0 & !mi("`post'") post `post' ("`dgm'") (`rep') ("CCA") ///
		(.) (.) (.) (.)

	* Method 3: MI
	mi set mlong
	mi register imputed Cobs
	capture noisily mi impute regress Cobs D##E, add(5)
	if _rc==0 capture noisily mi estimate, post: logit D E Cobs 
	if _rc==0 & !mi("`post'") post `post' ("`dgm'") (`rep') ("MI") ///
		(_b[E]) (_se[E]) (e(N)) (e(df_mi)[1,"D:E"])
	if _rc>0 & !mi("`post'") post `post' ("`dgm'") (`rep') ("MI") ///
		(.) (.) (.) (.)
end


// Perform simulation (1000 repetitions)
local reps 1000
local repsplus1 = `reps'+1
postfile simcheck99 str4(dgm) int(rep) str8(method) float(b se) int(N) float(df) ///
	using simcheck99_postfile, replace
postfile rngstates99 str4(dgm) int(rep) str2000(rngstate1 rngstate2 rngstate3) ///
	using rngstates99_postfile, replace

foreach dgm in MCAR MAR MNAR {
	di as text "DGM `dgm'"
	forvalues i=1/`repsplus1' {
		if `i'==1 _dots 0 , title("Simulation running (`reps' repetitions)")
		_dots `i' 0
		
		local rngstate1 = substr(c(rngstate),1,2000)
		local rngstate2 = substr(c(rngstate),2001,2000)
		local rngstate3 = substr(c(rngstate),4001,.)
		post rngstates99 ("`dgm'") (`i') ("`rngstate1'") ("`rngstate2'") ("`rngstate3'")
		
		if `i'>`reps' continue, break

		if "`dgm'"=="MCAR" local pmiss 0.3
		if "`dgm'"=="MAR" local pmiss .1+.2*E+.2*D
		if "`dgm'"=="MNAR" local pmiss .2+.2*Ctrue
		quietly gendata, obs(500) logite(-3+C) logitd(-1+C) pmiss(`pmiss')
		quietly anadata, rep(`i') post(simcheck99) dgm(`dgm')
	}
}
postclose simcheck99
postclose rngstates99


// Compute performance measures
use simcheck99_postfile, clear
sencode dgm, gen(dgmnum)
sencode method, gen(methodnum)
replace df = 1E7 if mi(df) // in case simsum version < 0.17.2

* scatterplot to explore estimates dataset
scatter se b, by(dgmnum methodnum)

simsum b, true(0) se(se) df(df) method(methodnum) id(rep) mcse by(dgmnum) ///
	clear bias empse relerror cover

* output results to excel
drop perfmeascode
label var dgmnum "DGM"
export excel using simcheck99out.xlsx, replace firstrow(varlabels)