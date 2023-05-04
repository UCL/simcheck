/*
STEP 12. WHY ARE MODEL-BASED SEs WRONG?
This program uses the same complete data for each repetition.
The true value is taken as the value in the complete data.
simcheck12.do
IW 18/01/2023
*/

// Preliminaries
prog drop _all
set seed 576819506
capture postclose simcheck12
capture postclose rngstates12


// Program to generate the complete data
program define gendata1
	version 14
	syntax, obs(int) logite(string) logitd(string)
	clear
	set obs `obs'
	drawnorm Ctrue
	gen E = runiform()<invlogit(`logite')
	gen D = runiform()<invlogit(`logitd')
end
	
	
// Program to impose the missing data
program define gendata2
	version 14
	syntax, pmiss(string)
	gen Cobs = Ctrue if runiform()>=`pmiss'
end


// Program to analyse the data
program define anadata
	version 14
	syntax, [rep(int 0) post(string)]

	* Method 2: CCA
	capture noisily logit D E Cobs 
	if _rc==0 & !mi("`post'") post `post' (`rep') ("CCA") (_b[E]) (_se[E]) (e(N)) (.)
	if _rc>0 & !mi("`post'") post `post' (`rep') ("CCA") (.) (.) (.) (.)

	* Method 3: MI
	mi set mlong
	mi register imputed Cobs
	capture noisily mi impute regress Cobs D##E, add(5)
	if _rc==0 capture noisily mi estimate, post: logit D E Cobs 
	if _rc==0 & !mi("`post'") post `post' (`rep') ("MI") (_b[E]) (_se[E]) (e(N)) (e(df_mi)[1,"D:E"])
	if _rc>0 & !mi("`post'") post `post' (`rep') ("MI") (.) (.) (.) (.)
end


// Create complete data and find true value 
quietly gendata1, obs(500) logite(-3+C) logitd(-1+C) 
save simcheck12_base, replace
logit D E Ctrue
local true = _b[E]


// Perform simulation (1000 repetitions)
local reps 1000
local repsplus1 = `reps'+1
postfile simcheck12 int(rep) str8(method) float(b se) int(N) float(df) ///
	using simcheck12_postfile, replace
postfile rngstates12 int(rep) str2000(rngstate1 rngstate2 rngstate3) ///
	using rngstates12_postfile, replace

forvalues i=1/`repsplus1' {
    if `i'==1 _dots 0 , title("Simulation running (`reps' repetitions)")
	_dots `i' 0
	
	local rngstate1 = substr(c(rngstate),1,2000)
	local rngstate2 = substr(c(rngstate),2001,2000)
	local rngstate3 = substr(c(rngstate),4001,.)

	if `i'>`reps' continue, break

	post rngstates12 (`i') ("`rngstate1'") ("`rngstate2'") ("`rngstate3'")
	use simcheck12_base, clear
	quietly gendata2, pmiss(0.3)
	quietly anadata, rep(`i') post(simcheck12)
}
postclose simcheck12
postclose rngstates12


// Compute performance measures
use simcheck12_postfile, clear
replace df = 1E7 if mi(df) // in case simsum version < 0.17.2
simsum b, true(`true') se(se) df(df) method(method) id(rep) mcse bias empse relerror cover