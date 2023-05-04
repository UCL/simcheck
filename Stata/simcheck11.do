/*
STEP 11. CHECK MONTE CARLO ERRORS
Note use of a non-sparse data generating mechanism
simcheck11.do
IW 18/01/2023
*/

// Preliminaries
set rngstream 1
set seed 576819506
do simcheck05
capture postclose simcheck11
capture postclose rngstates11
ssc install simsum


// Perform simulation (100 repetitions)
local reps 100
local repsplus1 = `reps'+1
postfile simcheck11 int(rep) str8(method) float(b se) int(N) float(df) ///
	using simcheck11_postfile, replace
postfile rngstates11 rep str2000(rngstate1 rngstate2 rngstate3) ///
	using rngstates11_postfile, replace

forvalues i=1/`repsplus1' {
    if `i'==1 _dots 0 , title("Simulation running (`reps' repetitions)")
	_dots `i' 0
	local rngstate1 = substr(c(rngstate),1,2000)
	local rngstate2 = substr(c(rngstate),2001,2000)
	local rngstate3 = substr(c(rngstate),4001,.)
	post rngstates11 (`i') ("`rngstate1'") ("`rngstate2'") ("`rngstate3'")

	if `i'>`reps' continue, break

	quietly gendata, obs(500) logite(-3+C) logitd(-1+C) pmiss(.3)
	quietly anadata, rep(`i') post(simcheck11)
}
postclose simcheck11
postclose rngstates11


// Compute performance measures
use simcheck11_postfile, clear
replace df = 1E7 if mi(df) // in case simsum version < 0.17.2
simsum b, true(0) se(se) df(df) method(method) id(rep) mcse bias empse relerror cover
