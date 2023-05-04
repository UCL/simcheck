/*
STEP 7. COUNT AND UNDERSTAND METHOD FAILURES
Uses a sparser data generating mechanism.
simcheck07.do
IW 18/01/2023
*/

// Preliminaries
set rngstream 1
set seed 576819506
do simcheck05
capture postclose simcheck07
capture postclose rngstates07


// Run simulation study
local reps 100
local repsplus1 = `reps'+1
postfile simcheck07 int(rep) str8(method) float(b se) int(N) float(df) ///
	using simcheck07_postfile, replace
postfile rngstates07 int(rep) str2000(rngstate1 rngstate2 rngstate3) ///
	using rngstates07_postfile, replace

forvalues i=1/`repsplus1' {
    if `i'==1 _dots 0 , title("Simulation running (`reps' repetitions)")
	_dots `i' 0

	local rngstate1 = substr(c(rngstate),1,2000)
	local rngstate2 = substr(c(rngstate),2001,2000)
	local rngstate3 = substr(c(rngstate),4001,.)
	post rngstates07 (`i') ("`rngstate1'") ("`rngstate2'") ("`rngstate3'")

	if `i'>`reps' continue, break

	quietly gendata, obs(200) logite(-4+C) logitd(-4+C) pmiss(.3)
	quietly anadata, rep(`i') post(simcheck07)
}

postclose simcheck07
postclose rngstates07


// Inspect results for method failures
use simcheck07_postfile, clear
summ
list if missing(b)
list if rep==13


// Reconstruct one data set with method failures
use rngstates07_postfile, clear
set rng mt64s // needed if you run this code snippet separately
local rngstate = rngstate1[13]+rngstate2[13]+rngstate3[13]
set rngstate `rngstate'

gendata, obs(200) logite(-4+C) logitd(-4+C) pmiss(.3)

// and explore it
tab E D if !missing(Cobs)
cap noi logit D E Cobs
