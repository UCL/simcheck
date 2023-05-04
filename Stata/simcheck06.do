/*
STEP 6. Make it easy to re-create any simulated data set
simcheck06.do
IW 18/01/2023
*/

// preliminaries
set rngstream 1
set seed 576819506
capture postclose simcheck06
capture postclose rngstates06 


// run simulation with 3 repetitions
local reps 3
local repsplus1 = `reps'+1
postfile simcheck06 int(rep) str8(method) float(b se) int(N) float(df) ///
	using simcheck06_postfile, replace
* The current rngstate cannot be stored as a single variable because it's ///
	>2048 characters, so we split it into substrings of length 2000
postfile rngstates06 int(rep) str2000(rngstate1 rngstate2) str1023(rngstate3) ///
	using rngstates06_postfile, replace

forvalues i=1/`repsplus1' {
	di as input "Repetition `i' of `reps'"
	
	di as input "Storing rngstates..."
	local rngstate1 = substr(c(rngstate),1,2000)
	local rngstate2 = substr(c(rngstate),2001,2000)
	local rngstate3 = substr(c(rngstate),4001,.)
	post rngstates06 (`i') ("`rngstate1'") ("`rngstate2'") ("`rngstate3'")
	
	if `i'>`reps' continue, break
	
	di as input "Generating data..."
	gendata, obs(500) logite(-1+C) logitd(-1+C) pmiss(.3)
	
	di as input "Analysing data..."
	anadata, rep(`i') post(simcheck06)
}

postclose simcheck06
postclose rngstates06


// view stored results for 3rd repetition
use simcheck06_postfile, clear
list if rep==3


// reconstruct data set for 3rd repetition and check it gives same results
use rngstates06_postfile, clear
set rng mt64s // needed if you run this code snippet separately
local rngstate = rngstate1[3]+rngstate2[3]+rngstate3[3] 
set rngstate `rngstate'
gendata, obs(500) logite(-1+C) logitd(-1+C) pmiss(.3)
anadata
// can verify that these results are the same

