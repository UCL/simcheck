/*
STEP 4. RUN A FEW REPETITIONS
simcheck04.do
IW 18/01/2023
*/

// preliminaries
set rngstream 1
set seed 576819506


// run 3 repetitions
capture postclose simcheck04
postfile simcheck04 int(rep) str8(method) float(b se) int(N) float(df) using simcheck04_postfile, replace
forvalues i=1/3 {
	gendata, obs(500) logite(-1+C) logitd(-1+C) pmiss(.3)
	anadata, rep(`i') post(simcheck04)
}
postclose simcheck04


// view results
use simcheck04_postfile, clear
list, sepby(rep)


