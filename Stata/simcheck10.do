/*
STEP 10. DEAL WITH OUTLIERS
Uses Firth regression to avoid outliers.
simcheck10.do
IW 18/01/2023
*/


// Preliminaries
set rngstream 1
set seed 576819506
do simcheck05
capture postclose simcheck10
capture postclose rngstates10
prog drop anadata
ssc install firthlogit 


// Changed program to analyse the data
program define anadata
	version 14
	syntax, [rep(int 0) post(string)]

	* Method 1: full data before data deletion
	capture noisily firthlogit D E Ctrue
	if _rc==0 & !mi("`post'") post `post' (`rep') ("Full") (_b[E]) (_se[E]) (e(N)) (.)
	if _rc>0 & !mi("`post'") post `post' (`rep') ("Full") (.) (.) (.) (.)

	* Method 2: CCA
	capture noisily firthlogit D E Cobs
	if _rc==0 & !mi("`post'") post `post' (`rep') ("CCA") (_b[E]) (_se[E]) (e(N)) (.)
	if _rc>0 & !mi("`post'") post `post' (`rep') ("CCA") (.) (.) (.) (.)

	* Method 3: MI
	mi set mlong
	mi register imputed Cobs
	capture noisily mi impute regress Cobs D##E, add(5) 
	if _rc==0 capture noisily mi estimate, post cmdok: firthlogit D E Cobs
	if _rc==0 & !mi("`post'") post `post' (`rep') ("MI") (_b[E]) (_se[E]) (e(N)) (e(df_mi)[1,"xb:E"])
	if _rc>0 & !mi("`post'") post `post' (`rep') ("MI") (.) (.) (.) (.)

end


// Perform simulation (100 repetitions)
local reps 100
local repsplus1 = `reps'+1
postfile simcheck10 int(rep) str8(method) float(b se) int(N) float(df) ///
	using simcheck10_postfile, replace
postfile rngstates10 int(rep) str2000(rngstate1 rngstate2 rngstate3) ///
	using rngstates10_postfile, replace

forvalues i=1/`repsplus1' {
    if `i'==1 _dots 0 , title("Simulation running (`reps' repetitions)")
	_dots `i' 0

	local rngstate1 = substr(c(rngstate),1,2000)
	local rngstate2 = substr(c(rngstate),2001,2000)
	local rngstate3 = substr(c(rngstate),4001,.)
	post rngstates10 (`i') ("`rngstate1'") ("`rngstate2'") ("`rngstate3'")

	if `i'>`reps' continue, break
	
	quietly gendata, obs(500) logite(-4+C) logitd(-1+C) pmiss(.3)
	quietly anadata, rep(`i') post(simcheck10)
}
postclose simcheck10
postclose rngstates10


// Explore estimates dataset
use simcheck10_postfile, clear
label var b "Estimate"
label var se "Standard error"
scatter se b, by(method, row(1) note("")) jitter(1) xsize(9) ysize(5) name(simcheck10, replace)
