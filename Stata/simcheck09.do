/*
STEP 9. UNDERSTAND OUTLIERS
Note use of the asis option on the CCA
simcheck09.do
IW 18/01/2023
*/


// Explore estimates dataset
use simcheck07_postfile, clear
sencode method, gen(methnum)
label var b "Estimate"
label var se "Standard error"


// Find where se=0 occurs
summ rep if se==0
list if rep==1


// Reconstruct a data set with se==0
use rngstates07_postfile, clear
local rngstate = rngstate1[13]+rngstate2[13]+rngstate3[13] 
set rng mt64s
set rngstate `rngstate'
gendata, obs(200) logite(-4+C) logitd(-4+C) pmiss(.3)


// Explore it
tab E D if !missing(Cobs)
cap noi logit D E Cobs
