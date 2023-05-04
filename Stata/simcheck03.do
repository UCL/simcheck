/*
STEP 3. GENERATE & ANALYSE A SINGLE DATA SET
simcheck03.do
IW 18/01/2023
*/


// preliminaries
set rngstream 1
set seed 576819506


// generate a single large data set
gendata, obs(100000) logite(-1+C) logitd(-1+C) pmiss(.3)


// summarise the data
summ
tab D E if mi(Cobs)
tab D E if !mi(Cobs)
logit D E


// fit the data generating models
logit E Ctrue
logit D Ctrue


// analyse the data
anadata


