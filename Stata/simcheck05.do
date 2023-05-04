/*
STEP 5. ANTICIPATE ANALYSIS FAILURES
simcheck05.do
IW 18/01/2023
*/

prog drop _all


// program to generate the data (unchanged)
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


// program to analyse the data (modified to capture errors)
program define anadata
    version 14
	syntax, [rep(int 0) post(string)]

	* Method 1: full data before data deletion
	capture noisily logit D E Ctrue 
    * If there is no error (_rc==0):
	if _rc==0 & !mi("`post'") post `post' (`rep') ("Full") (_b[E]) (_se[E]) (e(N)) (.)
    * If there is an error (_rc not 0), post empty results for this repetition
	if _rc>0 & !mi("`post'") post `post' (`rep') ("Full") (.) (.) (.) (.)

	* Method 2: CCA
	capture noisily logit D E Cobs 
	if _rc==0 & !mi("`post'") post `post' (`rep') ("CCA") (_b[E]) (_se[E]) (e(N)) (.)
	if _rc>0 & !mi("`post'") post `post' (`rep') ("CCA") (.) (.) (.) (.)

	* Method 3: MI
	mi set mlong
	mi register imputed Cobs
	capture noisily mi impute regress Cobs D##E, add(5) // expanded abbreviated commands
	if _rc==0 capture noisily mi estimate, post: logit D E Cobs 
	if _rc==0 & !mi("`post'") post `post' (`rep') ("MI") (_b[E]) (_se[E]) (e(N)) (e(df_mi)[1,"D:E"])
	if _rc>0 & !mi("`post'") post `post' (`rep') ("MI") (.) (.) (.) (.)
end


