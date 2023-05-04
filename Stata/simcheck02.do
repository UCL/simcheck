/*
STEP 2. WRITE CODE THAT SEPARATES DGM FROM ANALYSIS.
simcheck02.do
IW 18/01/2023
*/

prog drop _all


// program to generate the data
program define gendata
	version 14
	syntax, obs(int) logite(string) logitd(string) pmiss(string)
	clear
	set obs `obs'
	drawnorm Ctrue
	gen E = runiform() < invlogit(`logite')
	gen D = runiform() < invlogit(`logitd')
	gen Cobs = Ctrue if runiform()>=`pmiss'
end


// program to analyse the data
program define anadata
	version 14
	syntax, [rep(int 0) post(string)]

	* Method 1: full data before data deletion
	logit D E Ctrue
	if !mi("`post'") post `post' (`rep') ("Full") (_b[E]) (_se[E]) (e(N)) (.)
	* we are posting:            rep     method   est     se         N      df
	* df is only needed for MI but must be included for all

	* Method 2: CCA
	logit D E Cobs 
	if !mi("`post'") post `post' (`rep') ("CCA") (_b[E]) (_se[E]) (e(N)) (.)

	* Method 3: MI
	mi set mlong
	mi register imputed Cobs
	mi impute regress Cobs D##E, add(5)
	mi estimate, post: logit D E Cobs 
	if !mi("`post'") post `post' (`rep') ("MI") (_b[E]) (_se[E]) (e(N)) (e(df_mi)[1,"D:E"])
end


