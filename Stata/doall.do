// simcheck: do all Stata files
// IW 6sep2023

* set local path - change for other users
cd C:\ian\git\simcheck\Stata 

* do all files
foreach i in 02 03 04 05 06 07 08 08figure 09 10 11 12 99 {
	do simcheck`i'
}
do simzipplot

/* The following files are created by the code, 
   but are not included in the repository:
	Figure2.jpg
	Figure3.jpg
	rngstates06_postfile.dta
	rngstates07_postfile.dta
	rngstates10_postfile.dta
	rngstates11_postfile.dta
	rngstates12_postfile.dta
	rngstates99_postfile.dta
	simcheck04_postfile.dta
	simcheck06_postfile.dta
	simcheck07_postfile.dta
	simcheck08figure.do
	simcheck08figure.gph
	simcheck08figure.log
	simcheck10_postfile.dta
	simcheck11_postfile.dta
	simcheck12_base.dta
	simcheck12_postfile.dta
	simcheck99_postfile.dta
	simcheck99out.xlsx
*/