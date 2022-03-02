* IMPORTANT - WINDOWS USERS
* EXTRACT THE ZIP FILE IN C:\ 

* COMMAND TO INSTALL CLEMAO PACKAGE (UNIT ROOT TEST)

ssc install clemao_io

********************************************************************************
**************************             AUD              ************************
********************************************************************************

clear all

log using "AUD-resultats_`c(current_date)'.log", replace

capture log

set more off

global data "C:\Data and command"

cd "$data\"

import excel "C:\Data and command\Data.xlsx", sheet("AUD") firstrow

format Obs %tw

tsset Obs

set matsize 11000

g CT = Long/Short if Long>0|Short>0

mvencode CT, mv(0)

g IRD = IR - USIR

gen CTF = Short/Long

mvencode CTF, mv(0)

gen IRDF = USIR - IR

global FCvar CTF ER IRDF VIX SM SMUS

global levelvar CT ER IRD VIX SM SMUS

*Generate time dummy, following Chari et al., 2017, p. 12

*ME dummy is equal to one from the period 22 December 2008 (when the U.S.
*> interest rate reached the ZLB, i.e. 0.125) to April 2013.

gen ME=(Obs<=364)

*TAPER dummy is equal to one from the period May 2013 (wherein Ben Bernanke
*> first mentioned the possibility of tapering LSAP purchases).

gen TAPER=(Obs>=228 & Obs<=364)

*MTT dummy is equal to one when IRD>0, i.e. IR is larger than USIR (during MT).

gen MTT=(Obs>=365 & Obs<=482)

*MTF dummy is equal to one when IRD<0, i.e. IR is larger than USIR (during MT).

gen MTF=(Obs>=483)

***Unit root tests

** CLEMAO*****

*Ho: The variable has unit root (non-stationary)
*Reject if the t-statistic of (rho -1) is larger than the critical value of 5%
**Example in Baum(2005): t-statistic is -2.530 and the critical value is -3.56
***t>ct (CANNOT REJECT)

*ME

clemao1 CT if ME==1
putexcel B2=rscalars using AUD-results, sheet("CLEMAO") modify

clemao1 ER if ME==1
putexcel C2=rscalars using AUD-results, sheet("CLEMAO") modify

clemao1 IRD if ME==1
putexcel D2=rscalars using AUD-results, sheet("CLEMAO") modify

clemao1 VIX if ME==1
putexcel E2=rscalars using AUD-results, sheet("CLEMAO") modify

clemao1 SM if ME==1
putexcel F2=rscalars using AUD-results, sheet("CLEMAO") modify

clemao1 SMUS if ME==1
putexcel G2=rscalars using AUD-results, sheet("CLEMAO") modify

*MTT

clemao1 CT if MTT==1
putexcel B10=rscalars using AUD-results, sheet("CLEMAO") modify

clemao1 ER if MTT==1
putexcel C10=rscalars using AUD-results, sheet("CLEMAO") modify

clemao1 IRD if MTT==1
putexcel D10=rscalars using AUD-results, sheet("CLEMAO") modify

clemao1 VIX if MTT==1
putexcel E10=rscalars using AUD-results, sheet("CLEMAO") modify

clemao1 SM if MTT==1
putexcel F10=rscalars using AUD-results, sheet("CLEMAO") modify

clemao1 SMUS if MTT==1
putexcel G10=rscalars using AUD-results, sheet("CLEMAO") modify

*MTF

clemao1 CTF if MTF==1
putexcel B18=rscalars using AUD-results, sheet("CLEMAO") modify

clemao1 ER if MTF==1
putexcel C18=rscalars using AUD-results, sheet("CLEMAO") modify

clemao1 IRDF if MTF==1
putexcel D18=rscalars using AUD-results, sheet("CLEMAO") modify

clemao1 VIX if MTF==1
putexcel E18=rscalars using AUD-results, sheet("CLEMAO") modify

clemao1 SM if MTF==1
putexcel F18=rscalars using AUD-results, sheet("CLEMAO") modify

clemao1 SMUS if MTF==1
putexcel G18=rscalars using AUD-results, sheet("CLEMAO") modify

*Descriptive statistics

set more off

su CT if ME==1, detail 
putexcel B3=rscalars using AUD-results, sheet("DESC STATS") modify

su ER if ME==1, detail 
putexcel C3=rscalars using AUD-results, sheet("DESC STATS") modify

su IRD if ME==1, detail 
putexcel D3=rscalars using AUD-results, sheet("DESC STATS") modify

su VIX if ME==1, detail 
putexcel E3=rscalars using AUD-results, sheet("DESC STATS") modify

su SM if ME==1, detail 
putexcel F3=rscalars using AUD-results, sheet("DESC STATS") modify

su SMUS if ME==1, detail 
putexcel G3=rscalars using AUD-results, sheet("DESC STATS") modify

su CT if MTT==1, detail 
putexcel H3=rscalars using AUD-results, sheet("DESC STATS") modify

su ER if MTT==1, detail 
putexcel I3=rscalars using AUD-results, sheet("DESC STATS") modify

su IRD if MTT==1, detail 
putexcel J3=rscalars using AUD-results, sheet("DESC STATS") modify

su VIX if MTT==1, detail 
putexcel K3=rscalars using AUD-results, sheet("DESC STATS") modify

su SM if MTT==1, detail 
putexcel L3=rscalars using AUD-results, sheet("DESC STATS") modify

su SMUS if MTT==1, detail 
putexcel M3=rscalars using AUD-results, sheet("DESC STATS") modify

su CTF if MTF==1, detail 
putexcel N3=rscalars using AUD-results, sheet("DESC STATS") modify

su ER if MTF==1, detail 
putexcel O3=rscalars using AUD-results, sheet("DESC STATS") modify

su IRDF if MTF==1, detail 
putexcel P3=rscalars using AUD-results, sheet("DESC STATS") modify

su VIX if MTF==1, detail 
putexcel Q3=rscalars using AUD-results, sheet("DESC STATS") modify

su SM if MTF==1, detail 
putexcel R3=rscalars using AUD-results, sheet("DESC STATS") modify

su SMUS if MTF==1, detail 
putexcel S3=rscalars using AUD-results, sheet("DESC STATS") modify

********************************************************************************
*** VAR

*********************************** 
****ME
*********************************** 

set more off
foreach i of varlist $levelvar {
   forvalues j=1/11 {
    gen `i'_`j'ME=l`j'.`i' if ME==1
  }
}
quietly: var CT ER IRD VIX SM SMUS if ME==1, exog(TAPER) lags(1/10) small lutstats

varlmar, mlag(10)
putexcel B3=matrix(r(lm)) using AUD-results, sheet("ROBUST") modify

varsoc
putexcel B3=matrix(r(stats)) using AUD-results, sheet("LAG-LENGTH") modify

quietly: var CT ER IRD VIX SM SMUS if ME==1, exog(TAPER IRD_3ME ER_3ME SM_3ME SMUS_3ME) lags(1/2) small lutstats

vargranger
putexcel C3=matrix(r(gstats)) using AUD-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\AUD-ME-Stability.pdf", as(pdf) replace

*********************************** 
****MTT
*********************************** 

set more off
foreach i of varlist $levelvar {
   forvalues j=1/10 {
    gen `i'_`j'MTT=l`j'.`i' if MTT==1
  }
}
quietly: var CT ER IRD VIX SM SMUS if MTT==1, lags(1/10) small lutstats

varlmar, mlag(10)
putexcel G3=matrix(r(lm)) using AUD-results, sheet("ROBUST") modify

varsoc
putexcel M3=matrix(r(stats)) using AUD-results, sheet("LAG-LENGTH") modify

quietly: var CT ER IRD VIX SM SMUS if MTT==1, exog(IRD_9MTT CT_9MTT ER_9MTT SMUS_9MTT) lags(1/8) small lutstats

vargranger
putexcel J3=matrix(r(gstats)) using AUD-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\AUD-MTT-Stability.pdf", as(pdf) replace

*********************************** 
****MTF
*********************************** 

set more off
foreach i of varlist $FCvar {
   forvalues j=1/7 {
    gen `i'_`j'MTF=l`j'.`i' if MTF==1
  }
}
quietly: var CTF ER IRDF VIX SM SMUS if MTF==1, lags(1/6) small lutstats

varlmar, mlag(6)
putexcel L3=matrix(r(lm)) using AUD-results, sheet("ROBUST") modify

varsoc
putexcel X3=matrix(r(stats)) using AUD-results, sheet("LAG-LENGTH") modify

quietly: var CTF ER IRDF VIX SM SMUS if MTF==1, exog(IRDF_2MTF CTF_2MTF SM_2MTF SMUS_2MTF) lags(1) small lutstats

vargranger
putexcel Q3=matrix(r(gstats)) using AUD-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\AUD-MTF-Stability.pdf", as(pdf) replace

save AUD-Granger.dta, replace

saveold AUD-Granger_stata13.dta, replace

log close

********************************************************************************
**************************             BRL              ************************
********************************************************************************

clear all

log using "BRL-resultats_`c(current_date)'.log", replace

capture log

set more off

global data "C:\Data and command"

cd "$data\"

import excel "C:\Data and command\Data.xlsx", sheet("BRL") firstrow

format Obs %tw

tsset Obs

set matsize 11000

g CT = Long/Short if Long>0|Short>0

mvencode CT, mv(0)

g IRD = IR - USIR

global levelvar CT ER IRD VIX SM SMUS

*Generate time dummy, following Chari et al., 2017, p. 12

*ME dummy is equal to one from the period 22 December 2008 (when the U.S.
*> interest rate reached the ZLB, i.e. 0.125) to April 2013.

gen ME=(Obs<=94)

*MT dummy is equal to one from the period 12/22/2015 onwards (hike in the IR).

gen MT=(Obs>=95)

***Unit root tests

** CLEMAO*****

*Ho: The variable has unit root (non-stationary)
*Reject if the t-statistic of (rho -1) is larger than the critical value of 5%
**Example in Baum(2005): t-statistic is -2.530 and the critical value is -3.56
***t>ct (CANNOT REJECT)

*ME

clemao1 CT if ME==1
putexcel B2=rscalars using BRL-results, sheet("CLEMAO") modify

clemao1 ER if ME==1
putexcel C2=rscalars using BRL-results, sheet("CLEMAO") modify

clemao1 IRD if ME==1
putexcel D2=rscalars using BRL-results, sheet("CLEMAO") modify

clemao1 VIX if ME==1
putexcel E2=rscalars using BRL-results, sheet("CLEMAO") modify

clemao1 SM if ME==1
putexcel F2=rscalars using BRL-results, sheet("CLEMAO") modify

clemao1 SMUS if ME==1
putexcel G2=rscalars using BRL-results, sheet("CLEMAO") modify

*MT

clemao1 CT if MT==1
putexcel B10=rscalars using BRL-results, sheet("CLEMAO") modify

clemao1 ER if MT==1
putexcel C10=rscalars using BRL-results, sheet("CLEMAO") modify

clemao1 IRD if MT==1
putexcel D10=rscalars using BRL-results, sheet("CLEMAO") modify

clemao1 VIX if MT==1
putexcel E10=rscalars using BRL-results, sheet("CLEMAO") modify

clemao1 SM if MT==1
putexcel F10=rscalars using BRL-results, sheet("CLEMAO") modify

clemao1 SMUS if MT==1
putexcel G10=rscalars using BRL-results, sheet("CLEMAO") modify

*Descriptive statistics

set more off

su CT if ME==1, detail 
putexcel B3=rscalars using BRL-results, sheet("DESC STATS") modify

su ER if ME==1, detail 
putexcel C3=rscalars using BRL-results, sheet("DESC STATS") modify

su IRD if ME==1, detail 
putexcel D3=rscalars using BRL-results, sheet("DESC STATS") modify

su VIX if ME==1, detail 
putexcel E3=rscalars using BRL-results, sheet("DESC STATS") modify

su SM if ME==1, detail 
putexcel F3=rscalars using BRL-results, sheet("DESC STATS") modify

su SMUS if ME==1, detail 
putexcel G3=rscalars using BRL-results, sheet("DESC STATS") modify

su CT if MT==1, detail 
putexcel H3=rscalars using BRL-results, sheet("DESC STATS") modify

su ER if MT==1, detail 
putexcel I3=rscalars using BRL-results, sheet("DESC STATS") modify

su IRD if MT==1, detail 
putexcel J3=rscalars using BRL-results, sheet("DESC STATS") modify

su VIX if MT==1, detail 
putexcel K3=rscalars using BRL-results, sheet("DESC STATS") modify

su SM if MT==1, detail 
putexcel L3=rscalars using BRL-results, sheet("DESC STATS") modify

su SMUS if MT==1, detail 
putexcel M3=rscalars using BRL-results, sheet("DESC STATS") modify

********************************************************************************
*** VAR

*********************************** 
****ME
*********************************** 

set more off
foreach i of varlist $levelvar {
   forvalues j=1/11 {
    gen `i'_`j'ME=l`j'.`i' if ME==1
  }
}
quietly: var CT ER IRD VIX SM SMUS if ME==1, lags(1/10) small lutstats

varlmar, mlag(10)
putexcel B3=matrix(r(lm)) using BRL-results, sheet("ROBUST") modify

varsoc
putexcel B3=matrix(r(stats)) using BRL-results, sheet("LAG-LENGTH") modify

quietly: var CT ER IRD VIX SM SMUS if ME==1, exog(CT_5ME IRD_5ME ER_5ME SM_5ME) lags(1/4) small lutstats

vargranger
putexcel C3=matrix(r(gstats)) using BRL-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\BRL-ME-Stability.pdf", as(pdf) replace

*********************************** 
****MT
*********************************** 

set more off
foreach i of varlist $levelvar {
   forvalues j=1/11 {
    gen `i'_`j'MT=l`j'.`i' if MT==1
  }
}
quietly: var CT ER IRD VIX SM SMUS if MT==1, lags(1/10) small lutstats

varlmar, mlag(10)
putexcel G3=matrix(r(lm)) using BRL-results, sheet("ROBUST") modify

varsoc
putexcel M3=matrix(r(stats)) using BRL-results, sheet("LAG-LENGTH") modify

quietly: var CT ER IRD VIX SM SMUS if MT==1, exog(IRD_6MT VIX_6MT CT_6MT ER_6MT SM_6MT SMUS_6MT) lags(1/5) small lutstats

vargranger
putexcel J3=matrix(r(gstats)) using BRL-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\BRL-MT-Stability.pdf", as(pdf) replace

save BRL-Granger.dta, replace

saveold BRL-Granger_stata13.dta, replace

log close

********************************************************************************
**************************             CAD              ************************
********************************************************************************

clear all

log using "CAD-resultats_`c(current_date)'.log", replace

capture log

set more off

global data "C:\Data and command"

cd "$data\"

import excel "C:\Data and command\Data.xlsx", sheet("CAD") firstrow

format Obs %tw

tsset Obs

set matsize 11000

g CT = Long/Short if Long>0|Short>0

mvencode CT, mv(0)

g IRD = IR - USIR

gen CTF = Short/Long

mvencode CTF, mv(0)

gen IRDF = USIR - IR

global FCvar CTF ER IRDF VIX SM SMUS

global levelvar CT ER IRD VIX SM SMUS

*Generate time dummy, following Chari et al., 2017, p. 12

*ME dummy is equal to one from the period 22 December 2008 (when the U.S.
*> interest rate reached the ZLB, i.e. 0.125) to April 2013.

gen ME=(Obs<=364)

*TAPER dummy is equal to one from the period May 2013 (wherein Ben Bernanke
*> first mentioned the possibility of tapering LSAP purchases).

gen TAPER=(Obs>=228 & Obs<=364)

*MT dummy is equal to one from the period 12/22/2015 onwards (hike in the IR).

gen MT=(Obs>=365)

***Unit root tests

** CLEMAO*****

*Ho: The variable has unit root (non-stationary)
*Reject if the t-statistic of (rho -1) is larger than the critical value of 5%
**Example in Baum(2005): t-statistic is -2.530 and the critical value is -3.56
***t>ct (CANNOT REJECT)

*ME

clemao1 CT if ME==1
putexcel B2=rscalars using CAD-results, sheet("CLEMAO") modify

clemao1 ER if ME==1
putexcel C2=rscalars using CAD-results, sheet("CLEMAO") modify

clemao1 IRD if ME==1
putexcel D2=rscalars using CAD-results, sheet("CLEMAO") modify

clemao1 VIX if ME==1
putexcel E2=rscalars using CAD-results, sheet("CLEMAO") modify

clemao1 SM if ME==1
putexcel F2=rscalars using CAD-results, sheet("CLEMAO") modify

clemao1 SMUS if ME==1
putexcel G2=rscalars using CAD-results, sheet("CLEMAO") modify

*MT

clemao1 CT if MT==1
putexcel B10=rscalars using CAD-results, sheet("CLEMAO") modify

clemao1 ER if MT==1
putexcel C10=rscalars using CAD-results, sheet("CLEMAO") modify

clemao1 IRD if MT==1
putexcel D10=rscalars using CAD-results, sheet("CLEMAO") modify

clemao1 VIX if MT==1
putexcel E10=rscalars using CAD-results, sheet("CLEMAO") modify

clemao1 SM if MT==1
putexcel F10=rscalars using CAD-results, sheet("CLEMAO") modify

clemao1 SMUS if MT==1
putexcel G10=rscalars using CAD-results, sheet("CLEMAO") modify

*Descriptive statistics

set more off

su CT if ME==1, detail 
putexcel B3=rscalars using CAD-results, sheet("DESC STATS") modify

su ER if ME==1, detail 
putexcel C3=rscalars using CAD-results, sheet("DESC STATS") modify

su IRD if ME==1, detail 
putexcel D3=rscalars using CAD-results, sheet("DESC STATS") modify

su VIX if ME==1, detail 
putexcel E3=rscalars using CAD-results, sheet("DESC STATS") modify

su SM if ME==1, detail 
putexcel F3=rscalars using CAD-results, sheet("DESC STATS") modify

su SMUS if ME==1, detail 
putexcel G3=rscalars using CAD-results, sheet("DESC STATS") modify

su CTF if MT==1, detail 
putexcel H3=rscalars using CAD-results, sheet("DESC STATS") modify

su ER if MT==1, detail 
putexcel I3=rscalars using CAD-results, sheet("DESC STATS") modify

su IRDF if MT==1, detail 
putexcel J3=rscalars using CAD-results, sheet("DESC STATS") modify

su VIX if MT==1, detail 
putexcel K3=rscalars using CAD-results, sheet("DESC STATS") modify

su SM if MT==1, detail 
putexcel L3=rscalars using CAD-results, sheet("DESC STATS") modify

su SMUS if MT==1, detail 
putexcel M3=rscalars using CAD-results, sheet("DESC STATS") modify

********************************************************************************
*** VAR

*********************************** 
****ME
*********************************** 

set more off
foreach i of varlist $levelvar {
   forvalues j=1/11 {
    gen `i'_`j'ME=l`j'.`i' if ME==1
  }
}
quietly: var CT ER IRD VIX SM SMUS if ME==1, exog(TAPER) lags(1/10) small lutstats

varlmar, mlag(10)
putexcel B3=matrix(r(lm)) using CAD-results, sheet("ROBUST") modify

*SECOND STEP

varsoc
putexcel B3=matrix(r(stats)) using CAD-results, sheet("LAG-LENGTH") modify

quietly: var CT ER IRD VIX SM SMUS if ME==1, exog(TAPER IRD_3ME ER_3ME SM_3ME SMUS_3ME) lags(1/2) small lutstats

vargranger
putexcel C3=matrix(r(gstats)) using CAD-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\CAD-ME-Stability.pdf", as(pdf) replace

*********************************** 
****MT
*********************************** 

set more off
foreach i of varlist $FCvar {
   forvalues j=1/11 {
    gen `i'_`j'MT=l`j'.`i' if MT==1
  }
}
set more off

quietly: var CTF ER IRDF VIX SM SMUS if MT==1, lags(1/10) small lutstats

varlmar, mlag(10)
putexcel G3=matrix(r(lm)) using CAD-results, sheet("ROBUST") modify

varsoc
putexcel M3=matrix(r(stats)) using CAD-results, sheet("LAG-LENGTH") modify

quietly: var IRDF VIX CTF ER SMUS SM if MT==1, exog(IRDF_7MT VIX_7MT CTF_7MT ER_7MT SM_7MT SMUS_7MT) lags(1/6) small lutstats

vargranger
putexcel J3=matrix(r(gstats)) using CAD-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\CAD-MT-Stability.pdf", as(pdf) replace

save CAD-Granger.dta, replace

saveold CAD-Granger_stata13.dta, replace

log close

********************************************************************************
**************************             EUR              ************************
********************************************************************************

clear all

log using "EUR-resultats_`c(current_date)'.log", replace

capture log

set more off

global data "C:\Data and command"

cd "$data\"

import excel "C:\Data and command\Data.xlsx", sheet("EUR") firstrow

format Obs %tw

tsset Obs

set matsize 11000

g CT = Long/Short if Long>0|Short>0

mvencode CT, mv(0)

g IRD = IR - USIR

gen CTF = Short/Long

mvencode CTF, mv(0)

gen IRDF = USIR - IR

global FCvar CTF ER IRDF VIX SM SMUS

global levelvar CT ER IRD VIX SM SMUS

*Generate time dummy, following Chari et al., 2017, p. 12

*ME dummy is equal to one from the period 22 December 2008 (when the U.S.
*> interest rate reached the ZLB, i.e. 0.125) to April 2013.

gen ME=(Obs<=364)

*TAPER dummy is equal to one from the period May 2013 (wherein Ben Bernanke
*> first mentioned the possibility of tapering LSAP purchases).

gen TAPER=(Obs>=228 & Obs<=364)

*MT dummy is equal to one from the period 12/22/2015 onwards (hike in the IR).

gen MT=(Obs>=365)

***Unit root tests

** CLEMAO*****

*Ho: The variable has unit root (non-stationary)
*Reject if the t-statistic of (rho -1) is larger than the critical value of 5%
**Example in Baum(2005): t-statistic is -2.530 and the critical value is -3.56
***t>ct (CANNOT REJECT)

*ME

clemao1 CT if ME==1
putexcel B2=rscalars using EUR-results, sheet("CLEMAO") modify

clemao1 ER if ME==1
putexcel C2=rscalars using EUR-results, sheet("CLEMAO") modify

clemao1 IRD if ME==1
putexcel D2=rscalars using EUR-results, sheet("CLEMAO") modify

clemao1 VIX if ME==1
putexcel E2=rscalars using EUR-results, sheet("CLEMAO") modify

clemao1 SM if ME==1
putexcel F2=rscalars using EUR-results, sheet("CLEMAO") modify

clemao1 SMUS if ME==1
putexcel G2=rscalars using EUR-results, sheet("CLEMAO") modify

*MT

clemao1 CTF if MT==1
putexcel B10=rscalars using EUR-results, sheet("CLEMAO") modify

clemao1 ER if MT==1
putexcel C10=rscalars using EUR-results, sheet("CLEMAO") modify

clemao1 IRDF if MT==1
putexcel D10=rscalars using EUR-results, sheet("CLEMAO") modify

clemao1 VIX if MT==1
putexcel E10=rscalars using EUR-results, sheet("CLEMAO") modify

clemao1 SM if MT==1
putexcel F10=rscalars using EUR-results, sheet("CLEMAO") modify

clemao1 SMUS if MT==1
putexcel G10=rscalars using EUR-results, sheet("CLEMAO") modify

*Descriptive statistics

set more off

su CT if ME==1, detail 
putexcel B3=rscalars using EUR-results, sheet("DESC STATS") modify

su ER if ME==1, detail 
putexcel C3=rscalars using EUR-results, sheet("DESC STATS") modify

su IRD if ME==1, detail 
putexcel D3=rscalars using EUR-results, sheet("DESC STATS") modify

su VIX if ME==1, detail 
putexcel E3=rscalars using EUR-results, sheet("DESC STATS") modify

su SM if ME==1, detail 
putexcel F3=rscalars using EUR-results, sheet("DESC STATS") modify

su SMUS if ME==1, detail 
putexcel G3=rscalars using EUR-results, sheet("DESC STATS") modify

su CTF if MT==1, detail 
putexcel H3=rscalars using EUR-results, sheet("DESC STATS") modify

su ER if MT==1, detail 
putexcel I3=rscalars using EUR-results, sheet("DESC STATS") modify

su IRDF if MT==1, detail 
putexcel J3=rscalars using EUR-results, sheet("DESC STATS") modify

su VIX if MT==1, detail 
putexcel K3=rscalars using EUR-results, sheet("DESC STATS") modify

su SM if MT==1, detail 
putexcel L3=rscalars using EUR-results, sheet("DESC STATS") modify

su SMUS if MT==1, detail 
putexcel M3=rscalars using EUR-results, sheet("DESC STATS") modify

********************************************************************************
*** VAR

*********************************** 
****ME
*********************************** 

set more off
foreach i of varlist $levelvar {
   forvalues j=1/11 {
    gen `i'_`j'ME=l`j'.`i' if ME==1
  }
}
quietly: var CT ER IRD VIX SM SMUS if ME==1, exog(TAPER) lags(1/10) small lutstats

varlmar, mlag(10)
putexcel B3=matrix(r(lm)) using EUR-results, sheet("ROBUST") modify

varsoc
putexcel B3=matrix(r(stats)) using EUR-results, sheet("LAG-LENGTH") modify

quietly: var CT ER IRD VIX SM SMUS if ME==1, exog(TAPER IRD_2ME VIX_2ME CT_2ME ER_2ME SM_2ME SMUS_2ME) lags(1) small lutstats

vargranger
putexcel C3=matrix(r(gstats)) using EUR-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\EUR-ME-Stability.pdf", as(pdf) replace

*********************************** 
****MT
*********************************** 

set more off
foreach i of varlist $FCvar {
   forvalues j=1/11 {
    gen `i'_`j'MT=l`j'.`i' if MT==1
  }
}
quietly: var CTF ER IRDF VIX SM SMUS if MT==1, lags(1/10) small lutstats

varlmar, mlag(10)
putexcel G3=matrix(r(lm)) using EUR-results, sheet("ROBUST") modify

varsoc
putexcel M3=matrix(r(stats)) using EUR-results, sheet("LAG-LENGTH") modify

quietly: var CTF ER IRDF VIX SM SMUS if MT==1, exog(IRDF_8MT ER_8MT SMUS_8MT) lags(1/7) small lutstats

vargranger
putexcel J3=matrix(r(gstats)) using EUR-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\EUR-MT-Stability.pdf", as(pdf) replace

save EUR-Granger.dta, replace

saveold EUR-Granger_stata13.dta, replace

log close

********************************************************************************
**************************             JPY              ************************
********************************************************************************

clear all

log using "JPY-resultats_`c(current_date)'.log", replace

capture log

set more off

global data "C:\Data and command"

cd "$data\"

import excel "C:\Data and command\Data.xlsx", sheet("JPY") firstrow

format Obs %tw

tsset Obs

set matsize 11000

g CT = Long/Short if Long>0|Short>0

mvencode CT, mv(0)

g IRD = IR - USIR

gen CTF = Short/Long

mvencode CTF, mv(0)

gen IRDF = USIR - IR

global FCvar CTF ER IRDF VIX SM SMUS

*Generate time dummy, following Chari et al., 2017, p. 12

*ME dummy is equal to one from the period 22 December 2008 (when the U.S.
*> interest rate reached the ZLB, i.e. 0.125) to April 2013.

gen ME=(Obs<=364)

*TAPER dummy is equal to one from the period May 2013 (wherein Ben Bernanke
*> first mentioned the possibility of tapering LSAP purchases).

gen TAPER=(Obs>=228 & Obs<=364)

*MT dummy is equal to one from the period 12/22/2015 onwards (hike in the IR).

gen MT=(Obs>=365)

***Unit root tests

** CLEMAO*****

*Ho: The variable has unit root (non-stationary)
*Reject if the t-statistic of (rho -1) is larger than the critical value of 5%
**Example in Baum(2005): t-statistic is -2.530 and the critical value is -3.56
***t>ct (CANNOT REJECT)

*ME

clemao1 CTF if ME==1
putexcel B2=rscalars using JPY-results, sheet("CLEMAO") modify

clemao1 ER if ME==1
putexcel C2=rscalars using JPY-results, sheet("CLEMAO") modify

clemao1 IRDF if ME==1
putexcel D2=rscalars using JPY-results, sheet("CLEMAO") modify

clemao1 VIX if ME==1
putexcel E2=rscalars using JPY-results, sheet("CLEMAO") modify

clemao1 SM if ME==1
putexcel F2=rscalars using JPY-results, sheet("CLEMAO") modify

clemao1 SMUS if ME==1
putexcel G2=rscalars using JPY-results, sheet("CLEMAO") modify

*MT

clemao1 CTF if MT==1
putexcel B10=rscalars using JPY-results, sheet("CLEMAO") modify

clemao1 ER if MT==1
putexcel C10=rscalars using JPY-results, sheet("CLEMAO") modify

clemao1 IRDF if MT==1
putexcel D10=rscalars using JPY-results, sheet("CLEMAO") modify

clemao1 VIX if MT==1
putexcel E10=rscalars using JPY-results, sheet("CLEMAO") modify

clemao1 SM if MT==1
putexcel F10=rscalars using JPY-results, sheet("CLEMAO") modify

clemao1 SMUS if MT==1
putexcel G10=rscalars using JPY-results, sheet("CLEMAO") modify

*Descriptive statistics

set more off

su CTF if ME==1, detail 
putexcel B3=rscalars using JPY-results, sheet("DESC STATS") modify

su ER if ME==1, detail 
putexcel C3=rscalars using JPY-results, sheet("DESC STATS") modify

su IRDF if ME==1, detail 
putexcel D3=rscalars using JPY-results, sheet("DESC STATS") modify

su VIX if ME==1, detail 
putexcel E3=rscalars using JPY-results, sheet("DESC STATS") modify

su SM if ME==1, detail 
putexcel F3=rscalars using JPY-results, sheet("DESC STATS") modify

su SMUS if ME==1, detail 
putexcel G3=rscalars using JPY-results, sheet("DESC STATS") modify

su CTF if MT==1, detail 
putexcel H3=rscalars using JPY-results, sheet("DESC STATS") modify

su ER if MT==1, detail 
putexcel I3=rscalars using JPY-results, sheet("DESC STATS") modify

su IRDF if MT==1, detail 
putexcel J3=rscalars using JPY-results, sheet("DESC STATS") modify

su VIX if MT==1, detail 
putexcel K3=rscalars using JPY-results, sheet("DESC STATS") modify

su SM if MT==1, detail 
putexcel L3=rscalars using JPY-results, sheet("DESC STATS") modify

su SMUS if MT==1, detail 
putexcel M3=rscalars using JPY-results, sheet("DESC STATS") modify

********************************************************************************
*** VAR

*********************************** 
****ME
*********************************** 

set more off
foreach i of varlist $FCvar {
   forvalues j=1/11 {
    gen `i'_`j'ME=l`j'.`i' if ME==1
  }
}
quietly: var CTF ER IRDF VIX SM SMUS if ME==1, exog(TAPER) lags(1/10) small lutstats

varlmar, mlag(10)
putexcel B3=matrix(r(lm)) using JPY-results, sheet("ROBUST") modify

varsoc
putexcel B3=matrix(r(stats)) using JPY-results, sheet("LAG-LENGTH") modify

quietly: var CTF ER IRDF VIX SM SMUS if ME==1, exog(TAPER IRDF_3ME CTF_3ME ER_3ME SM_3ME SMUS_3ME) lags(1/2) small lutstats

vargranger
putexcel C3=matrix(r(gstats)) using JPY-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\JPY-ME-Stability.pdf", as(pdf) replace

*********************************** 
****MT
*********************************** 

set more off
foreach i of varlist $FCvar {
   forvalues j=1/11 {
    gen `i'_`j'MT=l`j'.`i' if MT==1
  }
}
quietly: var CTF ER IRDF VIX SM SMUS if MT==1, lags(1/10) small lutstats

varlmar, mlag(10)
putexcel G3=matrix(r(lm)) using JPY-results, sheet("ROBUST") modify

varsoc
putexcel M3=matrix(r(stats)) using JPY-results, sheet("LAG-LENGTH") modify

quietly: var CTF ER IRDF VIX SM SMUS if MT==1, exog(IRDF_4MT VIX_4MT CTF_4MT SMUS_4MT) lags(1/3) small lutstats

vargranger
putexcel J3=matrix(r(gstats)) using JPY-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\JPY-MT-Stability.pdf", as(pdf) replace

save JPY-Granger.dta, replace

saveold JPY-Granger_stata13.dta, replace

log close

********************************************************************************
**************************             MXN              ************************
********************************************************************************

clear all

log using "MXN-resultats_`c(current_date)'.log", replace

capture log

set more off

global data "C:\Data and command"

cd "$data\"

import excel "C:\Data and command\Data.xlsx", sheet("MXN") firstrow

format Obs %tw

tsset Obs

set matsize 11000

g CT = Long/Short if Long>0|Short>0

mvencode CT, mv(0)

g IRD = IR - USIR

global levelvar CT ER IRD VIX SM SMUS

*Generate time dummy, following Chari et al., 2017, p. 12

*ME dummy is equal to one from the period 22 December 2008 (when the U.S.
*> interest rate reached the ZLB, i.e. 0.125) to April 2013.

gen ME=(Obs<=364)

*TAPER dummy is equal to one from the period May 2013 (wherein Ben Bernanke
*> first mentioned the possibility of tapering LSAP purchases).

gen TAPER=(Obs>=228 & Obs<=364)

*MT dummy is equal to one from the period 12/22/2015 onwards (hike in the IR).

gen MT=(Obs>=365)

***Unit root tests

** CLEMAO*****

*Ho: The variable has unit root (non-stationary)
*Reject if the t-statistic of (rho -1) is larger than the critical value of 5%
**Example in Baum(2005): t-statistic is -2.530 and the critical value is -3.56
***t>ct (CANNOT REJECT)

*ME

clemao1 CT if ME==1
putexcel B2=rscalars using MXN-results, sheet("CLEMAO") modify

clemao1 ER if ME==1
putexcel C2=rscalars using MXN-results, sheet("CLEMAO") modify

clemao1 IRD if ME==1
putexcel D2=rscalars using MXN-results, sheet("CLEMAO") modify

clemao1 VIX if ME==1
putexcel E2=rscalars using MXN-results, sheet("CLEMAO") modify

clemao1 SM if ME==1
putexcel F2=rscalars using MXN-results, sheet("CLEMAO") modify

clemao1 SMUS if ME==1
putexcel G2=rscalars using MXN-results, sheet("CLEMAO") modify

*MT

clemao1 CT if MT==1
putexcel B10=rscalars using MXN-results, sheet("CLEMAO") modify

clemao1 ER if MT==1
putexcel C10=rscalars using MXN-results, sheet("CLEMAO") modify

clemao1 IRD if MT==1
putexcel D10=rscalars using MXN-results, sheet("CLEMAO") modify

clemao1 VIX if MT==1
putexcel E10=rscalars using MXN-results, sheet("CLEMAO") modify

clemao1 SM if MT==1
putexcel F10=rscalars using MXN-results, sheet("CLEMAO") modify

clemao1 SMUS if MT==1
putexcel G10=rscalars using MXN-results, sheet("CLEMAO") modify

*Descriptive statistics

set more off

su CT if ME==1, detail 
putexcel B3=rscalars using MXN-results, sheet("DESC STATS") modify

su ER if ME==1, detail 
putexcel C3=rscalars using MXN-results, sheet("DESC STATS") modify

su IRD if ME==1, detail 
putexcel D3=rscalars using MXN-results, sheet("DESC STATS") modify

su VIX if ME==1, detail 
putexcel E3=rscalars using MXN-results, sheet("DESC STATS") modify

su SM if ME==1, detail 
putexcel F3=rscalars using MXN-results, sheet("DESC STATS") modify

su SMUS if ME==1, detail 
putexcel G3=rscalars using MXN-results, sheet("DESC STATS") modify

su CT if MT==1, detail 
putexcel H3=rscalars using MXN-results, sheet("DESC STATS") modify

su ER if MT==1, detail 
putexcel I3=rscalars using MXN-results, sheet("DESC STATS") modify

su IRD if MT==1, detail 
putexcel J3=rscalars using MXN-results, sheet("DESC STATS") modify

su VIX if MT==1, detail 
putexcel K3=rscalars using MXN-results, sheet("DESC STATS") modify

su SM if MT==1, detail 
putexcel L3=rscalars using MXN-results, sheet("DESC STATS") modify

su SMUS if MT==1, detail 
putexcel M3=rscalars using MXN-results, sheet("DESC STATS") modify

********************************************************************************
*** VAR

*********************************** 
****ME
*********************************** 

set more off
foreach i of varlist $levelvar {
   forvalues j=1/11 {
    gen `i'_`j'ME=l`j'.`i' if ME==1
  }
}
quietly: var CT ER IRD VIX SM SMUS if ME==1, exog(TAPER) lags(1/10) small lutstats

varlmar, mlag(10)
putexcel B3=matrix(r(lm)) using MXN-results, sheet("ROBUST") modify

varsoc
putexcel B3=matrix(r(stats)) using MXN-results, sheet("LAG-LENGTH") modify

quietly: var CT ER IRD VIX SM SMUS if ME==1, exog(TAPER CT_2ME SM_2ME SMUS_2ME) lags(1) small lutstats

vargranger
putexcel C3=matrix(r(gstats)) using MXN-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\MXN-ME-Stability.pdf", as(pdf) replace

*********************************** 
****MT
*********************************** 

set more off
foreach i of varlist $levelvar {
   forvalues j=1/11 {
    gen `i'_`j'MT=l`j'.`i' if MT==1
  }
}
quietly: var CT ER IRD VIX SM SMUS if MT==1, lags(1/10) small lutstats

varlmar, mlag(10)
putexcel G3=matrix(r(lm)) using MXN-results, sheet("ROBUST") modify

varsoc
putexcel M3=matrix(r(stats)) using MXN-results, sheet("LAG-LENGTH") modify

quietly: var CT ER IRD VIX SM SMUS if MT==1, exog(VIX_4MT CT_4MT SM_4MT SMUS_4MT) lags(1/3) small lutstats

set more off

vargranger
putexcel J3=matrix(r(gstats)) using MXN-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\MXN-MT-Stability.pdf", as(pdf) replace

save MXN-Granger.dta, replace

saveold MXN-Granger_stata13.dta, replace

log close

********************************************************************************
**************************             NZD              ************************
********************************************************************************

clear all

log using "NZD-resultats_`c(current_date)'.log", replace

capture log

set more off

global data "C:\Data and command"

cd "$data\"

import excel "C:\Data and command\Data.xlsx", sheet("NZD") firstrow

format Obs %tw

tsset Obs

set matsize 11000

g CT = Long/Short if Long>0|Short>0

mvencode CT, mv(0)

g IRD = IR - USIR

gen CTF = Short/Long

mvencode CTF, mv(0)

gen IRDF = USIR - IR

global FCvar CTF ER IRDF VIX SM SMUS

global levelvar CT ER IRD VIX SM SMUS

*Generate time dummy, following Chari et al., 2017, p. 12

*ME dummy is equal to one from the period 22 December 2008 (when the U.S.
*> interest rate reached the ZLB, i.e. 0.125) to April 2013.

gen ME=(Obs<=364)

*TAPER dummy is equal to one from the period May 2013 (wherein Ben Bernanke
*> first mentioned the possibility of tapering LSAP purchases).

gen TAPER=(Obs>=228 & Obs<=364)

*MTT dummy is equal to one when IRD>0, i.e. IR is larger than USIR (during MT).

gen MTT=(Obs>=365 & Obs<=494)

*MTF dummy is equal to one when IRD<0, i.e. IR is larger than USIR (during MT).

gen MTF=(Obs>=495)

***Unit root tests

** CLEMAO*****

*Ho: The variable has unit root (non-stationary)
*Reject if the t-statistic of (rho -1) is larger than the critical value of 5%
**Example in Baum(2005): t-statistic is -2.530 and the critical value is -3.56
***t>ct (CANNOT REJECT)

*ME

clemao1 CT if ME==1
putexcel B2=rscalars using NZD-results, sheet("CLEMAO") modify

clemao1 ER if ME==1
putexcel C2=rscalars using NZD-results, sheet("CLEMAO") modify

clemao1 IRD if ME==1
putexcel D2=rscalars using NZD-results, sheet("CLEMAO") modify

clemao1 VIX if ME==1
putexcel E2=rscalars using NZD-results, sheet("CLEMAO") modify

clemao1 SM if ME==1
putexcel F2=rscalars using NZD-results, sheet("CLEMAO") modify

clemao1 SMUS if ME==1
putexcel G2=rscalars using NZD-results, sheet("CLEMAO") modify

*MTT

clemao1 CT if MTT==1
putexcel B10=rscalars using NZD-results, sheet("CLEMAO") modify

clemao1 ER if MTT==1
putexcel C10=rscalars using NZD-results, sheet("CLEMAO") modify

clemao1 IRD if MTT==1
putexcel D10=rscalars using NZD-results, sheet("CLEMAO") modify

clemao1 VIX if MTT==1
putexcel E10=rscalars using NZD-results, sheet("CLEMAO") modify

clemao1 SM if MTT==1
putexcel F10=rscalars using NZD-results, sheet("CLEMAO") modify

clemao1 SMUS if MTT==1
putexcel G10=rscalars using NZD-results, sheet("CLEMAO") modify

*MTF

clemao1 CT if MTF==1
putexcel B18=rscalars using NZD-results, sheet("CLEMAO") modify

clemao1 ER if MTF==1
putexcel C18=rscalars using NZD-results, sheet("CLEMAO") modify

clemao1 IRD if MTF==1
putexcel D18=rscalars using NZD-results, sheet("CLEMAO") modify

clemao1 VIX if MTF==1
putexcel E18=rscalars using NZD-results, sheet("CLEMAO") modify

clemao1 SM if MTF==1
putexcel F18=rscalars using NZD-results, sheet("CLEMAO") modify

clemao1 SMUS if MTF==1
putexcel G18=rscalars using NZD-results, sheet("CLEMAO") modify

*Descriptive statistics

set more off

su CT if ME==1, detail 
putexcel B3=rscalars using NZD-results, sheet("DESC STATS") modify

su ER if ME==1, detail 
putexcel C3=rscalars using NZD-results, sheet("DESC STATS") modify

su IRD if ME==1, detail 
putexcel D3=rscalars using NZD-results, sheet("DESC STATS") modify

su VIX if ME==1, detail 
putexcel E3=rscalars using NZD-results, sheet("DESC STATS") modify

su SM if ME==1, detail 
putexcel F3=rscalars using NZD-results, sheet("DESC STATS") modify

su SMUS if ME==1, detail 
putexcel G3=rscalars using NZD-results, sheet("DESC STATS") modify

su CT if MTT==1, detail 
putexcel H3=rscalars using NZD-results, sheet("DESC STATS") modify

su ER if MTT==1, detail 
putexcel I3=rscalars using NZD-results, sheet("DESC STATS") modify

su IRD if MTT==1, detail 
putexcel J3=rscalars using NZD-results, sheet("DESC STATS") modify

su VIX if MTT==1, detail 
putexcel K3=rscalars using NZD-results, sheet("DESC STATS") modify

su SM if MTT==1, detail 
putexcel L3=rscalars using NZD-results, sheet("DESC STATS") modify

su SMUS if MTT==1, detail 
putexcel M3=rscalars using NZD-results, sheet("DESC STATS") modify

su CTF if MTF==1, detail 
putexcel N3=rscalars using NZD-results, sheet("DESC STATS") modify

su ER if MTF==1, detail 
putexcel O3=rscalars using NZD-results, sheet("DESC STATS") modify

su IRDF if MTF==1, detail 
putexcel P3=rscalars using NZD-results, sheet("DESC STATS") modify

su VIX if MTF==1, detail 
putexcel Q3=rscalars using NZD-results, sheet("DESC STATS") modify

su SM if MTF==1, detail 
putexcel R3=rscalars using NZD-results, sheet("DESC STATS") modify

su SMUS if MTF==1, detail 
putexcel S3=rscalars using NZD-results, sheet("DESC STATS") modify

********************************************************************************
*** VAR

*********************************** 
****ME
*********************************** 

set more off
foreach i of varlist $levelvar {
   forvalues j=1/11 {
    gen `i'_`j'ME=l`j'.`i' if ME==1
  }
}
quietly: var CT ER IRD VIX SM SMUS if ME==1, exog(TAPER) lags(1/10) small lutstats

varlmar, mlag(10)
putexcel B3=matrix(r(lm)) using NZD-results, sheet("ROBUST") modify

varsoc
putexcel B3=matrix(r(stats)) using NZD-results, sheet("LAG-LENGTH") modify

quietly: var CT ER IRD VIX SM SMUS if ME==1, exog(TAPER IRD_3ME CT_3ME ER_3ME SM_3ME SMUS_3ME) lags(1/2) small lutstats

vargranger
putexcel C3=matrix(r(gstats)) using NZD-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\NZD-ME-Stability.pdf", as(pdf) replace

*********************************** 
****MTT
*********************************** 

set more off
foreach i of varlist $levelvar {
   forvalues j=1/11 {
    gen `i'_`j'MTT=l`j'.`i' if MTT==1
  }
}
quietly: var CT ER IRD VIX SM SMUS if MTT==1, lags(1/10) small lutstats

varlmar, mlag(10)
putexcel G3=matrix(r(lm)) using NZD-results, sheet("ROBUST") modify

varsoc
putexcel M3=matrix(r(stats)) using NZD-results, sheet("LAG-LENGTH") modify

quietly: var CT ER IRD VIX SM SMUS if MTT==1, exog(IRD_8MTT CT_8MTT SM_8MTT SMUS_8MTT) lags(1/7) small lutstats

vargranger
putexcel J3=matrix(r(gstats)) using NZD-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\NZD-MTT-Stability.pdf", as(pdf) replace

*********************************** 
****MTF
*********************************** 

set more off
foreach i of varlist $FCvar {
   forvalues j=1/7 {
    gen `i'_`j'MTF=l`j'.`i' if MTF==1
  }
}
quietly: var CTF ER IRDF VIX SM SMUS if MTF==1, lags(1/6) small lutstats

varlmar, mlag(6)
putexcel L3=matrix(r(lm)) using NZD-results, sheet("ROBUST") modify

varsoc
putexcel X3=matrix(r(stats)) using NZD-results, sheet("LAG-LENGTH") modify

quietly: var CTF ER IRDF VIX SM SMUS if MTF==1, exog(IRDF_2MTF VIX_2MTF CTF_2MTF ER_2MTF SM_2MTF SMUS_2MTF) lags(1) small lutstats

vargranger
putexcel Q3=matrix(r(gstats)) using NZD-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\NZD-MTF-Stability.pdf", as(pdf) replace

save NZD-Granger.dta, replace

saveold NZD-Granger_stata13.dta, replace

log close

********************************************************************************
**************************             RBL              ************************
********************************************************************************

clear all

log using "RBL-resultats_`c(current_date)'.log", replace

capture log

set more off

global data "C:\Data and command"

cd "$data\"

import excel "C:\Data and command\Data.xlsx", sheet("RBL") firstrow

format Obs %tw

tsset Obs

set matsize 11000

g CT = Long/Short if Long>0|Short>0

mvencode CT, mv(0)

g IRD = IR - USIR

global levelvar CT ER IRD VIX SM SMUS

*Generate time dummy, following Chari et al., 2017, p. 12

*ME dummy is equal to one from the period 22 December 2008 (when the U.S.
*> interest rate reached the ZLB, i.e. 0.125) to April 2013.

gen ME=(Obs<=315)

*TAPER dummy is equal to one from the period May 2013 (wherein Ben Bernanke
*> first mentioned the possibility of tapering LSAP purchases).

gen TAPER=(Obs>=184 & Obs<=315)

*MT dummy is equal to one from the period 12/22/2015 onwards (hike in the IR).

gen MT=(Obs>=316)

***Unit root tests

** CLEMAO*****

*Ho: The variable has unit root (non-stationary)
*Reject if the t-statistic of (rho -1) is larger than the critical value of 5%
**Example in Baum(2005): t-statistic is -2.530 and the critical value is -3.56
***t>ct (CANNOT REJECT)

*ME

clemao1 CT if ME==1
putexcel B2=rscalars using RBL-results, sheet("CLEMAO") modify

clemao1 ER if ME==1
putexcel C2=rscalars using RBL-results, sheet("CLEMAO") modify

clemao1 IRD if ME==1
putexcel D2=rscalars using RBL-results, sheet("CLEMAO") modify

clemao1 VIX if ME==1
putexcel E2=rscalars using RBL-results, sheet("CLEMAO") modify

clemao1 SM if ME==1
putexcel F2=rscalars using RBL-results, sheet("CLEMAO") modify

clemao1 SMUS if ME==1
putexcel G2=rscalars using RBL-results, sheet("CLEMAO") modify

*MT

clemao1 CT if MT==1
putexcel B10=rscalars using RBL-results, sheet("CLEMAO") modify

clemao1 ER if MT==1
putexcel C10=rscalars using RBL-results, sheet("CLEMAO") modify

clemao1 IRD if MT==1
putexcel D10=rscalars using RBL-results, sheet("CLEMAO") modify

clemao1 VIX if MT==1
putexcel E10=rscalars using RBL-results, sheet("CLEMAO") modify

clemao1 SM if MT==1
putexcel F10=rscalars using RBL-results, sheet("CLEMAO") modify

clemao1 SMUS if MT==1
putexcel G10=rscalars using RBL-results, sheet("CLEMAO") modify

*Descriptive statistics

set more off

su CT if ME==1, detail 
putexcel B3=rscalars using RBL-results, sheet("DESC STATS") modify

su ER if ME==1, detail 
putexcel C3=rscalars using RBL-results, sheet("DESC STATS") modify

su IRD if ME==1, detail 
putexcel D3=rscalars using RBL-results, sheet("DESC STATS") modify

su VIX if ME==1, detail 
putexcel E3=rscalars using RBL-results, sheet("DESC STATS") modify

su SM if ME==1, detail 
putexcel F3=rscalars using RBL-results, sheet("DESC STATS") modify

su SMUS if ME==1, detail 
putexcel G3=rscalars using RBL-results, sheet("DESC STATS") modify

su CT if MT==1, detail 
putexcel H3=rscalars using RBL-results, sheet("DESC STATS") modify

su ER if MT==1, detail 
putexcel I3=rscalars using RBL-results, sheet("DESC STATS") modify

su IRD if MT==1, detail 
putexcel J3=rscalars using RBL-results, sheet("DESC STATS") modify

su VIX if MT==1, detail 
putexcel K3=rscalars using RBL-results, sheet("DESC STATS") modify

su SM if MT==1, detail 
putexcel L3=rscalars using RBL-results, sheet("DESC STATS") modify

su SMUS if MT==1, detail 
putexcel M3=rscalars using RBL-results, sheet("DESC STATS") modify

********************************************************************************
*** VAR

*********************************** 
****ME
*********************************** 

set more off
foreach i of varlist $levelvar {
   forvalues j=1/11 {
    gen `i'_`j'ME=l`j'.`i' if ME==1
  }
}
quietly: var CT ER IRD VIX SM SMUS if ME==1, exog(TAPER) lags(1/10) small lutstats

varlmar, mlag(10)
putexcel B3=matrix(r(lm)) using RBL-results, sheet("ROBUST") modify

varsoc
putexcel B3=matrix(r(stats)) using RBL-results, sheet("LAG-LENGTH") modify

quietly: var CT ER IRD VIX SM SMUS if ME==1, exog(TAPER IRD_3ME CT_3ME ER_3ME SMUS_3ME) lags(1/2) small lutstats

vargranger
putexcel C3=matrix(r(gstats)) using RBL-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\RBL-ME-Stability.pdf", as(pdf) replace

*********************************** 
****MT
*********************************** 

set more off
foreach i of varlist $levelvar {
   forvalues j=1/11 {
    gen `i'_`j'MT=l`j'.`i' if MT==1
  }
}
quietly: var CT ER IRD VIX SM SMUS if MT==1, lags(1/10) small lutstats

varlmar, mlag(10)
putexcel G3=matrix(r(lm)) using RBL-results, sheet("ROBUST") modify

varsoc
putexcel M3=matrix(r(stats)) using RBL-results, sheet("LAG-LENGTH") modify

quietly: var CT ER IRD VIX SM SMUS if MT==1, exog(IRD_3MT VIX_3MT CT_3MT ER_3MT SM_3MT SMUS_3MT) lags(1/2) small lutstats

set more off

vargranger
putexcel J3=matrix(r(gstats)) using RBL-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\RBL-MT-Stability.pdf", as(pdf) replace

save RBL-Granger.dta, replace

saveold RBL-Granger_stata13.dta, replace

log close

********************************************************************************
**************************             CHF              ************************
********************************************************************************

clear all

log using "CHF-resultats_`c(current_date)'.log", replace

capture log

set more off

global data "C:\Data and command"

cd "$data\"

import excel "C:\Data and command\Data.xlsx", sheet("CHF") firstrow

format Obs %tw

tsset Obs

set matsize 11000

g CT = Long/Short if Long>0|Short>0

mvencode CT, mv(0)

g IRD = IR - USIR

gen CTF = Short/Long

mvencode CTF, mv(0)

gen IRDF = USIR - IR

global FCvar CTF ER IRDF VIX SM SMUS

*Generate time dummy, following Chari et al., 2017, p. 12

*ME dummy is equal to one from the period 22 December 2008 (when the U.S.
*> interest rate reached the ZLB, i.e. 0.125) to April 2013.

gen ME=(Obs<=364)

*TAPER dummy is equal to one from the period May 2013 (wherein Ben Bernanke
*> first mentioned the possibility of tapering LSAP purchases).

gen TAPER=(Obs>=228 & Obs<=364)

*MT dummy is equal to one from the period 12/22/2015 onwards (hike in the IR).

gen MT=(Obs>=365)

***Unit root tests

** CLEMAO*****

*Ho: The variable has unit root (non-stationary)
*Reject if the t-statistic of (rho -1) is larger than the critical value of 5%
**Example in Baum(2005): t-statistic is -2.530 and the critical value is -3.56
***t>ct (CANNOT REJECT)

*ME

clemao1 CTF if ME==1
putexcel B2=rscalars using CHF-results, sheet("CLEMAO") modify

clemao1 ER if ME==1
putexcel C2=rscalars using CHF-results, sheet("CLEMAO") modify

clemao1 IRDF if ME==1
putexcel D2=rscalars using CHF-results, sheet("CLEMAO") modify

clemao1 VIX if ME==1
putexcel E2=rscalars using CHF-results, sheet("CLEMAO") modify

clemao1 SM if ME==1
putexcel F2=rscalars using CHF-results, sheet("CLEMAO") modify

clemao1 SMUS if ME==1
putexcel G2=rscalars using CHF-results, sheet("CLEMAO") modify

*MT

clemao1 CTF if MT==1
putexcel B10=rscalars using CHF-results, sheet("CLEMAO") modify

clemao1 ER if MT==1
putexcel C10=rscalars using CHF-results, sheet("CLEMAO") modify

clemao1 IRDF if MT==1
putexcel D10=rscalars using CHF-results, sheet("CLEMAO") modify

clemao1 VIX if MT==1
putexcel E10=rscalars using CHF-results, sheet("CLEMAO") modify

clemao1 SM if MT==1
putexcel F10=rscalars using CHF-results, sheet("CLEMAO") modify

clemao1 SMUS if MT==1
putexcel G10=rscalars using CHF-results, sheet("CLEMAO") modify

*Descriptive statistics

set more off

su CTF if ME==1, detail 
putexcel B3=rscalars using CHF-results, sheet("DESC STATS") modify

su ER if ME==1, detail 
putexcel C3=rscalars using CHF-results, sheet("DESC STATS") modify

su IRDF if ME==1, detail 
putexcel D3=rscalars using CHF-results, sheet("DESC STATS") modify

su VIX if ME==1, detail 
putexcel E3=rscalars using CHF-results, sheet("DESC STATS") modify

su SM if ME==1, detail 
putexcel F3=rscalars using CHF-results, sheet("DESC STATS") modify

su SMUS if ME==1, detail 
putexcel G3=rscalars using CHF-results, sheet("DESC STATS") modify

su CTF if MT==1, detail 
putexcel H3=rscalars using CHF-results, sheet("DESC STATS") modify

su ER if MT==1, detail 
putexcel I3=rscalars using CHF-results, sheet("DESC STATS") modify

su IRDF if MT==1, detail 
putexcel J3=rscalars using CHF-results, sheet("DESC STATS") modify

su VIX if MT==1, detail 
putexcel K3=rscalars using CHF-results, sheet("DESC STATS") modify

su SM if MT==1, detail 
putexcel L3=rscalars using CHF-results, sheet("DESC STATS") modify

su SMUS if MT==1, detail 
putexcel M3=rscalars using CHF-results, sheet("DESC STATS") modify

********************************************************************************
*** VAR

*********************************** 
****ME
*********************************** 

set more off
foreach i of varlist $FCvar {
   forvalues j=1/11 {
    gen `i'_`j'ME=l`j'.`i' if ME==1
  }
}
quietly: var CTF ER IRDF VIX SM SMUS if ME==1, exog(TAPER) lags(1/10) small lutstats

varlmar, mlag(10)
putexcel B3=matrix(r(lm)) using CHF-results, sheet("ROBUST") modify

varsoc
putexcel B3=matrix(r(stats)) using CHF-results, sheet("LAG-LENGTH") modify

quietly: var CTF ER IRDF VIX SM SMUS if ME==1, exog(TAPER IRDF_2ME SM_2ME SMUS_2ME) lags(1) small lutstats

vargranger
putexcel C3=matrix(r(gstats)) using CHF-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\CHF-ME-Stability.pdf", as(pdf) replace

*********************************** 
****MT
*********************************** 

set more off
foreach i of varlist $FCvar {
   forvalues j=1/11 {
    gen `i'_`j'MT=l`j'.`i' if MT==1
  }
}
quietly: var CTF ER IRDF VIX SM SMUS if MT==1, lags(1/10) small lutstats

varlmar, mlag(10)
putexcel G3=matrix(r(lm)) using CHF-results, sheet("ROBUST") modify

varsoc
putexcel M3=matrix(r(stats)) using CHF-results, sheet("LAG-LENGTH") modify

quietly: var CTF ER IRDF VIX SM SMUS if MT==1, exog(IRDF_7MT VIX_7MT CTF_7MT SMUS_7MT) lags(1/6) small lutstats

vargranger
putexcel J3=matrix(r(gstats)) using CHF-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\CHF-MT-Stability.pdf", as(pdf) replace

save CHF-Granger.dta, replace

saveold CHF-Granger_stata13.dta, replace

log close

********************************************************************************
**************************             GBP              ************************
********************************************************************************

clear all

log using "GBP-resultats_`c(current_date)'.log", replace

capture log

set more off

global data "C:\Data and command"

cd "$data\"

import excel "C:\Data and command\Data.xlsx", sheet("GBP") firstrow

format Obs %tw

tsset Obs

set matsize 11000

g CT = Long/Short if Long>0|Short>0

mvencode CT, mv(0)

g IRD = IR - USIR

gen CTF = Short/Long

mvencode CTF, mv(0)

gen IRDF = USIR - IR

global FCvar CTF ER IRDF VIX SM SMUS

global levelvar CT ER IRD VIX SM SMUS

*Generate time dummy, following Chari et al., 2017, p. 12

*ME dummy is equal to one from the period 22 December 2008 (when the U.S.
*> interest rate reached the ZLB, i.e. 0.125) to April 2013.

gen ME=(Obs<=364)

*TAPER dummy is equal to one from the period May 2013 (wherein Ben Bernanke
*> first mentioned the possibility of tapering LSAP purchases).

gen TAPER=(Obs>=228 & Obs<=364)

*MT dummy is equal to one from the period 12/22/2015 onwards (hike in the IR).

gen MT=(Obs>=365)

***Unit root tests

** CLEMAO*****

*Ho: The variable has unit root (non-stationary)
*Reject if the t-statistic of (rho -1) is larger than the critical value of 5%
**Example in Baum(2005): t-statistic is -2.530 and the critical value is -3.56
***t>ct (CANNOT REJECT)

*ME

clemao1 CT if ME==1
putexcel B2=rscalars using GBP-results, sheet("CLEMAO") modify

clemao1 ER if ME==1
putexcel C2=rscalars using GBP-results, sheet("CLEMAO") modify

clemao1 IRD if ME==1
putexcel D2=rscalars using GBP-results, sheet("CLEMAO") modify

clemao1 VIX if ME==1
putexcel E2=rscalars using GBP-results, sheet("CLEMAO") modify

clemao1 SM if ME==1
putexcel F2=rscalars using GBP-results, sheet("CLEMAO") modify

clemao1 SMUS if ME==1
putexcel G2=rscalars using GBP-results, sheet("CLEMAO") modify

*MT

clemao1 CTF if MT==1
putexcel B10=rscalars using GBP-results, sheet("CLEMAO") modify

clemao1 ER if MT==1
putexcel C10=rscalars using GBP-results, sheet("CLEMAO") modify

clemao1 IRDF if MT==1
putexcel D10=rscalars using GBP-results, sheet("CLEMAO") modify

clemao1 VIX if MT==1
putexcel E10=rscalars using GBP-results, sheet("CLEMAO") modify

clemao1 SM if MT==1
putexcel F10=rscalars using GBP-results, sheet("CLEMAO") modify

clemao1 SMUS if MT==1
putexcel G10=rscalars using GBP-results, sheet("CLEMAO") modify

*Descriptive statistics

set more off

su CT if ME==1, detail 
putexcel B3=rscalars using GBP-results, sheet("DESC STATS") modify

su ER if ME==1, detail 
putexcel C3=rscalars using GBP-results, sheet("DESC STATS") modify

su IRD if ME==1, detail 
putexcel D3=rscalars using GBP-results, sheet("DESC STATS") modify

su VIX if ME==1, detail 
putexcel E3=rscalars using GBP-results, sheet("DESC STATS") modify

su SM if ME==1, detail 
putexcel F3=rscalars using GBP-results, sheet("DESC STATS") modify

su SMUS if ME==1, detail 
putexcel G3=rscalars using GBP-results, sheet("DESC STATS") modify

su CTF if MT==1, detail 
putexcel H3=rscalars using GBP-results, sheet("DESC STATS") modify

su ER if MT==1, detail 
putexcel I3=rscalars using GBP-results, sheet("DESC STATS") modify

su IRDF if MT==1, detail 
putexcel J3=rscalars using GBP-results, sheet("DESC STATS") modify

su VIX if MT==1, detail 
putexcel K3=rscalars using GBP-results, sheet("DESC STATS") modify

su SM if MT==1, detail 
putexcel L3=rscalars using GBP-results, sheet("DESC STATS") modify

su SMUS if MT==1, detail 
putexcel M3=rscalars using GBP-results, sheet("DESC STATS") modify

********************************************************************************
*** VAR

*********************************** 
****ME
*********************************** 

set more off
foreach i of varlist $levelvar {
   forvalues j=1/11 {
    gen `i'_`j'ME=l`j'.`i' if ME==1
  }
}
quietly: var CT ER IRD VIX SM SMUS if ME==1, exog(TAPER) lags(1/10) small lutstats

varlmar, mlag(10)
putexcel B3=matrix(r(lm)) using GBP-results, sheet("ROBUST") modify

varsoc
putexcel B3=matrix(r(stats)) using GBP-results, sheet("LAG-LENGTH") modify

quietly: var CT ER IRD VIX SM SMUS if ME==1, exog(TAPER ER_4ME SM_4ME SMUS_4ME) lags(1/3) small lutstats

vargranger
putexcel C3=matrix(r(gstats)) using GBP-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\GBP-ME-Stability.pdf", as(pdf) replace

*********************************** 
****MT
*********************************** 

set more off
foreach i of varlist $FCvar {
   forvalues j=1/11 {
    gen `i'_`j'MT=l`j'.`i' if MT==1
  }
}
quietly: var CTF ER IRDF VIX SM SMUS if MT==1, lags(1/10) small lutstats

varlmar, mlag(10)
putexcel G3=matrix(r(lm)) using GBP-results, sheet("ROBUST") modify

varsoc
putexcel M3=matrix(r(stats)) using GBP-results, sheet("LAG-LENGTH") modify

quietly: var CTF ER IRDF VIX SM SMUS if MT==1, exog(IRDF_3MT VIX_3MT CTF_3MT ER_3MT SMUS_3MT) lags(1/2) small lutstats

vargranger
putexcel J3=matrix(r(gstats)) using GBP-results, sheet("GRANGER") modify

varstable, graph
graph export "$data\GBP-MT-Stability.pdf", as(pdf) replace

save GBP-Granger.dta, replace

saveold GBP-Granger_stata13.dta, replace

log close
