clear all

capture log

set more off

global data "C:\Carry trade and negative interest rate policy in Switzerland\"

cd "$data"

log using "resultats_`c(current_date)'.log", replace

import excel "C:\Carry trade and negative interest rate policy in Switzerland\Carry trade and negative interest rate policy in Switzerland - Replication file (Data).xlsx", firstrow

format Obs %tw

tsset Obs

set matsize 11000

* FIRST-DIFFERENCES

gen DCTUSD = d.CTUSD
gen DCTEUR = d.CTEUR
gen DCTJPY = d.CTJPY
gen DCTGBP = d.CTGBP
gen DVIX = d.VIX
gen DSM = d.SM 
gen DFSMUSD = d.FSMUSD 
gen DFSMEUR = d.FSMEUR 
gen DFSMJPY = d.FSMJPY
gen DFSMGBP = d.FSMGBP 
gen DERUSD = d.ERUSD 
gen DERJPY = d.ERJPY 
gen DEREUR = d.EREUR 
gen DERGBP = d.ERGBP

* IRD

gen IRDUSD = LT12MIRCHF - SPOTIRUSD
gen IRDEUR = LT12MIRCHF - SPOTIREUR
gen IRDJPY = LT12MIRCHF - SPOTIRJPY
gen IRDGBP = LT12MIRCHF - SPOTIRGBP
gen DIRDUSD = d.IRDUSD
gen DIRDEUR = d.IRDEUR
gen DIRDJPY = d.IRDJPY
gen DIRDGBP = d.IRDGBP

* LOG

gen LVIX = ln(VIX)
gen LSM = ln(SM) 
gen LFSMUSD = ln(FSMUSD)
gen LFSMEUR = ln(FSMEUR)
gen LFSMJPY = ln(FSMJPY)
gen LFSMGBP = ln(FSMGBP) 
gen LERUSD = ln(ERUSD)
gen LERJPY = ln(ERJPY)
gen LEREUR = ln(EREUR) 
gen LERGBP = ln(ERGBP)

* LOG, FIRST-DIFFERENCES

gen DLVIX = d.LVIX
gen DLSM = d.LSM
gen DLFSMUSD = d.LFSMUSD
gen DLFSMEUR = d.LFSMEUR
gen DLFSMJPY = d.LFSMJPY
gen DLFSMGBP = d.LFSMGBP 
gen DLERUSD = d.LERUSD 
gen DLERJPY = d.LERJPY 
gen DLEREUR = d.LEREUR
gen DLERGBP = d.LERGBP 

* NEW TEST FOR EREUR

gen D2EREUR = d.DEREUR 
gen D2LEREUR = d.DLEREUR

*Group of variables
 
global levellog CTUSD CTEUR CTJPY CTGBP LVIX LSM LFSMUSD LFSMEUR LFSMJPY LFSMGBP LERUSD LERJPY LEREUR LERGBP IRDUSD IRDEUR IRDJPY IRDGBP

*Generate time dummy, following changes in IR (Switzerland, SNB)

*NI dummy is equal to one in the period of negative interest rates (23 December 
*> 2014, onwards). There is also a restriction in the data due to the Covid-19 crisis

gen NI=(Obs>=446)

*USME dummy is equal to one to the recent ME in the U.S. (8/6/2019)

gen USME=(Obs>=687)

* ZLBEUR is equal to one when IR reaches 0 (ECB) (3/22/2016)

gen ZLBEUR=(Obs>=511)

* NIJPY is equal to one when the BOJ placed a NIRP (9/27/2016)

gen NIJPY=(Obs>=538)

* BREXIT (effective date) (2/4/2020)

gen BREXIT=(Obs>=713)

***Unit root tests

**Two breaks (CLEMIO)

**Levels

clemio2 CTUSD if NI==1
putexcel B2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 CTEUR if NI==1
putexcel C2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 CTJPY if NI==1
putexcel D2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 CTGBP if NI==1
putexcel E2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 ERUSD if NI==1
putexcel F2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 EREUR if NI==1
putexcel G2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 ERJPY if NI==1
putexcel H2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 ERGBP if NI==1
putexcel I2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 IRDUSD if NI==1
putexcel J2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 IRDEUR if NI==1
putexcel K2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 IRDJPY if NI==1
putexcel L2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 IRDGBP if NI==1
putexcel M2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 FSMUSD if NI==1
putexcel N2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 FSMEUR if NI==1
putexcel O2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 FSMJPY if NI==1
putexcel P2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 FSMGBP if NI==1
putexcel Q2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 VIX if NI==1
putexcel R2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

clemio2 SM if NI==1
putexcel S2=rscalars using Unit_root_tests, sheet("CLEMIO2") modify

**First-differences

clemio2 DCTUSD if NI==1
putexcel B2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DCTEUR if NI==1
putexcel C2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DCTJPY if NI==1
putexcel D2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DCTGBP if NI==1
putexcel E2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DERUSD if NI==1
putexcel F2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DEREUR if NI==1
putexcel G2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DERJPY if NI==1
putexcel H2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DERGBP if NI==1
putexcel I2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DIRDUSD if NI==1
putexcel J2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DIRDEUR if NI==1
putexcel K2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DIRDJPY if NI==1
putexcel L2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DIRDGBP if NI==1
putexcel M2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DFSMUSD if NI==1
putexcel N2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DFSMEUR if NI==1
putexcel O2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DFSMJPY if NI==1
putexcel P2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DFSMGBP if NI==1
putexcel Q2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DVIX if NI==1
putexcel R2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

clemio2 DSM if NI==1
putexcel S2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

**New test for EREUR (D2EREUR)

clemio2 D2EREUR if NI==1
putexcel T2=rscalars using Unit_root_tests, sheet("CLEMIO2-FD") modify

**Logs

**Two breaks (CLEMIO)

**Levels

clemio2 LVIX if NI==1
putexcel B2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG") modify

clemio2 LSM if NI==1
putexcel C2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG") modify

clemio2 LFSMUSD if NI==1
putexcel D2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG") modify

clemio2 LFSMEUR if NI==1
putexcel E2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG") modify

clemio2 LFSMJPY if NI==1
putexcel F2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG") modify

clemio2 LFSMGBP if NI==1
putexcel G2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG") modify

clemio2 LERUSD if NI==1
putexcel H2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG") modify

clemio2 LERJPY if NI==1
putexcel I2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG") modify

clemio2 LEREUR if NI==1
putexcel J2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG") modify

clemio2 LERGBP if NI==1
putexcel K2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG") modify

**First-differences

clemio2 DLVIX if NI==1
putexcel B2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG-FD") modify

clemio2 DLSM if NI==1
putexcel C2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG-FD") modify

clemio2 DLFSMUSD if NI==1
putexcel D2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG-FD") modify

clemio2 DLFSMEUR if NI==1
putexcel E2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG-FD") modify

clemio2 DLFSMJPY if NI==1
putexcel F2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG-FD") modify

clemio2 DLFSMGBP if NI==1
putexcel G2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG-FD") modify

clemio2 DLERUSD if NI==1
putexcel H2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG-FD") modify

clemio2 DLERJPY if NI==1
putexcel I2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG-FD") modify

clemio2 DLEREUR if NI==1
putexcel J2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG-FD") modify

clemio2 DLERGBP if NI==1
putexcel K2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG-FD") modify

clemio2 D2LEREUR if NI==1
putexcel L2=rscalars using Unit_root_tests, sheet("CLEMIO2-LOG-FD") modify
 
***Descriptive statistics

set more off

su CTUSD if NI==1, detail 
putexcel B2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su CTEUR if NI==1, detail 
putexcel C2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su CTJPY if NI==1, detail 
putexcel D2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su CTGBP if NI==1, detail 
putexcel E2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su IRDUSD if NI==1, detail 
putexcel F2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su IRDEUR if NI==1, detail 
putexcel G2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su IRDJPY if NI==1, detail 
putexcel H2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su IRDGBP if NI==1, detail 
putexcel I2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su LERUSD if NI==1, detail 
putexcel J2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su LEREUR if NI==1, detail 
putexcel K2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su LERJPY if NI==1, detail 
putexcel L2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su LERGBP if NI==1, detail 
putexcel M2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su LFSMUSD if NI==1, detail 
putexcel N2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su LFSMEUR if NI==1, detail 
putexcel O2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su LFSMJPY if NI==1, detail 
putexcel P2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su LFSMGBP if NI==1, detail 
putexcel Q2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su LSM if NI==1, detail 
putexcel R2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

su LVIX if NI==1, detail 
putexcel S2=rscalars using Descriptive_statistics, sheet("DESC STATS") modify

********************************************************************************
**SVAR MODELS

***************
*************USD
***************

set more off

matrix A1 = (1, 0, 0, 0, 0, 0 \ ., 1, 0, 0, 0, 0 \ ., ., 1, 0, 0, 0 \ ., ., ., 1, 0, 0 \ ., ., ., ., 1, 0 \ ., ., ., ., ., 1)

matrix B1 = (., 0, 0, 0, 0, 0 \ 0, ., 0, 0, 0, 0 \ 0, 0, ., 0, 0, 0 \ 0, 0, 0, ., 0, 0 \ 0, 0, 0, 0, ., 0 \ 0, 0, 0, 0, 0, .)

* GENERATE LAGS

foreach i of varlist $levellog {
   forvalues j=1/13 {
    gen `i'_`j'NI=l`j'.`i' if NI==1
  }
}
quietly: svar IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM if NI==1, lags(1/10) small aeq(A1) beq(B1) lutstats

quietly: varlmar, mlag(10)
putexcel A2=matrix(r(lm)) using USD_model, sheet("LM_TEST") modify

quietly: svar IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM if NI==1, exog(LVIX_3NI CTUSD_3NI LERUSD_3NI LFSMUSD_3NI LSM_3NI) lags(1/2) small aeq(A1) beq(B1) lutstats

vargranger
putexcel C2=matrix(r(gstats)) using USD_model, sheet("GRANGER") modify

irf create USD_model, set(SVAR) step(20) bs replace

irf graph coirf, xlabel(0(4)20) irf(USD_model) yline(0,lcolor(black)) byopts(yrescale) response(CTUSD)
graph export "$data\USD_model - COIRF - Response CT.pdf", as(pdf) replace

irf graph coirf, xlabel(0(4)20) irf(USD_model) yline(0,lcolor(black)) byopts(yrescale) impulse(CTUSD)
graph export "$data\USD_model - COIRF - Impulse CT.pdf", as(pdf) replace

set more off

irf table sfevd, irf(USD_model) impulse(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) response(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) noci std
irf table coirf, irf(USD_model) impulse(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) response(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) nostd

********************************************************************************

***************
*************EUR
***************

set more off

quietly: svar IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM if NI==1, lags(1/10) small aeq(A1) beq(B1) lutstats

quietly: varlmar, mlag(10)
putexcel A2=matrix(r(lm)) using EUR_model, sheet("LM_TEST") modify

quietly: svar IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM if NI==1, exog(LVIX_3NI CTEUR_3NI LEREUR_4NI LFSMEUR_3NI LSM_3NI) lags(1/2) small aeq(A1) beq(B1) lutstats

vargranger
putexcel C2=matrix(r(gstats)) using EUR_model, sheet("GRANGER") modify

irf create EUR_model, step(20) bs replace

irf graph coirf, xlabel(0(4)20) irf(EUR_model) yline(0,lcolor(black)) byopts(yrescale) response(CTEUR)
graph export "$data\EUR_model - COIRF - Response CT.pdf", as(pdf) replace

irf graph coirf, xlabel(0(4)20) irf(EUR_model) yline(0,lcolor(black)) byopts(yrescale) impulse(CTEUR)
graph export "$data\EUR_model - COIRF - Impulse CT.pdf", as(pdf) replace

irf table sfevd, irf(EUR_model) impulse(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) response(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) noci std
irf table coirf, irf(EUR_model) impulse(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) response(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) nostd

********************************************************************************

***************
*************JPY
***************

set more off

quietly: svar IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM if NI==1, lags(1/10) small aeq(A1) beq(B1) lutstats

quietly: varlmar, mlag(10)
putexcel A2=matrix(r(lm)) using JPY_model, sheet("LM_TEST") modify

quietly: svar IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM if NI==1, exog(LVIX_2NI CTJPY_2NI LERJPY_2NI LFSMJPY_2NI LSM_2NI) lags(1) small aeq(A1) beq(B1) lutstats

vargranger
putexcel C2=matrix(r(gstats)) using JPY_model, sheet("GRANGER") modify

irf create JPY_model, step(20) bs replace

irf graph coirf, xlabel(0(4)20) irf(JPY_model) yline(0,lcolor(black)) byopts(yrescale) response(CTJPY)
graph export "$data\JPY_model - COIRF - Response CT.pdf", as(pdf) replace

irf graph coirf, xlabel(0(4)20) irf(JPY_model) yline(0,lcolor(black)) byopts(yrescale) impulse(CTJPY)
graph export "$data\JPY_model - COIRF - Impulse CT.pdf", as(pdf) replace

irf table sfevd, irf(JPY_model) impulse(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) response(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) noci std
irf table coirf, irf(JPY_model) impulse(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) response(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) nostd

********************************************************************************

***************
*************GBP
***************

set more off

quietly: svar IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM if NI==1, lags(1/10) small aeq(A1) beq(B1) lutstats

quietly: varlmar, mlag(10)
putexcel A2=matrix(r(lm)) using GBP_model, sheet("LM_TEST") modify

quietly: svar IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM if NI==1, exog(LVIX_2NI CTGBP_2NI LSM_2NI) lags(1) small aeq(A1) beq(B1) lutstats

vargranger
putexcel C2=matrix(r(gstats)) using GBP_model, sheet("GRANGER") modify

irf create GBP_model, step(20) bs replace

irf graph coirf, xlabel(0(4)20) irf(GBP_model) yline(0,lcolor(black)) byopts(yrescale) response(CTGBP)
graph export "$data\GBP_model - COIRF - Response CT.pdf", as(pdf) replace

irf graph coirf, xlabel(0(4)20) irf(GBP_model) yline(0,lcolor(black)) byopts(yrescale) impulse(CTGBP)
graph export "$data\GBP_model - COIRF - Impulse CT.pdf", as(pdf) replace

irf table sfevd, irf(GBP_model) impulse(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) response(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) noci std
irf table coirf, irf(GBP_model) impulse(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) response(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) nostd

********************************************************************************
*******************************************************************************
**ROBUSTNESS CHECK - DUMMY

***************
*************USD
***************

set more off

quietly: svar LVIX LSM LFSMUSD LERUSD IRDUSD CTUSD if NI==1, exog(USME LVIX_3NI CTUSD_3NI LERUSD_3NI LFSMUSD_3NI LSM_3NI) lags(1/2) small aeq(A1) beq(B1) lutstats

irf create USD_RCDUMMY, set(SVAR_RCDUMMY) step(20) bs replace

irf table sfevd, irf(USD_RCDUMMY) impulse(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) response(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) noci std
irf table coirf, irf(USD_RCDUMMY) impulse(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) response(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) nostd

********************************************************************************

***************
*************EUR
***************

set more off

quietly: svar LVIX LFSMEUR LSM LEREUR IRDEUR CTEUR if NI==1, exog(ZLBEUR LVIX_3NI CTEUR_3NI LEREUR_4NI LFSMEUR_3NI LSM_3NI) lags(1/2) small aeq(A1) beq(B1) lutstats

irf create EUR_RCDUMMY, step(20) bs replace

irf table sfevd, irf(EUR_RCDUMMY) impulse(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) response(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) noci std
irf table coirf, irf(EUR_RCDUMMY) impulse(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) response(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) nostd

********************************************************************************

***************
*************JPY
***************

set more off

quietly: svar IRDJPY LVIX LSM CTJPY LERJPY LFSMJPY if NI==1, exog(NIJPY LVIX_2NI CTJPY_2NI LERJPY_2NI LFSMJPY_2NI LSM_2NI) lags(1) small aeq(A1) beq(B1) lutstats

irf create JPY_RCDUMMY, step(20) bs replace

irf table sfevd, irf(JPY_RCDUMMY) impulse(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) response(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) noci std
irf table coirf, irf(JPY_RCDUMMY) impulse(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) response(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) nostd

********************************************************************************

***************
*************GBP
***************

set more off

quietly: svar LVIX LFSMGBP IRDGBP LSM CTGBP LERGBP if NI==1, exog(BREXIT LVIX_2NI CTGBP_2NI LSM_2NI) lags(1) small aeq(A1) beq(B1) lutstats

irf create GBP_RCDUMMY, step(20) bs replace

irf table sfevd, irf(GBP_RCDUMMY) impulse(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) response(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) noci std
irf table coirf, irf(GBP_RCDUMMY) impulse(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) response(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) nostd

********************************************************************************
*******************************************************************************
**ROBUSTNESS CHECK - NEW ORDERING

***************
*************USD
***************

set more off

quietly: svar LVIX LSM LFSMUSD LERUSD IRDUSD CTUSD if NI==1, exog(LVIX_3NI CTUSD_3NI LERUSD_3NI LFSMUSD_3NI LSM_3NI) lags(1/2) small aeq(A1) beq(B1) lutstats

irf create USD_RCNO, set(SVAR_RCNO) step(20) bs replace

irf table sfevd, irf(USD_RCNO) impulse(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) response(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) noci std
irf table coirf, irf(USD_RCNO) impulse(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) response(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) nostd

********************************************************************************

***************
*************EUR
***************

set more off

quietly: svar LVIX LFSMEUR LSM LEREUR IRDEUR CTEUR if NI==1, exog(LVIX_3NI CTEUR_3NI LEREUR_4NI LFSMEUR_3NI LSM_3NI) lags(1/2) small aeq(A1) beq(B1) lutstats

irf create EUR_RCNO, step(20) bs replace

irf table sfevd, irf(EUR_RCNO) impulse(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) response(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) noci std
irf table coirf, irf(EUR_RCNO) impulse(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) response(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) nostd

********************************************************************************

***************
*************JPY
***************

set more off

quietly: svar IRDJPY LVIX LSM CTJPY LERJPY LFSMJPY if NI==1, exog(LVIX_2NI CTJPY_2NI LERJPY_2NI LFSMJPY_2NI LSM_2NI) lags(1) small aeq(A1) beq(B1) lutstats

irf create JPY_RCNO, step(20) bs replace

irf table sfevd, irf(JPY_RCNO) impulse(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) response(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) noci std
irf table coirf, irf(JPY_RCNO) impulse(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) response(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) nostd

********************************************************************************

***************
*************GBP
***************

set more off

quietly: svar LVIX LFSMGBP IRDGBP LSM CTGBP LERGBP if NI==1, exog(LVIX_2NI CTGBP_2NI LSM_2NI) lags(1) small aeq(A1) beq(B1) lutstats

irf create GBP_RCNO, step(20) bs replace

irf table sfevd, irf(GBP_RCNO) impulse(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) response(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) noci std
irf table coirf, irf(GBP_RCNO) impulse(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) response(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) nostd

********************************************************************************
*******************************************************************************
**ROBUSTNESS CHECK - EXCLUDING CT

***************
*************USD
***************

set more off

matrix A1 = (1, 0, 0, 0, 0 \ ., 1, 0, 0, 0 \ ., ., 1, 0, 0 \ ., ., ., 1, 0 \ ., ., ., ., 1)

matrix B1 = (., 0, 0, 0, 0 \ 0, ., 0, 0, 0 \ 0, 0, ., 0, 0 \ 0, 0, 0, ., 0 \ 0, 0, 0, 0, .)

quietly: svar LVIX LSM LFSMUSD LERUSD IRDUSD if NI==1, exog(LVIX_3NI LERUSD_3NI LFSMUSD_3NI LSM_3NI) lags(1/2) small aeq(A1) beq(B1) lutstats

irf create USD_RCECT, set(SVAR_RCECT) step(20) bs replace

irf table sfevd, irf(USD_RCECT) impulse(IRDUSD LVIX LERUSD LFSMUSD LSM) response(IRDUSD LVIX LERUSD LFSMUSD LSM) noci std
irf table coirf, irf(USD_RCECT) impulse(IRDUSD LVIX LERUSD LFSMUSD LSM) response(IRDUSD LVIX LERUSD LFSMUSD LSM) nostd

********************************************************************************

***************
*************EUR
***************

set more off

quietly: svar LVIX LFSMEUR LSM LEREUR IRDEUR if NI==1, exog(LVIX_3NI LEREUR_4NI LFSMEUR_3NI LSM_3NI) lags(1/2) small aeq(A1) beq(B1) lutstats

irf create EUR_RCECT, step(20) bs replace

irf table sfevd, irf(EUR_RCECT) impulse(IRDEUR LVIX LEREUR LFSMEUR LSM) response(IRDEUR LVIX LEREUR LFSMEUR LSM) noci std
irf table coirf, irf(EUR_RCECT) impulse(IRDEUR LVIX LEREUR LFSMEUR LSM) response(IRDEUR LVIX LEREUR LFSMEUR LSM) nostd

********************************************************************************

***************
*************JPY
***************

set more off

quietly: svar IRDJPY LVIX LSM LERJPY LFSMJPY if NI==1, exog(LVIX_2NI LERJPY_2NI LFSMJPY_2NI LSM_2NI) lags(1) small aeq(A1) beq(B1) lutstats

irf create JPY_RCECT, step(20) bs replace

irf table sfevd, irf(JPY_RCECT) impulse(IRDJPY LVIX LERJPY LFSMJPY LSM) response(IRDJPY LVIX LERJPY LFSMJPY LSM) noci std
irf table coirf, irf(JPY_RCECT) impulse(IRDJPY LVIX LERJPY LFSMJPY LSM) response(IRDJPY LVIX LERJPY LFSMJPY LSM) nostd

********************************************************************************

***************
*************GBP
***************

set more off

quietly: svar LVIX LFSMGBP IRDGBP LSM LERGBP if NI==1, exog(LVIX_2NI LSM_2NI) lags(1) small aeq(A1) beq(B1) lutstats

irf create GBP_RCECT, step(20) bs replace

irf table sfevd, irf(GBP_RCECT) impulse(IRDGBP LVIX LERGBP LFSMGBP LSM) response(IRDGBP LVIX LERGBP LFSMGBP LSM) noci std
irf table coirf, irf(GBP_RCECT) impulse(IRDGBP LVIX LERGBP LFSMGBP LSM) response(IRDGBP LVIX LERGBP LFSMGBP LSM) nostd

********************************************************************************
*******************************************************************************
**ROBUSTNESS CHECK - REMOVING TYA

***************
*************USD
***************

set more off

quietly: svar LVIX LSM LFSMUSD LERUSD IRDUSD CTUSD if NI==1, lags(1/2) small aeq(A1) beq(B1) lutstats

irf create USD_NO_TYA, set(SVAR_NO_TYA) step(20) bs replace

irf table sfevd, irf(USD_NO_TYA) impulse(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) response(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) noci std
irf table coirf, irf(USD_NO_TYA) impulse(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) response(IRDUSD LVIX CTUSD LERUSD LFSMUSD LSM) nostd

********************************************************************************

***************
*************EUR
***************

set more off

quietly: svar LVIX LFSMEUR LSM LEREUR IRDEUR CTEUR if NI==1, lags(1/2) small aeq(A1) beq(B1) lutstats

irf create EUR_NO_TYA, step(20) bs replace

irf table sfevd, irf(EUR_NO_TYA) impulse(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) response(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) noci std
irf table coirf, irf(EUR_NO_TYA) impulse(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) response(IRDEUR LVIX CTEUR LEREUR LFSMEUR LSM) nostd

********************************************************************************

***************
*************JPY
***************

set more off

quietly: svar IRDJPY LVIX LSM CTJPY LERJPY LFSMJPY if NI==1, lags(1) small aeq(A1) beq(B1) lutstats

irf create JPY_NO_TYA, step(20) bs replace

irf table sfevd, irf(JPY_NO_TYA) impulse(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) response(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) noci std
irf table coirf, irf(JPY_NO_TYA) impulse(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) response(IRDJPY LVIX CTJPY LERJPY LFSMJPY LSM) nostd

********************************************************************************

***************
*************GBP
***************

set more off

quietly: svar LVIX LFSMGBP IRDGBP LSM CTGBP LERGBP if NI==1, lags(1) small aeq(A1) beq(B1) lutstats

irf create GBP_NO_TYA, step(20) bs replace

irf table sfevd, irf(GBP_NO_TYA) impulse(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) response(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) noci std
irf table coirf, irf(GBP_NO_TYA) impulse(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) response(IRDGBP LVIX CTGBP LERGBP LFSMGBP LSM) nostd

********************************************************************************
*******************************************************************************

save Carry_trade_and_negative_interest_rate_policy_in_Switzerland.dta, replace

saveold Carry_trade_and_negative_interest_rate_policy_in_Switzerland.dta, replace

save "$data\Carry_trade_and_negative_interest_rate_policy_in_Switzerland.dta", replace

log close
