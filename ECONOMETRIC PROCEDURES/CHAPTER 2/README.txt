Aug 2, 2020

------------------------------------------------------------------------------

This folder contains the replication files for the paper “Carry trade in developing and developed countries: A Granger causality analysis with the Toda-Yamamoto approach” by Bruno Thiago Tomio

Contact information: bttomio@furb.br

------------------------------------------------------------------------------

Instructions:

In the folder C:\, unzip the compressed file. This will create the folder C:\Data and command.
If you change the folder, you will have to change all directory folders in the Stata Do-file.

In the folder C:\Data and command, run Command.do with Stata 13. Execute the Stata Do-file (Ctrl + D).

For each country/currency, you will find the results in the folder C:\Data and command. 
For example, for Brazil, open the file BRL-results.xlsx.

Inside each <Currency>-results.xlsx file, you will find five tabs replicating the results:
1) DESC STATS (Appendix B - Descriptive statistics);
2) CLEMAO (Appendix C - Unit-root tests (Clemente-Montañés-Reyes));
3) LAG-LENGTH (Appendix D - Optimal lag-order);
4) ROBUST (Appendix E - Lagrange-multiplier (LM) test for residual autocorrelation); and
5) GRANGER (Appendix H - Granger causality tests).

Additionally, in the main folder, you will separated also find all stability tests (Appendix F - Stability conditions for VAR estimates (Eigenvalue stability condition)).
Take Australia as an example. You will find three files with stability tests: 
1) AUD-ME-Stability.pdf, for the Monetary Easing period;
2) AUD-MTT-Stability.pdf, for the Monetary Tightening (Target currency) period; and
2) AUD-MTF-Stability.pdf, for the Monetary Tightening (Funding currency) period.

The Do-File will also save two .dta files for each country.

If you wish to check the data, just go to the file Data.xlsx.

Please contact me if you have any doubt.