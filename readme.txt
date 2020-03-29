-----------------------------------------------------------------------------------------------
Daniel Kaufmann, “Is Deflation Costly After All? The Perils of Erroneous Historical 
Classifications," Journal of Applied Econometrics, forthcoming

readme.txt for replication files
-----------------------------------------------------------------------------------------------

Notes
-----------------------------------------------------------------------------------------------
Files with the extension *.R run in R (v. 3.6.1) and RStudio (v. 1.2.5019).
Files with the extension *.do run in Stata (v. 15). 
The GMM estimates are based on iterative procedures that may require to increase the number of 
iterations in the program (see options in the Stata files)

Data
-----------------------------------------------------------------------------------------------
BLSData.RData
	BLS CPI subindices to construct the modern replications

CompositeCPIMeasuringWorth.csv
	Composite CPI by Officer and Williamson (2016), www.measuringworth.com

RawDataProxy.csv
	PPI data by Warren and Pearson (1933) linked with modern replications by Hanes (2006)

RawData[Country].csv
	Historical data for various economies. See Online Appendix B6 for a detailed description

Codes
-----------------------------------------------------------------------------------------------
1_SimulatePlim.R -> Fig. 1, Fig C.1
	R program simulating the probability limit under various assumptions on the measurement
	errors.

2_RomerizeBLSData.R -> Fig. 2, Tab. 1, B4, B5, DataForStataUS.xlsx
	Uses BLSData.RData (Alternatively uses original BLS data downloaded from internet 
	as well as, RawDataUS.csv, CompositeCPIMeasuringWorth.csv and RawDataProxy.csv) 
	see Online Appendix B3 for a detailed description
	Computes the replication and the proxy for the US with historical and modern data. 
	In addition, prepares the US data set to in the Stata programs

3_ConstructStataData.R-> Fig. B2, DataForStata[Country].xlsx
	Uses RawData[Country].csv
	Imports the international data sources (see Online Appendix Tab. B6) and constructs data
        sets used in the Stata programs

4_ResultsUS.do -> Tab. 2, Tab C.3
	Uses DataForStataUS.xlsx
	Computes the results using US industrial production growth

5_ResultsUK.do -> Tab. 3
	Uses DataForStataUK.xlsx
	Computes the estimates for various real activity measures for the UK

6_ResultsCrossCountry.do-> Fig. 3, C.2, C.3
	Uses DataForStata[Country].xlsx
	Computes the cross-country results (main results and robustness tests)

7_SimulatedData.do -> Tab. 4
	Uses DataForStataUS.xlsx
	Computes the results with simulated error-ridden data

8_ResultsRobust.do-> Tab. C.1, C.2, C.3, D.1, D.2, D.3, D.4
	Uses DataForStataUS.xlsx
	Computes various robustness checks for the US

9_RobusntessGrid.do -> Fig. D.1
	Uses DataForStataUS.xlsx
	Computes the robustness tests for various assumptions on the misclassification rates


Contact
-----------------------------------------------------------------------------------------------
Daniel Kaufmann
daniel.kaufmann@unine.ch
