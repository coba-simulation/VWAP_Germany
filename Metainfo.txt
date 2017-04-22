
Name of QuantLet : VWAP_Forecast

Published in : The behavior of electricity prices at the German intraday market

Description : 'Quantlets for the codes used in my master thesis. The data published here are artificial and only for the purpose to illustrate how the codes work. This thesis uses the concept of generalized quantiles to compute probabilistic forecasts of volume weighted average prices (VWAP) stemming from the German intraday market for electricity contracts. These prices exhibit extreme values in both directions and correlate inter- and intradaily. Dimensions are reduced by functional principal component and factorisable sparse tail event curve techniques. The dependency structure of the factor scores for generalized quantiles is analyzed with a VAR model that allows for incorporation of exogenous information such as renewable energy production forecasts. Price forecasts from both models are evaluated with root mean squared error and mean absolute error. Interval forecasts are evaluated, to which share the interval captures observed prices. Supplementary material for this thesis is available online.'

Usage : 'Data preperation by data_and_helperfunctions.R has to be run at first. The output is VWAP.Rdata, a *.Rdata object that is used by the quantlets in the in the subfolders.'

Keywords : 
- expectile
- quantile regression
- FPCA
- FASTEC
- Short-term energy price forecasting
- VWAP
- High-dimensional data analysis
- Functional time series
- Forecast
- VAR
- Arima
- pca
- factor scores
- factor loadings

See also : 
- FASTECChinaTemper2008
- FPCA_Electricity_Application
- FPCA_Electricity_Simulation
- electricitypriceforecasting
- LQRCheck
- VWAP_Forecast
- VWAP_FPCA_Training
- VWAP_FPCA_Forecast
- VWAP_FASTEC_Training
- VWAP_FASTEC_Forecast
- VWAP_Differenceplot
- VWAP_Polygonplot
- VWAP_PriceLoadSequence
- VWAP_Descriptive
- VWAP_Surfaceplot


Author : Johannes Stoiber

Submitted : Sat, 22 April 2017 by Johannes Stoiber

Datafiles : 
- vwap_data.csv

Output :
- VWAP.Rdata
