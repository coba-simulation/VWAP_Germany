
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **VWAP_Forecast_DM** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : VWAP_Forecast_DM

Published in : The behavior of electricity prices at the German intraday market

Description : Conducts a Diebold-Marino tests on the different models.

Keywords : 'surface, functional data, electricity, residual load, VWAP, time-series, Functional
time series'

See also : 'VWAP_Forecast, VWAP_Polygonplot, VWAP_Differenceplot, VWAP_PriceLoadSequence,
VWAP_FPCA_Training, VWAP_FPCA_Test, FPCA_Electricity_Simulation, FPCA_Electricity_Application'

Author : Johannes Stoiber

Submitted : So, 23 April 2017 by Johannes Stoiber

Datafiles : VWAP.Rdata

```


### R Code:
```r
###############################################################################
##                                                                           ##
##       Diebold Marino Test on produced point forecast for                  ##
##       center of the distribution, i.e. tau = 0.5                          ##
##                                                                           ##
###############################################################################

# libraries
libraries = c("stargazer", "forecast")
lapply(libraries,function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries,require,quietly=TRUE,character.only=TRUE)


# load and prepare data with forecast errors
FPCA_bic   = read.csv(paste0(path,"/VWAP_Forecast_DM/Forecast_FPCA_BIC.csv"))
FASTEC_bic = read.csv(paste0(path,"/VWAP_Forecast_DM/Forecast_FASTEC_BIC.csv"))

# BIC models as dataframe
bic_error  = data.frame(FPCA_bic[, 1:5], FASTEC_bic, FPCA_bic[, 6:7] ) 
names(bic_error) = c(paste0(rep(c("FPCA ", "FASTEC "), each = 5), names(FPCA_bic)[1:5]), names(FPCA_bic)[6:7])

## Save p-value of the test 
for (i in 1:dim(bic_error)[2]){
  if (i == 1){
    # matrix to stor p-values from test
    dm_mat = matrix(ncol = dim(bic_error)[2], nrow = dim(bic_error)[2], rep(NA, dim(bic_error)[2]^2), 
                              dimnames = list(names(bic_error), names(bic_error)))
    }
  for (j in 1:dim(bic_error)[2]){
    if (i == j)next
    # conduct test and store p-value. test is that model [,j] is more  accurate than model [, i]
    dm_mat[i,j] = dm.test(bic_error[, i], bic_error[, j], alternative = "greater", power =  )$p.value
    dm_mat = round(dm_mat, digits = 3)
  }
}

stargazer(dm_mat)#, type = "text")

```
