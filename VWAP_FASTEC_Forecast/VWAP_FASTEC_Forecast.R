###############################################################################
##                                                                           ##
##       VWAP FASTEC Forecast. R-Script to apply FASTEC to desaisonalized    ##
##       the vwap weries, and forecast loadings with VAR.                    ##
##                                                                           ##
###############################################################################

# load the R Environment with prepared data and packages
load("VWAP.RData")
libraries = c("stargazer", "plyr","moments","zoo","forecast","urca","expectreg","fda.usc", "vars","lattice","tseries", "abind","sm","quantreg","matrixStats")
lapply(libraries,function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries,require,quietly=TRUE,character.only=TRUE)


###############################################################################
# set some prarameter
n             = 65                             # Train Data length
hour          = 24                             # number of avialable daily observations
TAU           = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)   # quantiles of interest
model         = "fourier"                      # estimation of seasonal component
                                               # "fourier" includes also dummies for weekday and holidays
                                               # "no" only includes weekday and holiday dummies
                                               # "smooth.spline", "sm.regression", "loess" do not include holiday and weekday effects
trsh          = .95                            # threshold how mouch should be explained by exogenous variables; apply PCA
lag.l         = 3                              # (lag.l-1) determines allowed maximum lag length for VAR models.chose fiteen to allow for past two weeks as impact; 30 days windwo: 3; 60 days window 5  
p             = 1                              # parameter demanded by for seasonality forecast
k             = 0                              # k is used to set increasing steps, only needed to vary in testperiod
lt            = length(TAU)                    # number of expectils
holiday_dummy = holiday_dummy                  # dummy variable indicating public holidays
window        = 65                             # shorter window
window        = ifelse(exists("window"),
                       window, n)              
crit          = c("SC", "AIC")                 # information criteria to select lag order
pf            = ceiling(hour^0.4)              # number of factors to choose
qt.5          = which(TAU %in% 0.5)            # index of 0.5 expectile
qt.l          = which(TAU %in% min(TAU))       # lowest quantile in TAU vector
qt.u          = which(TAU %in% max(TAU))       # highest quantile in TAU vector
h             = dim(vwap)[1] - n               # forecast length.
################################################################################

start = Sys.time()
# Variables and parameters for MQR
xx    = seq(1/hour, 1, length = hour)
X.fac = bs(xx, df = pf, intercept = TRUE)
X.val = X.fac[,]
lamb = lapply(X = c(1:lt), FUN = function(X){simtune(m = n, tau = TAU[X], XX = X.val, alpha = 0.1, B = 1000, const = 2)$lambda})
names(lamb) = paste(TAU)
sapply(lamb, "[[", 1)
itt              = 2000
epsilon          = 1e-06
kappa            = 1e-04

# Start loop for 1 day-ahead forecasts
for(k in 0:(h-2)){
  # assume we know all the exogen variables one day in advance.
  Input_list     = c(t(vwap[c(1:(n + k)), ]))
  Input_list_ex1 = c(t(spot[c(1:(n + k + p)), ]))
  Input_list_ex2 = c(t(con_act[c(1:(n + k + p)), ]))
  Input_list_ex3 = c(t(spv_act[c(1:(n + k + p)), ]))
  Input_list_ex4 = c(t(wnd_act[c(1:(n + k + p)), ]))
  Input_list_ex5 = c(t(con_for[c(1:(n + k + p)), ]))
  Input_list_ex6 = c(t(spv_for[c(1:(n + k + p)), ]))
  Input_list_ex7 = c(t(wnd_for[c(1:(n + k + p)), ]))
  
  ## DSC
  ## DSC: construct dummies for deseasonalization. wd and ph are only zeros
  ## because sund and wind do not care for weekend or public holidays
  tempseq = as.numeric(rep(rep(c(1:7), each = 1), length.out = n + k + 2))
  WD = sapply(1:6, FUN = function(num){as.numeric(tempseq == num)})
  wd = matrix(rep(0, prod(dim(WD)))           , ncol = dim(WD)[2])
  holiday_dummy = as.matrix(holiday_dummy, ncol = 1)
  ph      = matrix(rep(0, (prod(dim(holiday_dummy)))))
  
  # The seasonal component
  if(k == 0){DSC_Forc.mat = array(NA, dim = c((h - 1), hour))}
  DSC_Forc.mat[(k + p), ] = dsc(Input_list, WD, PH = holiday_dummy, n = n, k = k, hours = hour, p = p, model = model)$forecast
  
  # Compute deseasonalized series
  DSC_Input     = dsc(Input_list, WD = WD, PH = holiday_dummy, n = n, k = k, hours = hour, p = p, model = model)[[1]]
  DSC_Input_ex1 = dsc(Input_list_ex1, WD = WD, PH = holiday_dummy, n = n + p, k = k, hours = hour, p = p, model = model)[[1]]
  DSC_Input_ex2 = dsc(Input_list_ex2, WD = WD, PH = holiday_dummy, n = n + p, k = k, hours = hour, p = p, model = model)[[1]]
  DSC_Input_ex3 = dsc(Input_list_ex3, WD = wd, PH = ph, n = n + p, k = k, hours = hour, p = p,model = model)[[1]]
  DSC_Input_ex4 = dsc(Input_list_ex4, WD = wd, PH = ph, n = n + p, k = k, hours = hour, p = p,model = model)[[1]]
  DSC_Input_ex5 = dsc(Input_list_ex5, WD = WD, PH=holiday_dummy, n = n + p, k = k, hours = hour, p = p, model = model)[[1]]
  DSC_Input_ex6 = dsc(Input_list_ex6, WD = wd, PH = ph, n = n + p, k = k, hours = hour, p = p, model = model)[[1]]
  DSC_Input_ex7 = dsc(Input_list_ex7, WD = wd, PH = ph, n = n + p, k = k, hours = hour, p = p, model = model)[[1]]
  
  # create matrix of residuals. rows are 1-h intervalls. columns are the sepcific calender days
  resid_data_yearly = matrix(DSC_Input,nrow = hour)
  ex1_data_yearly   = matrix(DSC_Input_ex1, nrow = hour)
  ex2_data_yearly   = matrix(DSC_Input_ex2 - DSC_Input_ex3 - DSC_Input_ex4, nrow = hour)
  ex3_data_yearly   = matrix(DSC_Input_ex5 - DSC_Input_ex6 - DSC_Input_ex7, nrow = hour)
  ex4_data_yearly   = ex2_data_yearly - ex3_data_yearly
  
  # compute Principal Components of the explanatory variables
  PCA_ex  = list( pca.man(t(ex1_data_yearly), tau = trsh),
                  pca.man(t(ex2_data_yearly), tau = trsh),
                  pca.man(t(ex3_data_yearly), tau = trsh),
                  pca.man(t(ex4_data_yearly), tau = trsh))
  
  # get the scores
  scores.ex = lapply(lapply(PCA_ex, "[[", "scores" ), "[",c((n + k + p - window):(n + k + p)),TRUE)

  # MQR 
  Y         = data.matrix(resid_data_yearly[, c((n + k + p - window):(n + k ))])
  fit.vwap  = lapply(X = c(1:lt), FUN = function(X){mqr(Y = Y, X = X.val, tau = TAU[X], kappa = kappa,  epsilon = epsilon, lambda = lamb[[X]], itt =itt)})
  
  # eonomy SVD, and assign column names for factor loadings
  econU = lapply(fit.vwap, "[[", "U")
  econd = lapply(lapply(fit.vwap, "[[", "d"), diag)
  econV = lapply(lapply(fit.vwap, "[[", "V"), "[", TRUE, c(1:pf))
  econV = lapply(econV, "colnames<-", paste0("load",seq(1,pf)))
  
  # for both criteria
  for (IC in crit){
    # run VAR model
    model.spot  = lapply(X = c(1:lt), FUN = function(X){VAR(y = econV[[X]], exogen = scores.ex[[1]][-(window + p), ], lag.max = (lag.l - 1), type = "const", ic = IC)})
    model.RLac  = lapply(X = c(1:lt), FUN = function(X){VAR(y = econV[[X]], exogen = scores.ex[[2]][-(window + p), ], lag.max = (lag.l - 1), type = "const", ic = IC)})#, lag.max = (lag.l - 1), type = "const", ic = IC)})
    model.RLmk  = lapply(X = c(1:lt), FUN = function(X){VAR(y = econV[[X]], exogen = scores.ex[[3]][-(window + p), ], lag.max = (lag.l - 1), type = "const", ic = IC)})
    model.RLdf  = lapply(X = c(1:lt), FUN = function(X){VAR(y = econV[[X]], exogen = scores.ex[[4]][-(window + p), ], lag.max = (lag.l - 1), type = "const", ic = IC)})
    model.endo  = lapply(X = c(1:lt), FUN = function(X){VAR(y = econV[[X]], lag.max = (lag.l - 1), type = "const", ic = IC)})
    
    
    # forecast the factor loadings
    pred.spot = lapply(X =c(1:lt), FUN = function(X){predict(model.spot[[X]], n.ahead = p, dumvar = matrix(scores.ex[[1]][(window + p), ], nrow = p))})
    pred.RLac = lapply(X =c(1:lt), FUN = function(X){predict(model.RLac[[X]], n.ahead = p, dumvar = matrix(scores.ex[[2]][(window + p), ], nrow = p))})
    pred.RLmk = lapply(X =c(1:lt), FUN = function(X){predict(model.RLmk[[X]], n.ahead = p, dumvar = matrix(scores.ex[[3]][(window + p), ], nrow = p))})
    pred.RLdf = lapply(X =c(1:lt), FUN = function(X){predict(model.RLdf[[X]], n.ahead = p, dumvar = matrix(scores.ex[[4]][(window + p), ], nrow = p))})
    pred.endo = lapply(X =c(1:lt), FUN = function(X){predict(model.endo[[X]], n.ahead = p)})
    
    
    # reconstruct forecasted stochastic curve and add seasonal forecast
    forc.spot = lapply(X = c(1:lt), FUN = function(X){forecast.mqr(X = X.val, U = econU[[X]], D = econd[[X]], fitted.model = pred.spot[[X]], season = DSC_Forc.mat[(k + p), ])})
    forc.RLac = lapply(X = c(1:lt), FUN = function(X){forecast.mqr(X = X.val, U = econU[[X]], D = econd[[X]], fitted.model = pred.RLac[[X]], season = DSC_Forc.mat[(k + p), ])})
    forc.RLmk = lapply(X = c(1:lt), FUN = function(X){forecast.mqr(X = X.val, U = econU[[X]], D = econd[[X]], fitted.model = pred.RLmk[[X]], season = DSC_Forc.mat[(k + p), ])})
    forc.RLdf = lapply(X = c(1:lt), FUN = function(X){forecast.mqr(X = X.val, U = econU[[X]], D = econd[[X]], fitted.model = pred.RLdf[[X]], season = DSC_Forc.mat[(k + p), ])})
    forc.endo = lapply(X = c(1:lt), FUN = function(X){forecast.mqr(X = X.val, U = econU[[X]], D = econd[[X]], fitted.model = pred.endo[[X]], season = DSC_Forc.mat[(k + p), ])})
    
    # depending on selected IC, save forecasted values, container to store results is created in first loop (k=0)
    if (IC == "AIC"){
      if ( k == 0 ){
        # 3d-array to store results
        forc.endo.AIC = array(NA, dim = c(hour, (h - 1), lt) )
        forc.spot.AIC = array(NA, dim = c(hour, (h - 1), lt) )
        forc.RLac.AIC = array(NA, dim = c(hour, (h - 1), lt) )
        forc.RLmk.AIC = array(NA, dim = c(hour, (h - 1), lt) )
        forc.RLdf.AIC = array(NA, dim = c(hour, (h - 1), lt) )
      }  
      forc.endo.AIC[, k + p, ] = abind(forc.endo, along = 2)
      forc.spot.AIC[, k + p, ] = abind(forc.spot, along = 2)
      forc.RLac.AIC[, k + p, ] = abind(forc.RLac, along = 2)
      forc.RLmk.AIC[, k + p, ] = abind(forc.RLmk, along = 2)
      forc.RLdf.AIC[, k + p, ] = abind(forc.RLdf, along = 2)
    }
    
    if (IC == "SC"){
      if ( k == 0 ){
        # 3d-array to store results
        forc.endo.SC = array(NA, dim = c(hour, (h - 1), lt) )
        forc.spot.SC = array(NA, dim = c(hour, (h - 1), lt) )
        forc.RLac.SC = array(NA, dim = c(hour, (h - 1), lt) )
        forc.RLmk.SC = array(NA, dim = c(hour, (h - 1), lt) )
        forc.RLdf.SC = array(NA, dim = c(hour, (h - 1), lt) )
      }
      forc.endo.SC[, k + p, ] = abind(forc.endo, along = 2)
      forc.spot.SC[, k + p, ] = abind(forc.spot, along = 2)
      forc.RLac.SC[, k + p, ] = abind(forc.RLac, along = 2)
      forc.RLmk.SC[, k + p, ] = abind(forc.RLmk, along = 2)
      forc.RLdf.SC[, k + p, ] = abind(forc.RLdf, along = 2)
    } # end "SC" result savings
  } # end loop over IC                                                                        
} # end k loop
Sys.time() - start

###############################################################################
##                        Plot and Evaluation                                ##
##                                                                           ##
##       Table:  Model performance due to RMSE                               ##
##       Plot:   Daily Curves with forecast from best model                  ##
##                                                                           ##
###############################################################################

# Table:
# compute errors at median
origin = t(vwap[c((n+1):(n+h-1)), ])

# save results in list
median.Fit     = list("DA spot AIC"    = forc.spot.AIC[, , qt.5],
                      "RL actuals AIC" = forc.RLac.AIC[, , qt.5],
                      "RL MK AIC"      = forc.RLmk.AIC[, , qt.5],
                      "RL diff AIC"    = forc.RLdf.AIC[, , qt.5],
                      "VAR AIC"        = forc.endo.AIC[, , qt.5],
                      "DA spot SC"     = forc.spot.SC[, , qt.5],
                      "RL actuals SC"  = forc.RLac.SC[, , qt.5], 
                      "RL MK SC"       = forc.RLmk.SC[, , qt.5],
                      "RL diff SC"     = forc.RLdf.SC[, , qt.5],  
                      "VAR SC"         = forc.endo.SC[, , qt.5],
                      "DA spot"        = t(spot[c((n+1):(n+h-1)), ]),
                      "Trend"          = t(DSC_Forc.mat)
                      )
# compute errors
error.insample = t(sapply(X=c(1:length(median.Fit)), fit.eval, origin = origin, model = median.Fit))


bind.models = list(forc.spot.AIC,
                   forc.RLac.AIC,
                   forc.RLmk.AIC,
                   forc.RLdf.AIC,
                   forc.endo.AIC, 
                   forc.spot.SC, 
                   forc.RLac.SC, 
                   forc.RLmk.SC, 
                   forc.RLdf.SC, 
                   forc.endo.SC)

# add information how often VWAP is within predicted tau range 
error.insample = cbind(error.insample, 
                       "within_tau.01" = c(sapply(X = c(1:length(bind.models)), FUN = function(X){(hit.range(bind.models[[X]], origin, qt.l, qt.u))}), 
                                           rep(NA, 2)),
                       "within_tau.05" = c(sapply(X = c(1:length(bind.models)), FUN = function(X){(hit.range(bind.models[[X]], origin, 2, 6))}), 
                                           rep(NA, 2)),
                       "within_tau.25" = c(sapply(X = c(1:length(bind.models)), FUN = function(X){(hit.range(bind.models[[X]], origin, 3, 5))}), 
                                           rep(NA, 2)))



# assign model specifications
error.insample = data.frame("TS model"  = c(rep("VAR", 10), "DA Spot", "Trend"),
                            "Selection" = c(rep("AIC",5), rep("BIC", 5),  rep(" - ", 2)),
                            "Exogenous Variable" = c(rep(c("DA Spot", "RL actual", "RL forecast", "RL difference", " - "), 2), rep(" - ", 2)),
                            error.insample)

# order models by rmse
error.order    = error.insample[order(unlist(error.insample[, 5]), -unlist(error.insample[, 6]) ), ]

# Table in TeX output 
stargazer(error.order, summary = FALSE, rownames = FALSE, type = "text")




# Plot
# plot of forecast curves with best forecast from VAR SC with DA spot
par(mfrow = c(1,2),cex.lab = 1.2, cex.axis= 1.2, cex.main = 1.2 )
for(a in 2:3){
  fa = n + a
  # organzize the data
  plot.data = array(NA, dim = c(hour, (3 + lt)))
  plot.data[, c(1:3)] = cbind( c(t(vwap[fa, ])),  c(t(spot[fa, ])),  DSC_Forc.mat[a, ])
  
  # here can change the forecast model.
  for (j in c(1:lt) ) {
    plot.data[, (j + 3)] = forc.spot.SC[, a, j]
  }
  
  # plot the data.
  plot(plot.data[, 1], type = "l", col = "red",
       lty = 6,
       ylim = range(plot.data),
       main = rownames(vwap)[fa],
       xaxt = "n",
       xlab = "Hour",
       ylab = "EUR",
       cex.axis = 1.5,
       cex.lab = 1.5, 
       cex.main = 1.5,
       lwd = 2)
  lines(plot.data[, 2], lwd = 2, col = "darkgreen")
  lines(plot.data[, 3], lwd = 2, col = "blue")
  for (j in 1:lt){
    col.exp = ifelse(j == qt.5, "black", "grey")
    lines(plot.data[, (j + 3)], lwd = 2, col = col.exp)
  }
  axis(1, c(1, 7, 13, 19, 24), c("00:00", "06:00", "12:00", "18:00", "23:00"), cex.axis = 1.2)
}
dev.off()
