###############################################################################
##                                                                           ##
##       VWAP FPCA Test. R-Script to apply fpca model from trainperiod to    ##
##       the test data.                                                      ##
##       Evaluation and plots after estimation procedure                     ##
##                                                                           ##
###############################################################################

# load the R Environment with prepared data
load("VWAP.RData")
libraries = c("stargazer", "plyr", "moments", "zoo","forecast","urca","expectreg","fda.usc", "vars","lattice","tseries", "abind","sm","quantreg")
lapply(libraries,function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries,require,quietly=TRUE,character.only=TRUE)

###############################################################################
# set some prarameter
n             = 65                 # Train Data length
hour          = 24                 # number of avialable daily observations
tau           = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99) # expectiles of interest
model         = "fourier"          # estimation of seasonal component
                                   # "fourier" includes also dummies for weekday and holidays
                                   # "no" only includes weekday and holidays
                                   # "smooth.spline", "sm.regression", "loess" do not include holiday and weekday effects
trsh          = .95                # threshold how mouch should be explained by exogenous variables; apply PCA
lag.l         = 3                  # (lag.l-1) determines allowed maximum lag length for VAR models.chose fiteen to allow for past two weeks as impact; 30days window: 3, 60 days window 5  
p             = 1                  # parameter demanded by for seasonality forecast
k             = 0                  # kk is used to set increasing steps, only needed to vary in testperiod
lt            = length(tau)        # number of expectils
exp.5         = which(tau %in% 0.5)# index of 0.5 expectile
exp.l         = which(tau %in% min(tau))
exp.u         = which(tau %in% max(tau))
holiday_dummy = holiday_dummy      # dummy variable indicating public holidays
h             = dim(vwap)[1] - n  # number of forecasts
window        = 65                # window length, how many days should be included in the VAR/ARIMA model
#################################################################################

start = Sys.time()
for (k in 0:(h-2)){
  print(paste("k is ", k, sep = ""))
  
  if(k == 0){
    # container to store results from estimations
    # by SC/BIC
    SC.spot.arr = array(NA, dim = c((h - 1), hour, lt))
    SC.RLac.arr = array(NA, dim = c((h - 1), hour, lt))
    SC.RLmk.arr = array(NA, dim = c((h - 1), hour, lt))
    SC.RLdf.arr = array(NA, dim = c((h - 1), hour, lt))
    SC.endo.arr = array(NA, dim = c((h - 1), hour, lt))
    
    # by AIC
    AIC.spot.arr = array(NA, dim = c((h - 1), hour, lt))
    AIC.RLac.arr = array(NA, dim = c((h - 1), hour, lt))
    AIC.RLmk.arr = array(NA, dim = c((h - 1), hour, lt))
    AIC.RLdf.arr = array(NA, dim = c((h - 1), hour, lt))
    AIC.endo.arr = array(NA, dim = c((h - 1), hour, lt))
    
    # by arima modelling
    arima.spot.arr = array(NA, dim = c((h - 1), hour))
    arima.RLac.arr = array(NA, dim = c((h - 1), hour))
    arima.RLmk.arr = array(NA, dim = c((h - 1), hour))
    arima.RLdf.arr = array(NA, dim = c((h - 1), hour))
    arima.endo.arr = array(NA, dim = c((h - 1), hour))
    
    # by Trendmodel
    DSC_Forc.mat = array(NA, dim = c((h - 1), hour))
  }
  
  # input endogen
  Input_list     = c(t(vwap[1:(n + k), ]))
  
  # input exogen
  Input_list_ex1 = c(t(spot[1:(n + k + p), ]))
  Input_list_ex2 = c(t(con_act[1:(n + k + p), ]))
  Input_list_ex3 = c(t(spv_act[1:(n + k + p), ]))
  Input_list_ex4 = c(t(wnd_act[1:(n + k + p), ]))
  Input_list_ex5 = c(t(con_for[1:(n + k + p), ]))
  Input_list_ex6 = c(t(spv_for[1:(n + k + p), ]))
  Input_list_ex7 = c(t(wnd_for[1:(n + k + p), ]))
  
  
  # DSC: construct dummies for deseasonalization. wd and ph are only zeros
  # because sun and wind do not care for weekend or public holidays
  tempseq = as.numeric(rep(rep(c(1:7), each = 1), length.out = n + k + 2))
  WD = sapply(1:6, FUN = function(num){as.numeric(tempseq == num)})
  wd = matrix(rep(0, prod(dim(WD)))           , ncol = dim(WD)[2])
  holiday_dummy = as.matrix(holiday_dummy, ncol = 1)
  ph      = matrix(rep(0, (prod(dim(holiday_dummy)))))
  
  

  # the seasonal component
  DSC_Forc.mat[(k + p), ] = dsc(Input_list, WD, PH = holiday_dummy, n = n, k = k, hours = hour, p = p, model = model)$forecast
  
  # compute desaisonalized series of prices
  resid_data_yearly = matrix(dsc(Input_list, WD = WD, PH = holiday_dummy, n = n, k = k, hours = hour, p = p, model = model)[[1]], nrow = hour)
  ex1_data_yearly   = matrix(dsc(Input_list_ex1, WD = WD, PH = holiday_dummy, n = n + p, k = k, hours = hour, p = p, model = model)[[1]], nrow = hour)
  
  # actual residual load components
  DSC_Input_ex2   = dsc(Input_list_ex2, WD = WD, PH = holiday_dummy, n = n + p, k = k, hours = hour, p = p, model = model)[[1]]
  DSC_Input_ex3   = dsc(Input_list_ex3, WD = wd, PH = ph, n = n + p, k = k, hours = hour, p = p, model = model)[[1]]
  DSC_Input_ex4   = dsc(Input_list_ex4, WD = wd, PH = ph, n = n + p, k = k, hours = hour, p = p, model = model)[[1]]
  ex2_data_yearly = matrix(DSC_Input_ex2 - DSC_Input_ex3 - DSC_Input_ex4, nrow = hour)
  
  ## forecasted residual laod components
  DSC_Input_ex5   = dsc(Input_list_ex5, WD = WD, PH = holiday_dummy, n = n + p, k = k, hours = hour, p = p, model = model)[[1]]
  DSC_Input_ex6   = dsc(Input_list_ex6, WD = wd, PH = ph, n = n + p, k = k, hours = hour, p = p, model = model)[[1]]
  DSC_Input_ex7   = dsc(Input_list_ex7, WD = wd, PH = ph, n = n + p, k = k, hours = hour, p = p, model = model)[[1]]
  ex3_data_yearly = matrix(DSC_Input_ex5 - DSC_Input_ex6 - DSC_Input_ex7, nrow = hour)
  
  # difference in residual load (actuals - forecast)
  ex4_data_yearly = ex2_data_yearly - ex3_data_yearly
  
  # compute Principal Components of the explanatory variables
  PCA_ex  = list( pca.man(t(ex1_data_yearly), tau = trsh),
                  pca.man(t(ex2_data_yearly), tau = trsh),
                  pca.man(t(ex3_data_yearly), tau = trsh),
                  pca.man(t(ex4_data_yearly), tau = trsh))
  
  
  # get the scores for the window 
  scores.ex = lapply(lapply(PCA_ex, "[[", "scores" ), "[",c((n + k + p- window):(n + k + p)),TRUE)
  #scores.elbow = lapply(scores.ex, "[", TRUE, 1)
  #scores.ex = scores.elbow
  
  # set up for estimation procedure. xsim are the knots, mfit provides 3d array to store results from expectile sheet
  xsim  = seq(1/nrow(resid_data_yearly), 1, length.out = nrow(resid_data_yearly)) 
  

  if (k == 0){
    mfit  = array(0, dim = c(window, hour, lt)) # hour rows, traindata length cols, slices as length of tau
    # Compute expectile sheet for every day in window
    for (i in 1:window){
      mfit[i, , ] = expectreg.ls(resid_data_yearly[, (n - window + i)] ~ rb(xsim, "pspline", center = FALSE),
                                 estimate = "sheet", expectiles = tau, smooth = "gcv")$fitted
      }
  }else{
    # fist, expectile regression on new day, keep window length
    y    = expectreg.ls(resid_data_yearly[, (n + k)] ~ rb(xsim, "pspline", center = FALSE),
                        estimate = "sheet", expectiles = tau, smooth = "gcv")
    # add result to mfit and drop first one.
    yfit = array(y$fitted, dim = c(1, hour, length(tau)))
    mfit = abind(mfit[-1, , ], yfit, along = 1)
  }
  
  # mean function of estimated expectiles
  for (i in 1:hour){
    if (i == 1){exp.mean = array(NA, dim=c(hour,lt))}
    for (j in 1:lt){
      exp.mean[i,j] = mean(mfit[,i,j] )
    }
  }
  
  # subtract mean series
  center.mfit = sweep(mfit, 2:3, exp.mean)
  
  
  # compute the pc for the first 4
  result  = sapply(X = c(1:lt), FUN = fpca, fit = center.mfit, x = xsim, nh = 4)
  
  # extract the scores for each expectile. Get a list with 3 
  scores  = result[3,]
  
  # assign names to scores
  for (i in 1:length(scores)){
    colnames(scores[[i]]) = paste0("FPC", seq(1, 4))
  }
  
  # add a further element to the list containing nothing in order to estimate VAR/Arima without exogen variables
  scores.ex[(length(scores.ex) + 1)] = list(NULL)
  
  # compute forecasts for the scores
  arima.forc.50 = as.list(data.frame(forc.auto.arima(scores, scores.ex, expec = tau[exp.5], tau = tau)))
  
  curve.arima = lapply(X = c(1:5), KarLoe.ar, fd = result[, exp.5], mean = exp.mean[, exp.5], model = arima.forc.50, season = DSC_Forc.mat[(k + p), ], xsim = xsim)
  arima.spot.arr[(k + 1), ] = curve.arima[[1]]
  arima.RLac.arr[(k + 1), ] = curve.arima[[2]] 
  arima.RLmk.arr[(k + 1), ] = curve.arima[[3]]
  arima.RLdf.arr[(k + 1), ] = curve.arima[[4]]
  arima.endo.arr[(k + 1), ] = curve.arima[[5]]
  
  
  crit = c("SC", "AIC")
  for (IC in crit){
    # estimate models
    #IC = "SC"
    model.spot  = lapply(X = c(1:lt), FUN = function(X){VAR(y = scores[[X]], exogen = scores.ex[[1]][-(window + p), ], lag.max = (lag.l - 1), type = "const", ic = IC)})
    model.RLac  = lapply(X = c(1:lt), FUN = function(X){VAR(y = scores[[X]], exogen = scores.ex[[2]][-(window + p), ], lag.max = (lag.l - 1), type = "const", ic = IC)})#, lag.max = (lag.l - 1), type = "const", ic = IC)})
    model.RLmk  = lapply(X = c(1:lt), FUN = function(X){VAR(y = scores[[X]], exogen = scores.ex[[3]][-(window + p), ], lag.max = (lag.l - 1), type = "const", ic = IC)})
    model.RLdf  = lapply(X = c(1:lt), FUN = function(X){VAR(y = scores[[X]], exogen = scores.ex[[4]][-(window + p), ], lag.max = (lag.l - 1), type = "const", ic = IC)})
    model.endo  = lapply(X = c(1:lt), FUN = function(X){VAR(y = scores[[X]], lag.max = (lag.l - 1), type = "const", ic = IC)})
    
    # compute forecast
    pred.spot = lapply(X =c(1:lt), FUN = function(X){predict(model.spot[[X]], n.ahead = p, dumvar = matrix(scores.ex[[1]][(window + p), ], nrow = p))})
    pred.RLac = lapply(X =c(1:lt), FUN = function(X){predict(model.RLac[[X]], n.ahead = p, dumvar = matrix(scores.ex[[2]][(window + p), ], nrow = p))})
    pred.RLmk = lapply(X =c(1:lt), FUN = function(X){predict(model.RLmk[[X]], n.ahead = p, dumvar = matrix(scores.ex[[3]][(window + p), ], nrow = p))})
    pred.RLdf = lapply(X =c(1:lt), FUN = function(X){predict(model.RLdf[[X]], n.ahead = p, dumvar = matrix(scores.ex[[4]][(window + p), ], nrow = p))})
    pred.endo = lapply(X =c(1:lt), FUN = function(X){predict(model.endo[[X]], n.ahead = p)})
    
    
    # get the predicted scores
    forc.spot = lapply(X = c(1:lt), FUN = function(X){unlist(lapply(pred.spot[[X]]$fcst, "[[", 1))})
    forc.RLac = lapply(X = c(1:lt), FUN = function(X){unlist(lapply(pred.RLac[[X]]$fcst, "[[", 1))})
    forc.RLmk = lapply(X = c(1:lt), FUN = function(X){unlist(lapply(pred.RLmk[[X]]$fcst, "[[", 1))})
    forc.RLdf = lapply(X = c(1:lt), FUN = function(X){unlist(lapply(pred.RLdf[[X]]$fcst, "[[", 1))})
    forc.endo = lapply(X = c(1:lt), FUN = function(X){unlist(lapply(pred.endo[[X]]$fcst, "[[", 1))})
    
    # recompute curves with Karhunen Loeve and seasonal component
    if (IC == "SC"){
      SCfcurve.spot = lapply(X = c(1:lt), KarLoe, fd = result, mean = exp.mean, model = forc.spot, season = DSC_Forc.mat[(k + p), ], xsim = xsim)
      SCfcurve.RLac = lapply(X = c(1:lt), KarLoe, fd = result, mean = exp.mean, model = forc.RLac, season = DSC_Forc.mat[(k + p), ], xsim = xsim)
      SCfcurve.RLmk = lapply(X = c(1:lt), KarLoe, fd = result, mean = exp.mean, model = forc.RLmk, season = DSC_Forc.mat[(k + p), ], xsim = xsim)
      SCfcurve.RLdf = lapply(X = c(1:lt), KarLoe, fd = result, mean = exp.mean, model = forc.RLdf, season = DSC_Forc.mat[(k + p), ], xsim = xsim)
      SCfcurve.endo = lapply(X = c(1:lt), KarLoe, fd = result, mean = exp.mean, model = forc.endo, season = DSC_Forc.mat[(k + p), ], xsim = xsim)
    }
    if (IC == "AIC"){
      AICfcurve.spot = lapply(X = c(1:lt), KarLoe, fd = result, mean = exp.mean, model = forc.spot, season = DSC_Forc.mat[(k + p), ], xsim = xsim)
      AICfcurve.RLac = lapply(X = c(1:lt), KarLoe, fd = result, mean = exp.mean, model = forc.RLac, season = DSC_Forc.mat[(k + p), ], xsim = xsim)
      AICfcurve.RLmk = lapply(X = c(1:lt), KarLoe, fd = result, mean = exp.mean, model = forc.RLmk, season = DSC_Forc.mat[(k + p), ], xsim = xsim)
      AICfcurve.RLdf = lapply(X = c(1:lt), KarLoe, fd = result, mean = exp.mean, model = forc.RLdf, season = DSC_Forc.mat[(k + p), ], xsim = xsim)
      AICfcurve.endo = lapply(X = c(1:lt), KarLoe, fd = result, mean = exp.mean, model = forc.endo, season = DSC_Forc.mat[(k + p), ], xsim = xsim)
    }
  }
  SC.spot.arr[(k + 1), , ] = matrix(unlist(SCfcurve.spot), nrow = hour)
  SC.RLac.arr[(k + 1), , ] = matrix(unlist(SCfcurve.RLac), nrow = hour)
  SC.RLmk.arr[(k + 1), , ] = matrix(unlist(SCfcurve.RLmk), nrow = hour)
  SC.RLdf.arr[(k + 1), , ] = matrix(unlist(SCfcurve.RLdf), nrow = hour)
  SC.endo.arr[(k + 1), , ] = matrix(unlist(SCfcurve.endo), nrow = hour)
  
  AIC.spot.arr[(k + 1), , ] = matrix(unlist(AICfcurve.spot), nrow = hour)
  AIC.RLac.arr[(k + 1), , ] = matrix(unlist(AICfcurve.RLac), nrow = hour)
  AIC.RLmk.arr[(k + 1), , ] = matrix(unlist(AICfcurve.RLmk), nrow = hour)
  AIC.RLdf.arr[(k + 1), , ] = matrix(unlist(AICfcurve.RLdf), nrow = hour)
  AIC.endo.arr[(k + 1), , ] = matrix(unlist(AICfcurve.endo), nrow = hour)
} # end forecast loop
Sys.time() - start

###############################################################################
##                        Plot and Evaluation                                ##
##                                                                           ##
##       Table:  Model performance due to RMSE                               ##
##       Plot:   Daily Curves with forecast from best model                  ##
##                                                                           ##
###############################################################################

# Table: 
# evaluate estimation
origin  = matrix(unlist(as.data.frame(vwap[c((n + p):(n + p + k)), ])), nrow = (h - 1))
or.spot = matrix(unlist(as.data.frame(spot[c((n + p):(n + p + k)), ])), nrow = (h - 1))

# forecast of models at 50% expectile
bind.50    = list("DA spot AIC"      = AIC.spot.arr[, , exp.5],
                  "RL actuals AIC"   = AIC.RLac.arr[, , exp.5],
                  "RL MK AIC"        = AIC.RLmk.arr[, , exp.5],
                  "RL diff AIC"      = AIC.RLdf.arr[, , exp.5],
                  "VAR AIC"          = AIC.endo.arr[, , exp.5],
                  "DA spot SC"       = SC.spot.arr[, , exp.5],
                  "RL actuals SC"    = SC.RLac.arr[, , exp.5],
                  "RL MK SC"         = SC.RLmk.arr[, , exp.5],
                  "RL diff SC"       = SC.RLdf.arr[, , exp.5],
                  "VAR SC"           = SC.endo.arr[, , exp.5],
                  "DA spot Arima"    = arima.spot.arr,
                  "RL actuals Arima" = arima.RLac.arr,
                  "RL MK Arima"      = arima.RLmk.arr,
                  "RL diff Arima"    = arima.RLdf.arr, 
                  "Arima"            = arima.endo.arr,
                  "DA spot"          = or.spot, 
                  "Trend"            = DSC_Forc.mat)

error.eval   = t(sapply(X = c(1:length(bind.50)), fit.eval, origin = origin, model = bind.50))
TS.model = c(rep("VAR",10), rep("ARIMA",5), "DA Spot", "Trend")
Exogen.info = c(rep(c("DA Spot", "RL actual", "RL forecast", "RL difference", " - "),3), rep(" - ", 2))
Lag.select = c(rep("AIC",5), rep("BIC", 5), rep("AIC",5), rep(" - ", 2))

error.eval = data.frame("TS model" = TS.model, "Selection Criteria" = Lag.select, "Exogenous Variable" = Exogen.info, error.eval)

bind.models = list(AIC.spot.arr,
                   AIC.RLac.arr,
                   AIC.RLac.arr,
                   AIC.RLdf.arr, 
                   AIC.endo.arr, 
                   SC.spot.arr, 
                   SC.RLac.arr, 
                   SC.RLac.arr, 
                   SC.RLdf.arr, 
                   SC.endo.arr)

error.insample = cbind(error.eval, 
                       "within_tau.01" = c(sapply(X = c(1:length(bind.models)), FUN = function(X){(hit.range(bind.models[[X]], origin, exp.l, exp.u))}), 
                                   rep(NA, 7)),
                       "within_tau.05" = c(sapply(X = c(1:length(bind.models)), FUN = function(X){(hit.range(bind.models[[X]], origin, 2, 6))}), 
                                           rep(NA, 7)),
                       "within_tau.25" = c(sapply(X = c(1:length(bind.models)), FUN = function(X){(hit.range(bind.models[[X]], origin, 3, 5))}), 
                                           rep(NA, 7)))


error.order    =  error.insample[order(unlist(error.insample[, 5]), -unlist(error.insample[, 6])), ]
stargazer(error.order, summary = FALSE, rownames = FALSE, type = "text")



# Plot
# plot of forecast curves with best forecast from VAR SC with DA spot
par(mfrow = c(1,2),cex.lab = 1.2, cex.axis= 1.2, cex.main = 1.2 )
for(a in 2:3){
  fa = n + a
  # organzize the data
  plot.data = array(NA, dim = c(hour, (3 + lt)))
  plot.data[, c(1:3)] = cbind( c(t(vwap[fa, ])),  c(t(spot[fa, ])),  DSC_Forc.mat[a, ])
  # here can change the forecast model of interest 
  for (j in c(1:lt) ) {
    plot.data[, (j + 3)] = SC.spot.arr[a, , j]
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
    col.exp = ifelse(j == exp.5, "black", "grey")
    lines(plot.data[, (j + 3)], lwd = 2, col = col.exp)
  }
  axis(1, c(1, 7, 13, 19, 24), c("00:00", "06:00", "12:00", "18:00", "23:00"), cex.axis = 1.2)
}
dev.off()


