###############################################################################
##                                                                           ##
##        Preperation and helperfunctions for VWAP_Forecast                  ##
##                                                                           ##
##      This file contains some data preperation and helperfunctions         ##
##      to make sure that the quantlets run                                  ##
##                                                                           ##
###############################################################################

### preperations

rm(list = ls())
graphics.off()

libraries = c("stargazer", "plyr", "moments", "zoo", "forecast", "urca", "expectreg", "fda.usc", "vars",
              "lattice", "tseries", "abind", "sm", "quantreg")
lapply(libraries, function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries, require, quietly = TRUE, character.only = TRUE)

###############################################################################
##                             Data preperation                              ##
##                                                                           ##
##            Remark: Data are artificial and only for                       ##
##                    demonstration that the code works                      ##
###############################################################################

# specifiy paht here:
setwd("C:/Users/Johannes/Documents/GitHub/VWAP_Forecast")
df.data = read.csv("vwap_data.csv")

rel_act = matrix(df.data[,3], ncol = 24, byrow = TRUE, dimnames = list(unique(df.data[,1]), unique(df.data[,2])))
con_act = matrix(df.data[,4], ncol = 24, byrow = TRUE, dimnames = list(unique(df.data[,1]), unique(df.data[,2])))
spv_act = matrix(df.data[,5], ncol = 24, byrow = TRUE, dimnames = list(unique(df.data[,1]), unique(df.data[,2])))
wnd_act = matrix(df.data[,6], ncol = 24, byrow = TRUE, dimnames = list(unique(df.data[,1]), unique(df.data[,2])))
spv_for = matrix(df.data[,7], ncol = 24, byrow = TRUE, dimnames = list(unique(df.data[,1]), unique(df.data[,2])))
wnd_for = matrix(df.data[,8], ncol = 24, byrow = TRUE, dimnames = list(unique(df.data[,1]), unique(df.data[,2])))
con_for = matrix(df.data[,9], ncol = 24, byrow = TRUE, dimnames = list(unique(df.data[,1]), unique(df.data[,2])))
vwap    = matrix(df.data[,10], ncol = 24, byrow = TRUE, dimnames = list(unique(df.data[,1]), unique(df.data[,2])))
spot    = matrix(df.data[,11], ncol = 24, byrow = TRUE, dimnames = list(unique(df.data[,1]), unique(df.data[,2])))



###############################################################################
##                            Holiday Effects                                ##
###############################################################################

## create holiday dummy

# fill in this vector public holidays or other days with special effects.
holiday_dummy = as.numeric(rownames(vwap) %in% c("2015-01-01", "2015-01-02", "2015-01-06", "2015-02-07"))

###############################################################################
##                             Helperfunctions                               ##
###############################################################################


# function to compute fourier series
fourier.series = function(t, terms, period){
  n = length(t)
  X = matrix(NA, nrow = n, ncol = 2*terms)
  for(i in 1:terms){ 
    X[, 2*i-1] = sin(2*pi*i*t/period)
    X[, 2*i]   = cos(2*pi*i*t/period)
  }
  colnames(X) = paste(c("S", "C"), rep(1:terms, rep(2, terms)), sep = "")
  return(X)
}

# function to compute saisonalizde, desaisonalized data and produce forecast
dsc = function(Load_list, WD, PH, n, k, hours, p, model = "no"){
 
  # Create arrays with NA, length of Load_list, to save components
  DT_Load    = array(NA, dim = length(Load_list)) 
  season     = array(NA, dim = length(Load_list)) 
  
  # matrix with daily interval to store day ahead forecast
  season_forecast = matrix(ncol = 1, nrow = hours)
  
  
  # parameters for the nonparametric estiamtes estimates
  nonpar = c("smooth.spline", "sm.regression", "loess")
  days = 365
  year = 1:days
  
  
  for(i in 1:hours){
    # index that will indicate i-th interval of day
    index = as.numeric(seq(from = i, to = (n + k) * hours, by = hours))

    # get load demand from every days i-th qaurter/15-min interval of day
    temp = Load_list[index]
    
    # vector from 1 to length of training period
    t = 1:length(temp)
    
    # check if nonparametric estimation should be conducted
    if (model %in% nonpar){
      
      mat  = matrix(temp)
      mod_year = days - length(temp)%%days
      if(mod_year == max(year)){
         mod_year = 0
         mat = mat
      } else {mat = rbind(mat, matrix(NaN, nrow = mod_year, ncol = 1) )}
      
      YN1 = matrix(data = mat, nrow = days, byrow = FALSE)
      Y1  = rowMeans(YN1, na.rm = TRUE) #mean over years 
      
      if(model == "smooth.spline"){
        fit.mod   = smooth.spline(x = year, y = Y1)
        resid.mod = (mat - rep(fit.mod$y, dim(mat)[1]/days))[1:(length(mat) - mod_year)]
        fit.val   = fit.mod$y
      }
      if(model == "sm.regression"){
        hs        = h.select(x = year, y = Y1, method = "cv")
        fit.mod   = sm.regression(x = year, y = Y1, h = hs, eval.points = year, display = 'none')
        resid.mod = (mat - rep(fit.mod$estimate, dim(mat)[1]/days))[1:(length(mat) - mod_year)]
        fit.val   = fit.mod$estimate
      }

      if(model == "loess"){
        fit.mod   = loess(Y1 ~ year)
        resid.mod = (mat - rep(fit.mod$fitted, dim(mat)[1]/days))[1:(length(mat) - mod_year)]
        fit.val   = fit.mod$fitted
      }
      
      # extract residuals, to get the desaesonalized series, and also the seasonal part
      DT_Load[index] = resid.mod
      season[index]  = rep(fit.val, length = (n + k))

      # forecast
      if (i == hours){
        idx.np = ifelse((n + k + p)%%days == 0, days,  (n + k + p)%%days)
        season_forecast = matrix(season, nrow = hours)[, idx.np]
      }
    }
    if(model == "fourier"){
      # regress load of i-th interval of day on day, dummies for weekdays and dummies for public holiday and fourier series
      ltsc = lm(temp ~ t + WD[1:(n + k), ] + PH[1:(n + k), ]  + fourier.series(t, 4, 365.25))
      # n is training period. p is forecast horizon.
      new = as.data.frame(t(c(1, (max(t) + p), WD[(n + k + p), ], PH[(n + k + p), ],
                              fourier.series((max(t) + p), 4, 365.25))))
    
      # extract residuals, to get the desaesonalized series, and also extract the seasonal part
      DT_Load[index] = ltsc$residuals 
      season[index]  = ltsc$fitted.values

      # extract coefficients from regression
      coeff = ltsc$coefficients
      names(new) = names(coeff)
      season_forecast[i, 1] = sum(coeff * new)
    }
    if(model == "no"){
      # regress load of i-th interval of day on day, dummies for weekdays and dummies for public holiday
      ltsc = lm(temp ~ t + WD[1:(n + k), ] + PH[1:(n + k), ])
      # n is training period. p is forecast horizon.
      new = as.data.frame(t(c(1, (max(t) + p), WD[(n + k + p), ], PH[(n + k + p), ])))
      
      # extract residuals, to get the desaesonalized series, and also extract the seasonal part
      DT_Load[index] = ltsc$residuals 
      season[index]  = ltsc$fitted.values

      # extract coefficients from regression
      coeff = ltsc$coefficients
      names(new) = names(coeff)
      season_forecast[i,1]=sum(coeff * new)
     
    }
  }
  # return deseasonalized series and 1 step ahead forecast 
  return(list("detrended" = DT_Load, "fitted" = season, "forecast" = season_forecast))
}

# function to extract the pc scores
fpca = function(num, fit, x, nh = 4){
  # num= 1 # 1-7
  # fit= fit
  # x = x # equispaced intevalls defined above
  dataResid = Data2fd(x, t(fit[, , num]))
  PCA_Resid = pca.fd(dataResid, nharm = nh, centerfns = FALSE)
  return(PCA_Resid)
}

# function to plot the eigenfunctions of the fpca
plot.PC = function(ind, num, result){
  plot(result[1, ][[num]][ind], ylab = paste("FPC" , ind), xlab = "Hour", cex.lab = 2, cex.axis = 2,lwd = 3,
       ylim = c(-2, 2), xaxt = 'n')
  abline(h = 0, lty = 2)
  axis(1, c(0, 0.25, 0.5, 0.75, 1), c("00:00", "06:00", "12:00", "18:00", "23:00"), cex.axis = 1.5)
}

# function to plot the scores of the fpca
plot.score = function(ind, num, result){
  plot(result[3, ][[num]][, ind], ylab = bquote(alpha[.(ind)]),
       xlab = "Day",
       ylim = c(range(result[3,][[num]])),
       cex.lab = 2, cex.axis = 2,lwd = 3, type = "l")
  abline(h = 0, lty = 2)
}

# function to test for stationarity. ADF, KPSS
stationarity.test = function(x){
  adf.result  = apply(x, 2, adf.test)
  p.adf       = sapply(adf.result, "[[", "p.value")
  kpss.result = apply(x, 2, kpss.test)
  p.kpss      = sapply(kpss.result, "[[", "p.value")
  dd          = length(adf.result)
  result      = array(NA,
                      dim = c(2, dd),
                      dimnames = list(c("ADF", "KPSS"), paste("PC", seq(1:dd), sep = "")))
  result[1:2, ] = rbind(p.adf, p.kpss)
  return(result)
} 

# function to extract PC scores due to certain threshold
pca.man = function(X, tau = .9){
  pca = prcomp(X, center = TRUE, scale = TRUE)
  var = pca$sdev^2
  idx = which(cumsum(var)/sum(var) > tau)[1]
  expl = var/sum(var)
  cum.prop = cumsum(expl)
  scores = pca$x[, 1:idx]
  return(list("scores" = scores,"expl" = expl, "cum.prop" = cum.prop ))
}


# function to apply auto.arima on each score series
fit.auto.arima = function(end, exo, expec = tau[2], tau){
   expec = which(tau %in% expec)
  # for the number of different models
  le = length(exo)
  # pc to include:
  pc = dim(end[[expec]])[2]
  n = dim(end[[expec]])[1]
  for (i in 1:le){
    if (i ==1){result.arr = array(NA, dim = c(n, pc, le))}
    if (i < le){
      for (j in 1:pc){
        result.arr[, j, i] = fitted(auto.arima(end[[expec]][, j], xreg = exo[[i]], ic = "aic", seasonal = FALSE))
      }
    }
    if (i == le){
      for (j in 1:pc){
        result.arr[, j, i] = fitted(auto.arima(end[[expec]][, j], ic = "aic", seasonal = FALSE))
      }
    }
  }
  # reorganize into a list
  fit.scores = alply(result.arr, 3, .dims = TRUE)
  return(fit.scores)
}

# function to apply auto.arima on each score series and produce a forecast for t+1
forc.auto.arima = function(end, exo, expec = tau[2], tau){
  tau = tau
  expec = which(tau %in% expec)
  # for the number of different models
  le = length(exo) 
  # pc to include and dimension
  pc = dim(end[[expec]])[2]
  n = dim(end[[expec]])[1]
  for (i in 1:le){
    if (i ==1){result.arr = array(NA, dim = c(pc, le))}
    if (i < le){
      for (j in 1:pc){
        result.arr[j, i] = forecast(auto.arima(end[[expec]][, j], xreg = exo[[i]][c(1:n), ], ic = "aic", seasonal = FALSE),
                                    xreg = matrix(exo[[i]][(n+1), ], nrow = 1), h = 1)$mean
      }
    }
    if (i == le){
      for (j in 1:pc){
        result.arr[j, i] = forecast(auto.arima(end[[expec]][, j], ic = "aic", seasonal = FALSE), h = 1)$mean
      }
    }
  }
  return(result.arr)
}



# fit the model
fit.curve = function(num, fd, model, mean, season, xsim, startperiod = 10){
  fd = fd[, num]
  fit.model = apply(model[[num]], 1, FUN =  function(x){eval.fd(evalarg = xsim, fdobj = (fd$harmonics[1]*x[1]
                                                                                       + fd$harmonics[2]*x[2]
                                                                                       + fd$harmonics[3]*x[3]
                                                                                       + fd$harmonics[4]*x[4]))})
  
  fit.model = fit.model + mean[, num]
  # compute difference of columns to take lag order into account
  l = abs(dim(fit.model)[2] - dim(season)[2])
  if(l != 0){
    season = season[, -c(1:l)]
    fit = season + fit.model
    fit = fit[, -c(1:(startperiod - l))]
  }
  if (l == 0){
    fit = season + fit.model
  }
  return(fit)
}

fit.eval = function(num, model, origin){
  XX = c(model[[num]] - origin)
  MAE = mean(abs(XX))
  # MSE = mean(XX^2)
  RMSE = sqrt(mean(XX^2))
  return(c("MAE" = MAE, "RMSE" = RMSE))
}


# reconstruct VARX curves with Karhunen - Loeve
KarLoe = function(num, mean, model, fd, xsim, season){
  fd = fd[,num]
  x  = model[[num]]
  curve = eval.fd(evalarg = xsim, fdobj = (fd$harmonics[1]*x[1]
                                           + fd$harmonics[2]*x[2]
                                           + fd$harmonics[3]*x[3]
                                           + fd$harmonics[4]*x[4]))
  f.curve = curve + season + mean[, num]
  colnames(f.curve) = "fcst"
  return(f.curve)              
}

# reconstruct arima curve with Karhunen - Loeve
KarLoe.ar = function(num, model, mean, fd, xsim, season){
  x = model[[num]]
  curve = eval.fd(evalarg =xsim, fdobj = (fd$harmonics[1]*x[1]
                                          + fd$harmonics[2]*x[2]
                                          + fd$harmonics[3]*x[3]
                                          + fd$harmonics[4]*x[4]))
  f.curve = curve + season + mean
  colnames(f.curve) = "fcst"
  return(f.curve)              
}

# function to compute rolling window RMSE
rollingRMSE = function(r.length, origin, forc){
  err2 = (origin - forc)^2
  # for the framework
  for(i in 1:r.length){ 
    if (i == 1){
      num      = array(NA, dim=c((dim(err2)[2] + 1 - r.length), r.length))
      num[, i] = seq(1:(dim(err2)[2] + 1 - r.length))
    }else{
      num[, i] = num[, i - 1] + 1
    }
  }
  periodic.RMSE = array(NA, dim = c(dim(num)[1]))
  # rmse for desired window
  for(i in 1:c(dim(num)[1])){
    #i = 1
    periodic.RMSE[i] = sqrt(mean(err2[, num[i, ]]))
  }
  return(periodic.RMSE)
}

################### functions for FASTEC
# from: https://github.com/QuantLet/FASTEC-with-Expectiles
# mqr, G.qr, simtune
##################### Function: SFISTA algorithm ########################################
mqr = function(Y, X, tau, lambda, kappa = 1e-04, epsilon = 10^(-6), itt = 2000) {
  ### Initialize ################
  m           = ncol(Y)
  n           = nrow(Y)
  p           = ncol(X)
  X2norm      = base::norm(X, type = "2")
  #kappa       = 1e-04  # instead of epsilon/(2*m*n) by theory, like Chen,Lin,Kim,Carbonell and Xing we use fixed kappa, the value we choose here is equivalent to epsilon = 50.
  L           = X2norm^2/(kappa * m^2 * n^2)
  Omega       = matrix(0, nrow = p, ncol = m)
  delta       = 1      # step size
  error       = 1e+07
  L.error     = 1e+10
  it          = 1
  ### Output ###################
  A           = matrix(0, nrow = p, ncol = m)
  A_          = matrix(0, nrow = p, ncol = m)
  ### Main iteration #########
  while (it < itt & error > epsilon) {
    S         = svd(Omega - L^(-1) * G.qr(Omega, Y, X, tau, kappa, m = m, n = n), nu = p, nv = m)
    temp.sv   = S$d - (lambda/L)
    temp.sv[temp.sv < 0] = 0
    A         = S$u %*% diag(temp.sv, nrow = p, ncol = m) %*% t(S$v)
    delta.for = (1 + sqrt(1 + 4 * delta^2))/2
    Omega     = A + (delta - 1)/(delta.for) * (A - A_)
    error     = L.error - (sum((tau - matrix(as.numeric(Y - X %*% A < 0), n, m)) * 
                                 (Y - X %*% A)) + lambda * sum(temp.sv))  # Before: Put abs around to ensure that it is positive (just in case)
    L.error   = sum((tau - matrix(as.numeric(Y - X %*% A < 0), n, m)) * (Y - X %*% 
                                                                           A)) + lambda * sum(temp.sv)
    A_        = A
    delta     = delta.for
    it        = it + 1
    print(c(error, delta, sum((tau - matrix(as.numeric(Y - X %*% A < 0), n, m)) * 
                                (Y - X %*% A)), sum(temp.sv)))
    # if(it < 10){error=1000000}
  }
  list(Gamma = A,
       d = S$d,
       U = S$u,
       V = S$v,
       error = error,
       loss = sum((tau - matrix(as.numeric(Y - X %*% A < 0), n, m)) * (Y - X %*% A)), 
       norm = sum(temp.sv), lambda = lambda, iteration = it)
}
##################### Function: Computing the gradient of the loss function ##############
G.qr = function(A, Y, X, tau, kappa, m, n) {
  W          = (m * n * kappa)^(-1) * (Y - X %*% A)
  index_p    = which(W > tau, arr.ind = TRUE)
  W[index_p] = tau
  index_n    = which(W < tau - 1, arr.ind = TRUE)
  W[index_n] = tau - 1
  temp       = (-t(X) %*% W)/(m * n)
  temp
}

##################### Function: Estimation of penalizing parameter 'lambda' #############
simtune = function(m, tau, XX, alpha = 0.1, B = 500, const = 2) {
  set.seed(1001)
  sim.lambda      = numeric(0)
  n               = nrow(XX)
  for (i in 1:B) {
    W.temp        = matrix(as.numeric(runif(n * m) < tau) - tau, nrow = n, ncol = m)
    temp.score    = norm(t(XX) %*% W.temp/n, type = "F")
    sim.lambda[i] = temp.score/m
    lam           = const*quantile(sim.lambda, p = (1 - alpha))
  }
  list(sim.lambda = sim.lambda, lambda = lam)
}


# add seasonal and estmiated stochastic component 
curve.mqr = function(X, U, D, fitted.model, season, startid){
  curve.mat = X %*% U %*% D %*% t(fitted.model)
  diffs     = startid -1 - (dim(season)[2] - dim(curve.mat)[2])
  # take into account that diffs can be 0
  if(diffs == 0){
    trim.curve = curve.mat
  }else{
    trim.curve = curve.mat[, -c(1:diffs)]
  }
  trim.season = season[, -c(1:(startid-1))]
  result      = trim.curve + trim.season
  return(result)
}

# Add forecasted of seasonal and  stochastic component 
forecast.mqr = function(X, U, D, fitted.model, season){
  curve.mat = X %*% U %*% D %*% sapply(fitted.model[[1]], "[[",1)
  result      = curve.mat + season
  return(result)
}


# Count Percentage of vwap between predicted tau range 
hit.range = function(band.array, origin, lower, upper){
  sum(c(band.array[, , lower]) < c(origin) & c(band.array[, , upper]) > c(origin)) / length(c(band.array[, , upper]))
}

save.image(file="VWAP.RData")

