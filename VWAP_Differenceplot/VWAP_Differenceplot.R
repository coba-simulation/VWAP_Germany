###############################################################################
##                                                                           ##
##          Scatterplot: Price difference vs. Forecast error                 ##
##                                                                           ##
##          1. Scatterplot Price difference vs Forecast error                ##
##                                                                           ##
###############################################################################

# Close windows and clear variables                                                                   
graphics.off()
rm(list = ls(all=TRUE))

# load the R Environment with prepared data
load("VWAP.RData")

# Compute differences
p.diff    = spot - vwap
rl.diff   = con_for - wnd_for -spv_for - rel_act
ren.diff  = spv_for + wnd_for - spv_act - wnd_act


par(mfrow = c(1, 2), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
plot(c(t(p.diff)) ~ c(t(rl.diff)),
     xlab = "RL forecast error (MW)",
     ylab = "Price difference (EUR)")
abline(h = 0, v = 0, col = "grey")

plot(c(t(p.diff)) ~ c(t(ren.diff)),
     xlab = "SPV + WND forecast error (MW)",
     ylab = "Price difference (EUR)")
abline(h = 0, v = 0, col = "grey")
dev.off()
