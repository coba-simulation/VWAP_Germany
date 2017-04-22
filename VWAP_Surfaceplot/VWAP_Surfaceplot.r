###############################################################################
##                                                                           ##
##                Surfaceplot: VWAP and Residual load                        ##
##                                                                           ##
###############################################################################

load("VWAP.RData")
libraries = c("lattice")
lapply(libraries, function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries, require, quietly = TRUE, character.only = TRUE)

# VWAP png
wireframe(as.matrix(vwap), 
          xlab = list("Day", rot = 0, cex = 1.3),
          ylab = list("Hour", rot = 0, cex = 1.3), 
          zlab = list(" VWAP in EUR", rot= 90, cex = 1.3),
          aspect = c(0.6, 0.6),
          main = list("Surface VWAP", cex = 1.5),
          drape = TRUE,
          colorkey = TRUE,
          col.regions = colorRampPalette(c("black", "red", "yellow", "springgreen", "royalblue", "blue"))(100),
          screen=list(x = -90, y = 120, z = 0),
          trellis.par.set(list(axis.text=list(cex=1.5))))



# residual load
wireframe(as.matrix(rel_act), 
          xlab = list("Day", rot = 0, cex = 1.3),
          ylab = list("Hour", rot = 0, cex = 1.3), 
          zlab = list(" Residual Load in EUR", rot= 90, cex = 1.3),
          aspect = c(0.6, 0.6),
          main = list("Surface Residual Load", cex = 1.5),
          drape = TRUE,
          colorkey = TRUE,
          col.regions = colorRampPalette(c("black", "red", "yellow", "springgreen", "royalblue", "blue"))(100),
          screen=list(x = -90, y = 130, z = 0),
          trellis.par.set(list(axis.text=list(cex=1.5))))






