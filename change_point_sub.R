#CHANGEPOINT DETECTION - CHANGEPOINT.ONLINE PROJECT FOR R

# Daniel Morales
# dm9450@gmail.com

# Task: 
# Download and install the changepoint and ecp packages.
# Write a for loop to analyze a data set with an increasing number of data points.
# Graph the ouput adding a new timepoint in each iteration of the loop and updating the best changepoint locations.

#install.packages('changepoint')
#install.packages('ecp')
#install.packages('mvtnorm')
#install.packages('RColorBrewer')
#install.packages('matrixStats')

require("ecp")
require("RColorBrewer")
require("MatrixStats")
require("ggplot2")
require("mvtnorm")
require("changepoint")

setwd("~/Desktop/Projects/change_point")
n = 20
colors = brewer.pal(8, "Set2")
obv1 <- matrix(rmvnorm(n, mean=c(0,0), sigma=diag(2)))
obv2 <- matrix(rmvnorm(n, mean=c(2,2), sigma=matrix(c(0.5,0,0,0.5), nrow = 2)))
obv3 <- matrix(rmvnorm(n, mean=c(-1,0), sigma=matrix(c(0.2,0.1,0.1,0.2), nrow = 2)))
samples <- c(obv1, obv2, obv3)

increasing_obv <- c()
ix <- 100
for(ob in samples){
  name = paste0("obv",ix,"_change_point.png")
  ix <- ix + 1
  #png(name, width = 1600, height = 800)
  increasing_obv <- c(increasing_obv, ob)
  changes <- e.divisive(as.matrix(increasing_obv), alpha=0.1, sig.lvl = 0.01)$estimates
  change_ix <- changes[-1]
  changes <- change_ix[1:length(change_ix)-1]
  
  plot(increasing_obv, type="l", col=colors[1], ylim=c(-4,7), ylab="X", xlab="Time")
  for(ch in changes){
    abline(v=ch, col="red", lty=2)
  }
  #dev.off()
}



