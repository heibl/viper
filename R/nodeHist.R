## This code is part of the viper package
## © C. Heibl 2014 (last update 2018-04-04)

#' @export

nodeHist <- function(x, bottom, top, col = "#FEE090"){
  
  lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
  bt <- top - bottom
  counts <- bt * x$counts / max(x$counts) + bottom
  
  if (lastPP$direction %in% c("rightwards", "leftwards")) {
    if (lastPP$direction == "rightwards"){
      maxi <- max(lastPP$xx)
      coords.id <- 1:2
    } else {
      maxi <- min(lastPP$xx)
    }
  } else {
    if (lastPP$direction == "upwards"){
      maxi <- max(lastPP$yy)
      coords.id <- 2:1
    } else {
      maxi <- min(lastPP$yy)
    }
  }
  for (i in seq_along(x$mids)) {
    xy.id <- cbind(c(rep(maxi - x$breaks[i], 2),
                     rep(maxi - x$breaks[i + 1], 2)),
                   c(bottom, counts[i], counts[i], bottom))
    
    polygon(xy.id[, coords.id[1]], 
            xy.id[, coords.id[2]], 
            col = col, xpd = NA)
  }  
}

# plot(phy, dir = "up", x.lim = c(0, 28), show.tip.label = FALSE)
# nodeHist(x = x, bottom = 28, top = 25, col = "skyblue")
# axisChrono(side = 4)
# 
# plot(phy, x.lim = c(-3, 25), show.tip.label = FALSE)
# nodeHist(x = x, bottom = -3, top = 0, col = "skyblue")
# axisChrono(line = 1.5)
