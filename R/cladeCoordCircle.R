## This code is part of the viper package
## © C. Heibl 2014 (last update 2016-11-29)

#' @export

cladeCoordCircle <- function(lastPP, phy, nodes, align, offset){
  
  nodes <- lapply(nodes, descendants, phy = phy)
  nn <- sapply(nodes, length)
  
  # calculate radius for each clade
  # -------------------------------
  getRadius <- function(id, lPP)
    max(sqrt(lPP$xx[id]^2 + lPP$yy[id]^2))
  radius <- sapply(nodes, getRadius, lPP = lastPP)
  radius <- radius + offset[1]
  if ( align == "all" ) radius <- rep(max(radius), length(radius))
  
  # calculate starting position for each clade
  # ------------------------------------------
  startpos <- vector(length = length(nodes))
  for (i in seq(along = nodes))
    startpos[i] <- (min(nodes[[i]]) - 1) * 2 * pi/lastPP$Ntip
  
  # calculate radian for each clade
  # -------------------------------
  ang <- (nn - 1) / lastPP$Ntip
  x <- cbind(ang, 1 - ang)
  x <- apply(x, 1, function(x) c(0, cumsum(x)/sum(x)))
  x <- t(x)

  data.frame(x, radius = radius, startpos = startpos)
}