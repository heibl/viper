cladeCoordinates <- function(lastPP, phy, nodes, align){
  
  ## set direction-related parameters
  ## --------------------------------
  dir <- lastPP$direction
  d.sign <- ifelse(dir %in% c("rightwards", "upwards"), 1, -1)
  tipsID <- ifelse(dir %in% c("rightwards", "leftwards"), 
                   "tipsX", "tipsY")
  maxmin <- ifelse(dir %in% c("rightwards", "upwards"), 2, 1)
  cid <- ifelse(dir %in% c("rightwards", "leftwards"), 1, 2)
  
  d <- lapply(nodes, descendants, phy = phy)
  tips <- unlist(d)
  crown <- rep(nodes, sapply(d, length))
  subtending <- function(phy, nodes){
    phy$edge[phy$edge[, 2] %in% nodes, 1]
  }
  stem <- sapply(nodes, subtending, phy = phy)
  stem <- rep(stem, sapply(d, length))
  
  
  ## There are 3 groups of columsn: stem, crown, and tip,
  ## which give the coordinates for beginning (stem) and 
  ##  the ending (crown) of the clade's stem lineage and
  ## the beginning of the tip labels
  
  ## TEST: if xx.new is available it should be used for tips.*
  ## ---------------------------------------------------------
  xx <- ifelse("xx.new" %in% names(lastPP), "xx.new", "xx")
  obj <- data.frame(stem, stemX = lastPP$xx[stem],
             stemY = lastPP$yy[stem], 
             crown, crownX = lastPP$xx[crown],
             crownY = lastPP$yy[crown], 
             tips, tipsX = lastPP[[xx]][tips],
             tipsY = lastPP$yy[tips])
  
  ## space: x-coordinates of the end of the tiplabels
  ## ------------------------------------------------
  if (lastPP$show.tip.label & xx == "xx"){
    labelSpace <- string.dim(phy$tip.label[tips], 
                             cex = lastPP$cex, 
                             font = lastPP$font, 
                             srt = lastPP$srt)[, cid]
    labelSpace <- labelSpace + lastPP$label.offset
  } else {
    labelSpace <- rep(lastPP$label.offset, length(tips))
  }
  obj[tipsID] <- obj[tipsID] + labelSpace * d.sign
  
  if ( align == "all" ) {
    obj[tipsID] <- range(obj[tipsID])[maxmin]
  }
  obj
}