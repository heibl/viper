## This code is part of the ips package
## © C. Heibl 2014 (last update 2018-03-26)

#' @importFrom graphics axis lines mtext polygon strwidth
#' @export

barplotPhylo <- function (phy, x, tips, offset, grid = FALSE,
                          col, border, more.plots = 0, space,
                          text = NULL, pch = NULL, cex, ...) {
  ## parameters of previous call to plot.phylo
  ## -----------------------------------------
  lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  if ( missing(offset) ) offset <- lastPP$label.offset
  if ( missing(cex) ) cex <- lastPP$cex
  
  ## check tree and set tree-related paramters
  ## -----------------------------------------
  if ( !inherits(phy, "phylo") ) 
    stop("object 'phy' is not of class 'phylo'")
  if ( !lastPP$type %in% c("phylogram", "cladogram") )
    stop("currently only types 'phylogram' and 'cladogram' supported")
  ntip <- Ntip(phy)
  alltips <- 1:ntip
  if ( missing(tips) ) tips <- alltips
  
  ## reorder x to match phy
  ## ----------------------
  ## to do: make check explicit!
  if (is.vector(x)) x <- data.frame(x)
  test <- union(setdiff(rownames(x), phy$tip.label), 
                setdiff(phy$tip.label, rownames(x)))
  if ( length(test) > 0 ) stop("'phy' and 'x' do not match")
  id <- match(phy$tip.label, rownames(x))
  x <- x[id, , drop = FALSE]
  
  ## set direction-related parameters
  ## --------------------------------
  dir <- lastPP$direction
  if ( dir %in% c("upwards", "downwards") )
    stop("currently only directions 'rightwards' and 'leftwards' supported")
  d.sign <- ifelse(dir %in% c("rightwards", "upwards"), 1, -1)
  minmax <- ifelse(dir %in% c("rightwards", "upwards"), 2, 1)
  adj1 <- ifelse(dir %in% c("rightwards", "upwards"), 0, 1)
  crds <- c("xx", "yy")
  crds.id <- ifelse(dir %in% c("rightwards", "leftwards"), 1, 2)
  new.list.element <- paste(crds[crds.id], "new", sep = ".")
  srt <- ifelse(dir %in% c("rightwards", "leftwards"), 0, 90)
  
  ## coordinates for plotting
  ## ------------------------
  if (  dir %in% c("rightwards", "upwards") ){
    gord <- 1:2
  } else {
    gord <- 2:1
  }
  if ( any(c("rightwards", "leftwards") %in% dir) ){
    ord <- 1:2
  } else {
    ord <- 2:1
  }
  
  ## per.xy: coordinates perpendicular to edges
  ## ------------------------------------------
  if ( is.null(lastPP[[new.list.element]]) ){
    per.xy <- lastPP[[crds[crds.id]]]
    labelSpace <- strwidth(phy$tip.label, 
                           cex = lastPP$cex, 
                           font = lastPP$font)
    labelSpace <- labelSpace + lastPP$label.offset 
    per.xy[alltips] <- per.xy[alltips] + d.sign * labelSpace
    
  } else {
    per.xy <- lastPP[[new.list.element]]
  }
  if ( !missing(space) ) per.xy <- per.xy + space
  per.xy.inner <- per.xy # for grid
  negativeWidth <- ifelse(any(x[, 1] < 0), abs(min(x[, 1])), 0)
  plotWidth <- (lastPP$x.lim[2] - per.xy[1]) * 1/(more.plots + 1)
  valueWidth <- diff(range(x[, 1]))
  per.xy <- per.xy + negativeWidth * plotWidth/valueWidth 
  per.xy[tips] <- range(per.xy[tips])[minmax]
  
  ## plot bars
  ## ---------
  if ( length(col) == 1 ) col <- rep(col, Ntip(phy))
  if ( !is.null(pch) ){ 
    per.xy[tips] <- per.xy[tips] + d.sign * strwidth("-", "user", cex)
    cds <- cbind(per.xy[tips], tips)[, ord]
    xright <- x[, 1] * (plotWidth / valueWidth)
    xright <- xright + per.xy[tips]
    for ( i in seq_along(cds[, 1]) ){
      polygon(x = rep(c(cds[i, 1], xright[i]), each = 2),
              y = rep(cds[i, 2], 4) + c(-0.3, 0.3, 0.3, -0.3),
              col = col[i], border = border[i])
    }
    
  }
  ## plot grid
  ## ---------
  if ( grid ){
    abzug <- strwidth("-", "user", cex) * c(1, -1)# * d.sign
    for ( i in tips ) {
      xx <- c(per.xy.inner[i], per.xy[i])[gord] + abzug
      if ( diff(xx) > 0 ) lines(x = xx, y = rep(tips[i], 2), lty = 3)
    }    
  }
  ## plot axis
  ## ---------
  labels <- range(x[, 1])
  if ( all(labels >= 0) ){
    labels[1] <- 0
    at <- c(min(cds[, 1]), max(xright))
  } else {
    at <- c(min(xright), max(xright))
  }
  axis(side = 1, at = at, labels = round(labels, digits = 1), cex.axis = cex)
  mtext(text, side = 1, line = 2, at = mean(at), cex = cex)
  
  ## update and return 'last_plot.phylo'
  ## -----------------------------------
  if ( max(per.xy) > max(lastPP[[paste(substr(crds[crds.id], 1, 1), "lim", sep = ".")]]) )
    warning("The appended drawing exceeds the limit of the plotting region.",
            "\n  Consider setting 'x.lim' in 'plot.phylo' to at least ", 
            round(max(per.xy), 5), ".")
  
  ext <- string.dim("AAA", 
                    cex = lastPP$cex, 
                    font = lastPP$font,
                    srt = lastPP$srt)
  lastPP[[new.list.element]] <- per.xy + plotWidth + ext[1]
  assign("last_plot.phylo", lastPP, envir = .PlotPhyloEnv)
  invisible(lastPP)
}