## This code is part of the ips package
## © C. Heibl 2014 (last update 2018-09-13)

#' @title Automatic Placement of Edgelabels
#' @description Try to label edges in a plotted phylogeny in a way that edges
#'   and labels do not cross.
#' @param phy An object of class \code{\link[ape]{phylo}}.
#' @param text A vector of mode \code{"character"} containing the label text,
#'   e.g. clade names, geographic range, etc.
#' @param edge A vector of mode \code{"numeric"} containing the number of the edges to be labelled.
#' @param fine.tune A list ....
#' @param ... Further arguments passed to \code{\link{edgelabels}}.
#' @return Nothing, \code{smartEdgelabels} is called for its side effect.
#' @export

smartEdgelabels <- function(phy, text, edge, fine.tune, ...){
  
  lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
  x <- data.frame(text = text[order(edge)],
                  edge = sort(edge),
                  stringsAsFactors = FALSE)
  x$nodes <- phy$edge[x$edge, 2]
  x$xx <- lastPP$xx[x$nodes]
  x$yy <- lastPP$yy[x$nodes]
  
  ## Determine if label should be placed above
  ## or below edge
  ## -------------
  placeAbove <- function(phy, e){
    # e_pos <- which(phy$edge[, 2] == e)
    e_pos <- e
    anc <- phy$edge[e_pos, 1]
    sister_pos <- which(phy$edge[, 1] %in% anc)
    sister_pos <- setdiff(sister_pos, e_pos)
    e_pos > sister_pos
  }
  x$above <- sapply(x$edge, placeAbove, phy = phy)
  x$adj2 <- rep(1.2, nrow(x))
  x$adj2[x$above] <- -0.2
  x$adj1 <- 1.05
  
  
  ## User defined fine tuning
  ## ------------------------
  if (!missing(fine.tune)){
    for (h in seq_along(fine.tune)){
      id <- x$text == names(fine.tune)[h]
      children <- phy$edge[phy$edge[, 1] == x$nodes[id], 2]
      child <- ifelse(x$above[id], tail(children, 1), head(children, 1))
      x$yy[id] <- lastPP$yy[child]
      if (fine.tune[[h]] == "l"){
        x$adj1[id] <- 0
      } else {
        children2 <- phy$edge[phy$edge[, 1] == child, 2]
        child2 <- ifelse(x$above[id], tail(children2, 1), head(children2, 1))
        x$xx[id] <- lastPP$xx[child]
      }
      
    }
  }
  
  for (i in 1:nrow(x)){
    # print(x$text[i])
    # print(x$adj2[i])
    # nodelabels(x$text[i], x$node[i], frame = "none", 
    #            adj = c(1.05, x$adj2[i]), ...)
    text(x = x$xx[i],
         y = x$yy[i],
         adj = c(x$adj1[i], x$adj2[i]),
         labels = x$text[i], ...)
  }
  invisible(x)
}