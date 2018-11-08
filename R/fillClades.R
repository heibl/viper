## This code is part of the viper package
## © C. Heibl 2014 (last update 2018-07-13)

#' @title Highlight Clades in a Phylogeny
#' @description Highlights one or more monophyletic groups (clades) in a
#'   phylogeny by filling them with color.
#' @param phy An object of class \code{\link{phylo}}.
#' @param nodes A vector of mode \code{"numeric"} giving the most recent common
#'   ancestor (MRCA) node of the clades to be highlighted. You can use
#'   \code{\link{noi}} to obtain this vector.
#' @param col A vector of mode \code{"character"} giving the color(s) of the
#'   clades; will be receycled if appropriate; default is \code{"grey"}.
#' @param text A vector of mode \code{"character"}, giving (optional) text
#'   labels for clades; must be of the same length as \code{nodes}. The labels
#'   will be positioned right justified and vertically centered.
#' @param text.cex Numeric, giving the character expansion (size) of the text
#'   labels.
#' @param open.angle Numeric, giving the the angle left blank in degrees and has
#'   to match the value used in \code{\link{plot.phylo}} when choosing a
#'   circular plot type (\code{"fan"} or \code{"radial"}); for other plot types
#'   this has no effect.
#' @details On 2018-01-17, I added the optional plotting of labels. To this end,
#'   in non-ultrametric trees the tip-ward side of the polygons has to be
#'   aligned. This has only been tested for rightward phylograms. Maybe boxes
#'   should be extended vertically a bit to include tiplabels.And maybe the code
#'   should be organized a bit more systematically.
#' @export

fillClades <- function(phy, nodes, col, text, text.cex = 1, open.angle = 0){
  
  
  ## Initial checks and adjustments
  ## ------------------------------
  if (!inherits(phy, "phylo")) 
    stop("object 'phy' is not of class 'phylo'")
  lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  if (lastPP$direction %in% c("rightwards", "leftwards")){
    coord.id <- 1:2
  } else {
    coord.id <- 2:1 
  }
  print(lastPP)
  
  crds <- c("xx", "yy")
  cid <- ifelse(lastPP$direction %in% c("rightwards", "leftwards"), 1, 2)
  new.list.element <- paste(crds[cid], "new", sep = ".")
  new <- lastPP[[crds[cid]]]

  ## Filling is possible only for clades that
  ## consist of more than one species
  ## --------------------------------
  nt <- Ntip(phy) ## number of terminal nodes
  if (!missing(text)) text <- text[nodes > nt]
  nodes <- nodes[nodes > nt]
  nc <- length(nodes) ## number of clades
  
  ## Calculate radiant between two tips
  ## ----------------------------------
  unit.rad <- (2 * pi - (open.angle * pi / 180)) / nt
  # unit.rad <- (2 * pi ) / nt
  
  ## set colors
  if (missing(col)) col <- rep("grey90", length(nodes))
  if (length(col) < nc) 
    col <- rep(col, ceiling(nc / length(col)))
  
  ## Tentative patch for color ordering [2018-07-04]
  if (!missing(text)) text <- text[order(nodes)]
  nodes <- sort(nodes)
  print(nodes)
  names(col) <- names(nodes)
  print(dput(col))
  
  ## Loop over nodes
  ## ---------------
  plotting_choords <- list(length = length(nodes))
  if (lastPP$type %in% c("phylogram", "cladogram")){
    for (i in seq_along(nodes)){
      
      ## identify all nodes that lie on the border
      ## of the clade polygon: 
      ## un=upper border nodes, ln=lower border nodes
      un <- ln <- nodes[i]
      repeat {
        # un <- c(un, phy$edge[phy$edge[, 1] == tail(un, 1), 2][2])
        un <- c(un, tail(phy$edge[phy$edge[, 1] == tail(un, 1), 2], 1)) # fix for polytomies, 
                                                                    # requires sequential node ordering
        if ( tail(un, 1) <= nt ) break
      }
      un <- un[un != nodes[i]]
      
      repeat {
        ln <- c(ln, phy$edge[phy$edge[, 1] == tail(ln, 1), 2][1])
        if ( tail(ln, 1) <= nt ) break
      }
      ln <- rev(ln[ln != nodes[i]])
      
      int <- descendants(phy, nodes[i])
      int <- head(int[-1], -1)
      
      ## coordinates of polygon
      ## ----------------------
      xy.id <- cbind(c(nodes[i], 
                       rep(head(un, -1), each = 2), 
                       tail(un, 1), rev(int), head(ln, 1),
                       rep(tail(ln, -1), each = 2), 
                       nodes[i]),
                     c(rep(un, each = 2),
                       rev(int),
                       rep(ln, each = 2)))
      xy.id <- cbind(xy.id, xy.id[, 1] == xy.id[, 2])
      xy.id[, 3] <- xy.id[, 3] & xy.id[, 2] <= nt
      
      ## coordinates of polygon
      ## ----------------------
      co <- data.frame(x = lastPP$xx[xy.id[, coord.id[1]]],
                       y = lastPP$yy[xy.id[, coord.id[2]]],
                       tip = xy.id[, 3])
      
      ## UNDER DEVELOPMENT: add labels
      ## -----------------------------
      if (!missing(text)){
        tiplabelSpace <- string.dim(phy$tip.label, cex = lastPP$cex, 
                                 font = lastPP$font, srt = lastPP$srt)[, cid]
        tiplabelSpace <- tiplabelSpace + lastPP$label.offset
        labelSpace <- string.dim(text[i], cex = text.cex, 
                                 font = lastPP$font, srt = lastPP$srt)[, cid]
        labelSpace <- tiplabelSpace + labelSpace + lastPP$label.offset
        co$x[co$tip == 1] <- co$x[co$tip == 1] + labelSpace[xy.id[co$tip == 1, 2]]
        co$x[co$tip == 1] <- max(co$x[co$tip == 1])
        new[xy.id[co$tip == 1, 2]] <- co$x[co$tip == 1]
      }
      
      plotting_choords[[i]] <- co
      
      
        
      ## draw polygon
      ## ------------
      polygon(co$x, co$y, border = NA, col = col[i])
      if (!missing(text))
      text(x = max(co$x), y = mean(co$y), labels = text[i], cex = text.cex, adj = c(1, .5))
    } 
  }
  if (lastPP$type %in% "fan"){
    for (i in seq_along(nodes)){
      
      ## identify all nodes that lie on the border
      ## of the clade polygon: 
      ## un=upper border nodes, ln=lower border nodes
      un <- ln <- nodes[i]
      repeat {
        # un <- c(un, phy$edge[phy$edge[, 1] == tail(un, 1), 2][2]) ## works only for dichotomies! [2018-07-04]
        un <- c(un, tail(phy$edge[phy$edge[, 1] == tail(un, 1), 2], 1)) ## should also work for polytomies
        if ( tail(un, 1) <= nt ) break
      }
      un <- un[un != nodes[i]]
      
      repeat {
        ln <- c(ln, phy$edge[phy$edge[, 1] == tail(ln, 1), 2][1])
        if ( tail(ln, 1) <= nt ) break
      }
      ln <- rev(ln[ln != nodes[i]])
      
      int <- descendants(phy, nodes[i])
      int <- head(int[-1], -1)
      
      xy <- data.frame(nodes = c(ln, nodes[i], un, rev(int)))
      
      ## Calculate radius on which nodes are plotted
      ## -------------------------------------------
      getRadius <- function(node, lPP)
        max(sqrt(lPP$xx[node]^2 + lPP$yy[node]^2))
      
      xy$radius <- sapply(xy$nodes, getRadius, lPP = lastPP)
      
      ## Calculate radiant on which nodes are plotted
      ## --------------------------------------------
      getRadiant <- function(n, phy, d){
        dd <- phy$edge[phy$edge[, 1] == n, 2]
        rr <- d$radiant[d$nodes %in% dd]
        ifelse(any(is.na(rr)), NA, mean(rr))
      }
      d <- descendants(phy, nodes[i], type = "a")
      d <- data.frame(nodes = c(nodes[i], d), 
                      radiant = NA)
      terminal <- which(d$nodes <= nt)
      d$radiant[terminal] <- unit.rad * (d$nodes[terminal] - 1)
      while (any(is.na(d$radiant))){
        id <- which(is.na(d$radiant) )
        d$radiant[id] <- sapply(d$nodes[id], getRadiant, 
                                phy = phy, d = d)
      }
      xy$radiant <- d$radiant[match(xy$nodes, d$nodes)] 
      
      ## x-y coordiates
      xy$x <- lastPP$xx[xy$nodes]
      xy$y <- lastPP$yy[xy$nodes]
      
      ## Intercalate segment endpoints
      nn <- nrow(xy)
      id <- 1:(nn - 1)
      id <- data.frame(id, id + 1)
      ic <- apply(id, 1, function(z, n, nt) any(n[z] > nt), 
                  n = xy$nodes, nt = nt)
      id <- 1:nn
      for (j in 1:(nn - 1)){
        if (ic[j]) {
          alter <- (j + 1):nn
          id[alter] <- id[alter] + 1
        }
      }
      temp <- xy
      xy <- rep(NA, max(id))
      xy <- data.frame(nodes = xy, radius = xy,
                       radiant = xy, x = xy, y = xy)
      xy$nodes[id] <- temp$nodes
      xy$radius[id] <- temp$radius
      xy$radiant[id] <- temp$radiant
      xy$x[id] <- temp$x
      xy$y[id] <- temp$y
      
      ## fill in missing values: mv
      mv <- which(is.na(xy$nodes))
      mrca <- which(xy$nodes == nodes[i])
      for (j in mv){
        ## wttf: where to take radius and radiant from
        rt <- ifelse(j < mrca, -1, 1)
        rs <- ifelse(j < mrca, 1, -1)
        xy$x[j] <- cos(xy$radiant[j + rt]) * xy$radius[j + rs] 
        xy$y[j] <- sin(xy$radiant[j + rt]) * xy$radius[j + rs] 
      }
      
      ## Draw polygon
      ## ------------
      xy <- rbind(xy, xy[1, ])
      polygon(xy$x, 
              xy$y, 
              border = NA,
              col = col[i])
      
      ## Draw label
      ## ----------
      
    } 
  }
  
  # Update and return 'last_plot.phylo'
  # -----------------------------------
  if (lastPP$show.tip.label){
    if (lastPP$type %in% "fan"){ ## 2018-07-04
      warning("update of last_plot.phylo not yet implemented for type = 'fan'")
    } else {
      ## The tips of the filled clade have been modified already inside the loop.
      ## Now we need to get and modify the tips lying outside the filled clades:
      tips_outside <- do.call(rbind, plotting_choords)
      tips_outside <- tips_outside$y[tips_outside$tip == 1]
      tips_outside <- setdiff(1:nt, tips_outside)
      tiplabelSpace <- string.dim(phy$tip.label[tips_outside], 
                                  cex = lastPP$cex, 
                                  font = lastPP$font, 
                                  srt = lastPP$srt)[, cid]
      tiplabelSpace <- tiplabelSpace + lastPP$label.offset
      new[tips_outside] <- new[tips_outside] + tiplabelSpace
      lastPP[[new.list.element]] <- new
      assign("last_plot.phylo", lastPP, envir = .PlotPhyloEnv)
      invisible(lastPP)
    }
  }
}