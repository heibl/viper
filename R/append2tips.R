## This code is part of the ips package
## © C. Heibl 2014 (last update 2018-03-26)

#' @title Append Symbols or Text to Tip Labels
#' @description Add text or symbols to the tip labels of a phylogenetic tree.
#' @param phy An object of class \code{\link[ape]{phylo}}.
#' @param tips A vector of mode \code{"numeric"}, giving the tip numbers to
#'   which append text or symbols. If missing, all tips are used.
#' @param offset Used to control the distance of text or symbols from
#'   the tips of the phylogenetic tree. Can either be of mode
#'   \code{"numeric"} or \code{"character"}. In the latter case the width of the
#'   string is calculated and used as offset.
#' @param align Logical, if \code{TRUE} text or symbols will be aligned.
#' @param grid Logial, if \code{TRUE} dotted lines are plotted as a horizontal
#'   grid to aid visual orientation.
#' @param col A character string, giving the color for text or symbols.
#' @param text A character string, providing text to be appended to the tip
#'   labels.
#' @param pch An integer, specifying the symbol used (see \code{\link{points}}).
#' @param cex A real number, giving the \strong{c}haracter \strong{ex}pension
#'   (see \code{\link{par}}). If missing, \code{cex} is taking from the previous
#'   call of \code{\link[ape]{plot.phylo}}.
#' @param legend A character string to be used as a column legend.
#' @param legend.srt A vector of mode \code{"numeric"} to rotate \code{legend}.
#' @param legend.cex A real number, giving the \strong{c}haracter
#'   \strong{ex}pension for the legend.
#' @param ... Further graphical parameters (from par), such as srt, family and
#'   xpd.
#' @details Currently, \code{append2tips} is will work only for
#'   \code{\link[ape]{plot.phylo}} using types \code{"phylogram"} or
#'   \code{"cladogram"} and directions \code{"rightwards"} or
#'   \code{"leftwards"}.
#' @seealso \code{\link{plot.phylo}}, \code{\link{tiplabels}},
#'   \code{\link{mixedFontBinom}}
#' @examples
#' # phylogeny of true vipers
#' # ------------------------
#' #data(viperidae)
#' #plot.phylo(viperidae, cex = 0.9, label.offset = 0.01)
#'
#' # append geographical origin of accessions:
#' # -----------------------------------------
#' #txt <- origin[, 2]
#' #append2tips(viperidae, text = txt)
#'
#' # If the appended labels - like in the example above -
#' # exceed the plotting region, a warning is issued and it
#' # is suggested a new value for x.lim:
#' # -----------------------------------
#' #plot.phylo(viperidae, cex = 0.9, label.offset = 0.01, x.lim = 1.1)
#' #append2tips(viperidae, text = txt)
#'
#' # a lot of combinations are possible:
#' # -----------------------------------
#' #plot.phylo(viperidae, cex = 0.8, label.offset = 0.02, x.lim = 1.5)
#' #NTips <- length(viperidae$tip.label)
#' #append2tips(viperidae, text =
#' #	paste("A", 1:NTips, sep = "-"), col = "orange")
#' #append2tips(viperidae, tips <- c(5, 7:9, 14),
#' #	text = rep("xxx", 5), col = "slateblue")
#' #append2tips(viperidae, pch = 23, col = rainbow(NTips))
#' #append2tips(viperidae, pch = 21, col = "grey50",
#' #   align = TRUE, grid = TRUE)
#'
#' # ... it also works when actual tip labels are omitted:
#' # -----------------------------------------------------
#' #plot.phylo(viperidae, show.tip.label = FALSE, x.lim = 40,
#' #	cex = 0.8, type = "clado", use.edge.length = FALSE)
#' #txt <- origin[, 2]
#' #append2tips(viperidae, text = txt)
#'
#' # ... it can be combined with taxo.fonts:
#' # -----------------------------------------------------
#' #plot.phylo(viperidae, show.tip.label = FALSE, x.lim = 1.5)
#' #taxo.fonts(viperidae)
#' #txt <- origin[, 2]
#' #append2tips(viperidae, text = txt, offset = 0.1, grid = TRUE)
#' @importFrom ape Ntip
#' @importFrom graphics lines points strwidth
#' @export

append2tips <- function (phy, tips, offset, align = FALSE, grid = FALSE,
                         col = "red", text = NULL, pch = NULL, cex, 
                         legend, legend.srt = 90, legend.cex = cex, ...) {
  
  ## check tree and set tree-related paramters
  ## -----------------------------------------
  if (!inherits(phy, "phylo")) 
    stop("object 'phy' is not of class 'phylo'")
  ntip <- Ntip(phy)
  alltips <- 1:ntip
  if (missing(tips)) tips <- alltips
  
  ## parameters of previous call to plot.phylo
  ## -----------------------------------------
  lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  print(lastPP)
  
  ## check if type is supported
  ## --------------------------
  if (!lastPP$type %in% c("phylogram", "cladogram"))
    stop("currently only types 'phylogram' and 'cladogram' supported")
  
  ## set direction-related parameters
  ## --------------------------------
  dir <- lastPP$direction
  if (dir %in% c("upwards", "downwards"))
    stop("currently only directions 'rightwards' and 'leftwards' supported")
  d.sign <- ifelse(dir %in% c("rightwards", "upwards"), 1, -1)
  minmax <- ifelse(dir %in% c("rightwards", "upwards"), 2, 1)
  adj1 <- ifelse(dir %in% c("rightwards", "upwards"), 0, 1)
  crds <- c("xx", "yy")
  crds.id <- ifelse(dir %in% c("rightwards", "leftwards"), 1, 2)
  new.list.element <- paste(crds[crds.id], "new", sep = ".")
  srt <- ifelse(dir %in% c("rightwards", "leftwards"), 0, 90)
  
  ## Set label offset
  ## ----------------
  if (missing(offset)){
    offset <- lastPP$label.offset
  } else {
    if (is.character(offset))
      offset <- strwidth(offset, cex = lastPP$cex, font = lastPP$font)
  }
  
  ## Set missing arguments
  ## ---------------------
  if (missing(cex)) cex <- lastPP$cex
  
  ## Width for present tiplabels ('labelSpace') 
  ## and those to append ('symbolSpace')
  ## -----------------------------------------------
  if (lastPP$show.tip.label){
    labelSpace <- lastPP$label.offset + 
      strwidth(gsub("_", " ", phy$tip.label[tips]), 
               cex = lastPP$cex, font = lastPP$font)
  } else {
    labelSpace <-  rep(0, length(tips))
  }
  symbolSpace <- rep(0, length(tips))
  if (!is.null(pch)) symbolSpace <- strwidth("O", cex = cex)
  if (!is.null(text)) symbolSpace <- strwidth(text, cex = cex)
  space3 <- strwidth("-", "user", cex, lastPP$font)
  
  ## Coordinates for plotting
  ## ------------------------
  if (dir %in% c("rightwards", "upwards")){
    gord <- 1:2
  } else {
    gord <- 2:1
  }
  if (any(c("rightwards", "leftwards") %in% dir)){
    ord <- 1:2
  } else {
    ord <- 2:1
  }
  
  ## per.xy: coordinates perpendicular to edges
  ## ---------------------------------------
  if (is.null(lastPP[[new.list.element]])){
    per.xy <- lastPP[[crds[crds.id]]]
    per.xy[tips] <- per.xy[tips] + d.sign * (labelSpace + offset)
  } else {
    per.xy <- lastPP[[new.list.element]]
    per.xy[tips] <- per.xy[tips] + offset
  }
 
  print(per.xy)
  
  if (align) {
    per.xy.inner <- per.xy ## for plotting of grid
    per.xy[tips] <- range(per.xy[tips])[minmax]
  }
  
  ## Plot symbols
  ## ------------ 
  if (!is.null(pch)){ 
    per.xy[tips] <- per.xy[tips] + d.sign * strwidth("-", "user", cex)
    cds <- cbind(per.xy[tips], tips)[, ord]
    points(cds[, 1], cds[, 2], pch = pch, col = col, bg = col, 
           cex = cex, ...)
  }
  ## Plot text
  ## ---------
  if (!is.null(text)){
    cds <- cbind(per.xy[tips], tips)[, ord]
    text(cds[, 1], cds[, 2], text, col = col, cex = cex, adj = c(adj1, 0.5), 
         srt = srt, ...)
    per.xy[tips] <- per.xy[tips] + d.sign * strwidth(text, "user", cex, ...)
  }
  ## Plot grid
  ## ---------
  if (grid){
    abzug <- strwidth("-", "user", cex) * c(1, -1)# * d.sign
    for (i in tips) {
      xx <- c(per.xy.inner[i], per.xy[i])[gord] + abzug
      if (diff(xx) > 0) lines(x = xx, y = rep(tips[i], 2), lty = 3)
    }    
  }
  ## Plot legend
  ## -----------
  if (!missing(legend)){
    y <- nrow(cds)
    text(x = cds[y, 1], y = y + (1 * .5), labels = legend,
         cex = legend.cex, srt = legend.srt, adj = c(0, .5), xpd = NA)    
  }
  
  ## Update and return 'last_plot.phylo'
  ## -----------------------------------
  if (max(per.xy) > max(lastPP[[paste(substr(crds[crds.id], 1, 1), "lim", sep = ".")]]))
    warning("The appended text exceeds the limit of the plotting region.",
            "\n  Consider setting 'x.lim' in 'plot.phylo' to at least ", 
            round(max(per.xy), 5), ".")
  lastPP[[new.list.element]] <- per.xy
  lastPP$offset <- space3
  assign("last_plot.phylo", lastPP, envir = .PlotPhyloEnv)
  invisible(lastPP)
}