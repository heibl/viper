## This code is part of the viper package
## © C. Heibl 2014 (last update 2018-03-26)

#' @title Highlight Age Intervals in Chronogram
#' @description Highlight age intervals in a chronogram.
#' @param phy An object of class \code{phylo}.
#' @param age A vector of mode \code{"numeric"} giving an age or an age range.
#' @param clade An integer giving the the node number of the MRCA of a clade to
#'   which the timeframe should be restricted.
#' @param tips A vector of integers giving the numbers of tips to which the time
#'   frame should be restricted. Can be used for non-monophyletic groups.
#' @param \dots Further arguments.
#' @return None, \code{add.timeframe} is called for its side effects.
#' @seealso \code{\link{add.geoscale}}, \code{\link{noi}}
#' @examples 
#' # phylogentic relationships of bird orders:
#' data(bird.orders)
#' 
#' # plot tree:
#' # setting edge and tip colors to zero (invisible) improves
#' # resolution 
#' plot(bird.orders, edge.color = 0, tip.color = 0)
#' 
#' # highlight some geological event, etc. ...
#' add.timeframe(bird.orders, age = c(12, 14), col = "skyblue",
#'               border = NA)
#'               
#' # restrict to certain clade ...
#'cl <- noi(bird.orders, c("Struthioniformes", "Anseriformes"))
#'
#' add.timeframe(bird.orders, age = c(16:21), col = "pink",
#'               border = NA, clade = cl)
#'               
#' # or to some other non-monophyletic assemby:
#' add.timeframe(bird.orders, age = c(17:24), col = "yellow",
#'               border = NA, tips = 6:8)
#'
#' # now plot tree again:
#' # arguments must be exactly the same of course, except for
#' # edge and tip colors
#' plotPhyloUpon(bird.orders)
#' @importFrom ape branching.times
#' @importFrom graphics lines rect
#' @importFrom ips descendants
#' @importFrom utils head tail
#' @export

add.timeframe <- function(phy, age, clade = NULL, tips = NULL, ...){

	# get coordinates and number of tip labels of
	# preceeding 'plot.phylo' call
	# ----------------------------
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
	ntips <- lastPP$Ntip
	rootage <- branching.times(phy)[1]
	
	# clade
	if (!is.null(clade)){
		extension <- lastPP$cex/2
		d <- head(descendants(phy, clade), 1) - extension
		dd <- tail(descendants(phy, clade), 1) + extension
		yy <- sort(c(d, dd))
	}												
	else {
		if (is.null(tips))
			yy <- c(1, ntips)							
		else yy <- tips
		# adjust overshoot
		# ----------------
		yext <- 0.01 * ntips
		yy[1] <- yy[1] - yext # ybottom
		yy[2] <- yy[2] + yext # ytop
	}
	
	# plot line ...
	if (length(age) == 1){
		xx <- rep(rootage - age, 2)
		lines(xx, yy, ...)
	}
	# or rectangle
	else { 
		xleft <- rootage - max(age)
		xright <- rootage - min(age)
		rect(xleft, yy[1], xright, yy[2], ...)
	}
}
