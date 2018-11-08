## This code is part of the ips package
## © C. Heibl 2014 (last update 2018-04-04)

#' @title PDF of Large Phylogenies
#' @description Easily and quickly produce a PDF file showing a large phylogeny.
#' @param phy An object of class \code{\link[ape]{phylo}}.
#' @param lo Numeric, giving the label offset.
#' @param height Either \code{"auto"} or numeric giving the PDF height in
#'   inches.
#' @param cex Numeric, giving the character expansion for the tip labels.
#' @param edge.label A data.frame with two columns; the taxon names in the first
#'   column (\code{taxon}) are drawn on the edge numbers given by the second
#'   column (\code{edge}).
#' @param view Logical, idicating if the PDF should be opened immediately in the
#'   standard PDF viewer.
#' @param save Logical, indicating if the PDF file should be written to
#'   \code{file}.
#' @param file A character string giving the filename; only has an effect if
#'   \code{save = TRUE}.
#' @return None.
#' @seealso \code{\link{plot.phylo}}
#' @examples
#' tr <- rtree(1000)
#' bigtree(tr, save = FALSE)
#' @importFrom ape edgelabels nodelabels plot.phylo tiplabels
#' @export

bigtree <- function(phy, lo = 5, height = "auto",
                    cex = 0.1, edge.label,
                    view = TRUE, save = FALSE,
                    file = "bigtree.pdf"){
  
  
  if (height == "auto"){
    nt <- Ntip(phy)
    height <- nt/7
  }
  pdf(file, height = 46.81, width = 33.11)
  plot.phylo(phy, no.margin = TRUE,
       edge.width = .25,
       label.offset = lo, 
       cex = cex,
       type = "clado",
       use.edge.length = FALSE)
  if (!missing(edge.label)) edgelabels(edge.label$taxon, edge.label$edge, cex = cex)
  nodelabels(cex = cex, adj = c(1.1, -.3), frame = "n", col = "red")
  tiplabels(cex = cex, adj = c(-.25, .5), frame = "n", col = "blue")
  dev.off()
  if (view) system(paste("open", file))  
  if (!save) unlink(file)
}