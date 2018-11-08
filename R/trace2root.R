trace2root <- function(phy, tip, what = "nodes"){
  
  what <- match.arg(what, c("nodes", "edges", "branch.lengths", 
                     "sum"))
  lineage <- tip
  while ( tip != Ntip(phy) + 1 ){
    tip <- phy$edge[phy$edge[, 2] == tip, 1]
    lineage <- c(lineage, tip)
  }
  if ( what == "edges" ){
    lineage <- which(phy$edge[, 2] %in% lineage)
  }
  if ( what == "branch.lengths" ){
    lineage <- phy$edge.length[phy$edge[, 2] %in% lineage]
  }
  if ( what == "sum" ){
    lineage <- sum(phy$edge.length[phy$edge[, 2] %in% lineage])
  }
  lineage
}