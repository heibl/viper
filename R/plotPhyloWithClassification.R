## This code is part of the viper package
## © C. Heibl 2015 (last update 2018-09-24)

#' @title Plot Phylogentic Tree with Clade Highlighting
#' @description Plots a phylogenetic tree together with color highlighting of
#'   the underlying taxonomic classification.
#' @param phy An object of class \code{\link{phylo}}.
#' @param tax An object of class \code{\link{megapteraProj}} or a taxonomic
#'   table in parent-child format; if missing, the taxonomy is taken from the
#'   tip labels.
#' @param outgroup A vector of mode \code{"character"}. Outgroup will not be
#'   shown, if \code{outgroup = "dont.show"}.
#' @param min.support An integer giving a threshold for support values. Edges
#'   with lower support values will be collapsed into polytomies, if
#'   \code{collapes.unsupported = TRUE}.
#' @param collapse.unsupported Logical
#' @param genus.monophyly Logical indicating if monophyletic genera should be highlighted.
#' @param genus.col A vector of mode \code{"character"} giving a color for the filling of
#'   boxes drawn around monophyletic genera.
#' @param edges A vector of mode \code{"character"}.
#' @param higher.rank A vector of mode \code{"character"}.
#' @param tags A data frame with two columns: the first column contains tip
#'   labels as contained in phy, the second columns contains the labels with
#'   will be appended (within square bracket) to the corresponding tip labels.
#' @param xex A numeric, the x.lim extension; try larger values if parts of the
#'   plot are truncated at the right side of the plot.
#' @param pdf.height An integer ...
#' @param pdf.width An integer ...
#' @param title A vector of mode \code{"character"}.
#' @param file A vector of mode \code{"character"}.
#' @details \code{collapse.unsupported} assumes that the support values are
#'   found in \code{phy$node.label}; no edges will be collapsed, if this list
#'   element does not exist.
#' @importFrom ape add.scale.bar compute.brlen drop.tip is.monophyletic
#'   ladderize plot.phylo root
#' @importFrom ips collapseUnsupportedEdges fixNodes noi tipHeights
#' @importFrom grDevices dev.off pdf
#' @export

plotPhyloWithClassification <- function(phy, tax, outgroup = "dont.show", 
                                        min.support, 
                                        collapse.unsupported = TRUE,
                                        genus.monophyly = TRUE,
                                        genus.col = "grey95",
                                        edges, 
                                        higher.rank,
                                        higher.rank.cex = 0.85,
                                        tags, 
                                        xex = 1.25,
                                        pdf.height = "auto",
                                        pdf.width = 20,
                                        title, file){
  
  ## Check input data
  ## ----------------
  if (!inherits(phy, "phylo")) stop("'phy' is not of class 'phylo'")
  if (all(phy$nodelabel == "")) phy$node.label <- NULL
  
  ## If no taxonomy is given, take it from tip labels
  ## ------------------------------------------------
  if (missing(tax)){
    tax <- speclist2taxdump(phy$tip.label)
  }
  
  ## Does function run within a megaptera framework?
  ## ----------------------------------------------
  megaptera <- ifelse(inherits(tax, "megapteraProj"), TRUE, FALSE)
  
  ## Parent-child table of taxonomy
  ## ------------------------------
  if (megaptera){
    og <- tax@taxon@outgroup # get outgroup before replacing 'tax'
    tax <- dbReadTaxonomy(tax, subset = phy)
  } 
  tax$taxon <- gsub(" ", "_", tax$taxon) # use underscore
  if (!megaptera) tax <- taxdumpSubset(tax, species = phy$tip.label)
  
  ## Assess tip rank
  ## --------------
  tip_rank <- unique(tax$rank[tax$taxon %in% phy$tip.label])
  if (length(tip_rank) > 1) stop("implement me!")
  
  ## Modify tree
  ## -----------
  if (outgroup == "dont.show" & all(gsub(" ", "_", unlist(og)) %in% tax$taxon)) {
    
    ## Identify outgroup
    ## -----------------
    if (megaptera){
      og <- lapply(og, taxdumpChildren, tax = tax,
                   tip.rank = tip_rank, quiet = TRUE)
      og <- og[!sapply(og, is.null)]
      og <- do.call(rbind, og)
      og <- og$taxon[og$rank == tip_rank]
    }
    
    if (is.monophyletic(phy, og)) {
      phy <- root(phy, og)
      phy <- drop.tip(phy, og)
    } else {
      phy <- drop.tip(phy, og)
    }
    tax <- taxdumpDropTip(tax, og)
  }
  
  ## Collapse edges with support lower than 'min.support'
  ## -----------------------------------------------------
  if (collapse.unsupported & !is.null(phy$node.label))
    phy <- collapseUnsupportedEdges(phy, cutoff = min.support)
  
  #######################################################
  ## From here on tree topology does not change any more!
  #######################################################
  
  phy <- ladderize(phy)
  phy <- fixNodes(phy)
  tipnames <- phy$tip.label
  
  ## Monophyly of genera
  ## -------------------
  ecol <- tcol <- "black"
  if (tip_rank == "species" & genus.monophyly){
    gen <- data.frame(gen = strip.spec(phy$tip.label),
                      spec = phy$tip.label,
                      stringsAsFactors = FALSE)
    gen <- split(gen$spec, gen$gen)
    ## consider only genera with more than one species
    gen <- gen[sapply(gen, length) > 1]
    if (length(gen)){
      id <- sapply(gen, is.monophyletic, phy = phy)
      if (missing(edges)){
        ecol <- edge.color(phy, gen[id], col = "#2166ac")
      } else {
        ecol <- edge.color(phy, edges, col = "orange")
      }
      
      tcol <- tip.color(phy, gen[!id], regex = TRUE, 
                        col = rep("#b2182b", length(which(!id))))
      mp.gen <- noi(phy, gen[id])
      # n.spec <- sapply(gen[id], length)
      gen.names <- names(mp.gen)
      # gen.names[n.spec == 1] <- ""
      
      ## for monophyletic genera with more than 1 species,
      ## display only epithet:
      g <- gen.names[gen.names != ""]
      g <- phy$tip.label %in% unlist(gen[g])
    } else {
      genus.monophyly <- FALSE
    }
  } 
  
  ## Colors for higher ranks
  ## -----------------------
  if (!missing(higher.rank)){
    
    higher.rank <- tax$taxon[tax$rank == higher.rank]
    spec <- lapply(higher.rank, taxdumpChildren, tax = tax, tip.rank = tip_rank)
    spec <- lapply(spec, function(z) z$taxon[z$rank == tip_rank])
    spec <- lapply(spec, gsub, pattern = " ", replacement = "_")
    names(spec) <- higher.rank
    id <- sapply(spec, is.monophyletic, phy = phy)
    higher.rank <- noi(phy, spec[id])
  } else {
    higher.rank <- NULL
  }
  
  ## Truncate tip labels of monophyletic genera to epitheta
  ## ------------------------------------------------------
  if (tip_rank == "species" & genus.monophyly){
    phy$tip.label[g] <- gsub("^.+_", "", phy$tip.label[g]) ## cf. line 44
    
    ## Append tags to tip labels
    ## -------------------------
    if (!missing(tags)){
      tags[, 1] <- gsub(" ", "_", tags[, 1])
      tags <- tags[tags[, 1] %in% tipnames, ]
      tag <- rep("", length(tipnames))
      tag[match(tags[, 1], tipnames)] <- tags[, 2]
      tag[g] <- gsub("^.+_", "", tag[g])
      tag[tag != ""] <- paste0("[", tag[tag != ""], "]")
      phy$tip.label <- paste(phy$tip.label, tag, sep = "_")
    }
  }


  ## choose and open device
  ## ----------------------
  if (pdf.height == "auto") pdf.height <- Ntip(phy)/5
  if (is.null(phy$edge.length)) phy <- compute.brlen(phy, method = "Grafen")
  xlim <- max(tipHeights(phy)) * xex
  if (length(grep("pdf$", file))){
    pdf(file,
        # paper = "a4",
        height = pdf.height, width = pdf.width)
  
  } else {
    png(file, height = pdf.height, width = pdf.width, units = "in", res = 600)
  }

  plot.phylo(phy, label.offset = .001, cex = .75, 
             tip.color = "white", edge.color = NA,
             x.lim = xlim
  )

  ## highlight higher rank: color topology
  ## -------------------------------------
  # if (length(higher.rank)) {
  #   fillClades(phy, higher.rank, col = "lightsteelblue2")
  # }
  if (tip_rank == "species" & genus.monophyly){
    # box.clades(phy, mp.gen, col = genus.col, text = gen.names)
    # To do: fillClades, um text argument erweitern
    fillClades(phy, mp.gen, col = genus.col, text = paste0(" ", names(mp.gen)), text.cex = 0.8)
  }
  plotPhyloUpon(phy, label.offset = .001, cex = .75, 
                tip.color = tcol, edge.color = ecol,
                x.lim = xlim
  )
  ## highlight higher rank: annotate edges
  ## -------------------------------------
  if (length(higher.rank)) {
    cladeBars(phy, higher.rank, text = names(higher.rank), cex = higher.rank.cex)
  }
  # nodelabels(); tiplabels()
  #bubble(mm, phy) # representation by markers
  #append2tips(phy, align = phyUE, grid = TRUE, pch = 19)
  
  if (!missing(min.support) & !is.null(phy$node.label)) 
    node.support(phy$node.label, cutoff = min.support, cex = .6)
  add.scale.bar(length = .05, lwd = 1.5)
  if ( !missing(title) ) title(main = title)
  # legend("topleft", inset = c(.2, .35), legend = names(tax), 
  #        fill = clrs, ncol = 3)
  dev.off()
  invisible(lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv))
}



