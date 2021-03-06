% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotPhyloWithClassification.R
\name{plotPhyloWithClassification}
\alias{plotPhyloWithClassification}
\title{Plot Phylogentic Tree with Clade Highlighting}
\usage{
plotPhyloWithClassification(phy, tax, outgroup = "dont.show",
  min.support, collapse.unsupported = TRUE, genus.monophyly = TRUE,
  genus.col = "grey95", edges, higher.rank, higher.rank.cex = 0.85,
  tags, xex = 1.25, pdf.height = "auto", pdf.width = 20, title, file)
}
\arguments{
\item{phy}{An object of class \code{\link{phylo}}.}

\item{tax}{An object of class \code{\link{megapteraProj}} or a taxonomic
table in parent-child format; if missing, the taxonomy is taken from the
tip labels.}

\item{outgroup}{A vector of mode \code{"character"}. Outgroup will not be
shown, if \code{outgroup = "dont.show"}.}

\item{min.support}{An integer giving a threshold for support values. Edges
with lower support values will be collapsed into polytomies, if
\code{collapes.unsupported = TRUE}.}

\item{collapse.unsupported}{Logical}

\item{genus.monophyly}{Logical indicating if monophyletic genera should be highlighted.}

\item{genus.col}{A vector of mode \code{"character"} giving a color for the filling of
boxes drawn around monophyletic genera.}

\item{edges}{A vector of mode \code{"character"}.}

\item{higher.rank}{A vector of mode \code{"character"}.}

\item{tags}{A data frame with two columns: the first column contains tip
labels as contained in phy, the second columns contains the labels with
will be appended (within square bracket) to the corresponding tip labels.}

\item{xex}{A numeric, the x.lim extension; try larger values if parts of the
plot are truncated at the right side of the plot.}

\item{pdf.height}{An integer ...}

\item{pdf.width}{An integer ...}

\item{title}{A vector of mode \code{"character"}.}

\item{file}{A vector of mode \code{"character"}.}
}
\description{
Plots a phylogenetic tree together with color highlighting of
  the underlying taxonomic classification.
}
\details{
\code{collapse.unsupported} assumes that the support values are
  found in \code{phy$node.label}; no edges will be collapsed, if this list
  element does not exist.
}
