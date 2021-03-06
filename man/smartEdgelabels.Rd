% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smartEdgelabels.R
\name{smartEdgelabels}
\alias{smartEdgelabels}
\title{Automatic Placement of Edgelabels}
\usage{
smartEdgelabels(phy, text, edge, fine.tune, ...)
}
\arguments{
\item{phy}{An object of class \code{\link[ape]{phylo}}.}

\item{text}{A vector of mode \code{"character"} containing the label text,
e.g. clade names, geographic range, etc.}

\item{edge}{A vector of mode \code{"numeric"} containing the number of the edges to be labelled.}

\item{fine.tune}{A list ....}

\item{...}{Further arguments passed to \code{\link{edgelabels}}.}
}
\value{
Nothing, \code{smartEdgelabels} is called for its side effect.
}
\description{
Try to label edges in a plotted phylogeny in a way that edges
  and labels do not cross.
}
