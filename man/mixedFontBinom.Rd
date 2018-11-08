\name{mixedFontBinom}
\alias{mixedFontBinom}

\title{Tip Label Preparation
}
\description{
This function makes use of expressions to prepare tip labels of a tree for plotting and thereby cares for setting strings in italianor plein and produce the times symbol in hybrid taxa. 
}
\usage{
mixedFontBinom(x, always.upright = c("sp.", "ssp.", "aff.", "cf."))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a vector of mode \code{"character"} representing the tip labels of a tree
}
  \item{always.upright}{
a vector of mode \code{"character"} giving the strings that will not be set in italics.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
a vector of expressions
}
\author{
Christoph Heibl, using code by Emmanuel Paradis
}
\seealso{
\code{\link{append2tips}} for plotting more text or symbols at the tips of a tree.
}
\examples{

## an examples tree
tr <- read.tree(text = "(((a,(b,c)),d), e);")
tr$tip.label <- c("Gorilla_gorilla", 
                  "Pan_sp.", 
                  "Homo_sapiens_ssp._sapiens", 
                  "Pongo_pygmaeus", 
                  "Mentha_x_piperita")
                  
## prepare tip labels ...                 
tr$tip.label <- mixedFontBinom(tr$tip.label, 
                              always.upright = c("sp.", "ssp."))
                                  
## ... and plot
plot(tr)
plot(tr, tip.color = c(rep("black", 4), "green"))
}