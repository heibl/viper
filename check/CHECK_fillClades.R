library(viper)
data(viperidae)
phy <- viperidae
ns <- nodes <- noi(phy, regex = TRUE,
    list("Montivipera", "Macrovipera", "Vipera", "Daboia"))
cls <- c("yellow", "skyblue", "orange", "pink")

## parameters
## ----------
pt <- c("phylogram", "cladogram")
ptuel <- c(TRUE, FALSE)
pd <- c("rightwards", "leftwards", "upwards", "downwards")
pa <- c("cladewise", "all")
lims <- list(c(0, 1.6), NULL)
stl <- TRUE # show tiplabels
txt <- c("Montivipera", "Macrovipera", "Vipera", "Daboia")
#txt <- NULL

fn <- "check/figs/fillClades.pdf"
pdf(fn)

for ( g in seq_along(pt)[1:2] ) # type
  for ( h in seq_along(pd)[1:4] ) # direction
    for ( i in seq_along(pa)[1:2] ) { # align
      if ( h %in% 3:4 ) lims <- rev(lims)
      plot(phy, pt[g], use.edge.length = ptuel[g], direction = pd[h], 
           x.lim = lims[[1]], y.lim = lims[[2]], 
           edge.color = 0, tip.color = 0, show.tip.label = stl)
      fillClades(phy, ns, col = cls, text = txt)
      # clade.bars(phy, 33)
      plotPhyloUpon(phy, pt[g], use.edge.length = ptuel[g], 
                      direction = pd[h], show.tip.label = stl)
      title(paste("direction = '", pd[h], "', align = '", 
                  paste(pa[[i]], collapse = "' + '") , "'", sep = ""))
    }
dev.off()
system(paste("open", fn))
g <- h <- i <- 1