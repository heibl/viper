library(viper)
#data(viperidae)
phy <- viperidae
ns <- nodes <- noi(phy, regex = TRUE,
    list("Montivipera", "Macrovipera", "Vipera", "Daboia"))
cls <- c("yellow", "skyblue", "orange", "pink")
#cls <- "lightgrey"
xlim <- c(0, .8 * 1.25)

## plotting parameters
## -------------------
pt <- c("phylogram", "cladogram")
ptuel <- c(TRUE, FALSE)
pd <- c("rightwards", "leftwards", "upwards", "downwards")
pa <- list("tipwise", "cladewise", "all")

## PDF
## ---
fn <- "/Users/Stoffi/r/pkgs/viper/figs/check_box.tips.pdf"
pdf(fn)

for ( g in seq_along(pt)[1:2] )
  for ( h in seq_along(pd)[1:4] ) # direction
    for ( i in seq_along(pa)[1:3] ) {
     # g <- 1; h <- 3; i <- 2
      plot(phy, pt[g], use.edge.length = ptuel[g], direction = pd[h], #x.lim = xlim, 
           edge.color = 0, tip.color = 0)
      box.tips(phy, ns, col = cls, align = pa[[i]])
      plot.phylo.upon(phy, pt[g], use.edge.length = ptuel[g], 
                      direction = pd[h])
      clade.bars(phy, nodes = 33, barcol = "grey", align = "a", 
                 barwd = 6, lend = 0)
      title(paste("direction = '", pd[h], "', align = '", 
                  paste(pa[[i]], collapse = "' + '") , "'", sep = ""))
    }
dev.off()
system(paste("open", fn))