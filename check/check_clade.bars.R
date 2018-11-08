library(viper)
data(viperidae)
phy <- viperidae
clades <- c("Montivipera", "Macrovipera", "Vipera", "Daboia")
ns <- nodes <- noi(phy, regex = TRUE, group = as.list(clades))
cols <- c("yellow", "skyblue", "orange", "pink")

xlim <- c(0.8, 38.2) * 1.5

fn <- "/Users/stoffi/r/pkgs/viper/check/figs/check_clade.bars.pdf"
pdf(fn)

pt <- c("phylogram", "cladogram")
ptuel <- c(TRUE, FALSE)
pd <- c("rightwards", "leftwards", "upwards", "downwards")
pa <- list("cladewise", "all")

for ( g in seq_along(pt)[1:2] ) # type
  for ( h in seq_along(pd)[1:4] ) # direction
    for ( i in seq_along(pa)[1:2] ) { # alignment
      #g <- 1; h <- 1; i <- 1; srt <- 0
      plot(phy, pt[g], use.edge.length = ptuel[g], direction = pd[h]
      #, x.lim = xlim[g]
           )
      clade.bars(phy, ns, barcol = cols, align = pa[[i]],
                 lend = 1, text = clades, offset = 0)
      clade.bars(phy, nodes = 27, barcol = "grey", barwd = 6, lend = 1, 
                 offset = 0)
      title(paste("direction = '", pd[h], "', align = '", 
                  paste(pa[[i]], collapse = "' + '") , "'", sep = ""))
      
      plot(phy, pt[g], use.edge.length = ptuel[g], direction = pd[h]
       #   , x.lim = xlim[g]
           )
      clade.bars(phy, ns, barcol = cols, align = pa[[i]],
                 lend = 1, text = clades, srt = 90, offset = 0)
      clade.bars(phy, nodes = 27, barcol = "grey", barwd = 6, lend = 1, 
                 offset = 0)
      title(paste("direction = '", pd[h], "', align = '", 
                  paste(pa[[i]], collapse = "' + '") , "'", sep = ""))
    }

dev.off()
system(paste("open", fn))