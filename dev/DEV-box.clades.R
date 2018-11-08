par(mar = c(0, 0, 0, 0))

plot(phy, dir = "r", edge.color = 0, tip.color = 0)
box.clades.NEW(phy, nodes)
plot.phylo.upon(phy, dir = "r")

plot(phy, dir = "r", edge.color = 0, show.tip.label = FALSE)
box.clades.NEW(phy, nodes)
plot.phylo.upon(phy, dir = "r", show.tip.label = FALSE)

plot(phy, dir = "l", edge.color = 0, tip.color = 0)
box.clades.NEW(phy, nodes)
plot.phylo.upon(phy, dir = "l")

plot(phy, dir = "l", edge.color = 0, show.tip.label = FALSE)
box.clades.NEW(phy, nodes)
plot.phylo.upon(phy, dir = "l", show.tip.label = FALSE)

plot(phy, dir = "u", edge.color = 0, tip.color = 0)
box.clades.NEW(phy, nodes)
plot.phylo.upon(phy, dir = "u")

plot(phy, dir = "u", edge.color = 0, show.tip.label = FALSE)
box.clades.NEW(phy, nodes)
plot.phylo.upon(phy, dir = "u", show.tip.label = FALSE)

plot(phy, dir = "d", edge.color = 0, tip.color = 0)
box.clades.NEW(phy, nodes)
plot.phylo.upon(phy, dir = "d")

plot(phy, dir = "d", edge.color = 0, show.tip.label = FALSE)
box.clades.NEW(phy, nodes)
plot.phylo.upon(phy, dir = "d", show.tip.label = FALSE)
