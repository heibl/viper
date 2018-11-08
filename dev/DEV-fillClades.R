library(viper)
data("viperidae")
phy <- viperidae
gen <- data.frame(genus = gsub("_.*$", "", phy$tip.label),
                  species = phy$tip.label)
gen <- split(gen$species, f = gen$genus)
nodes <- noi(phy, gen)

## plot
plot(phy, type = "fan", no.margin = TRUE, 
     show.tip.label = TRUE, tip.color = 0)
# nodelabels(); tiplabels()
fillClades(phy, nodes, col = rainbow(length(nodes)))
plot.phylo.upon(phy, type = "fan", no.margin = TRUE, 
                show.tip.label = TRUE)
# nodelabels(); tiplabels()

lPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
id <- 29

getRadius <- function(id, lPP)
  max(sqrt(lPP$xx[id]^2 + lPP$yy[id]^2))

(min(nodes) - 1) * 2 * pi/lastPP$Ntip

## Aufgabe: Ich muss die Koordinaten der Endpunkte der Segemente 
## berechnen
