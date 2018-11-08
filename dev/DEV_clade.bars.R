library(viper)
data(viperidae)
phy <- viperidae
clades <- c("Montivipera", "Macrovipera", "Vipera", "Daboia")
ns <- nodes <- noi(phy, regex = TRUE, group = as.list(clades))
cols <- c("yellow", "skyblue", "orange", "pink")

plot(phy, type = "p", dir = "r")
cladeBars(phy, nodes)
plot(phy, type = "p", dir = "r", show.tip.label = FALSE)
cladeBars(phy, nodes)
plot(phy, type = "p", dir = "l")
cladeBars(phy, nodes)
plot(phy, type = "p", dir = "l", show.tip.label = FALSE)
cladeBars(phy, nodes)
plot(phy, type = "p", dir = "u")
cladeBars(phy, nodes)
plot(phy, type = "p", dir = "u", show.tip.label = FALSE)
cladeBars(phy, nodes)
plot(phy, type = "p", dir = "d")
cladeBars(phy, nodes)
plot(phy, type = "p", dir = "d", show.tip.label = FALSE)
cladeBars(phy, nodes)


## FAN
par(mar = c(0, 0, 0, 0))
plot(phy, type = "f", show.tip.label = FALSE,
     x.lim = c(-0.7, 0.92))
cladeBars(phy, nodes, offset = 0, 
          barcol = cols, barwd = 20,
          align = "all",
          text = clades, bar = TRUE)

## RADIAL: not yet implemented
par(mar = c(0, 0, 0, 0))
plot(phy, type = "r", show.tip.label = FALSE)
cladeBars(phy, nodes, offset = 0, 
          barcol = cols, barwd = 10,
          align = "cladewise",
          text = clades)
 

      