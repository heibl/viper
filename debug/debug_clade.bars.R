

## defaults
## --------
data(viperidae)
phy <- viperidae
clades <- c("Montivipera", "Macrovipera", "Vipera", "Daboia")
nodes <-  noi(phy, regex = TRUE, group = as.list(clades))
bars = TRUE
barcol = "grey50"
barwd = 3
text = NULL
textcol = "black"
srt <- 0
cex = 1
cols <- c("yellow", "skyblue", "orange", "pink") # default: NULL

## plot phylogeny
## --------------
g <- 1; h <- 3; i <- 1
plot(phy, pt[g], use.edge.length = ptuel[g], direction = pd[h])


