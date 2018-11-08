library(viper)
grid = FALSE; col = "red"; text = NULL; pch = NULL
phy <- rcoal(12)
set.seed(1234)
x <- runif(12, -3, +7)
names(x) <- phy$tip.label
x <- data.frame(x)

# only positive values
plot(phy, x.lim = 3.4)
barplotPhylo(phy, abs(x), pch = 21, lwd = 5)

## negative and positive values
plot(phy, x.lim = 3.4)
barplotPhylo(phy, x, pch = 21, lwd = 5)
