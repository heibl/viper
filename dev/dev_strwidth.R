s <- "Axototl"
plot(x = c(0, 1), y = c(0, 2))
## horizontal
points(.1, .3, pch = 4, col = "red")
text(.1, .3, s, cex = 3, adj = c(0,0 ))
x <- string.dim(s, cex = 3, srt = 0)
points(.1 + x[1], .3, pch = 4, col = "blue")
points(.1, .3 + x[2], pch = 4, col = "green")

## vertical
## remark: if using srt, adj still refers to the 
## dimensions like it did in the unrotated string
points(.1, .5, pch = 4, col = "red")
text(.1, .5, s, cex = 3, adj = c(0,1), srt = 90)
x <- string.dim(s, cex = 3, srt = 90)
points(.1 + x[1], .5, pch = 4, col = "blue")
points(.1, .5 + x[2], pch = 4, col = "green")

## diagonal: does not yet work
x <- .4; y <- .7
points(x, y, pch = 4, col = "red")
text(x, y, s, cex = 3, adj = c(0,1), srt = 45)
xy <- string.dim(s, cex = 3, srt = 45)
points(x + xy["x"], y, pch = 4, col = "blue")
points(x , y + xy["y"], pch = 4, col = "green")


