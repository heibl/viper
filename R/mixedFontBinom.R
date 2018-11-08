## This code is part of the viper package
## © C. Heibl 2014 (last update 2016-11-29)

#' @export

mixedFontBinom <- function (x, always.upright = c("sp.", "ssp.", "aff.", "cf.")) 
{
  prepExpr <- function(x){
    x <- unlist(strsplit(x, "_"))
    hybrid <- ifelse("x" %in% x, TRUE, FALSE)
    if ( hybrid) x <- x[x != "x"] 
    s <- !x %in% always.upright
    x[s] <- paste("italic(\"", x[s], "\")", sep = "")
    if (any(!s)) 
      x[!s] <- paste("plain(\"", x[!s], "\")", sep = "")
    if ( hybrid ){
      x <- paste(x, collapse = " %*% ")
    } else {
      x <- paste(x, collapse = "*\" \"*")
    }
    parse(text = x)
  }
  sapply(x, prepExpr)
}