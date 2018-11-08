## This code is part of the viper package
## © C. Heibl 2014 (last update 2018-04-04)

#' @importFrom ape makeNodeLabel prop.part
#' @export

match.nodelabels <- function(source, target){
	
	 	nl <- character(source$Nnode)
        pp <- prop.part(source, check.labels = FALSE)
        labs <- attr(pp, "labels")
        fl <- tempfile()
        for (i in seq_len(source$Nnode)) {
            cat(sort(labs[pp[[i]]]), sep = "\n", file = fl)
            nl[i] <- tools::md5sum(fl)
        }
        unlink(fl)
        source$node.label <- nl
    #x1 <- makeNodeLabel(source, "md5sum")
    x2 <- makeNodeLabel(target, "md5sum")
    match(x2$node.label, source$node.label)
}


