## This code is part of the viper package
## © C. Heibl 2014 (last update 2018-03-28)

#' @importFrom graphics par
#' @export

cladeBars <- function(phy, nodes, bars = TRUE, barcol = "grey", barwd = 5, 
                      text = NULL, textcol = "black", textor = "t", 
                      srt, cex = 1, offset = 0, align = "cladewise", 
                      s = "clade", ...){
  
  ## check tree
  ## ----------
  if (!inherits(phy, "phylo")) 
    stop("object 'phy' is not of class 'phylo'")
  
  nClades <- length(nodes)
  desNodes <- lapply(nodes, descendants, phy = phy)
  
  
  ## set bar colors
  if (length(barcol) < nClades) 
    barcol <- rep(barcol, ceiling(nClades / length(barcol)))
  ## set bar width
  if (length(barwd) < nClades)
    barwd <- rep(barwd, ceiling(nClades / length(barwd)))
  
  ## align: cladewise or all?
  ## ------------------------
  align <- match.arg(align, c("all", "cladewise"))
  
  ## parameters of previous call to plot.phylo
  ## -----------------------------------------
  lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
  
  ## extension: .4 the font height
  ## ------------------------------
  ext <- string.dim("A", 
                    cex = lastPP$cex, 
                    font = lastPP$font,
                    srt = lastPP$srt) * .3
  
  if (lastPP$type %in% c("phylogram", "cladogram")){
    
    cc <- cladeCoordinates(lastPP, phy, nodes, align = align)
    
    ## set direction-related parameters
    ## --------------------------------
    dir <- lastPP$direction
    d.sign <- ifelse(dir %in% c("rightwards", "upwards"), 1, -1)
    #   minmax <- ifelse(dir %in% c("rightwards", "upwards"), 2, 1)
    xadj <- ifelse(dir %in% c("rightwards", "upwards"), 0, 1)
    #   crds <- c("xx", "yy")
    cid <- ifelse(dir %in% c("rightwards", "leftwards"), 1, 2)
    #   new.list.element <- paste(crds[cid], "new", sep = ".")
    
    ## coordinates
    xy <- data.frame()
    for (i in nodes){
      tmp <- cc[cc$crown == i, ]
      if (lastPP$dir == "rightwards"){
        tmp <- c(rep(max(tmp$tipsX), 2), 
                 min(tmp$tipsY) - ext[, "y"],  
                 max(tmp$tipsY) + ext[, "y"])
      }
      if (lastPP$dir == "leftwards"){
        tmp <- c(rep(min(tmp$tipsX), 2), 
                 max(tmp$tipsY) + ext[, "y"],  
                 min(tmp$tipsY) - ext[, "y"])
      }
      if (lastPP$dir == "upwards"){
        tmp <- c(min(tmp$tipsX) - ext[, "x"],
                 max(tmp$tipsX) + ext[, "x"], 
                 rep(max(tmp$tipsY), 2))
      }
      if (lastPP$dir == "downwards"){
        tmp <- c(max(tmp$tipsX) + ext[, "x"],
                 min(tmp$tipsX) - ext[, "x"], 
                 rep(min(tmp$tipsY), 2))
      }
      xy <- rbind(xy, tmp)
    }
    names(xy) <- c("x1", "x2", "y1", "y2")
    
    ## set 'offset' values
    ## -------------------
    if (missing(offset)) offset <- "-"
    if (is.character(offset))
      offset <- string.dim(offset, cex = lastPP$cex, font = lastPP$font,
                           srt = lastPP$srt)[, cid]
    if ( length(offset) == 1 ) offset <- rep(offset, 2)
    offset[2] <- offset[2] + offset[1]
    
    ## plot bars
    ## ---------
    if (bars) {
      for (i in 1:nrow(xy)){
        lines(xy[i, 1:2]+ offset[1], xy[i, 3:4],     		 
              lwd = barwd[i], col = barcol[i], ...)
      }  
    } 
  
    ## string rotation
    ## ---------------
    if (missing(srt)) srt <- 0
    if (srt == 0){
      xadj <- ifelse(dir %in% c("rightwards", "upwards"), 0, 1)
      yadj <- .5
    } else {
      xadj <- .5
      yadj <- ifelse(dir %in% c("rightwards", "upwards"), 0, 1)
    }  
    
    ## per.xy: coordinates perpendicular to edges
    ## ------------------------------------------
    #   if ( is.null(lastPP[[new.list.element]]) ){
    #     per.xy <- lastPP[[crds[cid]]]
    #     labelSpace <- string.dim(phy$tip.label, cex = lastPP$cex, 
    #                              font = lastPP$font, srt = lastPP$srt)[, cid]
    #     labelSpace <- labelSpace + lastPP$label.offset
    #     per.xy[tips] <- per.xy[tips] + d.sign * labelSpace
    #   } else {
    #     per.xy <- lastPP[[new.list.element]]
    #   }
    
    #   per.xy <- per.xy + d.sign * offset[1]
    
    ## plot text
    ## ---------
    if (!is.null(text)){
      center <- ifelse(lastPP$dir %in% c("rightwards", "leftwards"),
                       "y", "x")
      center <- grep(center, names(xy))
      center <- rowMeans(xy[, center]) # center text
      texlen <- length(text)
      if ( length(cex) != texlen ) cex <- rep(cex, texlen)
      xy[, 1] <- xy[, 1] + offset[2] * d.sign
      
      for (i in seq_along(text))
        text(xy[i, 1], center[i], text[i], adj = c(xadj, yadj), srt = srt,
             cex = cex[i], col = textcol[i])
      #         thistxt <- unlist(strsplit(text[i], "\n"))
      #         if ( length(thistxt) == 1 )
      #           mixed.fonts.text(x = xy[i, 1], # - xs, 
      #                            y = center[i], 
      #                            txt = thistxt, cex = cex[i], 
      #                            col = textcol, 
      #                            s = s)							
      #         else {
      #           ys <- strheight("O", cex = cex[i]) * 0.75
      #           mixed.fonts.text(x = xy[i, 1] - xs, 
      #                            y = center[i] + ys, 
      #                            txt = thistxt[1], cex = cex[i], 
      #                            col = textcol, 
      #                            s = s)
      #           mixed.fonts.text(x = xy[i, 1] - xs, 
      #                            y = center[i] - ys, 
      #                            txt = thistxt[2], cex = cex[i], 
      #                            col = textcol, 
      #                            s = s)		
      #         }
      #     }
      #    }	## end of FOR loop
      ## add text space to xy
      ## --------------------------
      sh <- string.dim(s = text, cex = cex, srt = srt)[, cid] * d.sign
      for ( i in seq_along(desNodes)){
        xy[desNodes[[i]], 1:2] <- xy[desNodes[[i]], 1:2] + sh
      }
    }
    
  }
  
  # ------- FAN -------------------
  if (lastPP$type == "fan"){

    cc <- cladeCoordCircle(lastPP, phy, nodes, align, offset)
    
    ## calculate aspect ratio
    ## ----------------------
    u <- par("usr")
    user.asp <- diff(u[3:4])/diff(u[1:2])
    p <- par("pin")
    inches.asp <- p[2]/p[1]
    asp <- user.asp/inches.asp
    
    dx <- apply(cc[, 1:3], 1, diff)
    n <-  apply(dx, 2, function(dx) 
      max(2, floor(200 * dx[1])))
    n <- rep(61, length(nodes))
    
    # adjust 'textor' and 'cex'
    # ------------------------
    texlen <- length(text)
    if (length(textor) != texlen)
      textor <- rep(textor, texlen)
    if (length(cex) != texlen)
      cex <- rep(cex, texlen)
    
    # loop over nodes to draw bars and text
    # -------------------------------------
    for (i in seq(along = nodes)){
      if (bars) {
        t2p <- 2 * pi * seq(cc[i, 1] + .001, 
                            cc[i, 2] - .001, 
                            length = n[i]) 
        t2p <- t2p + cc[i, "startpos"]
        xcc <- cos(t2p) * cc[i, "radius"]
        ycc <- sin(t2p) * cc[i, "radius"] * asp
        lines(xcc, ycc, col = barcol[i], lwd = barwd, 
              lend = 2, ...)  
      }
      
      # text on fan-shaped phylogeny
      # ----------------------------	
      if (!is.null(text)){	
        if (textor[i] == "t"){
          letter <- unlist(strsplit(text[i], ""))
          nlett <- length(letter)
          #print(x); print(cc); print(startpos)
          ## 'radChar': radian of individual letters
          radChar <- 2 * pi * seq(cc[i, 1] + .005, 
                                  cc[i, 2] - .005, 
                                  length = nlett) 
          radChar <- radChar + cc[i, "startpos"]
          # 'er': extended radius to set text off bar
          er <- cc[i, "radius"] #+ strheight("o")
          xcc <- rev(cos(radChar) * er)
          ycc <- rev(sin(radChar) * er * asp)
          ## calculate tangent angles
          ## ------------------------
          tang <- rev(radChar * 180 / pi) # angle in degree
          if (cc[i, "startpos"] > pi){
            ## upper half of circle
            letter <- rev(letter)
            tang <- tang + 90 
          } else {
            ## lower half of circle
            tang <- tang - 90
          }
          
          for (j in seq(along = letter))
            text(xcc[j], ycc[j], letter[j], 
                 cex = cex[i], col = textcol, 
                 adj = c(.5, .5), srt = tang[j], ...)
        }
        if (textor[i] == "r"){
          id <- 31#n[i]/2 + 1
          stop("implement me!")
          # text(xcc[id], ycc[id], text[i], 
          #      cex = cex[i], col = textcol, 
          #      pos = pos[i], srt = ang[i], ...)
        }
        if (textor[i] == "v")
          warning("not yet implemented")
        #           text(xx[i], center[i], text[i], 
        #                cex = cex[i], col = textcol, 
        #                adj = c(.5, .5), srt = 90, ...)
      }
    }
  }
  ## update and return 'last_plot.phylo'
  ## -----------------------------------
  #   new <- lastPP[[crds[cid]]]
  #   for ( i in seq_along(desNodes) ) {
  #     new[desNodes[[i]]] <- xy[i, 1 + cid]
  #   }
  #   lastPP[[new.list.element]] <- new
  #   assign("last_plot.phylo", lastPP, envir = .PlotPhyloEnv)
  #   invisible(lastPP)  
}
