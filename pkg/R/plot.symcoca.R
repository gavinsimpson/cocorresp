"plot.symcoca" <-
function(x,
         choices= c(1:2),
         display = c("species", "site"),
         scaling = FALSE,
         oneFig = TRUE,
         type = c("points", "text", "none"),
         ask = prod(par("mfcol")) < 2 && dev.interactive(),
         cex = c(0.7, 0.7),
         pch = c(par("pch"), 3),
         col = c("black", "red"),
         ylab, xlab,
         main, sub = "",
         axes = TRUE,
         ann = par("ann"),
         ...)
  {
    if(!inherits(x, "symcoca"))
      stop("use only with \"symcoca\" objects")
    ## needs a check on choices length == 2,
    ## and max(choices) =< x$n.axes && any(choices > 0)
    ##choice.len <- length(choices)
    if(length(choices) != 2)
      stop("choices must be of length 2 only")
    if(max(choices) > x$n.axes)
      stop("axis choice > number of axes in model")
    if(any(choices < 1))
      stop("choices must be greater than 0")
    ## check to see what has been entered for display
    display <- match.arg(display, several.ok = TRUE)
    ## partition the plotting region if using only one figure
    if(oneFig)
      {
        opar <- par(mfrow = c(1, 2), no.readonly = TRUE)
        on.exit(par(opar))
      }
    else
      {
        opar <- par(ask = TRUE, no.readonly = TRUE)
        on.exit(par(opar))
      }
    ## FIXME - scaling is integer in scores.symcoca but I think
    ## it is better if it is logical. He we set scaling == 2
    ## is scaling == TRUE
    if(scaling)
      scaling <- 2
    ## this is for the type of plot
    type <- match.arg(type)
    plt.dat <- scores(x, choices = choices, scaling = scaling,
                      display = display)
    if (is.null(plt.dat$species) && is.null(plt.dat$site)) 
        plt.dat <- list(default = plt.dat)
    if("species" %in% display)
      {
        if("site" %in% display)
          {
            ranx1 <- range(plt.dat$species$U1[,1], plt.dat$site$X1[,1])
            rany1 <- range(plt.dat$species$U1[,2], plt.dat$site$X1[,2])
            ranx2 <- range(plt.dat$species$U2[,1], plt.dat$site$X2[,1])
            rany2 <- range(plt.dat$species$U2[,2], plt.dat$site$X2[,2])
          }
        else
          {
            ranx1 <- range(plt.dat$default$U1[,1])
            rany1 <- range(plt.dat$default$U1[,2])
            ranx2 <- range(plt.dat$default$U2[,1])
            rany2 <- range(plt.dat$default$U2[,2])
          }
      } else {
        ranx1 <- range(plt.dat$default$X1[,1])
        rany1 <- range(plt.dat$default$X1[,2])
        ranx2 <- range(plt.dat$default$X2[,1])
        rany2 <- range(plt.dat$default$X2[,2])
      }
    if(missing(ylab))
      ylabs <- substitute("Axis " * arg1 * " " * (lambda[arg1] == arg2),
                          list(arg1 = choices[2],
                               arg2 = round(x$lambda[choices[2]], 4)))
    if(missing(xlab))
      xlabs <- substitute("Axis " * arg1 * " " * (lambda[arg1] == arg2),
                          list(arg1 = choices[1],
                               arg2 = round(x$lambda[choices[1]], 4)))
    if(missing(main))
      {
        main1 <- x$nam.dat$namY
        main2 <- x$nam.dat$namX
      }
    plot.new()
    plot.window(xlim = ranx1, ylim = rany1, asp = 1, ...)
    if(!is.null(plt.dat$site))
      if(type == "text")
        text(plt.dat$site$X1, rownames(plt.dat$site$X1),
             cex = cex[1], col = col[1])
      else if (type == "points")
        points(plt.dat$site$X1, cex = cex[1], pch = pch[1], col = col[1])
    if(!is.null(plt.dat$species))
      if(type == "text")
        text(plt.dat$species$U1, rownames(plt.dat$species$U1),
             cex = cex[1], col = col[1])
      else if(type == "points")
        points(plt.dat$species$U1, cex = cex[2], pch = pch[2], col = col[2])
    if(!is.null(plt.dat$default))
      if(type == "text")
        text(plt.dat$default[[1]], rownames(plt.dat$default[[1]]),
             cex = cex[1], col = col[1])
      else if(type == "points")
        points(plt.dat$default[[1]], cex = cex[2], pch = pch[2], col = col[2])
    if(axes)
      {
        axis(1)
        axis(2)
        box()
      }
    if(ann)
      title(main = main1, sub = sub, ylab = ylabs, xlab = xlabs)
    plot.new()
    plot.window(xlim = ranx2, ylim = rany2, asp = 1, ...)
    if(!is.null(plt.dat$site))
      if(type == "text")
        text(plt.dat$site$X2, rownames(plt.dat$site$X2),
             cex = cex[1], col = col[1])
      else if(type == "points")
        points(plt.dat$site$X2, cex = cex[1], pch = pch[1], col = col[1])
    if(!is.null(plt.dat$species))
      if(type == "text")
        text(plt.dat$species$U2, rownames(plt.dat$species$U2),
             cex = cex[2], col = col[2])
      else if(type == "points")
        points(plt.dat$species$U2, cex = cex[2], pch = pch[2], col = col[2])
    if(!is.null(plt.dat$default))
      if(type == "text")
        text(plt.dat$default[[2]], rownames(plt.dat$default[[2]]),
             cex = cex[1], col = col[1])
      else if(type == "points")
        points(plt.dat$default[[2]], cex = cex[1], pch = pch[1], col = col[1])
    if(axes)
      {
        axis(1)
        axis(2)
        box()
      }
    if(ann)
      title(main = main2, sub = sub, ylab = ylabs, xlab = xlabs)
    invisible()
  }
