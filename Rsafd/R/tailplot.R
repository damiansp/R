`tailplot` <-
function(gpd.obj, tail="upper", optlog = NA, extend = 1.5, labels = T, ...)
{
    if (tail == "upper")
    {
        data <- as.numeric(gpd.obj$upper.exceed)
        threshold <- gpd.obj$upper.thresh
        xi <- gpd.obj$upper.par.ests["xi"]
        lambda <- gpd.obj$upper.par.ests["lambda"]
    }
    if (tail == "lower")
    {
        data <- -as.numeric(gpd.obj$lower.exceed)
        threshold <- -gpd.obj$lower.thresh
        xi <- gpd.obj$lower.par.ests["xi"]
        lambda <- gpd.obj$lower.par.ests["lambda"]
    }
    plotmin <- threshold
    if(extend <= 1)
        stop("extend must be > 1")
    plotmax <- max(data) * extend
    x <- qgpd(seq(from = 0.001, to = 0.999, length = 999),threshold, lambda,xi)
    x <- pmin(x, plotmax)
    x <- pmax(x, plotmin)
    ypoints <- ppoints(sort(data))
    y <- pgpd(x,threshold, lambda,xi)
    type <- "tail"
    if(!is.na(optlog))
        alog <- optlog
    else alog <- "xy"
    if (tail == "upper") prob <- gpd.obj$p.less.upper.thresh
    if (tail == "lower") prob <- gpd.obj$p.larger.lower.thresh

    ypoints <- (1 - prob) * (1 - ypoints)
    y <- (1 - prob) * (1 - y)
    shape <- xi
    scale <- lambda * (1 - prob)^xi
    location <- threshold - (scale * ((1 - prob)^( - xi) -1))/xi
    plot(sort(data), ypoints, xlim = range(plotmin, plotmax), 
            ylim = range(ypoints, y, na.rm = T), xlab = "", ylab = "", log = alog, axes = T, ...)
    lines(x[y >= 0], y[y >= 0])
    if(labels) 
    {
        PlotType <- switch(alog,
             x = "log scale for x only",
             xy = "log - log scale",
             yx = "log - log scale",
             "natural scale"
             )
        xxlab <- switch(tail,
            upper = "x",
            lower = "-x"
        )
        yylab <- switch(tail,
            upper = "1-F(x)",
            lower = "F(x)"
        )
        title(main = paste("Plot of ",tail," tail in ", PlotType,sep=""),xlab = xxlab, ylab = yylab)
    }
    lastcurve <- list(lastfit = gpd.obj, type = type, dist
         = "gpd", plotmin = plotmin, plotmax = plotmax,
        alog = alog, location = as.numeric(location),
        shape = as.numeric(shape), scale = as.numeric(scale))
    assign("lastcurve", lastcurve, pos = 1)
    invisible()
}

