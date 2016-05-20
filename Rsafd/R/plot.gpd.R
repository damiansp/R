`plot.gpd` <-
function(gpd.obj, tail="upper",  optlog = NA, extend = 1.5, labels = T, ...)
{
if (tail == "upper") {
    data <- as.numeric(gpd.obj$upper.exceed)
    threshold <- gpd.obj$upper.thres
    xi <- gpd.obj$upper.par.ests["xi"]
    lambda <- gpd.obj$upper.par.ests["lambda"]
}

## added here!
if (tail == "lower") {
    data <- -as.numeric(gpd.obj$lower.exceed)
    threshold <- -gpd.obj$lower.thres
    xi <- gpd.obj$lower.par.ests["xi"]
    lambda <- gpd.obj$lower.par.ests["lambda"]
}

    choices <- c("Excess Distribution", 
        "Tail of Underlying Distribution", 
        "Scatterplot of Residuals", "QQplot of Residuals")
    tmenu <- paste("plot:", choices)
    pick <- 1
    while(pick > 0) {
        pick <- menu(tmenu, title =
            "\nMake a plot selection (or 0 to exit):")
        if(pick >= 3) {
            excess <- data - threshold
            res <- logb(1 + (xi * excess)/lambda)/xi
        }
        if(pick == 3) {
            plot(res, ylab = "Residuals", xlab = "Ordering"
                )
            lines(lowess(1:length(res), res))
        }
        if(pick == 4)
            qplot(res)
        if(pick == 1 || pick == 2) {
            plotmin <- threshold
            if(extend <= 1)
                stop("extend must be > 1")
            plotmax <- max(data) * extend
            x <- qgpd(seq(from = 0.001, to = 0.999, length = 1000),
                xi, threshold, lambda)
            x <- pmin(x, plotmax)
            x <- pmax(x, plotmin)
            ypoints <- ppoints(sort(data))
            y <- pgpd(x, xi, threshold, lambda)
        }
        if(pick == 1) {
            type <- "eplot"
            if(!is.na(optlog))
                alog <- optlog
            else alog <- "x"
            if(alog == "xy")
                stop("Double log plot of Fu(x-u) does not make much sense"
                    )
            yylab <- "Fu(x-u)"
            shape <- xi
            scale <- lambda
            location <- threshold
        }
        if(pick == 2) {
            type <- "tail"
            if(!is.na(optlog))
                alog <- optlog
            else alog <- "xy"
            if (tail == "upper") prob <- gpd.obj$p.less.upper.thresh
            if (tail == "lower") prob <- gpd.obj$p.larger.lower.thresh

            ypoints <- (1 - prob) * (1 - ypoints)
            y <- (1 - prob) * (1 - y)
            yylab <- "1-F(x)"
            shape <- xi
            scale <- lambda * (1 - prob)^xi
            location <- threshold - (scale * ((1 - prob)^
                ( - xi) - 1))/xi
        }
        if(pick == 1 | pick == 2) {
            plot(sort(data), ypoints, xlim = range(plotmin,
                plotmax), ylim = range(ypoints, y,
                na.rm = T), xlab = "", ylab = "", log
                 = alog, axes = T, ...)
            lines(x[y >= 0], y[y >= 0])
            if(labels) {
                xxlab <- "x"
                if(alog == "x" | alog == "xy" | alog ==
                    "yx")
                    xxlab <- paste(xxlab,
                        "(on log scale)")
                if(alog == "xy" | alog == "yx" | alog ==
                    "y")
                    yylab <- paste(yylab,
                        "(on log scale)")
                title(xlab = xxlab, ylab = yylab)
            }
            details <- paste("threshold = ", format(signif(
                threshold, 3)), "   xi = ", format(
                signif(shape, 3)), "   scale = ",
                format(signif(scale, 3)),
                "   location = ", format(signif(
                location, 3)), sep = "")
            print(details)
            lastcurve <- list(lastfit = gpd.obj, type =
                type, dist = "gpd", plotmin = plotmin,
                plotmax = plotmax, alog = alog,
                location = as.numeric(location), shape
                 = as.numeric(shape), scale =
                as.numeric(scale))
            assign("lastcurve", lastcurve, pos = 1)
        }
    }
}

