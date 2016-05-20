`shape.plot` <-
function(data, tail="upper", method = "ml", from = 0.5, to = 0.98, nint = 30)
{
if (tail == "upper") {
    thresh <- "Threshold"
    toptext <- "Percent Data Points above Threshold"
}
if (tail == "lower") {
    data <- -data
    thresh <- "Threshold"
    toptext <- "Percent Data Points below Threshold"
}
    if(is(data, "series")) {
        data <- seriesData(data)
    }
    if(is.data.frame(data)) {
        data <- as.matrix(data)
    }
    data <- sort(unclass(data))
    assign("tempData", data, pos = 1)
    estFun <- get(paste("gpd", method, sep = "."))
    assign("tempEstFun", estFun, pos = 1)

    n <- length(data)
    l1 <- data[trunc(from * n)]
    l2 <- data[trunc(to * n)]
    x <- pretty(c(l1, l2), n = nint)

    one.y <- function(u)
    {
        xx <- tempData[tempData > u]
        excess <- xx - u
        gpd.est <- tempEstFun(sample = excess, location = 0)$param.est
        c(gpd.est[3], length(xx)/length(tempData))
    }

    iii <- apply(as.matrix(x), 1, one.y)
    yy <- iii[1,  ]
    ylim <- range(yy)
    ylim[1] <- ylim[1] - 0.5
    ylim[2] <- ylim[2] + 0.5

    t1 <- "Estimate of k"
    if(SHAPE.XI)
        t1 <- "Estimate of xi"

if (tail == "lower")
{ 
    plot(-x, yy, type = "l", xlab = thresh, ylab = t1,
        ylim = ylim)
    nl <- length(pretty(x))
    xx <- pretty(x)
    indB <- .C("empirfunc",
        as.double(xx),
        as.double(x),
        as.integer(length(xx)),
        as.integer(length(x)),
        as.integer(1:length(xx)))[[5]] + 1
        axis(3, at = -x[indB], lab = paste(format(round(iii[2, indB] * 100))))
    mtext(toptext, side = 3, line = 3)
}
else
{
    plot(x, yy, type =

     "l", xlab = thresh, ylab = t1,
        ylim = ylim)
    nl <- length(pretty(x))
    xx <- pretty(x)
    indB <- .C("empirfunc",
        as.double(xx),
        as.double(x),
        as.integer(length(xx)),
        as.integer(length(x)),
        as.integer(1:length(xx)))[[5]] + 1
        axis(3, at = x[indB], lab = paste(format(round(iii[2, indB] * 100))))
    mtext(toptext, side = 3, line = 3)
}
}

