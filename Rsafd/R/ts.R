
"localmean" <-
function(SERIES, WIND = 30)
{
    OUT <- aggregate(SERIES[2:(length(positions(SERIES)) - 1),  ], adj = 1, moving
         = WIND, FUN = mean)
    OUT
}

"localvar" <- 
function(SERIES, WIND = 30)
{
    OUT <- aggregate(SERIES[2:(length(positions(SERIES)) - 1),  ], adj = 1, moving
         = WIND, FUN = var)
    OUT
}

"pred.ar" <- 
function(series, ar.est, ahead = 1)
{
    # function to predict using an ar model:
    # ahead gives the number of predictions to make
    order <- ar.est$order
    series <- as.matrix(series)
    pred.out <- array(NA, dim = c(order + ahead, ncol(series)), dimnames = list(
        NULL, dimnames(series)[[2]]))
    mean.ser <- apply(series, 2, mean)
    ser.cent <- sweep(series, 2, mean.ser)
    pred.out[seq(order),  ] <- ser.cent[rev(nrow(series) - seq(order) + 1),  ]
    for(i in (order + 1):nrow(pred.out)) {
        pred.out[i,  ] <- apply(aperm(ar.est$ar, c(1, 3, 2)) * as.vector(
            pred.out[i - seq(order),  ]), 3, sum)
    }
    sweep(pred.out[ - seq(order),  , drop = F], 2, mean.ser, "+")
}
 


"begday" <- 
function(DAY)
{
    DAYDATE <- as.character(timeDate(as.character(DAY), format =
        "%m/%d/%Y"))
    timeDate(paste(DAYDATE, "00:00:00.000", sep = " "), format =
        "%m/%d/%Y %02H:%02M:%02S.%03N")
}

"noon" <- 
function(DAY)
{
    DAYDATE <- as.character(timeDate(as.character(DAY), format =
        "%m/%d/%Y"))
    timeDate(paste(DAYDATE, "12:00:00.000", sep = " "), format =
        "%m/%d/%Y %02H:%02M:%02S.%03N")
}

"sstl" <- 
function(SERIES, FREQ = 365.24000000000001)
{
    if(class(SERIES) != "timeSeries")
        stop("The function sstl is restricted to timeSeries objects")
    SDATE <- timeDate(as.character(min(positions(SERIES))), format =
        "%02m/%02d/%Y")
    YR <- as.integer(as.character(timeDate(as.character(SDATE), format =
        "%Y")))
    MO <- as.integer(as.character(timeDate(as.character(SDATE), format =
        "%02m")))
    DAY <- as.integer(as.character(timeDate(as.character(SDATE), format =
        "%02d")))
    TMP <- julian(m = MO, d = DAY, y = YR, origin = c(m = 1, d = 1, y = YR
        ))
    START <- YR + (1 + TMP)/365
    TMP.ts <- ts(seriesData(SERIES), start = START, frequency = FREQ)
    TMP.stl <- stl(TMP.ts, "periodic")
    OUT <- list(sea = timeSeries(pos = positions(SERIES), data = TMP.stl$
        sea), rem = timeSeries(pos = positions(SERIES), data = TMP.stl$
        rem))
    class(OUT) <- "sstl"
    OUT
}

"kalman" <- 
function(FF, SigV, GG, SigW, Xhat, Omega, Y)
{
    Delta <- GG %*% Omega %*% t(GG) + SigW
    Theta <- FF %*% Omega %*% t(GG)
    X <- FF %*% Xhat + Theta %*% solve(Delta) %*% (Y - GG %*% Xhat)
    Om <- FF %*% Omega %*% t(FF) + SigV - Theta %*% solve(Delta) %*% t(
        Theta)
    Ret <- list(xpred = X, error = Om)
    Ret
}

"readindex" <- 
function(FRAME)
{
    # Use the data frame FRAME resulting from the import of one of the Energy indexes
    # created from the DOW JONES data base, and create a tri-variate timeSeries with
    # components cap (for capitalization), divisor and index.
    names(FRAME) <- c("date", "nbstock", "cap", "divisor", "index")
    timeSeries(pos = makedate(FRAME[, 1]), data = FRAME[3:5])
}

"makedate" <- 
function(REAL)
{
    # This function takes a real like 19920106.00 and returns a date of the
    # form 10/6/1992
    YEAR <- floor(as.integer(REAL)/10000)
    TMP <- as.integer(REAL) - YEAR * 10000
    MONTH <- floor(TMP/100)
    DAY <- TMP - MONTH * 100
    TMP.julian <- julian(MONTH, DAY, YEAR)
    dates(TMP.julian)
}
