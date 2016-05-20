`sstl` <-
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

