`makedate` <-
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

