`localvar` <-
function(SERIES, WIND = 30)
{
    OUT <- aggregate(SERIES[2:(length(positions(SERIES)) - 1),  ], adj = 1, moving
         = WIND, FUN = var)
    OUT
}

