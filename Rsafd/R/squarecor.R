`squarecor` <-
function(X)
{
    LX <- log(X)
    TST <-  seriesMerge(der(LX,5),der(LX,-5),der(LX,-15),der(LX,-60))
    cor(TST*TST)
}

