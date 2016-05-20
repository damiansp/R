`abscor` <-
function(X)
{
    LX <- log(X)
    cor(abs(seriesMerge(der(LX,5),der(LX,-5),der(LX,-15),der(LX,-60))))
}

