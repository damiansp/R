`axcor` <-
function(X)
{
    LX <- log(X)
    cor(seriesMerge(der(LX,5),der(LX,-5),der(LX,-15),der(LX,-60)))
}

