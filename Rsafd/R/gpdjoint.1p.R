`gpdjoint.1p` <-
function(x, y, copula, x.est, y.est)
{
    h <- gpd.1p(x, x.est)
    g <- gpd.1p(y, y.est)
    val <- pcopula(copula, h, g)
    val
}

